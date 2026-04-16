# =============================================================================
# FILE:    PRG/Functions/residual_check_mmrm.R
# PURPOSE: Residual diagnostic plots for Mixed Model Repeated Measures (MMRM)
#          fitted in functions_linear_models.R. Produces seven standard
#          diagnostic plots to assess model assumptions.
#
# FUNCTIONS:
#   check_mmrm_two_way() -- Diagnostics for two-factor MMRM
#                           (heatwave_status * period, AR(1))
#   check_mmrm_one_way() -- Diagnostics for one-factor MMRM
#                           (heatwave_status only, AR(1))
#
# DIAGNOSTIC PLOTS PRODUCED (identical in both functions):
#   Plot 1 -- Residuals vs Fitted Values
#             Tests linearity: residuals should scatter randomly around 0
#             with no systematic curve. A LOESS smoother highlights trends.
#   Plot 2 -- Q-Q Plot of Residuals
#             Tests normality: points should fall close to the diagonal line.
#             Deviations in the tails indicate non-normal residuals.
#   Plot 3 -- Residuals by Heatwave Phase (and period for two_way)
#             Tests whether residuals are balanced across experimental groups.
#             Medians near 0 and similar spread across groups is ideal.
#   Plot 4 -- Scale-Location Plot (sqrt|standardized residuals| vs fitted)
#             Tests homoskedasticity: the LOESS line should be approximately
#             flat. An upward trend indicates increasing variance with the mean.
#   Plot 5 -- Residuals vs Observation Order
#             Tests for temporal autocorrelation not captured by the AR(1)
#             structure. Residuals should scatter randomly without waves or
#             systematic drift over the sequence.
#   Plot 6 -- Histogram of Residuals
#             Visual check of residual distribution shape. Should resemble
#             a bell curve centered on zero.
#   Plot 7 -- Residuals by Participant
#             Tests for participant-specific residual patterns not captured
#             by the random effects. Medians near 0 for all participants is
#             ideal. If the study has > 20 participants, a random sample of
#             20 is displayed.
# =============================================================================


# =============================================================================
# Function 1: check_mmrm_two_way()
# =============================================================================
#
# PURPOSE:
#   Fits the same two-factor MMRM as lmm_two_way() (heatwave_status * period,
#   AR(1) covariance, Kenward-Roger df) and produces seven residual diagnostic
#   plots. Intended to be run after lmm_two_way() to verify model assumptions.

check_mmrm_two_way <- function(data_list, outcome_vars) {
  
  for (outcome_name in names(outcome_vars)) {
    cat("\n", strrep("=", 60), "\n")
    cat("Residual diagnostics:", outcome_name, "\n")
    cat(strrep("=", 60), "\n\n")
    
    data        <- data_list[[outcome_vars[[outcome_name]]$data]]
    outcome_var <- outcome_vars[[outcome_name]]$var
    
    if (!(outcome_var %in% names(data))) {
      warning(paste("Variable", outcome_var, "not found in",
                    outcome_vars[[outcome_name]]$data))
      next
    }
    
    # Prepare data: identical to lmm_two_way() 
    filtered_data <- data %>%
      filter(heatwave_status %in% c("BEF_HW_June", "HW_June", "AFT_HW_June")) %>%
      mutate(
        period          = factor(period, levels = c("AM", "PM", "NI")),
        heatwave_status = factor(heatwave_status,
                                 levels = c("BEF_HW_June", "HW_June", "AFT_HW_June")),
        participant_id  = as.factor(participant_id),
        day_index       = as.integer(as.factor(date_converted)),
        period_index    = as.integer(period),
        visit           = ((day_index - 1) * 3) + period_index
      )
    filtered_data$visit <- as.factor(filtered_data$visit)
    
    # Fit MMRM (same specification as lmm_two_way) 
    formula_full <- reformulate(
      termlabels = c("heatwave_status * period", "ar1(visit | participant_id)"),
      response   = outcome_var
    )
    model_full <- mmrm(formula_full, data = filtered_data,
                       control = mmrm_control(method = "Kenward-Roger"))
    
    # Extract residuals and fitted values 
    # MMRM drops rows with missing outcome values when fitting.
    # The !is.na(fitted(...)) index aligns predictor columns with the
    # subset of rows actually used in model fitting.
    residuals_data <- data.frame(
      fitted          = fitted(model_full),
      residuals       = residuals(model_full),
      participant_id  = filtered_data$participant_id[!is.na(fitted(model_full))],
      heatwave_status = filtered_data$heatwave_status[!is.na(fitted(model_full))],
      period          = filtered_data$period[!is.na(fitted(model_full))]
    )
    
    # Standardized residuals (z-scored) and derived quantities for plots
    residuals_data$std_residuals          <- residuals_data$residuals /
      sd(residuals_data$residuals, na.rm = TRUE)
    residuals_data$abs_residuals          <- abs(residuals_data$residuals)
    residuals_data$sqrt_abs_std_residuals <- sqrt(abs(residuals_data$std_residuals))
    
    # Plot 1: Residuals vs Fitted 
    # Tests linearity. LOESS smoother should be approximately flat at y=0.
    p1 <- ggplot(residuals_data, aes(x = fitted, y = residuals)) +
      geom_point(alpha = 0.6) +
      geom_smooth(method = "loess", se = FALSE, color = "red") +
      geom_hline(yintercept = 0, linetype = "dashed") +
      labs(title = "Residuals vs Fitted Values",
           x = "Fitted Values", y = "Residuals") +
      theme_minimal()
    print(p1)
    
    # Plot 2: Q-Q Plot 
    # Tests normality of residuals. Points should follow the diagonal line.
    p2 <- ggplot(residuals_data, aes(sample = residuals)) +
      stat_qq() +
      stat_qq_line() +
      labs(title = "Q-Q Plot of Residuals",
           subtitle = "Points should follow the diagonal line") +
      theme_minimal()
    print(p2)
    
    # Plot 3: Residuals by heatwave phase and period 
    # Tests group balance. Median residuals should be near 0 in all groups.
    p3 <- ggplot(residuals_data, aes(x = heatwave_status, y = residuals)) +
      geom_boxplot() +
      facet_wrap(~ period) +
      geom_hline(yintercept = 0, linetype = "dashed") +
      labs(title = "Residuals by Heatwave Phase and Period",
           x = "Heatwave Status", y = "Residuals") +
      theme_minimal()
    print(p3)
    
    # Plot 4: Scale-Location 
    # Tests homoskedasticity. LOESS line should be approximately flat.
    # An upward trend indicates variance increasing with fitted values.
    p4 <- ggplot(residuals_data, aes(x = fitted, y = sqrt_abs_std_residuals)) +
      geom_point(alpha = 0.6) +
      geom_smooth(method = "loess", se = FALSE, color = "red") +
      labs(title = "Scale-Location Plot",
           subtitle = "Check for heteroskedasticity -- LOESS line should be flat",
           x = "Fitted Values", y = "sqrt(|Standardized Residuals|)") +
      theme_minimal()
    print(p4)
    
    # Plot 5: Residuals vs Observation Order 
    # Tests for residual autocorrelation not captured by AR(1).
    # No systematic wave pattern or drift is ideal.
    residuals_data$observation_order <- seq_len(nrow(residuals_data))
    p5 <- ggplot(residuals_data, aes(x = observation_order, y = residuals)) +
      geom_point(alpha = 0.6) +
      geom_smooth(method = "loess", se = FALSE, color = "red") +
      geom_hline(yintercept = 0, linetype = "dashed") +
      labs(title = "Residuals vs Observation Order",
           subtitle = "Check for temporal patterns not captured by AR(1)",
           x = "Observation Order", y = "Residuals") +
      theme_minimal()
    print(p5)
    
    # Plot 6: Histogram of Residuals 
    # Visual check of residual distribution. Should approximate a bell curve.
    p6 <- ggplot(residuals_data, aes(x = residuals)) +
      geom_histogram(bins = 30, alpha = 0.7, fill = "skyblue", color = "black") +
      geom_vline(xintercept = 0, linetype = "dashed", color = "red") +
      labs(title = "Distribution of Residuals",
           x = "Residuals", y = "Frequency") +
      theme_minimal()
    print(p6)
    
    # Plot 7: Residuals by Participant 
    # Tests for participant-specific patterns. Median per participant should
    # be near 0. If > 20 participants, a random sample of 20 is used.
   
    set.seed(123)
    unique_participants <- unique(residuals_data$participant_id)
    if (length(unique_participants) > 20) {
      sample_participants <- sample(unique_participants, 20)
      residuals_sample    <- residuals_data[
        residuals_data$participant_id %in% sample_participants, ]
    } else {
      residuals_sample <- residuals_data
    }
    
    p7 <- ggplot(residuals_sample, aes(x = factor(participant_id), y = residuals)) +
      geom_boxplot() +
      geom_hline(yintercept = 0, linetype = "dashed") +
      labs(title = "Residuals by Participant",
           subtitle = "Random sample of up to 20 participants shown",
           x = "Participant ID", y = "Residuals") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
    print(p7)
    
    cat("\nEnd of residual diagnostics for", outcome_name, "\n")
  }
}


# =============================================================================
# Function 2: check_mmrm_one_way()
# =============================================================================
#
# PURPOSE:
#   Fits the same one-factor MMRM as lmm_one_way() (heatwave_status only,
#   AR(1) covariance, Kenward-Roger df) and produces the same seven residual
#   diagnostic plots as check_mmrm_two_way(). Intended for daily outcomes
#   (body mass, urine frequency, physical activity) where there is no
#   period-of-day dimension.
#
#   The only structural differences from check_mmrm_two_way() are:
#     - Formula: heatwave_status (no * period interaction)
#     - Visit: sequential day index only (not three-slot per day)
#     - Plot 3: no period facet (heatwave phase only)


check_mmrm_one_way <- function(data_list, outcome_vars) {
  
  for (outcome_name in names(outcome_vars)) {
    cat("\n", strrep("=", 60), "\n")
    cat("Residual diagnostics:", outcome_name, "\n")
    cat(strrep("=", 60), "\n\n")
    
    data        <- data_list[[outcome_vars[[outcome_name]]$data]]
    outcome_var <- outcome_vars[[outcome_name]]$var
    
    if (!(outcome_var %in% names(data))) {
      warning(paste("Variable", outcome_var, "not found in",
                    outcome_vars[[outcome_name]]$data))
      next
    }
    
    # Prepare data: identical to lmm_one_way() 
    # visit = sequential day index only (no period dimension for daily outcomes)
    filtered_data <- data %>%
      filter(heatwave_status %in% c("BEF_HW_June", "HW_June", "AFT_HW_June")) %>%
      mutate(
        heatwave_status = factor(heatwave_status,
                                 levels = c("BEF_HW_June", "HW_June", "AFT_HW_June")),
        participant_id  = as.factor(participant_id),
        visit           = as.integer(as.factor(date_converted))
      )
    filtered_data$visit           <- as.factor(filtered_data$visit)
    filtered_data$heatwave_status <- factor(filtered_data$heatwave_status,
                                            levels = c("BEF_HW_June", "HW_June", "AFT_HW_June"))
    
    # Fit MMRM (same specification as lmm_one_way) 
    formula_full <- reformulate(
      termlabels = c("heatwave_status", "ar1(visit | participant_id)"),
      response   = outcome_var
    )
    model_full <- mmrm(formula_full, data = filtered_data,
                       control = mmrm_control(method = "Kenward-Roger"))
    
    # Extract residuals and fitted values
    residuals_data <- data.frame(
      fitted          = fitted(model_full),
      residuals       = residuals(model_full),
      participant_id  = filtered_data$participant_id[!is.na(fitted(model_full))],
      heatwave_status = filtered_data$heatwave_status[!is.na(fitted(model_full))]
    )
    
    residuals_data$std_residuals          <- residuals_data$residuals /
      sd(residuals_data$residuals, na.rm = TRUE)
    residuals_data$abs_residuals          <- abs(residuals_data$residuals)
    residuals_data$sqrt_abs_std_residuals <- sqrt(abs(residuals_data$std_residuals))
    
    # Plot 1: Residuals vs Fitted 
    p1 <- ggplot(residuals_data, aes(x = fitted, y = residuals)) +
      geom_point(alpha = 0.6) +
      geom_smooth(method = "loess", se = FALSE, color = "red") +
      geom_hline(yintercept = 0, linetype = "dashed") +
      labs(title = "Residuals vs Fitted Values",
           x = "Fitted Values", y = "Residuals") +
      theme_minimal()
    print(p1)
    
    # Plot 2: Q-Q Plot 
    p2 <- ggplot(residuals_data, aes(sample = residuals)) +
      stat_qq() +
      stat_qq_line() +
      labs(title = "Q-Q Plot of Residuals",
           subtitle = "Points should follow the diagonal line") +
      theme_minimal()
    print(p2)
    
    # Plot 3: Residuals by heatwave phase
    # No period facet for one-way (daily) outcomes
    p3 <- ggplot(residuals_data, aes(x = heatwave_status, y = residuals)) +
      geom_boxplot() +
      geom_hline(yintercept = 0, linetype = "dashed") +
      labs(title = "Residuals by Heatwave Phase",
           x = "Heatwave Status", y = "Residuals") +
      theme_minimal()
    print(p3)
    
    # Plot 4: Scale-Location 
    p4 <- ggplot(residuals_data, aes(x = fitted, y = sqrt_abs_std_residuals)) +
      geom_point(alpha = 0.6) +
      geom_smooth(method = "loess", se = FALSE, color = "red") +
      labs(title = "Scale-Location Plot",
           subtitle = "Check for heteroskedasticity -- LOESS line should be flat",
           x = "Fitted Values", y = "sqrt(|Standardized Residuals|)") +
      theme_minimal()
    print(p4)
    
    # Plot 5: Residuals vs Observation Order
    residuals_data$observation_order <- seq_len(nrow(residuals_data))
    p5 <- ggplot(residuals_data, aes(x = observation_order, y = residuals)) +
      geom_point(alpha = 0.6) +
      geom_smooth(method = "loess", se = FALSE, color = "red") +
      geom_hline(yintercept = 0, linetype = "dashed") +
      labs(title = "Residuals vs Observation Order",
           subtitle = "Check for temporal patterns not captured by AR(1)",
           x = "Observation Order", y = "Residuals") +
      theme_minimal()
    print(p5)
    
    # Plot 6: Histogram of Residuals 
    p6 <- ggplot(residuals_data, aes(x = residuals)) +
      geom_histogram(bins = 30, alpha = 0.7, fill = "skyblue", color = "black") +
      geom_vline(xintercept = 0, linetype = "dashed", color = "red") +
      labs(title = "Distribution of Residuals",
           x = "Residuals", y = "Frequency") +
      theme_minimal()
    print(p6)
    
    # Plot 7: Residuals by Participant
    set.seed(123)
    unique_participants <- unique(residuals_data$participant_id)
    if (length(unique_participants) > 20) {
      sample_participants <- sample(unique_participants, 20)
      residuals_sample    <- residuals_data[
        residuals_data$participant_id %in% sample_participants, ]
    } else {
      residuals_sample <- residuals_data
    }
    
    p7 <- ggplot(residuals_sample, aes(x = factor(participant_id), y = residuals)) +
      geom_boxplot() +
      geom_hline(yintercept = 0, linetype = "dashed") +
      labs(title = "Residuals by Participant",
           subtitle = "Random sample of up to 20 participants shown",
           x = "Participant ID", y = "Residuals") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
    print(p7)
    
    cat("\nEnd of residual diagnostics for", outcome_name, "\n")
  }
}
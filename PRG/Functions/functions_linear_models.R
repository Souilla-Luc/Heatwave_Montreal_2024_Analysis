# =============================================================================
# FILE:    PRG/Functions/functions_linear_models.R
# PURPOSE: Statistical modelling functions for the HeatSuite 2024 study.
#          Provides Mixed Model Repeated Measures (MMRM), Cumulative Link
#          Mixed Models (CLMM), and Generalized Linear Mixed Models (glmmTMB)
#          for continuous, ordinal, and binary outcomes respectively.
#
# FUNCTIONS:
#   MMRM continuous outcomes (mmrm package):
#     lmm_two_way()         Two-factor MMRM: heatwave_status * period
#     lmm_two_way_AC()      Two-factor MMRM: heatwave_status * AC
#     lmm_one_way()         One-factor MMRM: heatwave_status only
#     lmm_one_way_AC()      One-factor MMRM: heatwave_status * AC
#     lmm_recovery()        Day-by-day recovery MMRM (Post_D1 to Post_D7)
#
#   CLMM ordinal outcomes (ordinal package):
#     ordinal_mm()          Two-factor CLMM: heatwave_status * period
#     ordinal_mm_AC()       Two-factor CLMM: heatwave_status * AC
#     clmm_recovery()       Day-by-day recovery CLMM
#
#   glmmTMB binary outcomes (glmmTMB + marginaleffects packages):
#     glmmTMB_model()       Two-factor GLMM: heatwave_status * period
#     glmmTMB_model_AC()    Two-factor GLMM: heatwave_status * AC
#     glmmTMB_recovery()    Day-by-day recovery GLMM
#
# STATISTICAL APPROACH (all MMRM functions):
#   - Covariance structure: AR(1) via ar1(visit | participant_id)
#     Measurements closer in time are assumed to be more correlated than
#     those farther apart. More realistic than compound symmetry (ANOVA).
#   - Degrees of freedom: Kenward-Roger via mmrm_control(method = "Kenward-Roger")
#     Provides accurate df for small samples and unbalanced designs.
#   - Fixed effect tests: car::Anova(model, type = "II")
#     Type II is preferred over Type III when no interaction is present;
#     gives more power for each main effect.
#   - Post-hoc comparisons: emmeans() with mvt adjustment
#     Multivariate t-distribution controls Type I error for correlated data.
#   - Contrast order (all functions): HW vs BEF, AFT vs HW, AFT vs BEF
#     Coefficient vectors: c(-1, 1, 0) / c(0, -1, 1) / c(-1, 0, 1)
#     correspond to factor levels: BEF_HW_June (pos 1), HW_June (pos 2),
#     AFT_HW_June (pos 3)
#
# STATISTICAL APPROACH (CLMM functions):
#   - ordinal package clmm() with proportional odds assumption
#   - mode = "mean.class" in emmeans() is appropriate for ordinal scales:
#     returns the expected response class (mean of the ordinal scale)
#   - Profile likelihood confidence intervals via confint(method = "profile")
#
# STATISTICAL APPROACH (glmmTMB functions):
#   - glmmTMB() with family = binomial and AR(1) temporal correlation
#   - Post-hoc: marginaleffects::avg_comparisons() with comparison = "lnratioavg"
#     and transform = exp to produce population-averaged adjusted risk ratios
#   - WHY marginaleffects instead of emmeans: for nonlinear link functions
#     (logit), contrasts on the log-odds scale do not directly translate to
#     risk ratios. avg_comparisons() averages over the random effects
#     distribution to produce marginal (population-averaged) RRs on the
#     response (probability) scale -- more interpretable for binary outcomes.
#
# VISIT VARIABLE CONSTRUCTION:
#   Two-factor (heatwave x period): visit = (day_index - 1) * 3 + period_index
#     Creates a unique sequential integer per AM/PM/NI slot across all days:
#     Day 1 AM=1, Day 1 PM=2, Day 1 NI=3, Day 2 AM=4, ...
#   One-factor (heatwave only): visit = as.integer(as.factor(date_converted))
#     Creates a sequential integer per day: Day 1=1, Day 2=2, ...
# =============================================================================


# =============================================================================
# Function 1: lmm_two_way()
# =============================================================================
#
# PURPOSE:
#   Fits a Mixed Model Repeated Measures (MMRM) with two fixed factors:
#   heatwave_status (BEF / HW / AFT) and period of day (AM / PM / NI),
#   including their interaction. Uses AR(1) covariance structure across
#   visit (AM/PM/NI slots ordered sequentially across days).
#
# POST-HOC:
#   Main effect of heatwave_status: custom contrasts (HW-BEF, AFT-HW, AFT-BEF)
#   Main effect of period: all pairwise comparisons (AM-PM, AM-NI, PM-NI)
#   Both use mvt adjustment for Type I error control.
#   Interaction contrasts (heatwave_status | period) are available but
#   commented out uncomment if the interaction term is significant.

lmm_two_way <- function(data_list, outcome_vars, posthoc = TRUE) {
  
  for (outcome_name in names(outcome_vars)) {
    cat("\n", strrep("=", 60), "\n")
    cat("Analysis:", outcome_name, "\n")
    cat(strrep("=", 60), "\n\n")
    
    data        <- data_list[[outcome_vars[[outcome_name]]$data]]
    outcome_var <- outcome_vars[[outcome_name]]$var
    
    if (!(outcome_var %in% names(data))) {
      warning(paste("Variable", outcome_var, "not found in",
                    outcome_vars[[outcome_name]]$data))
      next
    }
    
    # Prepare data: filter, factor, and create the visit index
    # visit encodes each AM/PM/NI slot as a unique sequential integer across
    # all study days, required by ar1() for temporal ordering
    filtered_data <- data %>%
      filter(heatwave_status %in% c("BEF_HW_June", "HW_June", "AFT_HW_June")) %>%
      mutate(
        period          = factor(period, levels = c("AM", "PM", "NI")),
        heatwave_status = factor(heatwave_status,
                                 levels = c("BEF_HW_June", "HW_June", "AFT_HW_June")),
        participant_id  = as.factor(participant_id),
        day_index       = as.integer(as.factor(date_converted)), # Day 1=1, Day 2=2, ...
        period_index    = as.integer(period),                     # AM=1, PM=2, NI=3
        visit           = ((day_index - 1) * 3) + period_index   # unique slot per AM/PM/NI per day
      )
    filtered_data$visit <- as.factor(filtered_data$visit)
    
    # Fit MMRM with heatwave_status * period interaction and AR(1)
    formula_full <- reformulate(
      termlabels = c("heatwave_status * period", "ar1(visit | participant_id)"),
      response   = outcome_var
    )
    print(formula_full)
    model <- mmrm(formula_full, data = filtered_data,
                  control = mmrm_control(method = "Kenward-Roger"))
    
    cat("\nModel summary:\n")
    print(summary(model))
    
    # Type II ANOVA: tests each effect controlling for the others
    cat("\nANOVA Table (Type II, Kenward-Roger df):\n")
    print(car::Anova(model, type = "II"))
    
    # Post-hoc comparisons
    if (posthoc) {
      
      # Main effect of heatwave_status (marginalised over period)
      # Proportional weighting accounts for any imbalance across period cells
      cat("\nPost-hoc: main effect of heatwave_status\n")
      emm_heatwave <- emmeans(model, ~ heatwave_status,
                              weights = "proportional",
                              mode    = "kenward-roger")
      
      # Custom contrasts in publication order:
      #   c(-1, 1, 0) = HW - BEF  (heatwave vs pre-heatwave)
      #   c( 0,-1, 1) = AFT - HW  (recovery vs heatwave)
      #   c(-1, 0, 1) = AFT - BEF (recovery vs pre-heatwave)
      # Vector positions correspond to factor levels: BEF(1), HW(2), AFT(3)
      posthoc_hw <- contrast(
        emm_heatwave,
        method = list(
          "Heatwave vs. pre-heatwave" = c(-1,  1,  0),
          "Recovery vs. heatwave"     = c( 0, -1,  1),
          "Recovery vs. pre-heatwave" = c(-1,  0,  1)
        ),
        adjust = "mvt"
      )
      print(posthoc_hw)
      cat("\n95% Confidence intervals for heatwave_status contrasts:\n")
      print(confint(posthoc_hw))
      
      # Main effect of period of day (marginalised over heatwave_status)
      cat("\nPost-hoc: main effect of period\n")
      emm_period    <- emmeans(model, ~ period,
                               weights = "proportional",
                               mode    = "kenward-roger")
      posthoc_period <- pairs(emm_period, adjust = "mvt")
      print(posthoc_period)
      cat("\n95% Confidence intervals for period contrasts:\n")
      print(confint(posthoc_period))
      
      # Interaction contrasts (heatwave_status within each period) are
      # available but commented out. Uncomment if the interaction term
      # in the ANOVA table is statistically significant:
      # emm_inter  <- emmeans(model, ~ heatwave_status | period)
      # contrast_inter <- pairs(emm_inter, adjust = "mvt")
      # print(contrast_inter); print(confint(contrast_inter))
    }
    cat("\nEnd of analysis for", outcome_name, "\n")
  }
}


# =============================================================================
# Function 2: lmm_two_way_AC()
# =============================================================================
#
# PURPOSE:
#   Fits a two-factor MMRM with heatwave_status * AC as fixed effects.
#   Identical structure to lmm_two_way() but replaces period with AC status
#   to assess whether air conditioning access moderates heatwave effects.
#
# POST-HOC:
#   Main effect of heatwave_status, main effect of AC, and interaction
#   (AC effect within each heatwave period).


lmm_two_way_AC <- function(data_list, outcome_vars, posthoc = TRUE) {
  
  for (outcome_name in names(outcome_vars)) {
    cat("\n", strrep("=", 60), "\n")
    cat("Analysis:", outcome_name, "\n")
    cat(strrep("=", 60), "\n\n")
    
    data        <- data_list[[outcome_vars[[outcome_name]]$data]]
    outcome_var <- outcome_vars[[outcome_name]]$var
    
    if (!(outcome_var %in% names(data))) {
      warning(paste("Variable", outcome_var, "not found in",
                    outcome_vars[[outcome_name]]$data))
      next
    }
    
    filtered_data <- data %>%
      filter(heatwave_status %in% c("BEF_HW_June", "HW_June", "AFT_HW_June")) %>%
      mutate(
        period          = factor(period, levels = c("AM", "PM", "NI")),
        heatwave_status = factor(heatwave_status,
                                 levels = c("BEF_HW_June", "HW_June", "AFT_HW_June")),
        participant_id  = as.factor(participant_id),
        AC              = factor(AC, levels = c(0, 1), labels = c("NoAC", "AC")),
        day_index       = as.integer(as.factor(date_converted)),
        period_index    = as.integer(period),
        visit           = ((day_index - 1) * 3) + period_index
      )
    filtered_data$visit <- as.factor(filtered_data$visit)
    
    # Model includes heatwave_status * AC interaction
    # Period is omitted here use lmm_two_way() to include period effects
    formula_full <- reformulate(
      termlabels = c("heatwave_status * AC", "ar1(visit | participant_id)"),
      response   = outcome_var
    )
    print(formula_full)
    model <- mmrm(formula_full, data = filtered_data,
                  control = mmrm_control(method = "Kenward-Roger"))
    
    cat("\nModel summary:\n")
    print(summary(model))
    
    cat("\nANOVA Table (Type II, Kenward-Roger df):\n")
    print(car::Anova(model, type = "II"))
    
    if (posthoc) {
      
      cat("\nPost-hoc: main effect of heatwave_status\n")
      emm_heatwave <- emmeans(model, ~ heatwave_status)
      posthoc_hw <- contrast(
        emm_heatwave,
        method = list(
          "Heatwave vs. pre-heatwave" = c(-1,  1,  0),
          "Recovery vs. heatwave"     = c( 0, -1,  1),
          "Recovery vs. pre-heatwave" = c(-1,  0,  1)
        ),
        adjust = "mvt"
      )
      print(posthoc_hw)
      cat("\n95% Confidence intervals for heatwave_status contrasts:\n")
      print(confint(posthoc_hw))
      
      cat("\nPost-hoc: main effect of AC\n")
      emm_AC     <- emmeans(model, ~ AC)
      posthoc_AC <- pairs(emm_AC, adjust = "mvt")
      print(posthoc_AC)
      cat("\n95% Confidence intervals for AC contrasts:\n")
      print(confint(posthoc_AC))
      
      # Interaction: AC effect within each heatwave period
      cat("\nPost-hoc: AC effect within each heatwave period\n")
      emm_ac_by_hw  <- emmeans(model, ~ AC | heatwave_status)
      ac_contrasts   <- pairs(emm_ac_by_hw, adjust = "mvt")
      print(ac_contrasts)
      cat("\n95% Confidence intervals for interaction contrasts:\n")
      print(confint(ac_contrasts))
    }
    cat("\nEnd of analysis for", outcome_name, "\n")
  }
}


# =============================================================================
# Function 3: lmm_one_way()
# =============================================================================
#
# PURPOSE:
#   Fits a one-factor MMRM with heatwave_status as the only fixed effect.
#   Used for outcomes measured once per day (body mass, urine frequency,
#   physical activity) where there is no period-of-day dimension.
#   visit = sequential day index (Day 1=1, Day 2=2, ...).
#

lmm_one_way <- function(data_list, outcome_variables, posthoc = TRUE) {
  
  for (outcome_name in names(outcome_variables)) {
    cat("\n", strrep("=", 60), "\n")
    cat("Analysis:", outcome_name, "\n")
    cat(strrep("=", 60), "\n\n")
    
    data        <- data_list[[outcome_variables[[outcome_name]]$data]]
    outcome_var <- outcome_variables[[outcome_name]]$var
    
    if (!(outcome_var %in% names(data))) {
      warning(paste("Variable", outcome_var, "not found in",
                    outcome_variables[[outcome_name]]$data))
      next
    }
    
    # For daily outcomes, visit is simply the sequential day number
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
    
    formula_full <- reformulate(
      termlabels = c("heatwave_status", "ar1(visit | participant_id)"),
      response   = outcome_var
    )
    print(formula_full)
    model <- mmrm(formula_full, data = filtered_data,
                  control = mmrm_control(method = "Kenward-Roger"))
    
    cat("\nModel summary:\n")
    print(summary(model))
    cat("\n95% Confidence intervals for model coefficients (profile likelihood):\n")
    print(confint(model, method = "profile"))
    
    cat("\nANOVA Table (Type II, Kenward-Roger df):\n")
    print(car::Anova(model, type = "II"))
    
    if (posthoc) {
      cat("\nPost-hoc: main effect of heatwave_status\n")
      emm_heatwave <- emmeans(model, ~ heatwave_status)
      contrasts_hw <- contrast(
        emm_heatwave,
        method = list(
          "Heatwave vs. pre-heatwave" = c(-1,  1,  0),
          "Recovery vs. heatwave"     = c( 0, -1,  1),
          "Recovery vs. pre-heatwave" = c(-1,  0,  1)
        ),
        adjust = "mvt"
      )
      print(contrasts_hw)
      cat("\n95% Confidence intervals for heatwave_status contrasts:\n")
      print(confint(contrasts_hw))
    }
    cat("\nEnd of analysis for", outcome_name, "\n")
  }
}


# =============================================================================
# Function 4: lmm_one_way_AC()
# =============================================================================
#
# PURPOSE:
#   One-factor MMRM including AC as a moderator for daily outcomes.
#   Model: outcome ~ heatwave_status * AC + ar1(visit | participant_id)
#

lmm_one_way_AC <- function(data_list, outcome_vars, posthoc = TRUE) {
  
  for (outcome_name in names(outcome_vars)) {
    cat("\n", strrep("=", 60), "\n")
    cat("Analysis:", outcome_name, "\n")
    cat(strrep("=", 60), "\n\n")
    
    data        <- data_list[[outcome_vars[[outcome_name]]$data]]
    outcome_var <- outcome_vars[[outcome_name]]$var
    
    if (!(outcome_var %in% names(data))) {
      warning(paste("Variable", outcome_var, "not found in",
                    outcome_vars[[outcome_name]]$data))
      next
    }
    
    filtered_data <- data %>%
      filter(heatwave_status %in% c("BEF_HW_June", "HW_June", "AFT_HW_June")) %>%
      mutate(
        participant_id  = as.factor(participant_id),
        AC              = factor(AC, levels = c(0, 1), labels = c("NoAC", "AC")),
        visit           = as.integer(as.factor(date_converted))
      )
    filtered_data$visit           <- as.factor(filtered_data$visit)
    filtered_data$heatwave_status <- factor(filtered_data$heatwave_status,
                                            levels = c("BEF_HW_June", "HW_June", "AFT_HW_June"))
    
    formula_full <- reformulate(
      termlabels = c("heatwave_status * AC", "ar1(visit | participant_id)"),
      response   = outcome_var
    )
    print(formula_full)
    model <- mmrm(formula_full, data = filtered_data,
                  control = mmrm_control(method = "Kenward-Roger"))
    
    cat("\nModel summary:\n")
    print(summary(model))
    
    cat("\nANOVA Table (Type II, Kenward-Roger df):\n")
    print(car::Anova(model, type = "II"))
    
    if (posthoc) {
      cat("\nPost-hoc: main effect of heatwave_status\n")
      emm_heatwave <- emmeans(model, ~ heatwave_status)
      contrasts_hw <- contrast(
        emm_heatwave,
        method = list(
          "Heatwave vs. pre-heatwave" = c(-1,  1,  0),
          "Recovery vs. heatwave"     = c( 0, -1,  1),
          "Recovery vs. pre-heatwave" = c(-1,  0,  1)
        ),
        adjust = "mvt"
      )
      print(contrasts_hw)
      cat("\n95% Confidence intervals for heatwave_status contrasts:\n")
      print(confint(contrasts_hw))
      
      cat("\nPost-hoc: main effect of AC\n")
      emm_AC     <- emmeans(model, ~ AC)
      posthoc_AC <- pairs(emm_AC, adjust = "mvt")
      print(posthoc_AC)
      cat("\n95% Confidence intervals for AC contrasts:\n")
      print(confint(posthoc_AC))
    }
    cat("\nEnd of analysis for", outcome_name, "\n")
  }
}


# =============================================================================
# Function 5: lmm_recovery()
# =============================================================================
#
# PURPOSE:
#   Fits a day-by-day recovery MMRM to assess how quickly each outcome
#   returns to pre-heatwave baseline levels following the heatwave.
#
# DAY LABEL CONSTRUCTION:
#   - BEF_HW_June observations are pooled as "Baseline"
#   - HW_June observations are labelled "HW"
#   - AFT_HW_June observations are labelled "Post_D1" through "Post_D7"
#     where the day number = date_converted - 2024-06-20 (end of heatwave)
#
# MODEL: outcome ~ day_label + ar1(visit | participant_id)
#   Kenward-Roger df. Post-hoc contrasts: each Post_D vs Baseline.


lmm_recovery <- function(data_list, outcome_variables) {
  
  for (outcome_name in names(outcome_variables)) {
    cat("\n", strrep("=", 60), "\n")
    cat("Recovery analysis:", outcome_name, "\n")
    cat(strrep("=", 60), "\n\n")
    
    data        <- data_list[[outcome_variables[[outcome_name]]$data]]
    outcome_var <- outcome_variables[[outcome_name]]$var
    
    if (!(outcome_var %in% names(data))) {
      warning(paste("Variable", outcome_var, "not found"))
      next
    }
    
    # ── Prepare data: create day_label for each study phase ──────────────────
    # BEF = "Baseline" (pooled), HW = "HW" (pooled),
    # AFT = "Post_D1" ... "Post_D7" (individual days)
    # Post_D number = days elapsed since end of heatwave (2024-06-20)
    filtered_data <- data %>%
      filter(heatwave_status %in% c("BEF_HW_June", "HW_June", "AFT_HW_June")) %>%
      mutate(
        participant_id  = as.factor(participant_id),
        heatwave_status = factor(heatwave_status,
                                 levels = c("BEF_HW_June", "HW_June", "AFT_HW_June")),
        day_label = case_when(
          heatwave_status == "BEF_HW_June" ~ "Baseline",
          heatwave_status == "HW_June"     ~ "HW",
          heatwave_status == "AFT_HW_June" ~ paste0("Post_D",
                                                    as.integer(as.Date(date_converted) - as.Date("2024-06-20")))
        ),
        day_label = factor(day_label,
                           levels = c("Baseline", "HW", paste0("Post_D", 1:7))),
        visit = as.factor(as.integer(as.factor(date_converted)))
      )
    
    formula_recovery <- reformulate(
      termlabels = c("day_label", "ar1(visit | participant_id)"),
      response   = outcome_var
    )
    
    # tryCatch ensures the loop continues if a model fails to converge
    model <- tryCatch(
      mmrm(formula_recovery, data = filtered_data,
           control = mmrm_control(method = "Kenward-Roger")),
      error = function(e) {
        message("Model failed for ", outcome_name, ": ", conditionMessage(e))
        return(NULL)
      }
    )
    if (is.null(model)) next
    
    # Contrasts: each post-heatwave day vs Baseline
    # Only days that appear in the filtered data are included
    cat("\nPost-hoc: each post-heatwave day vs Baseline\n")
    emm       <- emmeans(model, ~ day_label, mode = "kenward-roger",
                         weights = "proportional")
    post_days <- paste0("Post_D", 1:7)
    post_days <- post_days[post_days %in% levels(filtered_data$day_label)]
    
    # Build one contrast vector per post-heatwave day
    contrast_list <- setNames(
      lapply(post_days, function(d) {
        lvls <- levels(filtered_data$day_label)
        v    <- rep(0, length(lvls))
        v[lvls == "Baseline"] <- -1
        v[lvls == d]          <-  1
        v
      }),
      paste(post_days, "vs Baseline")
    )
    
    contrasts_recovery <- contrast(emm, method = contrast_list, adjust = "mvt")
    cat("\nContrasts (each post-heatwave day vs Baseline):\n")
    print(contrasts_recovery)
    cat("\n95% Confidence intervals:\n")
    print(confint(contrasts_recovery))
    
    cat("\nEnd of recovery analysis for", outcome_name, "\n")
  }
}

# =============================================================================
# CLMM FUNCTIONS -- ORDINAL OUTCOMES
# =============================================================================

# =============================================================================
# Function 6: ordinal_mm()
# =============================================================================
#
# PURPOSE:
#   Fits a Cumulative Link Mixed Model (CLMM) for ordinal survey outcomes
#   using the ordinal package. The model includes heatwave_status * period
#   as fixed effects and a random intercept per participant.
#
# WHY CLMM:
#   Ordinal outcomes (e.g. thermal sensation: -1/0/1, sleep quality: 1-5)
#   violate the normality and linearity assumptions of MMRM. CLMM links a
#   latent continuous variable to the observed ordinal categories via
#   threshold parameters and a logit link (proportional odds model).
#
# POST-HOC:
#   emmeans() with mode = "mean.class" returns the expected response class
#   (mean of the ordinal scale), appropriate for ordinal outcomes.
#   mvt adjustment controls Type I error across multiple comparisons.
#

ordinal_mm <- function(data_list, outcome_variables) {
  
  for (outcome_name in names(outcome_variables)) {
    cat("\n", strrep("=", 60), "\n")
    cat("Analysis:", outcome_name, "\n")
    cat(strrep("=", 60), "\n\n")
    
    outcome_data <- outcome_variables[[outcome_name]]
    data_name    <- outcome_data$data
    response_var <- outcome_data$var
    data         <- data_list[[data_name]]
    
    # Convert response to an ordered factor with the specified level ordering
    data$value <- ordered(data[[response_var]], levels = outcome_data$levels)
    
    filtered_data <- data %>%
      filter(heatwave_status %in% c("BEF_HW_June", "HW_June", "AFT_HW_June")) %>%
      mutate(
        heatwave_status = factor(heatwave_status,
                                 levels = c("BEF_HW_June", "HW_June", "AFT_HW_June")),
        participant_id  = as.factor(participant_id)
      )
    filtered_data$heatwave_status <- factor(filtered_data$heatwave_status,
                                            levels = c("BEF_HW_June", "HW_June", "AFT_HW_June"))
    filtered_data$value <- ordered(data[[response_var]], levels = outcome_data$levels)
    
    # CLMM: proportional odds model with random intercept per participant
    # period may be removed from the formula for daily (one-factor) outcomes
    model_formula <- value ~ heatwave_status * period + (1 | participant_id)
    model         <- clmm(model_formula, data = filtered_data)
    
    cat("\nModel summary:\n")
    print(summary(model))
    cat("\n95% Confidence intervals (profile likelihood):\n")
    print(confint(model, method = "profile"))
    
    cat("\nANOVA Table (Type II):\n")
    print(car::Anova(model, type = "II"))
    
    # Main effect of heatwave_status
    # mode = "mean.class" returns expected ordinal response class
    cat("\nPost-hoc: main effect of heatwave_status\n")
    emm_heatwave <- emmeans(model, ~ heatwave_status, mode = "mean.class")
    contrasts_hw <- contrast(
      emm_heatwave,
      method = list(
        "Heatwave vs. pre-heatwave" = c(-1,  1,  0),
        "Recovery vs. heatwave"     = c( 0, -1,  1),
        "Recovery vs. pre-heatwave" = c(-1,  0,  1)
      ),
      adjust = "mvt"
    )
    print(contrasts_hw)
    cat("\n95% Confidence intervals for heatwave_status contrasts:\n")
    print(confint(contrasts_hw))
    
    cat("\nPost-hoc: main effect of period\n")
    emm_period     <- emmeans(model, ~ period, mode = "mean.class")
    posthoc_period <- pairs(emm_period, adjust = "mvt")
    print(posthoc_period)
    cat("\n95% Confidence intervals for period contrasts:\n")
    print(confint(posthoc_period))
    
    # Interaction contrasts available if interaction is significant:
    # emm_inter <- emmeans(model, ~ heatwave_status | period, mode = "mean.class")
    # print(pairs(emm_inter, adjust = "mvt"))
  }
  cat("\nEnd of analysis for", outcome_name, "\n")
}


# =============================================================================
# Function 7: ordinal_mm_AC()
# =============================================================================
#
# PURPOSE:
#   CLMM with heatwave_status * AC as fixed effects. Assesses whether
#   air conditioning access moderates ordinal perceptual responses.

ordinal_mm_AC <- function(data_list, outcome_variables) {
  
  for (outcome_name in names(outcome_variables)) {
    cat("\n", strrep("=", 60), "\n")
    cat("Analysis:", outcome_name, "\n")
    cat(strrep("=", 60), "\n\n")
    
    outcome_data <- outcome_variables[[outcome_name]]
    data_name    <- outcome_data$data
    response_var <- outcome_data$var
    data         <- data_list[[data_name]]
    
    data$value <- ordered(data[[response_var]], levels = outcome_data$levels)
    
    filtered_data <- data %>%
      filter(heatwave_status %in% c("BEF_HW_June", "HW_June", "AFT_HW_June")) %>%
      mutate(
        heatwave_status = factor(heatwave_status,
                                 levels = c("BEF_HW_June", "HW_June", "AFT_HW_June")),
        participant_id  = as.factor(participant_id),
        AC              = factor(AC, levels = c(0, 1), labels = c("NoAC", "AC"))
      )
    filtered_data$heatwave_status <- factor(filtered_data$heatwave_status,
                                            levels = c("BEF_HW_June", "HW_June", "AFT_HW_June"))
    filtered_data$value <- ordered(data[[response_var]], levels = outcome_data$levels)
    
    model_formula <- value ~ heatwave_status * AC + (1 | participant_id)
  
    model <- clmm(model_formula, data = filtered_data)
    
    cat("\nModel summary:\n")
    print(summary(model))
    cat("\n95% Confidence intervals (profile likelihood):\n")
    print(confint(model, method = "profile"))
    
    cat("\nANOVA Table (Type II):\n")
    print(car::Anova(model, type = "II"))
    
    cat("\nPost-hoc: main effect of heatwave_status\n")
    emm_heatwave     <- emmeans(model, ~ heatwave_status, mode = "mean.class")
    posthoc_heatwave <- pairs(emm_heatwave, adjust = "mvt")
    print(posthoc_heatwave)
    cat("\n95% Confidence intervals for heatwave_status contrasts:\n")
    print(confint(posthoc_heatwave))
    
    cat("\nPost-hoc: main effect of AC\n")
    emm_AC     <- emmeans(model, ~ AC)
    posthoc_AC <- pairs(emm_AC, adjust = "mvt")
    print(posthoc_AC)
    cat("\n95% Confidence intervals for AC contrasts:\n")
    print(confint(posthoc_AC))
  }
  # FIX: moved inside loop
  cat("\nEnd of analysis for", outcome_name, "\n")
}


# =============================================================================
# Function 8: clmm_recovery()
# =============================================================================
#
# PURPOSE:
#   Day-by-day recovery CLMM for ordinal outcomes. Compares each
#   post-heatwave day (Post_D1 to Post_D7) to the pre-heatwave baseline
#   using the same day_label construction as lmm_recovery().


clmm_recovery <- function(data_list, outcome_variables) {
  
  for (outcome_name in names(outcome_variables)) {
    cat("\n", strrep("=", 60), "\n")
    cat("Recovery analysis:", outcome_name, "\n")
    cat(strrep("=", 60), "\n\n")
    
    outcome_data <- outcome_variables[[outcome_name]]
    data_name    <- outcome_data$data
    response_var <- outcome_data$var
    data         <- data_list[[data_name]]
    
    data$value <- ordered(data[[response_var]], levels = outcome_data$levels)
    
    # Build day_label: Baseline (BEF pooled), HW (pooled), Post_D1...Post_D7
    filtered_data <- data %>%
      filter(heatwave_status %in% c("BEF_HW_June", "HW_June", "AFT_HW_June")) %>%
      mutate(
        participant_id = as.factor(participant_id),
        day_label = case_when(
          heatwave_status == "BEF_HW_June" ~ "Baseline",
          heatwave_status == "HW_June"     ~ "HW",
          heatwave_status == "AFT_HW_June" ~ paste0("Post_D",
                                                    as.integer(as.Date(date_converted) - as.Date("2024-06-20")))
        ),
        day_label = factor(day_label,
                           levels = c("Baseline", "HW", paste0("Post_D", 1:7)))
      )
    filtered_data$value <- ordered(data[[response_var]], levels = outcome_data$levels)
    
    # CLMM with period as a covariate to control for time-of-day effects
    model_formula <- value ~ day_label + period + (1 | participant_id)
    model <- tryCatch(
      clmm(model_formula, data = filtered_data),
      error = function(e) {
        message("Model failed for ", outcome_name, ": ", conditionMessage(e))
        return(NULL)
      }
    )
    if (is.null(model)) next
    
    cat("\nPost-hoc: each post-heatwave day vs Baseline\n")
    emm       <- emmeans(model, ~ day_label, mode = "mean.class")
    post_days <- paste0("Post_D", 1:7)
    post_days <- post_days[post_days %in% levels(filtered_data$day_label)]
    
    contrast_list <- setNames(
      lapply(post_days, function(d) {
        lvls <- levels(filtered_data$day_label)
        v    <- rep(0, length(lvls))
        v[lvls == "Baseline"] <- -1
        v[lvls == d]          <-  1
        v
      }),
      paste(post_days, "vs Baseline")
    )
    
    contrasts_recovery <- contrast(emm, method = contrast_list, adjust = "mvt")
    cat("\nContrasts (each post-heatwave day vs Baseline):\n")
    print(contrasts_recovery)
    cat("\n95% Confidence intervals:\n")
    print(confint(contrasts_recovery))
    
    cat("\nEnd of recovery analysis for", outcome_name, "\n")
  }
}


# =============================================================================
# glmmTMB FUNCTIONS -- BINARY OUTCOMES
# =============================================================================

# =============================================================================
# Function 9: glmmTMB_model()
# =============================================================================
#
# PURPOSE:
#   Fits a Generalized Linear Mixed Model (GLMM) with a binomial (logistic)
#   link for binary survey outcomes (comfort: 0/1, sweating: 0/1, at-home: 0/1).
#   Uses AR(1) temporal correlation structure within participants.
#
# WHY glmmTMB INSTEAD OF MMRM:
#   MMRM is designed for continuous Gaussian outcomes. Binary outcomes require
#   a nonlinear link function (logit). glmmTMB handles both the binary
#   distribution and AR(1) covariance in a single framework.
#
# WHY marginaleffects INSTEAD OF emmeans FOR POST-HOC:
#   For nonlinear models (logit), contrasts on the log-odds scale do not
#   directly translate to meaningful risk ratios. avg_comparisons() from
#   marginaleffects averages over the random effect distribution to produce
#   population-averaged adjusted Risk Ratios (RRs) on the probability scale.
#   comparison = "lnratioavg" computes log(mean(Y|X=a) / mean(Y|X=b))
#   which is then exponentiated via transform = exp to yield RR.
#   Reference: https://marginaleffects.com/bonus/logit.html
#
# DIAGNOSTICS (per model):
#   1. Convergence check: model$fit$convergence should equal 0
#   2. Fitting warnings: singularities, boundary issues
#   3. Random effect variances: check for near-zero variances
#   4. DHARMa residuals: simulation-based standardized residuals
#      - testDispersion(): overdispersion relative to model predictions
#      - testZeroInflation(): excess zeros relative to model expectations
#   5. R² values: conditional (with random effects) and marginal (fixed only)
#   6. Performance::check_model(): omnibus model diagnostic plot
#


glmmTMB_model <- function(data_list, outcome_variables) {
  
  # Load required packages should also be loaded in the calling script
  require(marginaleffects)
  require(DHARMa)
  require(performance)
  
  for (outcome_name in names(outcome_variables)) {
    cat("\n", strrep("=", 60), "\n")
    cat("Analysis:", outcome_name, "\n")
    cat(strrep("=", 60), "\n\n")
    
    outcome_data <- outcome_variables[[outcome_name]]
    data_name    <- outcome_data$data
    response_var <- outcome_data$var
    data         <- data_list[[data_name]]
    
    if (!(response_var %in% names(data))) {
      warning(paste("Variable", response_var, "not found in", data_name))
      next
    }
    
  
    data[[response_var]] <- factor(data[[response_var]])
    
    # Prepare filtered data with visit index for AR(1)
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
    
    # ar1(visit + 0 | participant_id): AR(1) correlation across visit slots
    # within each participant (no random intercept variance absorbed)
    formula_str   <- paste0(response_var,
                            " ~ heatwave_status * period + ar1(visit + 0 | participant_id)")
    model_formula <- as.formula(formula_str)
    model         <- glmmTMB(formula = model_formula, family = binomial,
                             data = filtered_data)
    
    # Diagnostic checks 
    cat("\nConvergence check (0 = converged):\n")
    print(model$fit$convergence)
    if (model$fit$convergence != 0) warning("Model did NOT converge!")
    
    cat("\nFitting warnings (boundary issues, singularities):\n")
    print(summary(model)$warnings)
    
    cat("\nRandom effect variances:\n")
    print(VarCorr(model))
    
    cat("\nResidual diagnostics (DHARMa simulation-based residuals):\n")
    # simulateResiduals() generates predictions from the fitted model and
    # computes standardized residuals. Ideal output: uniform distribution,
    # no patterns vs predicted values.
    sim_res <- simulateResiduals(model)
    plot(sim_res)
    testDispersion(sim_res)    # excess variance relative to model predictions
    testZeroInflation(sim_res) # excess zeros relative to model expectations
    # Temporal autocorrelation test (uncomment if residual patterns are observed):
    # testTemporalAutocorrelation(sim_res, time = filtered_data$visit,
    #                             group = filtered_data$participant_id)
    
    cat("\nR-squared (marginal = fixed effects only; conditional = fixed + random):\n")
    print(r2(model))
    
    cat("\nModel structure diagnostic (performance::check_model):\n")
    performance::check_model(model)
    
    # Model summary and ANOVA 
    cat("\nModel summary:\n")
    print(summary(model))
    
    cat("\nANOVA Table (Type II):\n")
    print(car::Anova(model, type = "II"))
    
    # Population-averaged adjusted Risk Ratios via marginaleffects 
    # avg_comparisons() with comparison = "lnratioavg" and transform = exp
    # produces exp(log(mean(Y|X=a) / mean(Y|X=b))) = RR on probability scale
    cat("\nPopulation-averaged adjusted Risk Ratios for heatwave_status:\n")
    rr_heatwave <- avg_comparisons(
      model,
      variables  = list(heatwave_status = "pairwise"),
      comparison = "lnratioavg",
      transform  = exp
    )
    print(rr_heatwave)
    
    cat("\nPopulation-averaged adjusted Risk Ratios for period:\n")
    rr_period <- avg_comparisons(
      model,
      variables  = list(period = "pairwise"),
      comparison = "lnratioavg",
      transform  = exp
    )
    print(rr_period)
  }
  cat("\nEnd of analysis for", outcome_name, "\n")
}


# =============================================================================
# Function 10: glmmTMB_model_AC()
# =============================================================================
#
# PURPOSE:
#   Binary GLMM with heatwave_status * AC as fixed effects. Assesses
#   whether air conditioning access moderates binary survey outcomes.
#


glmmTMB_model_AC <- function(data_list, outcome_variables) {
  
  require(marginaleffects)
  require(DHARMa)
  require(performance)
  
  for (outcome_name in names(outcome_variables)) {
    cat("\n", strrep("=", 60), "\n")
    cat("Analysis:", outcome_name, "\n")
    cat(strrep("=", 60), "\n\n")
    
    outcome_data <- outcome_variables[[outcome_name]]
    data_name    <- outcome_data$data
    response_var <- outcome_data$var
    data         <- data_list[[data_name]]
    
    if (!(response_var %in% names(data))) {
      warning(paste("Variable", response_var, "not found in", data_name))
      next
    }
    
   
    data[[response_var]] <- factor(data[[response_var]])
    
    filtered_data <- data %>%
      filter(heatwave_status %in% c("BEF_HW_June", "HW_June", "AFT_HW_June")) %>%
      mutate(
        period          = factor(period, levels = c("AM", "PM", "NI")),
        heatwave_status = factor(heatwave_status,
                                 levels = c("BEF_HW_June", "HW_June", "AFT_HW_June")),
        participant_id  = as.factor(participant_id),
        AC              = factor(AC, levels = c(0, 1), labels = c("NoAC", "AC")),
        day_index       = as.integer(as.factor(date_converted)),
        period_index    = as.integer(period),
        visit           = ((day_index - 1) * 3) + period_index
      )
    filtered_data$visit <- as.factor(filtered_data$visit)
    
    formula_str   <- paste0(response_var,
                            " ~ heatwave_status * AC + ar1(visit + 0 | participant_id)")
    model_formula <- as.formula(formula_str)
    model         <- glmmTMB(formula = model_formula, family = binomial,
                             data = filtered_data)
    
    cat("\nConvergence check (0 = converged):\n")
    print(model$fit$convergence)
    if (model$fit$convergence != 0) warning("Model did NOT converge!")
    
    cat("\nFitting warnings:\n")
    print(summary(model)$warnings)
    
    cat("\nRandom effect variances:\n")
    print(VarCorr(model))
    
    cat("\nDHARMa residual diagnostics:\n")
    sim_res <- simulateResiduals(model)
    plot(sim_res)
    testDispersion(sim_res)
    testZeroInflation(sim_res)
    
    cat("\nR-squared:\n")
    print(r2(model))
    
    cat("\nModel structure diagnostic:\n")
    performance::check_model(model)
    
    cat("\nModel summary:\n")
    print(summary(model))
    
    cat("\nANOVA Table (Type II):\n")
    print(car::Anova(model, type = "II"))
    
    cat("\nPopulation-averaged adjusted Risk Ratios for heatwave_status:\n")
    rr_heatwave <- avg_comparisons(
      model,
      variables  = list(heatwave_status = "pairwise"),
      comparison = "lnratioavg",
      transform  = exp
    )
    print(rr_heatwave)
    
    cat("\nPopulation-averaged adjusted Risk Ratios for AC:\n")
    rr_AC <- avg_comparisons(
      model,
      variables  = list(AC = "pairwise"),
      comparison = "lnratioavg",
      transform  = exp
    )
    print(rr_AC)
  }
  cat("\nEnd of analysis for", outcome_name, "\n")
}


# =============================================================================
# Function 11: glmmTMB_recovery()
# =============================================================================
#
# PURPOSE:
#   Day-by-day recovery analysis for binary outcomes. For each post-heatwave
#   day (Post_D1 to Post_D7), refits a separate model on the Baseline + that
#   day's data only, then computes the adjusted RR vs Baseline.
#
# NOTE ON REFITTING PER DAY:
#   Rather than using a single model with all days and post-hoc contrasts,
#   a separate model is fitted for each Baseline vs Post_Di comparison.
#   This avoids issues with near-empty cells when pooling all 7 post-heatwave
#   days into one model with a large day_label factor.
#


glmmTMB_recovery <- function(data_list, outcome_variables) {
  
  require(marginaleffects)
  
  for (outcome_name in names(outcome_variables)) {
    cat("\n", strrep("=", 60), "\n")
    cat("Recovery analysis:", outcome_name, "\n")
    cat(strrep("=", 60), "\n\n")
    
    data        <- data_list[[outcome_variables[[outcome_name]]$data]]
    outcome_var <- outcome_variables[[outcome_name]]$var
    
    if (!(outcome_var %in% names(data))) {
      warning(paste("Variable", outcome_var, "not found"))
      next
    }
    
    data[[outcome_var]] <- factor(data[[outcome_var]])
    
    # Build day_label: Baseline, HW, Post_D1...Post_D7
    filtered_data <- data %>%
      filter(heatwave_status %in% c("BEF_HW_June", "HW_June", "AFT_HW_June")) %>%
      mutate(
        participant_id = as.factor(participant_id),
        period         = factor(period, levels = c("AM", "PM", "NI")),
        day_label = case_when(
          heatwave_status == "BEF_HW_June" ~ "Baseline",
          heatwave_status == "HW_June"     ~ "HW",
          heatwave_status == "AFT_HW_June" ~ paste0("Post_D",
                                                    as.integer(as.Date(date_converted) - as.Date("2024-06-20")))
        ),
        day_label    = factor(day_label,
                              levels = c("Baseline", "HW", paste0("Post_D", 1:7))),
        day_index    = as.integer(as.factor(date_converted)),
        period_index = as.integer(period),
        visit        = as.factor(((day_index - 1) * 3) + period_index)
      )
    
    # Base formula  reused for each per-day contrast model
    formula_str <- paste0(outcome_var,
                          " ~ day_label + period + ar1(visit + 0 | participant_id)")
    
    # For each post-heatwave day, fit a Baseline vs Post_Di model 
    cat("\nAdjusted Risk Ratios -- each post-heatwave day vs Baseline:\n")
    
    post_days <- paste0("Post_D", 1:7)
    post_days <- post_days[post_days %in% levels(filtered_data$day_label)]
    
    for (d in post_days) {
      # Subset to Baseline + this specific post-heatwave day only
      filtered_data_contrast <- filtered_data %>%
        filter(day_label %in% c("Baseline", d)) %>%
        mutate(day_label = factor(day_label, levels = c("Baseline", d)))
      
      model_contrast <- tryCatch(
        glmmTMB(as.formula(formula_str), family = binomial,
                data = filtered_data_contrast),
        error = function(e) {
          message("Model failed for ", d, ": ", conditionMessage(e))
          return(NULL)
        }
      )
      if (is.null(model_contrast)) next
      
      rr <- avg_comparisons(
        model_contrast,
        variables  = list(day_label = "pairwise"),
        comparison = "lnratioavg",
        transform  = exp
      )
      cat("\n", d, "vs Baseline:\n")
      print(rr)
    }
    
    cat("\nEnd of recovery analysis for", outcome_name, "\n")
  }
}
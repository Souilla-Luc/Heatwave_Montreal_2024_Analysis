# =============================================================================
# FILE:    PRG/Functions/functions_check_clmm.R
# PURPOSE: Assumption checking for Cumulative Link Mixed Models (CLMM)
#          fitted in functions_linear_models.R.
#
# FUNCTION:
#   ordinal_mm_assumptions() -- Tests the proportional odds assumption for
#                               each fixed effect in the CLMM via likelihood
#                               ratio tests (LRT).
#
# THE PROPORTIONAL ODDS ASSUMPTION:
#   The CLMM used in this study (ordinal package, logit link) assumes
#   proportional odds: the effect of each predictor on the log-odds of
#   a higher response category is the same across all category thresholds.
#   For example, for thermal sensation (-1/0/1), the effect of heatwave_status
#   on the odds of "hot vs cold or neutral" must equal its effect on the odds
#   of "hot or neutral vs cold". If this assumption is violated, the single
#   coefficient estimated by the CLMM may misrepresent the relationship.
#
# TEST APPROACH:
#   For each fixed effect (heatwave_status, period):
#     1. Fit a proportional odds CLM (constrained: same effect at all thresholds)
#     2. Fit a non-proportional odds CLM (relaxed: effect allowed to vary per
#        threshold) using the nominal= argument of clm()
#     3. Compare the two models with a likelihood ratio test (LRT)
#        H0: proportional odds holds (models are equivalent)
#        Ha: proportional odds is violated (non-proportional fits better)
#
# INTERPRETATION:
#   p < 0.05: proportional odds assumption is VIOLATED for that variable.
#             Consider a partial proportional odds model or report with caution.
#   p >= 0.05: proportional odds assumption holds CLMM is appropriate.
#
# =============================================================================


# =============================================================================
# ordinal_mm_assumptions()
# =============================================================================
#
# PURPOSE:
#   For each outcome variable in outcome_variables, fits a proportional odds
#   CLM and tests the proportional odds assumption separately for
#   heatwave_status and period using likelihood ratio tests.
#

ordinal_mm_assumptions <- function(data_list, outcome_variables) {
  
  for (outcome_name in names(outcome_variables)) {
    cat("\n", strrep("=", 60), "\n")
    cat("Assumption check:", outcome_name, "\n")
    cat(strrep("=", 60), "\n\n")
    
    outcome_data <- outcome_variables[[outcome_name]]
    data_name    <- outcome_data$data
    response_var <- outcome_data$var
    data         <- data_list[[data_name]]
    
    # Convert response to ordered factor with the specified level ordering
    data$value <- ordered(data[[response_var]], levels = outcome_data$levels)
    
    # Filter to heatwave monitoring window and set factor levels
    filtered_data <- data %>%
      filter(heatwave_status %in% c("BEF_HW_June", "HW_June", "AFT_HW_June")) %>%
      mutate(
        heatwave_status = factor(heatwave_status,
                                 levels = c("BEF_HW_June", "HW_June", "AFT_HW_June")),
        participant_id  = as.factor(participant_id)
      )
    # Ensure ordered response is correctly set on filtered_data
    filtered_data$value <- ordered(data[[response_var]], levels = outcome_data$levels)
    
    # Step 1: Fit the constrained (proportional odds) CLM 
    # This is the same fixed-effect structure as the main CLMM in ordinal_mm(),
    # but without the random intercept (clm instead of clmm) because clmm()
    # does not support the nominal= argument needed for the relaxed model below.
    fixed_formula <- value ~ heatwave_status * period
    cat("Fitting proportional odds CLM (fixed effects only, no random intercept):\n")
    model_pom <- clm(fixed_formula, data = filtered_data)
    print(summary(model_pom))
    
    # Step 2: Test proportional odds for each fixed effect separately 
    # heatwave_status and period are tested one at a time.
    # Testing them jointly would confound which variable drives any violation.
    test_vars <- c("heatwave_status", "period")
    
    for (var in test_vars) {
      cat("\n--- Proportional odds test for:", var, "---\n")
      
      # The formula for both models is the same: value ~ heatwave_status * period
      # The ONLY difference is the nominal= argument:
      #   - model_pom: no nominal= argument -> proportional odds assumed
      #   - model_npom: nominal = ~ var -> that variable's effect is allowed
      #     to differ across response thresholds (non-proportional)
      nominal_formula <- value ~ heatwave_status * period
      nominal_arg     <- stats::formula(paste("~", var))
      
      model_npom <- clm(
        nominal_formula,
        data    = filtered_data,
        nominal = nominal_arg   # relaxes proportional odds for `var` only
      )
      
      #  Step 3: Likelihood ratio test comparing the two models
      # H0: proportional odds holds (the constrained model fits as well)
      # Ha: proportional odds is violated (non-proportional model fits better)
      # A significant p-value (< 0.05) indicates the assumption is violated.
      lr_test <- anova(model_pom, model_npom)
      print(lr_test)
      
      if (lr_test$`Pr(>Chisq)`[2] < 0.05) {
        cat("WARNING: Proportional odds assumption VIOLATED for:", var,
            "-- interpret CLMM results for this variable with caution.\n")
      } else {
        cat("OK: Proportional odds assumption holds for:", var,
            "-- CLMM is appropriate.\n")
      }
    }
  }
}
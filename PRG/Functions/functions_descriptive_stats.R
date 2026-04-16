# =============================================================================
# FILE:    PRG/Functions/functions_descriptive_stats.R
# PURPOSE: Descriptive statistics utilities for the HeatSuite 2024 study.
#          Provides functions to summarize baseline characteristics and
#          longitudinal monitoring data (physiological, environmental,
#          perceptual survey outcomes).
#
# FUNCTIONS -- BASELINE DATA:
#   1. calc_numerical_stats()            Summary stats for a numeric vector
#                                          (mean, SD, median, IQR, n)
#   2. calc_categorical_stats()          Counts and % for a categorical variable
#   3. calc_subcategory_stats()          Counts and % for binary indicator columns
#   4. calc_categorical_stats_multiple() Counts and % for semicolon-delimited
#                                          multi-response fields
#
# FUNCTIONS  MONITORING DATA:
#   5. create_survey_datasets()    Splits a long-format survey dataframe into
#                                    one dataframe per survey key
#   6. get_variable_summary()      Validates resp/value column mapping across
#                                    a list of survey datasets
#   7. descriptive_stats()         Main summary function; produces grouped
#                                    descriptive tables for all outcome variables
#   8. display_table()             Renders a dataframe as a styled HTML table
#
# UTILITY:
#   9. get_mode()                  Returns the mode (most frequent value)
#                                    of a vector; available for standalone use
#
# DEPENDENCIES:
#   dplyr      (group_by, summarize, filter, mutate, bind_rows, across, etc.)
#   stringr    (str_detect)
#   knitr      (kable)
#   kableExtra (kable_styling)
#   purrr      (used indirectly via bind_rows / lapply patterns)
#   These must be loaded in the calling script before sourcing this file.
# =============================================================================


# =============================================================================
# BASELINE DATA FUNCTIONS
# =============================================================================

# =============================================================================
# Function 1: calc_numerical_stats()
# =============================================================================
#
# PURPOSE:
#   Computes summary statistics for a single numeric vector and returns them
#   as a compact formatted string suitable for embedding in a baseline
#   characteristics table.
#
# NOTE:
#   A minimum of 3 non-missing values is required because quantile()
#   needs at least 3 observations to produce meaningful Q1 and Q3 estimates.

calc_numerical_stats <- function(data) {
  if (is.numeric(data) && sum(!is.na(data)) >= 3) {
    mean_val   <- round(mean(data,                       na.rm = TRUE), 2)
    sd_val     <- round(sd(data,                         na.rm = TRUE), 2)
    median_val <- round(median(data,                     na.rm = TRUE), 2)
    q1_val     <- round(quantile(data, 0.25,             na.rm = TRUE), 2)
    q3_val     <- round(quantile(data, 0.75,             na.rm = TRUE), 2)
    n_val      <- sum(!is.na(data))
    
    paste0(
      "Mean: ", mean_val, " (±", sd_val,
      ") [n=", n_val, "]; Median: ", median_val,
      " (Q1: ", q1_val, ", Q3: ", q3_val, ")"
    )
  } else {
    "Data is not numeric or insufficient for quartile calculation."
  }
}


# =============================================================================
# Function 2: calc_categorical_stats()
# =============================================================================
#
# PURPOSE:
#   Computes counts and percentages for each level of a categorical variable,
#   excluding missing values from the denominator. Returns both a compact
#   formatted string and the underlying frequency table.
#

calc_categorical_stats <- function(data, variable_name) {
  
  counts <- table(data, useNA = "ifany")
  
  # Exclude NA category from counts and percentages
  if ("<NA>" %in% names(counts)) {
    counts <- counts[names(counts) != "<NA>"]
  }
  
  # Percentages are computed on non-missing observations only
  total_count <- sum(counts)
  percentages <- round((counts / total_count) * 100, 2)
  
  result <- data.frame(
    Category   = names(counts),
    Count      = counts,
    Percentage = percentages
  )
  
  result$Formatted <- paste0(
    result$Category, ": ", result$Count, " (", result$Percentage, "%)"
  )
  
  formatted_result <- paste0(result$Formatted, collapse = "; ")
  
  # Append missing value count if any are present
  n_missing <- sum(is.na(data))
  if (n_missing > 0) {
    formatted_result <- paste0(formatted_result, "; Missing: ", n_missing)
  }
  
  return(list(Formatted = formatted_result, Table = result))
}


# =============================================================================
# Function 3: calc_subcategory_stats()
# =============================================================================
#
# PURPOSE:
#   Computes counts and percentages for a set of binary indicator columns,
#   where each column represents one subcategory (e.g. individual health
#   conditions coded as 0/1). Used for multi-select baseline variables
#   where each response is stored as a separate binary column.
#


calc_subcategory_stats <- function(data, columns, category_name) {
  
  if (!all(columns %in% names(data))) {
    stop("Some column names cannot be found in the dataframe.")
  }
  
  total_participants <- nrow(data)
  
  # Count the number of participants with a positive response (value > 0)
  # for each binary indicator column
  n_values    <- sapply(columns, function(col) sum(data[[col]] > 0, na.rm = TRUE))
  percentages <- round((n_values / total_participants) * 100, 2)
  
  result <- data.frame(
    Category    = category_name,
    Subcategory = columns,
    n           = n_values,
    Percentage  = percentages,
    Formatted   = paste0(n_values, " (", percentages, "%)")
  )
  
  return(result)
}


# =============================================================================
# Function 4: calc_categorical_stats_multiple()
# =============================================================================
#
# PURPOSE:
#   Parses fields where multiple responses are stored in a single cell
#   separated by a delimiter (default: ";"), then computes counts and
#   percentages for each unique response across all participants.
#
#
# DENOMINATOR CHOICE:
#   Percentages are computed relative to the number of participants
#   (num_participants), NOT relative to the total number of responses
#   selected (total_strategies, commented out). This means percentages
#   represent the proportion of participants who selected each response
#   at least once, and values can sum to more than 100%.
#   This is the standard approach for multi-select survey questions.
#

calc_categorical_stats_multiple <- function(data, separator = ";") {
  
  data <- as.character(data)
  data <- data[!is.na(data)]
  
  # Number of participants (rows) used as the denominator for percentages
  num_participants <- length(data)
  
  # Split each multi-response cell into individual responses and flatten
  split_data    <- strsplit(data, separator)
  flattened_data <- trimws(unlist(split_data))
  
  counts <- table(flattened_data)
  
  # Percentages based on number of participants who selected each response.
  # Values can sum to > 100% because participants may select multiple options.
  # Alternative: use total_strategies = sum(counts) as denominator to get
  # proportion of total selections (would always sum to 100%).
  percentages <- round((counts / num_participants) * 100, 2)
  
  result          <- paste0(names(counts), ": ", counts, " (", percentages, "%)")
  combined_result <- paste(result, collapse = "; ")
  
  return(combined_result)
}


# =============================================================================
# MONITORING DATA FUNCTIONS
# =============================================================================

# =============================================================================
# Function 5: create_survey_datasets()
# =============================================================================
#
# PURPOSE:
#   Splits a long-format survey dataframe (one row per response per key)
#   into a named list of dataframes, one per survey key. Used to separate
#   the seven perceptual survey dimensions before analysis.
#

create_survey_datasets <- function(data, survey_keys) {
  
  # NOTE: inside the lapply, `survey_key` is the loop variable (a character
  # string from survey_keys), while `key` refers to the column in `data`.
  # Using !!survey_key unquotes the loop variable for use in filter().
  survey_list <- setNames(
    lapply(survey_keys, function(survey_key) {
      data %>% filter(key == !!survey_key)
    }),
    paste0("survey_", gsub("-", "_", survey_keys))
  )
  
  return(survey_list)
}


# =============================================================================
# Function 6: get_variable_summary()
# =============================================================================
#
# PURPOSE:
#   Inspects the `value` (numeric code) and `resp` (character label) columns
#   across all datasets in a named list, reporting their types and the
#   unique response-to-value mapping for each. Used as a QC step before
#   running descriptive_stats() to verify that survey response scales are
#   correctly encoded.
#

get_variable_summary <- function(dataset_list) {
  
  summary_list <- lapply(names(dataset_list), function(dataset_name) {
    df <- dataset_list[[dataset_name]]
    
    if (!("value" %in% names(df)) | !("resp" %in% names(df))) {
      warning(paste("Skipping", dataset_name, "-- missing 'value' or 'resp' column"))
      return(NULL)
    }
    
    value_type <- class(df$value)
    resp_type  <- class(df$resp)
    
    # Unique resp -> value mapping, sorted for readability
    response_mapping <- df %>%
      select(resp, value) %>%
      distinct() %>%
      arrange(resp, value)
    
    list(
      Dataset          = dataset_name,
      Value_Type       = value_type,
      Resp_Type        = resp_type,
      Response_Mapping = response_mapping
    )
  })
  
  # Drop datasets that were skipped due to missing columns
  summary_list <- summary_list[!sapply(summary_list, is.null)]
  
  summary_df <- do.call(rbind, lapply(summary_list, function(x) {
    data.frame(
      Dataset          = x$Dataset,
      Value_Type       = x$Value_Type,
      Resp_Type        = x$Resp_Type,
      Response_Mapping = paste0(
        "Resp: ",  paste(x$Response_Mapping$resp,  collapse = ", "),
        "; Value: ", paste(x$Response_Mapping$value, collapse = ", ")
      ),
      stringsAsFactors = FALSE
    )
  }))
  
  return(summary_df)
}


# =============================================================================
# Function 7: descriptive_stats()
# =============================================================================
#
# PURPOSE:
#   Main descriptive statistics function. Iterates over all outcome variables
#   defined in `outcome_vars` and produces grouped summary tables. Supports
#   both numeric and categorical outcomes, with optional participant-level
#   weighting.
#
# GROUPING OPTIONS (group_by parameter):
#   "heatwave_period"  group by heatwave status × period of day (AM/PM/NI)
#   "heatwave"         group by heatwave status only (BEF/HW/AFT)
#   "period"           group by period of day only
#
# PARTICIPANT WEIGHTING (group_by_participant parameter):
#   FALSE (pooled): statistics are computed across all observations. Participants
#     with more measurements contribute more to group summaries. Useful for
#     population-level description.
#   TRUE (participant-weighted): statistics are first computed per participant,
#     then averaged across participants. Each participant contributes equally
#     regardless of measurement count. Recommended for publication tables
#     of repeated-measures data.
#

descriptive_stats <- function(data_list,
                              outcome_vars,
                              group_by_participant = FALSE,
                              group_by             = "heatwave_period",
                              categorical_grouping = TRUE,
                              numeric_variable     = TRUE) {
  
  results_list <- list()
  freq_list    <- list()
  
  for (outcome_name in names(outcome_vars)) {
    
    # Extract dataset and variable names 
    data        <- data_list[[outcome_vars[[outcome_name]]$data]]
    outcome_var <- outcome_vars[[outcome_name]]$var
    resp_var    <- outcome_vars[[outcome_name]]$resp
    
    if (!(outcome_var %in% names(data))) {
      warning(paste("Variable", outcome_var, "not found in dataset",
                    outcome_vars[[outcome_name]]$data))
      next
    }
    
    # Define grouping variables
    if (group_by == "heatwave_period") {
      grouping_vars <- c("heatwave_status", "period")
    } else if (group_by == "period") {
      grouping_vars <- c("period")
    } else if (group_by == "heatwave") {
      grouping_vars <- c("heatwave_status")
    } else {
      stop("Invalid value for 'group_by'. Choose 'heatwave_period', 'period', or 'heatwave'.")
    }
    
    # When participant-weighted, add participant_id as the innermost grouping
    # level so that statistics are first computed per participant. The
    # participant_id is then removed in the second-pass aggregation below.
    if (group_by_participant) {
      grouping_vars <- c("participant_id", grouping_vars)
    }
    
    # Numeric outcome summary
    if (numeric_variable) {
      
      stats <- data %>%
        group_by(across(all_of(grouping_vars))) %>%
        filter(heatwave_status %in% c("HW_June", "BEF_HW_June", "AFT_HW_June")) %>%
        summarize(
          outcome            = outcome_name,
          mean_value         = round(mean(.data[[outcome_var]], na.rm = TRUE), 2),
          sd_value           = round(sd(.data[[outcome_var]],   na.rm = TRUE), 2),
          median_value       = round(median(.data[[outcome_var]], na.rm = TRUE), 2),
          q1_value           = round(quantile(.data[[outcome_var]], 0.25, na.rm = TRUE), 2),
          q3_value           = round(quantile(.data[[outcome_var]], 0.75, na.rm = TRUE), 2),
          min_value          = round(min(.data[[outcome_var]], na.rm = TRUE), 2),
          max_value          = round(max(.data[[outcome_var]], na.rm = TRUE), 2),
          total_measurements = n(),
          # 95% CI of the mean: mean ± 1.96 * SE
          ci_lower = round(mean(.data[[outcome_var]], na.rm = TRUE) -
                             1.96 * sd(.data[[outcome_var]], na.rm = TRUE) / sqrt(total_measurements), 2),
          ci_upper = round(mean(.data[[outcome_var]], na.rm = TRUE) +
                             1.96 * sd(.data[[outcome_var]], na.rm = TRUE) / sqrt(total_measurements), 2),
          .groups = "drop"
        )
      
    } else {
      # Categorical outcome summary
      stats <- data %>%
        group_by(across(all_of(grouping_vars))) %>%
        filter(heatwave_status %in% c("HW_June", "BEF_HW_June", "AFT_HW_June")) %>%
        summarize(
          outcome            = outcome_name,
          # Mode: most frequently observed response label
          mode_value         = names(which.max(table(.data[[resp_var]]))),
          median_value       = median(as.numeric(.data[[outcome_var]]), na.rm = TRUE),
          q1_value           = quantile(as.numeric(.data[[outcome_var]]), 0.25, na.rm = TRUE),
          q3_value           = quantile(as.numeric(.data[[outcome_var]]), 0.75, na.rm = TRUE),
          min_value          = min(as.numeric(.data[[outcome_var]]), na.rm = TRUE),
          max_value          = max(as.numeric(.data[[outcome_var]]), na.rm = TRUE),
          total_measurements = n(),
          .groups            = "drop"
        )
      
      # Optional: compute response frequency table for categorical variables
      if (categorical_grouping) {
        freq_table <- data %>%
          group_by(across(all_of(grouping_vars)),
                   .data[[outcome_var]],
                   .data[[resp_var]]) %>%
          filter(heatwave_status %in% c("HW_June", "BEF_HW_June", "AFT_HW_June")) %>%
          count() %>%
          group_by(across(all_of(grouping_vars))) %>%
          mutate(percentage = round((n / sum(n)) * 100, 1)) %>%
          ungroup()
        
        freq_list[[outcome_name]] <- freq_table
      }
    }
    
    results_list[[outcome_name]] <- stats
  }
  
  # First-pass: combine all per-outcome results
  final_results      <- bind_rows(results_list)
  final_freq_results <- bind_rows(freq_list)
  
  # Second-pass: participant-level aggregation
  # When group_by_participant = TRUE, results_list contains one row per
  # participant per group. This second pass averages across participants
  # to produce one row per group the participant-weighted summary.
  if (group_by_participant) {
    
    results_list <- lapply(names(results_list), function(outcome_name) {
      df <- results_list[[outcome_name]]
      
      df %>%
        # Remove participant_id from grouping aggregate across participants
        group_by(across(all_of(setdiff(grouping_vars, "participant_id")))) %>%
        summarize(
          outcome = outcome_name,
          mean_of_means     = if ("mean_value"   %in% names(df)) round(mean(mean_value,   na.rm = TRUE), 2) else NA,
          mean_sd           = if ("sd_value"     %in% names(df)) round(mean(sd_value,     na.rm = TRUE), 2) else NA,
          median_of_medians = if ("median_value" %in% names(df)) round(median(median_value, na.rm = TRUE), 2) else NA,
          q1_of_q1          = if ("q1_value"     %in% names(df)) round(mean(q1_value,     na.rm = TRUE), 2) else NA,
          q3_of_q3          = if ("q3_value"     %in% names(df)) round(mean(q3_value,     na.rm = TRUE), 2) else NA,
          min_value         = if ("min_value"    %in% names(df)) round(mean(min_value,    na.rm = TRUE), 2) else NA,
          max_value         = if ("max_value"    %in% names(df)) round(mean(max_value,    na.rm = TRUE), 2) else NA,
          total_measurements = sum(total_measurements, na.rm = TRUE),
          ci_lower          = if ("ci_lower"     %in% names(df)) round(mean(ci_lower,     na.rm = TRUE), 2) else NA,
          ci_upper          = if ("ci_upper"     %in% names(df)) round(mean(ci_upper,     na.rm = TRUE), 2) else NA,
          .groups = "drop"
        )
    })
    
    final_results <- bind_rows(results_list)
  }
  
  return(list(stats = final_results, freq = final_freq_results))
}


# =============================================================================
# Function 8: display_table()
# =============================================================================
#
# PURPOSE:
#   Renders a summary dataframe as a styled HTML table using knitr and
#   kableExtra. Intended for use inside R Markdown documents.
#
# INPUT:
#   data  dataframe  summary table to display (e.g. descriptive_stats()$stats)
#                    NOTE: pass the $stats element, not the full list output
#                    of descriptive_stats(). Passing the full list will fail.
display_table <- function(data) {
  data %>%
    knitr::kable("html", align = "c") %>%
    kableExtra::kable_styling(
      full_width        = TRUE,
      bootstrap_options = c("striped", "hover", "condensed", "responsive")
    )
}


# =============================================================================
# Utility: get_mode()
# =============================================================================
#
# PURPOSE:
#   Returns the mode (most frequently occurring value) of a vector.
#   Available as a standalone utility function for use in ad hoc summaries.
#   Not called internally by other functions in this file.
#
get_mode <- function(x) {
  uniq_x <- unique(x)
  uniq_x[which.max(tabulate(match(x, uniq_x)))]
}
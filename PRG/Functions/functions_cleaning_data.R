# =============================================================================
# FILE:    PRG/Functions/functions_cleaning_data.R
# PURPOSE: Data cleaning utilities for the HeatSuite 2024 study.
#          Provides four functions to handle duplicate measurements and
#          implausible values across all physiological and perceptual datasets.
#
# FUNCTIONS:
#   1. set_reference_time()       -- Assigns a physiologically meaningful
#                                    reference time to each observation based
#                                    on its period of day (AM/PM/NI).
#   2. get_surrounding_indices()  -- Groups row indices of duplicate
#                                    measurements that share the same
#                                    participant / date / period.
#   3. remove_farthest_measure()  -- Retains only the measurement closest
#                                    to the reference time within each group,
#                                    discarding all others.
#   4. delete_outliers()          -- Removes values that differ from all
#                                    three preceding measurements by more
#                                    than a user-specified threshold.
#
# DEPENDENCIES:
#   dplyr    (arrange, group_by, mutate, filter, select, ungroup)
#   lubridate (days)
#   purrr    (reduce)
#   These must be loaded in the calling script before sourcing this file.
#
# TYPICAL PIPELINE (applied per outcome):
#   data |> set_reference_time()
#        |> set_adjusted_night_time()   # see functions_adjusted_ref_time.R
#        |> get_surrounding_indices()
#        |> remove_farthest_measure()
#
# =============================================================================


# =============================================================================
# Function 1: set_reference_time()
# =============================================================================
#
# PURPOSE:
#   Assigns a period-specific reference time to every row in the dataset.
#   This reference time is used downstream by remove_farthest_measure() to
#   select the single "best" measurement per participant per period per day.
#
# REFERENCE TIME DEFINITION:
#   - AM (Morning):   awakening time (sleepEnd_time from sleep analysis)
#   - NI (Night):     bedtime (sleepStart_time from sleep analysis)
#   - PM (Afternoon): 15:00 on the date of measurement (fixed reference)
#
# NA FALLBACK STRATEGY:
#   When sleep data is unavailable for a given day (e.g. participant did not
#   wear the device at night), reference_time is NA. A rolling lookback of
#   up to 10 prior days is applied to carry forward the most recent available
#   awakening or bedtime for the same participant and period. If no value is
#   found within 10 days, a population-level default is used:
#     - AM default: 08:00 (typical waking time for older adults)
#     - NI default: 20:00 (conservative early-evening bedtime for older adults)
#
# INPUT:
#   data_of_interest -- dataframe containing at minimum:
#     participant_id      character  participant identifier
#     date_converted      Date       calendar date of measurement
#     period              character  "AM", "PM", or "NI"
#     unix_time_converted POSIXct    datetime of measurement (local time)
#     sleepEnd_time       POSIXct    awakening time from sleep analysis
#                                    (NA if sleep data not available)
#     sleepStart_time     POSIXct    bedtime from sleep analysis
#                                    (NA if sleep data not available)
#     date_awakening      Date       date of awakening (used in NI check)
#     date_bedtime        Date       date of bedtime (used in NI check)
#
# OUTPUT:
#   The same dataframe with one new column added:
#     reference_time  POSIXct  period-specific reference time for each row
#
# NOTE ON NI SPECIAL CASE:
#   When bedtime falls after midnight (00:00-04:00), the reference_time is
#   shifted forward by 1 day. This is necessary because participants in the
#   NI period recorded measurements on the evening of day D, but their
#   bedtime (sleepStart_time) is stored on day D+1 (after midnight). Without
#   this correction, the time difference calculation would produce an error
#   of ~24 hours for these participants.

set_reference_time <- function(data_of_interest) {
  
  data_of_interest <- data_of_interest %>%
    arrange(participant_id, date_converted) %>%
    group_by(participant_id, period) %>%
    
    # Step 1: Assign reference time based on period of day
    mutate(
      reference_time = case_when(
        # AM: closest to awakening time (end of sleep episode)
        period == "AM" ~ as.POSIXct(sleepEnd_time,   origin = "1970-01-01", tz = "UTC"),
        # NI: closest to bedtime (start of sleep episode)
        period == "NI" ~ as.POSIXct(sleepStart_time, origin = "1970-01-01", tz = "UTC"),
        # PM: fixed reference at 15:00 on the date of measurement
        period == "PM" ~ as.POSIXct(
          paste(date_converted, "15:00:00"),
          format = "%Y-%m-%d %H:%M:%S",
          tz = "UTC"
        ),
        # All other cases (should not occur): set to NA
        TRUE ~ as.POSIXct(NA, origin = "1970-01-01", tz = "UTC")
      )
    ) %>%
    
    # Step 2: Rolling lookback for AM and NI when reference_time is NA
    # When sleep data is missing for a given day, carry forward the time-of-day
    # component from the most recent available reference_time within the last
    # 10 days. purrr::reduce() iterates lag offsets 1 through 10, using
    # coalesce() to keep the first non-NA value found.
    # Only the HH:MM:SS component is carried forward; the date remains the
    # current measurement date, ensuring correct calendar alignment.
    mutate(
      reference_time = case_when(
        
        period == "AM" & is.na(reference_time) ~
          reduce(
            1:10,
            ~ coalesce(
              .x,
              as.POSIXct(
                paste(date_converted, format(lag(reference_time, n = .y), "%H:%M:%S")),
                format = "%Y-%m-%d %H:%M:%S",
                tz = "UTC"
              )
            ),
            .init = reference_time
          ),
        
        period == "NI" & is.na(reference_time) ~
          reduce(
            1:10,
            ~ coalesce(
              .x,
              as.POSIXct(
                paste(date_converted, format(lag(reference_time, n = .y), "%H:%M:%S")),
                format = "%Y-%m-%d %H:%M:%S",
                tz = "UTC"
              )
            ),
            .init = reference_time
          ),
        
        TRUE ~ reference_time
      )
    ) %>%
    
    # Step 3: NI date correction for post-midnight bedtimes
    # When bedtime (sleepStart_time) falls between 00:00 and 04:00, it is
    # recorded on the next calendar day (day D+1) but belongs to the NI period
    # of day D. Shift reference_time forward by 1 day to compensate, but only
    # when the measurement itself is NOT in the early-morning window (which
    # would indicate a genuine next-day observation).
    mutate(
      reference_time = case_when(
        period == "NI" &
          (is.na(date_awakening) | is.na(date_bedtime)) &
          format(reference_time, "%H:%M:%S") >= "00:00:00" &
          format(reference_time, "%H:%M:%S") <= "04:00:00" &
          !(
            format(unix_time_converted, "%H:%M:%S") >= "00:00:01" &
              format(unix_time_converted, "%H:%M:%S") <= "04:00:00"
          ) ~ reference_time + lubridate::days(1),
        TRUE ~ reference_time
      )
    ) %>%
    
    # Step 4: Population-level defaults when reference_time is still NA 
    # Applied when no sleep data was found within the 10-day lookback window.
    # Defaults reflect typical schedules for community-dwelling older adults:
    #   AM = 08:00 (typical waking time)
    #   NI = 20:00 (conservative early-evening bedtime)
    mutate(
      reference_time = case_when(
        period == "AM" & is.na(reference_time) ~ as.POSIXct(
          paste(date_converted, "08:00:00"),
          format = "%Y-%m-%d %H:%M:%S",
          tz = "UTC"
        ),
        period == "NI" & is.na(reference_time) ~ as.POSIXct(
          paste(date_converted, "20:00:00"),
          format = "%Y-%m-%d %H:%M:%S",
          tz = "UTC"
        ),
        TRUE ~ reference_time
      )
    ) %>%
    
    ungroup()
  
  return(data_of_interest)
}


# =============================================================================
# Function 2: get_surrounding_indices()
# =============================================================================
#
# PURPOSE:
#   Groups row indices of duplicate measurements so that each group contains
#   all rows belonging to the same "duplicate set" (same participant, date,
#   and period). Used as input to remove_farthest_measure().
#
# HOW IT WORKS:
#   The input `indices` is a vector of row positions in `data` where duplicate
#   measurements start (i.e. the first row of each duplicate group).
#   The `data$row_num` column holds a pre-computed count of how many duplicate
#   rows exist for each group. Starting from each index, a sequence of
#   `row_num` consecutive rows is generated to cover the full group.
#
# PREREQUISITE:
#   The input dataframe `data` must contain a column named `row_num`
#   indicating the number of consecutive rows belonging to each group.
#   This column is typically created by the calling script using:
#     data <- data %>%
#       group_by(participant_id, date_converted, period) %>%
#       mutate(row_num = n()) %>%
#       ungroup()
#


get_surrounding_indices <- function(indices, data) {
  
  all_indices <- list()
  
  if (length(indices) == 0) {
    # No duplicates found -- nothing to process
    message("No duplicate indices provided. Returning empty list.")
  } else {
    for (index in indices) {
      # row_num at position `index` tells us how many consecutive rows
      # belong to this duplicate group (e.g. row_num = 3 means 3 duplicates)
      i <- data$row_num[index]
      
      # Build the full index sequence for this group
      surrounding_indices <- seq(index, index + (i - 1))
      
      all_indices[[paste("Index", index)]] <- surrounding_indices
    }
  }
  
  # Warn if any group resolved to an empty index vector (should not occur)
  for (group_name in names(all_indices)) {
    if (length(all_indices[[group_name]]) == 0) {
      message("WARNING: No indices found for group: ", group_name)
    }
  }
  
  return(all_indices)
}


# =============================================================================
# Function 3: remove_farthest_measure()
# =============================================================================
#
# PURPOSE:
#   For each group of duplicate measurements (same participant / date / period),
#   retains only the single measurement closest in time to the period reference
#   time and removes all others.
#
# REFERENCE TIME USED:
#   - AM and PM: reference_time (set by set_reference_time())
#   - NI:        adjusted_reference_time (set by set_adjusted_night_time())
#                Falls back to reference_time if adjusted_reference_time is
#                not present in the dataset.
#
# NI ADJUSTMENT (25,000-second threshold):
#   For the NI period, if the absolute time difference between a measurement
#   and the reference time exceeds 25,000 seconds (~6.9 hours), it is likely
#   that the reference time is off by one calendar day. In this case,
#   adjusted_reference_time is shifted back by 86,400 seconds (1 day) before
#   recalculating the time difference. This handles edge cases where bedtime
#   after midnight was not fully corrected by set_adjusted_night_time().
#

remove_farthest_measure <- function(data, surrounding_indices_list) {
  
  cleaned_data  <- data
  rows_to_remove <- integer(0)
  
  for (group_name in names(surrounding_indices_list)) {
    
    group_indices <- surrounding_indices_list[[group_name]]
    group_data    <- cleaned_data[group_indices, ]
    
    if (nrow(group_data) > 1) {
      
      group_data <- group_data %>%
        mutate(
          # Convert measurement time to numeric seconds for arithmetic
          date_time_unix = as.numeric(as.POSIXct(unix_time_converted, tz = "UTC")),
          
          # Convert reference_time to numeric
          reference_time_unix = as.numeric(as.POSIXct(reference_time, tz = "UTC")),
          
          # Use adjusted_reference_time for NI if available, otherwise fall back
          # to reference_time. adjusted_reference_time corrects for post-midnight
          # bedtimes that cross a calendar day boundary.
          adjusted_reference_time_unix = ifelse(
            "adjusted_reference_time" %in% names(group_data),
            as.numeric(as.POSIXct(adjusted_reference_time, tz = "UTC")),
            reference_time_unix
          ),
          
          # Compute initial absolute time difference to reference
          # NI uses adjusted_reference_time; AM and PM use reference_time
          time_diff = ifelse(
            period == "NI",
            abs(adjusted_reference_time_unix - date_time_unix),
            abs(date_time_unix - reference_time_unix)
          ),
          
          # NI edge case: if time_diff > 25,000 s (~6.9 h), the reference is
          # likely still off by one day. Subtract 86,400 s (1 day) and recompute.
          adjusted_reference_time_unix = ifelse(
            period == "NI" & time_diff > 25000,
            adjusted_reference_time_unix - 86400,
            adjusted_reference_time_unix
          ),
          
          # Final time difference used for selecting the closest measurement
          time_diff = ifelse(
            period == "NI",
            abs(adjusted_reference_time_unix - date_time_unix),
            abs(date_time_unix - reference_time_unix)
          )
        )
      
      # Keep only the row with the smallest time difference (closest to reference)
      closest_index  <- group_indices[which.min(group_data$time_diff)]
      
      # All other rows in the group are marked for removal
      rows_to_remove <- c(rows_to_remove, setdiff(group_indices, closest_index))
    }
  }
  
  # Remove all flagged rows in one operation
  if (length(rows_to_remove) > 0) {
    cleaned_data <- cleaned_data[-rows_to_remove, ]
  }
  
  return(cleaned_data)
}


# =============================================================================
# Function 4: delete_outliers()
# =============================================================================
#
# PURPOSE:
#   Removes observations where the measured value is implausibly far from
#   all recent measurements for the same participant. Designed to detect
#   device artefacts (e.g. a reading orders of magnitude outside the normal
#   range) rather than genuine biological variability.
#
# LOGIC:
#   For each row, the absolute difference between the current value and each
#   of the three immediately preceding values (for the same participant) is
#   computed. A row is considered VALID, and therefore retained, if at
#   least ONE of these three differences is within the specified threshold.
#
#   This conservative rule means: a measurement is only removed if it is
#   implausibly far from ALL three preceding values simultaneously. A single
#   adjacent measurement within the threshold is sufficient to retain the row.
#   NA differences (e.g. at the start of a participant's series) also count
#   as "within threshold" to avoid removing the first few observations.
#
# PARAMETERS:
#   df             dataframe   input data (must contain participant_id)
#   outcome_label  character   name of the numeric column to check
#   threshold      numeric     maximum acceptable absolute difference between
#                              consecutive measurements (default = 5)
#                              Units must match the outcome variable's units.
#
# OUTPUT:
#   The input dataframe with outlier rows removed and helper columns cleaned up.
#
# EXAMPLE:
#   # Remove body mass values differing > 4 kg from all 3 preceding measurements
#   cleaned <- delete_outliers(df = data, outcome_label = "mass", threshold = 4)
#
#   # Remove blood pressure values differing > 50 mmHg from all 3 preceding
#   cleaned <- delete_outliers(df = data, outcome_label = "sbp", threshold = 50)

delete_outliers <- function(df, outcome_label, threshold = 5) {
  
  df <- df %>%
    group_by(participant_id) %>%
    mutate(
      # Absolute difference from the immediately preceding measurement (lag 1)
      outcome_diff1 = abs(.data[[outcome_label]] - lag(.data[[outcome_label]], 1)),
      # Absolute difference from 2 measurements ago (lag 2)
      outcome_diff2 = abs(.data[[outcome_label]] - lag(.data[[outcome_label]], 2)),
      # Absolute difference from 3 measurements ago (lag 3)
      outcome_diff3 = abs(.data[[outcome_label]] - lag(.data[[outcome_label]], 3)),
      
      # A row is valid if ANY of the three lag differences is within threshold
      # OR if the lag difference is NA (start of series insufficient history
      # to compare, so the row is conservatively retained)
      is_valid = ifelse(
        is.na(outcome_diff1) | outcome_diff1 <= threshold |
          is.na(outcome_diff2) | outcome_diff2 <= threshold |
          is.na(outcome_diff3) | outcome_diff3 <= threshold,
        TRUE,
        FALSE
      )
    ) %>%
    filter(is_valid) %>%
    # Remove intermediate helper columns before returning
    select(-outcome_diff1, -outcome_diff2, -outcome_diff3, -is_valid) %>%
    ungroup()
  
  return(df)
}
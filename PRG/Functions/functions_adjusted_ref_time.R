# =============================================================================
# FILE:    PRG/Functions/functions_adjusted_ref_time.R
# PURPOSE: Adjusts reference timestamps for Night (NI) period measurements
#          to correctly align physiological or perceptual observations with
#          the participant's bedtime across calendar day boundaries.
#
# FUNCTION:
#   set_adjusted_night_time() -- Produces a new column `adjusted_reference_time`
#                                that corrects the raw `reference_time` (bedtime)
#                                for all six known edge cases arising from
#                                post-midnight measurements.
#
# DEPENDENCIES:
#   dplyr    (mutate, case_when, group_by, ungroup, lead)
#   lubridate (days, difftime)
#   These must be loaded in the calling script before sourcing this file.
#
# CONTEXT:
#   Called immediately after set_reference_time() (functions_cleaning_data.R).
#   Only operates on rows where period == "NI". AM and PM rows pass through
#   unchanged (adjusted_reference_time = NA for those rows).
#
# =============================================================================


# =============================================================================
# set_adjusted_night_time()
# =============================================================================
#
# PURPOSE:
#   The Night (NI) period spans 18:00 to 02:59 the following day, meaning a
#   single NI observation window crosses a calendar day boundary at midnight.
#   The raw reference_time (bedtime from sleep analysis) may therefore fall
#   on a different calendar date than the measurement (unix_time_converted),
#   creating apparent time differences of ~24 hours when the true gap is
#   only a few minutes.
#
#   This function resolves all such misalignments by constructing
#   `adjusted_reference_time` through six sequential case checks, applied
#   in order of specificity. Only the first matching case is applied to each
#   row; all remaining cases leave the value unchanged via TRUE ~ current_value.
#
# THE SIX CASES:
#
#   Case 1 -- Both reference and measurement are on the same calendar day,
#             both in the early-morning window (00:00-03:00).
#             Action: use reference_time as-is.
#             Scenario: participant went to bed just after midnight and the
#             measurement was taken in the same early-morning window.
#
#   Case 2 -- Both reference and measurement are on the same calendar day,
#             both in the evening window (18:00-23:59).
#             Action: use reference_time as-is.
#             Scenario: participant went to bed early (before midnight) and
#             measurement was taken the same evening.
#
#   Case 3 -- Reference and measurement are on different calendar days.
#             Reference is in the early-morning window (00:00-03:00) but
#             measurement is in the evening window (18:00-23:59).
#             Action: add 1 day to reference_time.
#             Scenario: the participant was measured on the evening of day D,
#             but their bedtime is stored as day D+1 (after midnight). Adding
#             1 day to the measurement's calendar day brings the reference
#             forward to day D+1, aligning with the stored bedtime date.
#
#   Case 4 -- adjusted_reference_time is still NA after Cases 1-3.
#             The next row for the same participant/period has a reference_time
#             within 12 hours of the current measurement.
#             Action: use the next row's reference_time (lead).
#             Scenario: the current row has missing sleep data but the
#             immediately following night has a valid bedtime that is
#             close enough to serve as a proxy reference.
#
#   Case 5 -- Measurement is in the early-morning window (00:00-03:00) and
#             adjusted_reference_time is more than 3 hours ahead.
#             Action: set adjusted_reference_time = unix_time_converted.
#             Scenario: a residual day-offset error left adjusted_reference_time
#             pointing to a time far from the actual measurement. Setting the
#             reference equal to the measurement time ensures the row is not
#             incorrectly penalised as "far from reference" during deduplication.
#
#   Case 6 -- adjusted_reference_time is more than 12 hours (720 minutes)
#             behind the measurement time.
#             Action: add 1 day to adjusted_reference_time.
#             Scenario: a final-pass correction for any remaining cases where
#             the reference is still on the wrong calendar day.
#
# FINAL CORRECTIONS (after all 6 cases):
#   Two additional corrections are applied based on time_diff_minutes
#   (measurement time minus adjusted_reference_time):
#     - Between -720 and -2000 min (-12h to -33h): reference is ahead of
#       measurement by more than 12 hours, suggesting a day-offset error.
#       Action: reset to reference_time.
#     - Between -2000 and -3000 min (-33h to -50h): reference is ahead by
#       more than a full day.
#       Action: reset to reference_time - 1 day.


set_adjusted_night_time <- function(data_merged) {
  
  data_merged <- data_merged %>%
    
    # Initialise adjusted_reference_time as NA for all rows.
    # AM and PM rows will remain NA throughout -- only NI rows are modified.
    mutate(
      adjusted_reference_time = as.POSIXct(NA, origin = "1970-01-01", tz = "UTC")
    ) %>%
    
    mutate(
      
      # Case 1: Same calendar day, both reference and measurement 00:00-03:00
      # Both bedtime and measurement fall in the same early-morning window on
      # the same date. No date shift needed -- use reference_time directly.
      adjusted_reference_time = case_when(
        period == "NI" &
          as.Date(reference_time) == as.Date(unix_time_converted) &
          format(reference_time,        "%H:%M:%S") >= "00:00:00" &
          format(reference_time,        "%H:%M:%S") <= "03:00:00" &
          format(unix_time_converted,   "%H:%M:%S") >= "00:00:00" &
          format(unix_time_converted,   "%H:%M:%S") <= "03:00:00" ~ reference_time,
        TRUE ~ adjusted_reference_time
      ),
      
      # Case 2: Same calendar day, both reference and measurement 18:00-23:59
      # Both bedtime and measurement fall in the same evening window on the
      # same date. No date shift needed -- use reference_time directly.
      adjusted_reference_time = case_when(
        period == "NI" &
          as.Date(reference_time) == as.Date(unix_time_converted) &
          format(reference_time,        "%H:%M:%S") >= "18:00:00" &
          format(reference_time,        "%H:%M:%S") <= "23:59:59" &
          format(unix_time_converted,   "%H:%M:%S") >= "18:00:00" &
          format(unix_time_converted,   "%H:%M:%S") <= "23:59:59" ~ reference_time,
        TRUE ~ adjusted_reference_time
      ),
      
      # Case 3: Different calendar days -- reference after midnight, measurement in evening
      # Measurement taken on the evening of day D; bedtime stored as day D+1
      # (because participant went to bed after midnight). Adding 1 day to
      # reference_time moves it to day D+1, aligning with the stored bedtime date.
      adjusted_reference_time = case_when(
        period == "NI" &
          as.Date(reference_time) != as.Date(unix_time_converted) &
          format(reference_time,        "%H:%M:%S") >= "00:00:00" &
          format(reference_time,        "%H:%M:%S") <= "03:00:00" &
          format(unix_time_converted,   "%H:%M:%S") >= "18:00:00" &
          format(unix_time_converted,   "%H:%M:%S") <= "23:59:59" ~ reference_time + days(1),
        TRUE ~ adjusted_reference_time
      )
    ) %>%
    
    # Case 4: Fill remaining NAs using the next row's reference_time
    # Grouped by participant and period to ensure lead() only looks within the
    # same participant's NI rows. If the next NI row's bedtime is within 12 hours
    # of the current measurement, it is close enough to use as a proxy reference.
    group_by(participant_id, period) %>%
    mutate(
      adjusted_reference_time = case_when(
        period == "NI" &
          is.na(adjusted_reference_time) &
          as.numeric(difftime(
            lead(reference_time), unix_time_converted, units = "hours"
          )) <= 12 ~ lead(reference_time),
        TRUE ~ adjusted_reference_time
      )
    ) %>%
    ungroup() %>%
    
    mutate(
      
      # Case 5: Early-morning measurement with reference > 3 hours ahead
      # If the measurement is between 00:00 and 03:00 but adjusted_reference_time
      # is more than 3 hours ahead, a residual day-offset error remains. Setting
      # reference equal to measurement time avoids penalising this row as
      # "far from reference" during the deduplication step.
      hours_discrepancy = as.numeric(
        difftime(adjusted_reference_time, unix_time_converted, units = "hours")
      ),
      adjusted_reference_time = case_when(
        format(unix_time_converted, "%H:%M:%S") >= "00:00:00" &
          format(unix_time_converted, "%H:%M:%S") <= "03:00:00" &
          hours_discrepancy >= 3 ~ unix_time_converted,
        TRUE ~ adjusted_reference_time
      ),
      
      # Case 6: Reference is more than 12 hours behind the measurement 
      # Any remaining case where adjusted_reference_time lags the measurement
      # by more than 720 minutes (12 hours) indicates the reference is still on
      # the wrong calendar day. Adding 1 day corrects this final offset.
      time_diff_minutes = as.numeric(
        difftime(unix_time_converted, adjusted_reference_time, units = "mins")
      ),
      adjusted_reference_time = case_when(
        time_diff_minutes > 720 ~ adjusted_reference_time + days(1),
        TRUE ~ adjusted_reference_time
      )
    ) %>%
    
    # Final fallback: if adjusted_reference_time is still NA, use reference + 1 day
    # Covers any remaining NI rows where all six cases above failed to assign a value.
    mutate(
      adjusted_reference_time = case_when(
        period == "NI" & is.na(adjusted_reference_time) ~ reference_time + days(1),
        TRUE ~ adjusted_reference_time
      )
    )
  
  # Final recalculation of time_diff_minutes 
  # Recompute after all adjustments are complete. This final value is used for:
  #   (a) downstream verification in the calling script
  #   (b) the two empirical threshold corrections immediately below
  data_merged <- data_merged %>%
    mutate(
      time_diff_minutes = as.numeric(
        difftime(unix_time_converted, adjusted_reference_time, units = "mins")
      )
    )
  
  # Final empirical corrections for residual day-offset errors 
  # Applied after all case logic. Thresholds were determined empirically from
  # inspection of the HeatSuite 2024 NI measurement time-difference distribution:
  #
  #   -720 to -2000 min (-12h to -33h): reference is more than 12 hours AHEAD
  #   of the measurement -- likely a +1 day offset error. Reset to reference_time.
  #
  #   -2000 to -3000 min (-33h to -50h): reference is more than a full day AHEAD
  #   of the measurement -- likely a +2 day offset error. Reset to reference_time - 1 day.
  data_merged <- data_merged %>%
    mutate(
      adjusted_reference_time = case_when(
        time_diff_minutes >= -2000 & time_diff_minutes <= -720  ~ reference_time,
        time_diff_minutes >= -3000 & time_diff_minutes <= -2000 ~ reference_time - days(1),
        TRUE ~ adjusted_reference_time
      )
    )
  
  return(data_merged)
}
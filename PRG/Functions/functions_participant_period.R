# =============================================================================
# FILE:    PRG/Functions/functions_participant_period.R
# PURPOSE: Assigns participant identifiers, time-of-day periods, and heatwave
#          status labels to all raw datasets collected during the HeatSuite
#          2024 study. These functions are applied at the start of every
#          data preparation script.
#
# FUNCTIONS:
#   1. add_participant_id_watch()  -- Matches each smartwatch MAC address to
#                                     a participant ID using a date-bounded
#                                     lookup table (baseline_long).
#   2. assign_study_period()       -- Converts Unix timestamps to local time,
#                                     assigns period of day (AM / PM / NI),
#                                     and filters to the study window.
#   3. assign_heatwave()           -- Labels each observation with its
#                                     heatwave monitoring period
#                                     (BEF_HW_June / HW_June / AFT_HW_June / NoHW).
#
# TYPICAL CALL ORDER (applied to every raw dataset):
#   data |> assign_study_period(unix_columns)
#        |> add_participant_id_watch(baseline_long, mac_col, time_col)
#        |> assign_heatwave()
#
# STUDY-SPECIFIC CONSTANTS (update if replicating in a different context):
#   Monitoring window : 2024-06-11 to 2024-06-27
#   Heatwave period   : 2024-06-18 to 2024-06-20 (Montreal Public Health definition)
#   Before heatwave   : 2024-06-11 to 2024-06-17
#   After heatwave    : 2024-06-21 to 2024-06-27
#   Timezone offset   : UTC-4 (Montreal EDT, Eastern Daylight Time)
#   Period boundaries : AM = 03:00-11:59 / PM = 12:00-17:59 / NI = 18:00-02:59
#
# DEPENDENCIES:
#   dplyr    (mutate, filter, left_join, select, rowwise, ungroup, case_when)
#   lubridate (ymd, ymd_hms, dmy, dmy_hms, hour, hours, as.Date)
#   These must be loaded in the calling script before sourcing this file.
#
# =============================================================================


# =============================================================================
# Function 1: add_participant_id_watch()
# =============================================================================
#
# PURPOSE:
#   Assigns a participant_id to each row of a raw dataset by matching the
#   smartwatch MAC address and measurement date against a lookup table
#   (baseline_long). The lookup table stores one row per participant per
#   watch assignment period, enabling correct attribution even when a
#   participant used multiple devices sequentially (e.g. after a watch was
#   lost or broken).
#
# MAC ADDRESS NORMALISATION:
#   MAC addresses appear in different formats across data sources
#   (e.g. "F6:77:DB:76:D2:6C" vs "F677DB76D26C"). All separators and
#   whitespace are removed and the result is uppercased before joining,
#   ensuring format-independent matching.
#
# DATE MATCHING:
#   The local_date derived from the measurement timestamp must fall within
#   the [date_start, date_end] interval for the participant's watch assignment
#   in baseline_long. The join uses relationship = "many-to-many" because
#   a single MAC address may appear in multiple rows of baseline_long when
#   a watch was reassigned between participants, and each measurement row
#   may match multiple candidate periods before the date filter resolves it
#   to exactly one participant.
#

add_participant_id_watch <- function(data,
                                     baseline_long,
                                     mac_col  = "macID",
                                     time_col = "local_time") {
  
  # Helper: normalise MAC address to uppercase without separators 
  # Removes colons, hyphens, and whitespace; converts to uppercase.
  # Example: "f6:77:db:76:d2:6c" -> "F677DB76D26C"
  normalize_mac <- function(x) {
    toupper(gsub("[:\\-]", "", trimws(x)))
  }
  
  # Helper: parse date from multiple possible input formats 
  # Tries four common date/datetime formats in order of specificity.
  # Falls back to the next format when parsing returns all NA.
  # Returns a Date (not POSIXct) for robust calendar-day comparison.
  parse_date_flexible <- function(x) {
    if (inherits(x, "Date"))    return(x)           # already a Date -- use as-is
    if (inherits(x, "POSIXct")) return(as.Date(x))  # POSIXct -- extract date component
    # character input -- try all known formats in order:
    parsed <- suppressWarnings(ymd_hms(x))          # "2024-04-18 10:20:00"
    if (all(is.na(parsed))) parsed <- suppressWarnings(dmy_hms(x))  # "18/04/2024 10:20:00"
    if (all(is.na(parsed))) parsed <- suppressWarnings(ymd(x))      # "2024-04-18"
    if (all(is.na(parsed))) parsed <- suppressWarnings(dmy(x))      # "18/04/2024"
    as.Date(parsed)
  }
  
  data %>%
    mutate(
      # Normalise MAC in the measurement data and extract the measurement date
      macID      = normalize_mac(.data[[mac_col]]),
      local_date = parse_date_flexible(.data[[time_col]])
    ) %>%
    left_join(
      # Normalise MAC and parse dates in the lookup table before joining
      baseline_long %>%
        mutate(
          macID      = normalize_mac(macID),
          date_start = as.Date(ymd(date_start)),
          date_end   = as.Date(ymd(date_end))
        ) %>%
        filter(!is.na(date_start) & !is.na(date_end)),
      by = "macID",
      # many-to-many is expected: one MAC may appear in multiple lookup rows
      # (e.g. a watch reassigned between participants), and each measurement
      # row may match multiple candidate periods before the date filter below
      # resolves the ambiguity to exactly one participant
      relationship = "many-to-many"
    ) %>%
    # Keep only rows where the measurement date falls within the device
    # assignment period for that participant
    filter(local_date >= date_start & local_date <= date_end) %>%
    select(-date_start, -date_end)
}


# =============================================================================
# Function 2: assign_study_period()
# =============================================================================
#
# PURPOSE:
#   Converts one or more Unix timestamp columns to local Montreal time,
#   assigns each observation to a period of day (AM / PM / NI), and
#   filters the dataset to the study monitoring window.
#
# TIMEZONE:
#   All raw Unix timestamps are stored in UTC. A fixed offset of -4 hours
#   is applied to convert to Montreal Eastern Daylight Time (EDT, UTC-4),
#   which was in effect throughout the June 2024 study period.
#   UPDATE THIS OFFSET if replicating the study in a different timezone
#   or season (e.g. EST = UTC-5 in winter).
#
# PERIOD OF DAY BOUNDARIES:
#   AM (Morning)   : 03:00 - 11:59
#   PM (Afternoon) : 12:00 - 17:59
#   NI (Night)     : 18:00 - 02:59 (next day)
#   The AM period starts at 03:00 (not 00:00) to capture late-night
#   measurements as Night rather than Morning, consistent with the
#   participants' circadian schedules.
#
# MONITORING WINDOW:
#   Only observations between 2024-06-11 00:00:00 and 2024-06-27 23:59:59
#   (UTC) are retained. This corresponds to the one-week pre-heatwave,
#   three-day heatwave, and one-week post-heatwave periods.
#   UPDATE start_date and end_date if replicating with a different window.
#

assign_study_period <- function(data_participant, unix_columns) {
  
  # Validate input
  if (length(unix_columns) == 0) {
    stop("Error: No Unix columns specified. Provide at least one column name.")
  }
  
  # Step 1: Convert each Unix column to local Montreal time (EDT, UTC-4) 
  for (col in unix_columns) {
    if (!col %in% names(data_participant)) {
      stop(paste("Error: Column", col, "does not exist in the dataset."))
    }
    new_col_name <- paste0(col, "_time_converted")
    data_participant <- data_participant %>%
      mutate(
        # Convert from Unix integer (seconds since 1970-01-01 UTC) to POSIXct,
        # then subtract 4 hours to convert UTC to Montreal EDT (UTC-4).
        # NOTE: update hours(4) to hours(5) for EST (UTC-5, winter months)
        !!new_col_name := as.POSIXct(.data[[col]], origin = "1970-01-01", tz = "UTC") - hours(4)
      )
  }
  
  # Step 2: Use the first converted column as the primary time reference 
  main_time_col <- paste0(unix_columns[1], "_time_converted")
  
  if (!main_time_col %in% names(data_participant)) {
    stop(paste("Error: Generated column", main_time_col, "not found. Check Unix conversion."))
  }
  
  # Step 3: Extract calendar date and assign period of day 
  data_participant <- data_participant %>%
    mutate(
      # Calendar date in local Montreal time
      date_converted = as.Date(.data[[main_time_col]]),
      
      # Period of day assignment:
      #   AM: 03:00-11:59 -- morning, includes early-morning post-midnight
      #   PM: 12:00-17:59 -- afternoon
      #   NI: 18:00-02:59 -- night (TRUE catches remaining hours 18:00-23:59
      #       AND 00:00-02:59, which are excluded from AM by the >= 3 condition)
      period = case_when(
        hour(.data[[main_time_col]]) >= 3 & hour(.data[[main_time_col]]) < 12 ~ "AM",
        hour(.data[[main_time_col]]) >= 12 & hour(.data[[main_time_col]]) < 18 ~ "PM",
        TRUE ~ "NI"
      )
    )
  
  # Step 4: Filter to the study monitoring window 
  # Dates are the heatwave study window for HeatSuite 2024 (Montreal, June 2024).
  # UPDATE start_date and end_date if applying to a different study period.
  start_date <- as.POSIXct("2024-06-11 00:00:00", tz = "UTC")  # Start: Jun 11, 2024
  end_date   <- as.POSIXct("2024-06-27 23:59:59", tz = "UTC")  # End:   Jun 27, 2024
  
  data_participant <- data_participant %>%
    filter(
      .data[[main_time_col]] >= start_date &
        .data[[main_time_col]] <= end_date
    )
  
  return(data_participant)
}


# =============================================================================
# Function 3: assign_heatwave()
# =============================================================================
#
# PURPOSE:
#   Labels each observation with its heatwave monitoring period based on the
#   Montreal Public Health definition of the June 2024 heatwave event:
#
#     BEF_HW_June : 2024-06-11 to 2024-06-17  (7-day pre-heatwave baseline)
#     HW_June     : 2024-06-18 to 2024-06-20  (3-day heatwave)
#     AFT_HW_June : 2024-06-21 to 2024-06-27  (7-day post-heatwave recovery)
#     NoHW        : all other dates within the study window
#
# MULTI-COLUMN SUPPORT:
#   The function automatically detects which datetime column is available
#   in the input dataframe and uses the first one found:
#     1. unix_time_converted  (physiological and perceptual data -- POSIXct)
#     2. sleepEnd_time        (sleep analysis data -- POSIXct)
#     3. date                 (pre-aggregated daily data -- Date)
#   This allows the function to be applied directly to datasets at any
#   stage of the pipeline without preprocessing.
#
# HEATWAVE DURATION VALIDATION:
#   The function validates that the defined heatwave period meets the
#   Montreal Public Health criterion of at least 72 hours (3 days).
#   A warning is issued if the period is shorter.
#   The heatwave_periods dataframe is structured to support multiple
#   heatwave events if the study is extended to other seasons or years
#   add additional rows to heatwave_periods to handle such cases.


assign_heatwave <- function(data_participant) {
  
  # Define heatwave event(s) 
  # Structured as a dataframe to support multiple heatwave events if needed.
  # Add additional rows for other heatwave episodes (different months or years).
  # All timestamps are in UTC; the comparison columns in the data are also UTC.
  heatwave_periods <- data.frame(
    start  = as.POSIXct("2024-06-18 00:00:01", tz = "UTC"),
    end    = as.POSIXct("2024-06-20 23:59:59", tz = "UTC"),
    period = "HW_June"
  )
  
  # Define fixed pre- and post-heatwave windows 
  before_start <- as.POSIXct("2024-06-11 00:00:01", tz = "UTC")
  before_end   <- as.POSIXct("2024-06-17 23:59:59", tz = "UTC")
  after_start  <- as.POSIXct("2024-06-21 00:00:00", tz = "UTC")
  after_end    <- as.POSIXct("2024-06-27 23:59:59", tz = "UTC")
  
  # Compute heatwave duration and validate against the 72-hour criterion 
  # Montreal Public Health defines a heatwave as >= 3 consecutive days above
  # threshold. The warning below flags any heatwave period defined as shorter.
  heatwave_periods$duration_hours <- as.numeric(
    difftime(heatwave_periods$end, heatwave_periods$start, units = "hours")
  )
  
  for (i in seq_len(nrow(heatwave_periods))) {
    if (heatwave_periods$duration_hours[i] <= 71) {
      message(sprintf(
        "WARNING: Heatwave period %s to %s is %.1f hours -- less than the 72-hour Montreal Public Health criterion.",
        format(heatwave_periods$start[i], "%Y-%m-%d"),
        format(heatwave_periods$end[i],   "%Y-%m-%d"),
        heatwave_periods$duration_hours[i]
      ))
    }
  }
  
  # Print heatwave period summary for verification in the knitted document
  print(heatwave_periods)
  
  # Assign heatwave_status using the first available datetime column 
  # Priority: unix_time_converted > sleepEnd_time > date
  # This allows the same function to be called on data at any pipeline stage.
  
  if ("unix_time_converted" %in% names(data_participant)) {
    # Standard path: POSIXct timestamp available (physiological, perceptual data)
    data_participant <- data_participant %>%
      rowwise() %>%
      mutate(
        heatwave_status = case_when(
          unix_time_converted >= heatwave_periods$start &
            unix_time_converted <= heatwave_periods$end   ~ "HW_June",
          unix_time_converted >= before_start &
            unix_time_converted <= before_end             ~ "BEF_HW_June",
          unix_time_converted >= after_start &
            unix_time_converted <= after_end              ~ "AFT_HW_June",
          TRUE                                            ~ "NoHW"
        )
      ) %>%
      ungroup()
    
  } else if ("sleepEnd_time" %in% names(data_participant)) {
    # Sleep analysis path: use awakening time as the observation timestamp
    data_participant <- data_participant %>%
      rowwise() %>%
      mutate(
        heatwave_status = case_when(
          sleepEnd_time >= heatwave_periods$start &
            sleepEnd_time <= heatwave_periods$end   ~ "HW_June",
          sleepEnd_time >= before_start &
            sleepEnd_time <= before_end             ~ "BEF_HW_June",
          sleepEnd_time >= after_start &
            sleepEnd_time <= after_end              ~ "AFT_HW_June",
          TRUE                                      ~ "NoHW"
        )
      ) %>%
      ungroup()
    
  } else if ("date" %in% names(data_participant)) {
    # Pre-aggregated daily data path: only a Date column is available
    data_participant <- data_participant %>%
      rowwise() %>%
      mutate(
        heatwave_status = case_when(
          date >= as.Date("2024-06-18") & date <= as.Date("2024-06-20") ~ "HW_June",
          date >= as.Date("2024-06-11") & date <= as.Date("2024-06-17") ~ "BEF_HW_June",
          date >  as.Date("2024-06-20") & date <= as.Date("2024-06-27") ~ "AFT_HW_June",
          TRUE                                                           ~ "NoHW"
        )
      ) %>%
      ungroup()
  }
  
  # Apply consistent factor ordering across all datasets
  # Ordered chronologically: BEF → HW → AFT → NoHW
  # This ordering is used by ggplot2 and statistical model functions to
  # ensure consistent plot and table presentation across all scripts.
  data_participant <- data_participant %>%
    mutate(
      heatwave_status = factor(
        heatwave_status,
        levels = c("BEF_HW_June", "HW_June", "AFT_HW_June", "NoHW")
      )
    )
  
  return(data_participant)
}
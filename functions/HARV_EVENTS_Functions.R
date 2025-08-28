## =========================================================================
##        FUNCTIONS FOR STORM EVENT IDENTIFICATION & ANALYSIS
## =========================================================================

library(dplyr)
library(lubridate)
library(tidyr)

# Load & preprocess any time series
load_series <- function(file, time_col, format, tz = "UTC") {
  df <- read.csv(file)
  df[[time_col]] <- as.POSIXct(df[[time_col]], format = format, tz = tz)
  arrange(df, .data[[time_col]])
}

find_events <- function(prec_df, time_col, val_col,
                        min_val = 1, gap_hr = 6, min_total = 5, min_dur_hr = 2) {
  prec_df %>%
    mutate(hour = floor_date(.data[[time_col]], "hour")) %>%
    group_by(hour) %>%
    summarise(val = sum(.data[[val_col]], na.rm = TRUE), .groups = "drop") %>%
    filter(val >= min_val) %>%
    arrange(hour) %>%
    mutate(gap = as.numeric(difftime(hour, lag(hour), units = "hours")),
           gap = replace_na(gap, Inf),
           event_id = cumsum(gap > gap_hr)) %>%
    group_by(event_id) %>%
    summarise(start_time = min(hour),
              end_time = max(hour),
              total_val = sum(val),
              duration = as.numeric(difftime(max(hour), min(hour), "hours")) + 1,
              intensity = total_val / (as.numeric(difftime(max(hour), min(hour), "hours")) + 1),
              .groups = "drop") %>%
    filter(total_val > min_total, duration > min_dur_hr) %>%
    arrange(start_time)
}

# Add antecedent totals
add_antecedent <- function(events, prec_df, hours_prior = 24) {
  hourly <- prec_df %>% mutate(hour = floor_date(datetime, "hour")) %>%
    group_by(hour) %>% summarise(val = sum(prec, na.rm = TRUE), .groups = "drop")
  
  events$antecedent <- sapply(1:nrow(events), function(i) {
    st <- events$start_time[i]
    hourly %>%
      filter(hour >= (st - hours(hours_prior)) & hour < st) %>%
      summarise(sum(val)) %>% pull()
  })
  events$total_plus_antecedent <- events$total_val + events$antecedent
  events
}

# Extract event-specific data
extract_events <- function(df, events, time_col) {
  bind_rows(lapply(1:nrow(events), function(i) {
    df %>%
      filter(.data[[time_col]] >= events$start_time[i] &
               .data[[time_col]] <= events$end_time[i]) %>%
      mutate(event_id = events$event_id[i])
  }))
}

# Peak finder
event_peaks <- function(event_df, id_col, time_col, vars) {
  event_df %>%
    group_by(.data[[id_col]]) %>%
    summarise(across(all_of(vars),
                     list(peak = ~max(.x, na.rm = TRUE),
                          peak_time = ~.data[[time_col]][which.max(.x)]),
                     .names = "{.col}_{.fn}"),
              .groups = "drop")
}

# Discharge → depth
flow_to_depth <- function(df, discharge_vars, timestep = 900, areas) {
  for (v in discharge_vars) {
    prefix <- sub("_.*", "", v)
    area <- areas[[prefix]]
    L <- paste0(v, "_L"); m3 <- paste0(v, "_m3"); mm <- paste0(v, "_mm")
    df[[L]]  <- df[[v]] * timestep
    df[[m3]] <- df[[L]] / 1000
    df[[mm]] <- (df[[m3]] / area) * 1000
  }
  df
}

add_drought_period <- function(events_df, drought_split_date) {
  drought_split_date <- as.POSIXct(drought_split_date, tz = "UTC")
  events_df %>%
    mutate(period = if_else(start_time < drought_split_date,
                            "pre-drought", "post-drought"))
}

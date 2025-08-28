## =========================================================================
##        FUNCTIONS FOR RESPONSE TIME ANALYSIS
## =========================================================================

library(dplyr)
library(lubridate)
library(tidyr)
library(ggplot2)
library(purrr)

# --- Function to find the initial response time based on a threshold 
find_initial_response_time <- function(streamflow, timestamps, baseline_window = 6, threshold_factor = 1.1) {
  if (length(streamflow) < baseline_window) return(NA_real_)     # adjust threshold and baseline as needed
  
  baseline_streamflow <- mean(streamflow[1:baseline_window], na.rm = TRUE)
  threshold <- baseline_streamflow * threshold_factor
  
  initial_response_index <- which(streamflow > threshold)[1]
  
  if (!is.na(initial_response_index) && !is.null(initial_response_index)) {
    return(timestamps[initial_response_index])
  } else {
    return(NA_real_)
  }
}

# ---- Function to Calculate Response Times for All Events and Sites
calc_response_times <- function(events, flow_df, datetime_col = "datetime",
                                start_time_col = "start_time", end_time_col = "end_time",
                                site_cols, baseline_window = 6, threshold_factor = 1.1,
                                pre_hours = 6, post_hours = 24) {  # adjust baseline, threshold, and events windows as needed
  
  site_names <- names(site_cols)
  
  # Add extended data window per event
  events_with_data <- events %>%
    rowwise() %>%
    mutate(
      start_time_ext = .data[[start_time_col]] - hours(pre_hours),
      end_time_ext = .data[[end_time_col]] + hours(post_hours),
      data_ext = list(
        flow_df %>%
          filter(.data[[datetime_col]] >= start_time_ext & .data[[datetime_col]] <= end_time_ext)
      )
    ) %>%
    ungroup()
  
  # Calculate initial response times for each site
  for (site in site_names) {
    site_col <- site_cols[[site]]
    col_resp_time <- paste0("init_resp_", site)
    
    events_with_data[[col_resp_time]] <- map2_dbl(
      events_with_data$data_ext,
      events_with_data[[start_time_col]],
      ~ {
        timestamps <- .x[[datetime_col]]
        stream <- .x[[site_col]]
        ts_init <- find_initial_response_time(streamflow = stream, timestamps = timestamps,
                                              baseline_window = baseline_window,
                                              threshold_factor = threshold_factor)
        if (is.na(ts_init)) NA_real_ else as.numeric(difftime(ts_init, .y, units = "hours"))
      }
    )
  }
  
  return(events_with_data)
}

# ---- Function to Reshape and Filter Response Time Data for Analysis
reshape_and_filter_response_times <- function(response_df, response_time_prefix = "init_resp_",
                                              rename_sites = NULL, filter_expr = NULL,
                                              log_transform_vars = NULL,
                                              period_col = NULL, period_cutoff = NULL,
                                              period_labels = c("Before", "After")) {
  # Pivot longer
  long_df <- response_df %>%
    pivot_longer(
      cols = starts_with(response_time_prefix),
      names_to = "site",
      values_to = "response_time"
    ) %>%
    mutate(
      site = sub(response_time_prefix, "", site)
    )
  
  # Rename sites if provided
  if (!is.null(rename_sites)) {
    long_df <- long_df %>%
      mutate(site = recode(site, !!!rename_sites))
  }
  
  # Filter if expression provided
  if (!is.null(filter_expr)) {
    long_df <- long_df %>% filter(!!rlang::enquo(filter_expr))
  }
  
  # Log transform specified variables
  if (!is.null(log_transform_vars)) {
    for (var in log_transform_vars) {
      if (var %in% colnames(long_df)) {
        long_df <- long_df %>% mutate(!!paste0("log_", var) := log(.data[[var]]))
      }
    }
  }
  
  # Create period variable if date and cutoff provided
  if (!is.null(period_col) && !is.null(period_cutoff)) {
    long_df <- long_df %>%
      mutate(period = if_else(.data[[period_col]] < period_cutoff, period_labels[1], period_labels[2]))
  }
  
  return(long_df)
}


# --- Function to Calculate Linear Interaction Models
fit_response_time_models <- function(df_long, response_time_col = "response_time",
                                     predictors = c("P_AR", "rain_int", "period"), # adjust with acual names
                                     site_col = "site",
                                     log_transform = TRUE) {
  # df_long: long format dataframe with response times and predictors
  # response_time_col: name of response time column (string)
  # predictors: vector of predictor variable names (strings)
  # site_col: column indicating site name/id (string)
  # log_transform: whether to log10-transform response and predictors in formula
  
  require(dplyr)
  
  models <- list()
  
  unique_sites <- unique(df_long[[site_col]])
  
  for (site in unique_sites) {
    sub_df <- df_long %>% filter(.data[[site_col]] == site)
    
    # Build formula string, with log10 transformation if specified
    if (log_transform) {
      pred_terms <- paste0("log10(", predictors, ")")
      formula_str <- paste0("log10(", response_time_col, ") ~ ", paste(pred_terms, collapse = " + "))
    } else {
      formula_str <- paste0(response_time_col, " ~ ", paste(predictors, collapse = " + "))
    }
    
    formula_obj <- as.formula(formula_str)
    
    model <- lm(formula_obj, data = sub_df)
    
    models[[site]] <- model
  }
  
  return(models)
}

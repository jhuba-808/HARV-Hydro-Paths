## =========================================================================
##              EXAMLE WORKFLOW FOR HARV_RESP
## =========================================================================

# --- Assume your existing dataframes
# events_df   : with columns at least start_time, end_time, rain_int, total_prec, P_AR, etc.
# flow_df     : with datetime and site-specific flow columns

# --- assign name flow columns
site_columns <- c(
  siteA = "siteA_flow",
  siteB = "siteB_flow",
  siteC = "siteC_flow"
)

# --- Calculate initial response time
events_with_response <- calc_response_times(
  events = events_df,
  flow_df = flow_df,
  datetime_col = "datetime",       # column in flow_df for timestamps
  start_time_col = "start_time",   # event start time col
  end_time_col = "end_time",       # event end time col
  site_cols = site_columns,
  baseline_window = 6,             # adjust baseline and threshold as needed
  threshold_factor = 1.1,            
  pre_hours = 6,                   # adjust pre- and post-event windows as needed
  post_hours = 24
)

# --- clean, filter, reshape, etc.

filter_condition <- expression(
  rain_int > 0,
  response_time > 0,
  P_ARF_24h > 1
)

site_rename_map <- c(
  siteA = "Site A",
  siteB = "Site B",
  siteC = "Site C"
)

# --- log transform
prepared_data <- reshape_and_filter_response_times(
  response_df = events_with_response,
  response_time_prefix = "init_resp_",
  rename_sites = site_rename_map,
  filter_expr = filter_condition,
  log_transform_vars = c("rain_int", "total_prec", "P_AR", "response_time"), # adjust as needed
  period_col = "start_time",
  period_cutoff = as.POSIXct("2016-06-15 00:00", tz = "UTC"),                # adjust datetime as needed
  period_labels = c("Period 1", "Period 2")                                  # include actual assigned names
)

## ------------------------------------------- ##
## --- data now ready for further analysis --- ##
## ------------------------------------------- ##

# --- linear interaction models
models <- fit_response_time_models(
  df_long = prepared_data,
  response_time_col = "response_time",
  predictors = c("P_AR", "rain_int", "period"),        # adjust with actual variable names
  site_col = "site",
  log_transform = TRUE
)

# --- print model summaries
for (site in names(models)) {
  cat("Model summary for", site, ":\n")
  print(summary(models[[site]]))
  cat("\n-----------------------\n")
}
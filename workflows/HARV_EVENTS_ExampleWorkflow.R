## =========================================================================
##                     HARV_EVENTS EXAMPLE WORFLOW
## =========================================================================

# --- Load precipitation data ---
prec <- load_series("precipitation.csv", "datetime", "%Y-%m-%dT%H:%M")
#names(prec) <- c("datetime", "prec") # standardize col names

# --- Find events & antecedent rainfall ---
events <- find_events(
  prec_df = prec,
  time_col = "datetime",
  val_col = "prec",
  min_val = 1,        # Minimum hourly total to consider
  gap_hr = 6,         # Max gap between non-zero hours before new event
  min_total = 5,      # Total rainfall required for a valid event
  min_dur_hr = 2      # Minimum duration (in hours)
)
events <- add_antecedent(events, prec)

# --- Load streamflow data ---
flow <- load_series("streamflow.csv", "datetime", "%Y-%m-%dT%H:%M")
# Columns: datetime, site1_dis, site2_dis, site3_dis

# --- Extract flow per event ---
flow_events <- extract_events(flow, events, "datetime")

# --- Calculate peaks ---
peaks <- event_peaks(flow_events, "event_id", "datetime", c("site1_dis", "site2_dis", "site3_dis"))

# --- Load baseflow/runoff data ---
bf <- load_series("baseflow.csv", "datetime", "%Y-%m-%dT%H:%M")

# --- Extract runoff data & convert to depth ---
bf_events <- extract_events(bf, events, "datetime")
areas <- list(site1 = 500000, site2 = 200000, site3 = 150000) # m²
bf_depth <- flow_to_depth(bf_events, discharge_vars = c("site1_ro","site2_ro","site3_ro","site1_bf","site2_bf","site3_bf"), 900, areas)

# --- Summarize runoff & baseflow per event ---
runoff_summary <- bf_depth %>%
  group_by(event_id) %>%
  summarise(across(ends_with("_mm"), sum, na.rm = TRUE),
            across(c(site1_ro, site2_ro, site3_ro,
                     site1_bf, site2_bf, site3_bf), max, na.rm = TRUE),
            .groups = "drop")

# --- Merge into final analysis table ---
final <- events %>%
  left_join(peaks, by = "event_id") %>%
  left_join(runoff_summary, by = "event_id")

final <- add_drought_period(events, drought_split_date = "2016-06-15")

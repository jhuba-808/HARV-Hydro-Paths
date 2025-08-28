## =========================================================================
##        FUNCTIONS FOR PIECEWISE REGRESSION
## =========================================================================


library(dplyr)
library(lubridate)
library(segmented)
library(ggplot2)

# Add SDFAI category to sdfai_results, join to event dataframe by year/month
add_sdafi_category_and_join <- function(event_df, sdfai_results) {
  library(lubridate)
  library(dplyr)
  
  sdfai_category <- sdfai_results %>%
    mutate(
      year = year(year_month),
      month = month(year_month),
      SDFAI_category = case_when(
        SDFAI > 1.5 ~ "Above Threshold",
        SDFAI < -1.5 ~ "Below Threshold",
        TRUE ~ "Normal"
      )
    ) %>%
    dplyr::select(year, month, SDFAI_category)
  
  event_df <- event_df %>%
    mutate(
      year = year(start_time),
      month = month(start_time)
    ) %>%
    dplyr::left_join(sdfai_category, by = c("year", "month"))
  
  return(event_df)
}

# Run piecewise regression
run_piecewise <- function(df, site_col, period_label, breakpoint = 50) {
  df_period <- df %>%
    filter(period == period_label, !is.na(.data[[site_col]]))
  
  formula_str <- paste(site_col, "~ total_val")
  lm_model <- lm(as.formula(formula_str), data = df_period)
  seg_model <- segmented(lm_model, seg.Z = ~total_val, psi = list(total_val = breakpoint))
  
  list(data = df_period, model = seg_model)
}

# Plot piecewise regression results
plot_piecewise <- function(result, site_name, y_var) {
  seg_model <- result$model
  df_period <- result$data
  
  ggplot(df_period, aes(x = total_val, y = .data[[y_var]])) +
    geom_point(aes(color = SDFAI_category), size = 2) +
    geom_line(aes(y = predict(seg_model, newdata = data.frame(total_val = df_period$total_val))),
              color = "black", size = 1) +
    geom_vline(xintercept = seg_model$psi[1, 1], linetype = "dashed", color = "green") +
    labs(x = "Total Precipitation (mm)",
         y = "Runoff Depth (mm)",
         color = "SDFAI Category",
         title = paste(site_name, "Piecewise Linear Regression")) +
    theme_classic() +
    scale_color_manual(values = c(
      "Above Threshold" = "coral",
      "Below Threshold" = "green3",
      "Normal" = "cornflowerblue"
    )) +
    coord_cartesian(xlim = c(0, 125), ylim = c(0, 20))
}

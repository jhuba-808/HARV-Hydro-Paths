## =========================================================================
##        FUNCTIONS FOR PIECEWISE REGRESSION
## =========================================================================

run_piecewise <- function(df, site_col, period_label, breakpoint = 50) {
  df_period <- df %>%
    filter(period == period_label, !is.na(.data[[site_col]]))
  
  formula_str <- paste(site_col, "~ total_val")
  lm_model <- lm(as.formula(formula_str), data = df_period)
  seg_model <- segmented(lm_model, seg.Z = ~total_val, psi = list(total_val = breakpoint))
  
  list(data = df_period, model = seg_model)
}

# Plot piecewise regression results
plot_piecewise_regression <- function(data, seg_model, stream_name, 
                                      x_var, y_var,
                                      x_label = NULL, y_label = NULL,
                                      xlim = NULL, ylim = NULL) {
  # Extract breakpoint and standard error
  breakpoint <- seg_model$psi[1, 2]
  breakpoint_se <- seg_model$psi[1, 3]
  
  # Confidence interval
  ci_lower <- breakpoint - 1.96 * breakpoint_se
  ci_upper <- breakpoint + 1.96 * breakpoint_se
  
  # Extract slopes
  slopes <- slope(seg_model)[[x_var]]
  slope_before <- slopes[1]
  slope_after <- slopes[2]
  slope_after_label <- sprintf("Slope: %.2f", slope_after)
  
  # Prediction data
  newdata <- data.frame(setNames(list(data[[x_var]]), x_var))
  preds <- predict(seg_model, newdata = newdata)
  
  # Axis labels fallback
  if (is.null(x_label)) x_label <- x_var
  if (is.null(y_label)) y_label <- y_var
  
  # Default axis limits if not specified
  if (is.null(xlim)) xlim <- range(data[[x_var]], na.rm = TRUE)
  if (is.null(ylim)) ylim <- range(data[[y_var]], na.rm = TRUE)
  
  ggplot(data, aes(x = .data[[x_var]], y = .data[[y_var]])) +
    # Shaded confidence interval
    annotate("rect", xmin = ci_lower, xmax = ci_upper,
             ymin = -Inf, ymax = Inf, alpha = 0.2, fill = "steelblue") +
    
    geom_point(size = 2, alpha = 0.6, color = "black") +
    
    # Regression line
    geom_line(aes(x = .data[[x_var]], y = preds),
              color = "orange", size = 1.2) +
    
    # Breakpoint line
    geom_vline(xintercept = breakpoint, linetype = "dashed", color = "steelblue", size = 1) +
    
    # Slope annotation
    annotate("text",
             x = breakpoint + (xlim[2] - breakpoint) * 0.4,
             y = ylim[2] * 0.55,
             label = slope_after_label,
             color = "orange",
             hjust = 0,
             size = 4.5) +
    
    labs(
      x = x_label,
      y = y_label,
      title = paste(stream_name, "Piecewise Linear Regression")
    ) +
    coord_cartesian(xlim = xlim, ylim = ylim) +
    theme_classic(base_size = 13)
}

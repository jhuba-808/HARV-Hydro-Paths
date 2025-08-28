## =========================================================================
##        EXAMPLE WORKFLOW FOR HARV_PWR
## =========================================================================

# Assume df already includes:
# total_val, [site]_ro_mm, period

sites <- c(site1 = "site1_ro_mm", site2 = "site2_ro_mm", site3 = "site3_ro_mm")
periods <- c("Period 1", "Period 2")

results <- list()
plots <- list()

for (site in names(sites)) {
  for (p in periods) {
    res <- run_piecewise(df, sites[[site]], p)
    results[[paste(site, p, sep = "_")]] <- res
    
    plots[[paste(site, p, sep = "_")]] <- plot_piecewise_regression(
      data = res$data,
      seg_model = res$model,
      stream_name = paste(site, p),
      x_var = "total_val",
      y_var = sites[[site]],
      x_label = "x-lab",
      y_label = "y-lab"
    )
  }
}

library(gridExtra)
do.call(grid.arrange, c(plots, ncol = 2))


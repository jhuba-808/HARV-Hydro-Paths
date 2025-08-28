## =========================================================================
##        EXAMPLE WORKFLOW FOR HARV_PWR
## =========================================================================

# Assume df already includes:
# total_prec, [site]_ro_mm, period, SDFAI_category

# Assume `final` is your event dataframe (like storm_peak_summary_filtered)
df_with_sdafi <- add_sdafi_category_and_join(df, sdfai_results)

# Then use final_with_sdafi in your piecewise regression workflow:
sites <- c(site1 = "site1_ro_mm", site2 = "site2_ro_mm", site3 = "site3_ro_mm")
periods <- c("pre-drought", "post-drought")

results <- list()
plots <- list()

for (site in names(sites)) {
  for (p in periods) {
    res <- run_piecewise(df_with_sdafi, sites[[site]], p)
    results[[paste(site, p, sep = "_")]] <- res
    plots[[paste(site, p, sep = "_")]] <- plot_piecewise(res, paste(site, p), sites[[site]])
  }
}

library(gridExtra)
do.call(grid.arrange, c(plots, ncol = 2))

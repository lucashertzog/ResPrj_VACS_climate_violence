tar_load(calc_model_bayesian)
model <- calc_model_bayesian[["constant_and_recent_long"]]

# Load the necessary libraries
library(rstanarm)
library(bayesplot)
library(loo)
library(ggplot2)
library(data.table)

# Print a summary of the model
print(model)

# Plot traceplots for each parameter and save as an image
traceplot_path <- "figures_and_tables/diagnostics/fig_diag_traceplots.png"
traceplot <- plot(model, plotfun = "trace")
ggsave(traceplot_path, plot = traceplot, width = 15, height = 10)

# Extract the summary and save as a CSV file
model_summary <- as.data.frame(summary(model))
write.csv(model_summary, "figures_and_tables/diagnostics/model_summary.csv")

# Check Rhat values
rhat_values <- model_summary$Rhat
print(rhat_values)

# Check Effective Sample Size (ESS)
ess_values <- model_summary$n_eff
print(ess_values)

# Save Rhat and ESS values as a table
diagnostics_table <- data.table(Parameter = rownames(model_summary), Rhat = rhat_values, ESS = ess_values)
write.csv(diagnostics_table, "figures_and_tables/diagnostics/diagnostics_table.csv")

# Generate and save posterior predictive checks plots
pp_dens_overlay <- pp_check(model, plotfun = "dens_overlay")
pp_hist <- pp_check(model, plotfun = "hist")
pp_boxplot <- pp_check(model, plotfun = "boxplot")
pp_scatter <- pp_check(model, plotfun = "scatter")

ggsave("figures_and_tables/diagnostics/pp_dens_overlay.png", plot = pp_dens_overlay, width = 8, height = 6)
ggsave("figures_and_tables/diagnostics/pp_hist.png", plot = pp_hist, width = 8, height = 6)
ggsave("figures_and_tables/diagnostics/pp_boxplot.png", plot = pp_boxplot, width = 8, height = 6)
ggsave("figures_and_tables/diagnostics/pp_scatter.png", plot = pp_scatter, width = 8, height = 6)

# Generate and save posterior predictive distribution overlay plot
y_rep <- posterior_predict(model)
ppc_dens_overlay_plot <- ppc_dens_overlay(y = model$y, yrep = y_rep[1:50, ])
ggsave("figures_and_tables/diagnostics/ppc_dens_overlay.png", plot = ppc_dens_overlay_plot, width = 8, height = 6)

# Generate and save posterior predictive intervals plot
ppc_intervals_plot <- ppc_intervals(y = model$y, yrep = y_rep[1:50, ])
ggsave("figures_and_tables/diagnostics/ppc_intervals.png", plot = ppc_intervals_plot, width = 8, height = 6)

# Calculate and print RMSE and MAE
rmse <- sqrt(mean((model$y - apply(y_rep, 2, mean))^2))
mae <- mean(abs(model$y - apply(y_rep, 2, mean)))
print(paste("RMSE: ", rmse))
print(paste("MAE: ", mae))

# Save RMSE and MAE as a table
error_metrics_table <- data.table(Metric = c("RMSE", "MAE"), Value = c(rmse, mae))
write.csv(error_metrics_table, "figures_and_tables/diagnostics/error_metrics_table.csv")

# Perform and save k-fold cross-validation results
kfold_results <- kfold(model, K = 10)
print(kfold_results)
write.csv(as.data.table(kfold_results), "figures_and_tables/diagnostics/kfold_results.csv")

# Load necessary data and libraries
# tar_load(calc_model_bayesian)
# tar_load(dat_imp)
# library(bayesplot)
# library(ggplot2)
# library(data.table)
# 
# library(rstantools)

do_posterior_predictive <- function(
    calc_model_bayesian,
    dat_imp
){
  


# Filter and preprocess the data
psu_counts <- dat_imp[, .(n_psu = uniqueN(cluster)), by = strata]
valid_strata <- psu_counts[n_psu > 1, strata]
dat_vacs_filtered <- dat_imp[strata %in% valid_strata]

# Convert factor to numeric
dat_vacs_filtered$viol_sex.imputed <- as.numeric(as.character(dat_vacs_filtered$viol_sex.imputed))

# Replace '98' with NA, and keep '0' and '1' as is
dat_vacs_filtered$viol_sex.imputed <- ifelse(dat_vacs_filtered$viol_sex.imputed == 98, NA, 
                                             dat_vacs_filtered$viol_sex.imputed)

# Define a list of custom labels for the predictors
custom_labels <- list(
  slight_to_moderate = "Slight to Moderate Drought",
  very_dry_drought_extreme = "Very Dry",
  constant_drought_extreme = "Prolonged and extreme",
  recent_long_period = "Recent and Long"
)

# Loop through each model in calc_model_bayesian
for (predictor in names(calc_model_bayesian)) {
  # Extract the model
  model <- calc_model_bayesian[[predictor]]
  
  # Generate posterior predictive samples
  y_rep <- posterior_predict(model)
  print(dim(y_rep))  # Print the dimensions of y_rep
  
  # Extract the observed data
  y_obs <- dat_vacs_filtered$viol_sex.imputed
  print(length(y_obs))  # Print the length of y_obs
  
  # Identify non-NA indices in the observed data
  non_na_indices <- which(!is.na(y_obs))
  y_obs_non_na <- y_obs[non_na_indices]
  print(length(y_obs_non_na))  # Print the length of y_obs_non_na
  
  # Ensure y_rep is a matrix
  y_rep <- as.matrix(y_rep)
  
  # Align the observations based on non-NA indices
  valid_indices <- non_na_indices[1:ncol(y_rep)]
  y_obs_aligned <- y_obs[valid_indices]
  y_rep_aligned <- y_rep[, 1:length(valid_indices), drop = FALSE]
  
  # Verify the alignment
  print(length(y_obs_aligned))  # Print the length of aligned y_obs
  print(dim(y_rep_aligned))  # Print the dimensions of aligned y_rep
  
  # Convert posterior predictions to binary outcomes using a threshold (e.g., 0.5)
  predicted_binary <- apply(y_rep_aligned, 2, function(x) ifelse(mean(x) > 0.5, 1, 0))
  
  # Align predictions to non-NA indices
  predicted_binary_aligned <- predicted_binary[1:length(valid_indices)]
  
  # Summarize the observed data
  observed_summary <- table(y_obs_aligned)
  print("Observed summary:")
  print(observed_summary)
  
  # Summarize the predicted data
  predicted_summary <- table(predicted_binary_aligned)
  print("Predicted summary:")
  print(predicted_summary)
  
  # Create a data frame for plotting
  summary_df <- data.table(
    Outcome = rep(c("0", "1"), each = 2),
    Type = rep(c("Observed", "Predicted"), 2),
    Count = c(as.numeric(observed_summary), as.numeric(predicted_summary))
  )
  
  # Create bar plot
  p <- ggplot(summary_df, aes(x = Outcome, y = Count, fill = Type)) +
    geom_bar(stat = "identity", position = "dodge") +
    labs(title = custom_labels[[predictor]], x = "Outcome", y = "Count") +
    theme_minimal()
  
  # Display the plot
  print(p)
  
  # Save the plot to a file
  ggsave(filename = paste0("figures_and_tables/diagnostics/ppc_", custom_labels[[predictor]], ".png"), plot = p)
}
return(p)
}

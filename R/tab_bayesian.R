tab_bayesian <- function(
    calc_model_bayesian
){
control_var <- c(
  "age.imputed",
  "edu_enrol.imputed",
  "pvt.imputed",
  "marital.imputed"
)

outcome_var <- "viol_sex.imputed"
pred_var <- c(
  "zero_to_moderate",
  "very_dry_drought_extreme",
  "constant_drought_extreme"
  ,
  "recent_long_period"
)
extract_model_results <- function(calc_model_bayesian, outcome_var, control_var) {
  # Create an empty list to store the extracted results for each predictor
  extracted_results <- list()
  
  # Loop over each model in the calc_model_bayesian
  for (predictor in names(calc_model_bayesian)) {
    model <- calc_model_bayesian[[predictor]]
    
    # Extract the posterior samples
    posterior_samples <- as.matrix(model)
    
    # Extract the posterior means (point estimates)
    point_estimates <- apply(posterior_samples, 2, mean)
    
    # Calculate odds ratios by exponentiating the coefficients
    odds_ratios <- exp(point_estimates)
    
    # Extract standard errors (posterior standard deviations)
    standard_errors <- apply(posterior_samples, 2, sd)
    
    # Extract 95% credible intervals for the fixed effects coefficients
    ci <- posterior_interval(model, prob = 0.95)
    
    # Calculate 95% credible intervals for the odds ratios
    ci_lower <- exp(ci[, 1])
    ci_upper <- exp(ci[, 2])
    
    # Calculate posterior probabilities of the coefficients being positive
    posterior_probabilities <- apply(posterior_samples, 2, function(x) mean(x > 0))
    
    # Estimate the number of observations used in the model
    n <- nrow(model$data)
    
    # Create a data frame with the results for the current predictor
    predictor_results <- data.frame(
      Estimate = point_estimates,
      StdError = standard_errors,
      OddsRatio = odds_ratios,
      CI_Lower = ci_lower,
      CI_Upper = ci_upper,
      Posterior_Probability = posterior_probabilities,
      Predictor = predictor,
      n = n
    )
    
    # Store the results in the extracted results list
    extracted_results[[predictor]] <- predictor_results
  }
  
  # Combine the results into a single data frame
  combined_results <- do.call(rbind, extracted_results)
  
  return(combined_results)
}
combined_results <- extract_model_results(calc_model_bayesian, outcome_var, control_var)

# Define the rows to keep
rows_to_keep <- c(
  "zero_to_moderate.zero_to_moderate1",
  "very_dry_drought_extreme.very_dry_drought_extreme1",
  "constant_drought_extreme.constant_drought_extreme1"
   ,
   "recent_long_period.recent_long_period1"
)

# Define the columns to keep and round
columns_to_keep <- c("Estimate", "StdError", "OddsRatio", "CI_Lower", "CI_Upper", "Posterior_Probability")

# Filter the rows
filtered_results <- combined_results[rownames(combined_results) %in% rows_to_keep, columns_to_keep]

# Round the specified columns to 2 decimal places
filtered_results <- round(filtered_results, 3)

# Print the filtered and rounded results
return(filtered_results)
}

do_model_bayesian <- function(
    dat_imp
){

control_var <- c(
  "age.imputed",
  "edu_enrol.imputed",
  "pvt",
  "marital.imputed"
)

outcome_var <- c("viol_sex.imputed")
pred_var <- c(
  "zero_to_moderate",
  "very_dry_drought_extreme",
  "constant_and_recent_long",
  "recent_long_period"
)

# Filter valid strata and recode variables
psu_counts <- dat_imp[, .(n_psu = uniqueN(cluster)), by = strata]
valid_strata <- psu_counts[n_psu > 1, strata]
dat_vacs_filtered <- dat_imp[strata %in% valid_strata]

dat_vacs_filtered$viol_sex.imputed <- ifelse(dat_vacs_filtered$viol_sex.imputed == "Not Applicable", NA, as.integer(dat_vacs_filtered$viol_sex.imputed == "Yes"))
dat_vacs_filtered$marital.imputed <- as.integer(dat_vacs_filtered$marital.imputed == "Yes")
dat_vacs_filtered$edu_enrol.imputed <- as.integer(dat_vacs_filtered$edu_enrol.imputed == "Yes")
dat_vacs_filtered$pvt <- as.integer(dat_vacs_filtered$pvt == "Yes")

# Survey design
design <- svydesign(
  id = ~ cluster,
  strata = ~ strata,
  weights = ~ weight,
  data = dat_vacs_filtered,
  single = "centered",
  nest = TRUE
)

# Calculate pseudo-maximum likelihood weights
pseudo_weights <- weights(design, type = "pml")

# Add pseudo-maximum likelihood weights to the data
dat_vacs_filtered$pseudo_weight <- pseudo_weights

# Function to fit mixed effects model with survey weights using rstanarm
fit_mixed_effects <- function(outcome, predictor, control_vars, data) {
  formula <- as.formula(paste(outcome, "~", predictor, "+", paste(control_vars, collapse = "+"), "+ (1 | adm0)"))
  stan_glmer(
    formula = formula,
    data = data,
    family = binomial(),
    weights = pseudo_weight,
    prior = normal(0, 1),
    prior_intercept = normal(0, 1),
    chains = 4, iter = 1000, seed = 12345,  # Increase number of chains and iterations
    adapt_delta = 0.99
  )
}

# Create an empty list to store the results for each predictor
results_list <- list()

# Loop over each predictor in pred_var
for (predictor in pred_var) {
  # Fit the model
  model <- fit_mixed_effects(outcome_var, predictor, control_var, dat_vacs_filtered)
  results_list[[predictor]] <- model
}

# Print the results list
return(results_list)
}



# # Extract the posterior samples
# posterior_samples <- as.matrix(model)
# 
# # Extract the posterior means (point estimates)
# point_estimates <- apply(posterior_samples, 2, mean)
# 
# # Calculate odds ratios by exponentiating the coefficients
# odds_ratios <- exp(point_estimates)
# 
# # Extract standard errors (posterior standard deviations)
# standard_errors <- apply(posterior_samples, 2, sd)
# 
# # Extract 95% credible intervals for the fixed effects coefficients
# ci <- posterior_interval(model, prob = 0.95)
# 
# # Calculate 95% credible intervals for the odds ratios
# ci_lower <- exp(ci[, 1])
# ci_upper <- exp(ci[, 2])
# 
# # Calculate posterior probabilities of the coefficients being positive
# posterior_probabilities <- apply(posterior_samples, 2, function(x) mean(x > 0))
# 
# # Create a data frame with the results for the current predictor
# predictor_results <- data.frame(
#   Estimate = point_estimates,
#   StdError = standard_errors,
#   OddsRatio = odds_ratios,
#   CI_Lower = ci_lower,
#   CI_Upper = ci_upper,
#   Posterior_Probability = posterior_probabilities,
#   Predictor = predictor,
#   n = nrow(na.omit(dat_vacs_filtered[, c(outcome_var, predictor, control_var), with = FALSE]))
# )
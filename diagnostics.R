tar_load(calc_model_bayesian)
model <- calc_model_bayesian[["constant_and_recent_long"]]

# Load the necessary library
library(rstanarm)

# Print a summary of the model
print(model)

# Plot traceplots for each parameter
plot(model, plotfun = "trace")

# Extract the summary
model_summary <- as.data.frame(summary(model))

# Check Rhat values
rhat_values <- model_summary$Rhat
print(rhat_values)

# Check Effective Sample Size (ESS)
ess_values <- model_summary$n_eff
print(ess_values)


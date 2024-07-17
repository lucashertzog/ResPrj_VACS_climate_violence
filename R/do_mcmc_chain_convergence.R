# library(rstan)
do_mcmc_chain_convergence <- function(
    calc_model_bayesian
){
  


# Define custom labels for predictors
custom_labels <- c(
  "zero_to_moderate" = "Zero to moderate drought",
  "very_dry_drought_extreme" = "Very dry",
  "constant_drought_extreme" = "Prolonged and extreme",
  "recent_long_period" = "Recent and long"
)

# Create an empty list to store Rhat values
rhat_values <- list()

# Function to extract Rhat values from a rstanarm model
extract_rhat <- function(stanfit) {
  # Extract summary which includes Rhat values
  summary_stanfit <- summary(stanfit)
  rhat_values <- summary_stanfit$summary[, "Rhat"]
  return(rhat_values)
}

# Loop over each model in calc_model_bayesian
for (predictor in names(calc_model_bayesian)) {
  # Extract the model
  model <- calc_model_bayesian[[predictor]]
  
  # Extract Rhat values
  rhat <- extract_rhat(model$stanfit)
  rhat_values[[predictor]] <- rhat
}

# Combine Rhat values into a single data table
rhat_table <- data.table(Parameter = NULL)
for (predictor in names(rhat_values)) {
  rhat_dt <- data.table(Parameter = names(rhat_values[[predictor]]),
                        Rhat = as.vector(rhat_values[[predictor]]),
                        Model = custom_labels[[predictor]])
  rhat_table <- rbind(rhat_table, rhat_dt, fill = TRUE)
}

# Format the table for better readability
rhat_table <- dcast(rhat_table, Parameter ~ Model, value.var = "Rhat")

# Optionally, save the table to a CSV file
write.csv(rhat_table, "figures_and_tables/diagnostics/rhat_values.csv", row.names = FALSE)
return(rhat_table)
}
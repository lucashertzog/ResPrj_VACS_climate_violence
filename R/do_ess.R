# 
# library(rstan)

do_ess <- function(
    calc_model_bayesian
){
  

# Define custom labels for predictors
custom_labels <- c(
  "slight_to_moderate" = "Zero to moderate drought",
  "very_dry_drought_extreme" = "Very dry",
  "constant_drought_extreme" = "Prolonged and extreme",
  "recent_long_period" = "Recent and long"
)

# Create an empty list to store ESS values
ess_values <- list()

# Function to extract ESS values from a rstanarm model
extract_ess <- function(stanfit) {
  # Extract summary which includes ESS values
  summary_stanfit <- summary(stanfit)
  ess_values <- summary_stanfit$summary[, "n_eff"]
  return(ess_values)
}

# Loop over each model in calc_model_bayesian
for (predictor in names(calc_model_bayesian)) {
  # Extract the model
  model <- calc_model_bayesian[[predictor]]
  
  # Extract ESS values
  ess <- extract_ess(model$stanfit)
  ess_values[[predictor]] <- ess
}

# Combine ESS values into a single data table
ess_table <- data.table(Parameter = NULL)
for (predictor in names(ess_values)) {
  ess_dt <- data.table(Parameter = names(ess_values[[predictor]]),
                       ESS = as.vector(ess_values[[predictor]]),
                       Model = custom_labels[[predictor]])
  ess_table <- rbind(ess_table, ess_dt, fill = TRUE)
}

# Format the table for better readability
ess_table <- dcast(ess_table, Parameter ~ Model, value.var = "ESS")

# Optionally, save the table to a CSV file
write.csv(ess_table, "figures_and_tables/diagnostics/ess_values.csv", row.names = FALSE)

return(ess_table)
}
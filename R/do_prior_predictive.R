# # Load necessary packages
# library(rstanarm)
# library(data.table)
# library(ggplot2)
# library(scales) 

do_prior_predictive <- function(
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
  "constant_drought_extreme",
  "recent_long_period"
)

# Assuming you have the data filtered and prepared as in your provided code
# Filter valid strata and recode variables
psu_counts <- dat_imp[, .(n_psu = uniqueN(cluster)), by = strata]
valid_strata <- psu_counts[n_psu > 1, strata]
dat_vacs_filtered <- dat_imp[strata %in% valid_strata]

# Convert factor to numeric
dat_vacs_filtered$viol_sex.imputed <- as.numeric(as.character(dat_vacs_filtered$viol_sex.imputed))

# Replace '98' with NA, and keep '0' and '1' as is
dat_vacs_filtered$viol_sex.imputed <- ifelse(dat_vacs_filtered$viol_sex.imputed == 98, NA, 
                                             dat_vacs_filtered$viol_sex.imputed)

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
pseudo_weight <- weights(design, type = "pml")
# Define custom labels for predictors
custom_labels <- c(
  "zero_to_moderate" = "Zero to moderate drought",
  "very_dry_drought_extreme" = "Very dry",
  "constant_drought_extreme" = "Prolonged and extreme",
  "recent_long_period" = "Recent and long"
)

# Function to conduct prior predictive check using rstanarm
prior_predictive_check <- function(outcome, predictor, control_vars, data) {
  formula <- as.formula(paste(outcome, "~", predictor, "+", paste(control_vars, collapse = "+"), "+ (1 | adm0)"))
  prior_only_model <- stan_glmer(
    formula = formula,
    data = data,
    family = binomial(),
    weights = pseudo_weight,
    prior_PD = TRUE,  # Simulate from prior only
    prior = normal(0, 1),  # Weakly informative prior
    prior_intercept = normal(0, 1),
    chains = 4, iter = 1000, seed = 12345,  # Increase number of chains and iterations
    adapt_delta = 0.99
  )
  
  # Generate prior predictive samples
  y_prior_pred <- posterior_predict(prior_only_model, draws = 1000)
  
  # Return simulated data
  return(y_prior_pred)
}

# Create an empty list to store the prior predictive check results for each predictor
prior_results_list <- list()

# Create directory if it doesn't exist
# dir.create("figures_and_tables/diagnostics", recursive = TRUE, showWarnings = FALSE)

# Loop over each predictor in pred_var
for (predictor in pred_var) {
  # Conduct prior predictive check
  y_prior_pred <- prior_predictive_check(outcome_var, predictor, control_var, dat_vacs_filtered)
  prior_results_list[[predictor]] <- y_prior_pred
  
  # Summarize the simulated data
  simulated_data <- data.table(y_prior_pred)
  melted_data <- melt(simulated_data, variable.name = "Draw", value.name = "Simulated")
  
  # Create bar plot
  summary_data <- melted_data[, .N, by = .(Simulated)]
  p <- ggplot(summary_data, aes(x = as.factor(Simulated), y = N)) +
    geom_bar(stat = "identity", fill = "blue", alpha = 0.5) +
    scale_y_continuous(labels = comma) +  # Remove scientific notation
    labs(title = custom_labels[[predictor]],
         x = "Simulated Values", y = "Frequency") +
    theme_minimal()
  
  # Save the plot to a file
  ggsave(filename = paste0("figures_and_tables/diagnostics/prior_predictive_check_", predictor, ".png"), plot = p)
}

}
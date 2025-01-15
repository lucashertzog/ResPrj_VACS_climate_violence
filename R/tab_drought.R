# tar_load(dat_mrg)
tab_drought <- function(
    dat_mrg
){
  # Ensure dat_mrg is treated as a data.table
  setDT(dat_mrg)
  
  # Calculate exclusive counts for each drought condition
  dat_mrg[, zero_to_moderate_exclusive := as.integer(zero_to_moderate == 1 & very_dry_drought_extreme == 0 & constant_drought_extreme == 0 & recent_long_period == 0)]
  dat_mrg[, very_dry_exclusive := as.integer(very_dry_drought_extreme == 1 & constant_drought_extreme == 0 & recent_long_period == 0)]
  dat_mrg[, recent_long_exclusive := as.integer(recent_long_period == 1 & constant_drought_extreme == 0)]
  dat_mrg[, constant_drought_extreme_exclusive := as.integer(constant_drought_extreme == 1)]  # This is naturally exclusive if defined correctly
  
  sample_size <- nrow(dat_mrg)
  
  # Update drought_exposure with exclusive counts
  drought_exposure <- data.table(
    Drought_Pattern = c("Zero_to_Moderate", "Very_Dry","Long", "Extreme"),
    Sample = c(
      sprintf("%d (%.2f)", sum(dat_mrg$zero_to_moderate_exclusive, na.rm = TRUE), 
              sum(dat_mrg$zero_to_moderate_exclusive, na.rm = TRUE) / sample_size * 100),
      sprintf("%d (%.2f)", sum(dat_mrg$very_dry_exclusive, na.rm = TRUE), 
              sum(dat_mrg$very_dry_exclusive, na.rm = TRUE) / sample_size * 100),
      sprintf("%d (%.2f)", sum(dat_mrg$recent_long_exclusive, na.rm = TRUE), 
              sum(dat_mrg$recent_long_exclusive, na.rm = TRUE) / sample_size * 100),
      sprintf("%d (%.2f)", sum(dat_mrg$constant_drought_extreme_exclusive, na.rm = TRUE), 
              sum(dat_mrg$constant_drought_extreme_exclusive, na.rm = TRUE) / sample_size * 100)

    )
  )
  
  # Add description column
  drought_exposure[, Description := ""]
  

return(drought_exposure)
}

# # Identify rows not falling into any exclusive category
# dat_mrg[, residual_category := as.integer(
#   zero_to_moderate_exclusive == 0 &
#     very_dry_exclusive == 0 &
#     recent_long_exclusive == 0 &
#     constant_drought_extreme_exclusive == 0
# )]
# 
# # Count the number of residual cases
# num_residual <- sum(dat_mrg$residual_category, na.rm = TRUE)
# cat("Number of residual cases:", num_residual, "\n")
# 
# # Inspect the residual rows
# residual_rows <- dat_mrg[residual_category == 1]
# 
# # View the residual rows
# head(residual_rows)
# 
# missing_counts <- dat_mrg[is.na(zero_to_moderate) | is.na(very_dry_drought_extreme) |
#                             is.na(recent_long_period) | is.na(constant_drought_extreme)]
# 
# cat("Number of rows with missing data:", nrow(missing_counts), "\n")


# tar_load(dat_mrg)
# 
# # Create a unique combination key for each row based on the four drought conditions
# dat_mrg[, combination := paste(
#   zero_to_moderate,
#   very_dry_drought_extreme,
#   constant_and_recent_long,
#   recent_long_period,
#   sep = "_")]
# 
# # Count the number of occurrences of each combination
# combination_counts <- dat_mrg[, .N, by = combination]
# 
# # Sort to see the most common combinations
# combination_counts <- combination_counts[order(-N)]
# 
# # Print the results
# print(combination_counts)

# tar_load(calc_drought)
# 
# # Filter data for extreme drought conditions
# constant <- calc_drought[constant_drought_extreme == 1, ]
# 
# # Calculate the range of 'both_methods_months'
# drought_month_range <- range(constant$both_methods_months, na.rm = TRUE)
# 
# # Display the results
# print(paste("The range of months experiencing constant and extreme drought conditions falls between",
#             drought_month_range[1], "and", drought_month_range[2], "months."))
# 
# #[1] "The range of months experiencing constant and extreme drought conditions falls between 8 and 44 months."

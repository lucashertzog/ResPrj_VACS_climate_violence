sens_tab_drought <- function(
    sens_dat_mrg
){
  # Ensure dat_mrg is treated as a data.table
  setDT(sens_dat_mrg)
  
  # Calculate exclusive counts for each drought condition
  sens_dat_mrg[, zero_to_moderate_exclusive := as.integer(zero_to_moderate == 1 & very_dry_drought_extreme == 0 & constant_and_recent_long == 0 & recent_long_period == 0)]
  sens_dat_mrg[, very_dry_exclusive := as.integer(very_dry_drought_extreme == 1 & constant_and_recent_long == 0 & recent_long_period == 0)]
  sens_dat_mrg[, recent_long_exclusive := as.integer(recent_long_period == 1 & constant_and_recent_long == 0)]
  sens_dat_mrg[, constant_and_recent_exclusive := as.integer(constant_and_recent_long == 1)]  # This is naturally exclusive if defined correctly
  
  sample_size <- nrow(sens_dat_mrg)
  
  # Update drought_exposure with exclusive counts
  drought_exposure <- data.table(
    Drought_Pattern = c("Zero_to_Moderate", "Very_Dry","Long", "Extreme"),
    Sample = c(
      sprintf("%d (%.2f)", sum(sens_dat_mrg$zero_to_moderate_exclusive, na.rm = TRUE), 
              sum(sens_dat_mrg$zero_to_moderate_exclusive, na.rm = TRUE) / sample_size * 100),
      sprintf("%d (%.2f)", sum(sens_dat_mrg$very_dry_exclusive, na.rm = TRUE), 
              sum(sens_dat_mrg$very_dry_exclusive, na.rm = TRUE) / sample_size * 100),
      sprintf("%d (%.2f)", sum(sens_dat_mrg$recent_long_exclusive, na.rm = TRUE), 
              sum(sens_dat_mrg$recent_long_exclusive, na.rm = TRUE) / sample_size * 100),
      sprintf("%d (%.2f)", sum(sens_dat_mrg$constant_and_recent_exclusive, na.rm = TRUE), 
              sum(sens_dat_mrg$constant_and_recent_exclusive, na.rm = TRUE) / sample_size * 100)
      
    )
  )
  
  # Add description column
  drought_exposure[, Description := ""]
  
  
  return(drought_exposure)
}

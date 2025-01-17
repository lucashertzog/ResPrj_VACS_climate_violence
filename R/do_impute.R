do_impute <- function(
    dat_mrg
){

  dat_mrg <- as.data.frame(dat_mrg)

  vars <- c("age", "marital", "edu_enrol", "viol_sex", "ever_viol_sex", "pvt",
            "slight_to_moderate",
            "very_dry_drought_extreme",
            "constant_drought_extreme",
            "recent_long_period")
  
  vars_impute <- dat_mrg[vars]
  
  vars_impute$age <- as.factor(vars_impute$age)

  imp <- missForest(vars_impute, maxiter = 10, ntree = 100)
  
  imputed_data <- imp$ximp
  
  names(imputed_data) <- paste0(names(imputed_data), ".imputed")
  
  imp <- cbind(dat_mrg, imputed_data)
  
  imp$age.imputed <- as.numeric(as.character(imp$age.imputed))
  
  setDT(imp)
  
  return(imp)
}
do_agg_drought <- function(
    calc_drought
){
  calc_drought[, zero_to_moderate := as.integer(!(
    constant_drought_extreme | very_dry_drought_extreme | constant_and_recent_long | recent_long_period )
    & (value > -1 & value < 0)
  )]
  
  spei <- calc_drought[, .(
    very_dry_drought_extreme = as.integer(any(very_dry_drought_extreme == 1)),
    recent_long_period = as.integer(any(recent_long_period == 1)),
    constant_drought_extreme = as.integer(any(constant_drought_extreme == 1)),
    constant_and_recent_long = as.integer(any(constant_and_recent_long == 1)),
    zero_to_moderate = as.integer(any(zero_to_moderate == 1))
  ), by = .(adm0, adm1, adm2)]
  

return(spei)
}
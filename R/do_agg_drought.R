do_agg_drought <- function(
    calc_drought
){
  
# we dont need all the dates, as we are creating dichotomous
spei <- calc_drought[, .(
  zero_to_moderate = as.integer(any(zero_to_moderate == 1)),
  very_dry_drought_extreme = as.integer(any(very_dry_drought_extreme == 1)),
  recent_long_period = as.integer(any(recent_long_period == 1)),
  constant_drought_extreme = as.integer(any(constant_drought_extreme == 1)),
  constant_and_recent_long = as.integer(any(constant_and_recent_long == 1))
), by = .(adm0, adm1, adm2)]


# lets check
foo <- spei[, .(
  num_zero_to_moderate = sum(zero_to_moderate, na.rm = TRUE),
  num_very_dry = sum(very_dry_drought_extreme, na.rm = TRUE),
  num_recent_long_period = sum(recent_long_period, na.rm = TRUE),
  num_constant = sum(constant_drought_extreme, na.rm = TRUE),
  num_constant_and_recent_long = sum(constant_and_recent_long, na.rm = TRUE)
)]
return(spei)
}
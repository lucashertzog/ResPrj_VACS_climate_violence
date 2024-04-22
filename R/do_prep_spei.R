# tar_load(dat_spei)
# library(lubridate)
# library(data.table)

do_prep_spei <- function(
    dat_spei
){
  
svy_dates <- data.table(
adm0 = c(
  "Cambodia", "Malawi", "Nigeria", "Uganda", "Zambia","Colombia", 
  "Côte d'Ivoire", "El Salvador", "Kenya", "Lesotho", "Moldova", 
  "Mozambique", "Namibia", "Zimbabwe"),
date = as.Date(c(
  "2013-02-01", "2013-09-01", "2014-05-01", "2015-09-01", "2014-08-01",
  "2018-08-01", "2018-06-01", "2017-11-01", "2018-12-01", "2018-06-01",
  "2019-03-01", "2019-07-01", "2019-03-01", "2017-01-01"))
)

svy_dates[, `:=` (
  prev_24m = date %m-% months(24),  # Start of the 24-month period
  survey_date = date  # End of the period, inclusive
)]

spei <- dat_spei[svy_dates, on = .(adm0), allow.cartesian = TRUE]

spei <- spei[date >= prev_24m & date <= survey_date]
 
spei <- spei[, .(adm0, adm1, adm2, date, value)]

adm1_countries <- c("Côte d'Ivoire", "Lesotho", "Moldova", "Mozambique")
adm2_countries <- c(
  "Cambodia", "Malawi", "Nigeria", "Uganda", "Zambia","Colombia", 
  "El Salvador", "Kenya", "Namibia", "Zimbabwe")

adm1_spei <- spei[adm0%in%adm1_countries]
adm2_spei <- spei[adm0%in%adm2_countries]

mean_adm1 <- adm1_spei[, .(value = mean(value)),
                       by = .(adm0, adm1, date)]

mean_adm1[, adm2 := NA_character_]

foo <- rbind(mean_adm1, adm2_spei)

foo[, dry_month := ifelse(value < -1, 1, 0)]

foo[, count_dry_months := sum(dry_month == 1, na.rm = TRUE), by = ifelse(adm0 %in% c("Côte d'Ivoire", "Lesotho", "Moldova", "Mozambique"), adm1, adm2)]

# Duration measure: count method 1 if count_dry_months >= 5, otherwise 0
foo[, count_method := ifelse(count_dry_months >= 5, 1, 0), by = ifelse(adm0 %in% c("Côte d'Ivoire", "Lesotho", "Moldova", "Mozambique"), adm1, adm2)]

# Now calculate the sum-method for drought severity
foo[, sum_reset := cumsum(value * (value < -1)), by = .(adm0, ifelse(adm0 %in% adm1_countries, adm1, adm2))]
foo[, reset := value > -1]
foo[, group_id := rleid(reset), by = .(adm0, ifelse(adm0 %in% adm1_countries, adm1, adm2))]
foo[, sum_reset := cumsum(value * (value < -1)), by = .(adm0, ifelse(adm0 %in% adm1_countries, adm1, adm2), group_id)]

# Evaluate the condition of reaching the threshold of -17.5 and reset if value > -1
foo[, threshold_reached := sum_reset <= -17.5, by = .(adm0, ifelse(adm0 %in% adm1_countries, adm1, adm2), group_id)]
foo[, count_threshold_months := sum(threshold_reached), 
    by = .(adm0, ifelse(adm0 %in% adm1_countries, adm1, adm2))]

return(foo)
}
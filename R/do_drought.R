# tar_load(dat_spei)
# library(lubridate)
# library(data.table)

do_drought <- function(
    dat_spei
){

spei <- dat_spei

adm1_countries <- c("Côte d'Ivoire", "Lesotho", "Moldova", "Mozambique")
adm2_countries <- c(
  "Cambodia", "Malawi", "Nigeria", "Uganda", "Zambia","Colombia", 
  "El Salvador", "Kenya", "Namibia", "Zimbabwe")

# split by admin that we have
adm1_spei <- spei[adm0%in%adm1_countries]
adm2_spei <- spei[adm0%in%adm2_countries]

mean_adm1 <- adm1_spei[, .(value = mean(value)),by = .(adm0, adm1, date)]
mean_adm1[, adm2 := NA_character_]

spei <- rbind(mean_adm1, adm2_spei)

# Add a new column for group identification based on conditions
spei[, group_id := ifelse(adm0 %in% c("Côte d'Ivoire", "Lesotho", "Moldova", "Mozambique"), 
                          paste(adm0, adm1), 
                          paste(adm0, adm1, adm2))]

# method inspired by O'Brien 2014. differences: O'Brien uses Hutchinson and we 
# use SPEI. timeframe selected in Australia was 7y due to the big dry. Also,
# HILDA had question about residence in previous waves (certainty on the stability
# of the weather exposure). In our study, we use 48 months adapting the 84 months
# O'Brien uses. Andrea Low's study in Lesotho uses 24m, which we consider a short
# timeframe as may not capture long duration drought

### Count Method ####
# Duration measure: The ‘count-method’ index #

# flag dry months
spei[, dry_month := ifelse(value < -1, 1, 0)]

# Reset condition for ending a drought
spei[, reset := value > -1]

# Create a running total of consecutive dry months, reset when not dry
spei[, consecutive_dry := cumsum(dry_month) - cummax(cumsum(dry_month) * reset), 
     by = .(adm0, ifelse(adm0 %in% adm1_countries, adm1, adm2))]

# Identify start of drought when there are at least 5 consecutive dry months
spei[, in_drought := fifelse(consecutive_dry >= 5, 1, 0)]

# Reset drought count whenever reset condition is met
spei[, drought_count := cumsum(in_drought) - cummax(cumsum(in_drought) * reset), 
     by = .(adm0, ifelse(adm0 %in% adm1_countries, adm1, adm2))]

# Calculate total number of months recorded in drought periods
spei[, count_method := sum(drought_count > 0), by = .(adm0, ifelse(adm0 %in% adm1_countries, adm1, adm2))]

### Sum Method as O'Brien 2014 ####
# Duration and Intensity: The ‘sum-method’ index# 

# resetting logic for drought severity sum-method
spei[, group_id := rleid(reset), by = .(adm0, ifelse(adm0 %in% adm1_countries, adm1, adm2))]

# Cumulative sum considering the resets
spei[, sum_reset := cumsum(value * (value < -1)), by = .(adm0, ifelse(adm0 %in% adm1_countries, adm1, adm2), group_id)]

# if threshold of -17.5 is reached, considering resets
spei[, threshold := sum_reset <= -17.5, by = .(adm0, ifelse(adm0 %in% adm1_countries, adm1, adm2), group_id)]

# total number of months recorded in sum-method drought periods
spei[, sum_method := sum(threshold, na.rm = TRUE), by = .(adm0, ifelse(adm0 %in% adm1_countries, adm1, adm2))]


### Cycles of drought ####
# For Sum-method cycles
spei[, sum_drought_start := shift(threshold, fill = FALSE) == FALSE & threshold == TRUE,
     by = .(adm0, ifelse(adm0 %in% adm1_countries, adm1, adm2))]
spei[, sum_method_cycles := sum(sum_drought_start, na.rm = TRUE), 
     by = .(adm0, ifelse(adm0 %in% adm1_countries, adm1, adm2))]

# For Count-method cycles
spei[, count_drought_start := shift(in_drought, fill = FALSE) == FALSE & in_drought == TRUE,
     by = .(adm0, ifelse(adm0 %in% adm1_countries, adm1, adm2))]
spei[, count_method_cycles := sum(count_drought_start, na.rm = TRUE), 
     by = .(adm0, ifelse(adm0 %in% adm1_countries, adm1, adm2))]

# For Both-methods cycles
spei[, both_methods_start := (shift(threshold, fill = FALSE) == FALSE & threshold == TRUE) & 
       (shift(in_drought, fill = FALSE) == FALSE & in_drought == TRUE),
     by = .(adm0, ifelse(adm0 %in% adm1_countries, adm1, adm2))]
spei[, both_methods_cycles := sum(both_methods_start, na.rm = TRUE), 
     by = .(adm0, ifelse(adm0 %in% adm1_countries, adm1, adm2))]

### Intensity ####
# total number of months in drought
spei[, total_drought_months := sum(threshold, na.rm = TRUE), 
     by = .(adm0, ifelse(adm0 %in% adm1_countries, adm1, adm2))]


### Drought extremity measures ####

#  total drought months that meet both criteria
spei[, both_methods_months := sum(threshold & in_drought, na.rm = TRUE), 
     by = .(adm0, ifelse(adm0 %in% adm1_countries, adm1, adm2))]

#  total sum-method
spei[, sum_method_cycles_aggregated := sum(sum_drought_start, na.rm = TRUE),
     by = .(adm0, ifelse(adm0 %in% adm1_countries, adm1, adm2))]

#  total count-method 
spei[, count_method_cycles_aggregated := sum(count_drought_start, na.rm = TRUE),
     by = .(adm0, ifelse(adm0 %in% adm1_countries, adm1, adm2))]

# average time per drought cycle
spei[, average_time_per_cycle := fifelse(sum_method_cycles > 0, total_drought_months / sum_method_cycles_aggregated, 0),
     by = .(adm0, ifelse(adm0 %in% adm1_countries, adm1, adm2))]

# cumulative average sum measure during threshold periods
spei[, sum_measure := sum(value[threshold], na.rm = TRUE), 
     by = .(adm0, ifelse(adm0 %in% adm1_countries, adm1, adm2))]

# standardised dryness intensity
spei[, standardised_intensity := fifelse(average_time_per_cycle > 0, 
                                         sum_measure / average_time_per_cycle, 
                                         0),
     by = .(adm0, ifelse(adm0 %in% adm1_countries, adm1, adm2))]

# 90th
percentiles <- spei[, .(
  both_methods_threshold = quantile(both_methods_months, 0.9, na.rm = TRUE),
  intensity_threshold = quantile(standardised_intensity, 0.1, na.rm = TRUE),
  sum_cycles_threshold = quantile(sum_method_cycles_aggregated, 0.9, na.rm = TRUE),
  count_cycles_threshold = quantile(count_method_cycles_aggregated, 0.9, na.rm = TRUE)
)]

# dichotomised
spei[, `:=` (
  constant_drought_extreme = as.integer(both_methods_months > percentiles$both_methods_threshold),
  very_dry_drought_extreme = as.integer(standardised_intensity < percentiles$intensity_threshold),
  extreme_sum_cycles_extreme = as.integer(sum_method_cycles_aggregated > percentiles$sum_cycles_threshold),
  extreme_count_cycles_extreme = as.integer(count_method_cycles_aggregated > percentiles$count_cycles_threshold)
)]

# summary
summary_extreme_conditions <- spei[, .(
  num_constant_drought_extreme = sum(constant_drought_extreme, na.rm = TRUE),
  num_very_dry_drought_extreme = sum(very_dry_drought_extreme, na.rm = TRUE),
  num_extreme_sum_cycles_extreme = sum(extreme_sum_cycles_extreme, na.rm = TRUE),
  num_extreme_count_cycles_extreme = sum(extreme_count_cycles_extreme, na.rm = TRUE)
)]

# categories for analysis
spei[, zero_to_moderate := as.integer(!constant_drought_extreme & !very_dry_drought_extreme)]
spei[, recent_24_start := date %m-% months(23)]
#recent_24_spei <- spei[date >= recent_24_start & consecutive_dry >= 12]
spei[, recent_long_period := as.integer(date >= recent_24_start & consecutive_dry >= 12)]
spei[, constant_and_recent_long := as.integer(constant_drought_extreme & recent_long_period)]


# we dont need all the dates, as we are creating dichotomous
spei <- spei[, .(
  zero_to_moderate = as.integer(any(zero_to_moderate == 1)),
  very_dry_drought_extreme = as.integer(any(very_dry_drought_extreme == 1)),
  recent_long_period = as.integer(any(recent_long_period == 1)),
  constant_drought_extreme = as.integer(any(constant_drought_extreme == 1)),
  constant_and_recent_long = as.integer(any(constant_and_recent_long == 1))
), by = .(adm0, adm1, adm2)]


# lets check
# foo <- spei[, .(
#   num_zero_to_moderate = sum(zero_to_moderate, na.rm = TRUE),
#   num_very_dry = sum(very_dry_drought_extreme, na.rm = TRUE),
#   num_recent_long_period = sum(recent_long_period, na.rm = TRUE),
#   num_constant = sum(constant_drought_extreme, na.rm = TRUE),
#   num_constant_and_recent_long = sum(constant_and_recent_long, na.rm = TRUE)
# )]

return(spei)
}
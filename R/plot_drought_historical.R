# # tar_load(dat_spei)
# 
# svy_dates <- data.table(
#   adm0 = c(
#     "Cambodia", "Malawi", "Nigeria",
#     "Uganda", "Zambia", "Colombia",
#     "Côte d'Ivoire", "El Salvador", "Kenya",
#     "Lesotho", "Moldova", "Mozambique",
#     "Namibia", "Zimbabwe"),
#   date = as.Date(c(
#     "2013-02-01", "2013-09-01", "2014-05-01",
#     "2015-09-01", "2014-08-01", "2018-08-01",
#     "2018-06-01", "2017-11-01", "2018-12-01",
#     "2018-06-01", "2019-03-01", "2019-07-01",
#     "2019-03-01", "2017-01-01")))
# 
# # Specify the country name here
# country <- "Lesotho"
# 
# # Get the end date for the specified country
# end_date <- svy_dates[adm0 == country, date]
# start_date <- seq(end_date, length = 2, by = "-48 months")[2]
# 
# # Subset the data based on the defined date range and specified country
# foo <- dat_spei[adm0 == country & date >= start_date & date <= end_date]
# 
# # Aggregate the data to calculate the mean SPEI value per month across all provinces
# foo_aggregated <- foo[, .(mean_value = mean(value, na.rm = TRUE)), by = date]
# 
# par(mfrow = c(2, 1), mar = c(2, 4, 2, 2))
# 
# # Plot 1: Mean SPEI values over time
# plot(foo_aggregated$date, foo_aggregated$mean_value, type = "p", xlab = "", ylab = "Mean SPEI", main = country, xaxt = "n")
# lines(foo_aggregated$date, foo_aggregated$mean_value)
# abline(h = -1, col = "red", lty = 2)
# 
# # Flag dry months and reset condition
# foo_aggregated[, dry_month := ifelse(mean_value < -1, 1, 0)]
# foo_aggregated[, reset := mean_value > -1]
# 
# # cumulative dry months
# foo_aggregated[, cum_dry_months := 0]
# 
# # Calculate the cumulative dry months with reset condition
# cum_dry_months <- 0
# 
# for (i in 1:nrow(foo_aggregated)) {
#   if (foo_aggregated$reset[i]) {
#     cum_dry_months <- 0
#   } else if (foo_aggregated$dry_month[i] == 1) {
#     cum_dry_months <- cum_dry_months + 1
#   } else {
#     cum_dry_months <- 0
#   }
#   foo_aggregated$cum_dry_months[i] <- cum_dry_months
# }
# 
# # Plot 2: Cumulative dry months over time
# plot(foo_aggregated$date, foo_aggregated$cum_dry_months, type = "p", xlab = "", ylab = "Counts",
#      main = "", xaxt = "n")
# lines(foo_aggregated$date, foo_aggregated$cum_dry_months)
# axis(1, at = seq(from = start_date, to = end_date, by = "years"),
#      labels = format(seq(from = start_date, to = end_date, by = "years"), "%Y"))
# abline(h = 5, col = "red", lty = 2)
# 
# # Reset plotting layout
# par(mfrow = c(1, 1))
# 

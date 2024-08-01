# tar_load(calc_drought)

plot_drought_prolonged <- function(
    calc_drought
){
  
# Define svy_dates
svy_dates <- data.table(
  adm0 = c(
    "Cambodia", "Malawi", "Nigeria",
    "Uganda", "Zambia", "Colombia",
    "Côte d'Ivoire", "El Salvador", "Kenya",
    "Lesotho", "Moldova", "Mozambique",
    "Namibia", "Zimbabwe"),
  date = as.Date(c(
    "2013-02-01", "2013-09-01", "2014-05-01",
    "2015-09-01", "2014-08-01", "2018-08-01",
    "2018-06-01", "2017-11-01", "2018-12-01",
    "2018-06-01", "2019-03-01", "2019-07-01",
    "2019-03-01", "2017-01-01")))

# Specify the country name here
country <- "Zimbabwe"
adm_name <- "Mwenezi"

# Get the end date for the specified country
end_date <- svy_dates[adm0 == country, date]
start_date <- seq(end_date, length = 2, by = "-48 months")[2]

# Subset calc_drought based on the defined date range and specified country
foo <- calc_drought[adm0 == country & date >= start_date & date <= end_date
                    & adm2 == adm_name
                    ]

# Aggregate the data to calculate the mean value and mean sum_reset per date
foo_aggregated <- foo[, .(
  mean_value = mean(value, na.rm = TRUE),
  mean_sum_reset = mean(sum_reset, na.rm = TRUE)
), by = date]

# Flag dry months and reset condition
foo_aggregated[, dry_month := ifelse(mean_value < -1, 1, 0)]
foo_aggregated[, reset := mean_value > -1]

# Calculate the cumulative dry months with reset condition
foo_aggregated[, cum_dry_months := 0]
cum_dry_months <- 0
for (i in 1:nrow(foo_aggregated)) {
  if (foo_aggregated$reset[i]) {
    cum_dry_months <- 0
  } else if (foo_aggregated$dry_month[i] == 1) {
    cum_dry_months <- cum_dry_months + 1
  } else {
    cum_dry_months <- 0
  }
  foo_aggregated$cum_dry_months[i] <- cum_dry_months
}

# Highlight the months where constant drought extreme is flagged
highlighted_dates <- foo[in_drought == 1, date]

highlighted_both <- foo[in_drought == 1 & threshold == TRUE, date]

# Plotting
png("figures_and_tables/Mwenezi_District_Zimbabwe.png", res = 150,  width = 900, height = 1000)

par(mfrow = c(3, 1), mar = c(0.2, 4, 2, 2), oma = c(3, 0, 0, 0)) 

# Plot 1: Mean SPEI values over time
plot(foo_aggregated$date, foo_aggregated$mean_value, type = "p", xlab = "", ylab = "Mean SPEI", main = paste(adm_name, country, sep = ", "), xaxt = "n")
lines(foo_aggregated$date, foo_aggregated$mean_value)
abline(h = -1, col = "black", lty = 2)
# Dot in green all values of dry months
points(foo_aggregated$date[foo_aggregated$dry_month == 1], 
       foo_aggregated$mean_value[foo_aggregated$dry_month == 1], 
       pch = 21, col = "green3", bg = "green3", cex = 2, lwd = 1)

legend("bottomleft", legend = "Dry month", 
       col = "green3", pch = 21, pt.cex = 2, bty = "o", pt.bg = "green3")

# Plot 2: Cumulative dry months over time
plot(foo_aggregated$date, foo_aggregated$cum_dry_months, type = "p", xlab = "", ylab = "Counts (months)", main = "", xaxt = "n")
lines(foo_aggregated$date, foo_aggregated$cum_dry_months)
#axis(1, at = seq(from = start_date, to = end_date, by = "years"), labels = format(seq(from = start_date, to = end_date, by = "years"), "%Y"))
abline(h = 5, col = "black", lty = 2)
points(highlighted_dates, foo_aggregated[date %in% highlighted_dates, cum_dry_months], 
       pch = 21, col = "deepskyblue4", bg = "deepskyblue4", cex = 2, lwd =1)
points(highlighted_both, foo_aggregated[date %in% highlighted_both, cum_dry_months],
       pch = 21, col = "firebrick3", bg = "deepskyblue4", cex = 2, lwd =2)

# Add legend for highlighted points
legend("topleft", legend = c("Recent and long drought", "Sum and count methods"), 
       col = c("deepskyblue4", "firebrick3"), pch = 21, pt.bg = c("deepskyblue4", "deepskyblue4"), 
       pt.lwd = c(1, 2), pt.cex = 2, bty = "o")

# Plot 3: Sum-method drought severity index
plot(foo_aggregated$date, foo_aggregated$mean_sum_reset, type = "l", xlab = "Date", ylab = "Sum of SPEI values", main = "")
abline(h = -17.5, col = "black", lty = 2)  # Threshold line
points(highlighted_dates, foo_aggregated[date %in% highlighted_dates, mean_sum_reset], 
       pch = 21, col = "deepskyblue4", bg = "deepskyblue4", cex = 2, lwd =1)
points(highlighted_both, foo_aggregated[date %in% highlighted_both, mean_sum_reset], 
       pch = 21, col = "firebrick3", bg = "deepskyblue4", cex = 2, lwd =2)

# Reset plotting layout
par(mfrow = c(1, 1))
dev.off()
}

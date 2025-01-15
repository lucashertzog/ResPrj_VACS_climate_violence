# tar_load(calc_drought)

plot_drought_prolonged <- function(
    calc_drought
){

# Specify the country name here
country <- "Uganda"
adm_name <- "Luwero"
  
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

# Get the end date for the specified country
end_date <- svy_dates[adm0 == country, date]
start_date <- seq(end_date, length = 2, by = "-48 months")[2]

# Subset calc_drought based on the defined date range and specified country
foo <- calc_drought[adm0 == country & date >= start_date & date <= end_date
                    & adm2 == adm_name]

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

# Initialise variables by marking all in_drought == 1 as saddlebrown
foo_aggregated[, color := ifelse(foo$in_drought == 1, "saddlebrown", NA)]
# Get the dates for saddlebrown points (in_drought)
saddlebrown_dates <- foo_aggregated[!is.na(color) & color == "saddlebrown", date]

# Define the time window for deepskyblue4 points
window_start <- end_date - months(24)
window_end <- end_date

# Identify the dates that meet the threshold criteria
highlighted_both <- foo[in_drought == 1 & threshold == TRUE, date]

# Initialise and calculate consecutive drought months
foo[, consecutive_drought := 0]
consecutive_drought <- 0
# Reset the flag for valid drought periods
valid_drought_periods <- FALSE

# # Iterate through the data to identify consecutive drought months within the 24-month window
# for (i in 1:nrow(foo)) {
#   if (foo$in_drought[i] == 1 && foo$date[i] >= window_start && foo$date[i] <= window_end) {
#     consecutive_drought <- consecutive_drought + 1
#     foo$consecutive_drought[i] <- consecutive_drought
#     
#     # If exactly 12 consecutive drought months are found, mark them as deepskyblue4
#     if (consecutive_drought == 12) {
#       foo_aggregated[(i-11):i, color := "deepskyblue4"]
#     }
#   } else {
#     consecutive_drought <- 0
#   }
# }

# Iterate through the data to identify consecutive drought months within the 24-month window
for (i in 1:nrow(foo)) {
  if (foo$in_drought[i] == 1 && foo$date[i] >= window_start && foo$date[i] <= window_end) {
    consecutive_drought <- consecutive_drought + 1
    foo$consecutive_drought[i] <- consecutive_drought
    
    # If exactly 12 consecutive drought months are found, mark them as deepskyblue4
    if (consecutive_drought == 12) {
      foo_aggregated[(i-11):i, color := "deepskyblue4"]
      valid_drought_periods <- FALSE  # Reset valid_drought_periods as this is handled by deepskyblue4
    }
  } else {
    # Mark as valid drought period if between 1 and 11 months within the 24-month window
    if (consecutive_drought > 0 & consecutive_drought < 12) {
      valid_drought_periods <- TRUE
    }
    consecutive_drought <- 0  # Reset drought count if in_drought is interrupted
  }
}

# If we have any valid drought periods, set the flag to TRUE
if (any(foo$consecutive_drought > 0 & foo$consecutive_drought < 12)) {
  valid_drought_periods <- TRUE
}

# Now, re-assign `highlighted_dates` and `highlighted_colors` after ensuring only valid points are flagged
highlighted_dates <- foo_aggregated[color == "deepskyblue4", date]
highlighted_colors <- foo_aggregated[color == "deepskyblue4", color]

# Get the dates for valid saddlebrown points (in_drought)
saddlebrown_dates <- foo_aggregated[color == "saddlebrown", date]

# Step to identify orange2 points starting from the 7th consecutive month
foo[, orange2_flag := FALSE]
for (i in 1:nrow(foo)) {
  if (foo$consecutive_drought[i] > 7 && foo$threshold[i] == TRUE) {
    foo$orange2_flag[i] <- TRUE
  } else if (foo$consecutive_drought[i] > 0 & foo$consecutive_drought[i] <= 7) {
    foo$orange2_flag[i] <- FALSE  # Stop marking after the first sequence break
  }
}

# Get the dates for the orange2 points
orange2_dates <- foo[orange2_flag == TRUE, date]

# Plotting
#png(paste0("figures_and_tables/", adm_name, "_aDistrict_", country, ".png"), res = 150,  width = 900, height = 1000)
par(mfrow = c(3, 1), mar = c(0.2, 4, 2, 2), oma = c(3, 0, 0, 0))

# Plot 1: Mean SPEI values over time
plot(foo_aggregated$date, foo_aggregated$mean_value, type = "p", xlab = "", ylab = "Mean SPEI", main = paste(adm_name, country, sep = ", "), xaxt = "n")
lines(foo_aggregated$date, foo_aggregated$mean_value)
abline(h = -1, col = "black", lty = 2)
# Dot in green all values of dry months
points(foo_aggregated$date[foo$dry_month == 1], 
       foo_aggregated$mean_value[foo$dry_month == 1], 
       pch = 21, col = "darkolivegreen3", bg = "darkolivegreen3", cex = 2, lwd = 1)

legend("topleft", legend = "Dry month", 
       col = "darkolivegreen3", pch = 21, pt.cex = 2, bty = "o", pt.bg = "darkolivegreen3")

# ## version of plot2 and 3 for recent and long
# Plot 2: Cumulative dry months over time
plot(foo_aggregated$date, foo_aggregated$cum_dry_months, type = "p", xlab = "", ylab = "Counts (months)", main = "", xaxt = "n")
lines(foo_aggregated$date, foo_aggregated$cum_dry_months)
abline(h = 5, col = "black", lty = 2)

# Plot valid saddlebrown points for in_drought months
points(saddlebrown_dates, foo_aggregated[date %in% saddlebrown_dates, cum_dry_months], pch = 21, col = "saddlebrown", bg = "saddlebrown", cex = 2, lwd = 1)

# Plot points within the 24-month window for deepskyblue4
points(foo_aggregated$date[
  foo_aggregated$date >= window_start & foo_aggregated$date <= window_end & foo_aggregated$date %in% highlighted_dates],
  foo_aggregated$cum_dry_months[foo_aggregated$date >= window_start & foo_aggregated$date <= window_end & foo_aggregated$date %in% highlighted_dates],
  pch = 21, col = highlighted_colors, bg = highlighted_colors, cex = 2, lwd = 1)

# Plot threshold points separately within the 24-month window
points(foo_aggregated$date[foo_aggregated$date >= window_start & foo_aggregated$date <= window_end & foo_aggregated$date %in% highlighted_both], foo_aggregated$cum_dry_months[foo_aggregated$date >= window_start & foo_aggregated$date <= window_end & foo_aggregated$date %in% highlighted_both], pch = 21, col = "firebrick3", bg = "deepskyblue4", cex = 2, lwd = 2)

# Plot the orange2 points
points(orange2_dates, foo_aggregated[date %in% orange2_dates, cum_dry_months], pch = 21, col = "orange2", bg = "orange2", cex = 2, lwd = 2)

# Base legend setup
legend_items <- c("Recent and long drought", "Sum and count methods")
legend_colors <- c("deepskyblue4", "firebrick3")
legend_bg <- c("deepskyblue4", "deepskyblue4")
legend_lwd <- c(1, 2)

# Conditionally add the "In Drought" legend item
# This ensures only periods that are exclusively between 1 and 11 months trigger the legend
if (valid_drought_periods) {
  legend_items <- c(legend_items, "In Drought")
  legend_colors <- c(legend_colors, "saddlebrown")
  legend_bg <- c(legend_bg, "saddlebrown")
  legend_lwd <- c(legend_lwd, 2)
}

# Conditionally add the "Prolonged and extreme" legend item if the orange points are plotted
if (any(foo$constant_drought_extreme == 1)) {
  legend_items <- c(legend_items, "Prolonged and extreme")
  legend_colors <- c(legend_colors, "orange2")
  legend_bg <- c(legend_bg, "orange2")
  legend_lwd <- c(legend_lwd, 2)
}

# Add the legend with the final items
legend("topleft", legend = legend_items,
       col = legend_colors, pch = 21, pt.bg = legend_bg,
       pt.lwd = legend_lwd, pt.cex = 2, bty = "o")

# abline(v = window_start, col = "black", lty = 2)

# Plot 3: Sum-method drought severity index
plot(foo_aggregated$date, foo_aggregated$mean_sum_reset, type = "l", xlab = "Date", ylab = "Sum of SPEI values", main = "")
abline(h = -17.5, col = "black", lty = 2)  # Threshold line

# Plot valid saddlebrown points for in_drought months
points(saddlebrown_dates, foo_aggregated[date %in% saddlebrown_dates, mean_sum_reset],
       pch = 21, col = "saddlebrown", bg = "saddlebrown", cex = 2, lwd = 1)

# Plot points within the 24-month window for deepskyblue4
points(foo_aggregated$date[
  foo_aggregated$date >= window_start & foo_aggregated$date <= window_end & foo_aggregated$date %in% highlighted_dates],
  foo_aggregated$mean_sum_reset[foo_aggregated$date >= window_start & foo_aggregated$date <= window_end & foo_aggregated$date %in% highlighted_dates],
  pch = 21, col = highlighted_colors, bg = highlighted_colors, cex = 2, lwd = 1)

# Plot threshold points separately within the 24-month window
points(foo_aggregated$date[
  foo_aggregated$date >= window_start & foo_aggregated$date <= window_end & foo_aggregated$date %in% highlighted_both],
  foo_aggregated$mean_sum_reset[foo_aggregated$date >= window_start & foo_aggregated$date <= window_end & foo_aggregated$date %in% highlighted_both],
  pch = 21, col = "firebrick3", bg = "deepskyblue4", cex = 2, lwd = 2)

# Plot the orange2 points, if applicable, within the window
points(orange2_dates, foo_aggregated[date %in% orange2_dates, mean_sum_reset],
       pch = 21, col = "orange2", bg = "orange2", cex = 2, lwd = 2)

# abline(v = window_start, col = "black", lty = 2)

# Reset plotting layout
par(mfrow = c(1, 1))
dev.off()
}

# ############## THE FOLLOWING CODE IS FOR PROLONGED AND EXTREME. WORKING BUT
# ############## COMMENTED OUT FOR GITHUB
# 
# # Specify the country name here
# country <- "Namibia"
# adm_name <- "Karasburg"
# 
# # Define svy_dates
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
# # Get the end date for the specified country
# end_date <- svy_dates[adm0 == country, date]
# start_date <- seq(end_date, length = 2, by = "-48 months")[2]
# 
# # Subset calc_drought based on the defined date range and specified country
# foo <- calc_drought[adm0 == country & date >= start_date & date <= end_date
#                     & adm2 == adm_name]
# 
# # Aggregate the data to calculate the mean value and mean sum_reset per date
# foo_aggregated <- foo[, .(
#   mean_value = mean(value, na.rm = TRUE),
#   mean_sum_reset = mean(sum_reset, na.rm = TRUE)
# ), by = date]
# 
# # Flag dry months and reset condition
# foo_aggregated[, dry_month := ifelse(mean_value < -1, 1, 0)]
# foo_aggregated[, reset := mean_value > -1]
# 
# # Calculate the cumulative dry months with reset condition
# foo_aggregated[, cum_dry_months := 0]
# cum_dry_months <- 0
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
# # Initialise variables by marking all in_drought == 1 as saddlebrown
# foo_aggregated[, color := ifelse(foo$in_drought == 1, "saddlebrown", NA)]
# # Get the dates for saddlebrown points (in_drought)
# saddlebrown_dates <- foo_aggregated[!is.na(color) & color == "saddlebrown", date]
# 
# # Define the time window for deepskyblue4 points
# window_start <- end_date - months(24)
# window_end <- end_date
# 
# # Identify the dates that meet the threshold criteria
# highlighted_both <- foo[in_drought == 1 & threshold == TRUE, date]
# 
# # Initialise and calculate consecutive drought months
# foo[, consecutive_drought := 0]
# consecutive_drought <- 0
# # Reset the flag for valid drought periods
# valid_drought_periods <- FALSE
# 
# # Iterate through the data to identify consecutive drought months without the 24-month window restriction
# for (i in 1:nrow(foo)) {
#   if (foo$in_drought[i] == 1) {
#     consecutive_drought <- consecutive_drought + 1
#     foo$consecutive_drought[i] <- consecutive_drought
#     
#     # If exactly 12 consecutive drought months are found, mark them as deepskyblue4
#     if (consecutive_drought == 12) {
#       foo_aggregated[(i-11):i, color := "deepskyblue4"]
#       valid_drought_periods <- FALSE  # Reset valid_drought_periods as this is handled by deepskyblue4
#     }
#   } else {
#     # Mark as valid drought period if between 1 and 11 months
#     if (consecutive_drought > 0 & consecutive_drought < 12) {
#       valid_drought_periods <- TRUE
#     }
#     consecutive_drought <- 0  # Reset drought count if in_drought is interrupted
#   }
# }
# 
# # If we have any valid drought periods, set the flag to TRUE
# if (any(foo$consecutive_drought > 0 & foo$consecutive_drought < 12)) {
#   valid_drought_periods <- TRUE
# }
# 
# # Now, re-assign `highlighted_dates` and `highlighted_colors` after ensuring only valid points are flagged
# highlighted_dates <- foo_aggregated[color == "deepskyblue4", date]
# highlighted_colors <- foo_aggregated[color == "deepskyblue4", color]
# 
# # Re-assign colors to foo_aggregated based on the conditions
# foo_aggregated[, color := NA]  # Start by resetting the color column
# 
# # Assign 'saddlebrown' to points where in_drought == 1
# foo_aggregated[foo$in_drought == 1, color := "saddlebrown"]
# 
# # Assign 'deepskyblue4' to points where exactly 12 consecutive drought months are found
# foo_aggregated[foo$consecutive_drought == 12, color := "deepskyblue4"]
# 
# # Assign 'orange2' to points starting from the 7th consecutive month that meet the threshold
# foo_aggregated[foo$orange2_flag == TRUE, color := "orange2"]
# 
# # Now, extract the dates for valid saddlebrown points (in_drought)
# saddlebrown_dates <- foo_aggregated[color == "saddlebrown", date]
# 
# 
# # Step to identify orange2 points starting from the 7th consecutive month
# foo[, orange2_flag := FALSE]
# for (i in 1:nrow(foo)) {
#   if (foo$consecutive_drought[i] > 11 && foo$threshold[i] == TRUE) {
#     foo$orange2_flag[i] <- TRUE
#   } else if (foo$consecutive_drought[i] > 0 & foo$consecutive_drought[i] <= 7) {
#     foo$orange2_flag[i] <- FALSE  # Stop marking after the first sequence break
#   }
# }
# 
# 
# # Get the dates for the orange2 points
# orange2_dates <- foo[orange2_flag == TRUE, date]
# 
# # Plotting
# png(paste0("figures_and_tables/", adm_name, "_aDistrict_", country, ".png"), res = 150,  width = 900, height = 1000)
# par(mfrow = c(3, 1), mar = c(0.2, 4, 2, 2), oma = c(3, 0, 0, 0))
# 
# # Plot 1: Mean SPEI values over time
# plot(foo_aggregated$date, foo_aggregated$mean_value, type = "p", xlab = "", ylab = "Mean SPEI", main = paste(adm_name, country, sep = ", "), xaxt = "n")
# lines(foo_aggregated$date, foo_aggregated$mean_value)
# abline(h = -1, col = "black", lty = 2)
# # Dot in green all values of dry months
# points(foo_aggregated$date[foo$dry_month == 1], 
#        foo_aggregated$mean_value[foo$dry_month == 1], 
#        pch = 21, col = "darkolivegreen3", bg = "darkolivegreen3", cex = 2, lwd = 1)
# 
# legend("topleft", legend = "Dry month", 
#        col = "darkolivegreen3", pch = 21, pt.cex = 2, bty = "o", pt.bg = "darkolivegreen3")
# 
# ### PLOTS 2 AND 3 FOR EXTREME AND PROLONGED ####
# # Plot 2: Cumulative dry months over time
# plot(foo_aggregated$date, foo_aggregated$cum_dry_months, type = "p", xlab = "", ylab = "Counts (months)", main = "", xaxt = "n")
# lines(foo_aggregated$date, foo_aggregated$cum_dry_months)
# abline(h = 5, col = "black", lty = 2)
# 
# # Plot valid saddlebrown points for in_drought months
# points(saddlebrown_dates, foo_aggregated[date %in% saddlebrown_dates, cum_dry_months], pch = 21, col = "saddlebrown", bg = "saddlebrown", cex = 2, lwd = 1)
# 
# # Plot threshold points separately
# points(foo_aggregated$date[foo_aggregated$date %in% highlighted_both], 
#        foo_aggregated$cum_dry_months[foo_aggregated$date %in% highlighted_both], 
#        pch = 21, col = "firebrick3", bg = "deepskyblue4", cex = 2, lwd = 2)
# 
# # Plot the orange2 points
# points(orange2_dates, foo_aggregated[date %in% orange2_dates, cum_dry_months], pch = 21, col = "orange2", bg = "orange2", cex = 2, lwd = 2)
# 
# # Base legend setup
# legend_items <- c("Sum and count methods")
# legend_colors <- c("firebrick3")
# legend_bg <- c("deepskyblue4")
# legend_lwd <- c(2)
# 
# # Conditionally add the "In Drought" legend item
# if (valid_drought_periods) {
#   legend_items <- c(legend_items, "In Drought")
#   legend_colors <- c(legend_colors, "saddlebrown")
#   legend_bg <- c(legend_bg, "saddlebrown")
#   legend_lwd <- c(legend_lwd, 2)
# }
# 
# # Conditionally add the "Prolonged and extreme" legend item if the orange points are plotted
# if (any(foo$constant_drought_extreme == 1)) {
#   legend_items <- c(legend_items, "Prolonged and extreme")
#   legend_colors <- c(legend_colors, "orange2")
#   legend_bg <- c(legend_bg, "orange2")
#   legend_lwd <- c(legend_lwd, 2)
# }
# 
# # Add the legend with the final items
# legend("topleft", legend = legend_items, 
#        col = legend_colors, pch = 21, pt.bg = legend_bg, 
#        pt.lwd = legend_lwd, pt.cex = 2, bty = "o")
# 
# # Plot 3: Sum-method drought severity index
# plot(foo_aggregated$date, foo_aggregated$mean_sum_reset, type = "l", xlab = "Date", ylab = "Sum of SPEI values", main = "")
# abline(h = -17.5, col = "black", lty = 2)  # Threshold line
# 
# # Plot valid saddlebrown points for in_drought months
# points(saddlebrown_dates, foo_aggregated[date %in% saddlebrown_dates, mean_sum_reset], 
#        pch = 21, col = "saddlebrown", bg = "saddlebrown", cex = 2, lwd = 1)
# 
# 
# # Plot threshold points separately
# points(foo_aggregated$date[foo_aggregated$date %in% highlighted_both], 
#        foo_aggregated$mean_sum_reset[foo_aggregated$date %in% highlighted_both],
#        pch = 21, col = "firebrick3", bg = "deepskyblue4", cex = 2, lwd = 2)
# 
# # Plot the orange2 points, if applicable
# points(orange2_dates, foo_aggregated[date %in% orange2_dates, mean_sum_reset], 
#        pch = 21, col = "orange2", bg = "orange2", cex = 2, lwd = 2)
# 
# # Reset plotting layout
# par(mfrow = c(1, 1))
# dev.off()

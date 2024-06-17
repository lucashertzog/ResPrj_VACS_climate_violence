# # [1] "Quthing"       "Mohale's Hoek" "Qacha's Nek"   "Mafeteng"     
# # [5] "Maseru"        "Thaba-Tseka"   "Mokhotlong"    "Berea"        
# # [9] "Leribe"        "Butha-Buthe"  
# # 
# # tar_load(dat_spei)
# # Filter the data for Leribe and the specified date range
# lso <- dat_spei[adm1=="Leribe" &
#                   date >= as.Date("2011-01-01") &
#                   date <= as.Date("2021-01-01")]
# 
# # Initialize plotting area to hold 2 plots in one column
# par(mfrow=c(2,1))
# 
# # Plot 1: SPEI values over time
# plot(lso$date, lso$value, type = "p", xlab = "", ylab = "SPEI", 
#      main = "Leribe province in Lesotho (2011-2021)", xaxt = "n")
# lines(lso$date, lso$value)
# axis(1, at = seq(from = as.Date("2011-01-01"), to = as.Date("2021-01-01"), by = "years"), 
#      labels = format(seq(from = as.Date("2011-01-01"), to = as.Date("2021-01-01"), by = "years"), "%Y"))
# abline(h = -1, col = "red", lty = 2)
# 
# # Flag dry months and reset condition
# lso[, dry_month := ifelse(value < -1, 1, 0)]
# lso[, reset := value > -1]
# 
# # Initialize the cumulative dry months
# lso[, cum_dry_months := 0]
# 
# # Calculate the cumulative dry months with reset condition
# cum_dry_months <- 0
# 
# for (i in 1:nrow(lso)) {
#   if (lso$reset[i]) {
#     cum_dry_months <- 0
#   } else if (lso$dry_month[i] == 1) {
#     cum_dry_months <- cum_dry_months + 1
#   } else {
#     cum_dry_months <- 0
#   }
#   lso$cum_dry_months[i] <- cum_dry_months
# }
# 
# # Plot 2: Cumulative dry months over time
# plot(lso$date, lso$cum_dry_months, type = "p", xlab = "", ylab = "Counts", 
#      main = "Cumulative Dry Months", xaxt = "n")
# lines(lso$date, lso$cum_dry_months)
# axis(1, at = seq(from = as.Date("2011-01-01"), to = as.Date("2021-01-01"), by = "years"), 
#      labels = format(seq(from = as.Date("2011-01-01"), to = as.Date("2021-01-01"), by = "years"), "%Y"))
# abline(h = 5, col = "red", lty = 2)
# 
# # Reset plotting layout
# par(mfrow=c(1,1))

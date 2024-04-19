
z <- dat_spei[dat_spei$adm0 == "Zimbabwe"]

tar_load(dat_spei)
library(data.table)
# Define survey end dates for each country
survey_dates <- data.table(
  country = c("Cambodia", "Malawi", "Nigeria", "Uganda", "Zambia",
              "Colombia", "Cote d'Ivoire", "El Salvador", "Kenya", "Lesotho",
              "Moldova", "Mozambique", "Namibia", "Zimbabwe"),
  date_end = as.IDate(c("2013-03-31", "2013-10-31", "2014-07-31", "2015-11-30", "2014-09-30",
                        "2018-10-31", "2018-09-30", "2017-12-01", "2019-01-31", "2018-09-30",
                        "2019-02-28", "2019-09-30", "2019-06-30", "2017-08-31"))
)

# Calculate 24 months prior dates
survey_dates[, date_start_24m := date_end %m-% months(24)]

# Load SPEI data assuming it is already a data.table named dat_spei
dat_spei[, date := as.IDate(date)]

# Function to filter data and calculate indices for adm level
# Adjusted function to correctly count unique drought months
calc_drought <- function(
    data, 
    start_date, 
    end_date, 
    threshold = -1, 
    intense_threshold = -17.5
    ){
  filtered_data <- data[date >= start_date & date <= end_date, .(date, value)]
  filtered_data[, drought := value < threshold]
  
  # Calculate unique months with drought conditions
  drought_months <- unique(filtered_data[drought == TRUE, .(year(date), month(date))])
  count_method <- nrow(drought_months)  # Number of unique months with drought
  
  # Calculate sum method
  filtered_data[, drought_sum := cumsum(ifelse(drought, value - threshold, NA)), by = rleid(drought)]
  
  intense_periods <- filtered_data[drought_sum <= intense_threshold & !is.na(drought_sum), .(total_intensity = sum(drought_sum))]
  
  sum_method <- ifelse(length(intense_periods$total_intensity) > 0, sum(intense_periods$total_intensity), 0.0)
  
  return(list(count_method = as.double(count_method), sum_method = as.double(sum_method)))
}



# Loop to apply calculations
results <- list()
for (i in 1:nrow(survey_dates)) {
  country_data <- dat_spei[adm0 == survey_dates$country[i]]
  
  # Determine the highest available admin level and calculate indices
  if (any(!is.na(country_data$adm3))) {
    indices <- country_data[, calc_drought(.SD, survey_dates$date_start_24m[i], survey_dates$date_end[i], -1, -17.5), by = adm3]
    indices[, admin_level := "adm3"]  # Mark the admin level used
  } else if (any(!is.na(country_data$adm2))) {
    indices <- country_data[, calc_drought(.SD, survey_dates$date_start_24m[i], survey_dates$date_end[i], -1, -17.5), by = adm2]
    indices[, admin_level := "adm2"]  # Mark the admin level used
  } else {
    indices <- country_data[, calc_drought(.SD, survey_dates$date_start_24m[i], survey_dates$date_end[i], -1, -17.5), by = adm1]
    indices[, admin_level := "adm1"]  # Mark the admin level used
  }
  
  indices[, country := survey_dates$country[i]]
  results[[i]] <- indices
}

# Combine all results into a single data.table
final_results <- rbindlist(results)
print(final_results)

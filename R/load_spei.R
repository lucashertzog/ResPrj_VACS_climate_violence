load_spei <- function(
    dir
){
  dat_spei <- readRDS(file.path(rootdir, file_dat_spei))
  setDT(dat_spei)

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
    prev_48m = date %m-% months(48),  # Start of the 48-month period
    prev_24m = date %m-% months(24),  # Start of the 24-month period
    baseline48_end = (date %m-% months(48)) %m+% months(1),  # End of the baseline period
    baseline24_end = (date %m-% months(24)) %m+% months(1),  # End of the baseline period
    survey_date = date  # End of the period, inclusive
  )]

  spei <- dat_spei[svy_dates, on = .(adm0), allow.cartesian = TRUE]

  # Create flags for each period separately
  spei[, period48 := fifelse(date <= baseline48_end, "baseline48",
                             fifelse(date >= prev_48m & date <= survey_date, "survey48", NA_character_))]

  spei[, period24 := fifelse(date <= baseline24_end, "baseline24",
                             fifelse(date >= prev_24m & date <= survey_date, "survey24", NA_character_))]


  # spei <- spei[date >= prev_48m & date <= survey_date]

  spei <- spei[, .(adm0, adm1, adm2, date, value, period48, period24)]

  adm1_countries <- c("Côte d'Ivoire", "Lesotho", "Moldova", "Mozambique")
  adm2_countries <- c(
    "Cambodia", "Malawi", "Nigeria", "Uganda", "Zambia","Colombia",
    "El Salvador", "Kenya", "Namibia", "Zimbabwe")

  # split by admin that we have
  adm1_spei <- spei[adm0%in%adm1_countries]
  adm2_spei <- spei[adm0%in%adm2_countries]

  mean_adm1 <- adm1_spei[, .(value = mean(value)),by = .(adm0, adm1, date, period24, period48)]
  mean_adm1[, adm2 := NA_character_]

  spei <- rbind(mean_adm1, adm2_spei)

  # Add a new column for group identification based on conditions
  spei[, group_id := ifelse(adm0 %in% c("Côte d'Ivoire", "Lesotho", "Moldova", "Mozambique"),
                            paste(adm0, adm1),
                            paste(adm0, adm1, adm2))]

  return(spei)
}
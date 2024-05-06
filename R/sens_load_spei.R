sens_load_spei <- function(
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
    prev_24m = date %m-% months(23),  # Start of the 48-month period
    survey_date = date  # End of the period, inclusive
  )]
  
  spei <- dat_spei[svy_dates, on = .(adm0), allow.cartesian = TRUE]
  
  spei <- spei[date >= prev_24m & date <= survey_date]
  
  spei <- spei[, .(adm0, adm1, adm2, date, value)]
  
  
  # spei_out [, spei_cat := fcase(
  #   spei_avg >= 2, "Extremely wet",
  #   spei_avg >= 1.5 & spei_avg < 2, "Severely wet",
  #   spei_avg >= 1 & spei_avg < 1.5, "Moderately wet",
  #   spei_avg > 0.5 & spei_avg < 1, "Mildly wet",
  #   spei_avg > -0.5 & spei_avg <= 0.5, "Normal",
  #   spei_avg > -1 & spei_avg < -0.5, "Mild drought",
  #   spei_avg > -1.5 & spei_avg <= -1, "Moderate drought",
  #   spei_avg > -2 & spei_avg <= -1.5, "Severe drought",
  #   spei_avg <= -2, "Extreme drought"
  #   )]
  # 
  
  return(spei)
}
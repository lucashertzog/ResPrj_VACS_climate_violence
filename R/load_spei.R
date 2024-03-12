load_spei <- function(
    dir
){
  spei <- readRDS(file.path(rootdir, file_dat_spei))
  setDT(spei)
  setnames(spei, tolower(names(spei)))
  foo <- spei[, .(uid, name_0, name_1, engtype_1, name_2, engtype_2, name_3, engtype_3, name_4, engtype_4, value, date)]
  
  foo <- foo[name_0 != "Tanzania"]
  
  setnames(foo, 
           old = c("uid", "name_0", 
                   "name_1", "engtype_1", 
                   "name_2", "engtype_2", 
                   "name_3", "engtype_3", 
                   "name_4", "engtype_4", 
                   "value"), 
           new = c("adm_id", "adm_0", 
                   "adm_1", "admtype_1", 
                   "adm_2", "admtype_2", 
                   "adm_3", "admtype_3", 
                   "adm_4", "admtype_4", 
                   "spei"))
  
  survey_dates <- list(
    "Cambodia" = as.Date("2013-02-01"),
    "Malawi" = as.Date("2013-09-01"),
    "Nigeria" = as.Date("2014-05-01"),
    "Zambia" = as.Date("2014-08-01"),
    "Colombia" = as.Date("2018-08-01"),
    "Côte d'Ivoire" = as.Date("2018-06-01"),
    "El Salvador" = as.Date("2017-01-01"), # check
    "Honduras" = as.Date("2017-01-01"), # check
    "Kenya" = as.Date("2018-12-01"),
    "Lesotho" = as.Date("2018-06-01"),
    "Moldova" = as.Date("2018-10-01"),
    "Mozambique" = as.Date("2019-07-01"),
    "Namibia" = as.Date("2019-03-01"),
    "Zimbabwe" = as.Date("2017-01-01")
  )
  

  foo$survey_start <- unlist(sapply(foo$adm_0, function(country) survey_dates[[country]]))

  foo <- foo[
    is.na(survey_start) | 
      (date >= (survey_start - 730) &
         date <= (survey_start))
  ]
  
  foo$survey_start <- as.Date(foo$survey_start, origin = "1970-01-01")
  
  setnames(foo,
           old = c("date", "survey_start"),
           new = c("date_exposure", "date_survey"))
  
  avg_spei <- foo[, .(spei_avg = mean(spei, na.rm = TRUE)),
                  by = .(adm_id)]
  
  spei_out <- merge(
    foo,
    avg_spei,
    by = "adm_id",
    all.x = TRUE
  )
  
  spei_out [, spei_cat := fcase(
    spei_avg >= 2, "Extremely wet",
    spei_avg >= 1.5 & spei_avg < 2, "Severely wet",
    spei_avg >= 1 & spei_avg < 1.5, "Moderately wet",
    spei_avg > 0.5 & spei_avg < 1, "Mildly wet",
    spei_avg > -0.5 & spei_avg <= 0.5, "Normal",
    spei_avg > -1 & spei_avg < -0.5, "Mild drought",
    spei_avg > -1.5 & spei_avg <= -1, "Moderate drought",
    spei_avg > -2 & spei_avg <= -1.5, "Severe drought",
    spei_avg <= -2, "Extreme drought"
    )]
  
  # QC
  spei_count <- spei_out[, .N, by = spei_cat]
  
  qc <- spei_out[is.na(spei_cat)]
  ## qc shows 274 NA
  # > unique(qc$adm_0)
  # [1] "Colombia" "Honduras" "Kenya" 
  ## honduras and colombia - islands NA
  ## kenya coastal close to mombassa NA
  nga <- spei_out[spei_out$adm_0 == "Nigeria"]
  paste(unique(nga$adm_2), collapse = ", ")
  
  
  return(spei_out)
}
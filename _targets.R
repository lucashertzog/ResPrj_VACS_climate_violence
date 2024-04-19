library(targets)
source("config.R")
tar_source()

tar_option_set(
  packages =
    c("targets",
      "data.table",
      "lubridate",
      "haven",
      "survey",
      "mice",
      "gtsummary"
    )
)

list(
  ### LOAD DATA ####
  #### vacs ####
  tar_target(
    dat_vacs,
    load_vacs(
      file.path(
        rootdir,
        file_dat_vacs
        )
      )
    )
  ,
  #### spei ####
  tar_target(
    dat_spei,
    load_spei(
      file.path(
        rootdir,
        file_dat_spei
        )
      )
    )
  ,
  ### IMPUTATION ####
  tar_target(
    imp,
    do_impute(
      dat_vacs
      )
  )
  ,
  ### FIGURES AND TABLES ####
  tar_target(
    tab1,
    tab_desc(
      dat_vacs
      )
    )
  )


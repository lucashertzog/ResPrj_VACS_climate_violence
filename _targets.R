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
      "gtsummary",
      "lubridate",
      "missForest"
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
  ### PREP ####
  tar_target(
    calc_drought,
    do_calc_drought(
      dat_spei
    )
  )
  ,
  tar_target(
    agg_drought,
    do_agg_drought(
      calc_drought
    )
  )
  ,
  ### MERGE ####
  tar_target(
    dat_mrg,
    do_mrg(
      dat_vacs,
      agg_drought
    )
  )
  ,
  ### IMPUTATION ####
  tar_target(
    dat_imp,
    do_impute(
      dat_mrg
      )
  )
  ,
  ### MODEL ####
  tar_target(
    calc_model1,
    do_model1(
      dat_imp
    )
  )
  # ,
  # ### FIGURES AND TABLES ####
  # tar_target(
  #   out_tab1,
  #   tab_desc(
  #     dat_vacs
  #     )
  #   )
  )


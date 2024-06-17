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
      "missForest",
      "ggplot2",
      "shadowtext",
      "rstanarm"
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
  #### Calc Drought ####
  tar_target(
    calc_drought,
    do_calc_drought(
      dat_spei
    )
  )
  ,
  # #### Excess Drought ####
  # tar_target(
  #   calc_drought_excess,
  #   do_calc_drought_excess(
  #     dat_spei
  #   )
  # )
  # ,
  #### Agg Drought ####
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
  # ### MODEL ####
  # tar_target(
  #   calc_model1,
  #   do_model1(
  #     dat_imp
  #   )
  # )
  # ,
  ### BAYESIAN MODEL ####
  tar_target(
    calc_model_bayesian,
    do_model_bayesian(
      dat_imp
    )
  )
  ,
  ### FIGURES AND TABLES ####
  #### Table 1 ####
  tar_target(
    out_tab1,
    tab_desc(
      dat_vacs
      )
    )
  ,
  # #### Table 2 ####
  # tar_target(
  #   out_tab2,
  #   tab_drought(
  #     dat_mrg
  #   )
  # )
  # ,
  # #### Table 3 ####
  # tar_target(
  #   out_tab3,
  #   tab_odds(
  #     calc_model1
  #   )
  # )
  # ,
  #### Table 3 Bayesian ####
  tar_target(
    out_tab3_bayes,
    tab_bayesian(
      calc_model_bayesian
    )
  )
  ,
  #### Plot 1 ####
  tar_target(
    out_plot1,
    plot_sex_viol(
      dat_vacs
    )
  )
  # ,
  # #### Plot 2 ####
  # tar_target(
  #   out_plot2,
  #   plot_drought(
  #     calc_drought
  #   )
  # )
  # ,
  # ### SENSITIVITY ANALYSIS ####
  # #### sens_spei ####
  # tar_target(
  #   sens_dat_spei,
  #   sens_load_spei(
  #     file.path(
  #       rootdir,
  #       file_dat_spei
  #     )
  #   )
  # )
  ,
  #### Sens Calc Drought ####
  tar_target(
    sens_calc_drought,
    sens_do_calc_drought(
      dat_spei
    )
  )
  ,
  #### Sens Agg Drought ####
  tar_target(
    sens_agg_drought,
    do_agg_drought(
      sens_calc_drought
    )
  )
  ,
  #### Sens Merge ####
  tar_target(
    sens_dat_mrg,
    do_mrg(
      dat_vacs,
      sens_agg_drought
    )
  )
  ,
  #### Sens Imputation ####
  tar_target(
    sens_dat_imp,
    do_impute(
      sens_dat_mrg
    )
  )
  # ,
  # #### Sens Table 2 ####
  # tar_target(
  #   sens_out_tab2,
  #   sens_tab_drought(
  #     sens_dat_mrg
  #   )
  # )
  ,
  ### SENS MODEL ####
  tar_target(
    sens_calc_model_bayesian,
    do_model_bayesian(
      sens_dat_imp
    )
  )
  ,
  #### Sens Table Odds ####
  tar_target(
    sens_out_tab3_bayes,
    tab_bayesian(
      sens_calc_model_bayesian
    )
  )
  )


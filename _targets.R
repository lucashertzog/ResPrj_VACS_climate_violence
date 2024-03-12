library(targets)
source("config.R")
tar_source()

tar_option_set(
  packages =
    c("targets",
      "data.table",
      "lubridate",
      "haven",
      "survey"
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
  
  
)

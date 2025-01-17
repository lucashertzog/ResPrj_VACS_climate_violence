library(targets)
tar_source()
load_packages(T)
source("config.R")
tar_visnetwork(targets_only = T)
tar_make(names = c(
  "dat_vacs",
  "dat_spei",
  "calc_drought",
  "agg_drought",
  "dat_mrg",
  "dat_imp"
  ))

# useful
tar_invalidate()
tar_objects()
tar_load_everything()

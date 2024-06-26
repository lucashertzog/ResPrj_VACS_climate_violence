library(targets)
tar_source()
load_packages(T)
source("config.R")
# lapply(list.files("R", full.names = TRUE), source)
tar_visnetwork(targets_only = T)
tar_make(names = c(
  "calc_drought",
  "agg_drought",
  "dat_mrg"
  ))

# useful
tar_invalidate()
tar_objects()
tar_load_everything()

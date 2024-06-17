library(targets)
tar_source()
load_packages(T)
source("config.R")
# lapply(list.files("R", full.names = TRUE), source)
tar_visnetwork(targets_only = T)
tar_make(names = c("agg_drought",
                   "dat_mrg",
                   "dat_imp",
                   "sens_calc_drought",
                   "sens_agg_drought",
                   "sens_dat_mrg",
                   "sens_dat_imp"
                   ))

# useful
tar_invalidate()
tar_objects()
tar_load_everything()

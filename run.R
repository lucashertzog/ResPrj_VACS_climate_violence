library(targets)
tar_source()
load_packages(T)
source("config.R")
lapply(list.files("R", full.names = TRUE), source)
tar_visnetwork(targets_only = T)
tar_make()

# useful
tar_invalidate(dat_spei)
tar_objects()
tar_load_everything()

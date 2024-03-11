library(targets)
tar_source()
load_packages(T)
lapply(list.files("R", full.names = TRUE), source)
tar_visnetwork(targets_only = T)
tar_make()

# useful
tar_invalidate()
tar_objects()
tar_load_everything()

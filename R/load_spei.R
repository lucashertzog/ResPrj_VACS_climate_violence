load_spei <- function(
    dir
){
  spei <- readRDS(file.path(rootdir, file_dat_spei))
  setDT(spei)
  
  # spei_out [, spei_cat := fcase(
  #   spei_avg >= 2, "Extremely wet",
  #   spei_avg >= 1.5 & spei_avg < 2, "Severely wet",
  #   spei_avg >= 1 & spei_avg < 1.5, "Moderately wet",
  #   spei_avg > 0.5 & spei_avg < 1, "Mildly wet",
  #   spei_avg > -0.5 & spei_avg <= 0.5, "Normal",
  #   spei_avg > -1 & spei_avg < -0.5, "Mild drought",
  #   spei_avg > -1.5 & spei_avg <= -1, "Moderate drought",
  #   spei_avg > -2 & spei_avg <= -1.5, "Severe drought",
  #   spei_avg <= -2, "Extreme drought"
  #   )]
  # 
 
  return(spei)
}
# hostname solution for multiple-user config.R
hostname <- Sys.info()["nodename"]

if (hostname == "C-A0055162" || hostname == "C-A0063844") { 
  rootdir <- "C:/Users/291828H/OneDrive - Curtin/projects/ResPrj_VACS_climate_violence"
}

file_dat_vacs <- "data_provided/vacs.dta"
file_dat_spei <- "data_provided/spei.rds"

figstabs <- "figures_and_tables/"
tab1 <- "table1.docx"

load_vacs <- function(
    dir
){
  vacs <- read_dta(file_dat_vacs)
  setDT(vacs)
  
  vacs[] <- lapply(
    vacs,
    function(x){
      attr(x, "label") <- NULL
      return(x)
    }
  )
  
  country_names <- setNames(
    c("Cambodia", "Colombia", "Côte d'Ivoire", "El Salvador", "Kenya", "Lesotho", "Malawi",
      "Moldova", "Mozambique", "Namibia", "Nigeria", "Uganda", "Zambia", "Zimbabwe"),
    c("cambodia", "colombia", "cote_divoire", "el_salvador", "kenya", "lesotho", "malawi",
      "moldova", "mozambique", "namibia", "nigeria", "uganda", "zambia", "zimbabwe")
  )
  
  vacs[, adm0 := country_names[adm0]]
  
  setDT(vacs)
  
  vacs$viol_sex <- as.factor(vacs$viol_sex)
  vacs$marital <- as.factor(vacs$marital)
  vacs$edu_enrol <- as.factor(vacs$edu_enrol)
  vacs$pvt <- as.factor(vacs$pvt)
  vacs$ever_viol_sex <- as.factor(vacs$ever_viol_sex)
  
  vacs[adm0 %in% c("Côte d'Ivoire", "Lesotho", "Moldova", "Mozambique"), adm2 := NA]
  
  vacs <- subset(vacs, select = -c(viol_ipv, ever_viol_ipv))
  
  # spei was not calculated San Andres, lets remove participants
  vacs <- vacs[!adm1 %in% c("San Adres y Providencia")]
  vacs <- vacs[!adm2 %in% c("San Andrés")]
  
  return(vacs)
}
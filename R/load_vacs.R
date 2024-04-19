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
  
  foo <- vacs
  setDT(foo)
  
  foo$viol_sex <- as.factor(foo$viol_sex)
  foo$viol_ipv <- as.factor(foo$viol_ipv)
  foo$ever_viol_ipv <- as.factor(foo$ever_viol_ipv)
  foo$marital <- as.factor(foo$marital)
  foo$edu_enrol <- as.factor(foo$edu_enrol)
  foo$pvt <- as.factor(foo$pvt)
  foo$ever_viol_sex <- as.factor(foo$ever_viol_sex)
  
  return(foo)
}
load_vacs <- function(
    dir
){
  foo <- read_dta(file_dat_vacs)
  setDT(foo)
  
  foo[] <- lapply(
    foo,
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
  
  foo[, adm0 := country_names[adm0]]
  
  
  
  return(vacs)
}
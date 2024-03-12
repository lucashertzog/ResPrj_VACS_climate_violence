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
    c("Cambodia", "Colombia", "Côte d'Ivoire", "El Salvador", "Honduras", "Kenya", "Lesotho", "Malawi",
      "Moldova", "Mozambique", "Namibia", "Nigeria", "Zambia", "Zimbabwe"),
    c("cambodia", "colombia", "cote_divoire", "el_salvador", "honduras", "kenya", "lesotho", "malawi",
      "moldova", "mozambique", "namibia", "nigeria", "zambia", "zimbabwe")
  )
  
  foo[, country := country_names[country]]
  
  vacs <- setnames(foo, "country", "adm_0")
  
  return(vacs)
}
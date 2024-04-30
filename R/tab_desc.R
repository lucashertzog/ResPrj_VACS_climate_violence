tab_desc <- function(
    dat_vacs
){
  # tar_load(dat_vacs)
  foo <- dat_vacs

  # Creating age groups
  foo[, age_group := cut(age, breaks = c(12, 17, 24), labels = c("13-17", "18-24"), include.lowest = TRUE)]
  
  design <- survey::svydesign(
    id = ~ cluster,
    strata = ~ strata,
    weights = ~ weight,
    data = foo,
    single = "centered",
    nest = TRUE
  )
  
  control_var <- c(
    "edu_enrol",
    "marital",
    "pvt"
  )
  
  outcome_var <- c(
    "ever_viol_sex",
    "viol_sex"
  )
  
  vars_to_recode <- c(control_var, outcome_var)
  
  for (var in vars_to_recode) {
    if (var %in% colnames(foo)) {
      # Determine which levels to include based on data presence
      existing_levels <- unique(na.omit(foo[[var]]))
      level_labels <- if ("98" %in% existing_levels) {
        setNames(c("No", "Yes", "Not Applicable"), c("0", "1", "98"))
      } else {
        setNames(c("No", "Yes"), c("0", "1"))
      }
      
      # Apply the appropriate factor recoding
      foo[, (var) := factor(get(var), levels = names(level_labels), labels = level_labels)]
    }
  }
  
  t <- 
    gtsummary::tbl_summary(
      data = foo,
      # by = adm_0,
      missing = "ifany",
      include = c("age", age_group, control_var, outcome_var, "adm0"),
      statistic = list(all_continuous() ~ "{mean} ({sd})", 
                       all_categorical() ~ "{n} / {N} ({p}%)"),
      # statistic = list(age ~ "{mean} ({sd})", all_categorical() ~ "{p}%"),
      type = list(age_group ~ "categorical",
                  age ~ "continuous"),
      label = list(age ~ "Age",
                   age_group ~ "Age group",
                   edu_enrol ~ "Attending school",
                   marital ~ "Ever married/relationship",
                   pvt ~ "Household is impoverished",
                   ever_viol_sex ~ "Sexual violence (lifetime)",
                   viol_sex ~ "Sexual violence (previous 12 months)",
                   adm0 ~ "Sample by country"
      ),
      missing_text = "Missing"
    ) %>%
    gtsummary::add_ci(include = all_categorical()) 
  
  # gtsummary::as_flex_table(t) %>%
  #   flextable::save_as_docx(path = file.path(figstabs, tab1))
  
  return(t)
}
do_impute <- function(
    dat
){
  
  tar_load(dat_vacs)
  
foo <- dat_vacs

setDT(foo)
# Convert specified variables to factors
special_cols <- c("viol_sex", "viol_ipv", "ever_viol_ipv")
foo[, (special_cols) := lapply(.SD, as.factor), .SDcols = special_cols]

# Backup '98' values
backup_98 <- foo[, lapply(.SD, function(x) x == "98"), .SDcols = special_cols]

# Replace '98' with NA for imputation process
for (col in special_cols) {
  for (i in seq_len(nrow(foo))) {
    if (!is.na(backup_98[[i, col]]) && backup_98[[i, col]]) {
      foo[i, (col) := NA_character_]
    }
  }
}

# methods for imputation
method_list <- rep("pmm", ncol(foo))
names(method_list) <- names(foo)
method_list[sapply(foo, is.factor)] <- "logreg"

# multiple imp
imputed_data <- mice(foo, m = 5, method = method_list, seed = 500, maxit = 10)

# Complete the data (example uses the first imputation)
completed_data <- complete(imputed_data, action = "long")

# Restore '98' values from backup

setDT(completed_data) 

### WORK FROM HERE 19/04/2014
## this step wont work as I have 5 imputed datasets:
for (col in special_cols) {
  for (i in seq_len(nrow(completed_data))) {
    if (!is.na(backup_98[[i, col]]) && backup_98[[i, col]]) {
      completed_data[i, (col) := "98"]
    }
  }
}


# Convert columns back to factor, ensuring '98' is included as a level
completed_data[, (special_cols) := lapply(.SD, function(x) factor(x, levels = c("0", "1", "98"))), .SDcols = special_cols]


foo <- completed_data
foo$viol_sex <- as.factor(foo$viol_sex)
foo$viol_ipv <- as.factor(foo$viol_ipv)
foo$ever_viol_ipv <- as.factor(foo$ever_viol_ipv)
foo$marital <- as.factor(foo$marital)
foo$edu_enrol <- as.factor(foo$edu_enrol)
foo$pvt <- as.factor(foo$pvt)
foo$ever_viol_sex <- as.factor(foo$ever_viol_sex)

imp <- foo

return(foo)
}
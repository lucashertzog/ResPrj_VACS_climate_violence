tab_odds <- function(
    calc_model1
){
data <- calc_model1[["viol_sex.imputed"]]

setDT(data)

data[, odds_ratios := as.numeric(odds_ratios)]
data[, ci_lower := as.numeric(ci_lower)]
data[, ci_upper := as.numeric(ci_upper)]

tab_odds <- data[c(2, 8, 14, 20), 
                      .(Dryness_pattern = predictor,
                        aOR_95CI = sprintf("%.2f (%.2f to %.2f)", odds_ratios, ci_lower, ci_upper),
                        P_value = p_values_formatted)]

return(tab_odds)
}
plot_sex_viol <- function(
    dat_vacs
){
  # tar_load(dat_vacs)
  # library(survey)
  setDT(dat_vacs)
  
  psu_counts <- dat_vacs[, .(n_psu = uniqueN(cluster)), by = strata]
  valid_strata <- psu_counts[n_psu > 1, strata]
  dat_vacs_filtered <- dat_vacs[strata %in% valid_strata]
  
  dat_vacs_filtered$ever_viol_sex <- factor(dat_vacs_filtered$ever_viol_sex, levels = c(0, 1))
  
  design <- survey::svydesign(
    id = ~ cluster,
    strata = ~ strata,
    weights = ~ weight,
    data = dat_vacs_filtered,
    single = "centered",
    nest = TRUE
  )
  
  # Calculate the proportions and their confidence intervals by adm0 directly
  results <- svyby(
    ~ ever_viol_sex,
    ~adm0, 
    design, 
    svyciprop,
    method="logit",
    na.rm=TRUE
  )
  
  results$lower_ci <- confint(results)[, 1]
  results$upper_ci <- confint(results)[, 2]
  

  # Adjust viol_sex to be 0 for all who answered 0 to ever_viol_sex
  dat_vacs_filtered$viol_sex_adj <- ifelse(dat_vacs_filtered$ever_viol_sex == 1, dat_vacs_filtered$viol_sex, 0)
  dat_vacs_filtered$viol_sex_adj <- factor(dat_vacs_filtered$viol_sex_adj, levels = c(0, 1))
  
  # Re-create the survey design with the adjusted dataset
  design_total <- survey::svydesign(
    id = ~ cluster,
    strata = ~ strata,
    weights = ~ weight,
    data = dat_vacs_filtered,
    single = "centered",
    nest = TRUE
  )
  
  # Calculate the proportions and their confidence intervals for viol_sex_adj in the total sample by adm0
  results_total_recent_viol_sex <- svyby(
    ~ viol_sex_adj,
    ~ adm0,
    design_total,
    svyciprop,
    method = "logit",
    na.rm = TRUE
  )
  
  # Extract confidence intervals
  results_total_recent_viol_sex$lower_ci <- confint(results_total_recent_viol_sex)[, 1]
  results_total_recent_viol_sex$upper_ci <- confint(results_total_recent_viol_sex)[, 2]
  
  # Merge the results on the 'adm0' column
  final_results <- merge(results, results_total_recent_viol_sex, by = "adm0", suffixes = c("_ever", "_recent"))
  
  # Rename columns for clarity
  names(final_results) <- c("adm0", 
                            "Proportion_Ever_Viol_Sex", "SE_Ever_Viol_Sex", "Lower_CI_Ever", "Upper_CI_Ever",
                            "Proportion_Recent_Viol_Sex", "SE_Recent_Viol_Sex", "Lower_CI_Recent", "Upper_CI_Recent")
  
  
  # Rounding all numerical columns to two decimal places
  final_results$Proportion_Ever_Viol_Sex <- round(final_results$Proportion_Ever_Viol_Sex, 2)
  final_results$SE_Ever_Viol_Sex <- round(final_results$SE_Ever_Viol_Sex, 2)
  final_results$Lower_CI_Ever <- round(final_results$Lower_CI_Ever, 2)
  final_results$Upper_CI_Ever <- round(final_results$Upper_CI_Ever, 2)
  
  final_results$Proportion_Recent_Viol_Sex <- round(final_results$Proportion_Recent_Viol_Sex, 2)
  final_results$SE_Recent_Viol_Sex <- round(final_results$SE_Recent_Viol_Sex, 2)
  final_results$Lower_CI_Recent <- round(final_results$Lower_CI_Recent, 2)
  final_results$Upper_CI_Recent <- round(final_results$Upper_CI_Recent, 2)
  

  final_results$Proportion_Recent_Viol_Sex <- final_results$Proportion_Recent_Viol_Sex * 100
  
  BLUE <- "#076fa2"
  RED <- "#E3120B"
  PINK <- "#F28784"
  BLACK <- "#202020"
  GREY <- "grey50"
 

  # Melt data
  setDT(final_results)
  final_results_long <- melt(final_results, 
                             id.vars = "adm0", 
                             measure.vars = c("Proportion_Ever_Viol_Sex", "Proportion_Recent_Viol_Sex"),
                             variable.factor = FALSE)
  
  # Create factors with nice labels
  final_results_long[, variable := factor(variable, 
                                          levels = c("Proportion_Ever_Viol_Sex", "Proportion_Recent_Viol_Sex"),
                                          labels = c("Ever", "Recent"))]
  
  # Plot
  plt <- ggplot(final_results_long, aes(x = value, y = adm0, fill = variable)) +
    geom_bar(stat = "identity", position = "identity") +
    scale_fill_manual(values = c("Ever" = PINK, "Recent" = RED)) +
    scale_x_continuous(limits = c(0, 45), breaks = seq(0, 45, by = 5), expand = c(0, 0), position = "top") +
    scale_y_discrete(expand = expansion(add = c(0, 0.5))) +
    theme(panel.background = element_rect(fill = "white"),
          panel.grid.major.x = element_line(color = "#A8BAC4", size = 0.3),
          axis.ticks.length = unit(0, "mm"),
          axis.title = element_blank(),
          axis.line.y.left = element_line(color = "black"),
          axis.text.y = element_blank(),
          axis.text.x = element_text(size = 16)) +
          # axis.text.x = element_text(family = "Econ Sans Cnd", size = 16)) +
    geom_shadowtext(data = final_results_long[variable == "Ever" & value < 8],
                    aes(label = adm0, x = value), 
                    hjust = 0, nudge_x = 0.3, 
                    # bg.colour = "white", bg.r = 0.2, family = "Econ Sans Cnd", size = 7) +
    bg.colour = "white", bg.r = 0.2, size = 7) +
    geom_text(data = final_results_long[variable == "Ever" & value >= 8], 
              aes(label = adm0, x = 0), 
              hjust = 0, nudge_x = 0.3, colour = "white", 
              # family = "Econ Sans Cnd", 
              size = 7)
  
  # Display the plot
  print(plt)
}

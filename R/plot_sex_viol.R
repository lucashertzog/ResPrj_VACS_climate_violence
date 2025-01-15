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
  

  cols <- names(final_results)[-1]
  final_results[cols] <- final_results[cols] * 100
  
  foo <- final_results[, !names(final_results) %in% c("adm0, SE_Ever_Viol_Sex", "SE_Recent_Viol_Sex")]

  BLUE <- "#076fa2"
  RED <- "#E3120B"
  PINK <- "#F28784"
  BLACK <- "#202020"
  GREY <- "grey"
  
  foo$adm0 <- factor(foo$adm0, levels = foo$adm0[order(foo$Proportion_Ever_Viol_Sex, decreasing = FALSE)])
  
  library(shadowtext)
  library(ggplot2)
  
 p <- ggplot(data = foo, aes(y = adm0)) +
    geom_col(aes(x = Proportion_Ever_Viol_Sex, fill = "Lifetime"), width = 0.6, position = position_dodge(width = 0.6)) +
    geom_col(aes(x = Proportion_Recent_Viol_Sex, fill = "Recent (12 months)"), width = 0.6) +
    scale_x_continuous(
      limits = c(0, 45),
      breaks = seq(0, 45, by = 5),
      expand = c(0, 0),
      position = "top"
    ) +
    scale_y_discrete(expand = expansion(add = c(0, 0.5))) +
    scale_fill_manual(values = c("Lifetime" = PINK, "Recent (12 months)" = RED)) +
    theme(
      panel.background = element_rect(fill = "white"),
      panel.grid.major.x = element_line(color = "#A8BAC4", size = 0.3),
      axis.ticks.length = unit(0, "mm"),
      axis.title = element_blank(),
      axis.line.y.left = element_line(color = BLACK),
      axis.text.y = element_blank(),
      axis.text.x = element_text(family = "Econ Sans Cnd", size = 14),
      legend.position = "top",
      legend.direction = "horizontal",
      legend.title = element_blank(),
      legend.text = element_text(family = "Econ Sans Cnd"),
      plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm")
    ) +
    geom_shadowtext(
      data = subset(foo, Proportion_Ever_Viol_Sex < 15),
      aes(x = Proportion_Ever_Viol_Sex + 0.3, label = adm0),
      hjust = 0,
      colour = RED,
      bg.colour = "white",
      bg.r = 0.2,
      family = "Econ Sans Cnd",
      size = 5
    ) +
    geom_text(
      data = subset(foo, foo$Proportion_Ever_Viol_Sex >= 15),
      aes(x = 0, label = adm0),
      hjust = 0,
      nudge_x = 0.3,
      colour = "white",
      family = "Econ Sans Cnd",
      size = 5
    )
  
  #ggsave("figures_and_tables/fig1.png", plot = p, width = 10, height = 11, dpi = 300, units = "in")
  
 ggsave(
   filename = "figures_and_tables/fig1.eps",  # Change the file extension to .eps
   plot = p,
   width = 10, 
   height = 11, 
   dpi = 300, 
   units = "in", 
   device = cairo_ps  # Use the cairo_ps device for EPS output
 )
 
  return(p)
 
}




# foo1 <- foo[, c("Proportion_Recent_Viol_Sex", "Proportion_Ever_Viol_Sex",
#                 "Lower_CI_Ever", "Upper_CI_Ever",
#                 "Lower_CI_Recent", "Upper_CI_Recent")]
# 
# countries <- foo$adm0
# 
# max_values <- apply(foo1, 1, max)
# 
# par(mar = c(2,6,2,2))
# 
# 
# ever <-  barplot(t(foo1[, "Proportion_Ever_Viol_Sex", drop = FALSE]),
#                  horiz = TRUE,
#                  col = PINK,
#                  border = PINK,
#                  space = 0.6,
#                  names.arg = countries, 
#                  las = 1,
#                  xlim = c(0, max(max_values)),
#                  axes = FALSE)
# 
# recent <-  barplot(t(foo1[, "Proportion_Recent_Viol_Sex", drop = FALSE]),
#                    horiz = TRUE,
#                    col = RED,
#                    border = RED,
#                    space = 0.6,
#                    names.arg = countries, 
#                    las = 1,
#                    add = TRUE,  # Important: This adds to the existing plot
#                    xlim = c(0, max(max_values)),
#                    axes = FALSE)  
# 
# 
# box(bty = "n") 
# # axis(side = 3, at = seq(0, max(max_values), by = 10), labels = TRUE, las = 0, lwd = 1)
# 
# abline(v=40, lty=1, col= GREY)
# abline(v=30, lty=1, col= GREY)
# abline(v=20, lty=1, col= GREY)
# abline(v=10, lty=1, col= GREY)
# abline(h=23.25, lty=1, col = BLACK)
# abline(v=0, lty=1, col= BLACK)
# 
# legend("bottomright",
#        # inset = c(-0.1, -0.1),
#        legend = c("Recent", "Lifetime"), 
#        fill = c(RED, PINK), 
#        cex = 0.75,  
#        horiz = TRUE,
#        bty = "y") 





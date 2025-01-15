#tar_load(dat_imp)

plot_drought_venn <- function(dat_imp){

  library(VennDiagram)
  
  # Identify overlaps
  dat_imp$overlap <- apply(dat_imp[, .(zero_to_moderate, very_dry_drought_extreme, constant_drought_extreme, recent_long_period)], 1, function(x) {
    paste(names(x)[which(x == 1)], collapse = ", ")
  })
  
  # Create counts for each category combination
  overlap_counts <- data.frame(
    zero_to_moderate = dat_imp$zero_to_moderate,
    very_dry_drought_extreme = dat_imp$very_dry_drought_extreme,
    constant_drought_extreme = dat_imp$constant_drought_extreme,
    recent_long_period = dat_imp$recent_long_period
  )
  
  # Create a Venn diagram
  venn_data <- list(
    `Slight to Moderate` = which(dat_imp$zero_to_moderate == 1),
    `Very Dry` = which(dat_imp$very_dry_drought_extreme == 1),
    `Prolonged and Extreme` = which(dat_imp$constant_drought_extreme == 1),
    `Recent and Long` = which(dat_imp$recent_long_period == 1)
  )
  
  tiff("figures_and_tables/Figure2_venn_diagram.tiff", width = 10, height = 8, units = "in", res = 300)
  
  venn.plot <- venn.diagram(
    x = venn_data,
    filename = NULL,
    fill = c("darkgrey", "orange", "#EC5E5E", "#78C0D6"),
    cex = 1,
    cat.cex = 1,
    cat.pos = 0,
    cat.dist = 0.05,
    fontface = "plain",
    fontfamily = "sans",
    cat.fontfamily = "sans",
    print.mode = c("raw", "percent"),
    scaled = TRUE,
    lty = "blank"
  )
  
  # Plot the Venn diagram
  grid.newpage()
  grid.draw(venn.plot)
  
  dev.off()
}





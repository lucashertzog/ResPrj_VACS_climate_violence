#tar_load(dat_imp)

plot_drought_overlap <- function(dat_imp){
  

# Load required libraries
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


venn.plot <- venn.diagram(
  x = venn_data,
  filename = NULL,
  fill = c("red", "blue", "green", "purple"),
  alpha = 0.5,
  cex = 1.5,
  cat.cex = 1.5,
  cat.pos = 0,
  cat.dist = 0.05,
  cat.col = c("black", "black", "black", "black"),
  margin = 0.1
)

# Plot the Venn diagram
grid.newpage()
grid.draw(venn.plot)
}

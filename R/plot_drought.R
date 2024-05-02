plot_drought <- function(
    calc_drought
){
  
foo <- setDT(calc_drought)

foo1 <- foo[, c("adm0",
             "adm1",
             "adm2",
             "date",
             "dry_month",
             "zero_to_moderate",
             "very_dry_drought_extreme",
             "recent_long_period",
             "constant_drought_extreme",
             "standardised_intensity")]

foo1$dry_month <- as.numeric(foo1$dry_month)
foo1$zero_to_moderate <- as.numeric(foo1$zero_to_moderate)
foo1$very_dry_drought_extreme <- as.numeric(foo1$very_dry_drought_extreme)
foo1$recent_long_period <- as.numeric(foo1$recent_long_period)
foo1$constant_drought_extreme <- as.numeric(foo1$constant_drought_extreme)

foo1_summary <- foo1[, .(
  total_dry_month = sum(dry_month, na.rm = TRUE),
  total_zero_to_moderate = sum(zero_to_moderate, na.rm = TRUE),
  total_very_dry = sum(very_dry_drought_extreme, na.rm = TRUE),
  total_recent_long = sum(recent_long_period, na.rm = TRUE),
  total_constant = sum(constant_drought_extreme, na.rm = TRUE)),
                     by = date]

foo1_summary[, total := total_zero_to_moderate + total_dry_month + total_very_dry]

foo1_summary[, `:=` (perc_zero_to_moderate = total_zero_to_moderate / total * 100,
                     perc_very_dry = total_very_dry / total * 100,
                     perc_dry = total_dry_month / total * 100)]

date_svy <- as.Date(c("2013-02-01", "2013-09-01", "2014-05-01", "2014-08-01", 
                            "2015-09-01", "2017-01-01", "2017-11-01", "2018-06-01", 
                            "2018-08-01", "2018-10-01", "2018-12-01", "2019-03-01", 
                            "2019-07-01"))

p <- ggplot(data = foo1_summary) +
  geom_area(aes(x = date, y = perc_zero_to_moderate, fill = "Zero to moderate"), alpha = 0.8) +
  geom_area(aes(x = date, y = perc_dry, fill = "Dry"), alpha = 0.8) +
  geom_area(aes(x = date, y = perc_very_dry, fill = "Very dry"), alpha = 0.8) +
  scale_y_continuous(
    limits = c(0, 100),
    breaks = seq(0, 100, by=25),
    expand = c(0, 0),
    position = "right"
  ) +
  scale_x_date(
    expand = expansion(add = c(0, 30)),
    date_breaks = "1 year",
    date_labels = "%Y"
    ) +
   scale_fill_manual(
     values = c("Very dry" = "#9D252B", "Dry" = "#F28784", "Zero to moderate" = "#EDCF9D", "Data collection" = "deepskyblue3"),
     limits = c("Very dry", "Dry", "Zero to moderate", "Data collection")) +
  theme(
    panel.background = element_rect(fill = "white"),
    panel.grid.major.y = element_line(color = "#A8BAC4", size = 0.3),
    axis.ticks.length = unit(0, "mm"),
    axis.title = element_blank(),
    axis.line.x = element_line(color = "#202020"),
    axis.text.x = element_text(family = "Econ Sans Cnd", size = 14),
    axis.text.y = element_text(family = "Econ Sans Cnd", size = 14, vjust = -0.5),
    legend.position = "top",
    legend.direction = "horizontal",
    legend.title = element_blank(),
    legend.text = element_text(family = "Econ Sans Cnd"),
    plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm")
  ) +
  geom_point(data = data.frame(date = date_svy), aes(x = date, y = 0.7), shape = 22, fill = "deepskyblue3", size = 3)

ggsave("figures_and_tables/fig2.png", plot = p, width = 10, height = 6, dpi = 300, units = "in")

return(p)
}
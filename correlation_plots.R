
# Correlation Plots -------------------------------------------------------
library(tidyverse) #for everything
library(ggh4x) #for facet_grid2

#pivot longer so all of the x-axis variables are in a "value" column

df_plot <- df %>%
  pivot_longer(cols = c(light_intensity, dead_plant_pct,
                        days_since_precip, air_temp, num_species),
               names_to = "variable", 
               values_to = "value") %>%
  mutate(variable = factor(variable, 
                           levels = c("light_intensity", "dead_plant_pct", 
                                      "days_since_precip", "air_temp", "num_species"),
                           labels = c("Light Intensity (Lux)", "Dead Plant %", 
                                      "Days Since Precipitation", "Air Temperature (ºC)", 
                                      "Number of Genera")))
# Corrected plot
# First calculate R² and P values for each facet
stats_data <- df_plot %>%
  group_by(group, variable) %>%
  summarise(
    model = list(lm(carbon_flux ~ value)),
    r_squared = sapply(model, function(x) summary(x)$r.squared),
    p_value = sapply(model, function(x) summary(x)$coefficients[2, 4]),
    .groups = "drop"
  ) %>%
  mutate(
    p_text = case_when(
      p_value < 0.001 ~ "p < 0.001",
      TRUE ~ paste("p =", round(p_value, 2))
    ),
    r2_text = paste("R² =", round(r_squared, 2)),
    label_text = paste(r2_text, p_text, sep = "\n")
  ) %>%
  select(group, variable, label_text)

# Then add to your plot
correlation_plots=ggplot(df_plot, aes(x = value, y = carbon_flux, color = group, fill = group)) +
  theme_classic() +
  geom_point(shape = 21, color = "black", size = 2.5) +
  geom_smooth(data = df_plot %>% filter(!(group == "Phedimus" & variable == "Number of Genera")), 
              method = "lm") +
geom_text(
  data = stats_data %>% 
    filter(!(group == "Phedimus" & variable == "Number of Genera")), 
  aes(label = label_text), 
  x = -Inf, y = Inf, 
  hjust = -0.1, vjust = 1.1,
  size = 3.5, color = "black",
  inherit.aes = FALSE
)+
  scale_fill_manual(values = c(
    "Poa" = "#97b66b",       # green
    "Mixed" = "#6b97b6",     # blue
    "Phedimus" = "#b66b97"   # red
  )) +
  scale_color_manual(values = c(
    "Poa" = "#97b66b",       # green
    "Mixed" = "#6b97b6",     # blue
    "Phedimus" = "#b66b97"   # red
  )) +
  facet_grid2(group ~ variable, scales = "free", independent = "x", switch = "both") +
  theme(strip.background = element_blank(),
        strip.placement = "outside",
        legend.position = "none",
        axis.title = element_text(size = 14),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        strip.text = element_text(size = 12)) +
  labs(y = bquote(NEE~(mmol~CO[2]~m^{-2}~hr^{-1})), x = NULL)
print (correlation_plots)
ggsave("correlation_plot_models.png", correlation_plots, width = 20, height = 10, dpi = 300)


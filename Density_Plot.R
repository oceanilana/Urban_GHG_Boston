
# Density Plot - All Data -------------------------------------------------
library(lubridate)
library(ggridges)
library(tidyverse)

flux_long <- flux_data %>%
  select(site, date, carbon_flux, ends_with("_pct")) %>%
  mutate(month = month(date, label = TRUE, abbr = TRUE)) %>%
  mutate(month = factor(month, levels = c("Jul", "Sep", "Oct", "Nov", "Feb"))) %>%
  pivot_longer(cols = ends_with("_pct"), names_to = "genus", values_to = "cover_pct") %>%
  filter(cover_pct > 0) %>%
  mutate(genus = str_remove(genus, "_pct"))  # Clean genus names

density_plot <- ggplot(flux_long, aes(x = carbon_flux, y = month, fill = stat(x))) +   
  geom_density_ridges_gradient(scale = 2, rel_min_height = 0.01, alpha = 0.9) +    
  scale_fill_gradient2(name = "Flux", 
                       low = "darkgreen", mid = "grey93", high = "tomato3", 
                       midpoint = 0) +  
  labs(x = bquote(NEE~(mmol~m^{-2}~hr^{-1})), y = "Month") +   
  scale_y_discrete(limits = rev(c("Jul", "Sep", "Oct", "Nov", "Feb"))) +
  theme_minimal(base_size = 14) +
  theme(
    panel.grid = element_blank(),
    axis.line = element_line(color = "black"),
    legend.position = "right",
    axis.text = element_text(color = "black"),       # <- make axis text black
    axis.title = element_text(color = "black")       # <- make axis titles black
  )

density_plot
ggsave("density_ridge_plot.png", density_plot, width = 14, height = 10, dpi = 300)




# flux_summary <- flux_data %>%
#   mutate(month = month(date, label = TRUE, abbr = TRUE)) %>%
#   mutate(month = factor(month, levels = c("Jul", "Sep", "Oct", "Nov", "Feb"))) %>%
#   group_by(month) %>%
#   summarise(
#     n_fluxes = n(),
#     median_flux = median(carbon_flux, na.rm = TRUE),
#     min_flux = min(carbon_flux, na.rm = TRUE),
#     max_flux = max(carbon_flux, na.rm = TRUE),
#     mean_flux=mean(carbon_flux, na.rm=T),
#     mean_dead_plant_pct = mean(dead_plant_pct, na.rm = TRUE)
#     
#   )
# 
# print(flux_summary)
# 
# library(dplyr)
# library(lubridate)
# library(knitr)
# 
# flux_summary <- flux_data %>%
#   mutate(month = month(date, label = TRUE, abbr = TRUE)) %>%
#   mutate(month = factor(month, levels = c("Jul", "Sep", "Oct", "Nov", "Feb"))) %>%
#   group_by(month) %>%
#   summarise(
#     `Number of Fluxes` = n(),
#     `Median Flux` = median(carbon_flux, na.rm = TRUE),
#     `Minimum Flux` = min(carbon_flux, na.rm = TRUE),
#     `Maximum Flux` = max(carbon_flux, na.rm = TRUE),
#     `Mean Flux` = mean(carbon_flux, na.rm = TRUE),
#     `Mean % Dead Plant Cover` = mean(dead_plant_pct, na.rm = TRUE)
#   )
# 
# kable(flux_summary, format = "markdown", digits = 2)
# 
# 
# flux_summary
# 
# weather_summary=flux_data%>%
#   mutate(month = month(date, label = TRUE, abbr = TRUE)) %>%
#   mutate(month = factor(month, levels = c("Jul", "Sep", "Oct", "Nov", "Feb"))) %>%
#   group_by(month) %>%
#   summarise(
#     median_temp=median(air_temp, na.rm=T),
#     min_temp=min(air_temp, na.rm = T),
#     max_temp=max(air_temp, na.rm = T),
#     mean_temp=mean(air_temp, na.rm=T),
#     median_light=median(light_intensity, na.rm=T), 
#     min_light=min(light_intensity, na.rm = T),
#     max_light=max(light_intensity, na.rm = T),
#     mean_light=mean(light_intensity, na.rm=T),
#     median_precip=median(days_since_precip, na.rm = T),
#     min_precip=min(days_since_precip, na.rm=T),
#     max_precip=max(days_since_precip, na.rm=T),
#     mean_precip=mean(days_since_precip, na.rm=T), 
#     median_green=median(green_cover, na.rm = T),
#     min_green=min(green_cover, na.rm=T), 
#     max_green=max(green_cover, na.rm=T),
#     mean_green=mean(green_cover, na.rm=T)
#   )
# temp_summary
# flux_data$days_since_precip
# flux_data$area_sq_m
# 
# 
# site_areas <- flux_data %>%
#   select(site, area_sq_m) %>%
#   distinct()
# 
# print(site_areas)


# Mixed Group Density Plot ------------------------------------------------

flux_long_mixed <- Mixed %>%
  select(site, date, carbon_flux, ends_with("_pct")) %>%
  mutate(month = month(date, label = TRUE, abbr = TRUE)) %>%
  mutate(month = factor(month, levels = c("Jul", "Sep", "Oct", "Nov", "Feb"))) %>%
  pivot_longer(cols = ends_with("_pct"), names_to = "genus", values_to = "cover_pct") %>%
  filter(cover_pct > 0) %>%
  mutate(genus = str_remove(genus, "_pct"))  # Clean genus names

density_plot_mixed=ggplot(flux_long_mixed, aes(x = carbon_flux, y = month, fill = stat(x))) +   
  geom_density_ridges_gradient(scale = 2, rel_min_height = 0.01, alpha = 0.9) +    
  scale_fill_gradient2(name = "Flux", 
                       low = "darkgreen", mid = "grey93", high = "tomato3", 
                       midpoint = 0) +  
  labs(x = bquote(CO[2]~Flux~(mmol~m^-2~h^-1)), y = "Month") +   
  scale_y_discrete(limits = rev(c("Jul", "Sep", "Oct", "Nov", "Feb")))+
  theme_minimal(base_size = 14) +
  theme(
    panel.grid = element_blank(),
    axis.line = element_line(color = "black"),
    legend.position = "right"
  )
density_plot_mixed

# Poa Group Density Plot --------------------------------------------------

flux_long_poa <- Poa %>%
  select(site, date, carbon_flux, ends_with("_pct")) %>%
  mutate(month = month(date, label = TRUE, abbr = TRUE)) %>%
  mutate(month = factor(month, levels = c("Jul", "Sep", "Oct", "Nov", "Feb"))) %>%
  pivot_longer(cols = ends_with("_pct"), names_to = "genus", values_to = "cover_pct") %>%
  filter(cover_pct > 0) %>%
  mutate(genus = str_remove(genus, "_pct"))  # Clean genus names

density_plot_poa=ggplot(flux_long_poa, aes(x = carbon_flux, y = month, fill = stat(x))) +   
  geom_density_ridges_gradient(scale = 2, rel_min_height = 0.01, alpha = 0.9) +    
  scale_fill_gradient2(name = "Flux", 
                       low = "darkgreen", mid = "grey93", high = "tomato3", 
                       midpoint = 0) +  
  labs(x = bquote(CO[2]~Flux~(mmol~m^-2~h^-1)), y = "Month") +   
  scale_y_discrete(limits = rev(c("Jul", "Sep", "Oct", "Nov", "Feb")))+
  theme_minimal(base_size = 14) +
  theme(
    panel.grid = element_blank(),
    axis.line = element_line(color = "black"),
    legend.position = "right"
  )
density_plot_poa

# Phedimus Density Plot ---------------------------------------------------

flux_long_phedimus <- Phedimus %>%
  select(site, date, carbon_flux, ends_with("_pct")) %>%
  mutate(month = month(date, label = TRUE, abbr = TRUE)) %>%
  mutate(month = factor(month, levels = c("Jul", "Sep", "Oct", "Nov", "Feb"))) %>%
  pivot_longer(cols = ends_with("_pct"), names_to = "genus", values_to = "cover_pct") %>%
  filter(cover_pct > 0) %>%
  mutate(genus = str_remove(genus, "_pct"))  # Clean genus names

density_plot_phed=ggplot(flux_long_phedimus, aes(x = carbon_flux, y = month, fill = stat(x))) +   
  geom_density_ridges_gradient(scale = 2, rel_min_height = 0.01, alpha = 0.9) +    
  scale_fill_gradient2(name = "Flux", 
                       low = "darkgreen", mid = "grey93", high = "tomato3", 
                       midpoint = 0) +  
  labs(x = bquote(CO[2]~Flux~(mmol~m^-2~h^-1)), y = "Month") +   
  scale_y_discrete(limits = rev(c("Jul", "Sep", "Oct", "Nov", "Feb")))+
  theme_minimal(base_size = 14) +
  theme(
    panel.grid = element_blank(),
    axis.line = element_line(color = "black"),
    legend.position = "right"
  )
density_plot_phed


# by season we define -----------------------------------------------------

flux_long <- flux_data %>%
  select(site, date, carbon_flux, defined_season, ends_with("_pct")) %>%
  pivot_longer(cols = ends_with("_pct"), names_to = "genus", values_to = "cover_pct") %>%
  filter(cover_pct > 0) %>%
  mutate(genus = str_remove(genus, "_pct")) %>%  # Clean genus names
  filter(defined_season != "other") %>%  # Remove any "other" season data
  mutate(defined_season = factor(defined_season, levels = c("growing", "senescence", "dormant")))

density_plot <- ggplot(flux_long, aes(x = carbon_flux, y = defined_season, fill = stat(x))) +   
  geom_density_ridges_gradient(scale = 1.5, rel_min_height = 0.01, alpha = 0.9) +    
  scale_fill_gradient2(name = "Flux", 
                       low = "darkgreen", mid = "grey93", high = "tomato3", 
                       midpoint = 0) +  
  labs(x = bquote(CO[2]~Flux~(mmol~m^-2~h^-1)), y = "Season") +   
  scale_y_discrete(limits = rev(c("growing", "senescence", "dormant")))+
  theme_minimal(base_size = 14) +
  theme(
    panel.grid = element_blank(),
    axis.line = element_line(color = "black"),
    legend.position = "right"
  )

density_plot
ggsave("density_ridge_plot_by_season.png", density_plot, width = 14, height = 10, dpi = 300)


## Final Figure in paper -------------------------------------------------------

# First calculate sample sizes for each season
# Create sample sizes dataframe with correct values
# Create sample sizes dataframe with correct values
sample_sizes <- data.frame(
  defined_season = c("growing", "senescence", "dormant"),
  n = c(54, 60, 49),
  label = c("n = 54", "n = 60", "n = 49")
)

density_plot <- ggplot(flux_long, aes(x = carbon_flux, y = defined_season, fill = stat(x))) +   
  geom_density_ridges_gradient(scale = 1.5, rel_min_height = 0.01, alpha = 0.9) +    
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey80", size = 1) +  
  geom_text(data = sample_sizes, 
            aes(x = -Inf, y = defined_season, label = label), 
            hjust = -0.1, vjust = 0.5, 
            size = 5, color = "black", inherit.aes = FALSE) +  
  # Add emission/uptake labels
  annotate("text", x = Inf, y = -Inf, label = "Net Emission (+)", 
           hjust = 1.1, vjust = -0.5, size = 6, color = "tomato3") +  
  annotate("text", x = -Inf, y = -Inf, label = "Net Uptake (-)", 
           hjust = -0.1, vjust = -0.5, size = 6, color = "darkgreen") +  
  scale_fill_gradient2(name = "Flux", 
                       low = "darkgreen", mid = "grey93", high = "tomato3", 
                       midpoint = 0, guide = "none") +  
  labs(x = bquote(NEE~(mmol~CO[2]~m^{-2}~hr^{-1})), y = "Season") +   
  scale_y_discrete(limits = rev(c("growing", "senescence", "dormant")),
                   labels = rev(c("Growing", "Senescence", "Dormant"))) +  # Capitalized labels
  theme_minimal(base_size = 14) +
  theme(
    panel.grid = element_blank(),
    axis.line = element_line(color = "black"),
    legend.position = "right",
    axis.text = element_text(color = "black"),       
    axis.title = element_text(color = "black"),     
    axis.title.x = element_text(size = 18),  
    axis.title.y = element_text(size = 18),  
    axis.text.x = element_text(size = 16),   
    axis.text.y = element_text(size = 16)    
  )


density_plot
ggsave("density_ridge_plot_by_season.png", density_plot, width = 14, height = 10, dpi = 300)

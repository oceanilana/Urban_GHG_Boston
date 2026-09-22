## Percent Cover Plots 
#Pct Cover plots for Slide ----------------------------------------------------
library(tidyverse)

# Make the data "long" so we can make pie charts.
flux_data_long <- flux_data %>%
  pivot_longer(cols = ends_with("_pct"), names_to = "Category", 
               values_to = "PercentCover")

# Normalize the percent cover values within each site.
flux_data_long_pct <- flux_data_long %>%
  group_by(site, date) %>%
  mutate(PercentCover = PercentCover / sum(PercentCover) * 100) %>%
  ungroup()

flux_data_long_pct <- flux_data_long_pct %>%
  mutate(site = factor(as.numeric(site), 
                             levels = sort(unique(as.numeric(site)))))


# Format labels: remove "_pct", capitalize first letter, italicize
genus_labels <- setNames(
  lapply(gsub("_pct$", "", unique(flux_data_long_pct$Category)), function(x) {
    cleaned <- gsub("_", " ", x)
    bquote(italic(.(tools::toTitleCase(cleaned))))
  }),
  unique(flux_data_long_pct$Category)
)
# Custom labeller for dates
month_day_labeller <- function(x) {
  format(as.Date(x), "%b %d")  
}

# Pie charts by site and date
exclude_genera <- c("cerichums_pct", "lepidium_pct", "paniceae_pct") # these genera were at sites we sampled originally but did not return to and did not see again. they were not used in the analysis.
pie_charts=ggplot(flux_data_long_pct, aes(x = "", y = PercentCover, fill = Category)) + 
  geom_bar(width = 1, stat = "identity") + 
  coord_polar("y") + 
  facet_grid(site ~ date, 
             labeller = labeller(
               date = month_day_labeller,
               site = function(x) paste("Site", x)
             )) +
  scale_fill_manual(
    values = c("grey98", "lemonchiffon2", 
               "gold1", "grey87", "darkgreen", "darkseagreen2", "forestgreen",
                "turquoise4", "olivedrab",
                "mistyrose3", "aquamarine3", "darkseagreen", "lightblue3",
               "chartreuse3", "yellow2", "khaki2", "palegreen", "papayawhip","palegreen3","darkseagreen3"),
    labels = genus_labels, 
    breaks = sort(setdiff(unique(flux_data_long_pct$Category), exclude_genera))  ) + 
  labs(title = "Percent Cover of Plant Types by Site") + 
  theme_void() + 
  theme(
    legend.title = element_blank(),
    strip.text.y.left = element_text(angle = 0),  # keeps "Site X" horizontal
    strip.placement = "outside",                 # moves strip labels outside plot area
    strip.background = element_blank()
  )
print(pie_charts)

ggsave("pie_charts.png", pie_charts, width = 16, height = 8, dpi = 300)

#Addressing reviewer comment #####
library(ggh4x)

pie_charts <- ggplot(flux_data_long_pct, aes(x = "", y = PercentCover, fill = Category)) + 
  geom_bar(width = 1, stat = "identity") + 
  coord_polar("y") + 
  facet_nested(group + site ~ defined_season + date, 
               labeller = labeller(
                 date = month_day_labeller,
                 site = function(x) paste("Site", x),
                 defined_season = function(x) tools::toTitleCase(x),
                 group = function(x) tools::toTitleCase(x)
               ),
               nest_line = TRUE) +
  scale_fill_manual(
    values = c("grey98", "lemonchiffon2", 
               "gold1", "grey87", "darkgreen", "darkseagreen2", "forestgreen",
               "turquoise4", "olivedrab",
               "mistyrose3", "aquamarine3", "darkseagreen", "lightblue3",
               "chartreuse3", "yellow2", "khaki2", "palegreen", "papayawhip","palegreen3","darkseagreen3"),
    labels = genus_labels, 
    breaks = sort(setdiff(unique(flux_data_long_pct$Category), exclude_genera))
  ) + 
  labs(title = "Percent Cover of Plant Types by Site") + 
  theme_void() + 
  theme(
    legend.title = element_blank(),
    strip.text.y.left = element_text(angle = 0),
    strip.placement = "outside",
    strip.background = element_blank()
  )

print(pie_charts)

library(ggplot2)
library(dplyr)

flux_summary <- data.frame(
  Season = c("Growing", "Growing", "Senescence", "Senescence", "Dormant", "Dormant"),
  Group = c("Poa", "Mixed", "Poa", "Mixed", "Poa", "Mixed"),
  Median = c(829, 163, -31, 103, 0, 78),
  CI_lower = c(-236, -317, -1468, -314, -222, -30),
  CI_upper = c(2406, 648, 816, 753, 375, 329)
)

# Force correct order
flux_summary <- flux_summary %>%
  mutate(Season = factor(Season, levels = c("Growing", "Senescence", "Dormant")))

# Custom colors
custom_colors <- c("Poa" = "#97b66b", "Mixed" = "#6b97b6")

# Plot
scaled_results_plot= ggplot(flux_summary, aes(x = Season, y = Median, color = Group)) +
  # Grey zero line first (so it’s behind everything else)
  geom_hline(yintercept = 0, color = "gray70", size = 0.8) +
  
  # Then error bars and points
  geom_errorbar(
    aes(ymin = CI_lower, ymax = CI_upper),
    width = 0.15,
    size = .75,
    position = position_dodge(width = 0.5)
  ) +
  geom_point(
    position = position_dodge(width = 0.5),
    size = 4
  ) +
  
  scale_color_manual(values = custom_colors) +
  labs(
    x = "Season",
    y = expression("Seasonal NEE  ("*Mg~C*")"),
    color = "Group"
  ) +
  theme_classic(base_size = 14) +
  theme(
    legend.position = "right",
    axis.text.x = element_text(angle = 30, hjust = 1)
  )

show(scaled_results_plot)

ggsave("scaled_results_plot.png", scaled_results_plot, width = 12, height = 10, dpi = 300)




# Move Legent -------------------------------------------------------------

scaled_results_plot <- ggplot(flux_summary, aes(x = Season, y = Median, color = Group)) +
  geom_hline(yintercept = 0, color = "gray70", size = 0.8) +
  geom_errorbar(
    aes(ymin = CI_lower, ymax = CI_upper),
    width = 0.15,
    size = .75,
    position = position_dodge(width = 0.5)
  ) +
  geom_point(
    position = position_dodge(width = 0.5),
    size = 4
  ) +
  scale_color_manual(values = custom_colors) +
  labs(
    x = "Season",
    y = expression("Seasonal NEE  ("*Mg~C*")"),
    color = "Group"
  ) +
  theme_classic(base_size = 14) +
  theme(
    axis.text.x = element_text(angle = 30, hjust = 1),
    legend.position = c(0.9, 0.1),      # place legend inside bottom right
    legend.background = element_blank(),  # remove legend box
    legend.key = element_blank(),         # remove boxes behind keys
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 11)
  )

show(scaled_results_plot)

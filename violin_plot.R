library(ggplot2)
library(ggpubr)
library(car)     # for leveneTest


# Levene's test
levene_result <- leveneTest(carbon_flux ~ cluster_label, data = df_with_clusters)
levene_p <- signif(levene_result$`Pr(>F)`[1], 3)
levene_label <- paste0("Levene's p = ", levene_p)

# Group letters from Dunn test
cluster_letters <- data.frame(
  cluster_label = c("Mixed", "Poa", "Phedimus"),
  label = c("a", "a", "b"),
  y = max(df_with_clusters$carbon_flux, na.rm = TRUE) * 1.18
)

# Plot
violin_plot <- ggplot(df_with_clusters, aes(x = cluster_label, y = carbon_flux, fill = cluster_label)) +
  geom_violin(trim = FALSE, alpha = 0.8, color = "black") +
  
  # Median point
  stat_summary(fun = median, geom = "point", shape = 23, size = 2.5, fill = "black", color = "black") +
  
  # IQR line
  stat_summary(fun.data = function(x) {
    data.frame(y = median(x),
               ymin = quantile(x, 0.25),
               ymax = quantile(x, 0.75))
  }, geom = "linerange", color = "black", linewidth = 0.7) +
  
  # Horizontal line at 0
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray30") +
  
  # Kruskal-Wallis p-value
  stat_compare_means(method = "kruskal.test", 
                     label.y = max(df_with_clusters$carbon_flux, na.rm = TRUE) * 1.1) +
  
  # Wilcoxon pairwise comparisons (asterisks)
  stat_compare_means(method = "wilcox.test",
                     comparisons = list(c("Poa", "Phedimus"),
                                        c("Poa", "Mixed"),
                                        c("Phedimus", "Mixed")),
                     label = "p.signif") +
  
  
  # Labels, theme, colors
  labs(
    title = "Carbon Flux by Plant Community Cluster",
    x = "Cluster",
    y = bquote(NEE~(mmol~CO[2]~m^{-2}~hr^{-1}))
  ) +
  theme_classic() +
  theme(legend.position = "none") +
  scale_fill_manual(values = c(
    "Poa" = "#97b66b",
    "Mixed" = "#6b97b6",
    "Phedimus" = "#b66b97"
  ))

# Show plot
violin_plot

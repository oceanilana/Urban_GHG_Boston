library(tidyverse)
library(pheatmap)
library(viridis)


# STEP 1: Reshape data to long format
df_long <- df %>%
  pivot_longer(
    cols = ends_with("_pct") & !c("bare_pct", "dead_plant_pct", "grass_pct_dead", 
                                  "leaves_pct_dead", "area_pct_dead", 
                                  "snow_cover_pct", "area_pct_snow", "cerichums_pct", "lepidium_pct", "paniceae_pct"),
    names_to = "species",
    values_to = "percent_cover"
  ) %>%
  mutate(species = str_remove(species, "_pct"))

# Filter out unwanted sites
df_filtered <- df_long %>%
  filter(!site %in% c("ducks_12", "castle_island_11"))

# Calculate average % cover per species per site
site_avg <- df_filtered %>%
  group_by(site, species) %>%
  summarise(avg_cover = mean(percent_cover, na.rm = TRUE), .groups = "drop")

# Pivot to wide format (rows = sites, columns = species)
site_matrix <- site_avg %>%
  pivot_wider(
    names_from = species,
    values_from = avg_cover,
    values_fill = list(avg_cover = 0)
  ) %>%
  column_to_rownames("site") %>%
  as.matrix()

# Capitalize genus names for column labels
colnames(site_matrix) <- colnames(site_matrix) %>%
  gsub("_", " ", .) %>%           
  tools::toTitleCase()            # capitalize first letter

# Capitalize genus names for column labels
colnames(site_matrix) <- colnames(site_matrix) %>%
  gsub("_", " ", .) %>%           
  tools::toTitleCase()            # capitalize first letter

# Order species (columns) by total abundance across all sites
species_order <- colSums(site_matrix, na.rm = TRUE) %>%
  sort(decreasing = TRUE) %>%
  names()
site_matrix_ordered <- site_matrix[, species_order]

# Log-transform for color scale (not for clustering)
site_matrix_log <- log10(site_matrix_ordered + 0.1)

# Cluster based on raw (untransformed) data
site_clust <- hclust(dist(site_matrix_ordered), method = "complete")

# Extract dendrogram clusters
site_groups <- cutree(site_clust, k = 3)

# Create a dataframe with cluster assignments
site_cluster_df <- data.frame(
  site = names(site_groups),
  cluster = as.factor(site_groups)
)

# Map cluster numbers to custom labels
cluster_labels <- c("Mixed", "Poa", "Phedimus")
site_cluster_df <- site_cluster_df %>%
  mutate(cluster_label = cluster_labels[cluster])

# Create a named vector of cluster colors
cluster_colors <- c("Mixed" = "#6b97b6", "Poa" = "#97b66b", "Phedimus" = "#b66b97")

# Prepare row annotation
annotation_row <- site_cluster_df %>%
  select(site, cluster_label) %>%
  as.data.frame()
rownames(annotation_row) <- annotation_row$site
annotation_row$site <- NULL
colnames(annotation_row) <- "Group"  # label for the cluster color bar

# Plot heatmap with cluster color bar and larger font sizes
cluster_plot <- pheatmap(
  site_matrix_log,
  cluster_rows = site_clust,        # keep dendrogram
  cluster_cols = FALSE,
  cutree_rows = 3,
  scale = "none",
  color = viridis(100),
  fontsize = 16,                     # base font size
  fontsize_row = 16,                 # row labels
  fontsize_col = 16,                 # column labels
  main = "Sites Clustered by Plant Composition",
  annotation_row = annotation_row,
  annotation_colors = list(Group = cluster_colors),
  annotation_legend = FALSE,         # remove cluster bar legend
  legend_breaks = c(min(site_matrix_log), max(site_matrix_log)),  
  legend_labels = c("Low", "High")   # custom legend labels
)
print(cluster_plot)

# Save the plot
ggsave("cluster_plot.png", cluster_plot, width = 14, height = 10, dpi = 300)

# Save cluster assignments
write_csv(site_cluster_df, "site_cluster_labels.csv")

# Merge cluster labels into your filtered long dataset
df_with_clusters <- df_filtered %>%
  left_join(site_cluster_df, by = "site")
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
    y = expression("Carbon Flux (" * mu * "mol m"^{-2} * " h"^{-1} * ")")
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



# Load packages
library(tidyverse)
library(tidytext)
library(igraph)
library(ggraph)
library(widyr)
library(stopwords)
library(tidygraph)

# Reshape data: Convert genus columns to long format
flux_long <- flux_data %>%
  select(site, carbon_flux, date, season, ends_with("_pct"), -dead_plant_pct, -bare_pct, -snow_cover_pct) %>% 
  pivot_longer(cols = -site, names_to = "genus", values_to = "cover_pct") %>%
  filter(cover_pct > 0)  %>%  # Remove zero values (no presence)
  mutate(genus = str_remove(genus, "_pct"))  # Clean genus names

# Compute genus co-occurrence at sites
genus_pairs <- flux_long %>%
  pairwise_count(genus, site, sort = TRUE, upper = FALSE) %>%
  filter(n >= 2)  # Keep only frequent co-occurrences

# Compute genus co-occurrence at sites
genus_pairs <- flux_long %>%
  pairwise_count(genus, site, sort = TRUE, upper = FALSE) %>%
  filter(n >= 2)  # Keep only frequent co-occurrences

# Build network graph
genus_graph <- graph_from_data_frame(genus_pairs, directed = FALSE)

# Plot the network
ggraph(genus_graph, layout = "fr") + 
  geom_edge_link(aes(edge_alpha = n), show.legend = FALSE) + 
  geom_node_point(size = 5, color="forestgreen") + 
  geom_node_text(aes(label = name), repel = TRUE, size = 4) + 
  theme_void() + 
  ggtitle("Genus Co-Occurrence Network Across Sites")

# Compute genus co-occurrence at sites, weighted by carbon flux similarity
genus_pairs_flux <- flux_long %>%
  pairwise_cor(genus, site, wt = carbon_flux, sort = TRUE, upper = FALSE) %>%
  filter(correlation > 0.3)  # Keep only meaningful correlations

# Build network graph with weighted edges
genus_graph_flux <- graph_from_data_frame(genus_pairs_flux, directed = FALSE)

# Plot the network, coloring by correlation strength
ggraph(genus_graph_flux, layout = "fr") +
  geom_edge_link(aes(edge_alpha = correlation), show.legend = TRUE) +
  geom_node_point(size = 5, color = "forestgreen") +
  geom_node_text(aes(label = name), repel = TRUE, size = 4) +
  theme_void() +
  ggtitle("Genus Co-Occurrence Network Weighted by Carbon Flux Correlation")

# Convert flux data to wide format (sites as rows, genera as columns)
flux_wide <- flux_long %>%
  pivot_wider(names_from = genus, values_from = cover_pct, values_fill = 0) %>%
  column_to_rownames(var = "site")

# Perform hierarchical clustering
d <- dist(flux_wide, method = "euclidean")  # Distance matrix
hc <- hclust(d, method = "ward.D2")  # Hierarchical clustering

# Plot dendrogram
plot(hc, main = "Site Clustering Based on Plant Genus Composition", cex = 0.8)


# # Load data
# titles_df <- read_csv("titles.csv")  # assumes a column "title"
# 
# # Clean and tokenize
# tokens <- titles_df %>%
#   mutate(id = row_number()) %>%
#   unnest_tokens(word, title) %>%
#   anti_join(get_stopwords(), by = "word") %>%
#   filter(str_length(word) > 3)
# 
# # Create keyword co-occurrence pairs
# word_pairs <- tokens %>%
#   pairwise_count(word, id, sort = TRUE, upper = FALSE) %>%
#   filter(n >= 2)  # Only show connections that occur at least twice
# 
# # Build the graph
# word_graph <- graph_from_data_frame(word_pairs, directed = FALSE)
# 
# # Plot the network
# ggraph(word_graph, layout = "fr") +
#   geom_edge_link(aes(edge_alpha = n), show.legend = FALSE) +
#   geom_node_point(size = 5, color = "steelblue") +
#   geom_node_text(aes(label = name), repel = TRUE, size = 3) +
#   theme_void() +
#   ggtitle("Keyword Co-Occurrence Network from Paper Titles")
# 
# # Create keyword co-occurrence pairs
# word_pairs <- tokens %>%
#   pairwise_count(word, id, sort = TRUE, upper = FALSE) %>%
#   filter(n >= 3)  # Only show connections that occur at least twice
# 
# # Build the graph
# word_graph <- graph_from_data_frame(word_pairs, directed = FALSE)
# 
# # Plot the network
# ggraph(word_graph, layout = "fr") +
#   geom_edge_link(aes(edge_alpha = n), show.legend = FALSE) +
#   geom_node_point(size = 5, color = "steelblue") +
#   geom_node_text(aes(label = name), repel = TRUE, size = 3) +
#   theme_void() +
#   ggtitle("Keyword Co-Occurrence Network from Paper Titles")
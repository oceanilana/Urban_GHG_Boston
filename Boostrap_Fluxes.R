# CO2 Flux Bootstrap Analysis
# Load required libraries
library(dplyr)
library(boot)
library(ggplot2)

# Set parameters
n_bootstrap <- 1000
set.seed(123)  # for reproducibility

# Define daylight hours for each season (calculated in another script)
daylight_hours <- list(
  growing = group1_total,  # value is 1142.15 
  senescence = group2_total, # value is 472.633
  dormant = group3_total     # Value is 1037.383
)

# Define total lawn area in Boston
total_lawn_area <- 248184731  # Calculated area in ArcGIS Pro

# bootstrap and calculate median with CI
bootstrap_median <- function(data, n_boot = 10000) {
  boot_medians <- replicate(n_boot, {
    sample_data <- sample(data, length(data), replace = TRUE)
    median(sample_data, na.rm = TRUE)
  })
  
  return(list(
    median = median(boot_medians),
    ci_lower = quantile(boot_medians, 0.025),
    ci_upper = quantile(boot_medians, 0.975)
  ))
}

# Function to process each season's data
process_season <- function(season_df, season_name, flux_column = "CO2_flux") {
  # Subset data for each group
  poa_data <- season_df %>% 
    filter(group == "Poa") %>% 
    pull(!!flux_column)
  
  mixed_data <- season_df %>% 
    filter(group == "Mixed") %>% 
    pull(!!flux_column)
  
  # Bootstrap for each group
  poa_bootstrap <- bootstrap_median(poa_data, n_bootstrap)
  mixed_bootstrap <- bootstrap_median(mixed_data, n_bootstrap)
  
  # Create results dataframe
  results <- data.frame(
    Season = season_name,
    Group = c("Poa", "Mixed"),
    Median_flux = c(poa_bootstrap$median, mixed_bootstrap$median),
    CI_lower = c(poa_bootstrap$ci_lower, mixed_bootstrap$ci_lower),
    CI_upper = c(poa_bootstrap$ci_upper, mixed_bootstrap$ci_upper),
    stringsAsFactors = FALSE
  )
  
  return(results)
}

# Process each season 
flux_column_name <- "carbon_flux" 

growing_results <- process_season(growing, "Growing", flux_column_name)
senescence_results <- process_season(senescence, "Senescence", flux_column_name)
dormant_results <- process_season(dormant, "Dormant", flux_column_name)

# Combine all bootstrap results
all_bootstrap_results <- rbind(growing_results, senescence_results, dormant_results)

print("Bootstrap Results (mmol m⁻² h⁻¹):")
print(all_bootstrap_results)

# Scale to seasonal totals
scale_to_seasonal_total <- function(bootstrap_results, daylight_hours, total_area) {
  bootstrap_results$Seasonal_total_median <- bootstrap_results$Median_flux * 
    daylight_hours * total_area
  
  bootstrap_results$Seasonal_total_CI_lower <- bootstrap_results$CI_lower * 
    daylight_hours * total_area
  
  bootstrap_results$Seasonal_total_CI_upper <- bootstrap_results$CI_upper * 
    daylight_hours * total_area
  
  return(bootstrap_results)
}

# Apply scaling to each season
scaled_results <- all_bootstrap_results %>%
  mutate(
    Daylight_hours = case_when(
      Season == "Growing" ~ daylight_hours$growing,
      Season == "Senescence" ~ daylight_hours$senescence,
      Season == "Dormant" ~ daylight_hours$dormant
    ),
    Seasonal_total_median = Median_flux * Daylight_hours * total_lawn_area,
    Seasonal_total_CI_lower = CI_lower * Daylight_hours * total_lawn_area,
    Seasonal_total_CI_upper = CI_upper * Daylight_hours * total_lawn_area
  )

print("\nScaled Seasonal Totals (mmol per season):")
print(scaled_results[, c("Season", "Group", "Seasonal_total_median", 
                         "Seasonal_total_CI_lower", "Seasonal_total_CI_upper")])

# Calculate annual totals for each group
annual_totals <- scaled_results %>%
  group_by(Group) %>%
  summarise(
    Annual_total_median = sum(Seasonal_total_median),
    Annual_total_CI_lower = sum(Seasonal_total_CI_lower),
    Annual_total_CI_upper = sum(Seasonal_total_CI_upper),
    .groups = 'drop'
  )

print("\nAnnual Totals (mmol per year):")
print(annual_totals)

# Convert to more readable units if desired (e.g., mol or kg CO2)
# 1 mmol = 0.001 mol
# 1 mol CO2 = 44.01 g CO2
annual_totals_kg <- annual_totals %>%
  mutate(
    Annual_total_median_kg = Annual_total_median * 0.001 * 44.01 / 1000,
    Annual_total_CI_lower_kg = Annual_total_CI_lower * 0.001 * 44.01 / 1000,
    Annual_total_CI_upper_kg = Annual_total_CI_upper * 0.001 * 44.01 / 1000
  )

print("\nAnnual Totals (kg CO2 per year):")
print(annual_totals_kg[, c("Group", "Annual_total_median_kg", 
                           "Annual_total_CI_lower_kg", "Annual_total_CI_upper_kg")])

# Save results
write.csv(scaled_results, "seasonal_co2_flux_results.csv", row.names = FALSE)
write.csv(annual_totals, "annual_co2_flux_totals.csv", row.names = FALSE)


# Plot Bootstrapped -------------------------------------------------------

# Prepare data for plotting - convert to Tg for better readability
plot_data <- scaled_results %>%
  mutate(
    # Convert to Tg CO2
    median_Tg = Seasonal_total_median * 0.001 * 44.01 * 1e-9,
    ci_lower_Tg = Seasonal_total_CI_lower * 0.001 * 44.01 * 1e-9,
    ci_upper_Tg = Seasonal_total_CI_upper * 0.001 * 44.01 * 1e-9,
    # Create custom season order
    custom_season = factor(Season, levels = c("Growing", "Senescence", "Dormant"))
  )

# Define colors for groups
group_colors <- c("Poa" = "#97b66b", "Mixed" = "#6b97b6")  # Adjust colors as needed

# Create the plot
p_bootstrap <- ggplot(plot_data, aes(x = custom_season, color = Group, group = Group)) +
  geom_point(aes(y = median_Tg), size = 3, position = position_dodge(width = 0.3)) +
  geom_errorbar(aes(ymin = ci_lower_Tg, ymax = ci_upper_Tg), width = 0.2, 
                position = position_dodge(width = 0.3), size = 1) +
  scale_color_manual(values = group_colors) +
  labs(title = expression("Seasonal CO"[2]~"Flux: Bootstrap Median ± 95% CI"),
      subtitle = "Points = Bootstrap Median, Error bars = 95% Confidence Intervals",
      x = "Season",
      y = expression("Seasonal CO"[2]~"Flux (Tg CO"[2]*")"),
      color = "Group") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

print(p_bootstrap)

# Alternative version with different units if preferred (kg CO2)
plot_data_kg <- scaled_results %>%
  mutate(
    # Convert to kg CO2 (thousands)
    median_kg_thousands = Seasonal_total_median * 0.001 * 44.01 / 1000,
    ci_lower_kg_thousands = Seasonal_total_CI_lower * 0.001 * 44.01 / 1000,
    ci_upper_kg_thousands = Seasonal_total_CI_upper * 0.001 * 44.01 / 1000,
    custom_season = factor(Season, levels = c("Growing", "Senescence", "Dormant"))
  )



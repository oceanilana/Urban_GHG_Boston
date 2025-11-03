
# Overall Model -----------------------------------------------------------

global.lm.se <- lm(carbon_flux~
                     air_temp*group+
                     dead_plant_pct*group+
                     days_since_precip*group+
                     num_species*group + 
                     light_intensity*group, 
                   data=scaled.df, weights=1/SE_CO2_flux^2)

#model diagnostics
plot(global.lm.se)
qqnorm(resid(global.lm.se))
qqline(resid(global.lm.se))

#model selection
options(na.action = "na.fail")
model_select <- dredge(global.lm.se)

# Extract all models from dredge
all_models <- get.models(model_select, subset = TRUE)

# Load required packages
library(MuMIn)
library(performance)

# Function to extract formula as a string
get_formula_string <- function(mod) {
  paste("carbon_flux ~", paste(attr(terms(mod), "term.labels"), collapse = " + "))
}

# Create the results dataframe with formulas and stats
all_model_stats_lmer <- lapply(seq_along(all_models), function(i) {
  mod <- all_models[[i]]
  r2_vals <- performance::r2(mod)
  
  data.frame(
    Model = i,
    Formula = get_formula_string(mod),
    AIC = AIC(mod),
    logLik = as.numeric(logLik(mod)),  # Convert to numeric
    R2_marginal = r2_vals$R2_marginal,
    R2_conditional = r2_vals$R2_conditional,
    Sigma = sigma(mod)
  )
}) %>%
  bind_rows() %>%
  arrange(AIC)

# Save top 5 models as a new dataframe
top5_models_df <- all_model_stats_lmer %>%
  arrange(AIC) %>%
  slice_head(n = 5) %>%
  select(Formula, AIC, R2_marginal, R2_conditional)

# View the results
print(top5_models_df)

# Save to CSV files
write.csv(top5_models_df, "top5_model_selection_lmer.csv", row.names = FALSE)
write.csv(all_model_stats_lmer, "model_selection_results_lmer_with_formulas.csv", row.names = FALSE)

# Debug -------------------------------------------------------------------

# First, let's check what we're working with
length(all_models)
class(all_models[[1]])

# Test the formula function on the first model
get_formula_string(all_models[[1]])

# Test R2 on the first model
performance::r2(all_models[[1]])

# Let's create a safer version with error handling:
all_model_stats_lmer <- lapply(seq_along(all_models), function(i) {
  mod <- all_models[[i]]
  
  #  get R2 values
  tryCatch({
    r2_vals <- performance::r2(mod)
    
    # Handle cases where R2 might be NULL or missing components
    r2_marginal <- if(is.null(r2_vals$R2_marginal)) NA else r2_vals$R2_marginal
    r2_conditional <- if(is.null(r2_vals$R2_conditional)) NA else r2_vals$R2_conditional
    
    # Get formula string
    formula_str <- get_formula_string(mod)
    if(length(formula_str) == 0) formula_str <- "No formula"
    
    data.frame(
      Model = i,
      Formula = formula_str,
      AIC = AIC(mod),
      logLik = as.numeric(logLik(mod)),
      R2_marginal = r2_marginal,
      R2_conditional = r2_conditional,
      Sigma = sigma(mod)
    )
  }, error = function(e) {
    # If there's an error, return a row with NAs
    data.frame(
      Model = i,
      Formula = paste("Error in model", i),
      AIC = AIC(mod),
      logLik = as.numeric(logLik(mod)),
      R2_marginal = NA,
      R2_conditional = NA,
      Sigma = sigma(mod)
    )
  })
}) %>%
  bind_rows() %>%
  arrange(AIC)

# Check the result
head(all_model_stats_lmer)

# Corrected version for lm models
all_model_stats_lmer <- lapply(seq_along(all_models), function(i) {
  mod <- all_models[[i]]
  
  # Safely get R2 values
  tryCatch({
    r2_vals <- performance::r2(mod)
    
    # For lm models, use R2 and adj.R2 instead of marginal/conditional
    r2_value <- if(is.null(r2_vals$R2)) NA else r2_vals$R2
    r2_adj <- if(is.null(r2_vals$R2_adjusted)) NA else r2_vals$R2_adjusted
    
    # Get formula string
    formula_str <- get_formula_string(mod)
    if(length(formula_str) == 0) formula_str <- "No formula"
    
    data.frame(
      Model = i,
      Formula = formula_str,
      AIC = AIC(mod),
      logLik = as.numeric(logLik(mod)),
      R2 = r2_value,
      R2_adjusted = r2_adj,
      Sigma = sigma(mod)
    )
  }, error = function(e) {
    # If there's an error, return a row with NAs
    data.frame(
      Model = i,
      Formula = paste("Error in model", i),
      AIC = AIC(mod),
      logLik = as.numeric(logLik(mod)),
      R2 = NA,
      R2_adjusted = NA,
      Sigma = sigma(mod)
    )
  })
}) %>%
  bind_rows() %>%
  arrange(AIC)

# Save top 5 models as a new dataframe
top5_models_df <- all_model_stats_lmer %>%
  arrange(AIC) %>%
  slice_head(n = 5) %>%
  select(Formula, AIC, R2, R2_adjusted)

# View the results
print(top5_models_df)

# Save to CSV files
write.csv(top5_models_df, "top5_model_selection_lm.csv", row.names = FALSE)
write.csv(all_model_stats_lmer, "model_selection_results_lm_with_formulas.csv", row.names = FALSE)

# Best Model --------------------------------------------------------------
# Get the best model from your selection
best_model <- all_models[[which.min(sapply(all_models, AIC))]]

# Get coefficients
coefs <- coef(best_model)
print(coefs)

# Get predicted effects for each group
library(emmeans)
# Example for air temperature effect by group
emmeans(best_model, ~ air_temp | group, at = list(air_temp = c(10, 20, 30)))

# Check effects for other key variables
# Dead plant percentage by group
emmeans(best_model, ~ dead_plant_pct | group, at = list(dead_plant_pct = c(0, 0.5, 1)))

# Days since precipitation by group  
emmeans(best_model, ~ days_since_precip | group, at = list(days_since_precip = c(0, 10, 20)))

# Light intensity by group
emmeans(best_model, ~ light_intensity | group, at = list(light_intensity = c(0, 0.5, 1)))

# Create interaction plots
library(ggplot2)

# Air temperature effects by group
ggplot(scaled.df, aes(x = air_temp, y = carbon_flux, color = group)) +
  scale_color_manual(values = c(
    "Poa" = "#97b66b",
    "Mixed" = "#6b97b6",
    "Phedimus" = "#b66b97"
  ))+
  scale_fill_manual(values = c(
    "Poa" = "#97b66b",
    "Mixed" = "#6b97b6",
    "Phedimus" = "#b66b97"
  ))+
  geom_point(alpha = 0.6) +
  geom_smooth(method = "lm", se = TRUE, aes(fill=group)) +
  labs(title = "Air Temperature Effects by Plant Community",
       x = "Air Temperature (scaled)", 
       y = "Carbon Flux") +
  theme_classic()

ggplot(scaled.df, aes(x = days_since_precip, y = carbon_flux, color = group)) +
  scale_color_manual(values = c(
    "Poa" = "#97b66b",
    "Mixed" = "#6b97b6",
    "Phedimus" = "#b66b97"
  ))+
  scale_fill_manual(values = c(
    "Poa" = "#97b66b",
    "Mixed" = "#6b97b6",
    "Phedimus" = "#b66b97"
  ))+
  geom_point(alpha = 0.6) +
  geom_smooth(method = "lm", se = TRUE, aes(fill=group)) +
  labs(title = "Days Since Precipitation Effects by Plant Community",
       x = "Days Since Precipitation (scaled)", 
       y = "Carbon Flux") +
  theme_classic()

ggplot(scaled.df, aes(x = dead_plant_pct, y = carbon_flux, color = group)) +
  scale_color_manual(values = c(
    "Poa" = "#97b66b",
    "Mixed" = "#6b97b6",
    "Phedimus" = "#b66b97"
  ))+
  scale_fill_manual(values = c(
    "Poa" = "#97b66b",
    "Mixed" = "#6b97b6",
    "Phedimus" = "#b66b97"
  ))+
  geom_point(alpha = 0.6) +
  geom_smooth(method = "lm", se = TRUE, aes(fill=group)) +
  labs(title = "Dead Plant Percent Effects by Plant Community",
       x = "Dead Plant Percent (scaled)", 
       y = "Carbon Flux") +
  theme_classic()

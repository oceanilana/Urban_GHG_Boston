
# Model Selection for each Group ------------------------------------------

# Packages 
library(tidyverse) #for everything
library(lme4) #for LMMs
library(lmerTest) #diagnoistics
library(performance) #diagnostics
library(MuMIn) #for model selection


# Scale Mixed Fluxes ------------------------------------------------------

scaled.df_mixed <- Mixed %>%
  select(carbon_flux, group, site, light_intensity, dead_plant_pct, 
         days_since_precip, air_temp, num_species) %>%
  mutate(across(c(group, site), as.factor)) %>%
  mutate_if(is.numeric, scale) %>%
  drop_na()


scaled.df_mixed <- Mixed %>%
  select( group, site, light_intensity, dead_plant_pct, 
          days_since_precip, air_temp, num_species, SE_CO2_flux) %>%
  mutate(across(c(group, site), as.factor)) %>%
  mutate_if(is.numeric, scale)

scaled.df_mixed$carbon_flux=Mixed$carbon_flux

scaled.df_mixed <- na.omit(scaled.df_mixed)

# Scale Phedimus Fluxes ---------------------------------------------------

scaled.df_phed <- Phedimus %>%
  select(carbon_flux, group, site, light_intensity, dead_plant_pct, 
         days_since_precip, air_temp, num_species) %>%
  mutate(across(c(group, site), as.factor)) %>%
  mutate_if(is.numeric, scale) %>%
  drop_na()


scaled.df_phed <- Phedimus %>%
  select( group, site, light_intensity, dead_plant_pct, 
          days_since_precip, air_temp, num_species, SE_CO2_flux) %>%
  mutate(across(c(group, site), as.factor)) %>%
  mutate_if(is.numeric, scale)

scaled.df_phed$carbon_flux=Phedimus$carbon_flux

scaled.df_phed <- na.omit(scaled.df_phed)

# Scale Poa Fluxes --------------------------------------------------------

scaled.df_poa <- Poa %>%
  select(carbon_flux, group, site, light_intensity, dead_plant_pct, 
         days_since_precip, air_temp, num_species) %>%
  mutate(across(c(group, site), as.factor)) %>%
  mutate_if(is.numeric, scale) %>%
  drop_na()


scaled.df_poa <- Poa %>%
  select( group, site, light_intensity, dead_plant_pct, 
          days_since_precip, air_temp, num_species, SE_CO2_flux) %>%
  mutate(across(c(group, site), as.factor)) %>%
  mutate_if(is.numeric, scale)

scaled.df_poa$carbon_flux=Poa$carbon_flux

scaled.df_poa <- na.omit(scaled.df_poa)

# Model Mixed -------------------------------------------------------------

global.lm.mixed <- lm(carbon_flux~
                       air_temp+
                       dead_plant_pct+
                       days_since_precip+
                       num_species + 
                       light_intensity, 
                     data=scaled.df_mixed, weights=1/SE_CO2_flux^2)

#model diagnostics
plot(global.lm.mixed)
qqnorm(resid(global.lm.mixed))
qqline(resid(global.lm.mixed))

#model selection
options(na.action = "na.fail")
model_select <- dredge(global.lm.mixed)

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
write.csv(top5_models_df, "top5_model_selection_mixed.csv", row.names = FALSE)
write.csv(all_model_stats_lmer, "model_selection_results_lmer_with_formulas_mixed.csv", row.names = FALSE)

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
write.csv(top5_models_df, "top5_model_selection_lm_mixed.csv", row.names = FALSE)
write.csv(all_model_stats_lmer, "model_selection_results_lm_with_formulas_mixed.csv", row.names = FALSE)

# Best Model Mixed --------------------------------------------------------------
# Get the best model from your selection
best_model_mixed <- all_models[[which.min(sapply(all_models, AIC))]]

# Get coefficients
coefs <- coef(best_model_mixed)
print(coefs)

# # Get predicted effects for each group
# library(emmeans)
# # Example for air temperature effect by group
# emmeans(best_model_mixed, ~ air_temp | group, at = list(air_temp = c(10, 20, 30)))
# 
# # Check effects for other key variables
# # Dead plant percentage by group
# emmeans(best_model_mixed, ~ dead_plant_pct | group, at = list(dead_plant_pct = c(0, 0.5, 1)))
# 
# # Days since precipitation by group
# emmeans(best_model_mixed, ~ days_since_precip | group, at = list(days_since_precip = c(0, 10, 20)))
# 
# # Light intensity by group
# emmeans(best_model_mixed, ~ light_intensity | group, at = list(light_intensity = c(0, 0.5, 1)))

# # Create interaction plots
# library(ggplot2)
# 
# # Air temperature effects by group
# ggplot(scaled.df_grow, aes(x = air_temp, y = carbon_flux, color = group)) +
#   scale_color_manual(values = c(
#     "Poa" = "#97b66b",
#     "Mixed" = "#6b97b6",
#     "Phedimus" = "#b66b97"
#   ))+
#   geom_point(alpha = 0.6) +
#   geom_smooth(method = "lm", se = TRUE) +
#   labs(title = "Air Temperature Effects by Plant Community",
#        x = "Air Temperature (scaled)", 
#        y = "Carbon Flux") +
#   theme_classic()
# 

# Model Phedimus ----------------------------------------------------------

global.lm.phed <- lm(carbon_flux~
                        air_temp+
                        dead_plant_pct+
                        days_since_precip+
                        num_species + 
                        light_intensity, 
                      data=scaled.df_phed, weights=1/SE_CO2_flux^2)

#model diagnostics
plot(global.lm.phed)
qqnorm(resid(global.lm.phed))
qqline(resid(global.lm.phed))

#model selection
options(na.action = "na.fail")
model_select <- dredge(global.lm.phed)

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
write.csv(top5_models_df, "top5_model_selection_phed.csv", row.names = FALSE)
write.csv(all_model_stats_lmer, "model_selection_results_lmer_with_formulas_phed.csv", row.names = FALSE)

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
write.csv(top5_models_df, "top5_model_selection_lm_phed.csv", row.names = FALSE)
write.csv(all_model_stats_lmer, "model_selection_results_lm_with_formulas_phed.csv", row.names = FALSE)

# Best Model Phedimus --------------------------------------------------------------
# Get the best model from your selection
best_model_phed <- all_models[[which.min(sapply(all_models, AIC))]]

# Get coefficients
coefs <- coef(best_model_phed)
print(coefs)

# # Get predicted effects for each group
# library(emmeans)
# # Example for air temperature effect by group
# emmeans(best_model_mixed, ~ air_temp | group, at = list(air_temp = c(10, 20, 30)))
# 
# # Check effects for other key variables
# # Dead plant percentage by group
# emmeans(best_model_mixed, ~ dead_plant_pct | group, at = list(dead_plant_pct = c(0, 0.5, 1)))
# 
# # Days since precipitation by group
# emmeans(best_model_mixed, ~ days_since_precip | group, at = list(days_since_precip = c(0, 10, 20)))
# 
# # Light intensity by group
# emmeans(best_model_mixed, ~ light_intensity | group, at = list(light_intensity = c(0, 0.5, 1)))

# # Create interaction plots
# library(ggplot2)
# 
# # Air temperature effects by group
# ggplot(scaled.df_grow, aes(x = air_temp, y = carbon_flux, color = group)) +
#   scale_color_manual(values = c(
#     "Poa" = "#97b66b",
#     "Mixed" = "#6b97b6",
#     "Phedimus" = "#b66b97"
#   ))+
#   geom_point(alpha = 0.6) +
#   geom_smooth(method = "lm", se = TRUE) +
#   labs(title = "Air Temperature Effects by Plant Community",
#        x = "Air Temperature (scaled)", 
#        y = "Carbon Flux") +
#   theme_classic()
# 

# Model Poa ---------------------------------------------------------------

global.lm.poa <- lm(carbon_flux~
                       air_temp+
                       dead_plant_pct+
                       days_since_precip+
                       num_species + 
                       light_intensity, 
                     data=scaled.df_poa, weights=1/SE_CO2_flux^2)

#model diagnostics
plot(global.lm.poa)
qqnorm(resid(global.lm.poa))
qqline(resid(global.lm.phed))

#model selection
options(na.action = "na.fail")
model_select <- dredge(global.lm.poa)

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
write.csv(top5_models_df, "top5_model_selection_poa.csv", row.names = FALSE)
write.csv(all_model_stats_lmer, "model_selection_results_lmer_with_formulas_poa.csv", row.names = FALSE)

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
write.csv(top5_models_df, "top5_model_selection_lm_poa.csv", row.names = FALSE)
write.csv(all_model_stats_lmer, "model_selection_results_lm_with_formulas_poa.csv", row.names = FALSE)

# Best Model Phedimus --------------------------------------------------------------
# Get the best model from your selection
best_model_poa <- all_models[[which.min(sapply(all_models, AIC))]]

# Get coefficients
coefs <- coef(best_model_poa)
print(coefs)

# # Get predicted effects for each group
# library(emmeans)
# # Example for air temperature effect by group
# emmeans(best_model_mixed, ~ air_temp | group, at = list(air_temp = c(10, 20, 30)))
# 
# # Check effects for other key variables
# # Dead plant percentage by group
# emmeans(best_model_mixed, ~ dead_plant_pct | group, at = list(dead_plant_pct = c(0, 0.5, 1)))
# 
# # Days since precipitation by group
# emmeans(best_model_mixed, ~ days_since_precip | group, at = list(days_since_precip = c(0, 10, 20)))
# 
# # Light intensity by group
# emmeans(best_model_mixed, ~ light_intensity | group, at = list(light_intensity = c(0, 0.5, 1)))

# # Create interaction plots
# library(ggplot2)
# 
# # Air temperature effects by group
# ggplot(scaled.df_grow, aes(x = air_temp, y = carbon_flux, color = group)) +
#   scale_color_manual(values = c(
#     "Poa" = "#97b66b",
#     "Mixed" = "#6b97b6",
#     "Phedimus" = "#b66b97"
#   ))+
#   geom_point(alpha = 0.6) +
#   geom_smooth(method = "lm", se = TRUE) +
#   labs(title = "Air Temperature Effects by Plant Community",
#        x = "Air Temperature (scaled)", 
#        y = "Carbon Flux") +
#   theme_classic()
# 

model_names <- ls(pattern = "^best_model")

model_summary <- data.frame(
  model_name = character(),
  formula = character(),
  AIC = numeric(),
  r_squared = numeric(),
  adj_r_squared = numeric(),
  stringsAsFactors = FALSE
)

for(name in model_names) {
  model <- get(name)
  model_summary <- rbind(model_summary, data.frame(
    model_name = name,
    formula = paste(deparse(formula(model)), collapse = ""),
    AIC = AIC(model),
    r_squared = summary(model)$r.squared,
    adj_r_squared = summary(model)$adj.r.squared
  ))
}

write.csv(model_summary, "best_models_summary.csv", row.names = FALSE)

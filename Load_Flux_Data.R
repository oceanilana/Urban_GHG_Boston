##
# Author: Ilana Jacobs 
# Read in the Data Master sheet-------------------------------------------------
flux_data = read.csv("campus_co2_flux_master_SE.csv", na.strings = c("ND", " ND", "ND ", "NA"))

str(flux_data) # look at the structure of the file

flux_data$date = as.POSIXct(flux_data$date, format = "%m/%d/%y")

## Remove "exclude" sites and Supplemental Sites
flux_data = subset(flux_data, site != "exclude")
flux_data = subset(flux_data, site != "castle_island_11")
flux_data = subset(flux_data, site != "ducks_12")
flux_data = subset(flux_data, site != "kenmore_2_1")

# Add new group names
flux_data = flux_data %>%
  mutate(group = case_when(
    site == "tiny_1"    ~ "Mixed",
    site == "kenmore_2" ~ "Mixed",
    site == "muddy_3"   ~ "Mixed",
    site == "charles_4" ~ "Mixed",
    site == "castle_5"  ~ "Poa",
    site == "beach_6"   ~ "Poa",
    site == "road_7"    ~ "Mixed",
    site == "lot_8"     ~ "Poa",
    site == "train_9"   ~ "Poa",
    site == "cds_10"    ~ "Phedimus",
    # site == "castle_island_11"     ~ "Poa",
    # site == "Ducks_12"   ~ "Poa",
    TRUE ~ site  # Keeps other site names unchanged
  ))

# Add numeric site_final column
flux_data = flux_data %>%
  mutate(site_final = case_when(
    site == "tiny_1"    ~ "8",
    site == "kenmore_2" ~ "9",
    site == "muddy_3"   ~ "10",
    site == "charles_4" ~ "7",
    site == "castle_5"  ~ "5",
    site == "beach_6"   ~ "4",
    site == "road_7"    ~ "1",
    site == "lot_8"     ~ "2",
    site == "train_9"   ~ "3",
    site == "cds_10"    ~ "6",
    TRUE ~ site  # Keeps other site names unchanged
  ))

# Make site_final a factor with levels 1–10
flux_data = flux_data %>%
  mutate(site_final = factor(site_final, levels = as.character(1:10)))

# Convert carbon flux to mmol (from umol)
flux_data$carbon_flux_umol = flux_data$carbon_flux  # save original µmol values
flux_data$carbon_flux = flux_data$carbon_flux / 1000  # convert to mmol
flux_data$SE_CO2_flux_umol=flux_data$SE_CO2_umol # save original output as slope values
flux_data$SE_CO2_flux=flux_data$SE_CO2_flux_umol/1000 # convert to mmol

# Rename columns: site_final -> site, site -> site_initial
flux_data = flux_data %>%
  rename(
    site_initial = site,
    site = site_final
  )
flux_data$air_temp_f=flux_data$air_temp
flux_data$air_temp=(flux_data$air_temp - 32) * 5 / 9

# Check the updated dataframe
head(flux_data)


# Subset by Group ---------------------------------------------------------

list2env(split(flux_data, flux_data$group), envir = .GlobalEnv)

# Add defined_season column -----------------------------------------------
flux_data = flux_data %>%
  mutate(defined_season = case_when(
    date >= as.POSIXct("2024-07-09") & date <= as.POSIXct("2024-09-24 23:59:59") ~ "growing",
    date >= as.POSIXct("2024-10-01") & date <= as.POSIXct("2024-11-05 23:59:59") ~ "senescence",
    date >= as.POSIXct("2024-11-14") & date <= as.POSIXct("2025-02-28 23:59:59") ~ "dormant",
    TRUE ~ "other"  # For any dates that don't fall into the defined seasons
  ))
head(flux_data)

# subset by season --------------------------------------------------------

# Create the seasonal dataframes using POSIXt format
growing <- flux_data[flux_data$date >= as.POSIXct("2024-07-09") & 
                       flux_data$date <= as.POSIXct("2024-09-24 23:59:59"), ]
senescence <- flux_data[flux_data$date >= as.POSIXct("2024-10-01") & 
                          flux_data$date <= as.POSIXct("2024-11-05 23:59:59"), ]
dormant <- flux_data[flux_data$date >= as.POSIXct("2024-11-14") & 
                       flux_data$date <= as.POSIXct("2025-02-28 23:59:59"), ]



# Load in Light Data ------------------------------------------------------
# Downloaded from Navy Database for Boston
light_24=read.csv("/Users/ilanajacobs/Thesis/DaylightHours_2024.csv", skip = 4)
light_25=read.csv("/Users/ilanajacobs/Thesis/DaylightHours2025.csv", skip = 4)


# Calculate Light Hours for Seasons ---------------------------------------

# Function to convert time string (HH:MM) to decimal hours
time_to_hours <- function(time_str) {
  if (is.na(time_str) || time_str == "" || is.null(time_str)) return(0)
  parts <- strsplit(as.character(time_str), ":")
  hours <- as.numeric(sapply(parts, function(x) x[1]))
  minutes <- as.numeric(sapply(parts, function(x) x[2]))
  return(hours + minutes/60)
}

# Extract the specific periods we need and convert to decimal hours

# July: days 9-31 (rows where Day column is 9-31)
july_subset <- light_24[light_24$Day >= 9 & light_24$Day <= 31, "July"]
july_hours <- sapply(july_subset, time_to_hours)
july_total <- sum(july_hours, na.rm = TRUE)

# August: all days (1-31)
august_hours <- sapply(light_24$Aug., time_to_hours)
august_total <- sum(august_hours, na.rm = TRUE)

# September: all days (1-30)
sep_subset <- light_24[light_24$Day <= 30, "Sep."]
sep_hours <- sapply(sep_subset, time_to_hours)
september_total <- sum(sep_hours, na.rm = TRUE)

# October: all days (1-31) 
october_hours <- sapply(light_24$Oct., time_to_hours)
october_total <- sum(october_hours, na.rm = TRUE)

# November: all days (1-30)
nov_subset <- light_24[light_24$Day <= 30, "Nov."]
nov_hours <- sapply(nov_subset, time_to_hours)
november_total <- sum(nov_hours, na.rm = TRUE)

# December: all days (1-31)
december_hours <- sapply(light_24$Dec., time_to_hours)
december_total <- sum(december_hours, na.rm = TRUE)

# January All Days
january_hours <- sapply(light_25$Jan., time_to_hours)
january_total <- sum(january_hours, na.rm = TRUE)

#February all days
february_hours <- sapply(light_25$Feb., time_to_hours)
february_total <- sum(february_hours, na.rm = TRUE)


# Calculate grand total
total_daylight_hours <- july_total + august_total + september_total + 
  october_total + november_total + december_total + january_total +february_total

# Display results
cat("July 9-31:", round(july_total, 2), "hours (", length(july_hours), "days)\n")
cat("August:", round(august_total, 2), "hours (31 days)\n") 
cat("September:", round(september_total, 2), "hours (30 days)\n")
cat("October:", round(october_total, 2), "hours (31 days)\n")
cat("November:", round(november_total, 2), "hours (30 days)\n")
cat("December:", round(december_total, 2), "hours (31 days)\n")
cat("January:", round(january_total, 2), "hours (31 days)\n")
cat("February:", round(february_total, 2), "hours (28 days)\n")
cat("\nTotal daylight hours:", round(total_daylight_hours, 2), "hours\n")
cat("Total daylight days equivalent:", round(total_daylight_hours/24, 2), "days\n")
cat("Number of calendar days:", 23 + 31 + 30 + 31 + 30 + 31+31+28, "days\n")


# Seasonal Groups ---------------------------------------------------------

# Calculate daylight hours for three specific time periods
# Using your data frame named 'light_24'

# Function to convert time string (HH:MM) to decimal hours
time_to_hours <- function(time_str) {
  if (is.na(time_str) || time_str == "" || is.null(time_str)) return(0)
  parts <- strsplit(as.character(time_str), ":")
  hours <- as.numeric(sapply(parts, function(x) x[1]))
  minutes <- as.numeric(sapply(parts, function(x) x[2]))
  return(hours + minutes/60)
}

# GROUP 1: July 9, 2024 - September 30, 2024
cat("GROUP 1: July 9, 2024 - September 30, 2024\n")
cat(paste(rep("=", 50), collapse=""), "\n")

# July 9-31 (23 days)
july_subset1 <- light_24[light_24$Day >= 9 & light_24$Day <= 31, "July"]
july_hours1 <- sapply(july_subset1, time_to_hours)
july_total1 <- sum(july_hours1, na.rm = TRUE)

# August 1-31 (31 days)
august_hours1 <- sapply(light_24$Aug., time_to_hours)
august_total1 <- sum(august_hours1, na.rm = TRUE)

# September 1-30 (30 days)
sep_subset1 <- light_24[light_24$Day <= 30, "Sep."]
sep_hours1 <- sapply(sep_subset1, time_to_hours)
sep_total1 <- sum(sep_hours1, na.rm = TRUE)

# Group 1 total
group1_total <- july_total1 + august_total1 + sep_total1
group1_days <- 23 + 31 + 30  # 84 days

cat("July 9-31:", round(july_total1, 2), "hours (23 days)\n")
cat("August:", round(august_total1, 2), "hours (31 days)\n")
cat("September:", round(sep_total1, 2), "hours (30 days)\n")
cat("GROUP 1 TOTAL:", round(group1_total, 2), "hours (", group1_days, "days)\n\n")

# GROUP 2: October 1, 2024 - November 13, 2024
cat("GROUP 2: October 1, 2024 - November 13, 2024\n")
cat(paste(rep("=", 50), collapse=""), "\n")

# October 1-31 (31 days)
october_hours2 <- sapply(light_24$Oct., time_to_hours)
october_total2 <- sum(october_hours2, na.rm = TRUE)

# November 1-13 (13 days)
nov_subset2 <- light_24[light_24$Day >= 1 & light_24$Day <= 13, "Nov."]
nov_hours2 <- sapply(nov_subset2, time_to_hours)
nov_total2 <- sum(nov_hours2, na.rm = TRUE)

# Group 2 total
group2_total <- october_total2 + nov_total2
group2_days <- 31 + 13  # 44 days

cat("October:", round(october_total2, 2), "hours (31 days)\n")
cat("November 1-13:", round(nov_total2, 2), "hours (13 days)\n")
cat("GROUP 2 TOTAL:", round(group2_total, 2), "hours (", group2_days, "days)\n\n")

# GROUP 3: November 14, 2024 - December 31, 2024
# (Note: Your data only goes through Dec 2024, not Feb 2025)
cat("GROUP 3: November 14, 2024 - December 31, 2024\n")
cat("(Note: Data only available through December 2024)\n")
cat(paste(rep("=", 50), collapse=""), "\n")

# November 14-30 (17 days)
nov_subset3 <- light_24[light_24$Day >= 14 & light_24$Day <= 30, "Nov."]
nov_hours3 <- sapply(nov_subset3, time_to_hours)
nov_total3 <- sum(nov_hours3, na.rm = TRUE)

# December 1-31 (31 days)
december_hours3 <- sapply(light_24$Dec., time_to_hours)
december_total3 <- sum(december_hours3, na.rm = TRUE)

# Januray 

january_hours3 <- sapply(light_25$Jan., time_to_hours)
january_total3 <- sum(january_hours3, na.rm = TRUE)

# February 

february_hours3 <- sapply(light_25$Feb., time_to_hours)
february_total3 <- sum(february_hours3, na.rm = TRUE)

# Group 3 total
group3_total <- nov_total3 + december_total3 + january_total3 +february_total3
group3_days <- 17 + 31 + 31 + 28# 48 days

cat("November 14-30:", round(nov_total3, 2), "hours (17 days)\n")
cat("December:", round(december_total3, 2), "hours (31 days)\n")
cat("GROUP 3 TOTAL:", round(group3_total, 2), "hours (", group3_days, "days)\n\n")

# SUMMARY
cat("SUMMARY OF ALL THREE GROUPS\n")
cat(paste(rep("=", 50), collapse=""), "\n")
cat("Group 1 (Jul 9 - Sep 30):", round(group1_total, 2), "hours (", group1_days, "days)\n")
cat("Group 2 (Oct 1 - Nov 13):", round(group2_total, 2), "hours (", group2_days, "days)\n")
cat("Group 3 (Nov 14 - Dec 31):", round(group3_total, 2), "hours (", group3_days, "days)\n")
cat("\nTotal across all groups:", round(group1_total + group2_total + group3_total, 2), "hours\n")
cat("Total days across all groups:", group1_days + group2_days + group3_days, "days\n")

# Average daylight per day for each group
cat("\nAverage daylight hours per day:\n")
cat("Group 1:", round(group1_total / group1_days, 2), "hours/day\n")
cat("Group 2:", round(group2_total / group2_days, 2), "hours/day\n")
cat("Group 3:", round(group3_total / group3_days, 2), "hours/day\n")

# Create a summary data frame for easy reference
summary_df <- data.frame(
  Group = c("Growing", "Senescence", "Dormant"),
  Period = c("Jul 9 - Sep 30", "Oct 1 - Nov 13", "Nov 14 - Dec 31"),
  Days = c(group1_days, group2_days, group3_days),
  Total_Hours = c(round(group1_total, 2), round(group2_total, 2), round(group3_total, 2)),
  Avg_Hours_Per_Day = c(round(group1_total/group1_days, 2), 
                        round(group2_total/group2_days, 2), 
                        round(group3_total/group3_days, 2))
)

print(summary_df)


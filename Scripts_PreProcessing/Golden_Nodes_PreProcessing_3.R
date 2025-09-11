# Pre-processing the golden nodes
# Made by Parivrudh Sharma 

###########################
rm(list=ls())
library(dplyr)
library(sf)  
library(ggplot2)
library(lubridate)
library(purrr)
library(caret)
library(Metrics)
library(gridExtra)
library(lmtest)
library(broom)
library(ggforce)
library(tidyr)
###########################

setwd("/Users/parivrudhsharma/Desktop/Desktop/Imperial College London/MSc Health Data Analytics and Machine Learning/Modules/Research Project/Data")

colocation_md <- read.csv("Data/Collocation_md_PreProcessed.csv")
deployment_md <- read.csv("Data/DeploymentSitesMetadata_PreProcessed.csv")
full_dataset <- read.csv("Data/TMPRHUM_alldatatoMay25.csv")
reference_data <- read.csv("Data/5yrs_T_HOP_MR9.csv")
calibration_models <- read.csv("Data/calibration_model_dataset.csv")
combined_sites_filtered <- read.csv("Data/LOOKUPTABLE_FINAL.csv")

View(colocation_md)
View(deployment_md)
View(full_dataset)
View(reference_data)
View(calibration_models)
View(combined_sites_filtered)

# Keep only CLCA and CLDP nodes from full_dataset
clca_data_only <- full_dataset %>%
  filter(grepl("^CLCA", SiteCode)) %>%
  mutate(DateTime = as.POSIXct(DateTime, tz = "UTC")) %>%
  arrange(SiteCode, DateTime) 

cldp_data_only <- full_dataset %>%
  filter(grepl("^CLDP", SiteCode)) %>%
  mutate(DateTime = as.POSIXct(DateTime, tz = "UTC")) %>%
  arrange(SiteCode, DateTime) 

calibration_models <- calibration_models %>%
  arrange(SiteCode, DateTime)

# for some reason when I upload calibration_models - the 00:00:00 get removed but nothing else is changed
# Convert DateTime column to character for string manipulation
calibration_models$DateTime <- as.character(calibration_models$DateTime)
# Append "00:00:00" where time is missing (i.e., just a date is printed)
calibration_models <- calibration_models %>%
  mutate(DateTime = ifelse(nchar(DateTime) == 10,
                           paste0(DateTime, " 00:00:00"),
                           DateTime))
# Convert DateTime column to proper POSIXct object
calibration_models$DateTime <- as.POSIXct(calibration_models$DateTime, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")

View(calibration_models)

########################### 

# Ok, as per BB we need to build weekly and monthly linear regression models for our 2 golden nodes, 
# to see how stable the coefficients are. 

# First we need to build a dataset to isolate these golden nodes - 
# the golden nodes are CLDP0029 and CLDP0030, what are the corresponding device codes? 

# Filter for just the golden nodes
golden_node_links <- combined_sites_filtered %>%
  filter(SiteCode_CLDP %in% c("CLDP0029", "CLDP0030")) %>%
  select(SiteCode_CLDP, DeviceCode) %>%
  distinct() %>%
  arrange(SiteCode_CLDP, DeviceCode)
View(golden_node_links) # AFZHPFH3 and ARNKZ5XY

# CLDP0029/CLCA0014/AFZHPFH3 and CLDP0030/CLCA0072/ARNKZ5XY are the golden nodes.

# Just as a check I want to see their calibration protocol 
View(colocation_md) # They both actually have a lot of missing data for its calibration...

# Ok so now lets focus on isolating the relevant CLDP codes from the full_dataset first

# Filter full_dataset for only the golden nodes
golden_nodes_only <- full_dataset %>%
  filter(SiteCode %in% c("CLDP0029", "CLDP0030"))
# View the resulting filtered data
View(golden_nodes_only)

# Ok now I need to add the reference temp next to it (I need to re-preprocess this as I didn't save the file...)
# The code is copied and pasted below from another preprocessing script -

########################### 

# Step 1: Filter only HP1 SiteCodes
hp1_data <- reference_data %>%
  filter(SiteCode == "HP1")
# Step 2: Convert DateTime column to character for string manipulation
hp1_data$DateTime <- as.character(hp1_data$DateTime)
# Step 3: Append "00:00:00" where time is missing (i.e., just a date is printed)
hp1_data <- hp1_data %>%
  mutate(DateTime = ifelse(nchar(DateTime) == 10,
                           paste0(DateTime, " 00:00:00"),
                           DateTime))
# Step 4: Convert DateTime column to proper POSIXct object
hp1_data$DateTime <- as.POSIXct(hp1_data$DateTime, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")
# Step 5: Sort by time
hp1_data <- hp1_data %>%
  arrange(DateTime)

# We have 15 minute interval data which we don't need. As instructed by BB - average into hourly 
# measurements. 

# Step 1: Clean and prepare
hp1_clean <- hp1_data %>%
  select(SiteCode, DateTime, Value) %>%
  mutate(DateTime = as.POSIXct(DateTime, tz = "UTC")) %>%
  arrange(DateTime)
# Step 2: Build hourly timestamps from first to last reading
start_time <- floor_date(min(hp1_clean$DateTime, na.rm = TRUE), unit = "hour")
end_time <- ceiling_date(max(hp1_clean$DateTime, na.rm = TRUE), unit = "hour")
hour_seq <- seq(from = start_time, to = end_time, by = "1 hour")
# Step 3: Preserve the first row exactly (special rule)
first_row <- hp1_clean[1, ] %>%
  transmute(SiteCode, DateTime = floor_date(DateTime, "hour"), Avg_Temp = Value)
# Step 4: Loop through remaining hours and compute rolling average
hour_seq_rest <- hour_seq[hour_seq != first_row$DateTime]  # Exclude first timestamp
avg_list <- list()  # Store results

for (i in seq_along(hour_seq_rest)) {
  current_hour <- hour_seq_rest[i]
  window_times <- current_hour - minutes(c(45, 30, 15, 0))
  
  temp_vals <- hp1_clean %>%
    filter(DateTime %in% window_times) %>%
    pull(Value)
  
  avg_temp <- if (length(temp_vals) > 0) mean(temp_vals, na.rm = TRUE) else NA
  
  avg_list[[as.character(current_hour)]] <- data.frame(
    SiteCode = "HP1",
    DateTime = current_hour,
    Avg_Temp = avg_temp
  )
  
  # Progress message
  if (i %% 1000 == 0) {
    message("Processed ", i, " of ", length(hour_seq_rest), " timestamps.")
  }
}
# Step 5: Combine into one dataframe
hp1_hourly_final <- bind_rows(first_row, bind_rows(avg_list))

# View or write to file
View(hp1_hourly_final)

sum(is.na(hp1_hourly_final$Avg_Temp)) # 417 missing values
# write.csv(hp1_hourly_final, "/Users/parivrudhsharma/Desktop/Desktop/Imperial College London/MSc Health Data Analytics and Machine Learning/Modules/Research Project/Data/Data/hourly_reference_data.csv", row.names = FALSE)
# this is the correct file now 

########################### 

# ok this new reference file now needs to be combined with golden_nodes_only

# Convert character DateTime in golden_nodes_only to POSIXct
golden_nodes_only <- golden_nodes_only %>%
  mutate(DateTime = as.POSIXct(DateTime, format = "%Y-%m-%d %H:%M:%S", tz = "UTC"))
# Merge reference temp into sensor data
merged_golden <- golden_nodes_only %>%
  left_join(hp1_hourly_final, by = "DateTime")
# View result
View(merged_golden)

merged_golden <- merged_golden %>%
  rename(True_Temp = Avg_Temp)

sum(is.na(merged_golden$True_Temp))
sum(is.na(merged_golden$TMP))

# write.csv(merged_golden, "/Users/parivrudhsharma/Desktop/Desktop/Imperial College London/MSc Health Data Analytics and Machine Learning/Modules/Research Project/Data/Data/Golden_nodes.csv", row.names = FALSE)

########################### 

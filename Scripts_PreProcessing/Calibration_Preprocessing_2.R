# Pre-processing for calibration data 
# Made by Parivrudh Sharma 

###########################
rm(list=ls())
library(dplyr)
library(sf)  
library(ggplot2)
library(lubridate)
library(purrr)
library(readxl)
###########################

setwd("/Users/parivrudhsharma/Desktop/Desktop/Imperial College London/MSc Health Data Analytics and Machine Learning/Modules/Research Project/Data")

colocation_md <- read.csv("Data/ColocationSitesMetadata.csv")
full_dataset <- read.csv("Data/TMPRHUM_alldatatoMay25.csv")
reference_data <- read.csv("Data/5yrs_T_HOP_MR9.csv")
device_codes <- read_excel("Data/devicecodesforpuru.xlsx")

View(colocation_md)
View(full_dataset)
View(reference_data)
View(device_codes)

###########################
# Ensure the StartDate and EndDate columns are in Date or POSIXct format
colocation_md$StartDate <- as.POSIXct(colocation_md$StartDate, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")
colocation_md$EndDate <- as.POSIXct(colocation_md$EndDate, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")
# Calculate duration in days
colocation_md$ColocationDays <- as.numeric(difftime(colocation_md$EndDate, colocation_md$StartDate, units = "days"))

full_dataset$DateTime <- as.POSIXct(full_dataset$DateTime, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")
########################### Need to place the sitecodes in order

# Sort the full_dataset by SiteCode and DateTime
full_dataset <- full_dataset %>%
  arrange(SiteCode, DateTime)

# Keep only CLCA and CLDP nodes from full_dataset
clca_data_only <- full_dataset %>%
  filter(grepl("^CLCA", SiteCode)) %>%
  mutate(DateTime = as.POSIXct(DateTime, tz = "UTC")) %>%
  arrange(SiteCode, DateTime) 

cldp_data_only <- full_dataset %>%
  filter(grepl("^CLDP", SiteCode)) %>%
  mutate(DateTime = as.POSIXct(DateTime, tz = "UTC")) %>%
  arrange(SiteCode, DateTime) 

########################### Checks for colocation_md 

# Find rows with negative or NA ColocationDays
problematic_rows <- colocation_md %>%
  filter(is.na(ColocationDays) | ColocationDays < 0)
# Display the problematic rows
print(problematic_rows)

# Issues - negative dates for certain nodes (are the start and end dates meant to be the other 
# way around?) and some dates are NAs (this is to do with the fact that calibration was recently started 
# and has not yet finished). Repeats also present. 

# We need a rule based approach - supervisor has confirmed that calibration dates in the collocation_md 
# dataset is what we go by and not the all the CLCA dates present in full_dataset. 

# Step 1: For each SiteCode in colocation_md, find actual start and end from clca_data_only
clca_ranges <- clca_data_only %>%
  group_by(SiteCode) %>%
  summarise(
    FullDataset_StartDate = min(DateTime, na.rm = TRUE),
    FullDataset_EndDate = max(DateTime, na.rm = TRUE)
  )

# Step 2: Join with colocation_md
colocation_comparison <- colocation_md %>%
  left_join(clca_ranges, by = "SiteCode") %>%
  mutate(
    FullDataset_CollocationDays = as.numeric(difftime(FullDataset_EndDate, FullDataset_StartDate, units = "days"))
  )

# View result
View(colocation_comparison)

# Ok so the negative ones with < -330 collocation days have essentially got a data entry error where 
# the end date year should be 2023 not 2022. Fix below - 

# Vector of target CLCA SiteCodes
target_sites <- c(
  "CLCA0625", "CLCA0621", "CLCA0613", "CLCA0611", "CLCA0601", "CLCA0616",
  "CLCA0617", "CLCA0626", "CLCA0593", "CLCA0594", "CLCA0592", "CLCA0591",
  "CLCA0600", "CLCA0596", "CLCA0598", "CLCA0599", "CLCA0608", "CLCA0589",
  "CLCA0590", "CLCA0588", "CLCA0618", "CLCA0586", "CLCA0587", "CLCA0595",
  "CLCA0566"
)
# Modify year of EndDate to 2023 for selected SiteCodes
colocation_md <- colocation_md %>%
  mutate(
    EndDate = if_else(
      SiteCode %in% target_sites,
      update(EndDate, year = 2023),
      EndDate
    )
  )
# View updated rows
View(colocation_md) # still need to update the collocation days column 

# Remaining 2 negative rows - one end date year needs to be changed to 2022 and the other needs the full 
# date switched around. Fix below - 

colocation_md <- colocation_md %>%
  mutate(
    EndDate = if_else(
      SiteCode %in% "CLCA0362",
      update(EndDate, year = 2022),
      EndDate
    )
  )

# Swap StartDate and EndDate for CLCA0014
colocation_md <- colocation_md %>%
  mutate(
    temp = StartDate,
    StartDate = if_else(SiteCode == "CLCA0014", EndDate, StartDate),
    EndDate = if_else(SiteCode == "CLCA0014", temp, EndDate)
  ) %>%
  select(-temp)

View(colocation_md) # still need to update the collocation days column 

# some comments - CLDP0029/CLCA0014 and CLDP0030/CLCA0072 are the golden nodes.
# Repeat Calculate duration in days
colocation_md$ColocationDays <- as.numeric(difftime(colocation_md$EndDate, colocation_md$StartDate, units = "days"))

########################### 

# ok Given that we have multiple calibrations for certain nodes, I want to check whether 
# this actually effects CLDP data at all - need to look at device codes file: 

# Step 1: Remove StartDate and EndDate columns
device_codes_clean <- device_codes %>%
  select(-StartDate, -EndDate)
# Step 2: Split into CLCA and CLDP based on SiteCode prefix
clca_sites <- device_codes_clean %>%
  filter(grepl("^CLCA", SiteCode)) %>%
  rename(SiteCode_CLCA = SiteCode)
cldp_sites <- device_codes_clean %>%
  filter(grepl("^CLDP", SiteCode)) %>%
  rename(SiteCode_CLDP = SiteCode)
# Step 3: Merge on DeviceCode using full join
combined_sites <- full_join(clca_sites, cldp_sites, by = "DeviceCode", relationship = "many-to-many")

View(combined_sites)

# Now we need to take all the unique nodes from CLDP only data and filter combined_sites based off that
# and then see which CLCA nodes are actually relevant 

# Get unique CLDP SiteCodes from cldp_data_only
unique_cldp_sites <- unique(cldp_data_only$SiteCode)

length(unique_cldp_sites) # 634 CLDP sites in full_dataset

# Filter combined_sites to keep only rows where SiteCode_CLDP is in that list
combined_sites_filtered <- combined_sites %>%
  filter(SiteCode_CLDP %in% unique_cldp_sites)

length(unique(combined_sites_filtered$SiteCode_CLDP)) # 633, so one CLDP node doesn't have a corresponding 
# CLCA node

########## 
# Need to remove from full_dataset as we cannot perform 1st stage calibration without CLCA 
# CLDP codes that have a CLCA mapping in combined_sites
mapped_cldp_sites <- unique(combined_sites$SiteCode_CLDP)
# Identify CLDP nodes with no CLCA equivalent in combined_sites
missing_cldp <- setdiff(unique_cldp_sites, mapped_cldp_sites)
missing_cldp # CLDP0287 
##########

# View the filtered result
View(combined_sites_filtered) # 778 rows. 

length(unique(combined_sites_filtered$SiteCode_CLDP)) # 633 - 2 golden nodes present, so 631 active nodes

unique_clca_sites <- unique(combined_sites_filtered$SiteCode_CLCA)
length(unique_clca_sites) # 700

# ok now use this list of unique_clca_sites to filter colocation_md from above
colocation_md_filtered <- colocation_md %>%
  filter(SiteCode %in% unique_clca_sites)

View(colocation_md_filtered)
length(unique(colocation_md_filtered$SiteCode)) # 700 unique CLCA sites

# Ok now we need to check weather we have any duplicated CLCA nodes (these are the CLDP relevant 
# CLCA nodes)

# Find duplicated SiteCodes and show their rows
duplicates <- colocation_md_filtered %>%
  group_by(SiteCode) %>%
  filter(n() > 1) %>%
  arrange(SiteCode, StartDate)

# View the duplicated entries
View(duplicates)

# ok we have multiple nodes which have had 2 periods of co-location, 
# and therefore we need to find the best method to deal with them. 

# Lets go with the assumption that they have been re-located due to the fact that there measurements 
# were faulty. In this case we need to drop the first co-location period entirely and use the final
# one only. If there is no end date on the final one (there are a few cases of this, then we need
# to drop that node entirely in my opinion)

# --- 1) For each duplicated SiteCode, keep the row with the latest StartDate
dup_keep <- duplicates %>%
  group_by(SiteCode) %>%
  slice_max(order_by = StartDate, n = 1, with_ties = FALSE) %>%
  ungroup()

View(dup_keep)

# ok use dup_keep to update colocation_md_filtered

# 1) Identify the duplicated SiteCodes I want to keep/replace with
dup_codes <- dup_keep %>% distinct(SiteCode)

# 2) Remove all existing rows for those SiteCodes from colocation_md_filtered,
#    then append the single resolved rows from dup_keep
colocation_md_updated <- colocation_md_filtered %>%
  anti_join(dup_codes, by = "SiteCode") %>%   # drop both/all originals
  bind_rows(dup_keep) %>%                     # add the kept (updated) row
  arrange(SiteCode, StartDate)

View(colocation_md_updated)
length(unique(colocation_md_updated$SiteCode)) # 700 unique CLCA sites

# drop the rows which dont have an end date now - 

colocation_md_updated <- colocation_md_updated %>%
  filter(!is.na(EndDate))

length(unique(colocation_md_updated$SiteCode)) # 681 unique CLCA nodes (with 2 golden nodes, dont remove)

# write.csv(colocation_md_updated, "Collocation_md_PreProcessed.csv", row.names = FALSE)
# Final colocation file

colocation_md <- read.csv("Data/Collocation_md_PreProcessed.csv")

View(colocation_md)
#This file will be used for downstream stuff

############################################

# Now that we have dropped a few CLCA nodes, that means we have also dropped CLDP nodes 
# we now need to update combined_sites_filtered to only keep the CLCA nodes which we have data for

# Get unique CLCA SiteCodes from colocation_md
valid_sites <- unique(colocation_md$SiteCode)

length(valid_sites) # 681, matches above

length(unique(combined_sites_filtered$SiteCode_CLDP)) # 633, before 

# Filter combined_sites_filtered to keep only matching CLCA sites
combined_sites_filtered <- combined_sites_filtered %>%
  filter(SiteCode_CLCA %in% valid_sites)
# View the result
View(combined_sites_filtered)

combined_sites_filtered <- combined_sites_filtered %>%
  distinct()

length(unique(combined_sites_filtered$SiteCode_CLCA)) # 681, matches
length(unique(combined_sites_filtered$SiteCode_CLDP)) # 618, after (lost 15 CLDP nodes)
length(unique(combined_sites_filtered$DeviceCode)) # 681

# write.csv(combined_sites_filtered, "LOOKUPTABLE_FINAL.csv", row.names = FALSE) 
# only use this from now on

combined_sites_filtered <- read.csv("Data/LOOKUPTABLE_FINAL.csv")

View(combined_sites_filtered)

########################## just adding 00:00:00 as it keeps going missing when uploading the dataset
colocation_md <- colocation_md %>%
  mutate(
    StartDate = as.character(StartDate),
    EndDate = as.character(EndDate),
    
    StartDate = ifelse(!grepl("\\d{2}:\\d{2}:\\d{2}$", StartDate) & !is.na(StartDate),
                       paste0(StartDate, " 00:00:00"),
                       StartDate),
    
    EndDate = ifelse(!is.na(EndDate) & !grepl("\\d{2}:\\d{2}:\\d{2}$", EndDate),
                     paste0(EndDate, " 00:00:00"),
                     EndDate),
    
    StartDate = ymd_hms(StartDate),
    EndDate = ymd_hms(EndDate)
  )
##########################

# Now using the colocation_md file we need to create a new dataset which contains hourly values from 
# between the colocation dates and then populate them from the CLCA date from full_dataset. 

# some comments - CLDP0029/CLCA0014 and CLDP0030/CLCA0072 are the golden nodes - lets remove as not needed
colocation_md <- colocation_md %>%
  filter(!SiteCode %in% c("CLCA0014", "CLCA0072"))

# First, I need to check whether every unique SiteCode has a uniqe DeviceCode
# Check how many unique DeviceCodes are associated with each SiteCode
sitecode_mapping_check <- colocation_md %>%
  group_by(SiteCode) %>%
  summarise(
    n_devicecodes = n_distinct(DeviceCode),
    DeviceCodes = paste(unique(DeviceCode), collapse = ", ")
  ) %>%
  arrange(desc(n_devicecodes))
# Filter those with more than one DeviceCode (i.e. non-unique mappings)
overlapping_sitecodes <- sitecode_mapping_check %>%
  filter(n_devicecodes > 1)
# View non-unique SiteCode → DeviceCode mappings
View(overlapping_sitecodes)

length(unique(colocation_md$DeviceCode)) # 679
length(unique(colocation_md$SiteCode)) # 679

# ok happy that SiteCode and DeviceCode match properly and there is no overlap

# Function to round up/down to nearest hour
round_up_hour <- function(dt) ceiling_date(dt, unit = "hour")
round_down_hour <- function(dt) floor_date(dt, unit = "hour")

# Safely generate hourly sequences only for rows with complete datetime info, remove NAs
hourly_time_df <- colocation_md %>%
  filter(!is.na(StartDate), !is.na(EndDate)) %>%
  rowwise() %>%
  mutate(
    Start_Hour = round_up_hour(StartDate),
    End_Hour = round_down_hour(EndDate),
    Hours = list(seq(from = Start_Hour, to = End_Hour, by = "1 hour"))
  ) %>%
  unnest(Hours) %>%
  rename(DateTime = Hours) %>%
  select(SiteCode, DeviceCode, DateTime) %>%
  ungroup()

# View result
View(hourly_time_df)
# ok this is our hourly calibration dataset without any temperature values. 

length(unique(hourly_time_df$DeviceCode)) # 679
length(unique(hourly_time_df$SiteCode)) # 679

# Now we need to add the raw sensor temperature values from full_dataset (clca_only)

# Join to add TMP to hourly_time_df
hourly_time_with_tmp <- hourly_time_df %>%
  left_join(clca_data_only %>% select(SiteCode, DateTime, TMP), 
            by = c("SiteCode", "DateTime"))

# View the result
View(hourly_time_with_tmp)

# Ok thats done... 

# Step 1: Get start and end datetime per DeviceCode
device_summary <- hourly_time_with_tmp %>%
  group_by(DeviceCode, SiteCode) %>%
  summarise(
    StartDate = min(DateTime, na.rm = TRUE),
    EndDate = max(DateTime, na.rm = TRUE),
    Total_Hours = n(),
    Missing_Hours = sum(is.na(TMP)),
    Percent_Missing = round(100 * Missing_Hours / Total_Hours, 2),
    .groups = "drop"
  )

# View the summary
View(device_summary)  
length(unique(device_summary$DeviceCode)) # 679

########################### 

# Now we need to pre-process the actual reference temperature data

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

View(hp1_data)

# Step 6: Plot and save as PDF
pdf("HP1_Temperature_Overview.pdf", width = 14, height = 6)
ggplot(hp1_data, aes(x = DateTime, y = ScaledValue)) +
  geom_line(color = "steelblue") +
  labs(title = "HP1 Temperature Over Time",
       x = "DateTime",
       y = "Temperature (ScaledValue)") +
  theme_minimal()
dev.off()

# We have 15 minute interval data which we don't need. As instructed by BB - average into hourly 
# measurements. 

# I first want to see the temperature range we have - this could be a limitation...
# Create a box plot of the temperature values
ggplot(hp1_data, aes(y = ScaledValue)) +
  geom_boxplot(fill = "skyblue", outlier.colour = "red", outlier.shape = 1) +
  labs(title = "Boxplot of HP1 Temperature Measurements",
       y = "Temperature (°C)") +
  theme_minimal()

# ok we have a nice spread! 

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

# Define the output PDF file path
pdf("HP1_Temperature_TimeSeries_corrected.pdf", width = 14, height = 6)
# Create the time series plot
ggplot(hp1_hourly_final, aes(x = DateTime, y = Avg_Temp)) +
  geom_line(color = "steelblue") +
  labs(
    title = "HP1 Hourly Temperature Time Series",
    x = "Date and Time",
    y = "Temperature (°C)"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )
# Close the PDF device
dev.off()

########################### 

# Now lets combine the 2 - hp1_hourly_final (ground truth) and hourly_time_with_tmp (sensor reading) on 
# DateTime - so we have the raw and ground truth temperature side by side. 

# First rename Avg_Temp to True_Temp
hp1_hourly_final <- hp1_hourly_final %>%
  rename(True_Temp = Avg_Temp)

# Then perform the left join on DateTime
merged_data <- hourly_time_with_tmp %>%
  left_join(hp1_hourly_final, by = "DateTime")

# Drop SiteCode.y and rename SiteCode.x
merged_data <- merged_data %>%
  select(-SiteCode.y) %>%
  rename(SiteCode = SiteCode.x)

View(merged_data)
# write.csv(merged_data, "calibration_model_dataset.csv", row.names = FALSE)

########################### 

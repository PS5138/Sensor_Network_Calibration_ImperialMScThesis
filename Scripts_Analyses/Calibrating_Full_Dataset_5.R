# Applying calibration (2-stages) to full dataset
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
library(openair)
library(worldmet)
library(leaflet)
library(geosphere)
library(ggspatial)
library(viridis)
library(viridisLite)
library(terra)
library(nngeo)
###########################

setwd("/Users/parivrudhsharma/Desktop/Desktop/Imperial College London/MSc Health Data Analytics and Machine Learning/Modules/Research Project/Data")

colocation_md <- read.csv("Data/Collocation_md_PreProcessed.csv")
deployment_md <- read.csv("Data/DeploymentSitesMetadata_PreProcessed.csv")
full_dataset <- read.csv("Data/TMPRHUM_alldatatoMay25.csv")
reference_data <- read.csv("Data/5yrs_T_HOP_MR9.csv")
calibration_models <- read.csv("Data/calibration_model_dataset.csv")
Golden_nodes <- read.csv("Data/Golden_nodes.csv")
combined_sites <- read.csv("Data/LOOKUPTABLE_FINAL.csv")
st_james_temp <- read.csv("Data/st_james_air_temp_2021_2025.csv")

View(colocation_md)
View(deployment_md)
View(full_dataset)
View(reference_data)
View(calibration_models)
View(Golden_nodes)
View(combined_sites)
View(st_james_temp)

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

calibration_models$True_Temp <- round(calibration_models$True_Temp, 1)

#######################################

filtered_calibration_models <- calibration_models
filtered_clean <- filtered_calibration_models

filtered_clean <- filtered_clean %>%
  rename(sensor_raw_tmp = TMP)

############################################

# Ok so now we have hourly temp from st_james, now we need to -
# 1. We need to first apply the precision scaling from the CLCA collocation data 
# 2. We need to apply accuracy scaling (weekly) based off the golden nodes to these precision scaled nodes
# 3. Then we need to find these 3 nodes - CLDP0009, CLDP0329, CLDP0495 - and take their temp readings into
# a single table and then join it up with st_james_temp.

# Then we just evaluate the error between each node.

############################################

# first step - 

View(cldp_data_only) # CLDP nodes from full_dataset (untouched)
View(filtered_clean) # CLCA nodes which have a corresponding CLDP node and 2 golden nodes removed (CLDP0029/CLDP0030)
View(combined_sites) # look-up table for CLCA, CLDP, and DeviceCodes - only contains unique CLDP sites. 

length(unique(cldp_data_only$SiteCode)) # 634 unique cldp sites...
length(unique(filtered_clean$SiteCode)) # 679 unique clca sites...

length(unique(combined_sites$DeviceCode)) # 681 unique device codes (2 golden nodes)
length(unique(combined_sites$SiteCode_CLCA)) # 681 unique CLCA (2 golden nodes), this matches with filtered_clean 
length(unique(combined_sites$SiteCode_CLDP)) # 618 unique CLDP (2 golden nodes)

# Some nodes need to be dropped from cldp_data_only 

length(unique(filtered_clean$DeviceCode)) # 679

############################################

# The extra nodes in CLDP_data_only and not combined_sites

# distinct CLDP site codes in each table
cldp_in_dataonly <- cldp_data_only %>% distinct(SiteCode) %>% rename(SiteCode_CLDP = SiteCode)
cldp_in_mapping  <- combined_sites %>% distinct(SiteCode_CLDP)

# the CLDP sites that have no mapping
unmapped_cldp <- cldp_in_dataonly %>%
  anti_join(cldp_in_mapping, by = "SiteCode_CLDP")

unmapped_cldp # a few

# Now lets remove those rows with those nodes so everything matches - 
cldp_data_only <- cldp_data_only %>%
  anti_join(unmapped_cldp, by = c("SiteCode" = "SiteCode_CLDP"))

length(unique(cldp_data_only$SiteCode)) # 618
length(unique(combined_sites$SiteCode_CLDP)) # 618 unique CLDP

# Now we should also remove the 2 golden nodes from cldp_data_only as its not required. 
cldp_data_only <- cldp_data_only %>%
  filter(SiteCode != "CLDP0029")
cldp_data_only <- cldp_data_only %>%
  filter(SiteCode != "CLDP0030")

length(unique(cldp_data_only$SiteCode)) # 616 (2 golden nodes removed)

# Ok now everything matches

########################################

# Lets add a device code column into cldp_data_only 

# Count how many device codes per CLDP site
multi_device_map <- combined_sites %>%
  count(SiteCode_CLDP, name = "n_device_codes") %>%
  arrange(desc(n_device_codes))

# List the actual CLDP sites with >1 device code
multi_device_sites <- multi_device_map %>%
  filter(n_device_codes > 1)

View(multi_device_sites)

# The CLCA dataset (filtered_clean) directly corresponds 1:1 - 679 unique device codes and 679 unique CLCA sites

# This means that within one cldp there are multiple devices and each of those devices will get a slightly different 
# precision scaling protocol from filtered_clean (given there are 679 regression models in there)

# ok so overall, 679 device code calibrations will be applied to 616 cldp sites (some of these have multiple devices
# within them)

# 1) Make sure the mapping has unique pairs (guards against dupes in the mapping table)
map_unique <- combined_sites %>%
  select(SiteCode_CLDP, DeviceCode) %>%
  distinct()

View(map_unique) 

length(unique(map_unique$DeviceCode)) # 681 (2 golden nodes present)
length(unique(map_unique$SiteCode_CLDP)) # 618 (2 golden nodes present)

# 2) Join (many-to-many is expected here)
cldp_data_only_full <- cldp_data_only %>%
  left_join(map_unique, by = c("SiteCode" = "SiteCode_CLDP"), relationship = "many-to-many")

# 3) Quick diagnostics
# Row counts before/after
n_before <- nrow(cldp_data_only)
n_after  <- nrow(cldp_data_only_full)
message("Rows before: ", n_before, " | after join: ", n_after)

# rows before - 10,194,367 and after - 12,267,466

View(cldp_data_only_full)

length(unique(cldp_data_only_full$SiteCode)) # 616
length(unique(cldp_data_only_full$DeviceCode)) # 679

length(unique(filtered_clean$DeviceCode)) # 679

# Now everything matches

########################################

# some exploratory analysis on this final deployed dataset

View(deployment_md_combined)

# load the shapefile (change path to where your GLA boundary lives)
london_boundary <- st_read("Data/gla/London_GLA_Boundary.shp") %>% 
  st_transform(4326)  # make sure it's WGS84 lat/long

# 1) Years present in the CLDP data
years <- sort(unique(lubridate::year(cldp_data_only_full$DateTime)))

# 2) Active CLDP nodes by year (from hourly data)
active_cldp <- cldp_data_only_full %>%
  mutate(Year = year(DateTime)) %>%
  group_by(Year, SiteCode) %>%
  summarise(n_obs = n(), .groups = "drop") %>%
  left_join(
    deployment_md_combined %>% 
      select(SiteCode, Latitude, Longitude, SiteClassification),
    by = "SiteCode"
  ) %>%
  filter(!is.na(Latitude), !is.na(Longitude)) %>%
  distinct(Year, SiteCode, Latitude, Longitude, SiteClassification) %>%
  mutate(Source = "CLDP")

# 3) UKMO reference sites (replicated across all years)
ukmo_sites <- deployment_md_combined %>%
  filter(grepl("^UKMO_", SiteCode) | SiteClassification == "Reference UKMO") %>%
  select(SiteCode, Latitude, Longitude, SiteClassification) %>%
  distinct()

ukmo_active <- tidyr::expand_grid(Year = years, ukmo_sites) %>%
  mutate(Source = "UKMO")

# 4) Golden nodes (replicated across all years)
golden_sites <- deployment_md_combined %>%
  filter(SiteCode %in% c("CLDP0029", "CLDP0030")) %>%
  select(SiteCode, Latitude, Longitude, SiteClassification) %>%
  distinct()

golden_active <- tidyr::expand_grid(Year = years, golden_sites) %>%
  mutate(Source = "GOLDEN")

# 5) Combine all
active_all <- bind_rows(active_cldp, ukmo_active, golden_active) %>%
  mutate(
    is_ukmo   = Source == "UKMO" | grepl("^UKMO_", SiteCode),
    is_golden = Source == "GOLDEN",
    is_cldp   = Source == "CLDP"
  )

# 6) To sf (WGS84)
active_sf <- st_as_sf(active_all, coords = c("Longitude", "Latitude"), crs = 4326)

# 7) Plot with legend
p <- ggplot() +
  geom_sf(data = london_boundary, fill = "grey98", colour = "grey80", linewidth = 0.3) +
  geom_sf(
    data = active_sf,
    aes(color = Source, shape = Source),
    size = 1.8, alpha = 0.85
  ) +
  facet_wrap(~ Year, ncol = 2) +
  coord_sf(expand = FALSE) +
  labs(
    title    = "Active Breathe London Nodes by Year",
    subtitle = NULL,
    color    = NULL,
    shape    = NULL
  ) +
  scale_color_manual(
    values = c("CLDP" = "#2C7FB8", "UKMO" = "red", "GOLDEN" = "black")
  ) +
  scale_shape_manual(
    values = c("CLDP" = 16, "UKMO" = 17, "GOLDEN" = 15)  # circle, triangle, square
  ) +
  theme_void(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold"),
    strip.text = element_text(face = "bold"),
    legend.position = "right",
    legend.box = "vertical",
    legend.text = element_text(size = 11),
    plot.margin = margin(8, 8, 8, 8)
  )

ggsave("active_nodes_by_year.pdf", p, width = 11.69, height = 8.27, units = "in")

# Number of deployed nodes per year - 

nodes_per_year <- cldp_data_only_full %>%
  mutate(year = year(DateTime)) %>%          # extract year from DateTime
  group_by(year) %>%
  summarise(
    n_nodes = n_distinct(SiteCode)
  ) %>%
  arrange(year)

nodes_per_year

#### just the UKMO sites alone - 

# 1) Read London boundary and put it in WGS84 (matches your lat/long)
london <- st_read("Data/gla/London_GLA_Boundary.shp", quiet = TRUE) |>
  st_transform(4326)

# 2) Filter UKMO reference sites from your combined table
ukmo_sites <- deployment_md_combined |>
  filter(SiteClassification == "Reference UKMO",
         !is.na(Latitude), !is.na(Longitude)) |>
  distinct(SiteCode, .keep_all = TRUE) |>
  st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326)

# 3) Plot
p <- ggplot() +
  geom_sf(data = london, fill = NA, linewidth = 0.6) +
  geom_sf(data = ukmo_sites, shape = 21, fill = "red", color = "black", size = 2.5, stroke = 0.3) +
  coord_sf() +
  theme_minimal(base_size = 11) +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title = element_blank()) +
  labs(title = "UKMO Reference Sites within Greater London")

# 4) Save (adjust size as you like)
ggsave("ukmo_reference_sites_london.pdf", p, width = 6, height = 6, units = "in")

########################################

# We need a table which shows - 
# Each sensor’s start and end date co-location dates and start and end deployment dates in table format. 
# Missing co-location data were quantified by calculating, for each node, the total number of expected hourly
# measurements, the number of missing measurements, and the corresponding percentage missing. An additional column 
# recorded the number of matching ground truth temperature observations available for calibration

## 1) Enrich CLCA records with mapping to CLDP site 
clca_enriched <- filtered_clean %>%
  left_join(
    combined_sites %>%
      select(SiteCode_CLCA, DeviceCode, SiteCode_CLDP),
    by = c("SiteCode" = "SiteCode_CLCA", "DeviceCode" = "DeviceCode")
  )

View(filtered_clean) # 556,767 rows

View(clca_enriched) # 580,807 rows -> increase expected given that 1 cldp node can match with multiple clca nodes/device codes.
length(unique(clca_enriched$SiteCode)) # 679
length(unique(clca_enriched$DeviceCode)) # 679
length(unique(clca_enriched$SiteCode_CLDP)) # 616

colSums(is.na(clca_enriched))

# the number of rows of clca_enriched is expected to go up here given that you have single cldp sites that map to
# to multiple device codes. 

## 2) Co-location summary per device (start/end + missingness)
clca_summary <- clca_enriched %>%
  group_by(DeviceCode) %>%
  summarise(
    CLDP_SiteCode     = first(na.omit(SiteCode_CLDP)),
    CLCA_Start        = min(DateTime, na.rm = TRUE),
    CLCA_End          = max(DateTime, na.rm = TRUE),
    Expected_Hourly   = n(),                             # total rows (expected hours)
    Missing_True      = sum(is.na(True_Temp)),           # NA in reference (“ground truth”)
    Pct_Missing_True  = round(100 * Missing_True / Expected_Hourly, 2),
    N_Matching_GT     = sum(!is.na(True_Temp)),          # non-NA True_Temp rows
    .groups = "drop"
  )

## 3) Deployment summary per CLDP site (start/end across years)
deploy_summary <- cldp_data_only_full %>%
  group_by(SiteCode) %>%
  summarise(
    CLDP_Start = min(DateTime, na.rm = TRUE),
    CLDP_End   = max(DateTime, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  rename(CLDP_SiteCode = SiteCode)

## 4) Final table: co-location + deployment
co_loc_deploy_table <- clca_summary %>%
  left_join(deploy_summary, by = "CLDP_SiteCode") %>%
  relocate(DeviceCode, CLDP_SiteCode, CLCA_Start, CLCA_End, CLDP_Start, CLDP_End)

## 5) Inspect
View(co_loc_deploy_table)

length(unique(co_loc_deploy_table$DeviceCode)) # 679

##############

# number of co-location observations - 

# Boxplot of Number of Matching Observations per node
pdf("Boxplot_CoLocation_Observations.pdf", width = 8, height = 6)
ggplot(co_loc_deploy_table, aes(x = "", y = `Number of Matching Observations`)) +
  geom_boxplot(fill = "skyblue", color = "black", outlier.color = "red") +
  labs(
    y = "Number of Observations",
    title = "Distribution of Matching Observations per Node during Co-location"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    axis.title.x = element_blank(),
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank()
  )
dev.off()

############## Spread of data points per deployed node -
  
colSums(is.na(cldp_data_only_full))

# Count rows per device
device_counts <- cldp_data_only_full %>%
  group_by(DeviceCode) %>%
  summarise(n_obs = n(), .groups = "drop")

# Boxplot of number of observations per device
p <- ggplot(device_counts, aes(y = n_obs)) +
  geom_boxplot(fill = "skyblue", outlier.shape = 21, outlier.color = "red") +
  labs(
    title = "Distribution of Observation Counts per Device",
    y = "Number of Observations"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(size = 13),
    axis.title.x = element_blank(),
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank()
  )

# Save PDF
pdf("Boxplot_DeviceCode_ObservationCounts.pdf", width = 8, height = 6)
print(p)
dev.off()

############## Spread of temperature with the deployed nodes and the golden nodes 

View(cldp_data_only_full)

length(unique(cldp_data_only_full$SiteCode)) # 616
length(unique(cldp_data_only_full$DeviceCode)) # 679

View(golden_clean)

# 1) Prepare datasets
tmp_all <- cldp_data_only_full %>% 
  filter(is.finite(TMP)) %>%
  mutate(Group = "All Deployed Nodes") %>%
  select(TMP, Group)

tmp_gold <- golden_clean %>%
  filter(is.finite(TMP)) %>%
  mutate(Group = "Golden Nodes") %>%
  select(TMP, Group)

# Combine both datasets
df <- bind_rows(tmp_all, tmp_gold)

# 2) Boxplot
p <- ggplot(df, aes(x = Group, y = TMP)) +
  geom_boxplot(fill = "skyblue", outlier.shape = 21, outlier.color = "red") +
  labs(
    title = "Comparison of Temperature Distributions: Deployed vs Golden Nodes",
    x = NULL, y = "Temperature (°C)"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(size = 13),
    axis.text.x = element_text(size = 12)
  )

# 3) Save as PDF
pdf("Boxplot_Deploy_vs_Golden_Combined.pdf", width = 8, height = 6)
print(p)
dev.off()

################# full, time series plot of cldp_data_only_full

pdf("Timeseries_TMP_by_Device_HOURLY.pdf", width = 16, height = 9)
ggplot(cldp_data_only_full, aes(x = DateTime, y = TMP, group = DeviceCode, colour = DeviceCode)) +
  geom_line(linewidth = 0.1, alpha = 0.3) +
  labs(
    title = "Hourly Uncalibrated Temperature Values by Device (2021–2025)",
    x = "Year", y = "Temperature (°C)"
  ) +
  theme_minimal(base_size = 11) +
  guides(colour = "none")
dev.off()

################# First-stage calibration repeat (we now need to start calibrating the wider network)

# Get unique DeviceCodes
device_ids <- unique(filtered_clean$DeviceCode)
# Empty list for plots and coefficients
plot_list <- list()
all_coefs <- list()
# Loop over each DeviceCode
for (i in seq_along(device_ids)) {
  device <- device_ids[i]
  df <- filtered_clean %>%
    filter(DeviceCode == device, !is.na(True_Temp), !is.na(sensor_raw_tmp))
  
  if (nrow(df) < 10) next  # Skip small datasets
  
  model <- lm(True_Temp ~ sensor_raw_tmp, data = df)
  r2 <- summary(model)$r.squared
  coefs <- coef(model)
  intercept <- round(coefs[1], 3)
  slope <- round(coefs[2], 3)
  
  # Store coefficients
  all_coefs[[device]] <- data.frame(
    DeviceCode = device,
    Intercept = intercept,
    Slope = slope,
    R2 = r2
  )
  
  # Create plot
  p <- ggplot(df, aes(x = sensor_raw_tmp, y = True_Temp)) +
    geom_point(alpha = 0.4) +
    geom_smooth(method = "lm", se = FALSE, color = "blue") +
    labs(
      title = paste0("Device: ", device),
      subtitle = paste0("y = ", slope, "x + ", intercept, "   |   R² = ", round(r2, 3)),
      x = "Sensor Raw TMP",
      y = "True Temperature"
    ) +
    theme_minimal(base_size = 10)
  
  plot_list[[length(plot_list) + 1]] <- p
}

# Save to multipage PDF
pdf("Device_Regression_Lines.pdf", width = 9, height = 6)
for (i in seq(1, length(plot_list), by = 6)) {
  end_i <- min(i + 5, length(plot_list))
  grid.arrange(grobs = plot_list[i:end_i], ncol = 2)
}
dev.off()

# Combine all coefficients
coefs_df <- do.call(rbind, all_coefs)
# Join coefficients back to filtered_clean and compute predicted values
filtered_clean <- filtered_clean %>%
  left_join(coefs_df, by = "DeviceCode") %>%
  mutate(precision_scaled_tmp = Intercept + Slope * sensor_raw_tmp)

View(filtered_clean) # all the coefficients are now present in filtered_clean 

# Now we need to add each device code's calibration from filtered_clean

# create calibration table - 
calib_by_device <- filtered_clean %>%
  group_by(DeviceCode) %>%
  summarise(
    n_obs = n(),
    intercept = first(na.omit(Intercept)),
    slope     = first(na.omit(Slope)),
    r2        = first(na.omit(R2)),
    # sanity checks: do we see multiple values within a device?
    n_unique_intercepts = n_distinct(na.omit(Intercept)),
    n_unique_slopes     = n_distinct(na.omit(Slope)),
    n_unique_r2         = n_distinct(na.omit(R2)),
    .groups = "drop"
  ) %>%
  mutate(flag_mixed = n_unique_intercepts > 1 |
           n_unique_slopes     > 1 |
           n_unique_r2         > 1) %>%
  select(DeviceCode, intercept, slope, r2, n_obs, flag_mixed)

View(calib_by_device)
length(unique(calib_by_device$DeviceCode)) # 679

# Ok now we need to apply these coefficients to each device code's tmp in cldp_data_only_full

# 1) Keep just the fields we need from the calibration table
calib_small <- calib_by_device %>%
  select(DeviceCode, intercept, slope)

# 2) Join the coefficients onto the CLDP data by DeviceCode
cldp_with_coefs <- cldp_data_only_full %>%
  left_join(calib_small, by = "DeviceCode")

View(cldp_with_coefs)

# Count NAs per column
colSums(is.na(cldp_with_coefs)) # no NAs

# 3) Apply the calibration to create precision_scaled_tmp
cldp_with_coefs <- cldp_with_coefs %>%
  mutate(precision_scaled_tmp = intercept + slope * TMP)

View(cldp_with_coefs)

# First-stage applied.

########################################

# Ok now we need to apply accuracy scaling, but first we need to fit proper weekly models -
# Need to use test_0030 and test_0029 that we created during analysis of the golden nodes. 

# for some reason when I upload Golden_nodes - the 00:00:00 get removed but nothing else is changed
# Convert DateTime column to character for string manipulation
Golden_nodes$DateTime <- as.character(Golden_nodes$DateTime)
# Append "00:00:00" where time is missing (i.e., just a date is printed)
Golden_nodes <- Golden_nodes %>%
  mutate(DateTime = ifelse(nchar(DateTime) == 10,
                           paste0(DateTime, " 00:00:00"),
                           DateTime))
# Convert DateTime column to proper POSIXct object
Golden_nodes$DateTime <- as.POSIXct(Golden_nodes$DateTime, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")
Golden_nodes$True_Temp <- round(Golden_nodes$True_Temp, 1)

# Filter and clean data
golden_clean <- Golden_nodes %>%
  filter(!is.na(TMP), !is.na(True_Temp)) %>%
  mutate(Date = as.Date(DateTime)) %>%
  arrange(SiteCode.x, DateTime)

View(golden_clean)

# Split by node
node_0029 <- golden_clean %>% filter(SiteCode.x == "CLDP0029", !is.na(True_Temp), !is.na(TMP))
node_0030 <- golden_clean %>% filter(SiteCode.x == "CLDP0030", !is.na(True_Temp), !is.na(TMP))
# Step 1: Extract training sets (first 17 days)
train_0029 <- node_0029 %>%
  filter(Date >= min(Date) & Date <= min(Date) + days(16))
train_0030 <- node_0030 %>%
  filter(Date >= min(Date) & Date <= min(Date) + days(16))
# Step 2: Remaining data is test set
test_0029 <- anti_join(node_0029, train_0029, by = "DateTime")
test_0030 <- anti_join(node_0030, train_0030, by = "DateTime")

# Step 3: Fit precision scaling models
model_0029 <- lm(True_Temp ~ TMP, data = train_0029)
model_0030 <- lm(True_Temp ~ TMP, data = train_0030)

# Apply models and round predictions
test_0029 <- test_0029 %>%
  mutate(precision_scaled_tmp = round(predict(model_0029, newdata = test_0029), 1))

test_0030 <- test_0030 %>%
  mutate(precision_scaled_tmp = round(predict(model_0030, newdata = test_0030), 1))

View(test_0029)
View(test_0030)

######################

# 1) Merge the two golden-node series side-by-side and assign non-overlapping weeks (Mon-start)
weekly_input <- full_join(
  test_0029 %>%
    transmute(DateTime,
              True_Temp_0029 = True_Temp,
              ps_0029_tmp    = precision_scaled_tmp),
  test_0030 %>%
    transmute(DateTime,
              True_Temp_0030 = True_Temp,
              ps_0030_tmp    = precision_scaled_tmp),
  by = "DateTime"
) %>%
  mutate(
    True_Temp = coalesce(True_Temp_0029, True_Temp_0030),
    Week      = floor_date(DateTime, "week", week_start = 1)  # Monday→Sunday
  ) %>%
  select(DateTime, Week, True_Temp, ps_0029_tmp, ps_0030_tmp) %>%
  arrange(Week, DateTime)

View(weekly_input)

# safer mean that returns NA (not NaN) when everything is NA
safe_mean <- function(x) if (all(is.na(x))) NA_real_ else mean(x, na.rm = TRUE)

# helper 
fit_week_model <- function(df, x_col) {
  dd <- df %>% filter(!is.na(.data[[x_col]]), !is.na(True_Temp))
  n  <- nrow(dd)
  if (n < 5) return(list(intercept = NA_real_, beta1 = NA_real_, n = n))
  
  fml <- reformulate(x_col, response = "True_Temp")  # True_Temp ~ x_col
  m   <- lm(fml, data = dd)
  co  <- coef(m)
  list(intercept = unname(co[1]), beta1 = unname(co[2]), n = n)
}

weekly_coefs <- weekly_input %>%
  group_by(Week) %>%
  group_map(~{
    m29 <- fit_week_model(.x, "ps_0029_tmp")
    m30 <- fit_week_model(.x, "ps_0030_tmp")
    
    tibble(
      Week           = .y$Week,
      Intercept_0029 = m29$intercept,
      Beta1_0029     = m29$beta1,
      N_0029         = m29$n,
      Intercept_0030 = m30$intercept,
      Beta1_0030     = m30$beta1,
      N_0030         = m30$n,
      average_offset = safe_mean(c(m29$intercept, m30$intercept)),
      average_b1     = safe_mean(c(m29$beta1,     m30$beta1))
    )
  }) %>%
  bind_rows() %>%
  ungroup()

# (same as before) Join back to get one final table
final_weekly_table <- weekly_input %>%
  left_join(weekly_coefs, by = "Week") %>%
  arrange(Week, DateTime)

View(final_weekly_table)

################################# ok now apply it directly on to cldp_with_coefs

View(cldp_with_coefs)
# need to rename intercept and slope to 1_intercept and 1_slope

cldp_with_coefs <- cldp_with_coefs %>%
  rename(`1_intercept` = intercept,
         `1_beta1` = slope)

# alright now we need to add in the coefficients and calculate the new accuracy_scaled tmp 

# 1) Rename the averaged columns in the weekly table
final_weekly_table <- final_weekly_table %>%
  rename(`2_average_offset` = average_offset,
         `2_average_b1`     = average_b1)

# 2) Make sure DateTime types line up (same class/tz) before the join
final_weekly_table <- final_weekly_table %>%
  mutate(DateTime = as.POSIXct(DateTime, tz = "UTC"))

cldp_with_coefs <- cldp_with_coefs %>%
  mutate(DateTime = as.POSIXct(DateTime, tz = "UTC"))

# 3) Keep only one row per timestamp of the weekly averages to avoid duplicates
weekly_avgs <- final_weekly_table %>%
  select(DateTime, `2_average_offset`, `2_average_b1`) %>%
  distinct()

View(weekly_avgs)

# 4) Map the averaged weekly coefficients onto the CLDP table (unmatched stay NA)
cldp_with_coefs <- cldp_with_coefs %>%
  left_join(weekly_avgs, by = "DateTime")

colSums(is.na(cldp_with_coefs)) # 50,169 NAs in both 2_average_offset and 2_average_b1

View(cldp_with_coefs)

################################# Some NAs analysis - 

# 1) Count NAs per month for each column
na_by_month <- cldp_with_coefs %>%
  mutate(Month = floor_date(DateTime, "month")) %>%
  group_by(Month) %>%
  summarise(
    NAs_offset = sum(is.na(`2_average_offset`), na.rm = TRUE),
    NAs_beta1  = sum(is.na(`2_average_b1`),     na.rm = TRUE),
    .groups = "drop"
  )

# Long format for a side-by-side plot
na_long <- na_by_month %>%
  pivot_longer(cols = c(NAs_offset, NAs_beta1),
               names_to = "Which",
               values_to = "NA_Count") %>%
  mutate(Which = recode(Which,
                        NAs_offset = "2_average_offset",
                        NAs_beta1  = "2_average_b1"))

# 2) Side-by-side monthly bars (one bar per column per month) 
ggplot(na_long, aes(x = Month, y = NA_Count, fill = Which)) +
  geom_col(position = "dodge") +
  labs(
    title = "Monthly NA counts for averaged weekly coefficients",
    x = "Month",
    y = "Number of NAs",
    fill = "Column"
  ) +
  theme_minimal()

# ok we can see the number missing data points in month/year here. 

# rows before 12,267,466, and 50,169 should go - 
cldp_with_coefs_cleaned <- cldp_with_coefs %>%
  drop_na()

View(cldp_with_coefs_cleaned) # rows - 12,217,297
# Check if any NAs remain
colSums(is.na(cldp_with_coefs_cleaned)) # 0

################################# Second stage calibration of the entire dataset - 

# ok now add the accuracy_scaled_tmp 

cldp_with_coefs_cleaned <- cldp_with_coefs_cleaned %>%
  mutate(accuracy_scaled_tmp = round((`2_average_b1` * precision_scaled_tmp) + `2_average_offset`, 1))

# write.csv(cldp_with_coefs_cleaned, "full_calibration.csv", row.names = FALSE)

################################# 

# Accessing NOAA
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

View(colocation_md)
View(deployment_md)
View(full_dataset)
View(reference_data)
View(calibration_models)
View(Golden_nodes)
View(combined_sites)

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

# NOAA reference sites for external validation - 

View(deployment_md)

# Search for nearby stations (returns a dataframe of sites within 50 km radius)
meta_sites <- getMeta(lat = 51.5074, lon = -0.1272, n = 10)
# View metadata
View(meta_sites)

# Create a dataframe with the first 6 UKMO sites
ukmo_sites <- meta_sites[1:6, ]
ukmo_sites$SiteCode <- paste0("UKMO_", gsub(" ", "_", ukmo_sites$station))
ukmo_sites$SiteName <- ukmo_sites$station
ukmo_sites$SiteClassification <- "Reference UKMO"

# Add missing columns with NA or dummy values
ukmo_sites$HeadHeight <- NA
ukmo_sites$ToRoad <- NA

# Select and rename columns to match deployment_md
ukmo_sites <- ukmo_sites[, c("SiteCode", "SiteName", "latitude", "longitude", "SiteClassification", "HeadHeight", "ToRoad")]
colnames(ukmo_sites) <- c("SiteCode", "SiteName", "Latitude", "Longitude", "SiteClassification", "HeadHeight", "ToRoad")

# Bind to deployment_md
deployment_md_combined <- rbind(deployment_md, ukmo_sites)

View(deployment_md_combined)

leaflet() %>%
  addProviderTiles(providers$OpenStreetMap) %>%
  addCircleMarkers(
    data = deployment_md_combined[deployment_md_combined$SiteClassification != "Reference UKMO", ],
    lat = ~Latitude,
    lng = ~Longitude,
    color = "blue",
    radius = 4,
    label = ~SiteName,
    group = "Sensor Nodes"
  ) %>%
  addCircleMarkers(
    data = deployment_md_combined[deployment_md_combined$SiteClassification == "Reference UKMO", ],
    lat = ~Latitude,
    lng = ~Longitude,
    color = "red",
    radius = 6,
    label = ~SiteName,
    group = "UKMO Reference Sites"
  ) %>%
  addLayersControl(
    overlayGroups = c("Sensor Nodes", "UKMO Reference Sites"),
    options = layersControlOptions(collapsed = FALSE)
  )

# We need to isolate nodes which are near a reference site now - 

# Filter UKMO reference sites
ukmo_sites <- deployment_md_combined[deployment_md_combined$SiteClassification == "Reference UKMO", ]
# Filter non-UKMO sensor nodes
sensor_nodes <- deployment_md_combined[deployment_md_combined$SiteClassification != "Reference UKMO", ]
# Set distance threshold in meters (change this as needed)
distance_threshold <- 300  # meters
# Create an empty list to store results
ukmo_nearby_nodes <- list()

# Loop through each UKMO site and find nearby nodes
for (i in 1:nrow(ukmo_sites)) {
  ukmo_site <- ukmo_sites[i, ]
  site_coords <- c(ukmo_site$Longitude, ukmo_site$Latitude)
  
  # Compute distance from current UKMO site to all other nodes
  distances <- distHaversine(
    matrix(site_coords, nrow = 1),
    matrix(c(sensor_nodes$Longitude, sensor_nodes$Latitude), ncol = 2)
  )
  
  # Find indices of nodes within the threshold
  nearby_indices <- which(distances <= distance_threshold)
  
  # Store filtered table in a list with name of UKMO site
  site_name_clean <- gsub(" ", "_", ukmo_site$SiteName)
  ukmo_nearby_nodes[[site_name_clean]] <- sensor_nodes[nearby_indices, ]
}

View(ukmo_nearby_nodes)

# Basically we can test 3 nodes within the St James Park UKMO site - CLDP0009, CLDP0329, CLDP0495. 

# Need to first see whether I can extract UKMO temp data for St James Park. 

importNOAA(code = "037700-99999", year = 2021:2025, hourly = TRUE)
# Import data (you already did this, but including for clarity)
st_james_hourly <- importNOAA(code = "037700-99999", year = 2021:2025, hourly = TRUE)

# Filter for dates between 2021-01-01 and now
st_james_temp <- st_james_hourly %>%
  filter(date >= as.POSIXct("2021-01-01") & date <= Sys.time()) %>%
  select(date, air_temp)

View(st_james_temp)
# write.csv(st_james_temp, "st_james_air_temp_2021_2025.csv", row.names = FALSE)

########################
st_james_temp <- read.csv("Data/st_james_air_temp_2021_2025.csv")
View(st_james_temp)
#######################

# Missing data - 

# source table 
dat_stjames <- st_james_temp %>%
  mutate(date = lubridate::ymd_hms(date, tz = "UTC"))

# helper: fill hourly sequence 
fill_to_hourly <- function(df) {
  df <- df %>% arrange(date)
  
  full_time <- tibble(date = seq(min(df$date, na.rm = TRUE),
                                 max(df$date, na.rm = TRUE),
                                 by = "hour"))
  
  out <- full_time %>%
    left_join(df, by = "date")
  
  out
}

# create filled dataset 
stjames_filled <- fill_to_hourly(dat_stjames)

# helper: monthly % missing 
monthly_missing_pct <- function(df, varname, label) {
  df %>%
    mutate(Month = as.Date(floor_date(date, "month"))) %>%
    group_by(Month) %>%
    summarise(
      n_hours   = n(),
      n_missing = sum(is.na(.data[[varname]])),
      pct_missing = 100 * n_missing / n_hours,
      .groups = "drop"
    ) %>%
    mutate(node = label)
}

# summarise for St James Park 
m_stjames <- monthly_missing_pct(stjames_filled, "air_temp", "St James Park")

# plot 
p_stjames <- ggplot(m_stjames, aes(x = Month, y = pct_missing)) +
  geom_col(width = 28) +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y", expand = c(0.01, 0)) +
  scale_y_continuous(limits = c(0, 100), breaks = seq(0, 100, 10),
                     labels = scales::label_percent(accuracy = 1, scale = 1)) +
  labs(title = "St James Park – Monthly % Missing Temperature",
       x = NULL, y = "% missing (hourly)") +
  theme_minimal(base_size = 12) +
  theme(plot.title = element_text(face = "bold"))

# save to PDF
# pdf("StJamesPark_Monthly_MissingTemperature.pdf", width = 11, height = 8)
print(p_stjames)
dev.off()

########################### 

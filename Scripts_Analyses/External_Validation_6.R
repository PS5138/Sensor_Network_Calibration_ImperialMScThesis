# External Validation 
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
cldp_with_coefs_cleaned <- read.csv("data/full_calibration.csv")

View(colocation_md)
View(deployment_md)
View(full_dataset)
View(reference_data)
View(calibration_models)
View(Golden_nodes)
View(combined_sites)
View(st_james_temp)
View(cldp_with_coefs_cleaned)

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

cldp_with_coefs_cleaned <- cldp_with_coefs_cleaned %>%
  mutate(precision_scaled_tmp = round(precision_scaled_tmp, 1))

# for some reason when I upload - the 00:00:00 get removed but nothing else is changed
# Convert DateTime column to character for string manipulation
cldp_with_coefs_cleaned$DateTime <- as.character(cldp_with_coefs_cleaned$DateTime)
# Append "00:00:00" where time is missing (i.e., just a date is printed)
cldp_with_coefs_cleaned <- cldp_with_coefs_cleaned %>%
  mutate(DateTime = ifelse(nchar(DateTime) == 10,
                           paste0(DateTime, " 00:00:00"),
                           DateTime))
# Convert DateTime column to proper POSIXct object
cldp_with_coefs_cleaned$DateTime <- as.POSIXct(cldp_with_coefs_cleaned$DateTime, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")

############################################

# Now lets extract the 3 devices which are next to st james reference site
# CLDP0009, CLDP0329, CLDP0495 - need to check that they only have 1 device code 

View(combined_sites)

# checked manaually - CLDP0009 (one), CLDP0329 (one), CLDP0495 (one) - ok we are good!

################################# 

# First I want their deployed start and end dates and I want to quantify missing data. 

nodes <- c("CLDP0009", "CLDP0329", "CLDP0495")
df    <- cldp_with_coefs_cleaned            # <- your data frame from the screenshot

# Which value columns do you want carried across?
want_cols <- c("TMP", "True_Temp", "precision_scaled_tmp", "accuracy_scaled_tmp")
keep_cols <- intersect(want_cols, names(df))  

# 1) Start/end per node 
ranges <- df %>%
  filter(SiteCode %in% nodes) %>%
  group_by(SiteCode) %>%
  summarise(
    start = min(DateTime, na.rm = TRUE),
    end   = max(DateTime, na.rm = TRUE),
    .groups = "drop"
  )

print(ranges)

# 2) Helper to build hourly skeleton + join values 
build_hourly <- function(site_id) {
  r <- ranges %>% filter(SiteCode == site_id)
  start_hr <- floor_date(r$start, unit = "hour")
  end_hr   <- ceiling_date(r$end, unit = "hour")
  
  skeleton <- tibble(
    SiteCode = site_id,
    DateTime = seq(from = start_hr, to = end_hr, by = "1 hour")
  )
  
  values <- df %>%
    filter(SiteCode == site_id) %>%
    select(SiteCode, DateTime, all_of(keep_cols)) %>%
    distinct()                    # just in case there are dupes at the same hour
  
  out <- skeleton %>%
    left_join(values, by = c("SiteCode", "DateTime")) %>%
    arrange(DateTime)
  
  # quick NA summary for you
  na_summary <- out %>%
    summarise(
      n_rows = n(),
      across(all_of(keep_cols), ~sum(is.na(.x)), .names = "NA_{col}")
    )
  message(site_id, " — hourly rows: ", na_summary$n_rows,
          "; NAs -> ", paste(names(na_summary)[-1], na_summary[-1], collapse = ", "))
  
  out
}

# 3) Build the three hourly data frames 
cldp0009_hourly <- build_hourly("CLDP0009")
cldp0329_hourly <- build_hourly("CLDP0329")
cldp0495_hourly <- build_hourly("CLDP0495")

# Named list - 
hourly_list <- list(
  CLDP0009 = cldp0009_hourly,
  CLDP0329 = cldp0329_hourly,
  CLDP0495 = cldp0495_hourly
)

# View(cldp0009_hourly)

# helper: monthly % missing for accuracy_scaled_tmp 
monthly_missing_pct <- function(df, node_label) {
  df %>%
    mutate(Month = as.Date(floor_date(DateTime, "month"))) %>%
    group_by(Month) %>%
    summarise(
      n_hours    = n(),
      n_missing  = sum(is.na(accuracy_scaled_tmp)),
      pct_missing = 100 * n_missing / n_hours,
      .groups = "drop"
    ) %>%
    mutate(node = node_label)
}

# build summaries 
m0009 <- monthly_missing_pct(cldp0009_hourly, "CLDP0009")
m0329 <- monthly_missing_pct(cldp0329_hourly, "CLDP0329")
m0495 <- monthly_missing_pct(cldp0495_hourly, "CLDP0495")

# plotting helper 
plot_missing <- function(miss_df, title_label){
  ggplot(miss_df, aes(x = Month, y = pct_missing)) +
    geom_col(width = 28) +
    scale_x_date(date_breaks = "1 year", date_labels = "%Y", expand = c(0.01, 0)) +
    scale_y_continuous(limits = c(0, 100), breaks = seq(0, 100, 10),
                       labels = scales::label_percent(accuracy = 1, scale = 1)) +
    labs(title = paste(title_label, "– Monthly % Missing (accuracy_scaled_tmp)"),
         x = NULL, y = "% missing (hourly)") +
    theme_minimal(base_size = 12) +
    theme(plot.title = element_text(face = "bold"))
}

p0009 <- plot_missing(m0009, "CLDP0009")
p0329 <- plot_missing(m0329, "CLDP0329")
p0495 <- plot_missing(m0495, "CLDP0495")

# save all three in one PDF (top to bottom)
pdf("Monthly_Missing_AccuracyScaledTMP_ThreeNodes.pdf", width = 11, height = 10)
grid.arrange(p0009, p0329, p0495, ncol = 1, heights = c(1, 1, 1))
dev.off()

############################################

# We need to create a new table now where we have the following columns - 
# Date_time, CLDP0009_rawtmp, CLDP0009_PSTemp, CLDP009_ASTemp, StJames_tmp for each of the three 
# sitecodes - 

st_james_temp <- st_james_temp %>%
  mutate(
    date = as.character(date),
    date = ifelse(!grepl("\\d{2}:\\d{2}:\\d{2}$", date),  # If time is missing
                  paste0(date, " 00:00:00"),              # Append midnight
                  date),
    date = as.POSIXct(date, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")
  )

# CLDP0009 - 

# Extract relevant columns for CLDP0009
cldp_filtered_0009 <- cldp_with_coefs_cleaned %>%
  filter(SiteCode == "CLDP0009") %>%
  mutate(DateTime = as.POSIXct(DateTime, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")) %>%
  select(DateTime,
         CLDP0009_rawtmp = TMP,
         CLDP0009_PSTemp = precision_scaled_tmp,
         CLDP0009_ASTemp = accuracy_scaled_tmp)

# Merge with St James data
final_merge_0009 <- cldp_filtered_0009 %>%
  left_join(st_james_temp %>% # Keeps the full CLDP dataset intact 
              rename(DateTime = date,
                     StJames_tmp = air_temp),
            by = "DateTime")

View(final_merge_0009)

colSums(is.na(final_merge_0009)) # StJames_tmp is missing 1123

# Count NAs per month
nas_by_month <- final_merge_0009 %>%
  mutate(Month = floor_date(DateTime, "month")) %>%
  group_by(Month) %>%
  summarise(NA_count = sum(is.na(StJames_tmp)), .groups = "drop")

# Plot as a line chart
ggplot(nas_by_month, aes(x = Month, y = NA_count)) +
  geom_line(color = "red", size = 1) +
  geom_point(color = "red", size = 2) +
  labs(
    title = "Monthly count of missing St James temperature data",
    x = "Month",
    y = "Number of NAs"
  ) +
  theme_minimal()

# CLDP0329 - 

# Extract relevant columns for CLDP0329
cldp_filtered_0329 <- cldp_with_coefs_cleaned %>%
  filter(SiteCode == "CLDP0329") %>%
  mutate(DateTime = as.POSIXct(DateTime, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")) %>%
  select(DateTime,
         CLDP0329_rawtmp = TMP,
         CLDP0329_PSTemp = precision_scaled_tmp,
         CLDP0329_ASTemp = accuracy_scaled_tmp)

# Merge with St James data
final_merge_0329 <- cldp_filtered_0329 %>%
  left_join(st_james_temp %>% # Keeps the full CLDP dataset intact 
              rename(DateTime = date,
                     StJames_tmp = air_temp),
            by = "DateTime")

View(final_merge_0329)

colSums(is.na(final_merge_0329)) # StJames_tmp is missing 1364

# Count NAs per month
nas_by_month <- final_merge_0329 %>%
  mutate(Month = floor_date(DateTime, "month")) %>%
  group_by(Month) %>%
  summarise(NA_count = sum(is.na(StJames_tmp)), .groups = "drop")

# Plot as a line chart
ggplot(nas_by_month, aes(x = Month, y = NA_count)) +
  geom_line(color = "red", size = 1) +
  geom_point(color = "red", size = 2) +
  labs(
    title = "Monthly count of missing St James temperature data",
    x = "Month",
    y = "Number of NAs"
  ) +
  theme_minimal()

# CLDP0495 - 

# Extract relevant columns for CLDP0495
cldp_filtered_0495 <- cldp_with_coefs_cleaned %>%
  filter(SiteCode == "CLDP0495") %>%
  mutate(DateTime = as.POSIXct(DateTime, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")) %>%
  select(DateTime,
         CLDP0495_rawtmp = TMP,
         CLDP0495_PSTemp = precision_scaled_tmp,
         CLDP0495_ASTemp = accuracy_scaled_tmp)

# Merge with St James data
final_merge_0495 <- cldp_filtered_0495 %>%
  left_join(st_james_temp %>% # Keeps the full CLDP dataset intact 
              rename(DateTime = date,
                     StJames_tmp = air_temp),
            by = "DateTime")

View(final_merge_0495)

colSums(is.na(final_merge_0495)) # StJames_tmp is missing 761

# Count NAs per month
nas_by_month <- final_merge_0495 %>%
  mutate(Month = floor_date(DateTime, "month")) %>%
  group_by(Month) %>%
  summarise(NA_count = sum(is.na(StJames_tmp)), .groups = "drop")

# Plot as a line chart
ggplot(nas_by_month, aes(x = Month, y = NA_count)) +
  geom_line(color = "red", size = 1) +
  geom_point(color = "red", size = 2) +
  labs(
    title = "Monthly count of missing St James temperature data",
    x = "Month",
    y = "Number of NAs"
  ) +
  theme_minimal()

###########################
View(final_merge_0009)
View(final_merge_0329)
View(final_merge_0495)
###########################

# Next step is calculating the error between the raw, PS, and AS for each 3 of the nodes - 

# Function to calculate RMSE
rmse <- function(pred, obs) {
  sqrt(mean((pred - obs)^2, na.rm = TRUE))
}

# Function to calculate MAE
mae <- function(pred, obs) {
  mean(abs(pred - obs), na.rm = TRUE)
}

# Function to process one node's dataset
process_node <- function(df, node_name) {
  df %>%
    mutate(Week = floor_date(DateTime, "week")) %>%
    group_by(Week) %>%
    summarise(
      RMSE_raw = rmse(!!sym(paste0(node_name, "_rawtmp")), StJames_tmp),
      RMSE_PS  = rmse(!!sym(paste0(node_name, "_PSTemp")), StJames_tmp),
      RMSE_AS  = rmse(!!sym(paste0(node_name, "_ASTemp")), StJames_tmp),
      MAE_raw  = mae(!!sym(paste0(node_name, "_rawtmp")), StJames_tmp),
      MAE_PS   = mae(!!sym(paste0(node_name, "_PSTemp")), StJames_tmp),
      MAE_AS   = mae(!!sym(paste0(node_name, "_ASTemp")), StJames_tmp),
      .groups = "drop"
    ) %>%
    mutate(Node = node_name)
}

# Apply to each dataset
node_0009 <- process_node(final_merge_0009, "CLDP0009")
node_0329 <- process_node(final_merge_0329, "CLDP0329")
node_0495 <- process_node(final_merge_0495, "CLDP0495")

# Combine all nodes
weekly_errors <- bind_rows(node_0009, node_0329, node_0495)

View(weekly_errors)

# Overall error for each node and method
overall_errors <- weekly_errors %>%
  group_by(Node) %>%
  summarise(
    Overall_RMSE_raw = mean(RMSE_raw, na.rm = TRUE),
    Overall_RMSE_PS  = mean(RMSE_PS, na.rm = TRUE),
    Overall_RMSE_AS  = mean(RMSE_AS, na.rm = TRUE),
    Overall_MAE_raw  = mean(MAE_raw, na.rm = TRUE),
    Overall_MAE_PS   = mean(MAE_PS, na.rm = TRUE),
    Overall_MAE_AS   = mean(MAE_AS, na.rm = TRUE)
  )

View(overall_errors)

# plotting - 

rmse_as_plot <- weekly_errors %>%
  mutate(WeekDate = as.Date(Week)) %>%          # <-- coerce to Date
  filter(!is.na(RMSE_AS)) %>%
  ggplot(aes(x = WeekDate, y = RMSE_AS, color = Node)) +
  geom_line(size = 0.9, alpha = 0.9) +
  scale_x_date(date_breaks = "6 months", date_labels = "%Y-%m") +
  labs(title = "Weekly RMSE (Accuracy-Scaled) vs St James",
       x = "Week", y = "RMSE (°C)", color = "Node") +
  theme_minimal(base_size = 12) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

mae_as_plot <- weekly_errors %>%
  mutate(WeekDate = as.Date(Week)) %>%
  filter(!is.na(MAE_AS)) %>%
  ggplot(aes(x = WeekDate, y = MAE_AS, color = Node)) +
  geom_line(size = 0.9, alpha = 0.9) +
  scale_x_date(date_breaks = "6 months", date_labels = "%Y-%m") +
  labs(title = "Weekly MAE (Accuracy-Scaled) vs St James",
       x = "Week", y = "MAE (°C)", color = "Node") +
  theme_minimal(base_size = 12) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

print(rmse_as_plot)
print(mae_as_plot)

###########################

# Another plot for each of the 3 nodes to show improvement between raw_rmse/mae, 
# PS_rmse/mae, and AS_rmse/mae. 

# 1) Tidy into long format (Week -> Date, Metric/Category -> columns)
weekly_long <- weekly_errors %>%
  mutate(WeekDate = as.Date(Week)) %>%
  select(WeekDate, Node, starts_with("RMSE_"), starts_with("MAE_")) %>%
  pivot_longer(
    cols = -c(WeekDate, Node),
    names_to = c("Metric", "Category"),
    names_sep = "_",
    values_to = "Error"
  ) %>%
  filter(!is.na(Error)) %>%
  mutate(
    Metric   = factor(Metric,   levels = c("RMSE", "MAE")),
    Category = factor(Category, levels = c("raw", "PS", "AS"),
                      labels = c("RAW", "PS", "AS"))
  )

# Helper to draw one node, one metric (RMSE or MAE)
plot_node_metric <- function(node_id, metric = c("RMSE","MAE")) {
  metric <- match.arg(metric)
  weekly_long %>%
    filter(Node == node_id, Metric == metric) %>%
    ggplot(aes(x = WeekDate, y = Error, color = Category)) +
    geom_hline(yintercept = 0, color = "black", size = 1.2) +
    geom_line(size = 0.95, alpha = 0.9) +
    scale_color_manual(values = c("RAW" = "#D55E00", "PS" = "#0072B2", "AS" = "#009E73")) +
    scale_x_date(date_breaks = "6 months", date_labels = "%Y-%m") +
    labs(
      title = paste0("Weekly ", metric, " — ", node_id),
      x = "Week",
      y = paste0(metric, " (°C)"),
      color = "Series"
    ) +
    theme_minimal(base_size = 12) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
}

# Build the six plots (3 nodes × 2 metrics)
p_0009_rmse <- plot_node_metric("CLDP0009", "RMSE")
p_0009_mae  <- plot_node_metric("CLDP0009", "MAE")

p_0329_rmse <- plot_node_metric("CLDP0329", "RMSE")
p_0329_mae  <- plot_node_metric("CLDP0329", "MAE")

p_0495_rmse <- plot_node_metric("CLDP0495", "RMSE")
p_0495_mae  <- plot_node_metric("CLDP0495", "MAE")

# Show them
print(p_0009_rmse)
print(p_0009_mae)
print(p_0329_rmse)
print(p_0329_mae)
print(p_0495_rmse)
print(p_0495_mae)

# Need to save the above into a pdf - 

# long form, RMSE only, with display labels to match your figure 
rmse_long <- weekly_errors %>%
  mutate(WeekDate = as.Date(Week)) %>%
  select(WeekDate, Node, starts_with("RMSE_")) %>%
  pivot_longer(
    cols = -c(WeekDate, Node),
    names_to   = c("Metric", "Category"),
    names_sep  = "_",
    values_to  = "RMSE"
  ) %>%
  filter(!is.na(RMSE)) %>%
  mutate(
    Category = factor(Category,
                      levels = c("PS", "AS", "raw"),
                      labels = c("1st Stage Calibrated vs Reference",
                                 "2nd Stage Calibrated vs Reference",
                                 "Uncalibrated vs Reference")
    )
  )

# styling palette/helper for a single node plot
pal <- c(
  "1st Stage Calibrated vs Reference"  = "#F26C6C",  # red
  "2nd Stage Calibrated vs Reference"  = "black",    # black
  "Uncalibrated vs Reference"          = "#21A3A3"   # teal
)

plot_rmse_node <- function(node_id, nice_title = NULL){
  nice_title <- nice_title %||% node_id
  rmse_long %>%
    filter(Node == node_id) %>%
    ggplot(aes(x = WeekDate, y = RMSE, colour = Category)) +
    geom_line(linewidth = 0.9, alpha = 0.95) +
    geom_point(size = 1.2, alpha = 0.9) +
    scale_color_manual(values = pal) +
    scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
    labs(
      title = paste0("Weekly Root Mean Squared Error (RMSE) – ", nice_title),
      x = "Year", y = "RMSE (°C)", color = NULL
    ) +
    theme_minimal(base_size = 12) +
    theme(
      legend.position = "right",
      axis.text.x = element_text(angle = 0, hjust = 0.5)
    )
}

# build the three node plots 
p_0009 <- plot_rmse_node("CLDP0009")
p_0329 <- plot_rmse_node("CLDP0329")
p_0495 <- plot_rmse_node("CLDP0495")

# save all three in one PDF
pdf("Weekly_RMSE_ThreeNodes.pdf", width = 8.5, height = 11)
gridExtra::grid.arrange(p_0009, p_0329, p_0495, ncol = 1, heights = c(1,1,1))
dev.off()

###########################

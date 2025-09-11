# First stage calibration
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

########################### check how much missing data we have for each SiteCode

colSums(is.na(calibration_models))

sitecode_summary <- calibration_models %>%
  group_by(SiteCode) %>%
  summarise(
    Total_Records = n(),
    Missing_True_Temp = sum(is.na(True_Temp)),
    Percent_Missing = round((Missing_True_Temp / Total_Records) * 100, 2)
  ) %>%
  arrange(desc(Percent_Missing))  # Optional: sort by missingness

# View the summary table
View(sitecode_summary) 

length(unique(calibration_models$SiteCode)) # 679

########################### spread of temperature values

View(calibration_models)

# 1) Long format with clear labels and order
twobox <- calibration_models %>%
  transmute(
    `Sensor (TMP)` = TMP,
    `Reference (HOP True_Temp)` = True_Temp
  ) %>%
  pivot_longer(everything(), names_to = "Source", values_to = "Temperature") %>%
  mutate(Source = factor(Source, levels = c("Sensor (TMP)", "Reference (HOP True_Temp)")))

# 2) One plot, two boxplots sharing the y-axis
p <- ggplot(twobox, aes(x = Source, y = Temperature)) +
  geom_boxplot(fill = "skyblue", outlier.shape = 21, outlier.color = "red") +
  labs(
    title = "Temperature Distributions During Co-location: Sensor Nodes vs Reference",
    x = NULL,
    y = "Temperature (°C)"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(size = 14, face = "bold"),
    axis.text.x = element_text(size = 12)
  )

# 3) Save as PDF
pdf("Boxplot_Sensor_vs_Reference_SINGLE.pdf", width = 8, height = 6)
print(p)
dev.off()

########################### 

# We need to look into DeviceCode and take that as the unique identifier rather than CLDP or CLCA 

# Step 1: Get all unique CLDP SiteCodes from cldp_data_only
unique_cldp_sites <- cldp_data_only %>%
  pull(SiteCode) %>%
  unique()

length(unique_cldp_sites) # 634

# View(filtered_combined_sites)

length(unique(combined_sites$DeviceCode)) # 681 unique device codes
length(unique(combined_sites$SiteCode_CLCA)) # 681 unique CLCA
length(unique(combined_sites$SiteCode_CLDP)) # 618 unique CLDP

# Ok so every unique device code has a unique CLCA sitecode but CLDP sitecodes are dupicated. In essence, 1 
# CLCA and device code corresponds to multiple CLDP sometimes. 

# also CLDP data has 634 unique sites so some will have to go as our lookup only has 618 
# we got rid of some CLDP sites due to insufficient CLCA data in another script 

length(unique(calibration_models$SiteCode)) # 679 (missing 2 golden nodes)
length(unique(calibration_models$DeviceCode)) # 679 (missing 2 golden nodes)

filtered_calibration_models <- calibration_models # just for downstream analysis.

########################### 

# ok so the first step needs to be precision scaling based off the colocation data. Filtered_calibration_
# models is our dataset here. 

# CLDP0029/CLCA0014/AFZHPFH3 and CLDP0030/CLCA0072/ARNKZ5XY are the golden nodes

filtered_clean <- filtered_calibration_models

length(unique(filtered_clean$DeviceCode)) # 679
View(filtered_clean)

# First lets rename some of these columns - 
filtered_clean <- filtered_clean %>%
  rename(sensor_raw_tmp = TMP)

colSums(is.na(filtered_clean))

# Now lets fit individual linear regressions (there should be 679) and then add a new column to filtered_clean
# with precision_scaled_tmp - 

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
      x = "Uncalibrated Temperature",
      y = "Reference Temperature"
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

View(filtered_clean)

filtered_clean <- filtered_clean %>%
  mutate(precision_scaled_tmp = round(precision_scaled_tmp, 1))  # round to 1 dp

length(unique(coefs_df$DeviceCode)) # 679 devices have been calibrated (precision)

# How many points were used for each node?

# 1) Count usable points per device (rows where both inputs exist)
per_device_counts <- filtered_clean %>%
  filter(!is.na(True_Temp), !is.na(sensor_raw_tmp)) %>%
  count(DeviceCode, name = "n_points")

# 2) Keep only the devices we actually calibrated (the 679 in coefs_df)
calibrated_devices <- unique(coefs_df$DeviceCode)
per_device_counts_cal <- per_device_counts %>%
  filter(DeviceCode %in% calibrated_devices)

View(per_device_counts_cal)

# 3) One boxplot showing the spread across the 679 devices
p_box <- ggplot(per_device_counts_cal, aes(x = "", y = n_points)) +
  geom_boxplot(outlier.shape = NA, width = 0.25, fill = "#bcd7ff") +
  geom_jitter(width = 0.08, alpha = 0.25, size = 0.8) +
  labs(
    title = "Data points used per CLCA device (calibrated set)",
    x = NULL, y = "Number of usable data points"
  ) +
  theme_minimal(base_size = 12) +
  theme(axis.text.x = element_blank())

print(p_box)

# 4) Quick summary stats
summary_stats <- per_device_counts_cal %>%
  summarise(
    devices = n(),
    min = min(n_points),
    q1  = quantile(n_points, 0.25),
    median = median(n_points),
    mean = mean(n_points),
    q3  = quantile(n_points, 0.75),
    max = max(n_points)
  )
print(summary_stats) # median is 385 points

# Ok now we need some more error metric to evaluate the 1st stage calibration 
# R-squared and RMSE and MAE on the device, don't do cross val 

# Calculate R-squared, RMSE, MAE for each calibrated device 
metrics_list <- list()

for (i in seq_along(device_ids)) {
  device <- device_ids[i]
  df <- filtered_clean %>%
    filter(DeviceCode == device, !is.na(True_Temp), !is.na(sensor_raw_tmp))
  
  if (nrow(df) < 10) next  # skip small datasets
  
  model <- lm(True_Temp ~ sensor_raw_tmp, data = df)
  preds <- predict(model, newdata = df)
  
  r2_val   <- summary(model)$r.squared
  rmse_val <- rmse(df$True_Temp, preds)
  mae_val  <- mae(df$True_Temp, preds)
  
  metrics_list[[device]] <- data.frame(
    DeviceCode = device,
    R2   = r2_val,
    RMSE = rmse_val,
    MAE  = mae_val
  )
}

metrics_df <- do.call(rbind, metrics_list)

View(metrics_df) 

# Example: 2 d.p. for R2, 2 d.p. for RMSE/MAE
metrics_rounded <- metrics_df %>%
  mutate(
    R2   = round(R2,   2),
    RMSE = round(RMSE, 2),
    MAE  = round(MAE,  2)
  )

# Drop the first column explicitly by index
metrics_clean <- metrics_rounded[, -1]

# write.csv(metrics_clean, "Co-Location Device Summary.csv", na = "")

# Helper to make one minimalist boxplot
plot_metric_box <- function(metric_name, y_label) {
  ggplot(metrics_df, aes(x = "", y = .data[[metric_name]])) +
    geom_boxplot(width = 0.25, fill = "#bcd7ff", outlier.shape = NA) +
    labs(
      title = paste0(metric_name, " per CLCA device (calibrated set)"),
      x = NULL,
      y = y_label
    ) +
    theme_minimal(base_size = 12) +
    theme(
      axis.text.x  = element_blank(),
      axis.title.x = element_blank()
    )
}

p_r2   <- plot_metric_box("R2",   "R²")
p_rmse <- plot_metric_box("RMSE", "RMSE (°C)")
p_mae  <- plot_metric_box("MAE",  "MAE (°C)")

# Save all three next to each other in one PDF
pdf("Boxplots_R2_RMSE_MAE_side_by_side.pdf", width = 12, height = 4.5)
grid.arrange(p_r2, p_rmse, p_mae, ncol = 3)
dev.off()

# Summary statistics 
summary_stats_all <- function(metric) {
  metrics_df %>%
    summarise(
      devices = n(),
      min     = min(.data[[metric]]),
      q1      = quantile(.data[[metric]], 0.25),
      median  = median(.data[[metric]]),
      mean    = mean(.data[[metric]]),
      q3      = quantile(.data[[metric]], 0.75),
      max     = max(.data[[metric]])
    )
}

summary_r2   <- summary_stats_all("R2")
summary_rmse <- summary_stats_all("RMSE")
summary_mae  <- summary_stats_all("MAE")

print(summary_r2)
print(summary_rmse)
print(summary_mae)

# check that the r2 is the same as before - 

mean_r2_from_filtered <- filtered_clean %>%
  group_by(DeviceCode) %>%
  summarise(R2_unique = unique(R2)) %>%  # one R² per device
  summarise(mean_R2 = mean(R2_unique, na.rm = TRUE))

print(mean_r2_from_filtered) # ok it matches 

# Now need to show residuals vs fitted plots - 

# Build residuals + fitted for every device
make_residuals_df <- function(dev_id) {
  df <- filtered_clean %>%
    filter(DeviceCode == dev_id, !is.na(True_Temp), !is.na(sensor_raw_tmp))
  if (nrow(df) < 10) return(NULL)
  
  m <- lm(True_Temp ~ sensor_raw_tmp, data = df)
  tibble(
    DeviceCode = dev_id,
    fitted     = fitted(m),
    resid      = resid(m),
    std_resid  = rstandard(m)  # standardized residuals
  )
}

device_ids <- unique(filtered_clean$DeviceCode)
resid_list <- lapply(device_ids, make_residuals_df)
resid_df   <- bind_rows(resid_list)

# Safety check
if (nrow(resid_df) == 0) stop("No residuals available (after skipping devices with <10 rows).")

# Save pooled (all devices) residuals vs fitted plot
p_all <- ggplot(resid_df, aes(x = fitted, y = resid)) +
  geom_hline(yintercept = 0, linetype = 2, linewidth = 0.3) +
  geom_point(alpha = 0.08, size = 0.8) +
  geom_smooth(se = FALSE, linewidth = 0.6, method = "loess", span = 0.8) +
  labs(
    title = "Residuals vs Fitted (all devices pooled)",
    x = "Fitted (°C)",
    y = "Residual (°C)"
  ) +
  theme_minimal(base_size = 12)

pdf("Residuals_vs_Fitted_ALL.pdf", width = 8, height = 6)
print(p_all)
dev.off()

# Per-device residuals vs fitted, 4 plots per page (2 x 2)
plot_resid_device <- function(dev_id) {
  d <- filter(resid_df, DeviceCode == dev_id)
  ggplot(d, aes(x = fitted, y = resid)) +
    geom_hline(yintercept = 0, linetype = 2, linewidth = 0.3) +
    geom_point(alpha = 0.4, size = 0.9) +
    geom_smooth(se = FALSE, linewidth = 0.6, method = "loess", span = 0.8) +
    labs(title = dev_id, x = "Fitted (°C)", y = "Residual (°C)") +
    theme_minimal(base_size = 10)
}

pdf("Residuals_vs_Fitted_per_device.pdf", width = 10, height = 8)
# stream in chunks of 4 to avoid holding 700 plots in memory
ids <- unique(resid_df$DeviceCode)
for (i in seq(1, length(ids), by = 4)) {
  slice_ids <- ids[i:min(i+3, length(ids))]
  grobs <- lapply(slice_ids, plot_resid_device)
  grid.arrange(grobs = grobs, ncol = 2, nrow = 2)
}
dev.off()

# Heat map for residuals aggregated - 

# warm “heat” palette (starts at white)
cols <- grDevices::colorRampPalette(
  c("white", "#ffffe5", "#fee391", "#fe9929", "#d95f0e", "#993404")
)(256)

p_all_heat <- ggplot(resid_df, aes(fitted, resid)) +
  stat_density_2d(
    aes(fill = after_stat(..count.. / max(..count..))),   # % of total points
    geom    = "raster",
    contour = FALSE,
    n       = 250
  ) +
  scale_fill_gradientn(
    colours = cols,
    name    = "Percentage",
    limits  = c(0, 1),                             # 0–100%
    labels  = scales::label_percent(accuracy = 1),
    oob     = scales::squish,
    na.value= "white",
    guide   = guide_colorbar()
  ) +
  geom_hline(yintercept = 0, linetype = 2, linewidth = 0.3) +
  geom_smooth(se = FALSE, linewidth = 0.6, method = "loess", span = 0.8,
              colour = "black") +
  labs(title = "Residuals vs Fitted (all devices pooled)",
       x = "Fitted (°C)", y = "Residual (°C)") +
  theme_minimal(base_size = 12) +
  theme(legend.position = "right")

pdf("Residuals_vs_Fitted_ALL_HEAT.pdf", width = 8, height = 6)
print(p_all_heat)
dev.off()

##########################################

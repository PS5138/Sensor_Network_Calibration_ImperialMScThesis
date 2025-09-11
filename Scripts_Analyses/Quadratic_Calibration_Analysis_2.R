# Building non-linear models (exploratory)
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

View(calibration_models)

########################### 

filtered_calibration_models <- calibration_models # just for downstream analysis.

########################### Ok in this script we will add non-linear components to establish whether
# that is better than the linear regression approach

# simple metrics 
rmse_vec <- function(truth, pred) sqrt(mean((truth - pred)^2))
mae_vec  <- function(truth, pred) mean(abs(truth - pred))
r2_vec   <- function(truth, pred) cor(truth, pred)^2

# All unique devices
device_ids <- unique(filtered_calibration_models$DeviceCode)

results_list <- vector("list", length(device_ids))
names(results_list) <- device_ids

for (device in device_ids) {
  df <- filtered_calibration_models %>%
    filter(DeviceCode == device) %>%
    select(TMP, True_Temp) %>%
    tidyr::drop_na()
  
  # skip if not enough data or no variation
  if (nrow(df) < 10 || dplyr::n_distinct(df$TMP) < 2) {
    results_list[[device]] <- data.frame(
      DeviceCode = device, RMSE = NA_real_, MAE = NA_real_, R2 = NA_real_
    )
    next
  }
  
  # Fit on ALL data for this device (quadratic)
  fit <- try(lm(True_Temp ~ TMP + I(TMP^2), data = df), silent = TRUE)
  if (inherits(fit, "try-error")) {
    results_list[[device]] <- data.frame(
      DeviceCode = device, RMSE = NA_real_, MAE = NA_real_, R2 = NA_real_
    )
    next
  }
  
  preds <- predict(fit, newdata = df)
  
  results_list[[device]] <- data.frame(
    DeviceCode = device,
    RMSE = rmse_vec(df$True_Temp, preds),
    MAE  = mae_vec(df$True_Temp, preds),
    R2   = r2_vec(df$True_Temp, preds)   # equivalent to summary(fit)$r.squared here
  )
}

# Combine to one table
results_df <- dplyr::bind_rows(results_list)

# Overall means (ignoring devices that failed/NA)
overall_summary <- results_df %>%
  summarise(
    Mean_RMSE = mean(RMSE, na.rm = TRUE),
    Mean_MAE  = mean(MAE,  na.rm = TRUE),
    Mean_R2   = mean(R2,   na.rm = TRUE)
  )

# Inspect
print(overall_summary)
View(results_df)   # 679

# Not really that different from linear approach (improvement to RMSE and MAE of 0.01 and R-squared is the same.)

########################### Visualisation of fitted vs residual plots

# 1. Residuals vs Fitted for Quadratic Model (Linearity + Homoscedasticity)
# Get unique DeviceCodes
device_ids <- unique(filtered_calibration_models$DeviceCode)
# Residuals vs Fitted plots (Quadratic Model)
residual_plots_quad <- list()

for (device in device_ids) {
  df <- filtered_calibration_models %>%
    filter(DeviceCode == device) %>%
    select(TMP, True_Temp) %>%
    na.omit()
  
  if (nrow(df) < 10) next
  
  # Fit quadratic model
  model <- lm(True_Temp ~ TMP + I(TMP^2), data = df)
  
  # Compute fitted values and residuals
  df <- df %>%
    mutate(
      fitted = predict(model, newdata = df),
      resid = True_Temp - fitted
    )
  
  # Generate plot
  p <- ggplot(df, aes(x = fitted, y = resid)) +
    geom_point(alpha = 0.5, color = "black") +
    geom_hline(yintercept = 0, color = "red", linetype = "dashed") +
    labs(
      title = paste("Device:", device),
      x = "Fitted Values (Quadratic)",
      y = "Residuals"
    ) +
    theme_minimal(base_size = 8)
  
  residual_plots_quad[[device]] <- p
}

# Plot (subset of 12 devices for review, e.g., devices 100 to 112)
grid.arrange(grobs = residual_plots_quad[1:12], ncol = 3)
# Introducing a quadratic term visibly improved the calibration for many devices, 
# flattening residual trends and reducing systematic bias. Although the overall gains in RMSE, MAE, and R-squared
# were modest (0.01), the quadratic model more faithfully captured the underlying sensor-reference 
# temperature relationship, particularly for devices showing nonlinear behaviour. 

########################### 

# Quadratic visualisation for pdf - 

filtered_clean <- filtered_calibration_models

# First lets rename some of these columns - 
filtered_clean <- filtered_clean %>%
  rename(sensor_raw_tmp = TMP)

# Now lets fit individual linear regressions (there should be 679) and then add a new column to filtered_clean
# with precision_scaled_tmp - 

device_ids <- unique(filtered_clean$DeviceCode)

plot_list <- list()
all_coefs  <- list()

for (device in device_ids) {
  df <- filtered_clean %>%
    filter(DeviceCode == device,
           !is.na(True_Temp),
           !is.na(sensor_raw_tmp))
  
  if (nrow(df) < 10) next
  
  # Quadratic model
  mod <- lm(True_Temp ~ sensor_raw_tmp + I(sensor_raw_tmp^2), data = df)
  s   <- summary(mod)
  
  b0 <- unname(coef(mod)[1])
  b1 <- unname(coef(mod)[2])
  b2 <- unname(coef(mod)[3])
  
  # Store coefficients (rounded for the table)
  all_coefs[[device]] <- data.frame(
    DeviceCode = device,
    Intercept  = round(b0, 3),
    Linear     = round(b1, 3),
    Quadratic  = round(b2, 3),
    R2         = round(s$r.squared, 3)
  )
  
  # Pretty equation for the subtitle
  eqn <- sprintf("y = %.3f + %.3f·x %s %.3f·x²   |   R² = %.3f",
                 b0, b1,
                 ifelse(b2 >= 0, "+", "−"),
                 abs(b2), s$r.squared)
  
  # Plot with QUADRATIC smoother
  p <- ggplot(df, aes(x = sensor_raw_tmp, y = True_Temp)) +
    geom_point(alpha = 0.35) +
    stat_smooth(method = "lm",
                formula = y ~ poly(x, 2, raw = TRUE),
                se = FALSE, color = "blue") +
    labs(title = paste0("Device: ", device),
         subtitle = eqn,
         x = "Uncalibrated temperature (°C)",
         y = "Reference temperature (°C)") +
    theme_minimal(base_size = 10)
  
  plot_list[[length(plot_list) + 1]] <- p
}

# Save plots to a multipage PDF (2 columns, 6 per page)
pdf("Device_Regression_Lines_Quadratic.pdf", width = 9, height = 6)
for (i in seq(1, length(plot_list), by = 6)) {
  grid.arrange(grobs = plot_list[i:min(i + 5, length(plot_list))], ncol = 2)
}
dev.off()

# Save coefficients table
coef_df <- dplyr::bind_rows(all_coefs)
# write.csv(coef_df, "CoLocation_Quadratic_Coefficients.csv")

# Calculate R-squared, RMSE, MAE for each calibrated device 
metrics_list <- list()

for (i in seq_along(device_ids)) {
  device <- device_ids[i]
  df <- filtered_clean %>%
    filter(DeviceCode == device, !is.na(True_Temp), !is.na(sensor_raw_tmp))
  
  if (nrow(df) < 10) next  # skip small datasets
  
  model <- lm(True_Temp ~ sensor_raw_tmp + I(sensor_raw_tmp^2), data = df)
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

# Example: 3 d.p. for R2, 2 d.p. for RMSE/MAE
metrics_rounded <- metrics_df %>%
  mutate(
    R2   = round(R2,   2),
    RMSE = round(RMSE, 2),
    MAE  = round(MAE,  2)
  )

# Drop the first column explicitly by index
metrics_clean <- metrics_rounded[, -1]

# write.csv(metrics_clean, "Co-Location Device Summary - Quadratic Regression.csv", na = "")

###############################################
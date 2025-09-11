# Golden nodes analyses (2nd stage calibration)
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
library(BlandAltmanLeh)
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

###########################################

filtered_calibration_models <- calibration_models # just for downstream analysis.
filtered_clean <- filtered_calibration_models

filtered_clean <- filtered_clean %>%
  rename(sensor_raw_tmp = TMP)

########################### Golden nodes 

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

# How many data points do we have for each node?
Golden_nodes %>%
  group_by(SiteCode.x) %>%
  summarise(
    actual_obs = n(),
    start_time = min(DateTime),
    end_time = max(DateTime),
    expected_obs = as.numeric(difftime(max(DateTime), min(DateTime), units = "hours")) + 1
  )
# ok so we basically have all the observations

########################### 

# 2nd step is accuracy scaling where we use the golden nodes for further adjustment...

# one way of exploring this is first to look at how many days of data do we normally have to perform 
# collocation. I dont mean ideally but actual days of data points used in the precision step. We can plot
# this - 

# Create a Date column
filtered_clean$Date <- as.Date(filtered_clean$DateTime)
# Filter out rows where sensor_raw_tmp is NA
non_na_data <- filtered_clean %>%
  filter(!is.na(sensor_raw_tmp))

View(non_na_data)  

# Count distinct days per device
daily_counts <- non_na_data %>%
  group_by(DeviceCode) %>%
  summarise(non_na_days = n_distinct(Date)) %>%
  ungroup()

summary(daily_counts$non_na_days) # 17 is the median number of days for collocation

# Save as PDF
pdf("Boxplot_CoLocation_Days.pdf", width = 8, height = 6)

ggplot(daily_counts, aes(y = non_na_days)) +
  geom_boxplot(
    fill = "skyblue",
    outlier.shape = 21,
    outlier.colour = "red",
    outlier.fill = "red"
  ) +
  labs(
    title = "Distribution of Co-Location Days of Sensor Data per Device",
    y = "Number of Valid Days"
  ) +
  scale_y_continuous(limits = c(0, 120)) +  # Strict y-axis bounds
  theme_minimal() +
  theme(
    axis.title.x = element_blank(),
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank()
  )

dev.off()

# ok so thats a nice plot where we can see the sensor distribution for collocation days
# If we take the median as 17 (ensure justification) - then we can train both the golden nodes on 17 days 
# worth of actual data points and then apply in chunks of 1 month to see how the RMSE holds up. 

View(Golden_nodes)

# before removing NAs show a line plot or something to track missing data (needs to be contextual though)

dat_golden <- golden_clean

# Ensure POSIXct and pick a timezone (change "UTC" if you have a known tz)
dat_golden <- dat_golden %>%
  mutate(DateTime = lubridate::ymd_hms(DateTime, tz = "UTC"))

fill_to_hourly <- function(df) {
  # ensure ordered timestamps
  df <- df %>% arrange(DateTime)
  
  # full hourly sequence from first to last observed time
  full_time <- tibble(DateTime = seq(min(df$DateTime, na.rm = TRUE),
                                     max(df$DateTime, na.rm = TRUE),
                                     by = "hour"))
  
  # join so original rows are preserved, gaps become new NA rows
  out <- full_time %>%
    left_join(df, by = "DateTime") %>%
    mutate(
      SiteCode.x = tidyr::replace_na(SiteCode.x, unique(df$SiteCode.x))
    ) %>%
    relocate(SiteCode.x, .before = DateTime)
  
  out
}

# Split → fill → recombine
gold_0029_filled <- dat_golden %>%
  filter(SiteCode.x == "CLDP0029") %>%
  fill_to_hourly()

gold_0030_filled <- dat_golden %>%
  filter(SiteCode.x == "CLDP0030") %>%
  fill_to_hourly()

View(gold_0029_filled)
View(gold_0030_filled)

# Now plotting percentage of missing NAs per month over the years for both nodes 

#--- helper: monthly % missing for a filled hourly series ---
monthly_missing_pct <- function(df, node_label) {
  df %>%
    mutate(Month = as.Date(floor_date(DateTime, "month"))) %>% 
    group_by(Month) %>%
    summarise(
      n_hours   = n(),
      n_missing = sum(is.na(TMP)),
      pct_missing = 100 * n_missing / n_hours,
      .groups = "drop"
    ) %>%
    mutate(node = node_label)
}

# 1) Summaries for each golden node
m29 <- monthly_missing_pct(gold_0029_filled, "CLDP0029")
m30 <- monthly_missing_pct(gold_0030_filled, "CLDP0030")

View(m29)

# 2) Plots (one per node)
p29 <- ggplot(m29, aes(x = Month, y = pct_missing)) +
  geom_col(width = 28) +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y", expand = c(0.01, 0)) +
  scale_y_continuous(limits = c(0, 100), breaks = seq(0, 100, 10),
                     labels = scales::label_percent(accuracy = 1, scale = 1)) +
  labs(title = "AFZHPFH3  – Monthly % Missing TMP",
       x = NULL, y = "% missing (hourly)") +
  theme_minimal(base_size = 12) +
  theme(plot.title = element_text(face = "bold"))

p30 <- ggplot(m30, aes(x = Month, y = pct_missing)) +
  geom_col(width = 28) +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y", expand = c(0.01, 0)) +
  scale_y_continuous(limits = c(0, 100), breaks = seq(0, 100, 10),
                     labels = scales::label_percent(accuracy = 1, scale = 1)) +
  labs(title = "ARNKZ5XY – Monthly % Missing Temperature",
       x = NULL, y = "% missing (hourly)") +
  theme_minimal(base_size = 12) +
  theme(plot.title = element_text(face = "bold"))

# 3) Save both to a single PDF (top/bottom)
pdf("GoldenNodes_Monthly_MissingTMP_Percent.pdf", width = 11, height = 8)
grid.arrange(p29, p30, ncol = 1, heights = c(1, 1))
dev.off()

##################################################

# Filter and clean data
golden_clean <- Golden_nodes %>%
  filter(!is.na(TMP), !is.na(True_Temp)) %>%
  mutate(Date = as.Date(DateTime)) %>%
  arrange(SiteCode.x, DateTime)

View(golden_clean)

################################################

fit_first_17days <- function(data, node_id) {
  df <- data %>%
    filter(SiteCode.x == node_id) %>%
    mutate(Date = as.Date(DateTime)) %>%
    arrange(DateTime)
  
  start_date <- min(df$Date, na.rm = TRUE)
  end_date   <- start_date + days(16)     # 17-day window (inclusive)
  
  sub <- df %>%
    filter(Date >= start_date, Date <= end_date) %>%
    tidyr::drop_na(TMP, True_Temp)
  
  if (nrow(sub) < 3) {
    return(tibble(
      node = node_id, start_date, end_date,
      n = nrow(sub), RMSE = NA_real_, MAE = NA_real_, R2 = NA_real_
    ))
  }
  
  m     <- lm(True_Temp ~ TMP, data = sub)
  pred  <- predict(m, newdata = sub)
  
  tibble(
    node       = node_id,
    start_date = start_date,
    end_date   = end_date,
    n          = nrow(sub),
    RMSE       = rmse(sub$True_Temp, pred),
    MAE        = mae(sub$True_Temp, pred),
    R2         = cor(sub$True_Temp, pred)^2
  )
}

# Run for both golden nodes and bind into one summary row per node
results_17day <- bind_rows(
  fit_first_17days(golden_clean, "CLDP0029"),
  fit_first_17days(golden_clean, "CLDP0030")
)

print(results_17day)

# Visualisation - 

# helper: first 17-day window for a node (inclusive)
get_17day_window <- function(df, node_id) {
  d0 <- df %>%
    filter(SiteCode.x == node_id) %>%
    mutate(Date = as.Date(DateTime)) %>%
    arrange(DateTime)
  
  start <- min(d0$Date, na.rm = TRUE)
  end   <- start + days(16)  # 17 days inclusive
  
  sub <- d0 %>%
    filter(Date >= start, Date <= end) %>%
    drop_na(TMP, True_Temp)
  
  list(sub = sub, start = start, end = end)
}

# build windows
w29 <- get_17day_window(golden_clean, "CLDP0029")
w30 <- get_17day_window(golden_clean, "CLDP0030")

# fit models (for the lines)
m29 <- lm(True_Temp ~ TMP, data = w29$sub)
m30 <- lm(True_Temp ~ TMP, data = w30$sub)

# plots
p29 <- ggplot(w29$sub, aes(TMP, True_Temp)) +
  geom_point(alpha = 0.25, size = 0.6) +
  geom_smooth(method = "lm", se = FALSE, colour = "steelblue") +
  labs(
    title = paste0("CLDP0029: first 17 days (", w29$start, " to ", w29$end, ")"),
    x = "Sensor TMP (°C)",
    y = "Reference temperature (°C)"
  ) +
  theme_minimal(base_size = 12)

p30 <- ggplot(w30$sub, aes(TMP, True_Temp)) +
  geom_point(alpha = 0.25, size = 0.6) +
  geom_smooth(method = "lm", se = FALSE, colour = "steelblue") +
  labs(
    title = paste0("CLDP0030: first 17 days (", w30$start, " to ", w30$end, ")"),
    x = "Sensor TMP (°C)",
    y = "Reference temperature (°C)"
  ) +
  theme_minimal(base_size = 12)

# save both in one PDF (vertical stack)
pdf("GoldenNodes_First17Days_OLS_2up.pdf", width = 9, height = 10)
grid.arrange(p29, p30, ncol = 1)
dev.off()

##################################################

# Function to fit on first 17 days, apply monthly, and compute RMSE
calculate_monthly_rmse <- function(df) {
  site <- unique(df$SiteCode.x)
  
  # Subset to first 17 days
  first_day <- min(df$Date)
  last_day <- first_day + days(16)
  train_data <- df %>% filter(Date >= first_day & Date <= last_day)
  
  model <- lm(True_Temp ~ TMP, data = train_data)
  
  # Generate month sequence after training window
  monthly_starts <- seq(from = last_day + 1, to = max(df$Date), by = "1 month")
  
  # Calculate RMSE for each month
  rmse_list <- map_dfr(monthly_starts, function(start_date) {
    end_date <- start_date %m+% months(1) - days(1)
    test_data <- df %>% filter(Date >= start_date, Date <= end_date)
    
    if (nrow(test_data) >= 10) {
      preds <- predict(model, newdata = test_data)
      rmse_val <- rmse(test_data$True_Temp, preds)
      tibble(SiteCode.x = site, Start = start_date, RMSE = rmse_val)
    } else {
      NULL
    }
  })
  
  return(rmse_list)
}

# Apply the function to each golden node
rmse_df <- golden_clean %>%
  group_by(SiteCode.x) %>%
  group_split() %>%
  map_dfr(calculate_monthly_rmse)

# Plot the RMSE over time
ggplot(rmse_df, aes(x = Start, y = RMSE, color = SiteCode.x)) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  labs(
    title = "Monthly RMSE Using Initial 17-Day Calibration",
    x = "Start of Month",
    y = "RMSE (°C)",
    color = "Device"
  ) +
  theme_minimal()

# Ok so we can see the variation present in RMSE over the years. This is mainly seasonal and there doesn't appear
# to be a drift in RMSE over time. Lets see weekly now - 

# Adjust the function to compute weekly RMSE instead of monthly
calculate_weekly_rmse <- function(df) {
  site <- unique(df$SiteCode.x)
  
  first_day <- min(df$Date)
  last_day <- first_day + days(16)
  train_data <- df %>% filter(Date >= first_day & Date <= last_day)
  
  model <- lm(True_Temp ~ TMP, data = train_data)
  
  weekly_starts <- seq(from = last_day + 1, to = max(df$Date), by = "1 week")
  
  rmse_list <- map_dfr(weekly_starts, function(start_date) {
    end_date <- start_date + days(6)
    test_data <- df %>% filter(Date >= start_date, Date <= end_date)
    
    if (nrow(test_data) >= 10) {
      preds <- predict(model, newdata = test_data)
      rmse_val <- rmse(test_data$True_Temp, preds)
      tibble(SiteCode.x = site, Start = start_date, RMSE = rmse_val)
    } else {
      NULL
    }
  })
  
  return(rmse_list)
}

# Apply the weekly function
rmse_weekly_df <- golden_clean %>%
  group_by(SiteCode.x) %>%
  group_split() %>%
  map_dfr(calculate_weekly_rmse)

# Plot weekly RMSE
ggplot(rmse_weekly_df, aes(x = Start, y = RMSE, color = SiteCode.x)) +
  geom_line(size = 0.8) +
  geom_point(size = 1.5) +
  labs(
    title = "Weekly RMSE Using Initial 17-Day Calibration",
    x = "Start of Week",
    y = "RMSE (°C)",
    color = "Device"
  ) +
  theme_minimal()

# there is more variation now, but the trend from above remains. It looks like errors are 
# low in the winter and then high in the summer. Remember that we are not looking at 
# coefficients here, the y-axis is RMSE. 

# Are both nodes being collocated for the first 17 days in winter and therefore winter RMSE's 
# are lower? 

first_17_days_data <- golden_clean %>%
  group_by(SiteCode.x) %>%
  filter(Date >= min(Date) & Date <= min(Date) + days(16)) %>%
  arrange(SiteCode.x, Date)

# View the rows
View(first_17_days_data)
# for CLDP0029 the data is in January and for CLDP0030 the data is in April...

# Either way both nodes perform better in winter. May be worth fitting a model of 17 days in summer, 
# and seeing the performance of that? 

# Same as above but for different 17 days - in summer (July)

# Helper: fit first 17 days *after a cutoff date* for one node
fit_first_17days_after <- function(data, node_id, cutoff = as.Date("2021-07-01")) {
  df <- data %>%
    dplyr::filter(SiteCode.x == node_id) %>%
    dplyr::mutate(Date = as.Date(DateTime)) %>%
    dplyr::arrange(DateTime) %>%
    dplyr::filter(Date >= cutoff)
  
  start_date <- min(df$Date, na.rm = TRUE)
  end_date   <- start_date + lubridate::days(16)  # 17 calendar days inclusive
  
  sub <- df %>%
    dplyr::filter(Date >= start_date, Date <= end_date) %>%
    tidyr::drop_na(TMP, True_Temp)
  
  if (nrow(sub) < 3) {
    return(tibble::tibble(
      node = node_id, start_date, end_date,
      n = nrow(sub), RMSE = NA_real_, MAE = NA_real_, R2 = NA_real_
    ))
  }
  
  m    <- lm(True_Temp ~ TMP, data = sub)
  pred <- predict(m, newdata = sub)
  
  tibble::tibble(
    node       = node_id,
    start_date = start_date,
    end_date   = end_date,
    n          = nrow(sub),
    RMSE       = Metrics::rmse(sub$True_Temp, pred),
    MAE        = Metrics::mae(sub$True_Temp, pred),
    R2         = cor(sub$True_Temp, pred)^2
  )
}

# Run for both golden nodes (summer-start window)
results_17day_summer <- dplyr::bind_rows(
  fit_first_17days_after(golden_clean, "CLDP0029", as.Date("2021-07-01")),
  fit_first_17days_after(golden_clean, "CLDP0030", as.Date("2021-07-01"))
)

print(results_17day_summer)

# Plots

# Helper to extract the first 17-day window after cutoff for plotting
get_17day_window_after <- function(df, node_id, cutoff = as.Date("2021-07-01")) {
  d0 <- df %>%
    dplyr::filter(SiteCode.x == node_id) %>%
    dplyr::mutate(Date = as.Date(DateTime)) %>%
    dplyr::arrange(DateTime) %>%
    dplyr::filter(Date >= cutoff)
  
  start <- min(d0$Date, na.rm = TRUE)
  end   <- start + lubridate::days(16)
  
  sub <- d0 %>%
    dplyr::filter(Date >= start, Date <= end) %>%
    tidyr::drop_na(TMP, True_Temp)
  
  list(sub = sub, start = start, end = end)
}

w29s <- get_17day_window_after(golden_clean, "CLDP0029", as.Date("2021-07-01"))
w30s <- get_17day_window_after(golden_clean, "CLDP0030", as.Date("2021-07-01"))

p29s <- ggplot2::ggplot(w29s$sub, ggplot2::aes(TMP, True_Temp)) +
  ggplot2::geom_point(alpha = 0.25, size = 0.6) +
  ggplot2::geom_smooth(method = "lm", se = FALSE, colour = "steelblue") +
  ggplot2::labs(
    title = paste0("CLDP0029: first 17 days from 2021-07-01 (", w29s$start, " to ", w29s$end, ")"),
    x = "Sensor TMP (°C)",
    y = "Reference temperature (°C)"
  ) +
  ggplot2::theme_minimal(base_size = 12)

p30s <- ggplot2::ggplot(w30s$sub, ggplot2::aes(TMP, True_Temp)) +
  ggplot2::geom_point(alpha = 0.25, size = 0.6) +
  ggplot2::geom_smooth(method = "lm", se = FALSE, colour = "steelblue") +
  ggplot2::labs(
    title = paste0("CLDP0030: first 17 days from 2021-07-01 (", w30s$start, " to ", w30s$end, ")"),
    x = "Sensor TMP (°C)",
    y = "Reference temperature (°C)"
  ) +
  ggplot2::theme_minimal(base_size = 12)

pdf("GoldenNodes_First17Days_fromJuly1_OLS_2up.pdf", width = 9, height = 10)
gridExtra::grid.arrange(p29s, p30s, ncol = 1)
dev.off()

#######################

# Function to train on fixed summer window (July 1–17, 2021) and apply weekly RMSE
calculate_weekly_rmse_summer_fit <- function(df) {
  site <- unique(df$SiteCode.x)
  
  # Training window: 1–17 July 2021
  train_data <- df %>% filter(Date >= as.Date("2021-07-01") & Date <= as.Date("2021-07-17"))
  model <- lm(True_Temp ~ TMP, data = train_data)
  
  # Weekly sequence starting after training window
  weekly_starts <- seq(from = as.Date("2021-07-18"), to = max(df$Date), by = "1 week")
  
  rmse_list <- map_dfr(weekly_starts, function(start_date) {
    end_date <- start_date + days(6)
    test_data <- df %>% filter(Date >= start_date & Date <= end_date)
    
    if (nrow(test_data) >= 10) {
      preds <- predict(model, newdata = test_data)
      rmse_val <- rmse(test_data$True_Temp, preds)
      tibble(SiteCode.x = site, Start = start_date, RMSE = rmse_val)
    } else {
      NULL
    }
  })
  
  return(rmse_list)
}

# Apply to each device
rmse_summer_df <- golden_clean %>%
  group_by(SiteCode.x) %>%
  group_split() %>%
  map_dfr(calculate_weekly_rmse_summer_fit)

# Plot RMSE over time
ggplot(rmse_summer_df, aes(x = Start, y = RMSE, color = SiteCode.x)) +
  geom_line(size = 1) +
  geom_point(size = 1.5) +
  labs(
    title = "Weekly RMSE Using 17-Day Summer Calibration (July 2021)",
    x = "Start of Week",
    y = "RMSE (°C)",
    color = "Device"
  ) +
  theme_minimal()

# you are seeing the same trend - during summer RMSE goes up and then it comes down during winter. 

# overall metrics - 
# ensure Date exists
golden_clean <- golden_clean %>%
  mutate(Date = as.Date(DateTime))

summer_fit_overall_metrics <- function(df_node) {
  node_id <- unique(df_node$SiteCode.x)
  
  # training: 1–17 July 2021
  train <- df_node %>%
    filter(Date >= as.Date("2021-07-01"),
           Date <= as.Date("2021-07-17")) %>%
    drop_na(TMP, True_Temp)
  
  # test: everything after 17 July 2021
  test <- df_node %>%
    filter(Date >= as.Date("2021-07-18")) %>%
    drop_na(TMP, True_Temp)
  
  if (nrow(train) < 3 || nrow(test) == 0) {
    return(tibble(
      SiteCode.x = node_id,
      n_train = nrow(train),
      n_test  = nrow(test),
      RMSE = NA_real_, MAE = NA_real_, R2 = NA_real_
    ))
  }
  
  m <- lm(True_Temp ~ TMP, data = train)
  pred <- predict(m, newdata = test)
  
  tibble(
    SiteCode.x = node_id,
    n_train = nrow(train),
    n_test  = nrow(test),
    RMSE = rmse(test$True_Temp, pred),
    MAE  = mae(test$True_Temp, pred),
    R2   = cor(test$True_Temp, pred)^2
  )
}

# run for each golden node
overall_after_summer <- golden_clean %>%
  group_by(SiteCode.x) %>%
  group_split() %>%
  map_dfr(summer_fit_overall_metrics)

overall_after_summer

######################################### We have looked at weekly RMSE above but now lets look at coefficient stability 

# Visualisation
# Prepare data: Create a single long-format dataframe for plotting
plot_data <- Golden_nodes %>%
  select(DateTime, SiteCode.x, TMP, True_Temp) %>%
  pivot_longer(cols = c("TMP", "True_Temp"),
               names_to = "Variable",
               values_to = "Temperature") %>%
  mutate(Label = ifelse(Variable == "TMP", SiteCode.x, "True_Temp"))

# Save plot to PDF
pdf("GoldenNodes_Sensor_vs_Reference.pdf", width = 10, height = 6)

ggplot(plot_data, aes(x = DateTime, y = Temperature, color = Label)) +
  geom_line(alpha = 0.7) +
  labs(title = "Sensor TMP and Reference True_Temp Over Time",
       x = "DateTime",
       y = "Temperature (°C)",
       color = "Legend") +
  theme_minimal()

dev.off()

###################

# Stability analyses
# Function to get coefficients for fixed-length windows
get_coeffs_by_window <- function(data, node_id, window_days) {
  df <- data %>% filter(SiteCode.x == node_id)
  
  start_time <- min(df$DateTime)
  end_time <- max(df$DateTime)
  
  windows <- seq(from = start_time, to = end_time, by = paste0(window_days, " days"))
  
  result <- map_dfr(seq_along(windows[-length(windows)]), function(i) {
    window_start <- windows[i]
    window_end <- windows[i + 1] - seconds(1)
    
    df_window <- df %>% filter(DateTime >= window_start & DateTime <= window_end)
    
    if (nrow(df_window) < 2 || all(is.na(df_window$TMP)) || all(is.na(df_window$True_Temp))) {
      return(tibble(
        SiteCode.x = node_id,
        start_date = window_start,
        end_date = window_end,
        intercept = NA,
        slope = NA,
        n = nrow(df_window)
      ))
    }
    
    model <- lm(True_Temp ~ TMP, data = df_window)
    tibble(
      SiteCode.x = node_id,
      start_date = window_start,
      end_date = window_end,
      intercept = coef(model)[1],
      slope = coef(model)[2],
      n = nrow(df_window)
    )
  })
  
  return(result)
}

# Generate tables
weekly_0029 <- get_coeffs_by_window(Golden_nodes, "CLDP0029", 7)
weekly_0030 <- get_coeffs_by_window(Golden_nodes, "CLDP0030", 7)

View(weekly_0029)
View(weekly_0030)

weekly_0029 <- weekly_0029 %>%
  select(-n) %>%   # drop unwanted columns
  rename(
    "Device Code"   = SiteCode.x,
    "Start Date"  = start_date,
    "End Date"    = end_date,
    Intercept   = intercept,
    Slope       = slope
  )

weekly_0029 <- weekly_0029 %>%
  mutate(`Device Code` = "AFZHPFH3")

weekly_0030 <- weekly_0030 %>%
  select(-n) %>%   # drop unwanted columns
  rename(
    "Device Code"   = SiteCode.x,
    "Start Date"  = start_date,
    "End Date"    = end_date,
    Intercept   = intercept,
    Slope       = slope
  )

weekly_0030 <- weekly_0030 %>%
  mutate(`Device Code` = "ARNKZ5XY")

# combine - 
weekly_combined <- bind_rows(weekly_0029, weekly_0030)

View(weekly_combined)

# Save to CSV
# write.csv(weekly_combined, "Weekly Coefficients - Golden Nodes.csv", row.names = FALSE)

prep_weekly <- function(df, label){
  df %>%
    janitor::clean_names() %>%            
    mutate(interval = "Weekly",
           start_date = as.Date(start_date)) %>%
    select(start_date, interval, intercept, slope) %>%
    pivot_longer(c(intercept, slope),
                 names_to = "coefficient", values_to = "value") %>%
    mutate(device = label)
}

d29 <- prep_weekly(weekly_0029, "CLDP0029")
d30 <- prep_weekly(weekly_0030, "CLDP0030")

make_plot <- function(dat, title){
  ggplot(dat, aes(x = start_date, y = value, colour = coefficient)) +
    geom_line(linewidth = 0.7) +
    geom_point(shape = 21, fill = "white", size = 2, stroke = 0.6) +
    labs(title = title, x = "Start of Week", y = "Coefficient Value", colour = "Coefficient") +
    theme_minimal(base_size = 12) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
}

p1 <- make_plot(d29, "CLDP0029: Intercept and Slope Over Time (Weekly Models)")
p2 <- make_plot(d30, "CLDP0030: Intercept and Slope Over Time (Weekly Models)")

pdf("GoldenNodes_Weekly_Coefficients.pdf", width = 10, height = 10)
gridExtra::grid.arrange(p1, p2, ncol = 1)
dev.off()

# these plots show how much your golden nodes' coefficients are varying throughout their lifespan

########################### Stability of coefficients using golden nodes, lets fix the offset

# Function using glm() with offset to fix intercept
get_fixed_intercept_slope <- function(data, node_id, window_days, fixed_intercept) {
  df <- data %>% filter(SiteCode.x == node_id)
  
  start_time <- min(df$DateTime)
  end_time <- max(df$DateTime)
  windows <- seq(from = start_time, to = end_time, by = paste0(window_days, " days"))
  
  result <- map_dfr(seq_along(windows[-length(windows)]), function(i) {
    window_start <- windows[i]
    window_end <- windows[i + 1] - seconds(1)
    
    df_window <- df %>%
      filter(DateTime >= window_start & DateTime <= window_end)
    
    if (nrow(df_window) < 3 || all(is.na(df_window$TMP)) || all(is.na(df_window$True_Temp))) {
      return(tibble(
        SiteCode.x = node_id,
        start_date = window_start,
        end_date = window_end,
        slope = NA,
        n = nrow(df_window)
      ))
    }
    
    model <- glm(True_Temp ~ 0 + TMP + offset(rep(fixed_intercept, nrow(df_window))), data = df_window)
    
    tibble(
      SiteCode.x = node_id,
      start_date = window_start,
      end_date = window_end,
      slope = coef(model)["TMP"],
      n = nrow(df_window)
    )
  })
  
  return(result)
}

# Choose fixed intercept (median)
fixed_intercept <- median(Golden_nodes$True_Temp, na.rm = TRUE)
# Apply to node CLDP0029
weekly_0029 <- get_fixed_intercept_slope(Golden_nodes, "CLDP0029", 7, fixed_intercept)
# Apply to node CLDP0030
weekly_0030 <- get_fixed_intercept_slope(Golden_nodes, "CLDP0030", 7, fixed_intercept)

# Tag intervals
weekly_0029 <- weekly_0029 %>% mutate(interval = "Weekly")
weekly_0030 <- weekly_0030 %>% mutate(interval = "Weekly")
# Combine and label
slopes_0029 <- bind_rows(weekly_0029) %>% # 
  mutate(node = "CLDP0029")
slopes_0030 <- bind_rows(weekly_0030) %>% # 
  mutate(node = "CLDP0030")
# Final dataset for plotting
all_slopes <- bind_rows(slopes_0029, slopes_0030)

# Plot for CLDP0029
ggplot(slopes_0029, aes(x = start_date, y = slope, colour = interval)) +
  geom_line(size = 1, aes(group = interval)) +
  geom_point(size = 2, shape = 21, fill = "white", stroke = 1) +
  geom_smooth(method = "lm", se = FALSE, linetype = "dashed", size = 1.2, aes(group = interval)) +
  labs(
    title = paste0("CLDP0029: Weekly vs Monthly Slope (β₁) with Fixed Intercept (", round(fixed_intercept, 2), ")"),
    x = "Start Date",
    y = "Slope (β₁)",
    colour = "Interval"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), text = element_text(size = 12))

# Plot for CLDP0030
ggplot(slopes_0030, aes(x = start_date, y = slope, colour = interval)) +
  geom_line(size = 1, aes(group = interval)) +
  geom_point(size = 2, shape = 21, fill = "white", stroke = 1) +
  geom_smooth(method = "lm", se = FALSE, linetype = "dashed", size = 1.2, aes(group = interval)) +
  labs(
    title = paste0("CLDP0030: Weekly vs Monthly Slope (β₁) with Fixed Intercept (", round(fixed_intercept, 2), ")"),
    x = "Start Date",
    y = "Slope (β₁)",
    colour = "Interval"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), text = element_text(size = 12))

# can visualise sensor drift over time in the unfixed coefficient. 

# pdf version - 

weekly_0029 <- weekly_0029 %>%
  mutate(Node = "CLDP0029") %>%
  select(Node, start_date, slope)

weekly_0030 <- weekly_0030 %>%
  mutate(Node = "CLDP0030") %>%
  select(Node, start_date, slope)

weekly_both <- dplyr::bind_rows(weekly_0029, weekly_0030) %>%
  dplyr::filter(is.finite(slope)) %>%           # keep valid estimates
  dplyr::arrange(Node, start_date)

p_weekly_slopes <- ggplot(weekly_both, aes(x = start_date, y = slope, colour = Node)) +
  geom_line(linewidth = 0.8, alpha = 0.9) +
  geom_point(shape = 21, fill = "white", size = 2, stroke = 0.9) +
  # add line of best fit per node
  geom_smooth(method = "lm", se = FALSE, linetype = "dashed", linewidth = 1) +
  labs(
    title = paste0("Weekly slope (β₁) with fixed intercept = ", round(fixed_intercept, 2)),
    x = "Week start",
    y = "Slope (β₁)"
  ) +
  scale_x_datetime(date_breaks = "6 months", date_labels = "%Y-%m") +
  theme_minimal(base_size = 12) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.title = element_blank()
  )

# Save to PDF again
pdf("GoldenNodes_Weekly_FixedIntercept_Slopes_withTrend.pdf", width = 10, height = 5.5)
print(p_weekly_slopes)
dev.off()

###############################################

# Need to show original error, without any calibration...

# Compute overall RMSE and MAE per golden node
overall_raw_metrics <- golden_clean %>%
  group_by(SiteCode.x) %>%
  summarise(
    n = n(),
    RMSE = rmse(True_Temp, TMP),
    MAE  = mae(True_Temp, TMP),
    .groups = "drop"
  )

print(overall_raw_metrics)

################################################

View(golden_clean)

# overall rmse using this approach without further accuracy scaling -
golden_rmse_df <- golden_clean %>%
  group_by(SiteCode.x) %>%
  group_split() %>%
  map_dfr(function(df) {
    site <- unique(df$SiteCode.x)
    
    # Identify first 17 distinct days
    first_days <- df %>%
      distinct(Date) %>%
      slice_head(n = 17) %>%
      pull(Date)
    
    # Training data = data from first 17 days
    train_data <- df %>% filter(Date %in% first_days)
    
    # Test data = after the last day used in training
    last_train_day <- max(first_days)
    post_train_data <- df %>% filter(Date > last_train_day)
    
    if (nrow(post_train_data) > 10) {
      model <- lm(True_Temp ~ TMP, data = train_data)
      preds <- predict(model, newdata = post_train_data)
      rmse_val <- rmse(post_train_data$True_Temp, preds)
      mae_val <- mae(post_train_data$True_Temp, preds)
      
      tibble(SiteCode = site, Overall_RMSE = rmse_val, Overall_MAE = mae_val)
    } else {
      NULL
    }
  })

# View results
print(golden_rmse_df) # RMSE - CLDP0029 0.986, CLDP0030 1.15 <- important to show, as these are the errors 
# we will likely get if we don't do 2nd stage calibration essentially. 

#########################################  

# plots to show variation in rmse with the above approach of single-step calibration -

# 1. 
first_17_days <- golden_clean %>%
  group_by(SiteCode.x) %>%
  filter(Date >= min(Date) & Date <= min(Date) + days(16)) %>%
  arrange(SiteCode.x, Date)

View(first_17_days)

# 2. Fit regression models separately for each node
models <- first_17_days %>%
  group_by(SiteCode.x) %>%
  do(model = lm(True_Temp ~ TMP, data = .)) %>%
  ungroup()

# 3. Drop the first 17 days from golden_clean → golden_clean_test
golden_clean_test <- golden_clean %>%
  anti_join(first_17_days, by = c("SiteCode.x", "DateTime"))

model_list <- setNames(models$model, models$SiteCode.x)

golden_clean_test <- golden_clean_test %>%
  group_by(SiteCode.x) %>%
  mutate(
    precision_scaled_tmp = {
      m <- model_list[[cur_group()$SiteCode.x]]
      if (is.null(m)) NA_real_ else predict(m, newdata = cur_data())
    }
  ) %>%
  ungroup()

View(golden_clean_test)

# Ok now for visualisation - 

# 1) Round predictions to 1 dp
golden_clean_test <- golden_clean_test %>%
  mutate(precision_scaled_tmp = round(precision_scaled_tmp, 1))

# 2) Weekly RMSE per node for raw vs calibrated
weekly_metrics <- golden_clean_test %>%
  mutate(Week = floor_date(DateTime, unit = "week", week_start = 1)) %>%
  group_by(SiteCode.x, Week) %>%
  summarise(
    RMSE_raw = sqrt(mean((TMP - True_Temp)^2, na.rm = TRUE)),
    RMSE_cal = sqrt(mean((precision_scaled_tmp - True_Temp)^2, na.rm = TRUE)),
    .groups = "drop"
  ) %>%
  pivot_longer(cols = c(RMSE_raw, RMSE_cal),
               names_to = "Series", values_to = "RMSE") %>%
  mutate(Series = recode(Series,
                         RMSE_raw = "Raw vs Reference",
                         RMSE_cal = "1st Stage Calibrated vs Reference"))

# 3) Two plots (one per node), two lines each
# Plot for CLDP0029
p_0029 <- weekly_metrics %>%
  filter(SiteCode.x == "CLDP0029") %>%
  ggplot(aes(x = Week, y = RMSE, color = Series)) +
  geom_line() +
  geom_point(size = 1) +
  labs(title = "Weekly RMSE – CLDP0029",
       x = "Week", y = "RMSE (°C)", color = NULL) +
  theme_minimal(base_size = 12)

# Plot for CLDP0030
p_0030 <- weekly_metrics %>%
  filter(SiteCode.x == "CLDP0030") %>%
  ggplot(aes(x = Week, y = RMSE, color = Series)) +
  geom_line() +
  geom_point(size = 1) +
  labs(title = "Weekly RMSE – CLDP0030",
       x = "Week", y = "RMSE (°C)", color = NULL) +
  theme_minimal(base_size = 12)

# Print or save
print(p_0029)
print(p_0030)

# Save both plots into one PDF stacked vertically
pdf("GoldenNodes_WeeklyRMSE.pdf", width = 9, height = 10)  
grid.arrange(p_0029, p_0030, ncol = 1)  
dev.off()

#################################################### some pre-processing below, to make analysis easier

# We basically need justification to average the coefficients from these nodes when applying them later on to
# calibrate (accuracy calibration) the rest of the node network. 

# Split the golden_clean dataset into two
golden_0029 <- golden_clean %>% filter(SiteCode.x == "CLDP0029")
golden_0030 <- golden_clean %>% filter(SiteCode.x == "CLDP0030")

View(golden_0029)
View(golden_0030)

# Fit precision scaling models
model_0029 <- lm(True_Temp ~ TMP, data = golden_0029)
model_0030 <- lm(True_Temp ~ TMP, data = golden_0030)

# Predict precision scaled values and round to 1dp
golden_0029 <- golden_0029 %>%
  mutate(Precision_Scaled = round(predict(model_0029, newdata = .), 1))

golden_0030 <- golden_0030 %>%
  mutate(Precision_Scaled = round(predict(model_0030, newdata = .), 1))

# Join the datasets by DateTime
precision_df <- inner_join(
  golden_0029 %>% select(DateTime, Scaled_0029 = Precision_Scaled),
  golden_0030 %>% select(DateTime, Scaled_0030 = Precision_Scaled),
  by = "DateTime"
)

View(precision_df)

# Run Bland–Altman plot on your precision-scaled data
bland.altman.plot(
  precision_df$Scaled_0029,
  precision_df$Scaled_0030,
  main = "Bland–Altman Plot: CLDP0029 vs CLDP0030 (Precision Scaled)",
  xlab = "Mean Temperature (°C)",
  ylab = "Difference in Temperature (°C)"
)

# Calculate mean difference (bias) and 95% limits of agreement
differences <- precision_df$Scaled_0029 - precision_df$Scaled_0030
mean_diff <- mean(differences, na.rm = TRUE)
sd_diff <- sd(differences, na.rm = TRUE)
loa_upper <- mean_diff + 1.96 * sd_diff
loa_lower <- mean_diff - 1.96 * sd_diff

cat("Bias (mean difference):", mean_diff, "\n")
cat("95% Limits of Agreement:", loa_lower, "to", loa_upper, "\n")

# The Bland–Altman analysis demonstrates strong agreement between the two golden nodes (CLDP0029 and 
# CLDP0030), with a negligible mean bias of −0.014°C and 95% limits of agreement ranging from −0.97°C 
# to +0.95°C. The differences are centered around zero with no clear trend across the temperature range, 
# indicating consistent performance under varying conditions. Given this high level of concordance and
# minimal systematic deviation, it is statistically justified to average the weekly regression coefficients 
# from both nodes when applying dynamic weekly accuracy scaling to the remaining deployed sensors.

# Assumption checking for this test -

###### The difference between these 2 sensors needs to be normally distributed - 
precision_df$diff <- precision_df$Scaled_0029 - precision_df$Scaled_0030

ggplot(precision_df, aes(x = diff)) +
  geom_histogram(aes(y = ..density..), bins = 50, fill = "skyblue", color = "black") +
  geom_density(color = "red", size = 1) +
  labs(title = "Histogram of Temperature Differences",
       x = "Difference in Temperature (°C)",
       y = "Density") +
  theme_minimal()

qqnorm(precision_df$diff, main = "Q-Q Plot of Temperature Differences")
qqline(precision_df$diff, col = "red", lwd = 2)

# Despite minor skew and heavy tails, your large sample size supports the validity of 
# the Bland–Altman analysis for practical purposes - ignore the qq stuff...

###### assumption 2 - homoscedasticity 

# Create absolute differences
precision_df$abs_diff <- abs(precision_df$Scaled_0029 - precision_df$Scaled_0030)
precision_df$mean_temp <- (precision_df$Scaled_0029 + precision_df$Scaled_0030) / 2

# Plot
ggplot(precision_df, aes(x = mean_temp, y = abs_diff)) +
  geom_point(alpha = 0.2) +
  geom_smooth(method = "loess", color = "blue") +
  labs(
    title = "Homoscedasticity Check: Absolute Difference vs Mean Temperature",
    x = "Mean Temperature (°C)",
    y = "Absolute Difference in Temperature (°C)"
  ) +
  theme_minimal()

# The absolute difference in temperature between CLDP0029 and CLDP0030 remains relatively low and stable 
# across the majority of the temperature range, particularly between approximately 0°C and 25°C. 
# However, the loess trend line clearly shows a gradual increase in variability at higher mean temperatures,
# especially above ~25°C. This suggests mild heteroscedasticity — the variance of the differences increases 
# as temperature increases.

# another assumption is independance...

# More assumption testing - 
# LMMs can be used for this given that we are using repeated obs data, but we need to decide whether we want to 
# group the data into daily measurements (assuming independence between days)

# to check for between day autocorrelation -
# Calculate daily mean difference
daily_diff <- precision_df %>%
  mutate(Date = as.Date(DateTime)) %>%
  group_by(Date) %>%
  summarise(daily_mean_diff = mean(Scaled_0029 - Scaled_0030, na.rm = TRUE))

ggplot(daily_diff, aes(x = Date, y = daily_mean_diff)) +
  geom_line() +
  theme_minimal() +
  labs(title = "Daily Mean Difference Between Golden Nodes",
       y = "Mean Difference", x = "Date")


acf(daily_diff$daily_mean_diff, main = "ACF of Daily Mean Differences")

# The autocorrelation plot of daily mean temperature differences between the two golden nodes shows 
# strong positive autocorrelation, with significant correlations persisting up to around 25 days. 
# This indicates that the differences are not independent over time, violating a key assumption of the 
# Bland–Altman method. The temporal dependency likely reflects gradual shifts such as seasonal effects. 
# Therefore, using simple Bland–Altman analysis on the full time series is inappropriate without adjustment. 
# This supports the need for approaches like weekly aggregation or time-aware models (e.g., linear mixed-effects
# models) to properly assess agreement between the nodes.

weekly_diff <- precision_df %>%
  mutate(Week = cut(DateTime, breaks = "week")) %>%
  group_by(Week) %>%
  summarise(weekly_mean_diff = mean(Scaled_0029 - Scaled_0030, na.rm = TRUE))

acf(weekly_diff$weekly_mean_diff, main = "ACF of Weekly Mean Differences")

# similar to above...

monthly_diff <- precision_df %>%
  mutate(Month = format(as.Date(DateTime), "%Y-%m")) %>%
  group_by(Month) %>%
  summarise(monthly_mean_diff = mean(Scaled_0029 - Scaled_0030, na.rm = TRUE))

acf(monthly_diff$monthly_mean_diff, main = "ACF of Monthly Mean Differences")

# monthly is preferred not because it is inherently better, but because its autocorrelation profile 
# is more manageable and better aligned with the assumptions of basic linear models or
# Bland–Altman-type agreement assessments.

# save above 3 plots in pdf -

## 1) Daily mean difference
daily_diff <- precision_df %>%
  mutate(Date = as.Date(DateTime)) %>%
  group_by(Date) %>%
  summarise(daily_mean_diff = mean(Scaled_0029 - Scaled_0030, na.rm = TRUE),
            .groups = "drop") %>%
  arrange(Date)

## 2) Weekly mean difference (ISO week start = Monday)
weekly_diff <- precision_df %>%
  mutate(Week = floor_date(as.POSIXct(DateTime), unit = "week", week_start = 1)) %>%
  group_by(Week) %>%
  summarise(weekly_mean_diff = mean(Scaled_0029 - Scaled_0030, na.rm = TRUE),
            .groups = "drop") %>%
  arrange(Week)

## 3) Monthly mean difference (use first day of month as a Date)
monthly_diff <- precision_df %>%
  mutate(Month = as.Date(format(as.Date(DateTime), "%Y-%m-01"))) %>%
  group_by(Month) %>%
  summarise(monthly_mean_diff = mean(Scaled_0029 - Scaled_0030, na.rm = TRUE),
            .groups = "drop") %>%
  arrange(Month)

## Helper to safely run ACF only if we have enough data
safe_acf <- function(x, main_title) {
  x <- x[is.finite(x)]
  if (length(x) >= 2) {
    acf(x, na.action = na.pass, main = main_title)
  } else {
    plot.new(); title(main = paste0(main_title, " (insufficient data)"))
  }
}

## One PDF with all three ACFs stacked
pdf("GoldenNodes_ACF_daily_weekly_monthly.pdf", width = 8, height = 10)
op <- par(mfrow = c(3, 1), mar = c(4, 4, 3, 1))

safe_acf(daily_diff$daily_mean_diff,
         "ACF of Daily Mean Differences (CLDP0029 - CLDP0030)")

safe_acf(weekly_diff$weekly_mean_diff,
         "ACF of Weekly Mean Differences (CLDP0029 - CLDP0030)")

safe_acf(monthly_diff$monthly_mean_diff,
         "ACF of Monthly Mean Differences (CLDP0029 - CLDP0030)")

par(op)
dev.off()

# Bland-Altman on monthly - 

# Step 1: Aggregate to monthly means
monthly_diff <- precision_df %>%
  mutate(Month = format(as.Date(DateTime), "%Y-%m")) %>%
  group_by(Month) %>%
  summarise(
    mean_temp = mean((Scaled_0029 + Scaled_0030)/2, na.rm = TRUE),
    diff = mean(Scaled_0029 - Scaled_0030, na.rm = TRUE)
  ) %>%
  ungroup()

# Step 2: Bland–Altman statistics
mean_diff <- mean(monthly_diff$diff)
sd_diff <- sd(monthly_diff$diff)
loa_upper <- mean_diff + 1.96 * sd_diff
loa_lower <- mean_diff - 1.96 * sd_diff

# Step 3: Plot
ggplot(monthly_diff, aes(x = mean_temp, y = diff)) +
  geom_point() +
  geom_hline(yintercept = mean_diff, linetype = "dashed", color = "blue") +
  geom_hline(yintercept = loa_upper, linetype = "dotted", color = "red") +
  geom_hline(yintercept = loa_lower, linetype = "dotted", color = "red") +
  labs(
    title = "Bland–Altman Plot: Monthly Means (CLDP0029 vs CLDP0030)",
    x = "Monthly Mean Temperature (°C)",
    y = "Monthly Mean Difference (°C)"
  ) +
  theme_minimal()

# The monthly Bland–Altman plot shows strong agreement between the two golden nodes, 
# with a negligible mean difference and most monthly differences falling within the 95% limits of 
# agreement. There is no clear temperature-dependent bias or increasing spread, suggesting consistent 
# agreement across the range. This supports the use of both nodes interchangeably for calibration purposes.

# The scatter appears homoscedastic (i.e. there is no obvious trend in the spread of differences across 
# the range of mean temperatures), which supports the assumption of consistent measurement agreement 
# across temperature ranges.

# LMM as well - 

library(lme4)

precision_df <- precision_df %>%
  mutate(Month = format(as.Date(DateTime), "%Y-%m"),
         diff = Scaled_0029 - Scaled_0030)

lmm_model <- lmer(diff ~ 1 + (1 | Month), data = precision_df)
summary(lmm_model)

# I used a linear mixed model (LMM) to test agreement between the two golden nodes (CLDP0029 and CLDP0030) 
# by modelling their hourly temperature differences, with month included as a random effect. 
# The model found a negligible overall bias of -0.011°C, with minimal variation in differences across 
# months (SD = 0.12°C), and most variation occurring within months (SD = 0.47°C). This suggests the
# two sensors show strong and consistent agreement over time, supporting your plan to average their 
# weekly calibration coefficients for accuracy scaling across the wider sensor network.

########################### 
########################### 

# ok so we have done simple precision scaling (17 days) earlier and applied it to the rest of the 
# 4-year data and we have error values. Now we need to explore dynamic calibration and then we 
# can apply that to the rest of the network. 

# lets do some basic pre-processing first before applying dynamic strategies. 

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

########################### 

# Averaging the 2 nodes' coefficents and figuring which time period is best for re-calibration - 

# Helper: Fit model and return coefficients
fit_model <- function(train) {
  model <- lm(True_Temp ~ precision_scaled_tmp, data = train)
  list(
    intercept = coef(model)[1],
    beta = coef(model)[2]
  )
}

# Main function to evaluate over periods
evaluate_periods <- function(data_0029, data_0030, period_type = "daily") {
  get_period <- function(datetime) {
    case_when(
      period_type == "daily" ~ as.Date(datetime),
      period_type == "weekly" ~ floor_date(as.Date(datetime), "week"),
      period_type == "monthly" ~ floor_date(as.Date(datetime), "month")
    )
  }
  
  data_0029 <- data_0029 %>% mutate(Period = get_period(DateTime))
  data_0030 <- data_0030 %>% mutate(Period = get_period(DateTime))
  
  all_periods <- sort(unique(c(unique(data_0029$Period), unique(data_0030$Period))))
  results <- list()
  
  for (p in all_periods) {
    d29 <- data_0029 %>% filter(Period == p) %>% arrange(DateTime)
    d30 <- data_0030 %>% filter(Period == p) %>% arrange(DateTime)
    
    # Reset test sets to avoid carry-over
    test29 <- NULL
    test30 <- NULL
    
    row <- list(
      Period = as.character(p),
      Train_Start = NA, Train_End = NA,
      Test_Start = NA, Test_End = NA,
      Intercept_0029 = NA, Beta_0029 = NA,
      Intercept_0030 = NA, Beta_0030 = NA,
      Intercept_Avg = NA, Beta_Avg = NA,
      RMSE_0029 = NA, MAE_0029 = NA,
      RMSE_0030 = NA, MAE_0030 = NA
    )
    
    # Fit 0029
    if (nrow(d29) >= 5) {
      split29 <- floor(0.7 * nrow(d29))
      train29 <- d29[1:split29, ]
      test29 <- d29[(split29 + 1):nrow(d29), ]
      
      model29 <- fit_model(train29)
      row$Intercept_0029 <- model29$intercept
      row$Beta_0029 <- model29$beta
      
      row$Train_Start <- min(train29$DateTime)
      row$Train_End <- max(train29$DateTime)
      row$Test_Start <- min(test29$DateTime)
      row$Test_End <- max(test29$DateTime)
    }
    
    # Fit 0030
    if (nrow(d30) >= 5) {
      split30 <- floor(0.7 * nrow(d30))
      train30 <- d30[1:split30, ]
      test30 <- d30[(split30 + 1):nrow(d30), ]
      
      model30 <- fit_model(train30)
      row$Intercept_0030 <- model30$intercept
      row$Beta_0030 <- model30$beta
      
      if (is.na(row$Train_Start)) {
        row$Train_Start <- min(train30$DateTime)
        row$Train_End <- max(train30$DateTime)
        row$Test_Start <- min(test30$DateTime)
        row$Test_End <- max(test30$DateTime)
      }
    }
    
    # Average coefficients
    intercepts <- c(row$Intercept_0029, row$Intercept_0030)
    betas <- c(row$Beta_0029, row$Beta_0030)
    row$Intercept_Avg <- mean(intercepts, na.rm = TRUE)
    row$Beta_Avg <- mean(betas, na.rm = TRUE)
    
    # Predict using averaged model
    if (!is.na(row$Intercept_Avg) && !is.na(row$Beta_Avg)) {
      if (!is.null(test29)) {
        preds_0029 <- row$Intercept_Avg + row$Beta_Avg * test29$precision_scaled_tmp
        row$RMSE_0029 <- rmse(test29$True_Temp, preds_0029)
        row$MAE_0029 <- mae(test29$True_Temp, preds_0029)
      }
      if (!is.null(test30)) {
        preds_0030 <- row$Intercept_Avg + row$Beta_Avg * test30$precision_scaled_tmp
        row$RMSE_0030 <- rmse(test30$True_Temp, preds_0030)
        row$MAE_0030 <- mae(test30$True_Temp, preds_0030)
      }
    }
    
    results[[length(results) + 1]] <- row
  }
  
  return(as.data.frame(do.call(rbind, results)))
}

# Run for all three
results_daily <- evaluate_periods(test_0029, test_0030, "daily")
results_weekly <- evaluate_periods(test_0029, test_0030, "weekly")
results_monthly <- evaluate_periods(test_0029, test_0030, "monthly")

# View outputs
View(results_daily)
View(results_weekly)
View(results_monthly)

# Summary table - 

# Coerce relevant columns to numeric for all three tables
results_daily$RMSE_0029 <- as.numeric(results_daily$RMSE_0029)
results_daily$RMSE_0030 <- as.numeric(results_daily$RMSE_0030)
results_daily$MAE_0029 <- as.numeric(results_daily$MAE_0029)
results_daily$MAE_0030 <- as.numeric(results_daily$MAE_0030)

results_weekly$RMSE_0029 <- as.numeric(results_weekly$RMSE_0029)
results_weekly$RMSE_0030 <- as.numeric(results_weekly$RMSE_0030)
results_weekly$MAE_0029 <- as.numeric(results_weekly$MAE_0029)
results_weekly$MAE_0030 <- as.numeric(results_weekly$MAE_0030)

results_monthly$RMSE_0029 <- as.numeric(results_monthly$RMSE_0029)
results_monthly$RMSE_0030 <- as.numeric(results_monthly$RMSE_0030)
results_monthly$MAE_0029 <- as.numeric(results_monthly$MAE_0029)
results_monthly$MAE_0030 <- as.numeric(results_monthly$MAE_0030)

# Create summary table of average error metrics by period
summary_errors <- data.frame(
  Period = c("Daily", "Weekly", "Monthly"),
  RMSE_0029 = c(mean(results_daily$RMSE_0029, na.rm = TRUE),
                mean(results_weekly$RMSE_0029, na.rm = TRUE),
                mean(results_monthly$RMSE_0029, na.rm = TRUE)),
  RMSE_0030 = c(mean(results_daily$RMSE_0030, na.rm = TRUE),
                mean(results_weekly$RMSE_0030, na.rm = TRUE),
                mean(results_monthly$RMSE_0030, na.rm = TRUE)),
  MAE_0029 = c(mean(results_daily$MAE_0029, na.rm = TRUE),
               mean(results_weekly$MAE_0029, na.rm = TRUE),
               mean(results_monthly$MAE_0029, na.rm = TRUE)),
  MAE_0030 = c(mean(results_daily$MAE_0030, na.rm = TRUE),
               mean(results_weekly$MAE_0030, na.rm = TRUE),
               mean(results_monthly$MAE_0030, na.rm = TRUE))
)

# View the summary
View(summary_errors)

# weekly wins but its close...need to average the 2 nodes.

# Some visualisations - 

# Lines I already have (force POSIXct weeks)
weekly_metrics <- golden_clean_test %>%
  mutate(Week = floor_date(DateTime, unit = "week", week_start = 1),
         Week = as.POSIXct(Week, tz = "UTC")) %>%
  group_by(SiteCode.x, Week) %>%
  summarise(
    RMSE_raw = sqrt(mean((TMP - True_Temp)^2, na.rm = TRUE)),
    RMSE_cal = sqrt(mean((precision_scaled_tmp - True_Temp)^2, na.rm = TRUE)),
    .groups = "drop"
  ) %>%
  pivot_longer(c(RMSE_raw, RMSE_cal), names_to = "Series", values_to = "RMSE") %>%
  mutate(Series = recode(Series,
                         RMSE_raw = "Raw vs Reference",
                         RMSE_cal = "1st Stage Calibrated vs Reference"))

# Build weekly Stage-2 *test* points from results_weekly 
weekly_test_points <- results_weekly %>%
  mutate(
    # Period is integer days since 1970-01-01 in your table
    Week = as.Date(as.numeric(Period), origin = "1970-01-01"),
    Week = floor_date(Week, unit = "week", week_start = 1),
    Week = as.POSIXct(Week, tz = "UTC")
  ) %>%
  transmute(
    Week,
    CLDP0029 = as.numeric(RMSE_0029),
    CLDP0030 = as.numeric(RMSE_0030)
  ) %>%
  pivot_longer(c(CLDP0029, CLDP0030),
               names_to = "SiteCode.x", values_to = "RMSE") %>%
  filter(!is.na(RMSE)) %>%
  mutate(Series = "2nd Stage (Test) vs Reference")

View(weekly_test_points)

# Plot function adding dots for Stage-2 test RMSE
plot_weekly_rmse <- function(node_id) {
  ggplot() +
    # Lines: raw + stage1
    geom_line(
      data = filter(weekly_metrics, SiteCode.x == node_id),
      aes(x = Week, y = RMSE, color = Series)
    ) +
    geom_point(
      data = filter(weekly_metrics, SiteCode.x == node_id),
      aes(x = Week, y = RMSE, color = Series), size = 1
    ) +
    # NEW: line + points for stage2 test
    geom_line(
      data = filter(weekly_test_points, SiteCode.x == node_id),
      aes(x = Week, y = RMSE, color = Series), linewidth = 0.7
    ) +
    geom_point(
      data = filter(weekly_test_points, SiteCode.x == node_id),
      aes(x = Week, y = RMSE, color = Series),
      size = 2, shape = 16
    ) +
    scale_color_manual(values = c(
      "Raw vs Reference"                   = "#21A3A3",
      "1st Stage Calibrated vs Reference"  = "#F26C6C",
      "2nd Stage (Test) vs Reference"      = "black"
    )) +
    labs(title = paste0("Weekly RMSE – ", node_id),
         x = "Week", y = "RMSE (°C)", color = NULL) +
    theme_minimal(base_size = 12)
}

p_0029 <- plot_weekly_rmse("CLDP0029")
p_0030 <- plot_weekly_rmse("CLDP0030")

print(p_0029)
print(p_0030)

# Combine plots side by side (or top-bottom)
combined_plot <- p_0029 + p_0030 + plot_layout(ncol = 1)  # 1 column, stacked

# Save to PDF
ggsave("Weekly_RMSE_GoldenNodes.pdf", combined_plot,
       width = 8, height = 10, units = "in")

##############################################

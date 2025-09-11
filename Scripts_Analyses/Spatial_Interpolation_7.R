# Spatial_Interpolation
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
library(INLA)
library(patchwork)
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

View(deployment_md)

View(cldp_with_coefs_cleaned) # rows 12,217,297

# add the long and lat positions - 
# Merge lat/lon into cldp_with_coefs_cleaned
cldp_with_coefs_cleaned <- cldp_with_coefs_cleaned %>%
  left_join(
    deployment_md %>% select(SiteCode, Latitude, Longitude),
    by = "SiteCode"
  )

############################################ yearly averaged maps - 

# load the shapefile (change path to where your GLA boundary lives)
london_boundary <- st_read("Data/gla/London_GLA_Boundary.shp") %>% 
  st_transform(4326)  # make sure it's WGS84 lat/long

# 1) Yearly site averages (per CLDP site, per year)
TEMP_COL <- "accuracy_scaled_tmp"

yearly_node_means <- cldp_with_coefs_cleaned |>
  mutate(Year = year(DateTime)) |>
  group_by(Year, SiteCode, Latitude, Longitude) |>
  summarise(temp = mean(.data[[TEMP_COL]], na.rm = TRUE), .groups = "drop") |>
  st_as_sf(coords = c("Longitude","Latitude"), crs = 4326)

# 2) Use the averaged sf as plotting data
pts_sf <- yearly_node_means
# one shared colour range across points + INLA predictions
lims_common <- range(yearly_node_means$temp, na.rm = TRUE)

# Optional: tidy the legend ticks so both plots show the same labels
breaks_common <- pretty(lims_common, n = 6)

# Define a single colour scale we will reuse everywhere
scale_temp_common <- scale_colour_viridis_c(
  name   = "°C",
  option = "C",           # same palette
  limits = lims_common,   # same numeric limits
  breaks = breaks_common, # same tick marks
  oob    = scales::squish # clamp values outside range
)
years  <- sort(unique(pts_sf$Year))

# 2) Update your yearly map function
map_year <- function(yr) {
  pts_sf |>
    dplyr::filter(Year == yr) |>
    ggplot() +
    geom_sf(data = london_boundary, fill = "white", colour = "grey85", linewidth = 0.2) +
    geom_sf(aes(colour = temp), size = 1.8, alpha = 0.9) +
    scale_temp_common +                      # << use the shared scale
    coord_sf(xlim = st_bbox(london_boundary)[c("xmin","xmax")],
             ylim = st_bbox(london_boundary)[c("ymin","ymax")],
             expand = FALSE) +
    labs(title = paste0("Yearly Average Temperature — ", yr)) +
    theme_void(base_size = 12) +
    theme(
      plot.title = element_text(face = "bold", hjust = 0, margin = margin(b = 6)),
      legend.position = "right",
      plot.margin = margin(8, 8, 8, 8)
    )
}

# 3) Re-run plots
plots <- lapply(2021:2025, map_year)

ggsave(
  "yearly_temp_maps_A4_multipage_matched.pdf",
  plot = wrap_plots(plots, ncol = 1),
  width = 8.27, height = 11.69, units = "in",
  device = cairo_pdf
)
# Ok so the above were just plotting averages. 

# for those cldp sites which had multiple device codes linked - we averaged everything.

#####################################

# Selecting priors for next step - 

# 1 - prior range

# yearly_pts_bng must be EPSG:27700 (metres)
nn_stats <- yearly_pts_bng %>%
  group_split(Year) %>%  # split into list by year
  lapply(function(g){
    nn_idx <- nngeo::st_nn(g, g, k = 2, progress = FALSE)   # first neighbour (k=1 is self)
    dmat   <- units::drop_units(sf::st_distance(g, g))      # full distance matrix in metres
    med    <- median(sapply(seq_along(nn_idx), function(i) dmat[i, nn_idx[[i]][2]]))
    tibble(Year = unique(g$Year), median_nn = med)
  }) %>%
  bind_rows()

nn_stats 

# 2 -  prior sigma 

# In more detail - 

# If I made it prior.sigma = c(3, 0.01), I'm telling the model “I’m fine with spatial variation 
# up to 3C between nearby points”.
# Why it matters for London sensors -
# My network is fairly dense (400m median). If sigma is too big, the model might fit 
# sharp peaks/dips that are really just sensor noise. If sigma is too small, the map will look 
# over-smoothed and fail to show genuine microclimate differences.

# 2023 had the most nodes so pick that year to check fluctuation

TEMP_COL <- "accuracy_scaled_tmp"

# 1) Daily spatial spread in 2023
daily_spatial_sd_2023 <- cldp_with_coefs_cleaned %>%
  filter(year(DateTime) == 2023) %>%
  mutate(Date = as.Date(DateTime)) %>%
  group_by(Date) %>%
  # keep days with enough concurrent sensors (tweak n_min)
  filter(n() >= 100) %>%
  summarise(
    n_sensors = n(),
    city_mean = mean(.data[[TEMP_COL]], na.rm = TRUE),
    sd_spatial = sd(.data[[TEMP_COL]] - city_mean, na.rm = TRUE),
    .groups = "drop"
  )

# 2) Summaries I can report
sd_summary <- daily_spatial_sd_2023 %>%
  summarise(
    days_used   = n(),
    sensors_med = median(n_sensors),
    sd_median   = median(sd_spatial, na.rm = TRUE),
    sd_IQR_low  = quantile(sd_spatial, 0.25, na.rm = TRUE),
    sd_IQR_high = quantile(sd_spatial, 0.75, na.rm = TRUE),
    sd_p90      = quantile(sd_spatial, 0.90, na.rm = TRUE),
    sd_p95      = quantile(sd_spatial, 0.95, na.rm = TRUE)
  )
print(sd_summary)

# 3) Suggest a PC prior for sigma from the distribution
s0 <- as.numeric(sd_summary$sd_p95)  # threshold so that only 5% of days exceed this
p  <- 0.01                           # prior probability P(sigma > s0)
prior_sigma <- c(s0, p)
prior_sigma

# I looked at all those daily SD values and found the 95th percentile -> 4.49°C. 
# That means only 5% of days had bigger spatial differences than this. I told INLA: “It’s only 
# 1% likely that σ (the model’s spatial variability parameter) is bigger than 4.49°C.” That’s
# what prior_sigma = c(4.49, 0.01) means — it’s a way of giving the model a sensible 
# upper bound based on real data.

# We measured how much temperatures vary across space each day, and then we used one of 
# the biggest realistic values from 2023 to tell INLA “don’t expect more variability than
# this most of the time.”

###################################

# Now we need to plot INLA/SPDE yearly model - 

TEMP_COL <- "accuracy_scaled_tmp"

yearly_pts <- cldp_with_coefs_cleaned %>%
  mutate(Year = year(DateTime)) %>%
  group_by(Year, SiteCode, Latitude, Longitude) %>%
  summarise(temp = mean(!!sym(TEMP_COL), na.rm = TRUE), .groups = "drop") %>%
  st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326)

# Work in a metric CRS for SPDE (British National Grid)
bng <- 27700
yearly_pts_bng    <- st_transform(yearly_pts, bng)
boundary_bng      <- st_transform(london_boundary, bng)

# helper: make a regular grid inside a polygon 
.make_grid <- function(boundary_sf, res = 1000) {
  bb  <- st_bbox(boundary_sf)
  xs  <- seq(bb["xmin"], bb["xmax"], by = res)
  ys  <- seq(bb["ymin"], bb["ymax"], by = res)
  grd <- expand.grid(x = xs, y = ys)
  pts <- st_as_sf(grd, coords = c("x", "y"), crs = st_crs(boundary_sf))
  pts[boundary_sf, , op = st_within]
}

# main: fit one year, predict over grid
fit_spde_year <- function(year,
                          pts_sf,        # sf POINTS with cols: Year, temp (numeric)
                          boundary_sf,   # sf POLYGON/POLYGONs in same CRS as pts_sf
                          mesh_max_edge = c(1500, 4000), # again justified...
                          mesh_cutoff   = 400, # we justified this based off median distance between nodes in 2023.
                          grid_res      = 1000,
                          prior_range   = c(400, 0.1),   # P(range < 2 km) = 0.1
                          prior_sigma   = c(4.49, 0.01)) {   # P(sigma>1)=0.01
  
  # filter year & basic guardrails
  d <- pts_sf %>%
    filter(.data$Year == year) %>%
    st_drop_geometry() %>%
    mutate(x = st_coordinates(pts_sf[pts_sf$Year == year, ])[ ,1],
           y = st_coordinates(pts_sf[pts_sf$Year == year, ])[ ,2]) %>%
    select(x, y, temp)
  
  if (nrow(d) < 5) {
    warning(sprintf("Year %s has too few points (%d). Skipping.", year, nrow(d)))
    return(NULL)
  }
  
  # build mesh on boundary + points
  bnd <- st_as_sf(boundary_sf) %>% st_union() %>% st_cast("MULTIPOLYGON")
  bnd_sp <- as_Spatial(bnd)  # INLA still uses sp classes for mesh boundary
  
  mesh <- inla.mesh.2d(
    boundary = inla.sp2segment(bnd_sp),
    loc = as.matrix(d[, c("x","y")]),
    max.edge = mesh_max_edge,
    cutoff   = mesh_cutoff
  )
  
  # SPDE model (PC priors)
  spde <- inla.spde2.pcmatern(
    mesh = mesh,
    alpha = 2,
    prior.range = prior_range,
    prior.sigma = prior_sigma
  )
  idx <- inla.spde.make.index("field", n.spde = spde$n.spde)
  
  # projector matrix for observations
  A_obs <- inla.spde.make.A(mesh, loc = as.matrix(d[, c("x","y")]))
  
  # prediction grid inside boundary
  pred_pts <- .make_grid(boundary_sf, res = grid_res)
  pred_xy  <- st_coordinates(pred_pts)
  A_pred   <- inla.spde.make.A(mesh, loc = pred_xy)
  
  # stacks (FIX: make Intercept length = nrow for each block)
  stk_obs <- inla.stack(
    data    = list(y = d$temp),
    A       = list(A_obs, 1),
    effects = list(idx, data.frame(Intercept = rep(1, nrow(d)))),
    tag     = "obs"
  )
  
  stk_pred <- inla.stack(
    data    = list(y = NA),
    A       = list(A_pred, 1),
    effects = list(idx, data.frame(Intercept = rep(1, nrow(pred_pts)))),
    tag     = "pred"
  )
  
  stk <- inla.stack(stk_obs, stk_pred)
  
  # model
  formula <- y ~ 1 + f(field, model = spde)
  fit <- inla(
    formula,
    data = inla.stack.data(stk),
    family = "gaussian",
    control.predictor = list(A = inla.stack.A(stk), compute = TRUE),
    control.compute   = list(dic = TRUE, waic = TRUE, cpo = TRUE),
    verbose = FALSE
  )
  
  # extract predictions for the pred block
  idx_pred <- inla.stack.index(stk, "pred")$data
  mu  <- fit$summary.fitted.values[idx_pred, "mean"]
  sdv <- fit$summary.fitted.values[idx_pred, "sd"]
  
  pred_sf <- pred_pts %>%
    mutate(pred_mean = mu,
           pred_sd   = sdv,
           Year      = year)
  
  list(
    year       = year,
    mesh       = mesh,
    spde       = spde,
    fit        = fit,
    pred_grid  = pred_sf  # sf POINTS with mean/sd
  )
}

years <- sort(unique(yearly_pts_bng$Year))
years <- years[years %in% 2022:2024]

spde_results <- lapply(
  years,
  fit_spde_year,
  pts_sf      = yearly_pts_bng,
  boundary_sf = boundary_bng,
  mesh_max_edge = c(1500, 4000),
  mesh_cutoff   = 300,
  grid_res      = 300
)

# bind predictions for quick plotting
pred_all <- do.call(rbind, lapply(spde_results, function(x) x$pred_grid))

# 1) One shared colour scale for all facets
rng <- range(pred_all$pred_mean, na.rm = TRUE)

scale_temp_common <- scale_colour_viridis_c(
  option = "C",            # nice continuous palette
  name   = "Temperature (°C)",
  limits = rng
)

# 2) Plot (and optionally save to PDF)
p_years <- ggplot() +
  geom_sf(data = boundary_bng, fill = "grey95", colour = NA) +
  geom_sf(data = pred_all, aes(colour = pred_mean), size = 1) +
  scale_temp_common +
  facet_wrap(~Year) +
  coord_sf(expand = FALSE) +
  theme_void()

print(p_years)

# Save
ggsave("INLA_SPDE_Yearly_Mean_Temperature.pdf", p_years, width = 10, height = 8)

########################################

## Prior anchors
rho0   <- 400      # metres  (from prior.range = c(400, 0.10))
sigma0 <- 4.49     # deg C   (from prior.sigma = c(4.49, 0.01))

## Helper: pull hyperparameter marginals from an INLA fit
extract_hyp <- function(fit, year_lab){
  mh  <- fit$fit$marginals.hyperpar
  nms <- names(mh)
  
  nm_r <- nms[grepl("Range", nms, ignore.case = TRUE)][1]
  nm_s <- nms[grepl("Stdev|Std|Sigma", nms, ignore.case = TRUE)][1]
  
  r <- as.data.frame(mh[[nm_r]]); names(r) <- c("x","density"); r$param <- "Range (m)"
  s <- as.data.frame(mh[[nm_s]]); names(s) <- c("x","density"); s$param <- "Stdev (°C)"
  
  out <- rbind(r, s); out$Year <- year_lab; out
}

## Build tidy posterior df across all fitted years
post_df <- do.call(rbind, lapply(spde_results, function(res) extract_hyp(res, res$year)))

## Prior anchor lines/labels
prior_lines <- data.frame(
  param = c("Range (m)","Stdev (°C)"),
  x     = c(rho0, sigma0),
  lab   = c(paste0("prior ρ₀ = ", rho0, " m (P[ρ<ρ₀]=0.10)"),
            paste0("prior σ₀ = ", sigma0, " °C (P[σ>σ₀]=0.01)"))
)

## Plot & save - 
# Split posteriors by parameter
post_r <- subset(post_df, param == "Range (m)")
post_s <- subset(post_df, param == "Stdev (°C)")

# Sensible x-limits
xmax_r <- quantile(post_r$x, 0.995, na.rm = TRUE)        # e.g. ~10–20 km
xmax_s <- max(quantile(post_s$x, 0.999, na.rm = TRUE),   # ensure prior line is visible
              sigma0 * 1.2)                              # a bit past σ0 (4.49)

# analytic PC prior curves
alpha_r  <- 0.10; rho0   <- 400
lambda_r <- -log(alpha_r) * rho0
prior_r  <- data.frame(
  x    = seq(50, xmax_r, length.out = 1000),
  dens = (lambda_r / (seq(50, xmax_r, length.out = 1000)^2)) *
    exp(-lambda_r / seq(50, xmax_r, length.out = 1000)),
  param = "Range (m)"
)

beta_s   <- 0.01; sigma0 <- 4.49
lambda_s <- -log(beta_s) / sigma0
prior_s  <- data.frame(
  x    = seq(0.001, xmax_s, length.out = 1000),
  dens = lambda_s * exp(-lambda_s * seq(0.001, xmax_s, length.out = 1000)),
  param = "Stdev (°C)"
)

# Prior anchor lines
prior_lines_r <- data.frame(param = "Range (m)", x = rho0,
                            lab = "prior ρ₀ = 400 m (P[ρ<ρ₀]=0.10)")
prior_lines_s <- data.frame(param = "Stdev (°C)", x = sigma0,
                            lab = "prior σ₀ = 4.49 °C (P[σ>σ₀]=0.01)")

# Range panel (faceted by year)
p_range <- ggplot(post_r, aes(x = x, y = density)) +
  geom_area(alpha = 0.2, fill = "grey70") +
  geom_line(linewidth = 0.8) +
  geom_line(data = prior_r, aes(x, dens), colour = "firebrick", linetype = "dotted") +
  geom_vline(data = prior_lines_r, aes(xintercept = x), colour = "firebrick", linetype = 2) +
  geom_text(data = prior_lines_r, aes(x = x, y = Inf, label = lab),
            colour = "firebrick", angle = 90, vjust = -0.35, hjust = 1, size = 3) +
  facet_wrap(~Year, scales = "free_y") +
  coord_cartesian(xlim = c(0, xmax_r)) +
  labs(title = "Range (ρ) posterior by year with PC prior",
       x = "Range (m)", y = "Density") +
  theme_minimal(base_size = 12)

# Sigma panel (faceted by year) — tight x-limits so it’s visible
p_sigma <- ggplot(post_s, aes(x = x, y = density)) +
  geom_area(alpha = 0.2, fill = "grey70") +
  geom_line(linewidth = 0.8) +
  geom_line(data = prior_s, aes(x, dens), colour = "firebrick", linetype = "dotted") +
  geom_vline(data = prior_lines_s, aes(xintercept = x), colour = "firebrick", linetype = 2) +
  geom_text(data = prior_lines_s, aes(x = x, y = Inf, label = lab),
            colour = "firebrick", angle = 90, vjust = -0.35, hjust = 1, size = 3) +
  facet_wrap(~Year, scales = "free_y") +
  coord_cartesian(xlim = c(0, xmax_s)) +
  labs(title = "Spatial s.d. (σ) posterior by year with PC prior",
       x = "σ (°C)", y = "Density") +
  theme_minimal(base_size = 12)

# Save one PDF with both panels (range on top, sigma below)
pdf("INLA_SPDE_PriorPosterior_Range_and_Sigma.pdf", width = 11, height = 8)
grid.arrange(p_range, p_sigma, ncol = 1, heights = c(1.1, 1))
dev.off()

## small table of posterior means & 95% CI
summ_tbl <- do.call(rbind, lapply(spde_results, function(res){
  sh <- res$fit$summary.hyperpar
  data.frame(
    Year              = res$year,
    Range_mean_m      = sh[grep("Range", rownames(sh), ignore.case = TRUE), "mean"],
    Range_0.025_m     = sh[grep("Range", rownames(sh), ignore.case = TRUE), "0.025quant"],
    Range_0.975_m     = sh[grep("Range", rownames(sh), ignore.case = TRUE), "0.975quant"],
    Stdev_mean_degC   = sh[grep("Stdev|Std|Sigma", rownames(sh), ignore.case = TRUE), "mean"],
    Stdev_0.025_degC  = sh[grep("Stdev|Std|Sigma", rownames(sh), ignore.case = TRUE), "0.025quant"],
    Stdev_0.975_degC  = sh[grep("Stdev|Std|Sigma", rownames(sh), ignore.case = TRUE), "0.975quant"],
    row.names = NULL
  )
}))
print(summ_tbl)

################################

## Extract DIC and WAIC for each year
dic_waic <- do.call(rbind, lapply(spde_results, function(res){
  if (is.null(res)) return(NULL)
  tibble(
    Year = res$year,
    DIC  = res$fit$dic$dic,
    WAIC = res$fit$waic$waic
  )
}))
print(dic_waic)


## Plot posterior standard deviation maps
ggplot() +
  geom_sf(data = boundary_bng, fill = "grey95", colour = NA) +
  geom_sf(data = pred_all, aes(colour = pred_sd), size = 1) +
  scale_colour_viridis_c(option = "plasma", name = "Posterior SD (°C)") +
  facet_wrap(~Year) +
  coord_sf(expand = FALSE) +
  theme_void() +
  labs(title = "Posterior Standard Deviation of Interpolated Temperatures")

############# Mesh

# spde_results is my list from fit_spde_year(...)
# Each element has $year and $mesh

plot_mesh_for_year <- function(res, boundary_sf, file){
  m <- res$mesh
  # base graphics device
  pdf(file, width = 7, height = 7)
  par(mar = c(1,1,1,1))
  plot(m, asp = 1, main = paste("SPDE mesh —", res$year))
  # overlay boundary (optional; needs same CRS as mesh coords)
  try({
    bnd_sp <- as_Spatial(boundary_sf) # 'sp' class
    plot(bnd_sp, add = TRUE, border = "grey20", lwd = 1.2)
  }, silent = TRUE)
  dev.off()
}

# Save meshes for 2022, 2023, 2024 (only if they exist)
yrs_wanted <- c(2022, 2023, 2024)
for(res in spde_results){
  if(res$year %in% yrs_wanted){
    plot_mesh_for_year(
      res,
      boundary_sf = boundary_bng,
      file = paste0("SPDE_Mesh_", res$year, ".pdf")
    )
  }
}

pdf("SPDE_Meshes_2022_2023_2024.pdf", width = 7, height = 9)
par(mfrow = c(3,1), mar = c(1,1,2,1))
for(y in yrs_wanted){
  res <- spde_results[[which(sapply(spde_results, `[[`, "year") == y)]]
  plot(res$mesh, asp = 1, main = paste("SPDE mesh —", y))
  try({
    bnd_sp <- as_Spatial(boundary_bng)
    plot(bnd_sp, add = TRUE, border = "grey20", lwd = 1.2)
  }, silent = TRUE)
}
dev.off()

###################################### seasonal - 

## 1) Tag seasons from DateTime 
cldp_with_coefs_cleaned <- cldp_with_coefs_cleaned %>%
  mutate(DateTime = ymd_hms(DateTime, tz = "UTC"),
         Month    = month(DateTime),
         Season   = case_when(
           Month %in% c(12, 1, 2)  ~ "Winter",
           Month %in% c(3, 4, 5)   ~ "Spring",
           Month %in% c(6, 7, 8)   ~ "Summer",
           Month %in% c(9,10,11)   ~ "Autumn"
         ))

## 2) Seasonal mean per site (accuracy_scaled_tmp only)
TEMP_COL <- "accuracy_scaled_tmp"

seasonal_pts <- cldp_with_coefs_cleaned %>%
  group_by(Season, SiteCode, Latitude, Longitude) %>%
  summarise(temp = mean(.data[[TEMP_COL]], na.rm = TRUE), .groups = "drop") %>%
  st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326)

# Work in metric CRS (British National Grid)
bng <- 27700
seasonal_pts_bng <- st_transform(seasonal_pts, bng)
boundary_bng     <- st_transform(london_boundary, bng)

## Helper: regular grid inside polygon
.make_grid <- function(boundary_sf, res = 1000) {
  bb  <- st_bbox(boundary_sf)
  xs  <- seq(bb["xmin"], bb["xmax"], by = res)
  ys  <- seq(bb["ymin"], bb["ymax"], by = res)
  grd <- expand.grid(x = xs, y = ys)
  pts <- st_as_sf(grd, coords = c("x","y"), crs = st_crs(boundary_sf))
  pts[boundary_sf, , op = st_within]
}

## 3) Fit one season & predict
fit_spde_season <- function(season,
                            pts_sf,
                            boundary_sf,
                            mesh_max_edge = c(1500, 4000),
                            mesh_cutoff   = 400,            # justified by ~median NN spacing
                            grid_res      = 300,
                            prior_range   = c(400, 0.10),   # P(ρ < 400 m) = 0.10
                            prior_sigma   = c(4.49, 0.01))  # P(σ > 4.49 °C) = 0.01
{
  d_sf <- pts_sf %>% filter(Season == season)
  if (nrow(d_sf) < 5) {
    warning(sprintf("Season %s has too few points (%d). Skipping.", season, nrow(d_sf)))
    return(NULL)
  }
  
  # coords in matrix
  xy <- st_coordinates(d_sf)
  d  <- d_sf %>% st_drop_geometry() %>%
    transmute(x = xy[,1], y = xy[,2], temp = temp)
  
  # boundary (sp for INLA mesh helper)
  bnd_sp <- as_Spatial(st_union(st_cast(boundary_sf, "MULTIPOLYGON")))
  
  mesh <- inla.mesh.2d(
    boundary = inla.sp2segment(bnd_sp),
    loc      = as.matrix(d[, c("x","y")]),
    max.edge = mesh_max_edge,
    cutoff   = mesh_cutoff
  )
  
  spde <- inla.spde2.pcmatern(
    mesh        = mesh,
    alpha       = 2,
    prior.range = prior_range,
    prior.sigma = prior_sigma
  )
  
  idx   <- inla.spde.make.index("field", n.spde = spde$n.spde)
  A_obs <- inla.spde.make.A(mesh, loc = as.matrix(d[, c("x","y")]))
  
  pred_pts <- .make_grid(boundary_sf, res = grid_res)
  A_pred   <- inla.spde.make.A(mesh, loc = st_coordinates(pred_pts))
  
  stk_obs <- inla.stack(
    data    = list(y = d$temp),
    A       = list(A_obs, 1),
    effects = list(idx, data.frame(Intercept = rep(1, nrow(d)))),
    tag     = "obs"
  )
  
  stk_pred <- inla.stack(
    data    = list(y = NA),
    A       = list(A_pred, 1),
    effects = list(idx, data.frame(Intercept = rep(1, nrow(pred_pts)))),
    tag     = "pred"
  )
  
  stk  <- inla.stack(stk_obs, stk_pred)
  fml  <- y ~ 1 + f(field, model = spde)
  
  fit <- inla(
    fml,
    data = inla.stack.data(stk),
    family = "gaussian",
    control.predictor = list(A = inla.stack.A(stk), compute = TRUE),
    control.compute   = list(dic = TRUE, waic = TRUE, cpo = TRUE),
    verbose = FALSE
  )
  
  idx_pred <- inla.stack.index(stk, "pred")$data
  mu  <- fit$summary.fitted.values[idx_pred, "mean"]
  sdv <- fit$summary.fitted.values[idx_pred, "sd"]
  
  pred_sf <- pred_pts %>%
    mutate(pred_mean = mu,
           pred_sd   = sdv,
           Season    = season)
  
  list(
    season    = season,
    mesh      = mesh,
    spde      = spde,
    fit       = fit,
    pred_grid = pred_sf
  )
}

## 4) Run all seasons 
seasons <- c("Winter","Spring","Summer","Autumn")
spde_season <- lapply(
  seasons,
  fit_spde_season,
  pts_sf      = seasonal_pts_bng,
  boundary_sf = boundary_bng,
  mesh_max_edge = c(1500, 4000),
  mesh_cutoff   = 400,
  grid_res      = 300
)
spde_season <- Filter(Negate(is.null), spde_season)

## Bind predictions
pred_season_all <- do.call(rbind, lapply(spde_season, `[[`, "pred_grid"))

## 5) Maps: seasonal posterior MEAN 
rng_mean <- range(pred_season_all$pred_mean, na.rm = TRUE)
scale_temp_common <- scale_colour_viridis_c(
  option = "C", name = "Temperature (°C)", limits = rng_mean
)

p_season_mean <- ggplot() +
  geom_sf(data = boundary_bng, fill = "grey95", colour = NA) +
  geom_sf(data = pred_season_all, aes(colour = pred_mean), size = 1) +
  scale_temp_common +
  facet_wrap(~Season) +
  coord_sf(expand = FALSE) +
  theme_void() +
  labs(title = "Posterior mean seasonal temperature (INLA–SPDE)")
ggsave("INLA_SPDE_Seasonal_Mean_Temperature.pdf", p_season_mean, width = 10, height = 8)

# 1) Seasonally demeaned anomalies (°C)
pred_season_all_anom <- pred_season_all %>%
  group_by(Season) %>%
  mutate(anom = pred_mean - mean(pred_mean, na.rm = TRUE)) %>%
  ungroup()

# symmetric colour limits per season set by global max |anom|
L <- max(abs(pred_season_all_anom$anom), na.rm = TRUE)

p_anom <- ggplot() +
  geom_sf(data = boundary_bng, fill = "grey95", colour = NA) +
  geom_sf(data = pred_season_all_anom, aes(colour = anom), size = 1) +
  scale_colour_gradient2(
    name   = "Anomaly (°C)",
    low    = "#2166AC", mid = "white", high = "#B2182B",
    midpoint = 0, limits = c(-L, L)
  ) +
  facet_wrap(~Season) +
  coord_sf(expand = FALSE) +
  theme_void() +
  labs(title = "Seasonal temperature anomalies (demeaned per season)")

ggsave("INLA_SPDE_Seasonal_Anomaly_Maps.pdf", p_anom, width = 10, height = 8)

# 2) seasonally standardised z-scores 
pred_season_all_z <- pred_season_all %>%
  group_by(Season) %>%
  mutate(z = (pred_mean - mean(pred_mean, na.rm = TRUE)) /
           sd(pred_mean, na.rm = TRUE)) %>%
  ungroup()

Lz <- max(abs(pred_season_all_z$z), na.rm = TRUE)

p_z <- ggplot() +
  geom_sf(data = boundary_bng, fill = "grey95", colour = NA) +
  geom_sf(data = pred_season_all_z, aes(colour = z), size = 1) +
  scale_colour_gradient2(
    name = "z-score",
    low = "#2166AC", mid = "white", high = "#B2182B",
    midpoint = 0, limits = c(-Lz, Lz)
  ) +
  facet_wrap(~Season) +
  coord_sf(expand = FALSE) +
  theme_void() +
  labs(title = "Seasonal temperature z-scores (standardised per season)")

ggsave("INLA_SPDE_Seasonal_Zscore_Maps.pdf", p_z, width = 10, height = 8)

## 6) seasonal posterior SD 
rng_sd <- range(pred_season_all$pred_sd, na.rm = TRUE)
p_season_sd <- ggplot() +
  geom_sf(data = boundary_bng, fill = "grey95", colour = NA) +
  geom_sf(data = pred_season_all, aes(colour = pred_sd), size = 1) +
  scale_colour_viridis_c(option = "plasma", name = "Posterior SD (°C)", limits = rng_sd) +
  facet_wrap(~Season) +
  coord_sf(expand = FALSE) +
  theme_void() +
  labs(title = "Posterior SD of seasonal temperatures (INLA–SPDE)")
ggsave("INLA_SPDE_Seasonal_SD_Temperature.pdf", p_season_sd, width = 10, height = 8)

## 7) Priors vs posteriors
rho0   <- 400
sigma0 <- 4.49

extract_hyp <- function(res){
  mh  <- res$fit$marginals.hyperpar
  nms <- names(mh)
  nm_r <- nms[grepl("Range", nms, ignore.case = TRUE)][1]
  nm_s <- nms[grepl("Stdev|Std|Sigma", nms, ignore.case = TRUE)][1]
  r <- as.data.frame(mh[[nm_r]]); names(r) <- c("x","density"); r$param <- "Range (m)"; r$Season <- res$season
  s <- as.data.frame(mh[[nm_s]]); names(s) <- c("x","density"); s$param <- "Stdev (°C)"; s$Season <- res$season
  rbind(r, s)
}
post_df <- do.call(rbind, lapply(spde_season, extract_hyp))
post_r  <- subset(post_df, param == "Range (m)")
post_s  <- subset(post_df, param == "Stdev (°C)")

# analytic PC priors (for reference curves)
alpha_r  <- 0.10; lambda_r <- -log(alpha_r) * rho0
x_r      <- seq(50, quantile(post_r$x, 0.995, na.rm = TRUE), length.out = 1000)
prior_r  <- data.frame(x = x_r, dens = (lambda_r / (x_r^2))*exp(-lambda_r/x_r))

beta_s   <- 0.01; lambda_s <- -log(beta_s) / sigma0
x_s      <- seq(0.001, max(sigma0*1.2, quantile(post_s$x, 0.999, na.rm = TRUE)), length.out = 1000)
prior_s  <- data.frame(x = x_s, dens = lambda_s*exp(-lambda_s*x_s))

p_range <- ggplot(post_r, aes(x, density)) +
  geom_area(alpha = .2, fill = "grey70") + geom_line() +
  geom_line(data = prior_r, aes(x, dens), colour = "firebrick", linetype = "dotted") +
  geom_vline(xintercept = rho0, colour = "firebrick", linetype = 2) +
  facet_wrap(~Season, scales = "free_y") +
  labs(title = "Range (ρ): prior and posterior by season", x = "Range (m)", y = "Density") +
  theme_minimal(12)

p_sigma <- ggplot(post_s, aes(x, density)) +
  geom_area(alpha = .2, fill = "grey70") + geom_line() +
  geom_line(data = prior_s, aes(x, dens), colour = "firebrick", linetype = "dotted") +
  geom_vline(xintercept = sigma0, colour = "firebrick", linetype = 2) +
  facet_wrap(~Season, scales = "free_y") +
  labs(title = "Spatial s.d. (σ): prior and posterior by season", x = "σ (°C)", y = "Density") +
  theme_minimal(12)

pdf("INLA_SPDE_Seasonal_PriorPosterior_Range_and_Sigma.pdf", width = 11, height = 8)
grid.arrange(p_range, p_sigma, ncol = 1, heights = c(1.1, 1))
dev.off()

## 8) Hyperparameter summary table (posterior mean & 95% CI) 
season_hyp_tbl <- do.call(rbind, lapply(spde_season, function(res){
  sh <- res$fit$summary.hyperpar
  data.frame(
    Season            = res$season,
    Range_mean_m      = sh[grep("Range", rownames(sh), ignore.case = TRUE), "mean"],
    Range_0.025_m     = sh[grep("Range", rownames(sh), ignore.case = TRUE), "0.025quant"],
    Range_0.975_m     = sh[grep("Range", rownames(sh), ignore.case = TRUE), "0.975quant"],
    Stdev_mean_degC   = sh[grep("Stdev|Std|Sigma", rownames(sh), ignore.case = TRUE), "mean"],
    Stdev_0.025_degC  = sh[grep("Stdev|Std|Sigma", rownames(sh), ignore.case = TRUE), "0.025quant"],
    Stdev_0.975_degC  = sh[grep("Stdev|Std|Sigma", rownames(sh), ignore.case = TRUE), "0.975quant"]
  )
}))
print(season_hyp_tbl)

# Order seasons (optional)
season_order <- c("Winter","Spring","Summer","Autumn")

season_tbl_fmt <- season_hyp_tbl %>%
  mutate(
    Season = factor(Season, levels = season_order)
  ) %>%
  arrange(Season) %>%
  transmute(
    Season,
    `Range mean (m)`   = round(Range_mean_m),
    `Range 95% CI (m)` = paste0(round(Range_0.025_m), "–", round(Range_0.975_m)),
    `σ mean (°C)`      = round(Stdev_mean_degC, 2),
    `σ 95% CI (°C)`    = paste0(round(Stdev_0.025_degC, 2), "–", round(Stdev_0.975_degC, 2))
  )

print(season_tbl_fmt)

## 9) WAIC and DIC 
season_dic_waic <- do.call(rbind, lapply(spde_season, function(res){
  tibble(
    Season = res$season,
    DIC    = res$fit$dic$dic,
    WAIC   = res$fit$waic$waic
  )
}))
print(season_dic_waic)

## 10) Mesh plots per season 
plot_mesh_for_season <- function(res, boundary_sf, file){
  pdf(file, width = 7, height = 7); par(mar = c(1,1,2,1))
  plot(res$mesh, asp = 1, main = paste("SPDE mesh —", res$season))
  try({
    bnd_sp <- as_Spatial(boundary_sf)
    plot(bnd_sp, add = TRUE, border = "grey20", lwd = 1.2)
  }, silent = TRUE)
  dev.off()
}
for(res in spde_season){
  plot_mesh_for_season(res, boundary_bng,
                       file = paste0("SPDE_Mesh_", res$season, ".pdf"))
}

## All seasonal SPDE meshes on 1 A4 page
pdf("SPDE_Mesh_All_Seasons_OnePage.pdf", width = 8.27, height = 11.69)

par(mfrow = c(2, 2),              # 2 columns × 2 rows
    mar   = c(0.7, 0.7, 2, 0.7),  # leave space for titles
    oma   = c(0.5, 0.5, 1, 0.5))  

draw_mesh <- function(res, boundary_sf, title){
  plot(res$mesh, asp = 1, main = title, cex.main = 1.2)
  try({
    bnd_sp <- as_Spatial(boundary_sf)
    plot(bnd_sp, add = TRUE, border = "grey30", lwd = 0.8)
  }, silent = TRUE)
}

# Fixed order & labels
seasons <- c("Winter","Spring","Summer","Autumn")
for (s in seasons) {
  res <- spde_season[[which(sapply(spde_season, `[[`, "season") == s)]]
  if (!is.null(res)) draw_mesh(res, boundary_bng, title = s)
}

mtext("SPDE meshes by season", outer = TRUE, cex = 1.3, line = 0)

dev.off()

######################################

### Load  libraries--------------------------------------------------------------
library(dplyr)
library(purrr)
library(stringr)
library(lubridate)
library(data.table)
library(ggplot2)
library(scales)
library(rootSolve)
library(tidyr)
library(stringr)
library(lubridate)
library(broom)
library(grid)
library(hms)
library(FSA)
library(lunar)
library(suncalc)
library(rlang)
library(patchwork)
library(zoo)
library(rNYOS)
library(sf)
library(ggspatial)
library(marmap)
library(rnaturalearth)
library(rnaturalearthdata)
library(ggnewscale)
library(cowplot)
library(rnaturalearthhires)
library(devtools)
library(metR)

### FIGURE 1 Map ####
# Set working directory and read data
setwd("/Users/sophiawill")
locations <- read.csv("/Volumes/SophiaWill/CHP1_SFW_Installation/Plots/For Manuscript/pile_locations.csv")
locations[16,2] <- "SRW"
locations[17,2] <- "SFW"

# Convert to sf object
my_sf <- st_as_sf(locations, coords = c("Long.turbine..degree.", "Lat.turbine..degree."))
my_sf <- st_set_crs(my_sf, 4326) # coordinate system

# Create shape column and label
my_sf$Shape <- ifelse(my_sf$F0.Code %in% c("SRW", "SFW"), "Bottom Lander", "Turbine")

# Convert to factor with desired legend order
my_sf$Shape <- factor(my_sf$Shape, levels = c("Turbine", "Bottom Lander"))

# Define shape values
shape_vals <- c("Turbine" = 16, "Bottom Lander" = 16)

# Read shapefile
lease <- st_read("/Volumes/SophiaWill/CHP1_SFW_Installation/Plots/For Manuscript/WindLeaseBoundaries/Offshore_Wind_Lease_Outlines.shp") 
lease <- st_transform(lease, crs = 4326)

# Subset just SFW
SFW <- lease[17,]
SRW <- lease[10,]

# Get bathymetry data from marmap
bathy <- getNOAA.bathy(lon1 = -71.3, lon2 = -70.9,
                       lat1 = 40.95, lat2 = 41.15, resolution = .05)

# Convert bathy to dataframe for ggplot
bathy_df <- fortify.bathy(bathy)

# Plot
small <- ggplot() +
  # Bathymetry
  geom_raster(data = bathy_df, aes(x = x, y = y), fill = "#66CCEE") +
  
  # Bathymetry contours at 30m, 40m, 50m
  geom_contour(
    data = bathy_df,
    aes(x = x, y = y, z = z),
    breaks = c(-45),  # depths are negative below sea level
    color = "black",
    linewidth = 0.4
  ) +
  
  # Turbine points
  geom_sf(data = my_sf, aes(shape = Shape, color = Shape), size = 3) +
  scale_shape_manual(name = "", values = shape_vals) +
  scale_color_manual(name = "", values = c("Turbine" = "#EE6677", "Bottom Lander" = "black")) +
  
  # Turbine labels (nudge to right)
  geom_sf_text(
    data = subset(my_sf, Shape == "Turbine"),
    aes(label = F0.Code),
    size = 4,
    color = "black",
    nudge_x = 0.007,
    nudge_y = 0.009
  ) +
  
  # Bottom lander labels (keep same position)
  geom_sf_text(
    data = subset(my_sf, Shape == "Bottom Lander"),
    aes(label = F0.Code),
    size = 4,
    color = "black",
    nudge_x = 0.0,
    nudge_y = 0.006
  ) +
  
  # Coastline
  geom_sf(data = ne_countries(scale = "medium", returnclass = "sf"), fill = "gray90", color = "black") +
  
  # Scale bar
  annotation_scale(
    location = "br",
    width_hint = 0.2,
    style = "ticks",
    bar_cols = c("black", "black"),
    height = unit(0.01, "cm"),   
    line_width = 1.2,      
    text_cex = 1               
  ) +
  
  # Manual contour
  annotate("text",
           x = -71.20,   
           y = 41.04, 
           label = "45 m",
           size = 4.2,
           color = "black",
           alpha = 1) +
  
  # Coordinate limits and theme
  coord_sf(xlim = c(-71.25, -71.05), ylim = c(40.98, 41.13), expand = FALSE) +
  scale_x_continuous(
    breaks = seq(-71.3, -71.0, by = 0.1)) +
  scale_y_continuous(
    breaks = seq(40.8, 41.4, by = 0.05)) +
  labs(x = "Longitude", y = "Latitude", title = "") +
  theme(legend.position = "bottom") +
  theme_minimal(base_size = 18)

# Get bathymetry data from marmap
bathy <- getNOAA.bathy(lon1 = -75, lon2 = -65,
                       lat1 = 39, lat2 = 42, resolution = .5)

# Convert bathy to dataframe for ggplot
bathy_df <- fortify.bathy(bathy)

land <- ne_countries(scale = "medium", returnclass = "sf")
states <- ne_states(country = "United States of America", returnclass = "sf")

big <- ggplot() +
  # Ocean background from bathy
  geom_raster(data = bathy_df, aes(x = x, y = y), fill = "#66CCEE") +
  new_scale_fill() +
  geom_sf(data = states, fill = "#BBBBBB", color = "black", size = 0.5) +
  geom_sf(data = SFW, aes(fill = "SFW lease area"), color = alpha("#CCBB44"), linewidth = 0.6) +
  geom_sf(data = SRW, aes(fill = "SRW lease area"), color = alpha("#228833"), linewidth = 0.6) +
  scale_fill_manual(name = "", values = c("SFW lease area" = "#CCBB44",
                                          "SRW lease area" = "#228833")) +
  
  # Scale bar
  annotation_scale(
    location = "br",
    width_hint = 0.2,
    style = "ticks",
    bar_cols = c("black", "black"),
    height = unit(0.01, "cm"),   
    line_width = 1.2,      
    text_cex = 1               
  ) +
  
  coord_sf(xlim = c(-73.5, -70.5), ylim = c(40.5, 41.5), expand = FALSE)+
  scale_x_continuous(
    breaks = seq(-73, -71, by = 0.5)) +
  scale_y_continuous(
    breaks = seq(40.6, 41.4, by = 0.2)) +
  labs(x = "Longitude", y = "Latitude", title = "") +
  theme_minimal(base_size = 18) +
  theme(legend.position = "bottom")

small <- small +
  theme(panel.border = element_rect(color = "#AA3377", fill = NA, size = 1.7))

big <- big +
  geom_rect(aes(xmin = -71.25, xmax = -71.05, ymin = 40.99, ymax = 41.13),
            color = "#AA3377", fill = NA, size = .7)

common_theme <- theme(
  plot.margin = margin(0, 0, 0, 0),
  axis.title.y = element_text(margin = margin(r = 8))
)

big2 <- big + common_theme +
  theme(
    legend.position = c(0.76, 0.07),  
    legend.justification = c(0, 0),
    
    legend.background = element_rect(fill = "white", color = "black", linewidth = 0.5),
    legend.key = element_rect(fill = "white", color = NA),
    
    legend.box.spacing = unit(0, "cm"),
    legend.spacing.y = unit(0.1, "cm"),
    legend.margin = margin(2, 2, 2, 2),
    legend.title = element_blank()
  ) +
  guides(
    fill = guide_legend(nrow = 2, byrow = TRUE),
    shape = guide_legend(nrow = 2, byrow = TRUE),
    color = guide_legend(nrow = 2, byrow = TRUE)
  )

small2 <- small + common_theme +
  theme(
    legend.position = c(0.5, 0.07),  
    legend.justification = c(0, 0),
    
    legend.background = element_rect(fill = "white", color = "black", linewidth = 0.5),
    legend.key = element_rect(fill = "white", color = NA),
    legend.box.spacing = unit(0, "cm"),
    legend.spacing.y = unit(0.1, "cm"),
    legend.margin = margin(2, 2, 2, 2),
    legend.title = element_blank()
  ) +
  guides(
    fill = guide_legend(nrow = 2, byrow = TRUE),
    shape = guide_legend(nrow = 2, byrow = TRUE),
    color = guide_legend(nrow = 2, byrow = TRUE)
  )

two <- (big2 | small2)
print(two)
ggsave(filename = "/Volumes/SophiaWill/CHP1_SFW_Installation/Plots/For Manuscript/Fig1.jpg", plot = two, width = 15, height = 8, dpi = 600, units = "in", limitsize = FALSE)
ggsave(filename = "/Volumes/SophiaWill/CHP1_SFW_Installation/Plots/For Manuscript/Fig1.svg", plot = two, width = 15, height = 8, dpi = 600, units = "in", device = "svg")
ggsave(filename = "/Volumes/SophiaWill/CHP1_SFW_Installation/Plots/For Manuscript/Fig1.eps", plot = two, width = 15, height = 8, dpi = 600, units = "in", device = "eps")

### FIGURE 3 Pile driving hits transmission loss calculations ####
# Set working directory and read hits
hits <- read.table("/Volumes/SophiaWill/CHP1_SFW_Installation/Plots/For Manuscript/piledriving.txt", sep = "\t", header = TRUE, stringsAsFactors = FALSE)
locations <- read.csv("/Volumes/SophiaWill/CHP1_SFW_Installation/Plots/For Manuscript/pile_locations.csv")
setDT(hits)
setDT(locations)

options(digits=10)  # Increase numeric precision in R

# Format with POSIXct
hits$Begin.Clock.Time <- as.POSIXct(hits$Begin.Clock.Time, format="%H:%M:%OS")
hits$Begin.Date <- as.Date(hits$Begin.Date)
locations$Begin.Date <- as.Date(locations$Begin.Date)

# Valid dates when pile driving for sure occurred
valid_dates <- as.Date(c("2023/06/20", "2023/07/02", "2023/07/06", "2023/07/08", 
                         "2023/07/09", "2023/07/12", "2023/07/21", "2023/07/23", "2023/07/24", # one day that went to next "UTC" day 
                         "2023/07/31", "2023/08/01", "2023/08/03", "2023/08/04", 
                         "2023/08/05", "2023/08/07"))

# Filter for hits that occurred in those valid dates
filtered_hits <- hits[Begin.Date %in% valid_dates]
setorder(filtered_hits, Begin.Time..s.) # Just in case make sure ordered properly

# Merge with location data
merged_hits <- merge(filtered_hits, locations, by = "Begin.Date", all.x = TRUE)

# Remove first row (turning on noise)
merged_hits <- merged_hits[-1,]

# Compute SPL (Sound Pressure Level)
merged_hits <- merged_hits %>%
  mutate(SPL = 20 * log10(RMS.Amp..Pa. / 1e-6))

#write.table(merged_hits, "/Volumes/SophiaWill/CHP1_SFW_Installation/Plots/For Manuscript/pile_driving_and_locations.csv", sep = ",", row.names = FALSE, quote = FALSE)

# Define transmission loss formulas
TL_20logR <- function(D, S, alpha) S - (20 * log10(D) + alpha * D)
TL_15logR <- function(D, S, alpha) S - (15 * log10(D) + alpha * D)
TL_10logR <- function(D, S, alpha) S - (10 * log10(D) + alpha * D)

# Function to compute total error (combined)
total_error <- function(S) {
  distances <- merged_hits$Dist.WARBLER..m..x  # Distance hits
  received_SPL <- merged_hits$SPL  # Observed SPL values
  alpha <- .000062  # Attenuation coefficient
  
  # Calculate predicted SPL values from both transmission loss formulas
  predicted_TL_20logR <- TL_20logR(distances, S, alpha)
  predicted_TL_10logR <- TL_10logR(distances, S, alpha)
  
  # Calculate errors for both models (squared error)
  error_20logR <- sum((predicted_TL_20logR - received_SPL)^2)
  error_10logR <- sum((predicted_TL_10logR - received_SPL)^2)
  
  # Return the combined error (sum of both errors)
  total_error_value <- error_20logR + error_10logR
  return(total_error_value)
}

# Optimize S
best_S <- optimize(total_error, interval = c(150, 220))$minimum
print(best_S)

# Given constants and data
alpha <- .000062
distances <- merged_hits$Dist.WARBLER..m..x
received_SPL <- merged_hits$SPL

# Predicted SPLs for each model using best_S
pred_20 <- TL_20logR(distances, best_S, alpha)
pred_10 <- TL_10logR(distances, best_S, alpha)

# Compute RMSE for each model
RMSE_20 <- sqrt(mean((pred_20 - received_SPL)^2))
RMSE_10 <- sqrt(mean((pred_10 - received_SPL)^2))

# Print results
cat("RMSE (20logR):", RMSE_20, "dB\n")
cat("RMSE (10logR):", RMSE_10, "dB\n")

opt_result <- optimize(total_error, interval = c(150, 220))
# Extract best S and minimum error
best_S <- opt_result$minimum
min_error <- opt_result$objective

# Print results
cat("Best Source Level (S):", best_S, "\n")
cat("Minimum Total Error:", min_error, "\n")

# Create a sequence of distance values (for example, from 1 to 100)
distance <- seq(1, 15000, by = 0.1)
#alpha <- 6.758569088e-07  # Attenuation coefficient
alpha <- 0.000062

# Calculate the transmission loss values based on the formulas
TL_20logR_values <- TL_20logR(distance, best_S, alpha)
TL_15logR_values <- TL_15logR(distance, best_S, alpha)
TL_10logR_values <- TL_10logR(distance, best_S, alpha)

# Create a data frame containing the distance and both TL values
hits_curve <- data.frame(distance, TL_20logR = TL_20logR_values, TL_10logR = TL_10logR_values, TL_15logR = TL_15logR_values)

# Plot measured SPL vs. modeled transmission loss
# Create a long-format data frame for the curves
hits_long <- hits_curve |>
  tidyr::pivot_longer(
    cols = c(TL_10logR, TL_15logR, TL_20logR),
    names_to = "Model",
    values_to = "TL"
  ) |>
  dplyr::mutate(
    Model = dplyr::recode(
      Model,
      TL_10logR = "TL = 10 log R - αR",
      TL_15logR = "TL = 15 log R - αR",
      TL_20logR = "TL = 20 log R - αR"
    )
  )

p <- ggplot() +
  geom_point(
    data = merged_hits,
    aes(x = Dist.WARBLER..m..x, y = SPL),
    alpha = 1,
    color = "darkgrey"
  ) +
  geom_line(
    data = hits_long,
    aes(x = distance, y = TL, color = Model),
    linewidth = 1
  ) +
  scale_color_manual(
    values = c(
      "TL = 10 log R - αR" = "#4477AA",
      "TL = 15 log R - αR" = "#228833",
      "TL = 20 log R - αR" = "#AA3377"
    ),
    name = "Transmission Loss Models"
  ) +
  theme_minimal(base_size = 14) +
  labs(
    x = "Distance from Source (m)",
    y = "SPL (dB)",
    title = ""
  ) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = c(.8,.8),
        legend.box.background = element_rect(color = "black", fill = "white", linewidth = .5, linetype = "solid"))

ggsave(filename = "/Volumes/SophiaWill/CHP1_SFW_Installation/Plots/For Manuscript/Fig3.jpg", plot = p, width = 12, height = 7, dpi = 600, device = "jpg")
ggsave("/Volumes/SophiaWill/CHP1_SFW_Installation/Plots/For Manuscript/Fig3.svg", plot = p, dpi = 600, width = 12, height = 7, units = "in")
ggsave("/Volumes/SophiaWill/CHP1_SFW_Installation/Plots/For Manuscript/Fig3.eps", plot = p, dpi = 600, width = 12, height = 7, units = "in")

# Calculate SL based on RL
merged_hits <- merged_hits %>%
  mutate(SL = SPL + 15 * log10(Dist.WARBLER..m..x) + alpha * Dist.WARBLER..m..x)

# Summarize SL by location
summary_stats <- merged_hits %>%
  group_by(F0.Code.x) %>%  
  summarise(
    mean_SL = mean(SL, na.rm = TRUE),
    median_SL = median(SL, na.rm = TRUE),
    min_SL = min(SL, na.rm = TRUE),
    max_SL = max(SL, na.rm = TRUE),
    sd_SL = sd(SL, na.rm = TRUE)
  )

# Set constant source level
fixed_S <- 200.19

# Function to compute transmission loss with variable coefficient
TL_nlogR <- function(D, S, alpha, n) {
  S - (n * log10(D) + alpha * D)
}

# Function to calculate total squared error for a given n
total_error_variable_n <- function(n) {
  distances <- merged_hits$Dist.WARBLER..m..x
  received_SPL <- merged_hits$SPL
  alpha <- .000062
  
  predicted_SPL <- TL_nlogR(distances, fixed_S, alpha, n)
  sum((predicted_SPL - received_SPL)^2)
}

# Use optimize to find best n between 15 and 20
best_n_result <- optimize(total_error_variable_n, interval = c(10, 20))

# Output the best transmission loss coefficient
cat("Best transmission loss coefficient (n):", best_n_result$minimum, "\n")
cat("Minimum squared error:", best_n_result$objective, "\n")

# Compute median single strike SEL
Summary_per_trubine <- merged_hits %>%
  group_by(Begin.Date) %>%
  summarise(
    median_SEL_SS = median(SEL..dB.),    
    SEL_cum = 10 * log10(sum(10^(SEL..dB. / 10), na.rm = TRUE)),
    n_strikes = n(),
    median_RMS_SPL = median(SPL, na.rm = TRUE),
    min_RMS_SPL = min(SPL, na.rm = TRUE),
    max_RMS_SPL = max(SPL, na.rm = TRUE)
  )

# Compute predicted SPLs for each spreading model using best_S
pred_10 <- TL_10logR(distances, best_S, alpha)
pred_15 <- TL_15logR(distances, best_S, alpha)
pred_20 <- TL_20logR(distances, best_S, alpha)

# Compute RMSE for each model
RMSE_10 <- sqrt(mean((pred_10 - received_SPL)^2))
RMSE_15 <- sqrt(mean((pred_15 - received_SPL)^2))
RMSE_20 <- sqrt(mean((pred_20 - received_SPL)^2))

# Print results neatly
cat("RMSE (10logR):", RMSE_10, "dB\n")
cat("RMSE (15logR):", RMSE_15, "dB\n")
cat("RMSE (20logR):", RMSE_20, "dB\n")


### Break into time frames of PD and no PD --------------------------------------------------------------
piledriving <- read.csv("/Volumes/SophiaWill/CHP1_SFW_Installation/Plots/For Manuscript/pile_driving_and_locations.csv")
piledriving$Begin.Clock.Time <- sub(".* ", "", piledriving$Begin.Clock.Time)

# Set maximum allowed interval and minimum hits for a valid pile-driving instance
maxint <- 300  # seconds
minhits <- 2 # hits, want at least 2 hits within 5 min so we know this wasn't just a random one off impulsive noise

# Compute Time Intervals and Group Pile-Driving Instances
piledriving <- piledriving %>%
  mutate(Interval_diff = c(NA, diff(Begin.Time..s.)),  # Compute time differences between hits
         Pile_Group = cumsum(Interval_diff > maxint | is.na(Interval_diff)), # Create new groups if interval > 10s
  )  

pile_driving_instances <- piledriving %>%
  group_by(Pile_Group) %>%
  summarise(
    Selection = first(Pile_Group),  # Renaming Pile_Group to Selection to make compatible with Raven Pro
    Begin.Date = first(Begin.Date),
    `Begin.Clock.Time` = Begin.Clock.Time[which.min(Begin.Time..s.)],
    `End.Clock.Time` = End.Clock.Time[which.max(End.Time..s.)],
    Hits = n(),
    F0.Code = first(F0.Code.x),
    Category = first(Category.x)
  ) %>%
  filter(Hits > minhits)  # Keep only groups with at least the minimum number of hits

pile_driving_instances <- pile_driving_instances %>%
  mutate(
    # Convert to proper Date with explicit format
    date = as.Date(Begin.Date, format = "%Y-%m-%d"),
    
    # Combine date and time correctly
    DateTimeStart = as.POSIXct(
      paste(date, Begin.Clock.Time),
      format = "%Y-%m-%d %H:%M:%OS",
      tz = "UTC"
    ),
    DateTimeEnd = as.POSIXct(
      paste(date, End.Clock.Time),
      format = "%Y-%m-%d %H:%M:%OS",
      tz = "UTC"
    ),
    duration = as_hms(DateTimeEnd - DateTimeStart)
  )

pile_driving_duration <- pile_driving_instances %>%
  group_by(F0.Code) %>%
  summarise(
    Duration = as_hms(sum(duration))
  ) # note for A11 it crosses into next "UTC day," so duration is 24 - value

### FIGURE 2 SPL by time ####
piledriving <- read.csv("/Volumes/SophiaWill/CHP1_SFW_Installation/Plots/For Manuscript/pile_driving_and_locations.csv")
setDT(piledriving)

# Start time of recording
start_time <- as.POSIXct("2024-06-20 01:00:00", tz = "UTC") 

# Combine 7/23-7/24 that technically went to new UTC day and 7/8-7/9 which is same pile
piledriving <- piledriving %>%
  mutate(
    Begin.Date = case_when(
      Begin.Date %in% c("2023-07-23", "2023-07-24") ~ "2023-07-23",
      Begin.Date %in% c("2023-07-08", "2023-07-09")   ~ "2023-07-08",
      TRUE ~ Begin.Date
    )
  )


# run this version instead of above for Spearman's
piledriving <- piledriving %>%
  mutate(
    Begin.Date = case_when(
      Begin.Date %in% c("2023-07-23", "2023-07-24") ~ "2023-07-23",
      TRUE ~ Begin.Date
    )
  )

# Create hit number per day
piledriving <- piledriving %>%
  group_by(Begin.Date) %>%
  mutate(hit_number = row_number()) %>%
  ungroup()

piledriving <- piledriving %>%
  mutate(
    DateTime = ymd_hms(paste(Begin.Date, End.Clock.Time))
  )

piledriving <- piledriving %>%
  arrange(F0.Code.x, DateTime) %>%              # Ensure correct order
  group_by(F0.Code.x) %>%
  mutate(
    gap_min = difftime(DateTime, lag(DateTime), units = "mins"),
    new_run = if_else(is.na(gap_min) | gap_min > 30, 1, 0),
    run_id = cumsum(new_run)
  )

# Compute rolling mean ONLY within each run
piledriving <- piledriving %>%
  group_by(F0.Code.x, run_id) %>%
  mutate(
    running_avg = zoo::rollmean(SPL, k = 50, fill = NA, align = "center")
  ) %>%
  ungroup()

# Custom palette
palette <- c(
  "orchid1", "gold", "cadetblue2", "tomato", "navy", 
  "coral4", "darkorchid2", "royalblue", "darkgreen", 
  "sienna1", "greenyellow", "deeppink1", "wheat3"
) # to be used for figure 2

bottom <- ggplot(piledriving, aes(x = hit_number, y = SPL, color = F0.Code.x)) +
  geom_line(aes(y = running_avg), size = 0.75) +
  scale_color_manual(values = palette) +
  labs(
    x = "Hit Number",
    y = "SPL (dB re 1 µPa @ 1m)",
    color = "Turbine"
  ) +
  theme_minimal(base_size = 14)

bottom
ggsave(filename = "/Volumes/SophiaWill/CHP1_SFW_Installation/Plots/For Manuscript/Fig2.jpg", plot = bottom, width = 12, height = 7, dpi = 600, device = "jpg")
ggsave("/Volumes/SophiaWill/CHP1_SFW_Installation/Plots/For Manuscript/Fig2.svg", plot = bottom, width = 12, height = 7, units = "in")
ggsave("/Volumes/SophiaWill/CHP1_SFW_Installation/Plots/For Manuscript/Fig2.eps", plot = bottom, width = 12, height = 7, units = "in", device = "eps")

spearman_results <- piledriving %>%
  group_by(Begin.Date) %>%
  summarise(
    test = list(cor.test(hit_number, SPL, method = "spearman"))
  ) %>%
  mutate(
    rho = map_dbl(test, ~ .x$estimate),
    p_value = map_dbl(test, ~ .x$p.value)
  ) %>%
  select(Begin.Date, rho, p_value)

spearman_results

### Look at background noise for distance calculations ####
rms_spl_data_list <- read.csv("/Volumes/SophiaWill/CHP1_SFW_Installation/Plots/For Manuscript/background_rms_spl.csv")

# quantiles
quantile(rms_spl_data_list$rms_spl, 0.1)
quantile(rms_spl_data_list$rms_spl, 0.5)
quantile(rms_spl_data_list$rms_spl, 0.9)

alpha <- .000062 # if use equation with freq = 1 kHz

# Function
f <- function(R) {
  200 - (15 * log10(R) + alpha * R) - 124
}

# Find all roots in a range
roots <- uniroot.all(f, c(1, 1e7))
print(roots/1000)
### Open and clean fish tracks data frame --------------------------------------------------------------
fish_tracks <- read.csv("/Volumes/SophiaWill/CHP1_SFW_Installation/Plots/For Manuscript/SRWFishTracks.csv", header = TRUE)

# Convert to data.table
setDT(pile_driving_instances)
setDT(fish_tracks)

# Start and End datetime  in POSIXct format
fish_tracks[, DateTime_S := as.POSIXct(paste(Date_S, Time_S), format="%Y%m%d %H:%M:%OS", tz="UTC")]
fish_tracks[, DateTime_E := as.POSIXct(paste(Date_E, Time_E), format="%Y%m%d %H:%M:%OS", tz="UTC")]

# Vectorize vertical speed
fish_tracks[, vertical_speed := Speed_4D_mean_unsmoothed * sin(Direction_vertical * pi / 180)]

# Vectorize horizontal speed
fish_tracks[, horizontal_speed := Speed_4D_mean_unsmoothed * cos(Direction_vertical * pi / 180)]

# get unique dates
unique_dates <- unique(pile_driving_instances$Begin.Date)

# initialize an empty dataframe to store results
pile_driving <- data.frame(
  date = as.Date(character()),
  start_time = as.POSIXct(character(), tz = "UTC"),
  end_time = as.POSIXct(character(), tz = "UTC")
)

### Compare PD and No PD --------------------------------------------------------------
# loop through each date and grab start/end timestamps
for (d in unique_dates) {
  day_data <- pile_driving_instances[pile_driving_instances$Begin.Date == d,]
  start_time <- day_data$Begin.Clock.Time[1]
  end_time <- day_data$End.Clock.Time[nrow(day_data)]
  pile_driving <- rbind(pile_driving, data.frame(
    date = d,
    start_time = start_time,
    end_time = end_time
  ))
}

# make date time combined start and end
pile_driving <- pile_driving %>%
  mutate(
    # Convert to proper Date with explicit format
    date = as.Date(date, format = "%Y-%m-%d"),  
    
    # Combine date and time correctly
    DateTimeStart = as.POSIXct(
      paste(date, start_time),
      format = "%Y-%m-%d %H:%M:%OS",
      tz = "UTC"
    ),
    DateTimeEnd = as.POSIXct(
      paste(date, end_time),
      format = "%Y-%m-%d %H:%M:%OS",
      tz = "UTC"
    )
  )

# need to manually combine the 7/23 and 7/24 because they are same pile, just UTC day cutting off
pile_driving[8,5] <- pile_driving[9,5] # place end of 7/24 with start of 7/23
pile_driving <- pile_driving[-9,] # remove the 7/24 line

### Break into periods of before / ramp / during / end --------------------------------------------------------------
pile_driving$Category <- pile_driving_instances$Category[match(pile_driving$date, pile_driving_instances$Begin.Date)]

pile_driving <- pile_driving %>%
  mutate(
    duration = as_hms(DateTimeEnd - DateTimeStart)
  )

# Define time periods for before / ramp / during / end
pile_driving <- pile_driving %>%
  mutate(
    before = DateTimeStart - as.difftime(2, units = "hours"),
    ramp_end = DateTimeStart + as.difftime(0.5, units = "hours"),
    # d1 lasts up to 1 hour after ramp_end, but no later than DateTimeEnd
    d1_end = pmin(ramp_end + as.difftime(1, units = "hours"), DateTimeEnd),
    # d2 lasts up to 1 hour after d1_end, but no later than DateTimeEnd
    d2_end = pmin(d1_end + as.difftime(1, units = "hours"), DateTimeEnd),
    d3_end = pmin(d2_end + as.difftime(1, units = "hours"), DateTimeEnd),
    d4_end = pmin(d3_end + as.difftime(1, units = "hours"), DateTimeEnd),
    d5_end = pmin(d4_end + as.difftime(1, units = "hours"), DateTimeEnd),
    d6_end = pmin(d5_end + as.difftime(1, units = "hours"), DateTimeEnd),
    d7_end = pmin(d6_end + as.difftime(1, units = "hours"), DateTimeEnd),
    d8_end = pmin(d7_end + as.difftime(1, units = "hours"), DateTimeEnd),
    d9_end = pmin(d8_end + as.difftime(1, units = "hours"), DateTimeEnd),
    d10_end = pmin(d9_end + as.difftime(1, units = "hours"), DateTimeEnd),
    after = DateTimeEnd + as.difftime(2, units = "hours")
  )

# Set data.table
setDT(pile_driving)

# Create combined periods with treatment labels: before, ramp-up, during, after
pd_long <- rbindlist(list(
  pile_driving[, .(start = before, end = DateTimeStart, treatment = "b", Category)],
  pile_driving[, .(start = DateTimeStart, end = ramp_end, treatment = "r", Category)],
  pile_driving[, .(start = ramp_end, end = d1_end, treatment = "d1", Category)],
  pile_driving[, .(start = d1_end, end = d2_end, treatment = "d2", Category)],
  pile_driving[, .(start = d2_end, end = d3_end, treatment = "d3", Category)],
  pile_driving[, .(start = d3_end, end = d4_end, treatment = "d4", Category)],
  pile_driving[, .(start = d4_end, end = d5_end, treatment = "d5", Category)],
  pile_driving[, .(start = d5_end, end = d6_end, treatment = "d6", Category)],
  pile_driving[, .(start = d6_end, end = d7_end, treatment = "d7", Category)],
  pile_driving[, .(start = d7_end, end = d8_end, treatment = "d8", Category)],
  pile_driving[, .(start = d8_end, end = d9_end, treatment = "d9", Category)],
  pile_driving[, .(start = d9_end, end = d10_end, treatment = "d10", Category)],
  pile_driving[, .(start = DateTimeEnd, end = after, treatment = "a", Category)]
))

# Set keys
setkey(pd_long, start, end)
setkey(fish_tracks, DateTime_S, DateTime_E)

# Run foverlaps 
overlaps <- foverlaps(fish_tracks, pd_long, type = "any", nomatch = 0L)
overlaps_unique <- overlaps[order(DateTime_S, start), .SD[1], by = .(DateTime_S, DateTime_E)]

# Merge treatment back
fish_tracks <- merge(
  fish_tracks,
  overlaps_unique[, .(DateTime_S, DateTime_E, treatment, Category)],
  by = c("DateTime_S", "DateTime_E"),
  all.x = TRUE
)

# Check observations by treatment
track_counts <- dplyr::count(fish_tracks, treatment, name = "track_count")

# Summarize by treatment
summary_by_treatment <- fish_tracks %>%
  group_by(treatment) %>%
  summarise(
    mean_speed = mean(Speed_4D_mean_unsmoothed, na.rm = TRUE),
    sd_speed = sd(Speed_4D_mean_unsmoothed, na.rm = TRUE),
    mean_tortuosity = mean(Tortuosity_3D, na.rm = TRUE),
    sd_tortuosity = sd(Tortuosity_3D, na.rm = TRUE),
    mean_changedepth = mean(Fish_track_change_in_depth, na.rm = TRUE),
    sd_changedepth = sd(Fish_track_change_in_depth, na.rm = TRUE)
  )

### FIGURE 4 Distribution of fish tracks--------------------------------------------------------------
# Extract date from DateTime_S and count number of tracks per day
daily_fish_tracks <- fish_tracks %>%
  mutate(track_date = as.Date(DateTime_S)) %>%
  group_by(track_date) %>%
  summarise(fish_track_count = n()) %>%
  arrange(track_date)

# Add a flag for pile driving day
daily_fish_tracks <- daily_fish_tracks %>%
  mutate(pile_driving_day = ifelse(track_date %in% pile_driving$date, "Pile Driving", "No Pile Driving"))

daily_fish_tracks <- daily_fish_tracks %>%
  mutate(period = ifelse(track_date < as.Date("2023-08-08"),
                         "Before",
                         "After"))
wilcox.test(fish_track_count ~ period, data = daily_fish_tracks)
shapiro.test(daily_fish_tracks$fish_track_count)

### Load fish tracks for lander comparisons ####
SFW <- read.csv("/Volumes/SophiaWill/CHP1_SFW_Installation/Plots/For Manuscript/SFWFishTracks.csv")
SRW <- read.csv("/Volumes/SophiaWill/CHP1_SFW_Installation/Plots/For Manuscript/SRWFishTracks.csv")
setDT(SFW)
setDT(SRW)

SFW[, DateTime_S := as.POSIXct(paste(Date_S, Time_S), format="%Y%m%d %H:%M:%OS", tz="UTC")]
SRW[, DateTime_S := as.POSIXct(paste(Date_S, Time_S), format="%Y%m%d %H:%M:%OS", tz="UTC")]

SFW_daily_fish_tracks <- SFW %>%
  mutate(track_date = as.Date(DateTime_S)) %>%
  group_by(track_date) %>%
  summarise(fish_track_count = n(),
            meanTS = mean(TS_mean)) %>%
  arrange(track_date)

SRW_daily_fish_tracks <- SRW %>%
  mutate(track_date = as.Date(DateTime_S)) %>%
  group_by(track_date) %>%
  summarise(fish_track_count = n(),
            meanTS = mean(TS_mean)) %>%
  arrange(track_date)

tracks <- ggplot() +
  
  # SFW = fixed color
  geom_point(
    data = SFW_daily_fish_tracks,
    aes(x = track_date, y = fish_track_count, color = "SFW"),
    size = 2
  ) +
  
  # SRW = colored by pile driving activity
  geom_point(
    data = daily_fish_tracks,
    aes(x = track_date, y = fish_track_count, color = pile_driving_day),
    size = 2
  ) +
  
  geom_vline(
    xintercept = as.Date("2023-08-08"),
    linetype = "dashed", color = "#EE6677", linewidth = 0.8
  ) +
  
  scale_color_manual(
    name = "Location / Activity",
    values = c(
      "SFW" = "#228833",
      "Pile Driving" = "#EE6677",
      "No Pile Driving" = "#4477AA"
    ),
    labels = c(
      "SFW" = "SFW – No Pile Driving",
      "Pile Driving" = "SRW – Pile Driving",
      "No Pile Driving" = "SRW – No Pile Driving"
    )
  ) +
  annotate("text",
           x = as.Date("2023-07-15"),   
           y = max(daily_fish_tracks$fish_track_count) * 0.9, 
           label = "DURING",
           size = 4,
           color = "black",
           alpha = 1) +
  annotate("text",
           x = as.Date("2023-09-01"),   
           y = max(daily_fish_tracks$fish_track_count) * 0.9, 
           label = "AFTER",
           size = 4,
           color = "black",
           alpha = 1) +
  theme_minimal(base_size = 14) +
  labs(
    x = "Date",
    y = "Number of fish tracks"
  ) +
  scale_x_date(date_labels = "%b %Y", date_breaks = "1 month") +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = c(.8,.8),
    legend.box.background = element_rect(
      color = "black", fill = "white", linewidth = .5
    )
  )

ggsave("/Volumes/SophiaWill/CHP1_SFW_Installation/Plots/For Manuscript/Fig4.jpg", plot = tracks, width = 12, height = 8, units = "in")
ggsave("/Volumes/SophiaWill/CHP1_SFW_Installation/Plots/For Manuscript/Fig4.svg", plot = tracks, width = 12, height = 8, units = "in")
ggsave("/Volumes/SophiaWill/CHP1_SFW_Installation/Plots/For Manuscript/Fig4.eps", plot = tracks, width = 12, height = 8, units = "in")

### Half hour time treatments--------------------------------------------------------------
# Add before, ramp, during, and after half-hour interval endpoints
pile_driving <- pile_driving %>%
  mutate(
    # Define the start of "before" period (4 hours before start)
    before_start = DateTimeStart - as.difftime(2, units = "hours"),
    
    # Half-hour before intervals (each one builds on the previous)
    before1_end = before_start + as.difftime(0.5, units = "hours"),
    before2_end = before1_end + as.difftime(0.5, units = "hours"),
    before3_end = before2_end + as.difftime(0.5, units = "hours"),
    before4_end = before3_end + as.difftime(0.5, units = "hours"),
    
    # Ramp end (0.5 hours after start)
    ramp_end = DateTimeStart + as.difftime(0.5, units = "hours"),
    
    # During intervals (0.5 to 7.5 hours after start, capped at DateTimeEnd)
    d1_end = pmin(ramp_end + as.difftime(0.5, units = "hours"), DateTimeEnd),
    d2_end = pmin(d1_end + as.difftime(0.5, units = "hours"), DateTimeEnd),
    d3_end = pmin(d2_end + as.difftime(0.5, units = "hours"), DateTimeEnd),
    d4_end = pmin(d3_end + as.difftime(0.5, units = "hours"), DateTimeEnd),
    d5_end = pmin(d4_end + as.difftime(0.5, units = "hours"), DateTimeEnd),
    d6_end = pmin(d5_end + as.difftime(0.5, units = "hours"), DateTimeEnd),
    d7_end = pmin(d6_end + as.difftime(0.5, units = "hours"), DateTimeEnd),
    d8_end = pmin(d7_end + as.difftime(0.5, units = "hours"), DateTimeEnd),
    d9_end = pmin(d8_end + as.difftime(0.5, units = "hours"), DateTimeEnd),
    d10_end = pmin(d9_end + as.difftime(0.5, units = "hours"), DateTimeEnd),
    d11_end = pmin(d10_end + as.difftime(0.5, units = "hours"), DateTimeEnd),
    d12_end = pmin(d11_end + as.difftime(0.5, units = "hours"), DateTimeEnd),
    d13_end = pmin(d12_end + as.difftime(0.5, units = "hours"), DateTimeEnd),
    d14_end = pmin(d13_end + as.difftime(0.5, units = "hours"), DateTimeEnd),
    d15_end = pmin(d14_end + as.difftime(0.5, units = "hours"), DateTimeEnd),
    
    # After intervals (up to 4 hours after DateTimeEnd)
    after1_end = DateTimeEnd + as.difftime(0.5, units = "hours"),
    after2_end = after1_end + as.difftime(0.5, units = "hours"),
    after3_end = after2_end + as.difftime(0.5, units = "hours"),
    after4_end = after3_end + as.difftime(0.5, units = "hours")
  )

# Create combined periods with treatment labels: before, ramp-up, during, after
pd_half <- rbindlist(list(
  # Before intervals (4h before to DateTimeStart)
  pile_driving[, .(start = before_start, end = before1_end, treatment.half = "before1", Category)],
  pile_driving[, .(start = before1_end, end = before2_end, treatment.half = "before2", Category)],
  pile_driving[, .(start = before2_end, end = before3_end, treatment.half = "before3", Category)],
  pile_driving[, .(start = before3_end, end = before4_end, treatment.half = "before4", Category)],
  
  # Ramp-up period (not required, but can be added if needed)
  pile_driving[, .(start = DateTimeStart, end = ramp_end, treatment.half = "ramp", Category)],
  
  # During intervals (0.5h to 7.5h after start)
  pile_driving[, .(start = ramp_end, end = d1_end, treatment.half = "during1", Category)],
  pile_driving[, .(start = d1_end, end = d2_end, treatment.half = "during2", Category)],
  pile_driving[, .(start = d2_end, end = d3_end, treatment.half = "during3", Category)],
  pile_driving[, .(start = d3_end, end = d4_end, treatment.half = "during4", Category)],
  pile_driving[, .(start = d4_end, end = d5_end, treatment.half = "during5", Category)],
  pile_driving[, .(start = d5_end, end = d6_end, treatment.half = "during6", Category)],
  pile_driving[, .(start = d6_end, end = d7_end, treatment.half = "during7", Category)],
  pile_driving[, .(start = d7_end, end = d8_end, treatment.half = "during8", Category)],
  pile_driving[, .(start = d8_end, end = d9_end, treatment.half = "during9", Category)],
  pile_driving[, .(start = d9_end, end = d10_end, treatment.half = "during10", Category)],
  pile_driving[, .(start = d10_end, end = d11_end, treatment.half = "during11", Category)],
  pile_driving[, .(start = d11_end, end = d12_end, treatment.half = "during12", Category)],
  pile_driving[, .(start = d12_end, end = d13_end, treatment.half = "during13", Category)],
  pile_driving[, .(start = d13_end, end = d14_end, treatment.half = "during14", Category)],
  pile_driving[, .(start = d14_end, end = d15_end, treatment.half = "during15", Category)],
  
  # After intervals (DateTimeEnd to +4h)
  pile_driving[, .(start = DateTimeEnd, end = after1_end, treatment.half = "after1", Category)],
  pile_driving[, .(start = after1_end, end = after2_end, treatment.half = "after2", Category)],
  pile_driving[, .(start = after2_end, end = after3_end, treatment.half = "after3", Category)],
  pile_driving[, .(start = after3_end, end = after4_end, treatment.half = "after4", Category)]
))

# Ensure keys are set for foverlaps
setkey(pd_half, start, end)
setkey(fish_tracks, DateTime_S, DateTime_E)

# Perform overlap join
overlaps_half <- foverlaps(fish_tracks, pd_half, type = "any", nomatch = 0L)

# Keep the first overlapping interval per fish track
overlaps_half_unique <- overlaps_half[order(DateTime_S, start), .SD[1], by = .(DateTime_S, DateTime_E)]

# Merge treatment.half back into fish_tracks
fish_tracks <- merge(
  fish_tracks,
  overlaps_half_unique[, .(DateTime_S, DateTime_E, treatment.half, Category)],
  by = c("DateTime_S", "DateTime_E"),
  all.x = TRUE
)

# Check observations by treatment
track_counts_half <- fish_tracks %>%
  dplyr::count(treatment.half, name = "track_count")

# Summarize by treatment
summary_by_treatment_half <- fish_tracks %>%
  group_by(treatment.half) %>%
  summarise(
    mean_speed = mean(Speed_4D_mean_unsmoothed, na.rm = TRUE),
    sd_speed = sd(Speed_4D_mean_unsmoothed, na.rm = TRUE),
    mean_tortuosity = mean(Tortuosity_3D, na.rm = TRUE),
    sd_tortuosity = sd(Tortuosity_3D, na.rm = TRUE),
    mean_changedepth = mean(Fish_track_change_in_depth, na.rm = TRUE),
    sd_changedepth = sd(Fish_track_change_in_depth, na.rm = TRUE)
  )

# Remove NA treatments for plotting
fish_tracks_clean_half <- fish_tracks %>%
  filter(!is.na(treatment.half))

# Set the correct treatment.half factor levels in chronological order
fish_tracks_clean_half$treatment.half <- factor(
  fish_tracks_clean_half$treatment.half,
  levels = c(paste0("before", 1:4), # save time instead of typing
             "ramp",
             paste0("during", 1:15),
             paste0("after", 1:4))
)

### All during into one ####
# Collapse all "d" periods into one
pd_long <- pd_long %>%
  mutate(
    treatment = ifelse(treatment %in% paste0("d", 1:10), "d", treatment)
  )

# Set keys again (just in case)
setkey(pd_long, start, end)
setkey(fish_tracks, DateTime_S, DateTime_E)

# Run foverlaps again with collapsed periods
overlaps <- foverlaps(fish_tracks, pd_long, type = "any", nomatch = 0L)
overlaps_unique <- overlaps[order(DateTime_S, start), .SD[1], by = .(DateTime_S, DateTime_E)]

# Merge treatment back
fish_tracks <- merge(
  fish_tracks,
  overlaps_unique[, .(DateTime_S, DateTime_E, treatment, Category)],
  by = c("DateTime_S", "DateTime_E"),
  all.x = TRUE
)

# Check observations by treatment
track_counts <- fish_tracks %>%
  dplyr::count(treatment.y, name = "track_count")

fish_tracks_clean <- fish_tracks %>%
  filter((!is.na(treatment.y)))

summary_by_treatment <- fish_tracks_clean %>%
  group_by(treatment.y) %>%
  summarise(
    mean_tort = mean(Tortuosity_3D, na.rm = TRUE),
    sd_tort = sd(Tortuosity_3D, na.rm = TRUE),
    mean_depth = mean(Fish_track_change_in_depth, na.rm = TRUE),
    sd_depth = sd(Fish_track_change_in_depth, na.rm = TRUE),
    mean_vert = mean(vertical_speed, na.rm = TRUE),
    sd_vert = sd(vertical_speed, na.rm = TRUE),
    mean_targ = mean(Target_depth_mean, na.rm = TRUE),
    sd_targ = sd(Target_depth_mean, na.rm = TRUE),
    mean_meanspeed = mean(Speed_4D_mean_unsmoothed, na.rm = TRUE),
    sd_meanspeed = sd(Speed_4D_mean_unsmoothed, na.rm = TRUE),
    mean_maxspeed = mean(Speed_4D_max_unsmoothed, na.rm = TRUE),
    sd_maxspeed = sd(Speed_4D_max_unsmoothed, na.rm = TRUE)
  )

# Remove NAs
fish_tracks_clean <- fish_tracks %>%
  filter(!is.na(treatment.y))

### Remove outliers ####
# List of variables to check for outliers
vars_to_check <- c(
  "Speed_2D_mean_unsmoothed", 
  "Speed_4D_mean_unsmoothed", 
  "Speed_4D_max_unsmoothed", 
  "Tortuosity_3D", 
  "Target_depth_mean", 
  "Fish_track_change_in_depth"
)

# Function to replace outliers with NA using IQR
replace_outliers_iqr <- function(x) {
  Q1 <- quantile(x, 0.25, na.rm = TRUE)
  Q3 <- quantile(x, 0.75, na.rm = TRUE)
  IQR_val <- Q3 - Q1
  lower <- Q1 - 1.5 * IQR_val
  upper <- Q3 + 1.5 * IQR_val
  x[x < lower | x > upper] <- NA
  return(x)
}

# Create new data frame with outliers replaced by NA
# IQR method
fish_tracks_outlier_free_iqr <- fish_tracks_clean %>%
  mutate(across(all_of(vars_to_check), replace_outliers_iqr))

# Function to replace outliers with NA using Z-score
replace_outliers_z <- function(x, threshold = 3) {
  z <- (x - mean(x, na.rm = TRUE)) / sd(x, na.rm = TRUE)
  x[abs(z) > threshold] <- NA
  return(x)
}

# Create new dataframe with Z-score outliers removed
fish_tracks_outlier_free_z <- fish_tracks_clean %>%
  mutate(across(all_of(vars_to_check), replace_outliers_z))

sum(is.na(fish_tracks_outlier_free_iqr$Speed_2D_mean_unsmoothed)) #9
sum(is.na(fish_tracks_outlier_free_z$Speed_2D_mean_unsmoothed)) #5
sum(is.na(fish_tracks_outlier_free_iqr$Speed_4D_mean_unsmoothed)) #3
sum(is.na(fish_tracks_outlier_free_z$Speed_4D_mean_unsmoothed)) #3
sum(is.na(fish_tracks_outlier_free_iqr$Speed_4D_max_unsmoothed)) #5
sum(is.na(fish_tracks_outlier_free_z$Speed_4D_max_unsmoothed)) #3
sum(is.na(fish_tracks_outlier_free_iqr$Tortuosity_3D)) #18
sum(is.na(fish_tracks_outlier_free_z$Tortuosity_3D)) #5
sum(is.na(fish_tracks_outlier_free_iqr$Fish_track_change_in_depth)) #19
sum(is.na(fish_tracks_outlier_free_z$Fish_track_change_in_depth)) #4
sum(is.na(fish_tracks_outlier_free_iqr$Target_depth_mean)) #4
sum(is.na(fish_tracks_outlier_free_z$Target_depth_mean)) #0

### Test for significance ####
summary_by_treatment <- fish_tracks_outlier_free_z %>%
  group_by(treatment.y) %>%
  summarise(
    mean_tort = mean(Tortuosity_3D, na.rm = TRUE),
    sd_tort = sd(Tortuosity_3D, na.rm = TRUE),
    mean_depth = mean(Fish_track_change_in_depth, na.rm = TRUE),
    sd_depth = sd(Fish_track_change_in_depth, na.rm = TRUE),
    mean_vert = mean(Speed_2D_mean_unsmoothed, na.rm = TRUE),
    sd_vert = sd(Speed_2D_mean_unsmoothed, na.rm = TRUE),
    mean_targ = mean(Target_depth_mean, na.rm = TRUE),
    sd_targ = sd(Target_depth_mean, na.rm = TRUE),
    mean_meanspeed = mean(Speed_4D_mean_unsmoothed, na.rm = TRUE),
    sd_meanspeed = sd(Speed_4D_mean_unsmoothed, na.rm = TRUE),
    mean_maxspeed = mean(Speed_4D_max_unsmoothed, na.rm = TRUE),
    sd_maxspeed = sd(Speed_4D_max_unsmoothed, na.rm = TRUE)
  )

kruskal.test(Speed_4D_mean_unsmoothed ~ treatment.y, data = fish_tracks_outlier_free_z) # .6
kruskal.test(Speed_4D_max_unsmoothed ~ treatment.y, data = fish_tracks_outlier_free_z) # .4

kruskal.test(Target_depth_mean ~ treatment.y, data = fish_tracks_outlier_free_z) # .02
dunnTest(Target_depth_mean ~ treatment.y, data = fish_tracks_outlier_free_z, method = "bonferroni")

kruskal.test(Tortuosity_3D ~ treatment.y, data = fish_tracks_outlier_free_z) # < .08
dunnTest(Tortuosity_3D ~ treatment.y, data = fish_tracks_outlier_free_z, method = "bonferroni")

kruskal.test(Fish_track_change_in_depth ~ treatment.y, data = fish_tracks_outlier_free_z) # < .001
dunnTest(Fish_track_change_in_depth ~ treatment.y, data = fish_tracks_outlier_free_z, method = "bonferroni")

kruskal.test(Speed_2D_mean_unsmoothed ~ treatment.y, data = fish_tracks_outlier_free_z) # .15

hist(fish_tracks_clean$Target_depth_mean)
shapiro.test(fish_tracks_clean$Target_depth_mean)

fish_tracks_clean$treatment <- factor(fish_tracks_clean$treatment, levels = c("b", "r", "d", "a"))
fish_tracks_clean$treatment.y <- factor(fish_tracks_clean$treatment.y, levels = c("b", "r", "d", "a"))

### FIGURE 5 Plot TS of fish tracks --------------------------------------------------------------
SRW <- SRW %>%
  mutate(track_date = as.Date(DateTime_S),
         week = floor_date(track_date, "week"))  # round down to week start

# Make sure TS_mean is numeric
SRW <- SRW %>%
  mutate(TS_mean = as.numeric(TS_mean)) %>%
  filter(!is.na(TS_mean))

# n per week
n_labels <- SRW %>%
  group_by(week) %>%
  summarise(n = n()) %>%
  ungroup()

# Make histogram bins and percentages per week
# Bin width = 0.5 dB
SRW_hist <- SRW %>%
  group_by(week) %>%
  mutate(bin = cut(
    TS_mean,
    breaks = seq(floor(min(TS_mean)), ceiling(max(TS_mean)), by = 0.5),
    include.lowest = TRUE
  )) %>%
  dplyr::count(week, bin) %>%
  group_by(week) %>%
  mutate(pct = 100 * n / sum(n)) %>%
  ungroup() %>%
  mutate(bin_mid = as.numeric(sub("\\((.+),.*", "\\1", bin)) + 0.25)  # midpoint for 0.5-dB bins

# Plot with facet wrap
facetTS <- ggplot(SRW_hist, aes(x = bin_mid, y = pct, fill = factor(week))) +
  geom_col(color = "#4477AA", alpha = 1, width = 0.5) +
  facet_wrap(~ week, nrow = 3) +
  # Add n labels at top of each facet
  geom_text(
    data = n_labels,
    aes(x = -45, y = 16, label = paste0("n = ", n), group = week),
    hjust = -0.1, vjust = 1,
    size = 4,
    inherit.aes = FALSE
  ) +
  geom_text(
    data = SRW_hist,
    aes(x = -50, y = 18, label = paste0(week), group = week),
    hjust = -0.1, vjust = 1,
    size = 4,
    inherit.aes = FALSE
  ) +
  scale_fill_manual(values = rep("#4477AA", length(unique(SRW$week)))) +
  labs(
    x = "Target Strength (dB)",
    y = "Percentage (%)"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    panel.grid.minor = element_blank(),
    #   panel.grid.major.x = element_blank(),
    #   strip.background = element_blank(),
    strip.text = element_blank(),
    legend.position = "none"
  )

ggsave("/Volumes/SophiaWill/CHP1_SFW_Installation/Plots/For Manuscript/Fig5.jpg", plot = facetTS, width = 12, height = 8, units = "in")
ggsave("/Volumes/SophiaWill/CHP1_SFW_Installation/Plots/For Manuscript/Fig5.svg", plot = facetTS, width = 12, height = 8, units = "in")
ggsave("/Volumes/SophiaWill/CHP1_SFW_Installation/Plots/For Manuscript/Fig5.eps", plot = facetTS, width = 12, height = 8, units = "in")

deployment_bins <- fish_tracks_clean %>%
  mutate(
    TS_bin = case_when(
      TS_mean >= -60 & TS_mean < -50 ~ "-60 to -50",
      TS_mean >= -50 & TS_mean < -40 ~ "-50 to -40",
      TS_mean >= -40 & TS_mean < -30 ~ "-40 to -30",
      TS_mean >= -30                ~ "> -30",
      TRUE                          ~ "< -60"
    )
  ) %>%
  filter(TS_bin != "< -60") %>%   # optional: drop weaker than -60
  count(TS_bin) %>%
  mutate(pct = 100 * n / sum(n))

deployment_bins
### FIGURE 6 Behavioral plot 2 panel ####
plot_half_hour_variable <- function(data, variable_name, y_label, filename, flip_y = FALSE) {
  var_sym <- rlang::sym(variable_name)
  
  # Factor order
  data$treatment.y <- factor(data$treatment.y, levels = c("b", "r", "d", "a"))
  data$treatment.half <- factor(
    data$treatment.half,
    levels = c(
      paste0("before", 1:4),
      "ramp",
      paste0("during", 1:14),
      paste0("after", 1:4)
    )
  )
  
  # Colors
  base_colors <- c(
    "b" = "#EE6677","r" = "#228833", "d" = "#4477AA","a" = "#AA3377"
  )
  
  left_axis_colors <- unname(base_colors[levels(data$treatment.y)])
  
  right_axis_colors <- c(
    rep(base_colors["b"], 4),
    base_colors["r"],
    rep(base_colors["d"], 14),
    rep(base_colors["a"], 4)
  )
  
  # Left panel
  p1 <- ggplot(
    data,
    aes(x = treatment.y, y = !!var_sym, color = treatment.y)
  ) +
    geom_jitter(width = 0.3, alpha = 1, size = 2) +
    scale_color_manual(values = base_colors) +
    scale_x_discrete(
      labels = c("b" = "Before", "r" = "Ramp", "d" = "During", "a" = "After")
    ) +
    labs(x = "Time Period", y = y_label) +
    theme_minimal(base_size = 14) +
    theme(
      legend.position = "none",
      axis.text.x = element_text(
        angle = 45,
        hjust = 1,
        color = left_axis_colors
      ),
      plot.margin = margin(10, 10, 10, 10)
    )
  
  if (flip_y) {
    p1 <- p1 + scale_y_reverse()
  }
  
  # Right panel
  half_colors <- c(
    setNames(rep(base_colors["b"], 4), paste0("before", 1:4)),
    "ramp" = unname(base_colors["r"]),
    setNames(rep(base_colors["d"], 14), paste0("during", 1:14)),
    setNames(rep(base_colors["a"], 4), paste0("after", 1:4))
  )
  
  p2 <- ggplot(
    data,
    aes(x = treatment.half, y = !!var_sym, color = treatment.half)
  ) +
    geom_jitter(width = 0.3, alpha = 1, size = 1.5) +
    scale_color_manual(values = half_colors, na.translate = FALSE) +
    scale_x_discrete(
      labels = c(
        setNames(paste("Before", 1:4), paste0("before", 1:4)),
        "ramp" = "Ramp",
        setNames(paste("During", 1:14), paste0("during", 1:14)),
        setNames(paste("After", 1:4), paste0("after", 1:4))
      )
    ) +
    labs(x = "Half-Hour Time Period", y = NULL) +
    theme_minimal(base_size = 14) +
    theme(
      legend.position = "none",
      axis.text.x = element_text(
        angle = 45,
        hjust = 1,
        color = right_axis_colors
      ),
      plot.margin = margin(10, 10, 10, 10)
    )
  
  if (flip_y) {
    p2 <- p2 + scale_y_reverse()
  }
  
  combined_plot <- p1 + p2 + patchwork::plot_layout(ncol = 2)
  ggsave(filename, plot = combined_plot, width = 10, height = 4, dpi = 600)
  return(combined_plot)
}

plot_half_hour_variable(
  data = fish_tracks_outlier_free_z,
  variable_name = "Target_depth_mean",
  y_label = "Mean target depth (m)",
  filename = "/Volumes/SophiaWill/CHP1_SFW_Installation/Plots/For Manuscript/Fig6_Mean_depth.jpg",
  flip_y = TRUE
)

plot_half_hour_variable(
  data = fish_tracks_outlier_free_z,
  variable_name = "Target_depth_mean",
  y_label = "Mean target depth (m)",
  filename = "/Volumes/SophiaWill/CHP1_SFW_Installation/Plots/For Manuscript/Fig6_Mean_depth.svg",
  flip_y = TRUE
)

plot_half_hour_variable(
  data = fish_tracks_outlier_free_z,
  variable_name = "Target_depth_mean",
  y_label = "Mean target depth (m)",
  filename = "/Volumes/SophiaWill/CHP1_SFW_Installation/Plots/For Manuscript/Fig6_Mean_depth.eps",
  flip_y = TRUE
)

plot_half_hour_variable(
  data = fish_tracks_outlier_free_z,
  variable_name = "Speed_4D_mean_unsmoothed",
  y_label = "Mean speed (m/s)",
  filename = "/Volumes/SophiaWill/CHP1_SFW_Installation/Plots/For Manuscript/Fig6_Mean_speed.jpg"
)

plot_half_hour_variable(
  data = fish_tracks_outlier_free_z,
  variable_name = "Speed_4D_mean_unsmoothed",
  y_label = "Mean speed (m/s)",
  filename = "/Volumes/SophiaWill/CHP1_SFW_Installation/Plots/For Manuscript/Fig6_Mean_speed.svg"
)

plot_half_hour_variable(
  data = fish_tracks_outlier_free_z,
  variable_name = "Speed_4D_mean_unsmoothed",
  y_label = "Mean speed (m/s)",
  filename = "/Volumes/SophiaWill/CHP1_SFW_Installation/Plots/For Manuscript/Fig6_Mean_speed.eps"
)

plot_half_hour_variable(
  data = fish_tracks_outlier_free_z,
  variable_name = "Tortuosity_3D",
  y_label = "Tortuosity",
  filename = "/Volumes/SophiaWill/CHP1_SFW_Installation/Plots/For Manuscript/Fig6_Tortuosity.jpg"
)

plot_half_hour_variable(
  data = fish_tracks_outlier_free_z,
  variable_name = "Tortuosity_3D",
  y_label = "Tortuosity",
  filename = "/Volumes/SophiaWill/CHP1_SFW_Installation/Plots/For Manuscript/Fig6_Tortuosity.svg"
)

plot_half_hour_variable(
  data = fish_tracks_outlier_free_z,
  variable_name = "Tortuosity_3D",
  y_label = "Tortuosity",
  filename = "/Volumes/SophiaWill/CHP1_SFW_Installation/Plots/For Manuscript/Fig6_Tortuosity.eps"
)
### FIGURE 7 Variance ####
fish_tracks <- fish_tracks %>%
  mutate(Date = lubridate::date(DateTime_S))

daily_stats <- fish_tracks %>%
  mutate(Period = if_else(Date < as.Date("2023-08-08"),
                          "Pile driving", "Post pile driving")) %>%
  group_by(Date) %>%
  summarise(
    Period = first(Period),  # keep period for each date
    mean_speed = mean(Speed_4D_mean_unsmoothed),
    se_meanspeed = sd(Speed_4D_mean_unsmoothed) / sqrt(sum(!is.na(Speed_4D_mean_unsmoothed))),
    sd_meanspeed = sd(Speed_4D_mean_unsmoothed),
    norm_meanspeed = se_meanspeed/mean_speed,
    norm_sdmean = sd_meanspeed / mean_speed,
    var_mean = var(Speed_4D_mean_unsmoothed),
    max_speed = mean(Speed_4D_max_unsmoothed),
    se_maxspeed = sd(Speed_4D_max_unsmoothed) / sqrt(sum(!is.na(Speed_4D_max_unsmoothed))),
    sd_maxspeed = sd(Speed_4D_max_unsmoothed),
    norm_maxspeed = se_maxspeed/max_speed,
    norm_sdmax = sd_maxspeed / max_speed,
    var_max = var(Speed_4D_max_unsmoothed),
    mean_tort = mean(Tortuosity_2D),
    se_tort = sd(Tortuosity_2D) / sqrt(sum(!is.na(Tortuosity_2D))),
    sd_tort = sd(Tortuosity_2D),
    norm_tort = se_tort/mean_tort,
    norm_sdtort = sd_tort / mean_tort,
    var_tort = var(Tortuosity_2D),
    vert_speed = mean(vertical_speed),
    se_vert = sd(vertical_speed) / sqrt(sum(!is.na(vertical_speed))),
    sd_vert = sd(vertical_speed),
    norm_vert = se_vert/vert_speed,
    norm_sdvert = sd_vert / vert_speed,
    var_vert = var(vertical_speed),
    change_in_depth = mean(Fish_track_change_in_depth),
    se_change = sd(Fish_track_change_in_depth) / sqrt(sum(!is.na(Fish_track_change_in_depth))),
    sd_change = sd(Fish_track_change_in_depth),
    norm_change = se_change/change_in_depth,
    norm_sdchange = sd_change/change_in_depth,
    var_change = var(Fish_track_change_in_depth),
    depth = mean(Target_depth_mean),
    se_depth = sd(Target_depth_mean) / sqrt(sum(!is.na(Target_depth_mean))),
    sd_depth = sd(Target_depth_mean),
    norm_depth = se_depth/depth,
    norm_sddepth = sd_depth / depth,
    var_depth = var(Target_depth_mean)
  ) %>%
  ungroup()

a <- ggplot(daily_stats, aes(x = Date, y = var_depth)) +
  geom_point(color = "#4477AA", size = 2, alpha = 1) +
  geom_vline(xintercept = as.Date("2023-08-08"), linetype = "dashed", color = "#EE6677", size = 0.8) + # no more pile driving
  theme_minimal() +
  labs(
    x = "Date",
    y = "Variance",
    title = ""
  ) +
  annotate("text",
           x = as.Date("2023-07-24"),   
           y = max(daily_stats$depth) * 0.35, 
           label = "DURING",
           size = 4,
           color = "black",
           alpha = 1) +
  annotate("text",
           x = as.Date("2023-08-21"),   
           y = max(daily_stats$depth) * 0.35, 
           label = "AFTER",
           size = 4,
           color = "black",
           alpha = 1) +
  scale_x_date(date_labels = "%b %d", date_breaks = "1 week") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

b <- ggplot(daily_stats, aes(x = Date, y = depth)) +
  geom_errorbar(aes(x = Date, ymin = depth - se_depth, ymax = depth + se_depth),
                width = 0.2, color = "gray50", alpha = 1) +
  geom_point(color = "#4477AA", size = 2, alpha = 1) +
  geom_vline(xintercept = as.Date("2023-08-08"), linetype = "dashed", color = "#EE6677", size = 0.8) + # no more pile driving
  theme_minimal() +
  labs(
    x = "Date",
    y = "Mean depth (+/- SE)",
    title = ""
  ) +
  annotate("text",
           x = as.Date("2023-07-24"),   
           y = max(daily_stats$depth) * 0.27, 
           label = "DURING",
           size = 4,
           color = "black",
           alpha = 1) +
  annotate("text",
           x = as.Date("2023-08-21"),   
           y = max(daily_stats$depth) * 0.27, 
           label = "AFTER",
           size = 4,
           color = "black",
           alpha = 1) +
  scale_x_date(date_labels = "%b %d", date_breaks = "1 week") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

combined_plot <- b + a + plot_layout(ncol = 2, widths = c(1, 1))
combined_plot

wilcox.test(var_depth ~ Period, data = daily_stats) # <.05

ggsave("/Volumes/SophiaWill/CHP1_SFW_Installation/Plots/For Manuscript/Fig7.jpg", plot = combined_plot, width = 12, height = 6, units = "in")
ggsave("/Volumes/SophiaWill/CHP1_SFW_Installation/Plots/For Manuscript/Fig7.svg", plot = combined_plot, width = 12, height = 6, units = "in")
ggsave("/Volumes/SophiaWill/CHP1_SFW_Installation/Plots/For Manuscript/Fig7.eps", plot = combined_plot, width = 12, height = 6, units = "in")

### Look at cumulative SEL  ####
hits_wb <- read.table(
  "/Volumes/SophiaWill/CHP1_SFW_Installation/Plots/For Manuscript/piledriving.txt",
  header = TRUE,
  sep = "\t",
  stringsAsFactors = FALSE
)

hits_wb <- hits_wb %>%
  mutate(
    Begin.Date = case_when(
      Begin.Date %in% c("2023/7/23", "2023/7/24") ~ "2023/7/23",
      TRUE ~ Begin.Date
    )
  )

hits_wb <- hits_wb %>%
  mutate(SPL = 20 * log10(RMS.Amp..Pa. / 1e-6))

# Per-turbine SEL/SPL summary (energy-domain corrected)
Summary_per_turbine_wb <- hits_wb %>%
  group_by(Begin.Date) %>%
  summarise(
    SEL_cum = 10 * log10(sum(10^(SEL..dB. / 10), na.rm = TRUE)),
    median_SEL_SS = 10 * log10(median(10^(SEL..dB. / 10), na.rm = TRUE)),
    min_RMS_SPL    = min(SPL, na.rm = TRUE),
    median_RMS_SPL = 10 * log10(median(10^(SPL / 10), na.rm = TRUE)),
    max_RMS_SPL    = max(SPL, na.rm = TRUE),
    n_strikes = n()
  )


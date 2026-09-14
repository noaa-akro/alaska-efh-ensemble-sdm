library(blockCV)
library(sf)
library(terra)
library(ggplot2)
library(dplyr)
library(purrr)
library(gstat)

EFH.path <- "M:/HCD/shared_hcd_projects/2028 EFH SDM/"
region <- "EBS"

raster.stack <- terra::rast(paste0(EFH.path, "2028_Covariates/", region, "/covariate_raster_stack_", region, ".tiff"))
rast <- raster.stack[[3:19]]




#blockCV::cv_spatial_autocor(r = raster.stack, num_sample = 25000)
# 1. Save the autocorrelation analysis output
autocor <- blockCV::cv_spatial_autocor(r = rast, num_sample = 50000)

# 2. Check available variable names rast in variograms
names(autocor$variograms) <- names(rast)
names(autocor$variograms)

# 2. Extract empirical variogram points (exp_var) for all layers
points_df <- imap_dfr(autocor$variograms, function(v, var_name) {
  v$exp_var %>%
    mutate(variable = var_name)
})

# 3. Generate fitted model line points (var_model) for all layers
lines_df <- imap_dfr(autocor$variograms, function(v, var_name) {
  max_d <- max(v$exp_var$dist, na.rm = TRUE)
  gstat::variogramLine(v$var_model, maxdist = max_d, n = 200) %>%
    mutate(variable = var_name)
})

# 4. Create faceted ggplot
ggplot() +
  # Empirical sample points
  geom_point(data = points_df, aes(x = dist / 1000, y = gamma), color = "black", size = 1.2, alpha = 0.8) +
  # Fitted variogram model line
  geom_line(data = lines_df, aes(x = dist / 1000, y = gamma), color = "#0072B2", linewidth = 0.8) +
  # Facet by raster variable with independent axis scales
  facet_wrap(~ variable, scales = "free") +
  labs(
    title = "Empirical and Fitted Variograms by Environmental Layer",
    x = "Distance (km)",
    y = "Semivariance (γ)"
  ) +
  geom_vline(xintercept = 252, color = "red", linetype = "dashed")+
  theme_bw() +
  theme(
    strip.background = element_rect(fill = "grey92"),
    strip.text = element_text(face = "bold"),
    axis.text = element_text(size = 7)
  )



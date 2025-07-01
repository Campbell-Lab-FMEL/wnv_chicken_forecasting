
pacman::p_load(
  tidyverse
  , terra
  , sf
  , tigris
  # ,
  # ,
)



###############################################################################################################
########################################### Loading template raster ########################################### 
###############################################################################################################

fl_ne <- rnaturalearth::ne_states("united states of america", returnclass = "sf") %>%
  filter(name == "Florida") %>%
  st_transform(32617) %>%
  rmapshaper::ms_filter_islands(., min_area = 5e+09) %>%
  st_simplify(preserveTopology = F, dTolerance = 1000)

fl_ne_units <- fl_ne %>%
  mutate(geometry = geometry / 1000)

write_rds(fl_ne_units, "data/fl_polygon_crop.rds")

blank <- rast("data/environment/climate/tmin_stack_seasonal.tif")$`2016_active` > Inf

blank <- blank %>%
  crop(fl_ne, mask = T)

###############################################################################################################





###############################################################################################################
########################################### Climate data processing ########################################### 
###############################################################################################################

##### Monthly rasters:
tmin_monthly_raw <- rast("data/environment/climate/tmin_stack_monthly.tif") %>%
  # scale() %>%
  crop(blank, mask = T)

prcp_monthly_raw <- rast("data/environment/climate/prcp_stack_monthly.tif") %>%
  # scale() %>%
  crop(blank, mask = T)

tmax_monthly_raw <- rast("data/environment/climate/tmax_stack_monthly.tif") %>%
  # scale() %>%
  crop(blank, mask = T)

##### Seasonal rasters:
prcp_seasonal_raw <- rast("data/environment/climate/prcp_stack_seasonal.tif") %>%
  # scale() %>%
  crop(blank, mask = T)

tmax_seasonal_raw <- rast("data/environment/climate/tmax_stack_seasonal.tif") %>%
  # scale() %>%
  crop(blank, mask = T)

tmin_seasonal_raw <- rast("data/environment/climate/tmin_stack_seasonal.tif") %>%
  # scale() %>%
  crop(blank, mask = T)

climate_monthly_df <- prcp_monthly_raw %>%
  as.data.frame(xy = T) %>%
  pivot_longer(-c(x, y), names_to = "layer") %>%
  rename("prcp" = "value",
         "date" = "layer") %>%
  bind_cols(
    tmax_monthly_raw %>%
      as.data.frame(xy = T) %>%
      pivot_longer(-c(x, y), names_to = "layer") %>%
      select(value) %>%
      rename("tmax" = "value"),
    tmin_monthly_raw %>%
      as.data.frame(xy = T) %>%
      pivot_longer(-c(x, y), names_to = "layer") %>%
      select(value) %>%
      rename("tmin" = "value")) %>%
  mutate(prcp_lag1 = lag(prcp, 1), 
         prcp_lag2 = lag(prcp, 2), 
         tmax_lag1 = lag(tmax, 1), 
         tmax_lag2 = lag(tmax, 2), 
         tmin_lag1 = lag(tmin, 1), 
         tmin_lag2 = lag(tmin, 2),
         month = str_split_fixed(date, "-", 2)[,1],
         year = str_split_fixed(date, "-", 2)[,2],
         season = if_else(month %in% 1:5, 1, 2),
         season_name = if_else(season == 1, "inactive", "active"),
         month = as.character(month),
         year = as.character(year)) %>%
  filter(year %in% c(2001:2021)) %>%
  rename("lon" = "x", "lat" = "y") %>%
  mutate(lat = lat / 1000,
         lon = lon / 1000) %>%
  select(lon, lat, year, month, season, season_name, 
         prcp, prcp_lag1, prcp_lag2, 
         tmax, tmax_lag1, tmax_lag2, 
         tmin, tmin_lag1, tmin_lag2)

climate_seasonal_df <- as.data.frame(prcp_seasonal_raw, xy = T) %>%
  pivot_longer(-c(x, y), names_to = "layer") %>%
  rename("prcp" = "value") %>%
  bind_cols(
    tmax_seasonal_raw %>%
      as.data.frame(xy = T) %>%
      pivot_longer(-c(x, y), names_to = "layer") %>%
      select(value) %>%
      rename("tmax" = "value"),
    tmin_seasonal_raw %>%
      as.data.frame(xy = T) %>%
      pivot_longer(-c(x, y), names_to = "layer") %>%
      select(value) %>%
      rename("tmin" = "value")) %>%
  select(x, y, layer, prcp, tmax, tmin) %>%
  mutate(prcp_lag1 = lag(prcp, 1), 
         prcp_lag2 = lag(prcp, 2), 
         tmax_lag1 = lag(tmax, 1), 
         tmax_lag2 = lag(tmax, 2), 
         tmin_lag1 = lag(tmin, 1), 
         tmin_lag2 = lag(tmin, 2),
         season_name = str_split_fixed(layer, "_", 2)[,2],
         year = str_split_fixed(layer, "_", 2)[,1],
         season = if_else(season_name == "inactive", 1, 2)) %>%
  filter(year %in% c(2001:2021)) %>%
  rename("lon" = "x", "lat" = "y") %>%
  mutate(lat = lat / 1000,
         lon = lon / 1000) %>%
  select(lon, lat, year, season, season_name,
         prcp, prcp_lag1, prcp_lag2, 
         tmax, tmax_lag1, tmax_lag2, 
         tmin, tmin_lag1, tmin_lag2) 

rm(prcp_monthly_raw, prcp_seasonal_raw,
   tmax_monthly_raw, tmax_seasonal_raw,
   tmin_monthly_raw, tmin_seasonal_raw)

write_rds(climate_monthly_df, "data/environment/climate/climate_monthly_df.rds")
write_csv(climate_monthly_df, "data/environment/climate/climate_monthly_df.csv")

write_rds(climate_seasonal_df, "data/environment/climate/climate_seasonal_df.rds")
write_csv(climate_seasonal_df, "data/environment/climate/climate_seasonal_df.csv")

###############################################################################################################





###############################################################################################################
########################################## Landcover data processing ########################################## 
###############################################################################################################

nlcd_annual <- rast("data/environment/landcover/nlcd_annual.tif")

subset_nlcd <- function(x){
  
  lulc <- c(
    x %in% c(
      "Developed, Open Space",
      "Developed, Low Intensity",
      "Developed, Mid Intensity",
      "Developed, High Intensity"),
    
    x %in% c(
      "Deciduous Forest",
      "Evergreen Forest",
      "Mixed Forest",
      "Shrub/Scrub",
      "Sedge/Herbaceous",
      "Grassland/Herbaceous"),
    
    x %in% c(
      "Deciduous Forest",
      "Evergreen Forest",
      "Mixed Forest"),
    
    x %in% c(
      "Woody Wetlands",
      "Emergent Herbaceous Wetlands")) 
  
  names(lulc) <- c("developed", "natural", "forest", "wetlands")
  
  return(lulc)
  
}


nlcd_annual_prop <- lapply(nlcd_annual, FUN = subset_nlcd)

developed <- c(
  nlcd_annual_prop[[1]]$developed, 
  nlcd_annual_prop[[2]]$developed, 
  nlcd_annual_prop[[3]]$developed, 
  nlcd_annual_prop[[4]]$developed, 
  nlcd_annual_prop[[5]]$developed, 
  nlcd_annual_prop[[6]]$developed, 
  nlcd_annual_prop[[7]]$developed, 
  nlcd_annual_prop[[8]]$developed, 
  nlcd_annual_prop[[9]]$developed, 
  nlcd_annual_prop[[10]]$developed, 
  nlcd_annual_prop[[11]]$developed, 
  nlcd_annual_prop[[12]]$developed, 
  nlcd_annual_prop[[13]]$developed, 
  nlcd_annual_prop[[14]]$developed, 
  nlcd_annual_prop[[15]]$developed, 
  nlcd_annual_prop[[16]]$developed, 
  nlcd_annual_prop[[17]]$developed, 
  nlcd_annual_prop[[18]]$developed, 
  nlcd_annual_prop[[19]]$developed, 
  nlcd_annual_prop[[20]]$developed, 
  nlcd_annual_prop[[21]]$developed, 
  nlcd_annual_prop[[22]]$developed
); names(developed) <- 2000:2021

wetlands <- c(
  nlcd_annual_prop[[1]]$wetlands, 
  nlcd_annual_prop[[2]]$wetlands, 
  nlcd_annual_prop[[3]]$wetlands, 
  nlcd_annual_prop[[4]]$wetlands, 
  nlcd_annual_prop[[5]]$wetlands, 
  nlcd_annual_prop[[6]]$wetlands, 
  nlcd_annual_prop[[7]]$wetlands, 
  nlcd_annual_prop[[8]]$wetlands, 
  nlcd_annual_prop[[9]]$wetlands, 
  nlcd_annual_prop[[10]]$wetlands, 
  nlcd_annual_prop[[11]]$wetlands, 
  nlcd_annual_prop[[12]]$wetlands, 
  nlcd_annual_prop[[13]]$wetlands, 
  nlcd_annual_prop[[14]]$wetlands, 
  nlcd_annual_prop[[15]]$wetlands, 
  nlcd_annual_prop[[16]]$wetlands, 
  nlcd_annual_prop[[17]]$wetlands, 
  nlcd_annual_prop[[18]]$wetlands, 
  nlcd_annual_prop[[19]]$wetlands, 
  nlcd_annual_prop[[20]]$wetlands, 
  nlcd_annual_prop[[21]]$wetlands, 
  nlcd_annual_prop[[22]]$wetlands
); names(wetlands) <- 2000:2021

forest <- c(
  nlcd_annual_prop[[1]]$forest, 
  nlcd_annual_prop[[2]]$forest, 
  nlcd_annual_prop[[3]]$forest, 
  nlcd_annual_prop[[4]]$forest, 
  nlcd_annual_prop[[5]]$forest, 
  nlcd_annual_prop[[6]]$forest, 
  nlcd_annual_prop[[7]]$forest, 
  nlcd_annual_prop[[8]]$forest, 
  nlcd_annual_prop[[9]]$forest, 
  nlcd_annual_prop[[10]]$forest, 
  nlcd_annual_prop[[11]]$forest, 
  nlcd_annual_prop[[12]]$forest, 
  nlcd_annual_prop[[13]]$forest, 
  nlcd_annual_prop[[14]]$forest, 
  nlcd_annual_prop[[15]]$forest, 
  nlcd_annual_prop[[16]]$forest, 
  nlcd_annual_prop[[17]]$forest, 
  nlcd_annual_prop[[18]]$forest, 
  nlcd_annual_prop[[19]]$forest, 
  nlcd_annual_prop[[20]]$forest, 
  nlcd_annual_prop[[21]]$forest, 
  nlcd_annual_prop[[22]]$forest
); names(forest) <- 2000:2021

natural <- c(
  nlcd_annual_prop[[1]]$natural, 
  nlcd_annual_prop[[2]]$natural, 
  nlcd_annual_prop[[3]]$natural, 
  nlcd_annual_prop[[4]]$natural, 
  nlcd_annual_prop[[5]]$natural, 
  nlcd_annual_prop[[6]]$natural, 
  nlcd_annual_prop[[7]]$natural, 
  nlcd_annual_prop[[8]]$natural, 
  nlcd_annual_prop[[9]]$natural, 
  nlcd_annual_prop[[10]]$natural, 
  nlcd_annual_prop[[11]]$natural, 
  nlcd_annual_prop[[12]]$natural, 
  nlcd_annual_prop[[13]]$natural, 
  nlcd_annual_prop[[14]]$natural, 
  nlcd_annual_prop[[15]]$natural, 
  nlcd_annual_prop[[16]]$natural, 
  nlcd_annual_prop[[17]]$natural, 
  nlcd_annual_prop[[18]]$natural, 
  nlcd_annual_prop[[19]]$natural, 
  nlcd_annual_prop[[20]]$natural, 
  nlcd_annual_prop[[21]]$natural, 
  nlcd_annual_prop[[22]]$natural
); names(natural) <- 2000:2021

developed <- developed %>%
  project(blank) %>%
  crop(blank, mask = T)

writeRaster(developed, "data/environment/landcover/developed_annual.tif", overwrite = T)
developed <- rast("data/environment/landcover/developed_annual.tif")

wetlands <- wetlands %>%
  project(blank) %>%
  crop(blank, mask = T)

writeRaster(wetlands, "data/environment/landcover/wetlands_annual.tif", overwrite = T)
wetlands <- rast("data/environment/landcover/wetlands_annual.tif")

natural <- natural %>%
  project(blank) %>%
  crop(blank, mask = T)

writeRaster(natural, "data/environment/landcover/natural_annual.tif", overwrite = T)
natural <- rast("data/environment/landcover/natural_annual.tif")

forest <- forest %>%
  project(blank) %>%
  crop(blank, mask = T)

writeRaster(forest, "data/environment/landcover/forest_annual.tif", overwrite = T)
forest <- rast("data/environment/landcover/forest_annual.tif")

landcover_df <- as.data.frame(developed, xy = T) %>%
  pivot_longer(-c(x, y), names_to = "layer") %>%
  rename("developed" = "value") %>%
  bind_cols(
    wetlands %>%
      as.data.frame(xy = T) %>%
      pivot_longer(-c(x, y), names_to = "layer") %>%
      select(value) %>%
      rename("wetlands" = "value"),
    natural %>%
      as.data.frame(xy = T) %>%
      pivot_longer(-c(x, y), names_to = "layer") %>%
      select(value) %>%
      rename("natural" = "value"),
    forest %>%
      as.data.frame(xy = T) %>%
      pivot_longer(-c(x, y), names_to = "layer") %>%
      select(value) %>%
      rename("forest" = "value")) %>%
  rename("year" = "layer", "lon" = "x", "lat" = "y") %>%
  mutate(lat = lat / 1000,
         lon = lon / 1000) %>%
  filter(year %in% c(2001:2021)) %>%
  select(lon, lat, year, 
         forest, developed, wetlands, natural) 

landcover_monthly_df <- landcover_df %>%
  sdmTMB::replicate_df(time_name = "month", time_values = 1:12) %>%
  mutate(season = if_else(month %in% 1:5, 1, 2),
         season_name = if_else(season == 1, "inactive", "active"),
         month = as.character(month),
         year = as.character(year)) %>%
  select(lon, lat, year, season, season_name, month,
         forest, developed, wetlands, natural) 

landcover_seasonal_df <- landcover_df %>%
  sdmTMB::replicate_df(time_name = "season", time_values = c(1, 2)) %>%
  mutate(season_name = if_else(season == 1, "inactive", "active"),
         year = as.character(year)) %>%
  filter(year %in% c(2001:2021)) %>%
  select(lon, lat, year, season, season_name,
         forest, developed, wetlands, natural) 

###############################################################################################################





###############################################################################################################
############################################## Combining all data ############################################# 
###############################################################################################################

climate_monthly_df <- read_rds("data/environment/climate/climate_monthly_df.rds")
climate_seasonal_df <- read_rds("data/environment/climate/climate_seasonal_df.rds")

landcover_monthly_df <- read_rds("data/environment/landcover/landcover_monthly_df.rds")
landcover_seasonal_df <- read_rds("data/environment/landcover/landcover_seasonal_df.rds")

env_monthly_df <- climate_monthly_df %>%
  left_join(landcover_monthly_df) %>%
  na.omit() %>%
  select(lon, lat, year, month, season, season_name,
         prcp, prcp_lag1, prcp_lag2, 
         tmax, tmax_lag1, tmax_lag2, 
         tmin, tmin_lag1, tmin_lag2,
         forest, developed, wetlands, natural) 

env_seasonal_df <- climate_seasonal_df %>%
  left_join(landcover_seasonal_df) %>%
  na.omit() %>%
  select(lon, lat, year, season, season_name,
         prcp, prcp_lag1, prcp_lag2, 
         tmax, tmax_lag1, tmax_lag2, 
         tmin, tmin_lag1, tmin_lag2,
         forest, developed, wetlands, natural) 

# write_rds(env_monthly_df, "data/environment/env_covs_monthly.rds")
# write_csv(env_monthly_df, "data/environment/env_covs_monthly.csv")
# 
# write_rds(env_seasonal_df, "data/environment/env_covs_seasonal.rds")
# write_csv(env_seasonal_df, "data/environment/env_covs_seasonal.csv")

###############################################################################################################





###############################################################################################################
###################################### Centering preds with model data ######################################## 
###############################################################################################################

site_monthly_df <- read_rds("data/chickens/seasonal/wnv_eeev_env_covs_2.rds")

site_seasonal_df <- read_rds("data/chickens/monthly/wnv_eeev_env_covs_2.rds")

env_seasonal_df_center <- env_seasonal_df %>% 
  mutate(prcp =      (prcp     /mean(site_seasonal_df$prcp))     /sd(site_seasonal_df$prcp),
         prcp_lag1 = (prcp_lag1/mean(site_seasonal_df$prcp_lag1))/sd(site_seasonal_df$prcp_lag1),
         prcp_lag2 = (prcp_lag2/mean(site_seasonal_df$prcp_lag2))/sd(site_seasonal_df$prcp_lag2),
         
         tmax =      (tmax     /mean(site_seasonal_df$tmax))     /sd(site_seasonal_df$tmax),
         tmax_lag1 = (tmax_lag1/mean(site_seasonal_df$tmax_lag1))/sd(site_seasonal_df$tmax_lag1),
         tmax_lag2 = (tmax_lag2/mean(site_seasonal_df$tmax_lag2))/sd(site_seasonal_df$tmax_lag2),
         
         tmin =      (tmin     /mean(site_seasonal_df$tmin))     /sd(site_seasonal_df$tmin),
         tmin_lag1 = (tmin_lag1/mean(site_seasonal_df$tmin_lag1))/sd(site_seasonal_df$tmin_lag1),
         tmin_lag2 = (tmin_lag2/mean(site_seasonal_df$tmin_lag2))/sd(site_seasonal_df$tmin_lag2)
  )

env_monthly_df_center <- env_monthly_df %>% 
  mutate(prcp =      (prcp     /mean(site_monthly_df$prcp))     /sd(site_monthly_df$prcp),
         prcp_lag1 = (prcp_lag1/mean(site_monthly_df$prcp_lag1))/sd(site_monthly_df$prcp_lag1),
         prcp_lag2 = (prcp_lag2/mean(site_monthly_df$prcp_lag2))/sd(site_monthly_df$prcp_lag2),
         
         tmax =      (tmax     /mean(site_monthly_df$tmax))     /sd(site_monthly_df$tmax),
         tmax_lag1 = (tmax_lag1/mean(site_monthly_df$tmax_lag1))/sd(site_monthly_df$tmax_lag1),
         tmax_lag2 = (tmax_lag2/mean(site_monthly_df$tmax_lag2))/sd(site_monthly_df$tmax_lag2),
         
         tmin =      (tmin     /mean(site_monthly_df$tmin))     /sd(site_monthly_df$tmin),
         tmin_lag1 = (tmin_lag1/mean(site_monthly_df$tmin_lag1))/sd(site_monthly_df$tmin_lag1),
         tmin_lag2 = (tmin_lag2/mean(site_monthly_df$tmin_lag2))/sd(site_monthly_df$tmin_lag2)
  )

summary(env_seasonal_df_center); summary(env_monthly_df_center)

write_rds(env_monthly_df, "data/environment/env_covs_monthly.rds")

write_rds(env_seasonal_df, "data/environment/env_covs_seasonal.rds")

###############################################################################################################

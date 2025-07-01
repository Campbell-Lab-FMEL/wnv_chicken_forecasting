
pacman::p_load(
  tidyverse
  , terra
  , sf
  , tigris
  # ,
  # ,
)

fl <- states() %>%
  filter(NAME == "Florida") %>%
  st_transform(32617) 

prcp <- rast("data/environment/climate/prcp_stack_monthly.tif") %>%
  project(fl, res = 1000) %>%
  crop(fl, mask = T)

tmax <- rast("data/environment/climate/tmax_stack_monthly.tif") %>%
  project(fl, res = 1000) %>%
  crop(fl, mask = T)

tmin <- rast("data/environment/climate/tmin_stack_monthly.tif") %>%
  project(fl, res = 1000) %>%
  crop(fl, mask = T)

# peak season is between June - December (6 - 12), off season January - May (1 - 5)

raster_map <- data.frame(
  month = str_split_fixed(names(tmin), "-", 2)[,1],
  year = str_split_fixed(names(tmin), "-", 2)[,2] %>% as.integer()) %>%
  mutate(season = if_else(month %in% 1:5, 1, 2),
         season_name = if_else(season == 1, "inactive", "active"),
         layer = row_number()); head(raster_map)

years <- unique(raster_map$year)

raster_annual_map <- raster_map %>%
  group_by(year, season) %>%
  summarize(year = unique(year),
            season = unique(season)) %>%
  ungroup() %>%
  mutate(season_name = if_else(season == 1, "inactive", "active"),
         year = as.character(year),
         season = as.character(season),
         lag0 = row_number(),
         lag1 = lag0 - 1, 
         lag2 = lag0 - 2); head(raster_annual_map)

prcp_list <- tmax_list <- tmin_list <- list()

for(i in 1:length(years)){
  
  ###########################################################
  ################## Current tmax and prcp ##################
  ###########################################################
  
  inactive <- raster_map$layer[raster_map$year == years[i] & raster_map$season == 1]
  active <- raster_map$layer[raster_map$year == years[i] & raster_map$season == 2]
  
  prcp_inactive <- sum(prcp[[inactive]], na.rm = T)
  prcp_active <- sum(prcp[[active]], na.rm = T)
  
  prcp_list[[paste0("year_", years[i])]] <- c(prcp_inactive, prcp_active)

  tmax_inactive <- mean(tmax[[inactive]], na.rm = T)
  tmax_active <- mean(tmax[[active]], na.rm = T)
  
  tmax_list[[paste0("year", years[i])]] <- c(tmax_inactive, tmax_active)
  
  tmin_inactive <- mean(tmin[[inactive]], na.rm = T)
  tmin_active <- mean(tmin[[active]], na.rm = T)
  
  tmin_list[[paste0("year", years[i])]] <- c(tmin_inactive, tmin_active) 
  
  ###########################################################
  
    cat("\r Processed climate for year: ", years[i], " | Progress: ", 
      round(
        (i/length(years))*100,
        0),
      "% complete", sep = "")
  
  if(i == length(years)){
    
    prcp_seasonal <- rast(prcp_list)
    tmax_seasonal <- rast(tmax_list)
    tmin_seasonal <- rast(tmin_list)
    
    names(prcp_seasonal) <- paste(raster_annual_map$year, raster_annual_map$season_name, sep = "_")
    names(tmax_seasonal) <- paste(raster_annual_map$year, raster_annual_map$season_name, sep = "_")
    names(tmin_seasonal) <- paste(raster_annual_map$year, raster_annual_map$season_name, sep = "_")
    
  }
}

writeRaster(prcp, "data/environment/climate/prcp_stack_monthly.tif", overwrite = T)
writeRaster(tmax, "data/environment/climate/tmax_stack_monthly.tif", overwrite = T)
writeRaster(tmin, "data/environment/climate/tmin_stack_monthly.tif", overwrite = T)

writeRaster(prcp_seasonal, "data/environment/climate/prcp_stack_seasonal.tif", overwrite = T)
writeRaster(tmax_seasonal, "data/environment/climate/tmax_stack_seasonal.tif", overwrite = T)
writeRaster(tmin_seasonal, "data/environment/climate/tmin_stack_seasonal.tif", overwrite = T)

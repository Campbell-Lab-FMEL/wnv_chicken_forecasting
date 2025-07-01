pacman::p_load(
  tidyverse
  , terra
  , tigris
  , sf
  , climateR
)

# Get a Florida shapefile from Tigris
fl <- states() %>%
  filter(NAME == "Florida") %>%
  st_transform(32617)

plot(fl)

# Set the dates for downloading raster data
dates <- tibble(
  date = seq(as.Date("2000-01-01"), as.Date("2021-12-31"), by = "day"),
  year = year(date),
  month = month(date), 
  day = day(date)) %>%
  group_by(year, month) %>%
  summarize(
    start = min(date), 
    end = max(date)) %>%
  mutate(year = year(start),
         month = month(start),
         month_year = paste(month, year, sep = "-"))

# Initialize list objects for each weather variable of interest
prcp_sum <- tmax <- tmin <- tmean <- list()

for(i in 1:nrow(dates)){
  # Downloading daily precipitation, summing to the monthly level
  prcp_sum[[paste(dates$month_year[i])]] <- getDaymet(AOI = fl,
                                                varname = "prcp",
                                                startDate = paste(dates$start[i]),
                                                endDate = paste(dates$end[i]),
                                                dryrun = F)[[1]] %>%
    sum()

  cat("\r processed cumulative precipitation: ",
      round(
        (i/nrow(dates))*100,
        0),
      "% complete", sep = "")

  # Downloading daily max temp, averaging to monthly level
  tmax[[paste(dates$month_year[i])]] <- getDaymet(AOI = fl,
                                            varname = "tmax",
                                            startDate = paste(dates$start[i]),
                                            endDate = paste(dates$end[i]),
                                            dryrun = F)[[1]] %>%
    mean()

  cat("\r processed average max temperature: ",
      round(
        (i/nrow(dates))*100,
        0),
      "% complete", sep = "")

  # Downloading daily min temp, averaging to monthly level
  tmin[[paste(dates$month_year[i])]] <- getDaymet(AOI = fl,
                                                  varname = "tmin",
                                                  startDate = paste(dates$start[i]),
                                                  endDate = paste(dates$end[i]),
                                                  dryrun = F)[[1]] %>%
    mean()

  cat("\r processed average min temperature: ",
      round(
        (i/nrow(dates))*100,
        0),
      "% complete", sep = "")

}

# Cropping and masking rasters to the shapefile (clean up)
prcp_stack <- rast(prcp_sum) # %>%
  # project("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0") %>%
  # crop(fl, mask = T)

tmax_stack <- rast(tmax) # %>%
  # project("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0") %>%
  # crop(fl, mask = T)

tmin_stack <- rast(tmin) # %>%
  # project("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0") %>%
  # crop(fl, mask = T)


# prcp_stack <- rast("data/environment/climate/prcp_stack_monthly.tif") %>%
#   project("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
# 
# tmax_stack <- rast("data/environment/climate/tmax_stack_monthly.tif") %>%
#   project("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
# 
# tmin_stack <- rast("data/environment/climate/tmin_stack_monthly.tif") %>%
#   project("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")

# Writing to disk
terra::writeRaster(prcp_stack, "data/environment/climate/prcp_stack_monthly.tif", overwrite = T)
terra::writeRaster(tmax_stack, "data/environment/climate/tmax_stack_monthly.tif", overwrite = T)
terra::writeRaster(tmin_stack, "data/environment/climate/tmin_stack_monthly.tif", overwrite = T)


pacman::p_load(
  tidyverse
  , terra
  , FedData
  , pbapply
  # ,
  # ,
)

## Loading data:

blank <- rast("data/environment/climate/prcp_stack_monthly.tif")$`1-2000` > Inf

## Download NLCD data

years <- c(2001, 2004, 2006, 2008, 2011, 2016, 2019)

nlcd_list <- pblapply(years, 
                      FUN = function(x){
                        get_nlcd(
                          template = blank, 
                          "Florida_NLCD",
                          year = x)})

nlcd_raw <- rast(nlcd_list)
names(nlcd_raw) <- years
plot(nlcd_raw, legend = F)

## Create a raster guide to map time steps:

raster_mapping <- data.frame(
  year = 2000:2021,
  # data_year = c(2000, 2001, 2002, 2003, 2004, 2005, 2006, 2007, 2008, 2009, 2010,
  #               2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020, 2021
  nlcd_year = c(2001, 2001, 2001, 2004, 2004, 2006, 2006, 2008, 2008, 2008, 2011,
                2011, 2011, 2011, 2011, 2016, 2016, 2016, 2016, 2019, 2019, 2019)) %>%
  mutate(layer = row_number())

nlcd_annual <- lapply(raster_mapping$nlcd_year, 
                      FUN = function(x){
                        nlcd_raw[[paste(x)]]}) %>%
  unlist() %>%
  rast()
names(nlcd_annual) <- raster_mapping$year
plot(nlcd_annual, legend = F)

writeRaster(nlcd_annual, "data/environment/landcover/nlcd_annual.tif", overwrite = T)
write_rds(nlcd_annual, "data/environment/landcover/nlcd_annual.rds")


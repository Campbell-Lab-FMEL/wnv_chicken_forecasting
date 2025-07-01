
pacman::p_load(
  tidyverse
  , terra 
  , elevatr
  , sf
  , tigris
  # , 
  # ,
)

# Get a Florida shapefile from Tigris
fl <- states() %>%
  filter(NAME == "Florida") %>%
  st_transform(32617)

blank <- rast("data/environment/climate/prcp_stack_monthly.tif")$`1-2000` > Inf

elev <- get_elev_raster(fl, 
                        prj = st_crs(fl), 
                        z = 8) %>%
  rast() %>%
  project(blank) %>%
  crop(blank, mask = T)

topo <- terrain(elev, v = c("slope", "aspect", "TPI", "TRI", "roughness", "flowdir"), neighbors = 8, unit = "degrees") %>%
  c(elev, .)

names(topo) <- c("elev", "slope", "aspect", "TPI", "TRI", "roughness", "flowdir")

plot(topo)

terra::writeRaster(topo, "data/environment/dem/dem_metrics.tif", overwrite = T)

### integrate all environmental covariates
dem <- rast("data/environment/dem/dem_metrics.tif")


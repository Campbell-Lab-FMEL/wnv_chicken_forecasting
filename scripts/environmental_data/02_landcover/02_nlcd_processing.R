
pacman::p_load(
  tidyverse
  , terra
  , sf
  , janitor
  , tigris
  # ,
  # ,
)

######## extracting data

nlcd_annual <- rast("data/environment/landcover/nlcd_annual.tif")

names(nlcd_annual) <- 2000:2021

data_monthly <- read_rds("data/chickens/monthly/wnv_eeev_clim.rds")

data_years <- data_monthly %>%
  st_drop_geometry() %>%
  select(year) %>%
  unique() %>%
  arrange(year) %>%
  pull(year)

nlcd_extract <- data_list <- list()

for(i in 1:length(data_years)){
  # for(i in 1:2){
  
  data_list[[paste(data_years[i])]] <- data_monthly %>%
    filter(year == as.integer(data_years[i]))  %>%
    group_by(ID) %>%
    summarize(years = n()) %>%
    st_buffer(2500)
  
  nlcd_extract[[paste(data_years[i])]] <- terra::extract(
    nlcd_annual[[paste(data_years[i])]],
    data_list[[i]]) %>%
    table() %>%
    as.data.frame.matrix() %>%
    bind_cols(st_drop_geometry(data_list[[i]])) %>%
    dplyr::relocate(ID)
  
  cat("\r Progress: ", 
      round(
        (i/length(data_years)*100), 0), 
      "% complete", 
      sep = "")
  
}

nlcd_prop <- do.call(rbind, nlcd_extract) %>%
  mutate(total = rowSums(select_if(., is.numeric), na.rm = TRUE)) %>%
  clean_names() %>%
  mutate(developed = (rowSums(
    dplyr::select(., 
                  developed_open_space,
                  developed_low_intensity,
                  developed_medium_intensity,
                  developed_high_intensity))/total) %>% round(5)) %>%
  
  mutate(cropland = (rowSums(
    dplyr::select(., 
                  pasture_hay,
                  cultivated_crops))/total) %>% round(5)) %>%
  
  mutate(natural = (rowSums(
    dplyr::select(., 
                  deciduous_forest,
                  evergreen_forest,
                  mixed_forest,
                  shrub_scrub,
                  sedge_herbaceous,
                  grassland_herbaceous))/total) %>% round(5)) %>%
  
  mutate(forest = (rowSums(
    dplyr::select(., 
                  deciduous_forest,
                  evergreen_forest,
                  mixed_forest))/total) %>% round(5)) %>%
  
  mutate(wetlands = (rowSums(
    dplyr::select(., 
                  woody_wetlands,
                  emergent_herbaceous_wetlands))/total) %>% round(5)) %>%
  
  rownames_to_column(var = "year") %>%
  mutate(year = as.numeric(year) %>% floor()) %>%
  dplyr::select(id, year, developed, cropland, natural, forest, wetlands)

#######

write_csv(nlcd_prop, "data/environment/landcover/site_landcover_prop_2.csv")
nlcd_prop <- read_csv("data/environment/landcover/site_landcover_prop_2.csv")

##########
# writing extracted data

data_monthly_nlcd <- data_monthly %>%
  left_join(nlcd_prop, by = c("ID" = "id", "year" = "year")) %>%
  relocate(county, ID, lat, lon, year, month, season, testing, wnv, eeev, 
           tmax, tmax_lag1, tmax_lag2, 
           tmin, tmin_lag1, tmin_lag2, 
           prcp, prcp_lag1, prcp_lag2, 
           developed, cropland, natural, forest, wetlands,
           geometry)

write_rds(data_monthly_nlcd, "data/chickens/monthly/wnv_eeev_env_covs_2.rds")
write_csv(data_monthly_nlcd, "data/chickens/monthly/wnv_eeev_env_covs_2.csv")

data_seasonal_nlcd <- read_rds("data/chickens/seasonal/wnv_eeev_clim.rds") %>%
  left_join(nlcd_prop, by = c("ID" = "id", "year" = "year")) %>%
  relocate(county, ID, lat, lon, year, season, testing, wnv, eeev, 
           tmax, tmax_lag1, tmax_lag2, 
           tmin, tmin_lag1, tmin_lag2, 
           prcp, prcp_lag1, prcp_lag2, 
           developed, cropland, natural, forest, wetlands,
           geometry)
sum(is.na(data_seasonal_nlcd$tmax)); sum(is.na(data_seasonal_nlcd$wetlands))

write_rds(data_seasonal_nlcd, "data/chickens/seasonal/wnv_eeev_env_covs_2.rds")
write_csv(data_seasonal_nlcd, "data/chickens/seasonal/wnv_eeev_env_covs_2.csv")

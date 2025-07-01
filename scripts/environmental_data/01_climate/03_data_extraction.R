
pacman::p_load(
  tidyverse
  , terra
  , sf
  , zoo
  # ,
  # ,
)

########

prcp_monthly <- rast("data/environment/climate/prcp_stack_monthly.tif")
tmax_monthly <- rast("data/environment/climate/tmax_stack_monthly.tif")
tmin_monthly <- rast("data/environment/climate/tmin_stack_monthly.tif")

#######

data_monthly <- read_rds("data/chickens/monthly/wnv_eeev_data.rds") %>%
  st_transform(32617)

plot(tmin_monthly$`1-2000`); points(data_monthly, pch = 21)

### creating lagged versions of the chicken data:

data_monthly_lag <- data_monthly %>%
  mutate(date_lag1 = make_date(year, month) %m-% months(1),
         month_lag1 = month(date_lag1),
         year_lag1 = year(date_lag1),
         date_lag2 = make_date(year, month) %m-% months(2),
         month_lag2 = month(date_lag2),
         year_lag2 = year(date_lag2)) 

sites <- data_monthly %>%
  select(ID, lon, lat) %>%
  unique()

prcp_extract <- extract(prcp_monthly, sites, method = "bilinear", xy = T, ID = F) %>%
  bind_cols(ID = sites %>%
              select(ID, lon, lat)) %>%
  pivot_longer(
    cols = matches("\\d+-\\d{4}"),         
    names_to = c("month", "year"),         
    names_sep = "-",                       
    names_transform = list(                
      month = as.integer,
      year = as.integer),
    values_to = "prcp")

tmax_extract <- extract(tmax_monthly, sites, method = "bilinear", xy = T, ID = F) %>%
  bind_cols(ID = sites %>%
              select(ID, lon, lat)) %>%
  pivot_longer(
    cols = matches("\\d+-\\d{4}"),         
    names_to = c("month", "year"),         
    names_sep = "-",                       
    names_transform = list(                
      month = as.integer,
      year = as.integer),
    values_to = "tmax")

tmin_extract <- extract(tmin_monthly, sites, method = "bilinear", xy = T, ID = F) %>%
  bind_cols(ID = sites %>%
              select(ID, lon, lat)) %>%
  pivot_longer(
    cols = matches("\\d+-\\d{4}"),         
    names_to = c("month", "year"),         
    names_sep = "-",                       
    names_transform = list(                
      month = as.integer,
      year = as.integer),
    values_to = "tmin")

clim_extract <- prcp_extract %>%
  bind_cols(tmax = tmax_extract$tmax, 
            tmin = tmin_extract$tmin) %>%
  st_drop_geometry() %>%
  select(-c("geometry"))

data_monthly_clim <- data_monthly_lag %>%
  left_join(clim_extract, 
            by = c("ID", "month", "year"))

data_monthly_clim_lag1 <- data_monthly_lag %>%
  left_join(clim_extract, 
            by = c("ID", "month_lag1" = "month", "year_lag1" = "year")) %>%
  rename(tmax_lag1 = "tmax", 
         tmin_lag1 = "tmin", 
         prcp_lag1 = "prcp") %>%
  st_drop_geometry()

data_monthly_clim_lag2 <- data_monthly_lag %>%
  left_join(clim_extract, 
            by = c("ID", "lat", "lon", "month_lag2" = "month", "year_lag2" = "year")) %>%
  rename(tmax_lag2 = "tmax", 
         tmin_lag2 = "tmin", 
         prcp_lag2 = "prcp") %>%
  st_drop_geometry()

data_monthly_clim_lags <- data_monthly_clim %>%
  left_join(data_monthly_clim_lag1) %>%
  left_join(data_monthly_clim_lag2) %>%
  select(county, ID, lat, lon, year, month, season, testing, wnv, eeev, 
         tmax, tmin, prcp,
         tmax_lag1, tmin_lag1, prcp_lag1,
         tmax_lag2, tmin_lag2, prcp_lag2,
         geometry) 

data_monthly_clim_lags %>%
  st_drop_geometry() %>%
  filter(season == "2") %>%
  group_by(ID, season) %>%
  summarize(n = n(), 
            wnv = sum(wnv, na.rm = T)) %>%
  as.data.frame()

write_rds(data_monthly_clim_lags, "data/chickens/monthly/wnv_eeev_clim.rds")
write_csv(data_monthly_clim_lags, "data/chickens/monthly/wnv_eeev_clim.csv")

########

prcp_seasonal <- rast("data/environment/climate/prcp_stack_seasonal.tif")
tmax_seasonal <- rast("data/environment/climate/tmax_stack_seasonal.tif")
tmin_seasonal <- rast("data/environment/climate/tmin_stack_seasonal.tif")

#######

data_seasonal <- read_rds("data/chickens/seasonal/wnv_eeev_data.rds") %>%
  st_transform(32617)

### creating lagged versions of the seasonal chicken data:

data_seasonal_lag <- data_monthly %>%
  mutate(date_lag1 = make_date(year, month) %m-% months(6),
         month_lag1 = month(date_lag1),
         year_lag1 = year(date_lag1),
         date_lag2 = make_date(year, month) %m-% months(12),
         month_lag2 = month(date_lag2),
         year_lag2 = year(date_lag2)) %>%
  select(county, ID, lat, lon, 
         year, month, testing,
         year_lag1, month_lag1, 
         year_lag2, month_lag2, 
         season, wnv, eeev, geometry); data_seasonal_lag

data_seasonal_clim_lag1 <- data_seasonal_lag %>%
  left_join(clim_extract, 
            by = c("ID", "lat", "lon", "month_lag1" = "month", "year_lag1" = "year")) %>%
  rename(tmax_lag1 = "tmax", 
         tmin_lag1 = "tmin", 
         prcp_lag1 = "prcp") %>%
  st_drop_geometry()

data_seasonal_clim_lag2 <- data_seasonal_lag %>%
  left_join(clim_extract, 
            by = c("ID", "lat", "lon", "month_lag2" = "month", "year_lag2" = "year")) %>%
  rename(tmax_lag2 = "tmax", 
         tmin_lag2 = "tmin", 
         prcp_lag2 = "prcp") %>%
  st_drop_geometry()

data_seasonal_clim_lags <- data_monthly_clim %>%
  left_join(data_seasonal_clim_lag1, by = c("county", "ID", "year", "month", "season", "testing", "wnv", "eeev")) %>%
  left_join(data_seasonal_clim_lag2, by = c("county", "ID", "year", "month", "season", "testing", "wnv", "eeev")) %>%
  rename("lat" = "lat.x", "lon" = "lon.x") %>%
  group_by(county, ID, lat, lon, year, season) %>%
  summarize(
    testing = sum(testing), 
    wnv = sum(wnv), 
    eeev = sum(eeev), 
    tmax = mean(tmax), 
    tmin = mean(tmin), 
    prcp = sum(prcp),
    tmax_lag1 = mean(tmax_lag1), 
    tmin_lag1 = mean(tmin_lag1), 
    prcp_lag1 = sum(prcp_lag1),
    tmax_lag2 = mean(tmax_lag2), 
    tmin_lag2 = mean(tmin_lag2), 
    prcp_lag2 = sum(prcp_lag2)) %>%
  ungroup() %>%
  select(county, ID, lat, lon, year, season, testing, wnv, eeev, 
         tmax, tmin, prcp,
         tmax_lag1, tmin_lag1, prcp_lag1,
         tmax_lag2, tmin_lag2, prcp_lag2,
         geometry) 

data_seasonal_clim_lags %>%
  st_drop_geometry() %>%
  filter(season == "2") %>%
  group_by(ID, season) %>%
  summarize(n = n(), 
            wnv = sum(wnv, na.rm = T)) %>%
  as.data.frame()

write_rds(data_seasonal_clim_lags, "data/chickens/seasonal/wnv_eeev_clim.rds")
write_csv(data_seasonal_clim_lags, "data/chickens/seasonal/wnv_eeev_clim.csv")


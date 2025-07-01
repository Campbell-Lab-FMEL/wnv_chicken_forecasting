
pacman::p_load(
  tidyverse
  , sf
  # ,
  # ,
)

# ROSF chicken data 

## 
# data <- read_csv("data/chickens/data.csv")
data <- read_csv("data/chickens/raw_data/compiled_chicken_data_clean_v2.csv") %>%
  filter(!is.na(lat)) %>%
  filter(!is.na(lon)) %>%
  mutate(season = if_else(month %in% 1:5, 1, 2),
         season_name = if_else(season == 1, "inactive", "active"))

########

data %>%
  filter(season == 2) %>%
  filter(testing == 1) %>%
  group_by(ID) %>%
  summarize(n_years = n_distinct(year)) %>%
  pull(n_years) %>%
  mean()

data %>%
  filter(season == 2) %>%
  filter(testing == 1) %>%
  group_by(year) %>%
  summarize(n_sites = n_distinct(ID)) %>%
  pull(n_sites) %>%
  mean()

summ_week <-  data %>%
  filter(testing == 1) %>%
  filter(season == 2) %>%
  group_by(year, week) %>%
  summarize(n_samples = n(),
            n_sites = n_distinct(ID)) 

mean(summ_week$n_samples)/526

data %>%
  filter(testing == 1) %>%
  filter(season == 2) %>%
  group_by(year, month) %>%
  summarize(n_samples = n(),
            n_sites = n_distinct(ID)) %>%
  pull(n_sites) %>%
  mean()

########

data_monthly <- data %>%
  group_by(county, ID, lat, lon, year, month) %>%
  summarize(wnv = sum(n_positive_wnv, na.rm = T),
            eeev = sum(n_positive_eeev, na.rm = T),
            testing = sum(testing, na.rm = T)) %>%
  ungroup() %>%
  mutate(season = if_else(month %in% 1:5, 1, 2),
         season_name = if_else(season == 1, "inactive", "active")) %>%
  filter(!is.na(wnv)) %>%
  filter(!is.na(eeev)) %>%
  st_as_sf(x = ., 
           coords = c("lon", "lat"),
           crs = "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0") %>%
  bind_cols(
    st_coordinates(.)) %>%
  rename(lat = "Y", lon = "X") %>%
  select(county, ID, lat, lon, year, month, season, testing, wnv, eeev, geometry)

monthly_data_check <- data_monthly %>%
  st_drop_geometry() %>%
  filter(season == "2") %>%
  group_by(county, ID) %>%
  summarize(n = n(), 
            wnv = sum(wnv, na.rm = T)) %>%
  as.data.frame() %>% 
  arrange(ID)

write_rds(data_monthly, "data/chickens/monthly/wnv_eeev_data.rds")
write_csv(data_monthly, "data/chickens/monthly/wnv_eeev_data.csv")

data_seasonal <- data_monthly %>%
  group_by(county, ID, year, season) %>%
  summarize(wnv = sum(wnv, na.rm = T),
            eeev = sum(eeev, na.rm = T),
            testing = sum(testing, na.rm = T)) %>%
  ungroup() %>%
  bind_cols(
    st_coordinates(.)) %>%
  rename(lat = "Y", lon = "X") %>%
  select(county, ID, lat, lon, year, season, wnv, eeev, geometry)

data_seasonal %>%
  st_drop_geometry() %>%
  filter(season == "2") %>%
  group_by(ID, season) %>%
  summarize(n = n(), 
            wnv = sum(wnv, na.rm = T)) %>%
  as.data.frame()

write_rds(data_seasonal, "data/chickens/seasonal/wnv_eeev_data.rds")
write_csv(data_seasonal, "data/chickens/seasonal/wnv_eeev_data.csv")


pacman::p_load(
  tidyverse
  , sf
  , sdmTMB
  # ,
  # ,
)

### Monthly data

data_monthly <- read_rds("data/chickens/monthly/wnv_eeev_env_covs.rds") %>%
  mutate_at(vars(starts_with(c("prcp", "tmax", "tmin")), "developed", "natural", "wetlands", "testing"), 
            .funs = function(x){as.numeric(scale(x))}) %>%
  mutate_at(vars("year", "season", "ID", "county"), as.factor) %>%
  mutate(season = ifelse(season == 1, "inactive", "active")) %>%
  st_transform(32617) %>%
  mutate(geometry = geometry / 1000) %>%
  bind_cols(as.data.frame(st_coordinates(.)))

# setting training and prediction data
data_monthly_active <- data_monthly %>%
  filter(season == "active") %>%
  left_join(
    data.frame(
      month = rep(6:12, length(2001:2019)),
      year = rep(2001:2019, length(6:12))) %>%
      arrange(year, month) %>%
      mutate(time_step = row_number(),
             year = as.factor(year)) %>%
      select(month, year, time_step)) %>%
  mutate(date = make_date(month = as.character(month), year = as.character(year)))

data_monthly_active_train <- data_monthly_active %>%
  filter(!year %in% 2018:2019) 

### Seasonal data

data_seasonal <- read_rds("data/chickens/seasonal/wnv_eeev_env_covs.rds") %>%
  mutate(county_ID = paste(county, ID, sep = "_") %>% as.factor()) %>%
  mutate_at(vars(starts_with(c("prcp", "tmax", "tmin")), "developed", "natural", "wetlands", "testing"), 
            .funs = function(x){as.numeric(scale(x))}) %>%
  mutate_at(vars("year", "season", "ID", "county"), as.factor) %>%
  mutate(season = ifelse(season == 1, "inactive", "active")) %>%
  left_join(
    data.frame(
      year = 2001:2019) %>%
      mutate(time_step = row_number(),
             year = as.factor(year)) %>%
      select(year, time_step)) %>%
  st_transform(32617) %>%
  mutate(geometry = geometry / 1000) %>%
  bind_cols(as.data.frame(st_coordinates(.))) %>%
  mutate(date = make_date(year = as.character(year)))

# setting training and prediction data
data_seasonal_active <- data_seasonal %>%
  filter(season == "active") 

data_seasonal_active_train <- data_seasonal_active %>%
  filter(!year %in% 2016:2019)

#############################################################################################################################################

sdmtmb_seasonal <- read_rds("data/chickens/model_predictions/sdmTMB/sdmtmb_seasonal_update.rds")

sdmtmb_monthly <- read_rds("data/chickens/model_predictions/sdmTMB/sdmtmb_monthly_update.rds")

#############################################################################################################################################





###############################################################################################################################################
############################################################## Prediction plotting ############################################################
###############################################################################################################################################

# monthly active

data_monthly_active_agg <- data_monthly_active %>%
  group_by(month, year) %>%
  summarize(wnv_mean = mean(wnv),
            wnv_sum = sum(wnv), 
            se = sd(wnv)/sqrt(n())) %>%
  mutate(date = make_date(month = as.character(month), year = as.character(year)))

sdmtmb_monthly_preds <- sdmtmb_monthly %>%
  predict(newdata = data_monthly_active %>%
            st_drop_geometry() %>%
            filter(year %in% 2018:2019),
          re_form = NA, re_form_iid = NA, se_fit = T) %>%
  mutate(date = make_date(month = as.character(month), year = as.character(year)))
write_rds(sdmtmb_monthly_preds, "data/chickens/model_predictions/sdmTMB/sdmtmb_monthly_preds.rds")

sdmtmb_seasonal_preds <- sdmtmb_seasonal %>%
  predict(newdata = data_seasonal_active %>%
            st_drop_geometry() %>%
            filter(year %in% 2016:2019),
          re_form = NA, re_form_iid = NA, se_fit = T)
write_rds(sdmtmb_seasonal_preds, "data/chickens/model_predictions/sdmTMB/sdmtmb_seasonal_preds.rds")
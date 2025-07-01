
pacman::p_load(
  tidyverse
  , terra
  , sf
  , sdmTMB
  , INLA
  , evaluate
  # ,
  # ,
)

# setwd("/blue/guralnick/jbaecher/chickens")
# setwd("D:\\UFL Dropbox\\Joseph Baecher\\UF\\postdoc\\chickens")

###############################################################################################################################################
############################################################# load models #############################################################
###############################################################################################################################################

sdmtmb_seasonal <- read_rds("data/chickens/model_predictions/sdmTMB/sdmtmb_seasonal_update.rds")

sdmtmb_monthly <- read_rds("data/chickens/model_predictions/sdmTMB/sdmtmb_monthly_update.rds")

#############################################################################################################################################



###############################################################################################################################################
############################################################# get statewide preds #############################################################
###############################################################################################################################################

seasonal_offset <- log(mean(sdmtmb_seasonal$data$testing) + 1)
monthly_offset <- log(mean(sdmtmb_monthly$data$testing) + 1)


#### seasonal ####

env_seasonal_df <- read_rds("data/environment/env_covs_seasonal.rds")

env_seasonal_df_preds <- bind_cols(
  env_seasonal_df, 
  data.frame(
    # testing = rep(0, nrow(env_seasonal_df)), 
    county = rep(NA, nrow(env_seasonal_df)),  
    ID = rep(NA, nrow(env_seasonal_df)))) %>%
  filter(season_name == "active") %>%
  filter(!year %in% c("2020", "2021", "2022")) %>%
  rename("Y" = "lat", 
         "X" = "lon") %>%
  left_join(
    data.frame(
      year = 2001:2019) %>%
      mutate(time_step = row_number(),
             year = as.factor(year)) %>%
      select(year, time_step)) %>%
  mutate(offset = seasonal_offset)

sdmtmb_seasonal_state_preds <- sdmtmb_seasonal %>%
  predict(newdata = env_seasonal_df_preds, 
          se_fit = F,
          # re_form = NA,
          re_form_iid = NA, 
          offset = env_seasonal_df_preds$offset)

write_rds(sdmtmb_seasonal_state_preds, "data/chickens/model_predictions/sdmTMB/sdmtmb_seasonal_state_preds_rf.rds")
sdmtmb_seasonal_state_preds <- read_rds("data/chickens/model_predictions/sdmTMB/sdmtmb_seasonal_state_preds_rf.rds")
#### monthly ####

env_monthly_df <- read_rds("data/environment/env_covs_monthly.rds")

env_monthly_df_preds <- bind_cols(
  env_monthly_df, 
  data.frame(
    testing = rep(0, nrow(env_monthly_df)), 
    county = rep(NA, nrow(env_monthly_df)),  
    ID = rep(NA, nrow(env_monthly_df)))) %>%
  filter(season_name == "active") %>%
  filter(!year %in% c("2020", "2021", "2022")) %>%
  rename("Y" = "lat", 
         "X" = "lon") %>%
  left_join(
    data.frame(
      month = rep(6:12, length(2001:2019)),
      year = rep(2001:2019, length(6:12))) %>%
      arrange(year, month) %>%
      mutate(time_step = row_number(),
             year = as.factor(year),
             month = as.character(month)) %>%
      select(month, year, time_step)) %>%
  mutate(offset = monthly_offset)

sdmtmb_monthly_state_preds <- sdmtmb_monthly %>%
  predict(newdata = env_monthly_df_preds, 
          se_fit = F,
          # re_form = NA, 
          re_form_iid = NA, 
          offset = env_monthly_df_preds$offset)

write_rds(sdmtmb_monthly_state_preds, "data/chickens/model_predictions/sdmTMB/sdmtmb_monthly_state_preds_rf.rds")

###############################################################################################################################################


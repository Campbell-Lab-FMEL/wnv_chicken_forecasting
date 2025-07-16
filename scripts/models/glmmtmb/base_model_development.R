
pacman::p_load(
  tidyverse
  , sf
  , performance
  , glmmTMB
  , AICcmodavg
  , sjPlot
  , DHARMa
  , buildmer
  # ,
  # ,
)

conflicted::conflict_prefer_all("dplyr", quiet = T)


#############################################################################################################################################
################################################### Loading custom functions ################################################################ 
#############################################################################################################################################

source("functions/compare_models.R")
source("functions/extract_model_data.R")
source("functions/buildmermod_to_glmmtmb.R")
source("functions/stepwise_vif.R")

#############################################################################################################################################






#############################################################################################################################################
###################################################### Processing chicken data ############################################################## 
#############################################################################################################################################

### Monthly data
data_monthly <- read_rds("data/chickens/monthly/wnv_eeev_env_covs.rds") %>%
  left_join(
    data.frame(
      month = rep(1:12, 19), 
      year = rep(2001:2019, each = 12)) %>%
      mutate(time_step = row_number()) %>%
      select(year, month, time_step), 
    by = c("year", "month")) %>%
  mutate_at(
    vars(
      starts_with(c("prcp", "tmax", "tmin")), 
      "developed", "natural", "wetlands"), 
    .funs = function(x){as.numeric(scale(x))}) %>%
  mutate_at(vars("year", "season", "ID", "county", "time_step"), as.factor) %>%
  mutate(season = ifelse(season == 1, "inactive", "active"))

# removing 2019 will hold out ~ 5% of observations 

data_monthly_train <- data_monthly %>%
  filter(season == "active") %>%
  filter(!year %in% c(2018, 2019)) 

data_monthly_pred <- data_monthly %>%
  filter(season == "active") %>%
  filter(year %in% c(2018, 2019)) 

### Seasonal data

data_seasonal <- read_rds("data/chickens/seasonal/wnv_eeev_env_covs.rds") %>%
  left_join(
    data.frame(
      season = rep(1:2, 19),
      year = rep(2001:2019, each = 2)) %>%
      mutate(time_step = row_number()) %>%
      dplyr::select(year, season, time_step), 
    by = c("year", "season")) %>%
  mutate_at(
    vars(
      starts_with(c("prcp", "tmax", "tmin")), 
      "developed", "natural", "wetlands"), 
    .funs = function(x){as.numeric(scale(x))}) %>%
  mutate_at(vars("year", "season", "ID", "county", "time_step"), as.factor) %>%
  mutate(season = ifelse(season == 1, "inactive", "active"),
         testing = scale(testing)[,1])

# removing 2019 will hold out ~ 6% of observations 

data_seasonal_train <- data_seasonal %>%
  filter(season == "active") %>%
  filter(!year %in% 2016:2019) 

data_seasonal_pred <- data_seasonal %>%
  filter(season == "active")  %>%
  filter(year %in% 2016:2019)

#############################################################################################################################################




#############################################################################################################################################
############################################# Beginning Tier 1 of modeling workflow ######################################################### 
#############################################################################################################################################

##### Creating model formulas

t1_formulas <- c(
    "wnv ~ 1"
  
  , "wnv ~ prcp"
  , "wnv ~ prcp_lag1"
  , "wnv ~ prcp_lag2"
  
  , "wnv ~ poly(prcp, 2)"
  , "wnv ~ poly(prcp_lag1, 2)"
  , "wnv ~ poly(prcp_lag2, 2)"
  
  , "wnv ~ tmax"
  , "wnv ~ tmax_lag1"
  , "wnv ~ tmax_lag2"
  
  , "wnv ~ poly(tmax, 2)"
  , "wnv ~ poly(tmax_lag1, 2)"
  , "wnv ~ poly(tmax_lag2, 2)"
  
  , "wnv ~ tmin"
  , "wnv ~ tmin_lag1"
  , "wnv ~ tmin_lag2"
  
  , "wnv ~ poly(tmin, 2)"
  , "wnv ~ poly(tmin_lag1, 2)"
  , "wnv ~ poly(tmin_lag2, 2)"
  
  , "wnv ~ developed"
  , "wnv ~ natural"
  , "wnv ~ wetlands"
  
  , "wnv ~ poly(developed, 2)"
  , "wnv ~ poly(natural, 2)"
  , "wnv ~ poly(wetlands, 2)") 

#### Tier 1 models: 

### Monthly

t1_monthly_models <- compare_models(
  formulas = paste0(t1_formulas, " + year + testing + (1 | county / ID)"),
  ziformulas = "~0",
  families = "nbinom1",
  data = data_monthly_train)

t1_monthly_models$aictab 

# summarizing t1 monthly models

t1_monthly_vars <- bind_rows(
  bind_cols(extract_model_data(t1_monthly_models$models), season = "active")) %>%
  # filter(!zero_span == T) %>%
  filter(!str_detect(term, fixed("2)2")) == T) %>%
  filter(!str_detect(term, "year") == T) %>%
  filter(!term == "testing") %>%
  select(season, term, estimate, std.error, conf.low, conf.high, zero_span, sig) %>%
  as.data.frame()

#### Seasonal

t1_seasonal_models <- compare_models(
  formulas = paste0(t1_formulas, " + year + testing + (1 | county / ID)"),
  ziformulas = "~0",
  families = "nbinom1",
  data = data_seasonal_train)

t1_seasonal_models$aictab 

## summarize t1 seasonal models: 

t1_seasonal_vars <- bind_rows(
  bind_cols(extract_model_data(t1_seasonal_models$models), season = "active")) %>%
  # filter(!zero_span == T) %>%
  # filter(!str_detect(term, fixed("2)2")) == T) %>%
  filter(!str_detect(term, "year") == T) %>%
  filter(!term == "testing") %>%
  select(season, term, estimate, std.error, conf.low, conf.high, zero_span, sig) %>%
  as.data.frame()

##### Monthly:

## generate "formula()" compatible version of term names for monthly data
t2_monthly_sig_vars <- t1_monthly_vars %>%
  filter(!str_detect(term, "poly") == T) %>%
  filter(!str_detect(term, "year") == T) %>%
  filter(!zero_span == TRUE) %>%
  arrange(term) %>%
  select(season, term) %>%
  bind_rows(
    data.frame(
      t1_monthly_vars %>%
        filter(str_detect(term, "poly") == T) %>%
        filter(!str_detect(term, "year") == T) %>%
        filter(!zero_span == TRUE) %>%
        select(season, term) %>%
        mutate(term = paste0(str_split_fixed(term, fixed("2)"), n = 2)[,1], "2)")))) %>%
  arrange(season); t2_monthly_sig_vars

## create formula object for active season terms
t2_monthly_global_formula <- t2_monthly_sig_vars %>%
  filter(season == "active") %>%
  select(term) %>%
  as_vector() %>%
  paste(collapse = " + ") %>%
  as.data.frame(); t2_monthly_global_formula$.


# seasonal

## generate "formula()" compatible version of term names for seasonal data
t2_seasonal_sig_vars <- t1_seasonal_vars %>%
  filter(!str_detect(term, "poly") == T) %>%
  filter(!zero_span == TRUE) %>%
  arrange(term) %>%
  select(season, term) %>%
  bind_rows(
    data.frame(
      t1_seasonal_vars %>%
        filter(str_detect(term, "poly") == T) %>%
        filter(!str_detect(term, "year") == T) %>%
        filter(!zero_span == TRUE) %>%
        select(season, term) %>%
        mutate(term = paste0(str_split_fixed(term, fixed("2)"), n = 2)[,1], "2)"))  )  )

## create formula object for active season terms
t2_seasonal_global_formula <- t2_seasonal_sig_vars %>%
  filter(season == "active") %>%
  select(term) %>%
  as_vector() %>%
  paste(collapse = " + ") %>%
  as.data.frame(); t2_seasonal_global_formula$.


######

##### Seasonal:
seasonal_lin_vars <- data.frame(term = str_remove(t1_formulas, "wnv ~ ")[2:25]) %>%
  filter(!str_detect(term, fixed("poly(")))

seasonal_poly_vars <- data.frame(term = str_remove(t1_formulas, "wnv ~ ")[2:25]) %>%
  filter(str_detect(term, fixed("poly(")))

seasonal_poly_vars <- bind_rows(
  data.frame(term = paste0(seasonal_poly_vars$term, "1")),
  data.frame(term = paste0(seasonal_poly_vars$term, "2")))

seasonal_vars <- bind_rows(
  seasonal_lin_vars,
  seasonal_poly_vars) 

seasonal_var_sel <- seasonal_vars %>%
  left_join(t1_seasonal_vars) %>%
  select(-c(season, std.error, zero_span, sig)) %>%
  rename(est_t1 = estimate, low_t1 = conf.low, high_t1 = conf.high) %>%
  left_join(
    t1_seasonal_vars %>%
      filter(!zero_span == TRUE) %>%
      select(-c(season, std.error, zero_span, sig)) %>%
      rename(est_t2 = estimate, low_t2 = conf.low, high_t2 = conf.high))
write_rds(seasonal_var_sel, "data/chickens/model_predictions/glmmTMB/seasonal_var_sel.rds")

t2_seasonal_fixed <- broom.mixed::tidy(t2_seasonal_glmmtmb$model)

#############################################################################################################################################




#############################################################################################################################################
############################################# Beginning Tier 2 of modeling workflow ######################################################### 
#############################################################################################################################################

## Buildmer models for VIF calculation 

## Buildmer model: Seasonal - Active model 
t2_seasonal_buildmer_model <- buildglmmTMB(
  formula(paste0("wnv ~ ", t2_seasonal_global_formula$., " + year + offset(log(testing+1)) + (1 | county / ID)")),
  buildmerControl = list(direction = 'backward',
                         include = ~ year + offset(log(testing+1)) + (1 | county/ID),
                         args = list(ziformula = ~0)),
  family = "nbinom1",
  data = data_seasonal_train)
write_rds(t2_seasonal_buildmer_model, "data/chickens/model_predictions/glmmTMB/t2_seasonal_buildermod.rds")

t2_seasonal_buildmer_model <- read_rds("data/chickens/model_predictions/glmmTMB/t2_seasonal_buildermod.rds")

t2_seasonal_glmmtmb <- buildmermod_to_glmmtmb(buildmermod = t2_seasonal_buildmer_model,
                                                     data = data_seasonal_train,
                                                     response = "wnv",
                                                     ziformula = "~0",
                                                     fixed = "year + offset(log(testing+1))",
                                                     ranef = "(1 | county / ID)")
t2_seasonal_glmmtmb[["vif"]] <- check_collinearity(t2_seasonal_glmmtmb$model)

write_rds(t2_seasonal_glmmtmb, "data/chickens/model_predictions/glmmTMB/t2_seasonal_glmmtmb.rds")
t2_seasonal_glmmtmb <- read_rds("data/chickens/model_predictions/glmmTMB/t2_seasonal_glmmtmb.rds")

###### Buildmer model: Monthly - Active model 

t2_monthly_buildmer_model <- buildglmmTMB(
  formula(paste0("wnv ~ ", t2_monthly_global_formula$., " + year + offset(log(testing+1)) + (1 | county / ID)")),
  buildmerControl = list(direction = 'backward',
                         include = ~ year + offset(log(testing+1)) + (1 | county/ID),
                         args = list(ziformula = ~0)),
  family = "nbinom1",
  data = data_monthly_train)
write_rds(t2_monthly_buildmer_model, "data/chickens/model_predictions/glmmTMB/t2_monthly_buildermod.rds")
t2_monthly_buildmer_model <- read_rds("data/chickens/model_predictions/glmmTMB/t2_monthly_buildermod.rds")

t2_monthly_glmmtmb <- buildmermod_to_glmmtmb(buildmermod = t2_monthly_buildmer_model,
                                                    data = data_monthly_train,
                                                    fixed = "year + offset(log(testing+1))",
                                                    ziformula = "~0",
                                                    response = "wnv",
                                                    ranef = "(1 | county / ID)")

t2_monthly_glmmtmb[["vif"]] <- check_collinearity(t2_monthly_glmmtmb$model)

write_rds(t2_monthly_glmmtmb, "data/chickens/model_predictions/glmmTMB/t2_monthly_glmmtmb.rds")
t2_monthly_glmmtmb <- read_rds("data/chickens/model_predictions/glmmTMB/t2_monthly_glmmtmb.rds")

#########################

t2_seasonal_glmmtmb$vif
t2_monthly_glmmtmb$vif

monthly_stepwise_vif <- stepwise_vif(
  variables = t2_monthly_glmmtmb$variables %>%
    mutate(term = str_replace(term, pattern = fixed("2)1"), replacement = fixed("2)"))),
  vif_thresh = 5, 
  response = "wnv",
  keep_fixed = "year + offset(log(testing+1))",
  keep_raneff = "(1 | county / ID)",
  ziformula = "~0",
  family = "nbinom1",
  data = data_monthly_train
)

write_rds(monthly_stepwise_vif, "data/chickens/model_predictions/monthly_glmmtmb_final_model.rds")
monthly_stepwise_vif <- read_rds("data/chickens/model_predictions/glmmTMB/monthly_glmmtmb_final_model.rds")
monthly_stepwise_vif$model

seasonal_stepwise_vif <- stepwise_vif(
  variables = t2_seasonal_glmmtmb$variables %>%
    mutate(term = str_replace(term, pattern = fixed("2)1"), replacement = fixed("2)"))),
  vif_thresh = 5, 
  response = "wnv",
  keep_fixed = "year + offset(log(testing+1))",
  keep_raneff = "(1 | county / ID)",
  ziformula = "~0",
  family = "nbinom1",
  data = data_seasonal_train
)
write_rds(seasonal_stepwise_vif, "data/chickens/model_predictions/glmmTMB/seasonal_glmmtmb_final_model.rds")
seasonal_stepwise_vif <- read_rds("data/chickens/model_predictions/glmmTMB/seasonal_glmmtmb_final_model.rds")

seasonal_final_variables <- seasonal_stepwise_vif$model %>%
  broom.mixed::tidy() %>% 
  filter(!str_detect(term, fixed("2)2"))) %>%
  filter(!str_detect(term, fixed("year")))

monthly_final_variables <- monthly_stepwise_vif$model %>%
  broom.mixed::tidy() %>% 
  filter(!str_detect(term, fixed("2)2"))) %>%
  filter(!str_detect(term, fixed("year")))

#############################################################################################################################################

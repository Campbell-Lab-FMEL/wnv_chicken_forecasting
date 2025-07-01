
pacman::p_load(
  tidyverse
  , terra
  , sf
  , sp
  , sdmTMB
  , INLA
  , qpcR
  # ,
  # ,
); conflicted::conflict_prefer_all("dplyr", quiet = T)

#############################################################################################################################################




#############################################################################################################################################
###################################################### Processing chicken data ############################################################## 
#############################################################################################################################################

### Monthly data

data_monthly <- read_rds("data/chickens/monthly/wnv_eeev_env_covs.rds") %>%
  mutate_at(vars(starts_with(c("prcp", "tmax", "tmin")), "developed", "natural", "wetlands"), 
            .funs = function(x){as.numeric(scale(x))}) %>%
  mutate_at(vars("year", "season", "ID", "county"), as.factor) %>%
  mutate(season = ifelse(season == 1, "inactive", "active")) %>%
  st_transform(32617) %>%
  mutate(geometry = geometry / 1000) %>%
  bind_cols(as.data.frame(st_coordinates(.))) %>%
  mutate(date = make_date(month = as.character(month), year = as.character(year))) 

# setting training and prediction data
data_monthly_active <- data_monthly %>%
  filter(season == "active") %>%
  left_join(
    data.frame(
      month = rep(6:12, length(2001:2019)),
      year = rep(2001:2019, length(6:12))) %>%
      arrange(year, month) %>%
      mutate(time_step = row_number(),
             year = as.factor(year),
             date = make_date(month = as.character(month), year = as.character(year))) %>%
    select(date, month, year, time_step)) %>%
  mutate(fold = ifelse(year %in% 2018:2019, 1, 2))

data_monthly_train <- data_monthly_active %>%
  filter(!year %in% 2018:2019)

### Seasonal data

data_seasonal <- read_rds("data/chickens/seasonal/wnv_eeev_env_covs.rds") %>%
  mutate(county_ID = paste(county, ID, sep = "_") %>% as.factor()) %>%
  mutate_at(vars(starts_with(c("prcp", "tmax", "tmin")), "developed", "natural", "wetlands"), 
            .funs = function(x){as.numeric(scale(x))}) %>%
  mutate_at(vars("year", "season", "ID", "county"), as.factor) %>%
  mutate(season = ifelse(season == 1, "inactive", "active")) %>%
  left_join(
    data.frame(
      year = 2001:2019) %>%
      mutate(time_step = row_number(),
             year = as.factor(year),
             fold = ifelse(year %in% 2016:2019, 1, 2)) %>%
      select(year, time_step)) %>%
  st_transform(32617) %>%
  mutate(geometry = geometry / 1000) %>%
  bind_cols(as.data.frame(st_coordinates(.)))

# setting training and prediction data
data_seasonal_active <- data_seasonal %>%
  filter(season == "active") %>%
  mutate(fold = ifelse(year %in% 2016:2019, 1, 2))

data_seasonal_train <- data_seasonal_active %>%
  filter(!year %in% 2016:2019)

#############################################################################################################################################



###############################################################################################################################################
############################################################ Creating a spatial mesh ##########################################################
###############################################################################################################################################

locs_seasonal <- data_seasonal_train %>%
  st_coordinates() %>%
  as.data.frame()

locs_monthly <- data_monthly_train %>%
  st_coordinates() %>%
  as.data.frame()

fl_crop <- read_rds("data/fl_polygon_crop.rds")

fl_coords <- fl_crop %>%
  st_buffer(2) %>%
  st_sample(8000) %>%
  st_coordinates() %>%
  as.data.frame() %>%
  bind_rows(locs_monthly)

{domain <- fl_coords %>%
    coordinates() %>%
    inla.nonconvex.hull(convex = -.02,
                        resolution = c(56,51)); 
  plot(fl_crop %>% st_geometry(), border = "red"); 
  lines(domain); 
  plot(data_seasonal["county"], add = T)}

max_edge <- locs_seasonal$X %>%
  range() %>%
  diff();

offset_fact1 <- 1
offset_fact2 <- 20
maxedge_fact <- (3*5)
cutoff_fact <- 5
n_knots <- 100

mesh_domain_seasonal <- fmesher::fm_mesh_2d_inla(
  loc = locs_seasonal,
  boundary = domain,
  max.edge = c(5,10)*((max_edge)/maxedge_fact),
  offset = c(max_edge/offset_fact1, max_edge/offset_fact2),
  cutoff = cutoff_fact); mesh_domain_seasonal$n; plot(mesh_domain_seasonal)

mesh_domain_monthly <- fmesher::fm_mesh_2d_inla(
  loc = locs_monthly,
  boundary = domain,
  max.edge = c(5,10)*((max_edge)/maxedge_fact),
  offset = c(max_edge/offset_fact1, max_edge/offset_fact2),
  cutoff = cutoff_fact); mesh_domain_monthly$n; plot(mesh_domain_monthly)

sdmTMB_mesh_seasonal <- make_mesh(locs_seasonal, c("X", "Y"),
                                  n_knots = n_knots,
                                  mesh = mesh_domain_seasonal); plot(sdmTMB_mesh_seasonal, 
                                                                     col = "green")

sdmTMB_mesh_monthly <- make_mesh(locs_monthly, c("X", "Y"),
                                 n_knots = n_knots,
                                 mesh = mesh_domain_monthly); plot(sdmTMB_mesh_monthly, col = "green")

###############################################################################################################################################

 



###############################################################################################################################################
############################################################## Running sdmTMB models ##########################################################
###############################################################################################################################################

seasonal_offset <- log(data_seasonal_train$testing + 1)
monthly_offset <- log(data_monthly_train$testing + 1)

sdmtmb_seasonal <- sdmTMB(
  wnv ~ 1 + poly(prcp, 2) + poly(prcp_lag1, 2) + poly(tmax_lag2, 2) + poly(tmin_lag1, 2) + (1 | county) + (1 | ID),
  offset = seasonal_offset,
  time = "time_step",
  extra_time = 16:19,
  mesh = sdmTMB_mesh_seasonal,
  spatial = "off",
  spatiotemporal = "ar1",
  family = nbinom1(),
  data = data_seasonal_train %>% st_drop_geometry())
write_rds(sdmtmb_seasonal,  "data/chickens/model_predictions/sdmTMB/sdmtmb_seasonal_update.rds")
sdmtmb_seasonal <- read_rds("data/chickens/model_predictions/sdmTMB/sdmtmb_seasonal_update.rds")

sdmtmb_seasonal_fixed_params <- tidy(sdmtmb_seasonal, effects = "fixed")
sanity(sdmtmb_seasonal)

sdmtmb_monthly <- sdmTMB(
  wnv ~ 1 + poly(developed, 2) + poly(prcp, 2) + poly(prcp_lag1, 2) + poly(prcp_lag2, 2) +
        poly(tmax, 2) + poly(tmin_lag2, 2) + poly(wetlands, 2) +
        (1 | county) + (1 | ID),
  offset = monthly_offset,
  time = "time_step",
  extra_time = 120:133,
  mesh = sdmTMB_mesh_monthly,
  spatial = "off",
  spatiotemporal = "ar1",
  family = nbinom1(),
  data = data_monthly_train %>% st_drop_geometry())
write_rds(sdmtmb_monthly,  "data/chickens/model_predictions/sdmTMB/sdmtmb_monthly_update.rds")
sdmtmb_monthly <- read_rds("data/chickens/model_predictions/sdmTMB/sdmtmb_monthly_update.rds")

sdmtmb_monthly_fixed_params <- tidy(sdmtmb_monthly, effects = "fixed")
sanity(sdmtmb_monthly)

###############################################################################################################################################

sdmtmb_seasonal_ranef <- sdmTMB(
  wnv ~ (1 | county) + (1 | ID),
  offset = seasonal_offset, 
  time = "time_step",
  extra_time = 16:19,
  mesh = sdmTMB_mesh_seasonal,
  spatial = "off",
  spatiotemporal = "ar1",
  family = sdmTMB::nbinom1(),
  data = data_seasonal_train %>% st_drop_geometry())

sdmtmb_seasonal_fixed <- sdmTMB(
  wnv ~ 1 + poly(prcp, 2) + poly(prcp_lag1, 2) + poly(tmax_lag2, 2) + poly(tmin_lag1, 2),
  offset = seasonal_offset, 
  time = "time_step",
  extra_time = 16:19,
  mesh = sdmTMB_mesh_seasonal,
  spatial = "off",
  spatiotemporal = "ar1",
  family = sdmTMB::nbinom1(),
  data = data_seasonal_train %>% st_drop_geometry()) 

sdmtmb_seasonal_intercept <- sdmTMB(
  wnv ~ 1,
  offset = seasonal_offset, 
  time = "time_step",
  extra_time = 16:19,
  mesh = sdmTMB_mesh_seasonal,
  spatial = "off",
  spatiotemporal = "ar1",
  family = sdmTMB::nbinom1(),
  data = data_seasonal_train %>% st_drop_geometry())

sdmtmb_monthly_ranef <- sdmTMB(
  wnv ~ (1 | county) + (1 | ID),
  offset = monthly_offset, 
  time = "time_step",
  extra_time = 120:133,
  mesh = sdmTMB_mesh_monthly,
  spatial = "off",
  spatiotemporal = "ar1",
  family = sdmTMB::nbinom1(),
  data = data_monthly_train %>% st_drop_geometry())

sdmtmb_monthly_fixed <- sdmTMB(
  wnv ~ 1 + poly(developed, 2) + poly(prcp, 2) + poly(prcp_lag1, 2) + poly(prcp_lag2, 2) + 
    poly(tmax, 2) + poly(tmin_lag2, 2) + poly(wetlands, 2),  
  offset = monthly_offset, 
  time = "time_step",
  extra_time = 120:133,
  mesh = sdmTMB_mesh_monthly,
  spatial = "off",
  spatiotemporal = "ar1",
  family = sdmTMB::nbinom1(),
  data = data_monthly_train %>% st_drop_geometry())

sdmtmb_monthly_intercept <- sdmTMB(
  wnv ~ 1,
  offset = monthly_offset, 
  time = "time_step",
  extra_time = 120:133,
  mesh = sdmTMB_mesh_monthly,
  spatial = "off",
  spatiotemporal = "ar1",
  family = sdmTMB::nbinom1(),
  data = data_monthly_train %>% st_drop_geometry())

glmmtmb_seasonal <- sdmTMB(
  wnv ~ 1 + poly(prcp, 2) + poly(prcp_lag1, 2) + poly(tmax_lag2, 2) + 
    poly(tmin_lag1, 2) + (1 | county) + (1 | ID),
  offset = seasonal_offset,
  time = "time_step",
  extra_time = 16:19,
  mesh = sdmTMB_mesh_seasonal,
  spatial = "off",
  spatiotemporal = "off",
  family = nbinom1(),
  data = data_seasonal_train %>% st_drop_geometry())

glmmtmb_seasonal_ranef <- sdmTMB(
  wnv ~ (1 | county) + (1 | ID),
  offset = seasonal_offset, 
  time = "time_step",
  extra_time = 16:19,
  mesh = sdmTMB_mesh_seasonal,
  spatial = "off",
  spatiotemporal = "off",
  family = sdmTMB::nbinom1(),
  data = data_seasonal_train %>% st_drop_geometry())

glmmtmb_seasonal_intercept <- sdmTMB(
  wnv ~ 1,
  offset = seasonal_offset, 
  time = "time_step",
  extra_time = 16:19,
  mesh = sdmTMB_mesh_seasonal,
  spatial = "off",
  spatiotemporal = "off",
  family = sdmTMB::nbinom1(),
  data = data_seasonal_train %>% st_drop_geometry())

glmmtmb_monthly <- sdmTMB(
  wnv ~ 1 + poly(developed, 2) + poly(prcp, 2) + poly(prcp_lag1, 2) + poly(prcp_lag2, 2) +
    poly(tmax, 2) + poly(tmin_lag2, 2) + poly(wetlands, 2) +
    (1 | county) + (1 | ID),
  offset = monthly_offset,
  time = "time_step",
  extra_time = 120:133,
  mesh = sdmTMB_mesh_monthly,
  spatial = "off",
  spatiotemporal = "off",
  family = nbinom1(),
  data = data_monthly_train %>% st_drop_geometry())

glmmtmb_monthly_ranef <- sdmTMB(
  wnv ~ (1 | county) + (1 | ID),
  offset = monthly_offset, 
  time = "time_step",
  extra_time = 120:133,
  mesh = sdmTMB_mesh_monthly,
  spatial = "off",
  spatiotemporal = "off",
  family = sdmTMB::nbinom1(),
  data = data_monthly_train %>% st_drop_geometry())

glmmtmb_monthly_intercept <- sdmTMB(
  wnv ~ 1,
  offset = monthly_offset, 
  time = "time_step",
  extra_time = 120:133,
  mesh = sdmTMB_mesh_monthly,
  spatial = "off",
  spatiotemporal = "off",
  family = sdmTMB::nbinom1(),
  data = data_monthly_train %>% st_drop_geometry())

#################################

seasonal_mods <- list(
  sdmtmb_seasonal,
  sdmtmb_seasonal_ranef,
  sdmtmb_seasonal_fixed,
  sdmtmb_seasonal_intercept,
  glmmtmb_seasonal,
  glmmtmb_seasonal_ranef,
  glmmtmb_seasonal_intercept)
write_rds(seasonal_mods, "data/chickens/model_predictions/sdmTMB/selection/seasonal_mods_selection.rds")
seasonal_mods <- read_rds("data/chickens/model_predictions/sdmTMB/selection/seasonal_mods_selection.rds")

monthly_mods <- list(
  sdmtmb_monthly,
  sdmtmb_monthly_ranef,
  sdmtmb_monthly_fixed,
  sdmtmb_monthly_intercept,
  glmmtlmb_monthly,
  glmmtmb_monthly_ranef,
  glmmtmb_monthly_intercept)
write_rds(monthly_mods, "data/chickens/model_predictions/sdmTMB/selection/monthly_mods.rds")
monthly_mods <- read_rds("data/chickens/model_predictions/sdmTMB/selection/monthly_mods.rds")

######

seasonal_mods_devexp <- data.frame(
  model = c(
    "GMRF, fixed effects, random effects",
    "GMRF, random effects",
    "GMRF, fixed effects",
    "GMRF, intercepts",
    "Non-spatial, fixed effects, random effects",
    "Non-spatial, random effects",
    "Non-spatial, intercept"),
  devexp = lapply(seasonal_mods, FUN = function(x){
    1 - deviance(x) / deviance(seasonal_mods[[7]])
    }) %>%
    unlist()); seasonal_mods_devexp

monthly_mods_devexp <- data.frame(
  model = c(
    "GMRF, fixed effects, random effects",
    "GMRF, random effects",
    "GMRF, fixed effects",
    "GMRF, intercepts",
    "Non-spatial, fixed effects, random effects",
    "Non-spatial, random effects",
    "Non-spatial, intercept"),
  devexp = lapply(monthly_mods, FUN = function(x){
    1 - deviance(x) / deviance(monthly_mods[[7]])
  }) %>%
    unlist()); monthly_mods_devexp

# seasonal_mods_edf <- lapply(
#   seasonal_mods[1:6], FUN = function(x){
#     cAIC(x, what = "EDF")
#   }); seasonal_mods_edf

# monthly_mods_edf <- lapply(
#   monthly_mods[1:6], FUN = function(x){
#     cAIC(x, what = "EDF")
#   }); monthly_mods_edf

seasonal_mods_edf <- list()
seasonal_mods_edf[["GMRF, fixed effects, random effects"]]        <- cAIC(seasonal_mods[[1]], what = "EDF")
seasonal_mods_edf[["GMRF, random effects"]]                       <- cAIC(seasonal_mods[[2]], what = "EDF")
seasonal_mods_edf[["GMRF, fixed effects"]]                        <- cAIC(seasonal_mods[[3]], what = "EDF")
seasonal_mods_edf[["GMRF, intercepts"]]                           <- cAIC(seasonal_mods[[4]], what = "EDF")
seasonal_mods_edf[["Non-spatial, fixed effects, random effects"]] <- cAIC(seasonal_mods[[5]], what = "EDF")
seasonal_mods_edf[["Non-spatial, random effects"]]                <- cAIC(seasonal_mods[[6]], what = "EDF")

monthly_mods_edf <- list()
monthly_mods_edf[["GMRF, fixed effects, random effects"]]        <- cAIC(monthly_mods[[1]], what = "EDF")
monthly_mods_edf[["GMRF, random effects"]]                       <- cAIC(monthly_mods[[2]], what = "EDF")
monthly_mods_edf[["GMRF, fixed effects"]]                        <- cAIC(monthly_mods[[3]], what = "EDF")
monthly_mods_edf[["GMRF, intercepts"]]                           <- cAIC(monthly_mods[[4]], what = "EDF")
monthly_mods_edf[["Non-spatial, fixed effects, random effects"]] <- cAIC(monthly_mods[[5]], what = "EDF")
monthly_mods_edf[["Non-spatial, random effects"]]                <- cAIC(monthly_mods[[6]], what = "EDF")

#####

aic_seasonal <- data.frame(
  model = c(
    "GMRF, fixed effects, random effects",
    "GMRF, random effects",
    "GMRF, fixed effects",
    "GMRF, intercepts",
    "Non-spatial, fixed effects, random effects",
    "Non-spatial, random effects",
    "Non-spatial, intercept"),
  aic = lapply(seasonal_mods, FUN = function(x){
  AIC(x)}) %>%
  unlist()) %>% 
  mutate(delta_aic = aic - min(aic), 
         aic_weights = round(qpcR::akaike.weights(aic)$weights, 3))

aic_monthly <- data.frame(
  model = c(
    "GMRF, fixed effects, random effects",
    "GMRF, random effects",
    "GMRF, fixed effects",
    "GMRF, intercepts",
    "Non-spatial, fixed effects, random effects",
    "Non-spatial, random effects",
    "Non-spatial, intercept"),
  aic = lapply(monthly_mods, FUN = function(x){
    AIC(x)}) %>%
    unlist()) %>% 
  mutate(delta_aic = aic - min(aic), 
         aic_weights = round(qpcR::akaike.weights(aic)$weights, 3))


##################

###############################################################################################################################################
################################################################ cross validation #############################################################
###############################################################################################################################################

locs_seasonal_cv <- data_seasonal_active %>%
  st_coordinates() %>%
  as.data.frame()

locs_monthly_cv <- data_monthly_active %>%
  st_coordinates() %>%
  as.data.frame()

max_edge_cv <- locs_seasonal_cv$X %>%
  range() %>%
  diff();

mesh_domain_seasonal_cv <- fmesher::fm_mesh_2d_inla(
  loc = locs_seasonal_cv,
  boundary = domain,
  max.edge = c(5,10)*((max_edge_cv)/maxedge_fact),
  offset = c(max_edge_cv/offset_fact1, max_edge_cv/offset_fact2),
  cutoff = cutoff_fact); mesh_domain_seasonal_cv$n; plot(mesh_domain_seasonal_cv)

mesh_domain_monthly_cv <- fmesher::fm_mesh_2d_inla(
  loc = locs_monthly_cv,
  boundary = domain,
  max.edge = c(5,10)*((max_edge)/maxedge_fact),
  offset = c(max_edge/offset_fact1, max_edge/offset_fact2),
  cutoff = cutoff_fact); mesh_domain_monthly$n; plot(mesh_domain_monthly)

sdmTMB_mesh_seasonal_cv <- make_mesh(locs_seasonal_cv, c("X", "Y"),
                                  n_knots = n_knots,
                                  mesh = mesh_domain_seasonal_cv); plot(sdmTMB_mesh_seasonal_cv, col = "green")

sdmTMB_mesh_monthly_cv <- make_mesh(locs_monthly_cv, c("X", "Y"),
                                 n_knots = n_knots,
                                 mesh = mesh_domain_monthly_cv); plot(sdmTMB_mesh_monthly_cv, col = "green")

###############################################################################################################################################

library(future)
plan(multisession, workers = 2)

data_seasonal_active_cv <- data_seasonal_active %>%
  mutate(offset = log(testing + 1))

data_monthly_active_cv <- data_monthly_active %>%
  mutate(offset = log(testing + 1))

sdmtmb_seasonal_cv <- sdmTMB_cv(
  wnv ~ 1 + poly(prcp, 2) + poly(prcp_lag1, 2) + poly(tmax_lag2, 2) + poly(tmin_lag1, 2) + (1 | county) + (1 | ID),
  offset = "offset",
  time = "time_step",
  mesh = sdmTMB_mesh_seasonal_cv,
  spatial = "off",
  spatiotemporal = "ar1",
  family = nbinom1(),
  fold_ids = "fold",
  k_folds = 2,
  data = data_seasonal_active_cv %>% st_drop_geometry())

write_rds(sdmtmb_seasonal_cv, "data/chickens/model_predictions/sdmtmb_seasonal_cv.rds")
sdmtmb_seasonal_cv <- read_rds("data/chickens/model_predictions/sdmtmb_seasonal_cv.rds")

sanity(sdmtmb_seasonal_cv$models[[1]])
sanity(sdmtmb_seasonal_cv$models[[2]])

sdmtmb_seasonal_cv$fold_loglik[[1]] # -2403.566 # this should be the out-of-sample fold (the more positive the better)
sdmtmb_seasonal_cv$fold_loglik[[2]] # -10465.43 # this should be the within-sample fold (the more positive the better)
sdmtmb_seasonal_cv$sum_loglik       # -12869 # this is the overall LL (training and test sets)
sdmtmb_seasonal %>% logLik()        # -5580.219 # this is the regular model log likelihood

# the out-of-sample LL is nearly 4x more positive than the within-sample LL, and roughly twice as low as the original training model
####

sdmtmb_monthly_cv <- sdmTMB_cv(
    wnv ~ 1 + poly(developed, 2) + poly(prcp, 2) + poly(prcp_lag1, 2) + poly(prcp_lag2, 2) +
          poly(tmax, 2) + poly(tmin_lag2, 2) + poly(wetlands, 2) +
          (1 | county) + (1 | ID),
  offset = "offset",
  time = "time_step",
  mesh = sdmTMB_mesh_monthly_cv,
  spatial = "off",
  spatiotemporal = "ar1",
  family = nbinom1(),
  fold_ids = "fold",
  k_folds = 2,
  data = data_monthly_active_cv %>% st_drop_geometry())

write_rds(sdmtmb_monthly_cv, "data/chickens/model_predictions/sdmtmb_monthly_cv.rds")
sdmtmb_monthly_cv <- read_rds("data/chickens/model_predictions/sdmtmb_monthly_cv.rds")

sdmtmb_monthly_cv$fold_loglik[[1]] # -3991.648 # this should be the out-of-sample fold (the more positive the better)
sdmtmb_monthly_cv$fold_loglik[[2]] # -16952.25 # this should be the within-sample fold (the more positive the better)
sdmtmb_monthly_cv$sum_loglik       # -11672.51 # this is the overall LL (training and test sets)
sdmtmb_monthly %>% logLik()        # -11715.89 # this is the regular model log likelihood

# the out-of-sample LL is over 4x more positive than the within-sample LL, and nearly 3x lower than the original training model


pacman::p_load(
  tidyverse
  , terra
  , sf
  , sdmTMB
  , INLA
  , inlabru
  , lubridate
  , ggdist
  , sp
  # ,
  # ,
); conflicted::conflict_prefer_all("dplyr", quiet = T)



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
             year = as.factor(year)) %>%
      mutate(date = make_date(month = as.character(month), year = as.character(year))) %>%
      select(month, year, date, time_step))

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
             year = as.factor(year)) %>%
      select(year, time_step)) %>%
  st_transform(32617) %>%
  mutate(geometry = geometry / 1000) %>%
  bind_cols(as.data.frame(st_coordinates(.)))

# setting training and prediction data
data_seasonal_active <- data_seasonal %>%
  filter(season == "active") 

data_seasonal_train <- data_seasonal_active %>%
  filter(!year %in% 2016:2019)

#############################################################################################################################################





###############################################################################################################################################
############################################################ Creating a spatial mesh ##########################################################
###############################################################################################################################################

## extract coords
locs_seasonal <- data_seasonal_train %>%
  st_coordinates() %>%
  as.data.frame()

locs_monthly <- data_monthly_train %>%
  st_coordinates() %>%
  as.data.frame()

## Load polygon for spatial domain 
fl_crop <- read_rds("data/fl_polygon_crop.rds")

### Sample polygon to create a non-convex hull of spatial area
fl_coords <- fl_crop %>%
  st_buffer(2) %>%
  st_sample(8000) %>%
  st_coordinates() %>%
  as.data.frame() %>%
  bind_rows(locs_monthly)

### Fit non-convex hull of sampled spatial area
domain <- fl_coords %>%
    coordinates() %>%
    inla.nonconvex.hull(convex = -.02,
                        resolution = c(56,51))

## Calculating mesh parameters: max edge, offset, and cutoff 

### Calculate longitudinal range of spatial area as a rule-of-thumb for parameterizing max edge
max_edge <- locs_seasonal %>%
  select(X) %>%
  range() %>%
  diff();

### Setting offset factors (for interior and exterior portions of the mesh), cutoff factors, and knots
offset_fact1 <- 1
offset_fact2 <- 20
maxedge_fact <- (3*5)
cutoff_fact <- 5
n_knots <- 100

## Create mesh with spatial domain
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

## Translate domain mesh into a mesh object for sdmTMB
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

## Create sampling offset variables
seasonal_offset <- log(data_seasonal_train$testing + 1)
monthly_offset <- log(data_monthly_train$testing + 1)

## Fit sdmTMB models 

### seasonal model 
sdmtmb_seasonal <- sdmTMB(
  wnv ~ 1 + 
        poly(prcp, 2) + 
        poly(prcp_lag1, 2) + 
        poly(tmax_lag2, 2) + 
        poly(tmin_lag1, 2) + 
        (1 | county) + (1 | ID),
  offset = seasonal_offset, 
  time = "time_step",
  extra_time = 16:19,
  mesh = sdmTMB_mesh_seasonal,
  spatial = "off",
  spatiotemporal = "ar1",
  family = nbinom1(),
  data = data_seasonal_train %>% st_drop_geometry())

sanity(sdmtmb_seasonal)

write_rds(sdmtmb_seasonal,  "data/chickens/model_predictions/sdmTMB/sdmtmb_seasonal_update.rds")

sdmtmb_seasonal_dharma <- simulate(sdmtmb_seasonal, nsim = 100, type = "mle-mvn", newdata = data_seasonal_active_train) %>%
  dharma_residuals(sdmtmb_seasonal, return_DHARMa = T)
plot(sdmtmb_seasonal_dharma)

### monthly model 
sdmtmb_monthly <- sdmTMB(
  wnv ~ 1 + 
        poly(developed, 2) + 
        poly(prcp, 2) + 
        poly(prcp_lag1, 2) + 
        poly(prcp_lag2, 2) + 
        poly(tmax, 2) + 
        poly(tmin_lag2, 2) + 
        poly(wetlands, 2) + 
        (1 | county) + (1 | ID),
  offset = monthly_offset, 
  time = "time_step",
  extra_time = 120:133,
  mesh = sdmTMB_mesh_monthly,
  spatial = "off",
  spatiotemporal = "ar1",
  family = nbinom1(),
  data = data_monthly_train %>% st_drop_geometry())

sanity(sdmtmb_monthly)

write_rds(sdmtmb_monthly,  "data/chickens/model_predictions/sdmTMB/sdmtmb_monthly_update.rds")

sdmtmb_monthly_dharma <- simulate(sdmtmb_monthly, nsim = 100, type = "mle-mvn") %>%
  dharma_residuals(sdmtmb_monthly, return_DHARMa = T)
plot(sdmtmb_monthly_dharma)

###############################################################################################################################################

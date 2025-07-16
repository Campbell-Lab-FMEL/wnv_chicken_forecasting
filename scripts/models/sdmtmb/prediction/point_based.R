
pacman::p_load(
  tidyverse
  , terra
  , sf
  , sdmTMB
  , INLA
  , lubridate
  # ,
  # ,
); conflicted::conflict_prefer_all("dplyr", quiet = T)



#############################################################################################################################################
############################################################ Loading functions ############################################################## 
#############################################################################################################################################

source("/functions/truncate.R")

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
      mutate(date = make_date(month = as.character(month), year = as.character(year))) %>%
      select(month, year, date, time_step))

data_monthly_train <- data_monthly_active %>%
  filter(!year %in% 2018:2019) 

data_monthly_pred <- data_monthly_active %>%
  filter(year %in% 2018:2019) 

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

data_seasonal_pred <- data_seasonal_active %>%
  filter(year %in% 2016:2019)

#############################################################################################################################################





###############################################################################################################################################
############################################################## Load sdmTMB models #############################################################
###############################################################################################################################################

## Monthly model
sdmtmb_monthly <- read_rds("data/chickens/model_predictions/sdmTMB/sdmtmb_monthly_update.rds")

## Seasonal model
sdmtmb_seasonal <- read_rds("data/chickens/model_predictions/sdmTMB/sdmtmb_seasonal_update.rds")

#############################################################################################################################################





###############################################################################################################################################
############################################################## Epsilon correction #############################################################
###############################################################################################################################################

### get epsilon updated estimates
sdmtmb_seasonal_sims <- simulate(
  sdmtmb_seasonal,
  newdata = data_seasonal_active %>% st_drop_geometry(),
  offset = seasonal_offset,
  type = "mle-mvn", # fixed effects at MLE values and random effect MVN draws
  mle_mvn_samples = "multiple", # take an MVN draw for each sample
  nsim = 500, # increase this for more stable results
  observation_error = FALSE # do not include observation error
)

write_rds(sdmtmb_seasonal_sims,  "data/chickens/model_predictions/sdmTMB/sdmtmb_seasonal_sims.rds")
sdmtmb_seasonal_sims <- read_rds("data/chickens/model_predictions/sdmTMB/sdmtmb_seasonal_sims.rds")

sdmtmb_monthly_sims <- simulate(
  sdmtmb_monthly,
  newdata = data_monthly_active %>% st_drop_geometry(),
  offset = monthly_offset,
  type = "mle-mvn", # fixed effects at MLE values and random effect MVN draws
  mle_mvn_samples = "multiple", # take an MVN draw for each sample
  nsim = 500, # increase this for more stable results
  observation_error = FALSE # do not include observation error
)

write_rds(sdmtmb_monthly_sims,  "data/chickens/model_predictions/sdmTMB/sdmtmb_monthly_sims.rds")
sdmtmb_monthly_sims <- read_rds("data/chickens/model_predictions/sdmTMB/sdmtmb_monthly_sims.rds")

seasonal_sims <- log(sdmtmb_seasonal_sims)
attr(seasonal_sims, "time") <- sdmtmb_seasonal$time # or "year"
attr(seasonal_sims, "link") <- "log"
row.names(seasonal_sims) <- data_seasonal_active$year

monthly_sims <- log(sdmtmb_monthly_sims)
attr(monthly_sims, "time") <- sdmtmb_monthly$time # or "year"
attr(monthly_sims, "link") <- "log"
row.names(monthly_sims) <- data_monthly_active$time_step

###############################################################################################################################################





###############################################################################################################################################
################################################ Plotting seasonal epsilon-corrected data #####################################################
###############################################################################################################################################

data_seasonal_active_agg <- data_seasonal_active %>%
  st_drop_geometry() %>%
  group_by(year, time_step) %>%
  summarize(wnv = sum(wnv)) %>%
  mutate(month = "10", 
         date = make_date(year = as.character(year), month = month))

sdmtmb_seasonal_sims_ind <- bind_rows(
  get_index_sims(seasonal_sims, level = 0.95) %>%
    mutate(ci = "95",
           upr = truncate(upr, upper = 0.95)),
  get_index_sims(seasonal_sims, level = 0.90) %>%
    mutate(ci = "90",
           upr = truncate(upr, upper = 0.95)),
  get_index_sims(seasonal_sims, level = 0.80) %>%
    mutate(ci = "80",
           upr = truncate(upr, upper = 0.95)),
  get_index_sims(seasonal_sims, level = 0.50) %>%
    mutate(ci = "50",
           upr = truncate(upr, upper = 0.95))) %>%
  mutate(year = rep(as.character(2001:2019), 4),
         month = "10",
         date = make_date(year = year, month = month))

### Code for producing Figure 2 -- seasonal panel

seasonal_temp_pred <- ggplot() +
  geom_area(data = data_seasonal_active_agg, aes(x = date, y = wnv, fill = "Empirical"), col = "grey20") +
  ggdist::geom_lineribbon(data = sdmtmb_seasonal_sims_ind,
                          aes(x = date, y = est, ymin = lwr, ymax = upr, fill = "Model prediction", alpha = ci), col = "darkcyan", linewidth = 1) +
  geom_line(data = sdmtmb_seasonal_sims_ind,
                          aes(x = date, y = est, alpha = ci), col = "darkcyan", linewidth = 1) +
  geom_vline(xintercept = ymd("2016-02-21"), col = "grey40", linetype = "dashed") +
  annotate("text", x = ymd("2015-10-27"), y = 1275, label = "Prediction horizon", angle = 90, fontface = "bold", col = "grey40") +
  scale_x_date(limits = c(min(data_seasonal_active_agg$date), max(data_seasonal_active_agg$date)),
               breaks = seq(ymd("2001-10-10"), ymd("2019-09-21"), length.out = 6),
               labels = c(2001, 2004, 2008, 2012, 2016, 2020),
               expand = expansion(add = 0)) +
  scale_y_continuous(limits = c(0, 1600),
                     breaks = c(5, seq(400, 1200, by = 400), 1595),
                     labels = as.character(seq(0, 1600, by = 400)),
                     expand = expansion(add = 0)) +
  scale_fill_manual("Estimate", values = c("Empirical" = "#F69C73FF", "Model prediction" = "darkcyan")) +
  scale_alpha_manual("Prediction CI (%)", values = c("95" = 0.15, "90" = 0.20, "80" = 0.25, "50" = 0.30)) +
  labs(y = "WNV seropositive counts", x = NULL) +
  theme_bw() +
  theme(
    legend.position = "none"
    , panel.grid.major = element_blank()
    , panel.grid.minor = element_blank()
    , panel.border = element_rect(colour = "black", fill = NA, linewidth = 1)
    , axis.line = element_blank()
    , axis.ticks = element_line(colour = "black", linewidth = 0.75)
    , axis.text = element_text(color = "black", face = "bold")
    , axis.title = element_text(size = 12, face = "bold")
    , plot.margin = margin(8, 15, 1, 8)
  ); seasonal_temp_pred

###############################################################################################################################################




###############################################################################################################################################
################################################# Plotting monthly epsilon-corrected data #####################################################
###############################################################################################################################################

data_monthly_active_agg <- data_monthly_active %>%
  st_drop_geometry() %>%
  group_by(year, month, time_step) %>%
  summarize(wnv = sum(wnv)) %>%
  mutate(date = make_date(month = as.character(month), year = as.character(year)))

sdmtmb_monthly_sims_ind <- bind_rows(
  get_index_sims(monthly_sims, level = 0.95) %>%
    mutate(ci = "95", 
           upr = truncate(upr, upper = 0.95)),
  get_index_sims(monthly_sims, level = 0.90) %>%
    mutate(ci = "90", 
           upr = truncate(upr, upper = 0.95)),
  get_index_sims(monthly_sims, level = 0.80) %>%
    mutate(ci = "80", 
           upr = truncate(upr, upper = 0.95)),
  get_index_sims(monthly_sims, level = 0.50) %>%
    mutate(ci = "50", 
           upr = truncate(upr, upper = 0.95))) %>%
  left_join(
    data.frame(
      month = rep(6:12, length(2001:2019)),
      year = rep(2001:2019, length(6:12))) %>%
      arrange(year, month) %>%
      mutate(time_step = row_number(),
             year = as.factor(year))) %>%
  mutate(date = make_date(month = month, year = as.character(year)))

### Code for producing Figure 2 -- monthly panel

monthly_temp_pred <- ggplot() +
  geom_area(data = data_monthly_active_agg, aes(x = date, y = wnv, fill = "Empirical"), col = "grey20") +
  ggdist::geom_lineribbon(data = sdmtmb_monthly_sims_ind, 
                          aes(x = date, y = est, ymin = lwr, ymax = upr, fill = "Model prediction", alpha = ci), col = "darkcyan", linewidth = 1) +
  geom_vline(xintercept = ymd("2017-12-27"), col = "grey40", linetype = "dashed") +
  annotate("text", x = ymd("2017-10-25"), y = 350, label = "Prediction horizon", angle = 90, fontface = "bold", col = "grey40") +
  scale_x_date(limits = c(ymd("2010-05-01"), max(data_monthly_active_agg$date)),
               breaks = seq(ymd("2010-05-05"), ymd("2019-11-26"), length.out = 6),
               labels = c(2010, 2012, 2014, 2016, 2018, 2020),
               expand = expansion(add = 0)) +
  scale_y_continuous(limits = c(0, 700),
                     breaks = seq(0, 700, by = 100),
                     # labels = as.character(seq(0, 500, by = 100)),
                     expand = expansion(add = 0)) +
  scale_fill_manual("Estimate", values = c("Empirical" = "#F69C73FF", "Model prediction" = "darkcyan")) +
  scale_alpha_manual("Prediction CI (%)", values = c("95" = 0.15, "90" = 0.20, "80" = 0.25, "50" = 0.30)) +
  labs(y = "WNV seropositive counts", x = "Date") +
  theme_bw() +
  theme(
    legend.position = "inside"
    , legend.position.inside = c(0.32, 0.80)
    , legend.background = element_blank()
    , legend.title = element_text(size = 10, face = "bold", color = "grey10")
    , legend.text = element_text(size = 10, color = "grey10")
    , legend.box = "horizontal"
    , panel.grid.major = element_blank()
    , panel.grid.minor = element_blank()
    , panel.border = element_rect(colour = "black", fill = NA, linewidth = 1)
    , axis.line = element_blank()
    , axis.ticks = element_line(colour = "black", linewidth = 0.75)
    , axis.text = element_text(color = "black", face = "bold")
    , axis.title = element_text(size = 12, face = "bold")
    , plot.margin = margin(8, 15, 1, 8)
  ); monthly_temp_pred

ggpubr::annotate_figure(
  ggpubr::ggarrange(seasonal_temp_pred, monthly_temp_pred, ncol = 1, align = "hv"), 
  right = grid::textGrob("Monthly                                                                          Seasonal",
                         hjust = 0.5,
                         vjust = 1,
                         rot = -90,
                         gp = grid::gpar(cex = 1, fontface = "bold"))); 
ggsave("figures/Alex/wnv/sdmtmb/temporal_predictions.jpeg", width = 6, height = 8, dpi = 600)

###############################################################################################################################################



pacman::p_load(
  tidyverse
  , terra
  , sf
  , sdmTMB
  , INLA
  , ggpubr
  # ,
  # ,
)

setwd("/blue/guralnick/jbaecher/chickens")

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
      select(month, year, time_step))

data_monthly_active_train <- data_monthly_active %>%
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

data_seasonal_active_train <- data_seasonal_active %>%
  filter(!year %in% 2016:2019)

#############################################################################################################################################

sdmtmb_seasonal <- read_rds("data/chickens/model_predictions/sdmTMB/sdmtmb_seasonal_update.rds")

sdmtmb_monthly <- read_rds("data/chickens/model_predictions/sdmTMB/sdmtmb_monthly_update.rds")

#############################################################################################################################################

n <- 10

## Monthly:
# prcp_lag2
sdmtmb_monthly_newdata_prcp_lag2 <- sdmtmb_monthly %>%
  predict(se_fit = TRUE, re_form = NA, re_form_iid = NA,
          newdata = data.frame(
            prcp =      rep(mean(data_monthly_active$prcp), n),
            prcp_lag1 = rep(mean(data_monthly_active$prcp_lag1), n),
            prcp_lag2 = seq(min(data_monthly_active$prcp_lag2), 
                            max(data_monthly_active$prcp_lag2), length.out = n),
            tmin_lag2 = rep(mean(data_monthly_active$tmin_lag2), n),
            tmax =      rep(mean(data_monthly_active$tmax), n),
            developed = rep(mean(data_monthly_active$developed), n),
            wetlands =  rep(mean(data_monthly_active$wetlands), n),
            testing =   rep(mean(data_monthly_active$testing), n),
            county = NA, ID = NA, time_step = 19)
          )

# tmax
sdmtmb_monthly_newdata_tmax <- sdmtmb_monthly %>%
  predict(se_fit = TRUE, re_form = NA, re_form_iid = NA,
          newdata = data.frame(
            prcp =      rep(mean(data_monthly_active$prcp), n),
            prcp_lag1 = rep(mean(data_monthly_active$prcp_lag1), n),
            prcp_lag2 = rep(mean(data_monthly_active$prcp_lag2), n),
            tmin_lag2 = rep(mean(data_monthly_active$tmin_lag2), n),
            tmax =      seq(min(data_monthly_active$tmax),
                            max(data_monthly_active$tmax), length.out = n),
            developed = rep(mean(data_monthly_active$developed), n),
            wetlands =  rep(mean(data_monthly_active$wetlands), n),
            testing =   rep(mean(data_monthly_active$testing), n),
            county = NA, ID = NA, time_step = 19)
  )

# tmin_lag2
sdmtmb_monthly_newdata_tmin_lag2 <- sdmtmb_monthly %>%
  predict(se_fit = TRUE, re_form = NA, re_form_iid = NA,
          newdata = data.frame(
            prcp =      rep(mean(data_monthly_active$prcp), n),
            prcp_lag1 = rep(mean(data_monthly_active$prcp_lag1), n),
            prcp_lag2 = rep(mean(data_monthly_active$prcp_lag2), n),
            tmin_lag2 = seq(min(data_monthly_active$tmin_lag2),
                            max(data_monthly_active$tmin_lag2), length.out = n),
            tmax =      rep(mean(data_monthly_active$tmax), n),
            developed = rep(mean(data_monthly_active$developed), n),
            wetlands =  rep(mean(data_monthly_active$wetlands), n),
            testing =   rep(mean(data_monthly_active$testing), n),
            county = NA, ID = NA, time_step = 19)
  )

## Seasonal:
# prcp_lag1
sdmtmb_seasonal_newdata_prcp_lag1 <- sdmtmb_seasonal %>%
  predict(se_fit = TRUE, re_form = NA, re_form_iid = NA,
          newdata = data.frame(
            prcp = rep(mean(data_seasonal_active$prcp), n),
            prcp_lag1 = seq(min(data_seasonal_active$prcp_lag1),
                            max(data_seasonal_active$prcp_lag1), length.out = n),
            tmax_lag2 = rep(mean(data_seasonal_active$tmax_lag2), n),
            tmin_lag1 = rep(mean(data_seasonal_active$tmin_lag1), n),
            testing = rep(mean(data_seasonal_active$testing), n),
            county = NA, ID = NA, time_step = 19))


sdmtmb_plot_preds <- list(
  sdmtmb_monthly_newdata_prcp_lag2,
  sdmtmb_monthly_newdata_tmax,
  sdmtmb_monthly_newdata_tmin_lag2,
  sdmtmb_seasonal_newdata_prcp_lag1)

write_rds(sdmtmb_plot_preds, "data/chickens/model_predictions/sdmTMB/sdmtmb_plot_preds.rds")

sdmtmb_preds_plots <- ggarrange(
  
  ggplot(sdmtmb_plot_preds[[1]], aes(x = prcp_lag2)) +
    geom_ribbon(aes(ymin = exp(est - est_se), ymax = exp(est + est_se)), fill = "darkcyan", alpha = 0.4) +
    geom_smooth(aes(y = exp(est)), col = "grey20", se = F, linewidth = 0.5) +
    scale_x_continuous(limits = c(-1.7, 7), expand = expansion(add = 0)) +
    scale_y_continuous(limits = c(0, 0.007), expand = expansion(add = 0)) +
    labs(x = parse(text = "Cumulative~Precipitation^2~~(2~mo.~lag)"), y = NULL) +
    theme_bw(),
  
  ggplot(sdmtmb_plot_preds[[2]], aes(x = tmax)) +
    geom_ribbon(aes(ymin = exp(est - est_se), ymax = exp(est + est_se)), fill = "darkcyan", alpha = 0.4) +
    geom_smooth(aes(y = exp(est)), col = "grey20", se = F, linewidth = 0.5) +
    scale_x_continuous(limits = c(-3.3, 2), expand = expansion(add = 0)) +
    scale_y_continuous(limits = c(0, 0.0085), expand = expansion(add = 0)) +
    labs(x = parse(text = "Maximum~Temperature^2"), y = NULL) +
    theme_bw(),
  
  ggplot(sdmtmb_plot_preds[[3]], aes(x = tmin_lag2)) +
    geom_ribbon(aes(ymin = exp(est - est_se), ymax = exp(est + est_se)), fill = "darkcyan", alpha = 0.4) +
    geom_smooth(aes(y = exp(est)), col = "grey20", se = F, linewidth = 0.5) +
    scale_x_continuous(limits = c(-1.5, 1.8), expand = expansion(add = 0)) +
    scale_y_continuous(limits = c(0, 0.025), expand = expansion(add = 0)) +
    labs(x = parse(text = "Minimum~Temperature^2~~(2~mo.~lag)"), y = NULL) +
    theme_bw(),
  
  ggplot(sdmtmb_plot_preds[[4]], aes(x = prcp_lag1)) +
    geom_ribbon(aes(ymin = exp(est - est_se), ymax = exp(est + est_se)), fill = "darkcyan", alpha = 0.4) +
    geom_smooth(aes(y = exp(est)), col = "grey20", se = F, linewidth = 0.5) +
    scale_x_continuous(limits = c(-2.65, 4), expand = expansion(add = 0)) +
    scale_y_continuous(limits = c(0, 0.025), expand = expansion(add = 0)) +
    labs(x = parse(text = "Cumulative~Precipitation^2~~(6~mo.~lag)"), y = NULL) +
    theme_bw(),
  
  align = "h",
  nrow = 4) +             #  T,    R,    B,     L
  theme(plot.margin = margin(0.0, 0.25, 0.00, 0.05, "cm")); sdmtmb_preds_plots

sdmtmb_preds_plots_ann <- annotate_figure(
  sdmtmb_preds_plots, 
  left = grid::textGrob("WNV seropositive counts",
                        rot = 90,
                        hjust = 0.5,
                        vjust = 0.5,
                        gp = grid::gpar(cex = 1, fontface = "bold")),
  right = grid::textGrob("Monthly                                        Monthly                                    Monthly                                           Seasonal    ",
                         hjust = 0.5,
                         vjust = 1.5,
                         rot = -90,
                         gp = grid::gpar(cex = 1, fontface = "bold"))
); ggsave("figures/Alex/wnv/sdmtmb/sdmtmb_preds_plots_ann.jpeg", 
       width = 3.5, height = 10, dpi = 600)

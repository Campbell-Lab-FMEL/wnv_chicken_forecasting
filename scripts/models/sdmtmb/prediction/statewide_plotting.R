
pacman::p_load(
  tidyverse
  , terra
  , sf
  , sdmTMB
  , INLA
  , wesanderson
  , ggpubr
  , cowplot
  # ,
  # ,
)

setwd("C:/Users/jbaecher/UFL Dropbox/Joseph Baecher/UF/postdoc/chickens")

###########################################################################################################

truncate <- function(obj, lower, upper){
  if(missing(upper)){
    obj[obj > stats::quantile(obj, lower, na.rm = T)] <- stats::quantile(obj, lower, na.rm = T)
  }
  if(missing(lower)){
    obj[obj > stats::quantile(obj, upper, na.rm = T)] <- stats::quantile(obj, upper, na.rm = T)
  } 
  else(
    obj[obj > stats::quantile(obj, c(lower, upper, na.rm = T))] <- stats::quantile(obj, c(lower, upper, na.rm = T))
  )
  return(obj)
}

###########################################################################################################

env_seasonal_df <- read_rds("data/environment/env_covs_seasonal.rds")

env_seasonal_df_preds <- bind_cols(
  env_seasonal_df, 
  data.frame(
    testing = rep(0, nrow(env_seasonal_df)), 
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
      select(year, time_step)); rm(env_seasonal_df)

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
      select(month, year, time_step)); rm(env_monthly_df)

###########################################################################################################

pal <- wes_palette("Zissou1", 256, type = "continuous")

fl <- read_rds("data/fl_polygon_crop.rds")

seasonal_statewide_raw <- read_rds("data/chickens/model_predictions/sdmTMB/sdmtmb_seasonal_state_preds_rf.rds") 

seasonal_statewide <- env_seasonal_df_preds %>%
  select(X, Y, year) %>%
  bind_cols(
    data.frame(
      mean = rowMeans(seasonal_statewide_raw), 
      se = apply(seasonal_statewide_raw, 1, FUN = function(x){sd(x)/sqrt(100)}))) %>%
  mutate(mean = exp(mean),
         mean_0999 = truncate(mean, upper = 0.999),
         mean_099 = truncate(mean, upper = 0.99),
         mean_098 = truncate(mean, upper = 0.98),
         mean_095 = truncate(mean, upper = 0.95),
         mean_090 = truncate(mean, upper = 0.90),

         se = exp(se),
         se_0999 = truncate(se, upper = 0.999),
         se_099 = truncate(se, upper = 0.99),
         se_098 = truncate(se, upper = 0.98),
         se_095 = truncate(se, upper = 0.95)); rm(seasonal_statewide_raw)

seasonal_statewide_summ <- seasonal_statewide %>% 
  group_by(X, Y) %>%
  summarize(mean = mean(mean), 
            sum = sum(mean),
            se = mean(se))

monthly_statewide_raw <- read_rds("data/chickens/model_predictions/sdmTMB/sdmtmb_monthly_state_preds_rf.rds")

monthly_statewide <- env_monthly_df_preds %>%
  select(X, Y, year, month) %>%
  bind_cols(
    data.frame(
      mean = rowMeans(monthly_statewide_raw), 
      se = apply(monthly_statewide_raw, 1, FUN = function(x){sd(x)/sqrt(100)}))) %>%
  mutate(mean = exp(mean),
         mean_0999 = truncate(mean, upper = 0.999),
         mean_099 = truncate(mean, upper = 0.99),
         mean_098 = truncate(mean, upper = 0.98),
         mean_095 = truncate(mean, upper = 0.95),
         mean_090 = truncate(mean, upper = 0.90),
         
         se = exp(se),
         se_0999 = truncate(se, upper = 0.999),
         se_099 = truncate(se, upper = 0.99),
         se_098 = truncate(se, upper = 0.98),
         se_095 = truncate(se, upper = 0.95)); rm(monthly_statewide_raw)

monthly_statewide <- monthly_statewide %>%
left_join(
  data.frame(
    month = as.character(6:12),
    name = c("June", "July", "August", "September", "October", "November", "December")
  )
)
monthly_statewide$names <- factor(monthly_statewide$name, levels = c(c("June", "July", "August", "September", "October", "November", "December")))

monthly_statewide_summ <- monthly_statewide %>% 
  group_by(X, Y) %>%
  summarize(
    mean = mean(mean), 
    sum = sum(mean),
    se = mean(se),
    
    mean_0999 = mean(mean_0999),
    mean_099 = mean(mean_099),
    mean_098 = mean(mean_098),
    mean_095 = mean(mean_095),
    mean_090 = mean(mean_090),
    
    se_0999 = mean(se_0999),
    se_099 = mean(se_099),
    se_098 = mean(se_098),
    se_095 = mean(se_095)) %>%
   mutate(
     sum_0999 = truncate(sum, upper = 0.999),
     sum_099 = truncate(sum, upper = 0.99),
     sum_098 = truncate(sum, upper = 0.98),
     sum_095 = truncate(sum, upper = 0.95),
     sum_090 = truncate(sum, upper = 0.90),
   )

###########################################################################################################

base <- ggplot() +
  theme_void() +
  theme(
    legend.position = "inside"
    , legend.position.inside = c(0.3, 0.45)
    , legend.key.height = unit(0.55, "cm")
    , legend.key.width = unit(0.4, "cm")
    , legend.ticks = element_blank()
    , legend.frame = element_rect(colour = "black", linewidth = 0.3)
    , legend.text = element_text(size = 7, colour = "black", face = "bold")
  )

seasonal_uncertainty <- base +
  geom_raster(data = seasonal_statewide_summ, aes(x = X, y = Y, fill = se)) +
  geom_sf(data = fl, fill = NA, col = "black") +
  scale_fill_gradientn(NULL, 
                       colors = pal, 
                       limits = c(min(seasonal_statewide_summ$se), max(seasonal_statewide_summ$se)),
                       breaks = seq(min(seasonal_statewide_summ$se), max(seasonal_statewide_summ$se), length.out = 5),
                       labels = seq(min(seasonal_statewide_summ$se), max(seasonal_statewide_summ$se), length.out = 5) %>% 
                         round(2) %>% as.character(),
                       na.value = NA) +
  theme(); seasonal_uncertainty

seasonal_mean <- base +
  geom_raster(data = seasonal_statewide_summ, aes(x = X, y = Y, fill = mean)) +
  geom_sf(data = fl, fill = NA, col = "black") +
  scale_fill_gradientn(NULL, 
                       colors = pal, 
                       limits = c(0, max(seasonal_statewide_summ$mean)),
                       breaks = seq(0, max(seasonal_statewide_summ$mean), length.out = 5),
                       labels = seq(0, max(seasonal_statewide_summ$mean), length.out = 5) %>% round(2) %>% as.character(),
                       na.value = NA); seasonal_mean
  
###########################################################################################################

monthly_uncertainty <- base +
  geom_raster(data = monthly_statewide_summ, aes(x = X, y = Y, fill = se)) +
  geom_sf(data = fl, fill = NA, col = "black") +
  scale_fill_gradientn(NULL, 
                       colors = pal, 
                       limits = c(min(monthly_statewide_summ$se), max(monthly_statewide_summ$se)),
                       breaks = seq(min(monthly_statewide_summ$se), max(monthly_statewide_summ$se), length.out = 5),
                       labels = seq(min(monthly_statewide_summ$se), max(monthly_statewide_summ$se), length.out = 5) %>% round(2),
                       na.value = NA) +
  theme(); monthly_uncertainty

monthly_mean <- base +
  geom_raster(data = monthly_statewide_summ, aes(x = X, y = Y, fill = mean_090)) +
  geom_sf(data = fl, fill = NA, col = "black") +
  scale_fill_gradientn(NULL,
                       colors = pal,
                       limits = c(0, max(monthly_statewide_summ$mean_090)),
                       breaks = seq(0, max(monthly_statewide_summ$mean_090), length.out = 5),
                       labels = seq(0, max(monthly_statewide_summ$mean_090), length.out = 5) %>% round(2),
                       na.value = NA); monthly_mean

ggpubr::annotate_figure(
  ggpubr::ggarrange(
    monthly_mean, 
    monthly_uncertainty, 
    seasonal_mean, 
    seasonal_uncertainty, 
    nrow = 2, ncol = 2, align = "hv"),
  right = grid::textGrob("Monthly                                                     Seasonal",
                         hjust = 0.5,
                         vjust = 1,
                         rot = -90,
                         gp = grid::gpar(cex = 1, fontface = "bold")),
  top = grid::textGrob("Prediction mean                               Uncertainty",
                       hjust = 0.5,
                       vjust = 1,
                       gp = grid::gpar(cex = 1, fontface = "bold")))
ggsave("figures/Alex/wnv/sdmtmb/statewide_predictions/agg_preds_fl.jpeg", width = 6, height = 6, dpi = 600)

###########################################################################################################

seasonal_panel <- ggplot() +
  geom_raster(data = seasonal_statewide, aes(x = X, y = Y, fill = mean_0999)) +
  geom_sf(data = fl, fill = NA, col = "black") +
  scale_fill_gradientn("WNV \n seropositive \n counts" , 
                       colors = pal, 
                       limits = c(0, max(seasonal_statewide$mean_0999)),
                       breaks = seq(0, max(seasonal_statewide$mean_0999), length.out = 3),
                       # labels = seq(0, max(seasonal_statewide$mean_0999), length.out = 5) %>% round(2),
                       labels = c("Low", "Mid", "High"),
                       na.value = NA) +
  facet_wrap(~year, nrow = 5, ncol = 4) +
  theme_void() +
  theme(
    , legend.position = "inside"
    , legend.position.inside = c(0.87, 0.09)
    , legend.title.position = "left"
    , legend.title = element_text(size = 10, face = "bold", hjust = 0.5, vjust = 0.5)
    , legend.key.height = unit(0.45, "cm")
    , legend.key.width = unit(0.4, "cm")
    , legend.ticks = element_blank()
    , legend.frame = element_rect(colour = "black", linewidth = 0.3)
    , legend.text = element_text(size = 7, colour = "black", face = "bold")
    , plot.title = element_text(face = "bold")
    , strip.text = element_text(face = "bold", size = 10)
  ); seasonal_panel
# ggsave("figures/Alex/wnv/sdmtmb/statewide_predictions/annual_preds_rf.jpg", width = 6.5, height = 8, dpi = 600)

monthly_panel <- ggplot() +
  geom_raster(data = monthly_statewide %>%
                filter(year %in% as.character(2014:2015)), aes(x = X, y = Y, fill = mean_098)) +
  geom_sf(data = fl, fill = NA, col = "black") +
  scale_fill_gradientn("Monthly \n WNV \n seropositive \n counts" , 
                       colors = pal, na.value = NA) +
  facet_grid(rows = vars(year), cols = vars(names), switch="both") +
  theme_void() +
  theme(
    legend.position = "none"
    , plot.title = element_text(face = "bold")
    , strip.text = element_text(face = "bold", size = 10)
    , strip.text.y.left = element_text(angle = 90)
  ); monthly_panel

seasonal_monthly_panel <- ggarrange(
  seasonal_panel, 
  monthly_panel, 
  nrow = 2, 
  heights = c(1, 0.25)
); seasonal_monthly_panel
ggsave("figures/Alex/wnv/sdmtmb/statewide_predictions/annual_monthly_preds_rf.jpg", width = 6.5, height = 10, dpi = 600)

monthly_statewide_unc <- monthly_statewide %>%
  as.data.frame() 
  
for(i in 1:19){
  year <- paste0("Year: ", seasonal_statewide %>%
                   filter(time_step == i) %>%
                   pull(year) %>%
                   unique())

  ggplot() +
    geom_sf(data = fl, fill = "grey40", col = "black") +
    geom_raster(data = seasonal_statewide %>%
                  filter(time_step == i), aes(x = X, y = Y, fill = est_0999)) +
    geom_sf(data = fl, fill = NA, col = "black") +
    ggtitle(paste(year)) +
    scale_fill_gradientn("WNV \n seropositive \n counts" ,
                         colors = pal,
                         limits = seasonal_statewide$est_0999 %>% range() %>% round(6),
                         breaks = seq(seasonal_statewide$est_0999 %>% min(),
                                      seasonal_statewide$est_0999 %>% max(),
                                      length.out = 5) %>% round(6),
                         labels = c("0.0", as.character(seq(seasonal_statewide$est_0999 %>% min(),
                                                            seasonal_statewide$est_0999 %>% max(),
                                                            length.out = 5) %>% round(2))[2:5]),
                         na.value = NA) +
    theme_void() +
    theme(
      , legend.position = "inside"
      , legend.position.inside = c(0.3, 0.3)
      , legend.title.position = "left"
      , legend.title = element_text(size = 15, face = "bold", hjust = 0.5, vjust = 0.5)
      , legend.key.height = unit(1, "cm")
      , legend.key.width = unit(0.5, "cm")
      , legend.ticks = element_blank()
      , legend.frame = element_rect(colour = "black", linewidth = 0.3)
      , legend.text = element_text(size = 10, colour = "black", face = "bold")
      , plot.title = element_text(face = "bold", hjust = 0.5, size = 20)
    ); ggsave(paste0("figures/Alex/wnv/animate/seasonal/time_step_", i, ".jpg"), width = 7, height = 7, dpi = 600)
}

#######################################################################################################################################

data_monthly <- read_rds("data/chickens/monthly/wnv_eeev_env_covs.rds") %>%
  st_drop_geometry() %>%
  group_by(year, month) %>%
  summarize(wnv = sum(wnv, na.rm = T)) %>%
  mutate(date = make_date(year, month))

main_plot <- ggplot() +
  geom_sf(data = fl, fill = "grey40", col = "black") +
  geom_raster(data = seasonal_statewide %>%
                filter(time_step == 1), aes(x = X, y = Y, fill = est_0999)) +
  geom_sf(data = fl, fill = NA, col = "black") +
  ggtitle(paste(year)) +
  scale_fill_gradientn("WNV \n seropositive \n counts" ,
                       colors = pal,
                       limits = seasonal_statewide$est_0999 %>% range() %>% round(6),
                       breaks = seq(seasonal_statewide$est_0999 %>% min(),
                                    seasonal_statewide$est_0999 %>% max(),
                                    length.out = 5) %>% round(6),
                       labels = c("0.0", as.character(seq(seasonal_statewide$est_0999 %>% min(),
                                                          seasonal_statewide$est_0999 %>% max(),
                                                          length.out = 5) %>% round(2))[2:5]),
                       na.value = NA) +
  theme_void() +
  theme(
    , legend.position = "inside"
    , legend.position.inside = c(0.93, 0.8)
    , legend.title.position = "left"
    , legend.title = element_text(size = 10, face = "bold", hjust = 0.5, vjust = 0.5)
    , legend.key.height = unit(0.75, "cm")
    , legend.key.width = unit(0.4, "cm")
    , legend.ticks = element_blank()
    , legend.frame = element_rect(colour = "black", linewidth = 0.3)
    , legend.text = element_text(size = 8, colour = "black", face = "bold")
    , plot.title = element_text(face = "bold", hjust = 0.5, size = 20)
  ); main_plot

inset <- ggplot(data_monthly) +
  geom_area(aes(x = date, y = wnv), fill = "#F69C73FF") +
  geom_line(aes(x = date, y = wnv)) +
  scale_x_date(limits = c(ymd("2000-12-01"), ymd("2020-01-01")), 
               breaks = c(ymd("2000-12-25"), ymd("2005-03-01"), ymd("2010-02-01"), ymd("2015-02-01"), ymd("2019-12-15")),
               labels = c("2000", "2005", "2010", "2015", "2020"),
               expand = expansion(add = 0.01)) +
  scale_y_continuous(limits = c(0, 325), expand = expansion(add = 1)) +
  labs(y = "WNV seropositive counts", x = "Date") +
  theme_classic() +
  theme(
    panel.grid.major = element_line(colour = "grey80")
    , panel.grid.minor = element_line(colour = "grey90")
    , panel.border = element_rect(colour = "black", fill = NA, linewidth = 1)
    , axis.line = element_blank()
    , axis.ticks.y = element_line(colour = "black", linewidth = 0.75)
    , axis.text = element_text(color = "black", face = "bold")
    , axis.title = element_text(size = 15, face = "bold")
    , plot.margin = margin(8, 12, 1, 8))

ggdraw() +
  draw_plot(main_plot) +
  draw_plot(inset, x = 0.05, y = 0.05, width = .5, height = .5)


for(i in 97:133){
  year_month <- paste0("Year: ", monthly_statewide %>%
                         filter(time_step == i) %>%
                         pull(year) %>%
                         unique(),
                       "       Month: ",
                       monthly_statewide %>%
                         filter(time_step == i) %>%
                         pull(month) %>%
                         unique())
  
  date <- make_date(month = monthly_statewide %>%
                      filter(time_step == i) %>%
                      pull(month) %>%
                      unique(),
                    year = monthly_statewide %>%
                      filter(time_step == i) %>%
                      pull(year) %>%
                      unique())
  
  main <- ggplot() +
    geom_sf(data = fl, fill = "grey40", col = "black") +
    geom_raster(data = monthly_statewide %>%
                  filter(time_step == i), aes(x = X, y = Y, fill = est_098)) +
    geom_sf(data = fl, fill = NA, col = "black") +
    ggtitle(paste(year_month)) +
    scale_fill_gradientn("" ,
                         colors = pal,
                         limits = monthly_statewide$est_098 %>% range() %>% round(6),
                         breaks = seq(monthly_statewide$est_098 %>% min(),
                                      monthly_statewide$est_098 %>% max(),
                                      length.out = 5) %>% round(6),
                         labels = c("0.0", as.character(seq(monthly_statewide$est_098 %>% min(),
                                                            monthly_statewide$est_098 %>% max(),
                                                            length.out = 5) %>% round(2))[2:5]),
                         na.value = NA) +
    theme_void() +
    theme(
      , legend.position = "inside"
      , legend.position.inside = c(0.98, 0.75)
      , legend.title.position = "left"
      , legend.title = element_text(size = 10, face = "bold", hjust = 0.5, vjust = 0.5)
      , legend.key.height = unit(1, "cm")
      , legend.key.width = unit(0.5, "cm")
      , legend.ticks = element_blank()
      , legend.frame = element_rect(colour = "black", linewidth = 0.3)
      , legend.text = element_text(size = 8, colour = "black", face = "bold")
      , plot.title = element_text(face = "bold", hjust = 0.5, size = 20)
    )
  
  wnv <- data_monthly$wnv[data_monthly$date == date]
  
  inset <- ggplot(data_monthly) +
    geom_area(aes(x = date, y = wnv), fill = "#F69C73FF") +
    # geom_line(aes(x = date, y = wnv)) +
    geom_point(x = date, y = wnv, shape = 21, col = "black", fill = NA, size = 2) +
    geom_segment(x = date, y = wnv, xend = date, yend = 0) +
    scale_x_date(limits = c(ymd("2000-12-01"), ymd("2020-01-01")), 
                 breaks = c(ymd("2000-12-25"), ymd("2005-03-01"), ymd("2010-02-01"), ymd("2015-02-01"), ymd("2019-12-15")),
                 labels = c("2000", "2005", "2010", "2015", "2020"),
                 expand = expansion(add = 0.01)) +
    scale_y_continuous(limits = c(0, 325), expand = expansion(add = 1)) +
    labs(y = "WNV seropositive counts", x = "Date") +
    theme_classic() +
    theme(
      panel.grid.major = element_line(colour = "grey80")
      , panel.grid.minor = element_line(colour = "grey90")
      , panel.border = element_rect(colour = "black", fill = NA, linewidth = 1)
      , axis.line = element_blank()
      , axis.ticks.y = element_line(colour = "black", linewidth = 0.75)
      , axis.text = element_text(color = "black", face = "bold")
      , axis.title = element_text(size = 15, face = "bold")
      , plot.margin = margin(8, 12, 1, 8))
  
  ggdraw() +
    draw_plot(main) +
    draw_plot(inset, x = 0.05, y = 0.05, width = .5, height = .5)
  
  ; ggsave(paste0("figures/Alex/wnv/animate/monthly/time_step_", i, ".jpg"), width = 8, height = 7, dpi = 200)
}

##########################################################

ggplot() +
  geom_raster(data = seasonal_statewide, aes(x = X, y = Y, fill = epsilon_st)) +
  geom_sf(data = fl, fill = NA, col = "black") +
  scale_fill_gradientn("Spatiotemporal \n random \n fields" , 
                       colors = pal, 
                       limits = c(min(seasonal_statewide$epsilon_st), max(seasonal_statewide$epsilon_st)),
                       breaks = seq(min(seasonal_statewide$epsilon_st), max(seasonal_statewide$epsilon_st), length.out = 5),
                       labels = seq(min(seasonal_statewide$epsilon_st), max(seasonal_statewide$epsilon_st), length.out = 5) %>%
                         round(2),
                       na.value = NA) +
  facet_wrap(~year, nrow = 5, ncol = 4) +
  theme_void() +
  theme(
    , legend.position = "inside"
    , legend.position.inside = c(0.88, 0.09)
    , legend.title.position = "left"
    , legend.title = element_text(size = 10, face = "bold", hjust = 0.5, vjust = 0.5)
    , legend.key.height = unit(0.6, "cm")
    , legend.key.width = unit(0.4, "cm")
    , legend.ticks = element_blank()
    , legend.frame = element_rect(colour = "black", linewidth = 0.3)
    , legend.text = element_text(size = 7, colour = "black", face = "bold")
    , plot.title = element_text(face = "bold")  
    )
ggsave("figures/Alex/wnv/sdmtmb/statewide_predictions/randomfields.jpg", width = 7, height = 8, dpi = 600)

seasonal_statewide_mean <- seasonal_statewide %>%
  group_by(X, Y) %>%
  summarize(est = mean(est),
            epsilon_st = mean(epsilon_st))
  
ggplot() +
  geom_raster(data = seasonal_statewide_mean, aes(x = X, y = Y, fill = epsilon_st)) +
  geom_sf(data = fl, fill = NA, col = "black") +
  scale_fill_gradientn("Spatiotemporal \n random \n fields" , 
                       colors = pal, 
                       limits = c(min(seasonal_statewide_mean$epsilon_st), max(seasonal_statewide_mean$epsilon_st)),
                       breaks = seq(min(seasonal_statewide_mean$epsilon_st), max(seasonal_statewide_mean$epsilon_st), length.out = 5),
                       labels = seq(min(seasonal_statewide_mean$epsilon_st), max(seasonal_statewide_mean$epsilon_st), length.out = 5) %>%
                         round(2),
                       na.value = NA) +
  theme_void() +
  theme(
    , legend.position = "inside"
    , legend.position.inside = c(0.4, 0.5)
    , legend.title.position = "left"
    , legend.title = element_text(size = 10, face = "bold", hjust = 0.5, vjust = 0.5)
    , legend.key.height = unit(0.6, "cm")
    , legend.key.width = unit(0.4, "cm")
    , legend.ticks = element_blank()
    , legend.frame = element_rect(colour = "black", linewidth = 0.3)
    , legend.text = element_text(size = 7, colour = "black", face = "bold")
    , plot.title = element_text(face = "bold")  
  )
ggsave("figures/Alex/wnv/sdmtmb/statewide_predictions/randomfields_staticmean.jpg", width = 7, height = 8, dpi = 600)


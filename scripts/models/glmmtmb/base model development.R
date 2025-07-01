
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
################################################### Loading custom functions ################################################################ 
#############################################################################################################################################

source("functions/compare_models.R")
source("functions/extract_model_data.R")
source("functions/buildmermod_to_glmmtmb.R")
source("functions/stepwise_vif.R")

### Determine correct distributions for monthly and seasonal data

## Monthly:

# distr_monthly_models <- compare_models(
#   formulas = "wnv ~ poly(prcp_lag1, 2) + developed + year + (1 | ID)",
#   ziformulas = c("~0", "~0", "~0", "~1", "~1", "~1", "~.", "~.", "~."),
#   families = c("poisson", "nbinom1", "nbinom2",
#                "poisson", "nbinom1", "nbinom2",
#                "truncated_poisson", "truncated_nbinom1", "truncated_nbinom2"),
#   data = data_monthly_train)
# 
# distr_monthly_models$aictab
# 
# plot_model(distr_monthly_models$models[["zinbinom1"]], type = "pred", terms = "prcp_lag1 [-4:8, by=0.25]", se = T) +
#   labs(x = "Cumulative Precip (seasonal lag)", y = "Predicted WNV conversion prevalence") +
#   ggtitle("") +
#   theme(plot.title = element_blank()) +
#   theme_bw()
# 
# distr_seasonal_models <- compare_models(
#   formulas = "wnv ~ poly(prcp_lag1, 2) + developed + year + (1 | ID)",
#   ziformulas = c("~0", "~0", "~0", "~1", "~1", "~1", "~.", "~.", "~."),
#   families = c("poisson", "nbinom1", "nbinom2",
#                "poisson", "nbinom1", "nbinom2",
#                "truncated_poisson", "truncated_nbinom1", "truncated_nbinom2"),
#   data = data_seasonal_train)
# 
# distr_seasonal_models$aictab$Modnames_fix <- c("truncated_nbinom1", "truncated_nbinom2",
#                                                       "zinf-nbinom1", "nbinom1",
#                                                       "truncated_poisson", "zinf-nbinom2", "nbinom2",
#                                                       "zinf-poisson", "poisson")
# 
# distr_seasonal_models$aictab$Modnames_fix
# 
# names(distr_seasonal_models$models) <- c("poisson", "nbinom1", "nbinom2",
#                                                 "zinf-poisson", "zinf-nbinom1", "zinf-nbinom2",
#                                                 "truncated_poisson", "truncated_nbinom1", "truncated_nbinom2")
# 
# ### Create basic model formulas
# 
# ## Multi-tier modeling system to determine variables of primary importance:
# 
# # Test of random effects: 
# 
# re_formulas <- c(
#   "wnv ~ year"
#   , "wnv ~ year + (1 | county)"
#   , "wnv ~ year + (1 | ID)"
#   , "wnv ~ year + (1 | county / ID)"
#   # temporal structures didn't run properly... not sure why or what changed...
#   , "wnv ~ time_step + (time_step | county)"
#   , "wnv ~ time_step + (time_step | ID)"
#   , "wnv ~ time_step + (1 | county) + (time_step | county)"
#   , "wnv ~ time_step + (1 | ID ) + (time_step | ID)"
#   , "wnv ~ time_step + ar1(as.factor(time_step) + 0 | county)"
#   , "wnv ~ time_step + ar1(as.factor(time_step) + 0 | ID)"
# )
# 
# ### Monthly
# 
# re_monthly_models <- compare_models(
#   formulas = re_formulas,
#   ziformulas = "~1",
#   families = "nbinom1",
#   data = data_monthly_train)
# 
# re_monthly_models$aictab
# 
# top_re_monthly <- re_monthly_models[[paste0(re_monthly_models$aictab$Modnames[1])]]
# 
# dharma_re_monthly <- top_re_monthly_active %>%
#   simulateResiduals(); plot(dharma_re_monthly_active)
# 
# 
# ### Seasonal
# 
# re_seasonal_models <- compare_models(
#   formulas = re_formulas,
#   ziformulas = "~1",
#   families = "nbinom1",
#   data = data_seasonal_train)
# 
# re_seasonal_models$aictab
# 
# top_re_seasonal <- re_seasonal_models[[paste0(re_seasonal_models$aictab$Modnames[1])]]
# 
# dharma_re_seasonal <- top_re_seasonal %>%
#   simulateResiduals(); plot(dharma_re_seasonal)

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

#####

#### Tier 1 models: 

### Monthly

t1_monthly_models <- compare_models(
  formulas = paste0(t1_formulas, " + year + testing + (1 | county / ID)"),
  ziformulas = "~0",
  families = "nbinom1",
  data = data_monthly_train)

# t1_monthly_models$aictab 

# summarizing t1 monthly models

t1_monthly_vars <- bind_rows(
  bind_cols(extract_model_data(t1_monthly_models$models), season = "active")) %>%
  # filter(!zero_span == T) %>%
  filter(!str_detect(term, fixed("2)2")) == T) %>%
  filter(!str_detect(term, "year") == T) %>%
  filter(!term == "testing") %>%
  select(season, term, estimate, std.error, conf.low, conf.high, zero_span, sig) %>%
  as.data.frame()

# ggplot(t1_monthly_vars) +
#   aes(x = reorder(term, estimate), y = estimate, group = season) +
#   geom_linerange(aes(ymin = estimate - std.error, ymax = estimate + std.error, col = season), 
#                  position = position_dodge(width = 0.5), alpha = 0.5, linewidth = 1.5) +
#   geom_pointrange(aes(ymin = conf.low, ymax = conf.high, col = season), 
#                   position = position_dodge(width = 0.5), size = 0.5) +
#   coord_flip() +
#   geom_abline(slope = 0, linetype = "dashed", col = "grey40") +
#   labs(x = "Term", y = expression(beta)) +
#   theme_bw()

#### Seasonal

t1_seasonal_models <- compare_models(
  formulas = paste0(t1_formulas, " + year + testing + (1 | county / ID)"),
  ziformulas = "~0",
  families = "nbinom1",
  data = data_seasonal_train)

## summarize t1 seasonal models: 

t1_seasonal_vars <- bind_rows(
  bind_cols(extract_model_data(t1_seasonal_models$models), season = "active")) %>%
  # filter(!zero_span == T) %>%
  # filter(!str_detect(term, fixed("2)2")) == T) %>%
  filter(!str_detect(term, "year") == T) %>%
  filter(!term == "testing") %>%
  select(season, term, estimate, std.error, conf.low, conf.high, zero_span, sig) %>%
  as.data.frame()

# ggplot(t1_seasonal_vars) +
#   aes(x = reorder(term, estimate), y = estimate, group = season) +
#   geom_linerange(aes(ymin = estimate - std.error, ymax = estimate + std.error, col = season), 
#                  position = position_dodge(width = 0.5), alpha = 0.5, linewidth = 1.5) +
#   geom_pointrange(aes(ymin = conf.low, ymax = conf.high, col = season), 
#                   position = position_dodge(width = 0.5), size = 0.5) +
#   coord_flip() +
#   # annotate("text", x = 1:48, y = -55, label = reorder(t1_monthly_sig_vars$sig, t1_monthly_sig_vars$estimate)) +
#   geom_abline(slope = 0, linetype = "dashed", col = "grey40") +
#   labs(x = "Term", y = expression(beta)) +
#   theme_bw()

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

## create formula object for inactive season terms
# t2_monthly_inglobal_formula <- t2_monthly_sig_vars %>%
#   filter(season == "inactive") %>%
#   select(term) %>%
#   as_vector() %>%
#   paste(collapse = " + ") %>%
#   as.data.frame(); t2_monthly_inglobal_formula$.

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

## create formula object for inactive season terms
# t2_seasonal_inglobal_formula <- t2_seasonal_sig_vars %>%
#   filter(season == "inactive") %>%
#   select(term) %>%
#   as_vector() %>%
#   paste(collapse = " + ") %>%
#   as.data.frame(); t2_seasonal_inglobal_formula$.

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

######


########### Buildmer models for VIF calculation 

###### Buildmer model: Seasonal - Active model 
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




###############

data_seasonal_pred <- seasonal_glmmtmb_model %>%
  predict(data_seasonal_pred, se.fit = T, type = "response") %>%
  as.data.frame() %>%
  mutate(sample = ifelse(year %in% c("2018", "2019"), "predict", "train")) %>%
  bind_cols(data_seasonal_pred)
# data_seasonal_pred_rmse <- data_seasonal_pred %>%
#   filter(sample == "predict") %>%
#   mutate(rmse = sqrt(mean(wnv - preds))) %>%
#   select(rmse)

# monthly active
data_monthly_pred <- monthly_glmmtmb_model %>%
  predict(data_monthly_train, se.fit = T, type = "response") %>%
  as.data.frame()
# %>%
#   mutate(sample = ifelse(year %in% c("2018", "2019"), "predict", "train")) %>%
#   bind_cols(data_seasonal)
# data_seasonal_pred_rmse <- data_seasonal_pred %>%
#   filter(sample == "predict") %>%
#   mutate(rmse = sqrt(mean(wnv - preds))) %>%
#   select(rmse)

rmse_comp <- data.frame(
  model = c("seasonal_active", "seasonal_inactive", "monthly_active", "monthly_inactive"),
  terms = c(seasonal_glmmtmb_formula, seasonal_inglmmtmb_formula, monthly_glmmtmb_formula, monthly_inglmmtmb_formula),
  # rmse = c(data_seasonal_pred_rmse, data_seasonal_inpred_rmse, data_monthly_pred_rmse, data_monthly_inpred_rmse)
  rmse = rep(NA, 4))

########## 

seasonal_plot_model <- ggpubr::ggarrange(
  
  plot_model(seasonal_glmmtmb_model, 
             type = "pred", terms = "tmin_lag2 [-4:2, by=0.05]", se = T) + 
    labs(x = parse(text = "Minimum~Temperature^2~~(1~year~lag)")) +
    ylim(c(0, 2)) +
    ggtitle("") +
    theme_bw() +
    theme(plot.title = element_blank(),
          axis.title.y = element_blank(),
          axis.title.x = element_text(size = rel(1.2), vjust = 0.7)),
  
  grid::grid.rect(gp=grid::gpar(col="white")),
  
  plot_model(seasonal_glmmtmb_model, 
             type = "pred", terms = "prcp [-2:4, by=0.05]", se = T) + 
    labs(x = parse(text = "Cumulative~Precipitation^2~~(current)")) +
    ylim(c(0, 2)) +
    ggtitle("") +
    theme_bw() +
    theme(plot.title = element_blank(),
          axis.title.y = element_blank(),
          axis.title.x = element_text(size = rel(1.2), vjust = 0.7)),
  
  plot_model(seasonal_glmmtmb_model, 
             type = "pred", terms = "prcp_lag1 [-2:4, by=0.05]", se = T) + 
    labs(x = "Cumulative precipitation  (6 month lag)") +
    ylim(c(0, 2)) +
    ggtitle("") +
    theme_bw() +
    theme(plot.title = element_blank(),
          axis.title.y = element_blank(),
          axis.title.x = element_text(size = rel(1.2), vjust = 0.7)),
  
  plot_model(seasonal_glmmtmb_model, 
             type = "pred", terms = "prcp_lag2 [-2:4, by=0.05]", se = T) + 
    labs(x = parse(text = "Cumulative~Precipitation^2~~(1~year~lag)")) +
    ylim(c(0, 2)) +
    ggtitle("") +
    theme_bw() +
    theme(plot.title = element_blank(),
          axis.title.y = element_blank(),
          axis.title.x = element_text(size = rel(1.2), vjust = 0.7)),
  
  plot_model(seasonal_glmmtmb_model, 
             type = "pred", terms = "developed [-2:3, by=0.05]", se = T) + 
    labs(x = parse(text = "Prop(Developed)^2")) +
    ylim(c(0, 2)) +
    ggtitle("")  +
    theme_bw() +
    theme(plot.title = element_blank(),
          axis.title.y = element_blank(),
          axis.title.x = element_text(size = rel(1.2), vjust = 0.7)),
  
  nrow = 3, 
  ncol = 2); seasonal_plot_model

seasonal_plot_model_ann <- ggpubr::annotate_figure(seasonal_plot_model, 
                                                       left = grid::textGrob("Predicted WNV conversion prevalence", 
                                                                             rot = 90, 
                                                                             vjust = 0.8, 
                                                                             gp = grid::gpar(cex = 1.2)),
                                                       top = grid::textGrob("glmmTMB: Seasonal-Active", 
                                                                            hjust = 0.5, 
                                                                            vjust = 0.7,
                                                                            gp = grid::gpar(cex = 1.5, fontface = "bold"))); seasonal_plot_model_ann
ggsave("figures/Alex/wnv/glmmTMB/seasonal_plot_model_ann.jpeg", 
       width = 8, height = 8, dpi = 600)

##############

seasonal_inplot_model <- ggpubr::ggarrange(
  
  plot_model(seasonal_inglmmtmb_model, 
             type = "pred", terms = "tmin [all]", se = T) + 
    labs(x = parse(text = "Munimum~Temperature^2~~(current)")) +
    # ylim(c(0, 2)) +
    ggtitle("") +
    theme_bw() +
    theme(plot.title = element_blank(),
          axis.title.y = element_blank(),
          axis.title.x = element_text(size = rel(1.2), vjust = 0.7)),
  
  grid::grid.rect(gp=grid::gpar(col="white")),
  
  plot_model(seasonal_inglmmtmb_model, 
             type = "pred", terms = "prcp [-2:4, by=0.05]", se = T) + 
    labs(x = parse(text = "Cumulative~Precipitation^2~~(current)")) +
    # ylim(c(0, 2)) +
    ggtitle("") +
    theme_bw() +
    theme(plot.title = element_blank(),
          axis.title.y = element_blank(),
          axis.title.x = element_text(size = rel(1.2), vjust = 0.7)),
  
  plot_model(seasonal_inglmmtmb_model, 
             type = "pred", terms = "prcp_lag2 [-2:4, by=0.05]", se = T) + 
    labs(x = parse(text = "Cumulative~Precipitation^2~~(1~year~lag)")) +
    # ylim(c(0, 2)) +
    ggtitle("") +
    theme_bw() +
    theme(plot.title = element_blank(),
          axis.title.y = element_blank(),
          axis.title.x = element_text(size = rel(1.2), vjust = 0.7)),
  

  
  nrow = 2, 
  ncol = 2); seasonal_inplot_model

seasonal_plot_model_ann <- ggpubr::annotate_figure(seasonal_plot_model, 
                                                          left = grid::textGrob("Predicted WNV conversion prevalence", 
                                                                                rot = 90, 
                                                                                vjust = 0.8, 
                                                                                gp = grid::gpar(cex = 1.2)),
                                                          top = grid::textGrob("glmmTMB: Seasonal-Active", 
                                                                               hjust = 0.5, 
                                                                               vjust = 0.7,
                                                                               gp = grid::gpar(cex = 1.5, fontface = "bold"))); seasonal_plot_model_ann
ggsave("figures/Alex/wnv/glmmTMB/seasonal_plot_model_ann.jpeg", 
       width = 8, height = 8, dpi = 600)

#############

monthly_plot_model <- ggpubr::ggarrange(
  
  plot_model(monthly_glmmtmb_model, 
             type = "pred", terms = "prcp_lag1 [-4:6, by=0.05]", se = T) + 
    labs(x = parse(text = "Cumulative~Precipitation^2~~(6~month~lag)")) +
    ylim(c(0, 0.25)) +
    ggtitle("") +
    theme_bw() +
    theme(plot.title = element_blank(),
          axis.title.y = element_blank(),
          axis.title.x = element_text(size = rel(1.2), vjust = 0.7)),
  
  plot_model(monthly_glmmtmb_model, 
             type = "pred", terms = "tmin_lag1 [-3:4, by=0.05]", se = T) + 
    labs(x = parse(text = "Minimum~Temperature^2~~(6~month~lag)")) +
    ylim(c(0, 0.25)) +
    ggtitle("") +
    theme_bw() +
    theme(plot.title = element_blank(),
          axis.title.y = element_blank(),
          axis.title.x = element_text(size = rel(1.2), vjust = 0.7)),
  
  plot_model(monthly_glmmtmb_model, 
             type = "pred", terms = "tmin_lag2 [-4:4, by=0.05]", se = T) + 
    labs(x = parse(text = "Minimum~Temperature^2~~(1~year~lag)")) +
    ylim(c(0, 0.25)) +
    ggtitle("") +
    theme_bw() +
    theme(plot.title = element_blank(),
          axis.title.y = element_blank(),
          axis.title.x = element_text(size = rel(1.2), vjust = 0.7)),

  plot_model(monthly_glmmtmb_model, 
             type = "pred", terms = "developed [-2:4, by=0.05]", se = T) + 
    labs(x = parse(text = "Prop(Developed)^2")) +
    ylim(c(0, 0.25)) +
    ggtitle("")  +
    theme_bw() +
    theme(plot.title = element_blank(),
          axis.title.y = element_blank(),
          axis.title.x = element_text(size = rel(1.2), vjust = 0.7)),
  
  nrow = 2, 
  ncol = 2); monthly_plot_model

monthly_plot_model_ann <- ggpubr::annotate_figure(monthly_plot_model, 
                                                         left = grid::textGrob("Predicted WNV conversion prevalence", 
                                                                               rot = 90, 
                                                                               vjust = 0.8, 
                                                                               gp = grid::gpar(cex = 1.2)),
                                                         top = grid::textGrob("glmmTMB: Monthly-Active", 
                                                                              hjust = 0.5, 
                                                                              vjust = 0.7,
                                                                              gp = grid::gpar(cex = 1.5, fontface = "bold"))); monthly_plot_model_ann
ggsave("figures/Alex/wnv/glmmTMB/monthly_plot_model_ann.jpeg", 
       width = 8, height = 8, dpi = 600)

##############

monthly_inplot_model <- plot_model(monthly_inglmmtmb_model, 
                                           type = "pred", terms = "tmin [all]", se = T) + 
  labs(x = "Minimum temperature (6 month lag)", y = "Predicted WNV conversion prevalence") +
  # ylim(c(0, 1.5)) +
  ggtitle("glmmTMB: Monthly-Inactive") +
  theme_bw() +
  theme(plot.title = element_text(face = "bold", vjust = 0.7, hjust = 0.5),
        axis.title = element_text(vjust = 0.7)); monthly_inplot_model

ggsave("figures/Alex/wnv/glmmTMB/monthly_inplot_model.jpeg", width = 4, height = 4, dpi = 600)

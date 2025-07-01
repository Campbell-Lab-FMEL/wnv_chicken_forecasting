

create_bivariate_formulas <- function(focal_season, sig_vars){
  
  pacman::p_load(
    tidyverse
    , Hmisc
    , corrplot
    # ,
    # ,
  )
  
  ################ Chicken data ###################
  
  ##### Monthly data
  
  setwd("C:/Users/jbaecher/UFL Dropbox/Joseph Baecher/UF/postdoc/chickens")
  
  data_seasonal_corr <- data_seasonal %>%
    st_drop_geometry() %>%
    select(tmax:prcp_lag2) %>%
    mutate(poly_tmax = tmax^2,
           poly_tmin = tmin^2,
           poly_prcp = prcp^2,
           "poly(tmax_lag1, 2)1" = tmax_lag1^2,
           poly_tmin_lag1 = tmin_lag1^2,
           poly_prcp_lag1 = prcp_lag1^2,
           poly_tmax_lag2 = tmax_lag2^2,
           poly_tmin_lag2 = tmin_lag2^2,
           poly_prcp_lag2 = prcp_lag2^2) %>%
    as.matrix() %>%
    rcorr()
  
  flattenCorrMatrix <- function(cormat, pmat) {
    ut <- upper.tri(cormat)
    data.frame(
      row = rownames(cormat)[row(cormat)[ut]],
      column = rownames(cormat)[col(cormat)[ut]],
      cor  =(cormat)[ut],
      p = pmat[ut]
    )
  }
  
  data_seasonal_corr_flat <- flattenCorrMatrix(data_seasonal_corr$r, data_seasonal_corr$P)
  
  data_seasonal_corr_rules <- data_seasonal_corr_flat %>%
    filter(abs(cor) < 0.1)
  
  data_seasonal_corr_rules <- bind_rows(
    data_seasonal_corr_rules, 
    data.frame(
      column = data_seasonal_corr_rules$row,
      row = data_seasonal_corr_rules$column,
      cor = data_seasonal_corr_rules$cor,
      p = data_seasonal_corr_rules$p)) %>%
    arrange(row)
  
  sig_terms <- sig_vars %>%
    filter(season == paste(focal_season)) %>%
    filter(!str_detect(term, "poly")) %>%
    select(term) %>%
    bind_rows(data.frame(
      p1 = str_split_fixed(sig_vars$term, fixed("("), 2)[,1] ,
      p1 = str_split_fixed(sig_vars$term, fixed("("), 2)[,2] ) %>%
        mutate(p2 = str_split_fixed(p1.1, ",", 2)[,1]) %>%
        filter(!p2 == "") %>%
        transmute(term = paste(p1, p2, sep = "_"))) %>%
    arrange(term)
  
  t2_terms <- data_seasonal_corr_rules %>%
    filter(row %in% c(sig_terms$term)) %>%
    filter(column %in% c(sig_terms$term))
  
  formulas <- paste0("wnv ~ ", t2_terms$row, " + ", t2_terms$column, 
         " + (1 | country / ID") %>%
    as_vector()
  
  output <- list(  )
  
  output[["sig_terms"]] <- sig_terms
  output[["t2_terms"]] <- t2_terms
  output[["formulas"]] <- formulas
  
  return(output)
  
}
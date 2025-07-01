

buildmermod_to_glmmtmb <- function(buildmermod, data, response, ziformula, fixed, ranef){
  
  pacman::p_load(
    tidyverse
    , performance
    , glmmTMB
    , buildmer
    # ,
    # ,
  )
  
  outputs <- list()
  
  buildmermod_vars <- buildmermod %>%
    fixef() %>% 
    unlist() %>% 
    as.data.frame() %>% 
    rownames_to_column("term") %>%
    select(term) %>%
    mutate(term = str_remove(term, "cond."),
           term = str_remove(term, "disp.")) %>%
    filter(!term %in% c("(Intercept)", "zi.(Intercept)", "year", "testing")) %>%
    filter(!str_detect(term, fixed("year"))) %>%
    filter(!str_detect(term, fixed(", 2)2")) == T) 
  
  buildmermod_vars_formula <- buildmermod_vars %>%
    filter(!str_detect(term, "poly") == T) %>%
    bind_rows(
      data.frame(
        buildmermod_vars %>%
          filter(str_detect(term, "poly") == T) %>%
          mutate(term = paste0(str_split_fixed(term, fixed("2)"), n = 2)[,1], "2)")))) %>%
    as_vector() %>%
    paste(collapse = " + ")
  
  glmmtmb_model <- glmmTMB(
    formula(paste0(response, " ~ ", fixed, " + ", buildmermod_vars_formula, " + ", ranef)),
    ziformula = formula(paste0(ziformula)),
    family = "nbinom1",
    data = data)
  
  outputs[["variables"]] <- buildmermod_vars
  outputs[["formula"]] <- buildmermod_vars_formula
  outputs[["model"]] <- glmmtmb_model
  
  return(outputs)
  
}
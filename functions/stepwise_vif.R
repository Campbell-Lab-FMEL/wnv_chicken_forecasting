
pacman::p_load(
  tidyverse
  , glmmTMB
  , performance
  # ,
  # ,
)

stepwise_vif <- function(
    variables,   # fixed effects variable names to be checked (as column in data.frame)
    vif_thresh,  # threshold of VIF to be removed (numeric element)
    response,    # response variable name (character element)
    keep_fixed,  # partial formula with fixed effect variables to be kept (character string with '+' between variables)
    keep_raneff, # partial formula with random effect variables to be kept (character string with '+' between variables)
    ziformula,   # zero-inflation formula
    family,      # family name for model (character)
    data         # data to fit the model on
    ){
  
  output <- list()
  
  formula <- variables %>%
    as_vector() %>%
    paste(collapse = " + ") %>%
    as.data.frame()
  
  model <- glmmTMB(
    formula(paste0(response, " ~ ", formula, " + ", keep_fixed, " + ", keep_raneff)),
    ziformula = formula(paste0(ziformula)),
    family = paste0(family), 
    data = data)
  
  vif <- check_collinearity(model)
  
  over_threshold <- vif %>%
    as.data.frame() %>%
    filter(!Term == "year") %>%
    filter(VIF > vif_thresh)
  
  cat("\r Running intitial model")
  
  while(nrow(over_threshold) > 0) {
    
  remove_var <- over_threshold$Term[over_threshold$VIF == max(over_threshold$VIF)]
  
  cat("\r VIF >",
      vif_thresh,
      "was detected. Removing variable:",
      remove_var,
      "with  VIF =",
      max(over_threshold$VIF))
    
    new_formula <- vif %>%
      as.data.frame() %>%
      filter(!Term == remove_var) %>% 
      select(Term) %>%
      as_vector() %>%
      paste(collapse = " + ") %>%
      as.data.frame()
    
    model <- glmmTMB(
      formula(paste0(response, " ~ ", new_formula, " + ", keep_fixed, " + ", keep_raneff)),
      ziformula = formula(paste0(ziformula)),
      family = paste0(family), 
      data = data)
    
    vif <- check_collinearity(model)
    
    over_threshold <- vif %>%
      as.data.frame() %>%
      filter(!Term == "year") %>%
      filter(VIF > vif_thresh)
    }
  
  output[["vif"]] <- vif
  output[["model"]] <- model
  output[["formula"]] <- new_formula
  
  return(output)
  
}

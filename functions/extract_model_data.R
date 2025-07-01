
extract_model_data <- function(models){
  
  pacman::p_load(
    tidyverse
    , broom.mixed
    , pbapply
    # ,
    # ,
  )
  
  model_data <- pblapply(models, FUN = function(x){
    broom.mixed::tidy(x, conf.int = T)}) %>%
    bind_rows() %>%
    filter(!effect == "ran_pars") %>%
    filter(!term == "(Intercept)") %>%
    mutate(zero_span = ifelse(sign(conf.low) == sign(conf.high), F, T)) %>%
    mutate(sig = cut(p.value, breaks = c(-Inf, 0.01, 0.05, 0.10, Inf), 
                     labels = c("***", "**", "*", ""), right = FALSE)) %>%
    arrange(estimate) 
  
  return(model_data)
  
}
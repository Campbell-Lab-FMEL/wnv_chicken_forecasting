
pacman::p_load(
  tidyverse
  , sf
  , glmmTMB
  , AICcmodavg
  , DHARMa
  # ,
  # ,
)

compare_models <- function(formulas, ziformulas, families, data){
  
  if (length(families) == 1) {
    
    families <- rep(families, length(formulas))
    
    ziformulas <- rep(ziformulas, length(families))
    
  } else {
    
    formulas <- rep(formulas, length(families))
    
  }
  
  models <- list()
  
  for(i in 1:length(formulas)){
    
    models[[i]] <- glmmTMB(
      formula(paste0(formulas[i])), 
      ziformula = formula(paste0(ziformulas[i])),
      family = paste0(families[i]), 
      data = data)
    
    cat("\r Fitted model: ",
        formulas[i],
        " with zi-formula = ",
        ziformulas[i], 
        " and ",
        families[i], 
        " distribution.",
        "Process ",
        round(
          (i/length(formulas))*100,
          0),
        " % complete", sep = "")
  }
  
  if(i == length(families)){
    
    cat("\r Final model has been fitted. Calculating  ", sep = "")
    
    names(models) <- paste(formulas, families, ziformulas, sep = " | ")
    
    aictab <- aictab(
      models, 
      names(models), 
      second.ord = F)
    
    top_mod <- models[[paste0(aictab$Modnames[1])]]
    
    # dharma <- top_mod %>%
    #   simulateResiduals()
    
    output <- list(
      models, 
      aictab, 
      top_mod); names(output) <- c("models", "aictab", "top_mod")
    
    return(output)
  }
}


pacman::p_load(
  tidyverse
  , sdmTMB
  , tinyVAST
  , INLA
  # ,
  # ,
)

predict_hess_sampler <- function(
    model, 
    data){
  
  cat("getting standard model predictions")
  
  pred_obj <- predict(model, 
                      newdata = data, 
                      re_form = T,
                      re_form_iid = T, 
                      se_fit = T,
                      return_tmb_object = T  )
  
  if(class(model) == "tinyVAST"){
    
    cat("sampling inner Hessian matrix to extract standard errors")
    
    samples <- attr(pred_obj$obj$env$MC(antithetic=FALSE, keep=TRUE, n=100), "samples")  
    last_par <- pred_obj$obj$env$last.par.best
    last_par[pred_obj$obj$env$lrandom()] <- samples[,1]
    rep <- pred_obj$obj$report( last.par )
    
  }
  
  if(class(model) == "sdmTMB"){
    
    cat("sampling inner Hessian matrix to extract standard errors")
    
    samples <- attr(pred_obj$fit_obj$tmb_obj$env$MC(antithetic=FALSE, keep=TRUE, n=100), "samples")  
    last_par <- pred_obj$fit_obj$tmb_obj$last.par.best
    last_par[pred_obj$fit_obj$tmb_obj$env$lrandom()] <- samples[,1]
    rep <- pred_obj$fit_obj$tmb_obj$report( last.par )
    
  }
  
  else(cat("you messed up somewhere..."))
  
  cat("compiling prediction and Hessian sample data")
  
  pred_df <- pred_obj$data %>%
    mutate(est = exp(est),
           est_se = exp(est_se))
    bind_cols(
      data.frame(
    eta = rep$proj_eta, 
    epsilon_st_A_vec = rep$proj_epsilon_st_A_vec,
    fe = rep$proj_fe, 
    iid_re_i = rep$proj_iid_re_i, 
    omega_s_A = rep$proj_omega_s_A,
    rf = rep$proj_rf,
    rw_i = rep$proj_rw_i,
    zeta_s_A = rep$proj_zeta_s_A  )
    )
  
  return(pred_df)
  
}


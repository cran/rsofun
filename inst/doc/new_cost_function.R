## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

library(rsofun)
library(dplyr)
library(ggplot2)

## ----eval = FALSE-------------------------------------------------------------
#  # Define calibration settings and parameter ranges from previous work
#  settings_rmse <- list(
#    method = 'GenSA',                   # minimizes the RMSE
#    metric = cost_rmse_pmodel,          # our cost function
#    control = list(                     # control parameters for optimizer GenSA
#      maxit = 100),
#    par = list(                         # bounds for the parameter space
#      kphio = list(lower=0.02, upper=0.2, init=0.05)
#    )
#  )
#  
#  # Calibrate the model and optimize the free parameters using
#  # demo datasets
#  pars_calib_rmse <- calib_sofun(
#    # calib_sofun arguments:
#    drivers = p_model_drivers,
#    obs = p_model_validation,
#    settings = settings_rmse,
#    # extra arguments passed to the cost function:
#    par_fixed = list(         # fix all other parameters
#      kphio_par_a        = 0.0,        # set to zero to disable temperature-dependence
#                                       # of kphio, setup ORG
#      kphio_par_b        = 1.0,
#      soilm_thetastar    = 0.6 * 240,  # to recover paper setup with soil moisture stress
#      soilm_betao        = 0.0,
#      beta_unitcostratio = 146.0,
#      rd_to_vcmax        = 0.014,      # value from Atkin et al. 2015 for C3 herbaceous
#      tau_acclim         = 30.0,
#      kc_jmax            = 0.41
#    ),
#    targets = "gpp"           # define target variable GPP
#  )

## ----eval = FALSE-------------------------------------------------------------
#  # Define calibration settings
#  settings_likelihood <- list(
#    method = 'BayesianTools',
#    metric = cost_likelihood_pmodel,            # our cost function
#    control = list(                             # optimization control settings for
#      sampler = 'DEzs',                           # BayesianTools::runMCMC
#      settings = list(
#        burnin = 1500,
#        iterations = 3000
#      )),
#    par = list(
#      kphio = list(lower = 0, upper = 0.2, init = 0.05),
#      kphio_par_a = list(lower = -0.5, upper = 0.5, init = -0.1),
#      kphio_par_b = list(lower = 10, upper = 40, init =25),
#      err_gpp = list(lower = 0.1, upper = 4, init = 0.8)
#    )
#  )
#  
#  # Calibrate the model and optimize the free parameters using
#  # demo datasets
#  pars_calib_likelihood <- calib_sofun(
#    # calib_sofun arguments:
#    drivers = p_model_drivers,
#    obs = p_model_validation,
#    settings = settings_likelihood,
#    # extra arguments passed ot the cost function:
#    par_fixed = list(         # fix all other parameters
#      soilm_thetastar    = 0.6 * 240,  # to recover paper setup with soil moisture stress
#      soilm_betao        = 0.0,
#      beta_unitcostratio = 146.0,
#      rd_to_vcmax        = 0.014,      # value from Atkin et al. 2015 for C3 herbaceous
#      tau_acclim         = 30.0,
#      kc_jmax            = 0.41
#    ),
#    targets = "gpp"
#  )

## ----eval = FALSE-------------------------------------------------------------
#  # Define calibration settings for two targets
#  settings_joint_likelihood <- list(
#    method = "BayesianTools",
#    metric = cost_likelihood_pmodel,
#    control = list(
#      sampler = "DEzs",
#      settings = list(
#        burnin = 1500,             # kept artificially low
#        iterations = 3000
#      )),
#    par = list(kc_jmax = list(lower = 0.2, upper = 0.8, init = 0.41),  # uniform priors
#               err_gpp = list(lower = 0.001, upper = 4, init = 1),
#               err_vcmax25 = list(lower = 0.000001, upper = 0.0001, init = 0.00001))
#  )
#  
#  # Run the calibration on the concatenated data
#  par_calib_join <- calib_sofun(
#    drivers = rbind(p_model_drivers,
#                    p_model_drivers_vcmax25),
#    obs = rbind(p_model_validation,
#                p_model_validation_vcmax25),
#    settings = settings_joint_likelihood,
#    # arguments for the cost function
#    par_fixed = list(         # fix parameter value from previous calibration
#      kphio              = 0.041,
#      kphio_par_a        = 0.0,
#      kphio_par_b        = 16,
#      soilm_thetastar    = 0.6 * 240,  # to recover paper setup with soil moisture stress
#      soilm_betao        = 0.0,
#      beta_unitcostratio = 146.0,
#      rd_to_vcmax        = 0.014,      # value from Atkin et al. 2015 for C3 herbaceous
#      tau_acclim         = 30.0
#    ),
#    targets = c('gpp', 'vcmax25')
#  )

## ---- eval = FALSE------------------------------------------------------------
#  function(par, obs, drivers){
#    # Your code
#  }

## ---- eval = FALSE------------------------------------------------------------
#  function(par, obs, drivers){
#  
#    # Set values for the list of calibrated and non-calibrated model parameters
#    params_modl <- list(
#      kphio              = 0.09423773,
#      kphio_par_a        = 0.0,
#      kphio_par_b        = 25,
#      soilm_thetastar    = par[1],
#      soilm_betao        = par[2],
#      beta_unitcostratio = 146.0,
#      rd_to_vcmax        = 0.014,
#      tau_acclim         = 30.0,
#      kc_jmax            = 0.41
#    )
#  
#    # Run the model
#    df <- runread_pmodel_f(
#      drivers,
#      par = params_modl,
#      makecheck = TRUE,
#      parallel = FALSE
#    )
#  
#    # Your code to compute the cost
#  }

## -----------------------------------------------------------------------------
cost_mae <- function(par, obs, drivers){

  # Set values for the list of calibrated and non-calibrated model parameters
  params_modl <- list(
    kphio              = 0.09423773,
    kphio_par_a        = 0.0,
    kphio_par_b        = 25,
    soilm_thetastar    = par[1],
    soilm_betao        = par[2],
    beta_unitcostratio = 146.0,
    rd_to_vcmax        = 0.014,
    tau_acclim         = 30.0,
    kc_jmax            = 0.41
  ) # Set values for the list of calibrated and non-calibrated model parameters
  
  
  # Run the model
  df <- runread_pmodel_f(
    drivers = drivers,
    par = params_modl,
    makecheck = TRUE,
    parallel = FALSE
  )
  
  # Clean model output to compute cost
  df <- df %>%
    dplyr::select(sitename, data) %>%
    tidyr::unnest(data)
    
  # Clean validation data to compute cost
  obs <- obs %>%
    dplyr::select(sitename, data) %>%
    tidyr::unnest(data) %>%
    dplyr::rename('gpp_obs' = 'gpp') # rename for later
    
  # Left join model output with observations by site and date
  df <- dplyr::left_join(df, obs, by = c('sitename', 'date'))
  
  # Compute mean absolute error
  cost <- mean(abs(df$gpp - df$gpp_obs), na.rm = TRUE)
  
  # Return the computed cost
  return(cost)
}

## ----eval = FALSE-------------------------------------------------------------
#  # Define calibration settings and parameter ranges
#  settings_mae <- list(
#    method = 'GenSA',
#    metric = cost_mae, # our cost function
#    control = list(
#      maxit = 100),
#    par = list(
#      soilm_thetastar = list(lower=0.0, upper=3000, init=0.6*240),
#      soilm_betao = list(lower=0, upper=1, init=0.2)
#    )
#  )
#  
#  # Calibrate the model and optimize the free parameters
#  pars_calib_mae <- calib_sofun(
#    drivers = p_model_drivers,
#    obs = p_model_validation,
#    settings = settings_mae
#  )


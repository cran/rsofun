## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

library(rsofun)
library(dplyr)
library(ggplot2)

## ----eval = FALSE-------------------------------------------------------------
#  library(rsofun)
#  
#  biomee_gs_leuning_drivers
#  biomee_p_model_drivers
#  biomee_validation

## -----------------------------------------------------------------------------
# print parameter settings
biomee_gs_leuning_drivers$params_siml

# print forcing
head(biomee_gs_leuning_drivers$forcing)

## ----eval = FALSE-------------------------------------------------------------
#  set.seed(2023)
#  
#  # run the model
#  biomee_gs_leuning_output <- runread_biomee_f(
#       biomee_gs_leuning_drivers,
#       makecheck = TRUE,
#       parallel = FALSE
#       )
#  
#  # split out the annual data
#  biomee_gs_leuning_output <- biomee_gs_leuning_output$data[[1]]$output_annual_tile

## ----echo = FALSE, eval = FALSE-----------------------------------------------
#  # Save output
#  save(biomee_gs_leuning_output, file = "files/biomee_gs_leuning_output.rda")

## ----echo = FALSE-------------------------------------------------------------
load("files/biomee_gs_leuning_output.rda")

## ----fig.width=7--------------------------------------------------------------
# we only have one site so we'll unnest
# the main model output
cowplot::plot_grid(
  biomee_gs_leuning_output |>
    ggplot() +
    geom_line(aes(x = year, y = GPP)) +
    theme_classic()+labs(x = "Year", y = "GPP"),
  biomee_gs_leuning_output |>
    ggplot() +
    geom_line(aes(x = year, y = plantC)) +
    theme_classic()+labs(x = "Year", y = "plantC")
)

## -----------------------------------------------------------------------------
# print parameter settings
biomee_p_model_drivers$params_siml

# print forcing for P-model
head(biomee_p_model_drivers$forcing)

## ----eval = FALSE-------------------------------------------------------------
#  # run the model
#  biomee_p_model_output <- runread_biomee_f(
#       biomee_p_model_drivers,
#       makecheck = TRUE,
#       parallel = FALSE
#       )
#  
#  # split out the annual data for visuals
#  biomee_p_model_output <- biomee_p_model_output$data[[1]]$output_annual_tile

## ----echo = FALSE, eval = FALSE-----------------------------------------------
#  # Save output
#  save(biomee_p_model_output, file = "files/biomee_p_model_output.rda")

## ----echo = FALSE-------------------------------------------------------------
load("files/biomee_p_model_output.rda")

## ----fig.width=7--------------------------------------------------------------
# we only have one site so we'll unnest
# the main model output

cowplot::plot_grid(
  biomee_p_model_output %>% 
    ggplot() +
    geom_line(aes(x = year, y = GPP)) +
    theme_classic()+labs(x = "Year", y = "GPP"),
  biomee_p_model_output %>% 
    ggplot() +
    geom_line(aes(x = year, y = plantC)) +
    theme_classic()+labs(x = "Year", y = "plantC")
)

## ----eval = FALSE-------------------------------------------------------------
#  # Mortality as DBH
#  settings <- list(
#    method              = "GenSA",
#    metric              = cost_rmse_biomee,
#    control = list(
#      maxit = 10
#    ),
#    par = list(
#        phiRL = list(lower=0.5, upper=5, init=3.5),
#        LAI_light = list(lower=2, upper=5, init=3.5),
#        tf_base = list(lower=0.5, upper=1.5, init=1),
#        par_mort = list(lower=0.1, upper=2, init=1))
#  )
#  
#  pars <- calib_sofun(
#    drivers = biomee_gs_leuning_drivers,
#    obs = biomee_validation_2,
#    settings = settings
#  )

## ----echo = FALSE-------------------------------------------------------------
# Take values from the chunk before, which was run locally
pars <- list(
  par = c(phiRL = 0.9709220,
          LAI_light = 4.5722199,
          tf_base = 0.5849346,
          par_mort = 1.5779371)
)

## ----eval = FALSE-------------------------------------------------------------
#  # replace parameter values by calibration output
#  drivers <- biomee_p_model_drivers
#  drivers$params_species[[1]]$phiRL[]  <- pars$par[1]
#  drivers$params_species[[1]]$LAI_light[]  <- pars$par[2]
#  drivers$params_tile[[1]]$tf_base <- pars$par[3]
#  drivers$params_tile[[1]]$par_mort <- pars$par[4]
#  
#  # run the model with new parameter values
#  biomee_p_model_output_calib <- runread_biomee_f(
#       drivers,
#       makecheck = TRUE,
#       parallel = FALSE
#       )
#  
#  # split out the annual data
#  biomee_p_model_output_calib <- biomee_p_model_output_calib$data[[1]]$output_annual_tile

## ----echo = FALSE, eval = FALSE-----------------------------------------------
#  # Save output
#  save(biomee_p_model_output_calib, file = "files/biomee_p_model_output_calib.rda")

## ----echo = FALSE-------------------------------------------------------------
load("files/biomee_p_model_output_calib.rda")

## ----fig.width=7--------------------------------------------------------------
# unnest model output for our single site
cowplot::plot_grid(
  ggplot() +
    geom_line(data = biomee_p_model_output,
              aes(x = year, y = GPP)) +
    geom_line(data = biomee_p_model_output_calib,
              aes(x = year, y = GPP),
              color = "grey50") +
    theme_classic() + 
    labs(x = "Year", y = "GPP") +
    geom_hline(yintercept = biomee_validation_2$data[[1]]$targets_obs[1],
                lty=2),        # plot observation
  
  ggplot() +
    geom_line(data = biomee_p_model_output,
              aes(x = year, y = plantC)) +
    geom_line(data = biomee_p_model_output_calib,
              aes(x = year, y = plantC),
              color = "grey50") +
    theme_classic() + 
    labs(x = "Year", y = "plantC") +
    geom_hline(yintercept = biomee_validation_2$data[[1]]$targets_obs[4],
               lty = 2)        # plot observation
)






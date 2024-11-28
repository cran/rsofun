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
#  out <- runread_biomee_f(
#       biomee_gs_leuning_drivers,
#       makecheck = TRUE,
#       parallel = FALSE
#       )
#  
#  # split out the annual data
#  biomee_gs_leuning_output_annual_tile <- out$data[[1]]$output_annual_tile
#  biomee_gs_leuning_output_annual_cohorts <- out$data[[1]]$output_annual_cohorts

## ----simulate_biomee_gs_leuning_run, include = FALSE--------------------------
# TODO: get rid of this and always fully run the vignettes
# fake output since model isn't run
# saveRDS(out, "files/biomee_use.Rmd__biomee_gs_leuning_output___out.RDS")
out <- readRDS("files/biomee_use.Rmd__biomee_gs_leuning_output___out.RDS")
biomee_gs_leuning_output_annual_tile <- out$data[[1]]$output_annual_tile
biomee_gs_leuning_output_annual_cohorts <- out$data[[1]]$output_annual_cohorts

## ----fig.width=7--------------------------------------------------------------
# we only have one site so we'll unnest
# the main model output
biomee_gs_leuning_output_annual_tile |>
  ggplot() +
  geom_line(aes(x = year, y = GPP)) +
  theme_classic()+labs(x = "Year", y = "GPP")

biomee_gs_leuning_output_annual_tile |>
  ggplot() +
  geom_line(aes(x = year, y = plantC)) +
  theme_classic()+labs(x = "Year", y = "plantC")

biomee_gs_leuning_output_annual_cohorts %>% group_by(cohort,year) %>%
  summarise(npp=sum(NPP*density/10000)) %>% mutate(cohort=as.factor(cohort)) %>%
  ggplot(aes(x=cohort,y=npp,fill=year)) +
  geom_bar(stat="identity") +
  theme_classic()+labs(x = "Cohort", y = "NPP")


## -----------------------------------------------------------------------------
# print parameter settings
biomee_p_model_drivers$params_siml

# print forcing for P-model
head(biomee_p_model_drivers$forcing)

## ----eval = FALSE-------------------------------------------------------------
#  # run the model
#  out <- runread_biomee_f(
#       biomee_p_model_drivers,
#       makecheck = TRUE,
#       parallel = FALSE
#       )
#  
#  # split out the annual data for visuals
#  biomee_p_model_output_annual_tile <- out$data[[1]]$output_annual_tile
#  biomee_p_model_output_annual_cohorts <- out$data[[1]]$output_annual_cohorts

## ----simulate_biomee_p_model_run, include = FALSE-----------------------------
# TODO: get rid of this and always fully run the vignettes
# fake output since model isn't run
# saveRDS(out, "files/biomee_use.Rmd__biomee_p_model_output___out.RDS")
out <- readRDS("files/biomee_use.Rmd__biomee_p_model_output___out.RDS")
biomee_p_model_output_annual_tile <- out$data[[1]]$output_annual_tile
biomee_p_model_output_annual_cohorts <- out$data[[1]]$output_annual_cohorts

## ----fig.width=7--------------------------------------------------------------
# we only have one site so we'll unnest
# the main model output
biomee_p_model_output_annual_tile %>%
  ggplot() +
  geom_line(aes(x = year, y = GPP)) +
  theme_classic() +
  labs(x = "Year", y = "GPP")

biomee_p_model_output_annual_tile %>%
  ggplot() +
  geom_line(aes(x = year, y = plantC)) +
  theme_classic() +
  labs(x = "Year", y = "plantC")

biomee_p_model_output_annual_cohorts %>% group_by(cohort,year) %>%
  summarise(npp=sum(NPP*density/10000)) %>% mutate(cohort=as.factor(cohort)) %>%
  ggplot(aes(x=cohort,y=npp,fill=year)) +
  geom_bar(stat="identity") +
  theme_classic()+labs(x = "Cohort", y = "NPP")


## ----eval = FALSE-------------------------------------------------------------
#  # Mortality as DBH
#  settings <- list(
#    method = "GenSA",
#    metric = cost_rmse_biomee,
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
#  # Using BiomeEP (with P-model for photosynthesis)
#  pars <- calib_sofun(
#    drivers = biomee_p_model_drivers,
#    obs = biomee_validation,
#    settings = settings
#  )

## ----include = FALSE----------------------------------------------------------
# TODO: get rid of this and always fully run the vignettes
# fake output since calibration isn't run
# saveRDS(pars, "files/biomee_use.Rmd__biomee_p_model_output___pars.RDS")
# pars <- readRDS("files/biomee_use.Rmd__biomee_p_model_output___pars.RDS")
pars <- list(
  par = c(phiRL = 1.5597893,    #2.0492210,
          LAI_light = 4.9657286,#4.5462360,
          tf_base = 0.9885088,  #0.5006033,
          par_mort = 0.1665635  #1.9317278)
))

## ----eval = FALSE-------------------------------------------------------------
#  # replace parameter values by calibration output
#  drivers <- biomee_p_model_drivers
#  drivers$params_species[[1]]$phiRL[]  <- pars$par[1]
#  drivers$params_species[[1]]$LAI_light[]  <- pars$par[2]
#  drivers$params_tile[[1]]$tf_base <- pars$par[3]
#  drivers$params_tile[[1]]$par_mort <- pars$par[4]
#  
#  # run the model with new parameter values
#  calibrated_out <- runread_biomee_f(
#       drivers,
#       makecheck = TRUE,
#       parallel = FALSE
#       )
#  
#  # split out the annual data
#  biomee_p_model_calibratedOutput_annual_tile <- calibrated_out$data[[1]]$output_annual_tile

## ----simulate_biomee_p_model_calibration, include = FALSE---------------------
# TODO: get rid of this and always fully run the vignettes
# fake output since calibrated simulation isn't run
# saveRDS(calibrated_out, "files/biomee_use.Rmd__biomee_p_model_output___calibrated_out.RDS")
calibrated_out <- readRDS("files/biomee_use.Rmd__biomee_p_model_output___calibrated_out.RDS")
biomee_p_model_calibratedOutput_annual_tile <- calibrated_out$data[[1]]$output_annual_tile

## ----fig.width=7--------------------------------------------------------------
# unnest model output for our single site
ggplot() +
  geom_line(data = biomee_p_model_output_annual_tile,
            aes(x = year, y = GPP)) +
  geom_line(data = biomee_p_model_calibratedOutput_annual_tile,
            aes(x = year, y = GPP),
            color = "grey50") +
  theme_classic() + 
  labs(x = "Year", y = "GPP")

ggplot() +
  geom_line(data = biomee_p_model_output_annual_tile,
            aes(x = year, y = plantC)) +
  geom_line(data = biomee_p_model_calibratedOutput_annual_tile,
            aes(x = year, y = plantC),
            color = "grey50") +
  theme_classic() + 
  labs(x = "Year", y = "plantC")


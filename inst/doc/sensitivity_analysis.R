## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  fig.align = "center"
)

library(rsofun)
library(dplyr)
library(ggplot2)
library(tidyr)
library(sensitivity)
library(BayesianTools)

## -----------------------------------------------------------------------------
# Define log-likelihood function
ll_pmodel <- function(
    par_v                 # a vector of all calibratable parameters including errors
){
  rsofun::cost_likelihood_pmodel(        # reuse likelihood cost function
    par_v,
    obs = rsofun::p_model_validation,
    drivers = rsofun::p_model_drivers,
    targets = "gpp"
  )
}

# Compute log-likelihood for a given set of parameters
ll_pmodel( par_v = c(
  kphio              = 0.09423773, # setup ORG in Stocker et al. 2020 GMD
  kphio_par_a        = 0.0,        # set to zero to disable temperature-dependence of kphio
  kphio_par_b        = 1.0,
  soilm_thetastar    = 0.6 * 240,  # to recover old setup with soil moisture stress
  soilm_betao        = 0.0,
  beta_unitcostratio = 146.0,
  rd_to_vcmax        = 0.014,      # value from Atkin et al. 2015 for C3 herbaceous
  tau_acclim         = 30.0,
  kc_jmax            = 0.41,
  error_gpp          = 0.9         # value from previous simulations
))

## -----------------------------------------------------------------------------
# best parameter values (from previous literature)
par_cal_best <- c(
    kphio              = 0.09423773,
    kphio_par_a        = -0.0025,
    kphio_par_b        = 20,
    soilm_thetastar    = 0.6*240,
    soilm_betao        = 0.2,
    beta_unitcostratio = 146.0,
    rd_to_vcmax        = 0.014,
    tau_acclim         = 30.0,
    kc_jmax            = 0.41,
    error_gpp          = 1
  )

# lower bound
par_cal_min <- c(
    kphio              = 0.03,
    kphio_par_a        = -0.004,
    kphio_par_b        = 10,
    soilm_thetastar    = 0,
    soilm_betao        = 0,
    beta_unitcostratio = 50.0,
    rd_to_vcmax        = 0.01,
    tau_acclim         = 7.0,
    kc_jmax            = 0.2,
    error_gpp          = 0.01
  )

# upper bound
par_cal_max <- c(
    kphio              = 0.15,
    kphio_par_a        = -0.001,
    kphio_par_b        = 30,
    soilm_thetastar    = 240,
    soilm_betao        = 1,
    beta_unitcostratio = 200.0,
    rd_to_vcmax        = 0.1,
    tau_acclim         = 60.0,
    kc_jmax            = 0.8,
    error_gpp          = 4
  )

## ----eval = FALSE-------------------------------------------------------------
#  morris_setup <- BayesianTools::createBayesianSetup(
#    likelihood = ll_pmodel,
#    prior = BayesianTools::createUniformPrior(par_cal_min, par_cal_max, par_cal_best),
#    names = names(par_cal_best)
#  )

## ----eval = FALSE, echo = TRUE------------------------------------------------
#  set.seed(432)
#  morrisOut <- sensitivity::morris(
#    model = morris_setup$posterior$density,
#    factors = names(par_cal_best),
#    r = 1000,
#    design = list(type = "oat", levels = 20, grid.jump = 3),
#    binf = par_cal_min,
#    bsup = par_cal_max,
#    scale = TRUE)

## ----eval = FALSE, echo = FALSE-----------------------------------------------
#  # Save Morris sensitivity output because it takes very long to compute
#  save(morrisOut, file = "files/morrisOut.rda")

## ----eval = TRUE, echo = FALSE------------------------------------------------
# Load Morris sensitivity output
load("files/morrisOut.rda")

## ----eval = TRUE, fig.width=7, fig.height=4-----------------------------------
# Summarise the morris output
morrisOut.df <- data.frame(
  parameter = names(par_cal_best),
  mu.star = apply(abs(morrisOut$ee), 2, mean, na.rm = T),
  sigma = apply(morrisOut$ee, 2, sd, na.rm = T)
) %>%
  arrange( mu.star )

morrisOut.df |>
  tidyr::pivot_longer( -parameter, names_to = "variable", values_to = "value") |>
  ggplot(aes(
    reorder(parameter, value),
    value, 
    fill = variable),
    color = NA) +
  geom_bar(position = position_dodge(), stat = 'identity') +
  scale_fill_brewer("", labels = c('mu.star' = expression(mu * "*"),
                                   'sigma' = expression(sigma)),
                    palette = "Greys") +
  theme_classic() +
  theme(
    axis.text = element_text(size = 6),
    axis.title = element_blank(),
    legend.position = c(0.05, 0.95), legend.justification = c(0.05, 0.95)
  )


## ----eval = FALSE, echo = TRUE------------------------------------------------
#  set.seed(2023)
#  
#  # Define calibration settings
#  settings_calib <- list(
#    method = "BayesianTools",
#    metric = rsofun::cost_likelihood_pmodel,
#    control = list(
#      sampler = "DEzs",
#      settings = list(
#        burnin = 3000,
#        iterations = 9000,
#        nrChains = 3,        # number of independent chains
#        startValue = 3       # number of internal chains to be sampled
#      )),
#    par = list(
#      kphio = list(lower = 0.03, upper = 0.15, init = 0.05),
#      kphio_par_a = list(lower = -0.004, upper = -0.001, init = -0.0025),
#      kphio_par_b = list(lower = 10, upper = 30, init =25),
#      err_gpp = list(lower = 0.1, upper = 3, init = 0.8)
#    )
#  )
#  
#  # Calibrate kphio-related parameters and err_gpp
#  par_calib <- calib_sofun(
#    drivers = p_model_drivers,
#    obs = p_model_validation,
#    settings = settings_calib,
#    par_fixed = list(
#      soilm_thetastar    = 0.6*240,
#      soilm_betao        = 0.2,
#      beta_unitcostratio = 146.0,
#      rd_to_vcmax        = 0.014,
#      tau_acclim         = 30.0,
#      kc_jmax            = 0.41),
#    targets = "gpp"
#  )
#  
#  # This code takes 15 minutes to run

## ----eval = FALSE, echo = FALSE-----------------------------------------------
#  # Calibrates kphio, betao, kc_jmax - top 3 model params
#  set.seed(2023)
#  
#  # Define calibration settings
#  settings_calib <- list(
#    method = "BayesianTools",
#    metric = rsofun::cost_likelihood_pmodel,
#    control = list(
#      sampler = "DEzs",
#      settings = list(
#        burnin = 1500,
#        iterations = 6000,
#        nrChains = 3       # number of chains to be sampled
#      )),
#    par = list(
#      kphio = list(lower = 0.03, upper = 0.15, init = 0.05),
#      soilm_betao = list(lower = 0, upper = 1, init = 0.2),
#      kc_jmax = list(lower = 0.2, upper = 0.8, init = 0.41),
#      err_gpp = list(lower = 0.1, upper = 3, init = 0.8)
#    )
#  )
#  
#  par_calib <- calib_sofun(
#    drivers = p_model_drivers,
#    obs = p_model_validation,
#    settings = settings_calib,
#    par_fixed = list(
#      kphio_par_a = -0.0025,
#      kphio_par_b = 20,
#      soilm_thetastar    = 0.6*240,
#      beta_unitcostratio = 146.0,
#      rd_to_vcmax        = 0.014,
#      tau_acclim         = 30.0),
#    targets = "gpp"
#  )

## ----eval = FALSE, echo = FALSE-----------------------------------------------
#  # Save calibration output because it takes very long to compute
#  save(par_calib, file = "files/par_calib.rda")

## ----eval = TRUE, echo = FALSE------------------------------------------------
# Load calibration output
load("files/par_calib.rda")

## ----fig.height = 10, fig.width = 7-------------------------------------------
plot(par_calib$mod)

## ----fig.height = 10, fig.width = 7, eval = FALSE, echo = FALSE---------------
#  # Define function for plotting chains separately
#  plot_acf_mcmc <- function(chains, par_names){
#    # chains: from the BayesianTools output
#    n_chains <- length(chains)
#    par(mfrow = c(length(par_names), n_chains))
#    for(par_name in par_names){
#      for(i in 1:n_chains){
#        chains[[i]][, par_name] |>
#          pacf(main = paste0("Series of ", par_name, " , chain ", i))
#      }
#    }
#  }
#  
#  plot_acf_mcmc(par_calib$mod$chain, c("kphio", "kphio_par_a", "kphio_par_b", "err_gpp"))

## ----fig.width=5, fig.height=5------------------------------------------------
correlationPlot(par_calib$mod, thin = 1)   # use all samples, no thinning

## -----------------------------------------------------------------------------
gelmanDiagnostics(par_calib$mod)

## -----------------------------------------------------------------------------
summary(par_calib$mod)

## ----echo = TRUE, eval = FALSE------------------------------------------------
#  # Evaluation of the uncertainty coming from the model parameters' uncertainty
#  
#  # Sample parameter values from the posterior distribution
#  samples_par <- getSample(par_calib$mod,
#                               thin = 30,              # get every 30th sample
#                               whichParameters = 1:4) |>
#    as.data.frame() |>
#    dplyr::mutate(mcmc_id = 1:n()) |>
#    tidyr::nest(.by = mcmc_id, .key = "pars")
#  
#  run_pmodel <- function(sample_par){
#    # Function that runs the P-model for a sample of parameters
#    # and also adds the new observation error
#  
#    out <- runread_pmodel_f(
#      drivers = p_model_drivers,
#      par =  list(                      # copied from par_fixed above
#        kphio = sample_par$kphio,
#        kphio_par_a = sample_par$kphio_par_a,
#        kphio_par_b = sample_par$kphio_par_b,
#        soilm_thetastar    = 0.6*240,
#        soilm_betao        = 0.2,
#        beta_unitcostratio = 146.0,
#        rd_to_vcmax        = 0.014,
#        tau_acclim         = 30.0,
#        kc_jmax            = 0.41)       # value from posterior
#    )
#  
#    # return modelled GPP and prediction for a new GPP observation
#    gpp <- out$data[[1]][, "gpp"]
#    data.frame(gpp = gpp,
#               gpp_pred = gpp + rnorm(n = length(gpp), mean = 0,
#                                     sd = sample_par$err_gpp),
#               date = out$data[[1]][, "date"])
#  }
#  
#  set.seed(2023)
#  # Run the P-model for each set of parameters
#  pmodel_runs <- samples_par |>
#    dplyr::mutate(sim = purrr::map(pars, ~run_pmodel(.x))) |>
#    # format to obtain 90% credible intervals
#    dplyr::select(mcmc_id, sim) |>
#    tidyr::unnest(sim) |>
#    dplyr::group_by(date) |>
#    # compute quantiles for each day
#    dplyr::summarise(
#      gpp_q05 = quantile(gpp, 0.05, na.rm = TRUE),
#      gpp = quantile(gpp, 0.5, na.rm = TRUE),          # get median
#      gpp_q95 = quantile(gpp, 0.95, na.rm = TRUE),
#      gpp_pred_q05 = quantile(gpp_pred, 0.05, na.rm = TRUE),
#      gpp_pred_q95 = quantile(gpp_pred, 0.95, na.rm = TRUE)
#    )

## ----eval = FALSE, echo = FALSE-----------------------------------------------
#  save(pmodel_runs, file = "files/pmodel_runs.rda")

## ----echo = FALSE, eval = TRUE------------------------------------------------
load("files/pmodel_runs.rda")

## ----fig.width=7, fig.height=5------------------------------------------------
# Plot the credible intervals computed above
# for the first year only
plot_gpp_error <- ggplot(data = pmodel_runs |>
    dplyr::slice(1:365)) +             # Plot only first year
  geom_ribbon(
    aes(ymin = gpp_q05, 
        ymax = gpp_q95,
        x = date),
    fill = 'blue', alpha = 0.5) +
  geom_ribbon(
    aes(ymin = gpp_pred_q05, 
        ymax = gpp_pred_q95,
        x = date),
    fill = 'grey40', alpha = 0.2) +
  geom_line(
    aes(
      date,
      gpp
    ),
    colour = "grey40",
    alpha = 0.8
  ) +
  theme_classic() +
  theme(panel.grid.major.y = element_line()) +
  labs(
    x = 'Date',
    y = expression(paste("GPP (g C m"^-2, "s"^-1, ")"))
  )

# Define GPP validation data (first year)
validation_data <- p_model_validation$data[[1]][1:365, ]

# Include observations in the plot
plot_gpp_error +  
  geom_line(
    data = validation_data,
    aes(
      date,
      gpp
    ),
    alpha = 0.8
  )

## ----fig.width=7, fig.height=5, echo = FALSE, eval = FALSE--------------------
#  #
#  # Plot observed and predicted GPP, with a 95% confidence interval using err_gpp
#  plot_gpp_error <- ggplot(data = runread_pmodel_f(
#      drivers = p_model_drivers,
#      par =  list(
#        kphio = par_calib$par[1],        # values from posterior
#        kphio_par_a = par_calib$par[2],
#        kphio_par_b = par_calib$par[3],
#        soilm_thetastar    = 0.6*240,    # copied from par_fixed above
#        soilm_betao        = 0.2,
#        beta_unitcostratio = 146.0,
#        rd_to_vcmax        = 0.014,
#        tau_acclim         = 30.0,
#        kc_jmax            = 0.41)
#    ) |>
#      dplyr::select(sitename, data) |>
#      tidyr::unnest(data) |>
#      dplyr::slice(1:365)) +             # Plot only first year
#    geom_ribbon(
#      aes(ymin = gpp - 2*par_calib$par[4],
#          ymax = gpp + 2*par_calib$par[4],
#          x = date),
#      fill = 'grey40', alpha = 0.2) +
#    geom_line(
#      aes(
#        date,
#        gpp
#      ),
#      colour = "grey40",
#      alpha = 0.8
#    ) +
#    theme_classic() +
#    theme(panel.grid.major.y = element_line()) +
#    labs(
#      x = 'Date',
#      y = expression(paste("GPP (g C m"^-2, "s"^-1, ")"))
#    )
#  
#  # Define GPP validation data (first year)
#  validation_data <- p_model_validation$data[[1]][1:365, ]
#  
#  # Include observations in the plot
#  plot_gpp_error +
#    geom_line(
#      data = validation_data,
#      aes(
#        date,
#        gpp
#      ),
#      alpha = 0.8
#    )


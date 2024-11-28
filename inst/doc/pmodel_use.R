## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  fig.align = "center",
  fig.width = 7,
  fig.height = 5
)

library(rsofun)
library(dplyr)
library(ggplot2)

## -----------------------------------------------------------------------------
library(rsofun)

# this is to deal with an error p_model_drivers.rds not being found 
p_model_drivers

p_model_validation

## -----------------------------------------------------------------------------
p_model_drivers_vcmax25

p_model_validation_vcmax25

## -----------------------------------------------------------------------------
# define model parameter values from previous
# work
params_modl <- list(
    kphio              = 0.04998,    # setup ORG in Stocker et al. 2020 GMD
    kphio_par_a        = 0.0,        # set to zero to disable temperature-dependence of kphio
    kphio_par_b        = 1.0,
    soilm_thetastar    = 0.6 * 240,  # to recover old setup with soil moisture stress
    soilm_betao        = 0.0,
    beta_unitcostratio = 146.0,
    rd_to_vcmax        = 0.014,      # value from Atkin et al. 2015 for C3 herbaceous
    tau_acclim         = 30.0,
    kc_jmax            = 0.41
  )

# run the model for these parameters
output <- rsofun::runread_pmodel_f(
  p_model_drivers,
  par = params_modl
  )

## -----------------------------------------------------------------------------
# Load libraries for plotting
library(dplyr)
library(tidyr)
library(ggplot2)

# Create data.frame for plotting
df_gpp_plot <- rbind(
  output |>
    filter(sitename == "FR-Pue") |>
    unnest(data) |>
    select(date, gpp) |>
    mutate(type = "P-model output"),
  p_model_validation |>
    filter(sitename == "FR-Pue") |>
    unnest(data) |>
    select(date, gpp) |>
    mutate(type = "Observed")
)
df_gpp_plot$type <- factor(df_gpp_plot$type,
                           levels = c('P-model output',
                                      'Observed'))

# Plot GPP
ggplot(data = df_gpp_plot) +
  geom_line(
    aes(x = date,
        y = gpp,
        color = type),
    alpha = 0.7
  ) +
  scale_color_manual(values = c(
    'P-model output'='grey70',
    'Observed'='black')) +
  theme_classic() +
  theme(panel.grid.major.y = element_line()) +
  labs(
    x = 'Date',
    y = expression(paste("GPP (g C m"^-2, "s"^-1, ")")),
    colour = ""
  )

## -----------------------------------------------------------------------------
settings <- list(
  method              = "GenSA",
  metric              = cost_rmse_pmodel,
  control = list(
    maxit = 100),
  par = list(
    kphio = list(lower=0.02, upper=0.2, init = 0.05)
    )
)

## ----eval=FALSE---------------------------------------------------------------
#  # calibrate the model and optimize free parameters
#  pars <- calib_sofun(
#      drivers = p_model_drivers,
#      obs = p_model_validation,
#      settings = settings,
#      # extra arguments passed to the cost function:
#      targets = "gpp",             # define target variable GPP
#      par_fixed = params_modl[-1]  # fix non-calibrated parameters to previous
#                                   # values, removing kphio
#    )

## ----simulate_calibration_run, include = FALSE--------------------------------
# fake variable as optimization isn't run
pars <- list()
pars$par["kphio"] <- 0.03580962

## -----------------------------------------------------------------------------
# Update the parameter list with calibrated value
params_modl$kphio <- pars$par["kphio"]

# Run the model for these parameters
output_new <- rsofun::runread_pmodel_f(
  p_model_drivers,
  par = params_modl
  )

# Update data.frame for plotting
df_gpp_plot <- rbind(
  df_gpp_plot,
  output_new |>
    filter(sitename == "FR-Pue") |>
    unnest(data) |>
    select(date, gpp) |>
    mutate(type = "P-model output (calibrated)")
)
df_gpp_plot$type <- factor(df_gpp_plot$type,
                           levels = c('P-model output',
                                      'P-model output (calibrated)',
                                      'Observed'))

# Plot GPP
ggplot(data = df_gpp_plot) +
  geom_line(
    aes(x = date,
        y = gpp,
        color = type),
    alpha = 0.7
  ) +
  scale_color_manual(values = c(
    'P-model output'='grey70',
    'P-model output (calibrated)'='grey40',
    'Observed'='black')) +
  theme_classic() +
  theme(panel.grid.major.y = element_line()) +
  labs(
    x = 'Date',
    y = expression(paste("GPP (g C m"^-2, "s"^-1, ")")),
    colour = ""
  )


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
# call to the included p-model demo drivers
rsofun::p_model_drivers

# call to the included BiomeE (p-model) demo drivers
rsofun::biomee_p_model_drivers

# call to the included BiomeE (gs leuning) demo drivers
rsofun::biomee_gs_leuning_drivers

## -----------------------------------------------------------------------------
# Accessing the site information for the first site
rsofun::biomee_gs_leuning_drivers$site_info[[1]]

## -----------------------------------------------------------------------------
# Detailed look at the forcing data for the P-model
rsofun::p_model_drivers$forcing[[1]]


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
# call to the included p-model demo data
rsofun::p_model_drivers

## -----------------------------------------------------------------------------
# detailed look at the forcing data
rsofun::p_model_drivers$forcing


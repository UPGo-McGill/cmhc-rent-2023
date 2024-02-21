#### 01 STARTUP ################################################################


# Optionally install packages from GitHub ---------------------------------

# remotes::install_github("UPGo-McGill/upgo")
# remotes::install_github("UPGo-McGill/strr")
# remotes::install_github("UPGo-McGill/matchr")


# Load packages -----------------------------------------------------------

library(tidyverse)
library(upgo)
library(strr)
library(sf)
library(mapview)
library(future)
library(progressr)
library(slider)
library(patchwork)
library(qs)
library(cancensus)
library(tsibble)
plan(multisession)
handlers(global = TRUE)


# Set global variables ----------------------------------------------------

col_palette <-
  c("#B8D6BE", "#73AE80", "#B5C0DA", "#6C83B5", "#2A5A5B", "#B58A6C", "#5B362A",
    "#AE7673")

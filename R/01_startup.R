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

# col_palette <-
#   c("#B8D6BE", "#73AE80", "#B5C0DA", "#6C83B5", "#2A5A5B", "#B58A6C", "#5B362A",
#     "#AE7673")

impute <- function(x) {
  x |> 
    inner_join(monthly_impute, by = c("id", "year", "rent", "universe")) |> 
    mutate(rent = coalesce(rent, rent_new), 
           universe = coalesce(universe, univ_new)) |> 
    select(-rent_new, -univ_new)
}

mutate_region <- function(x) {
  x |> 
    mutate(region = case_when(
      province %in% c("British Columbia", "Ontario", "Québec", "Quebec") ~ 
        province, 
      province %in% c("Alberta", "Saskatchewan", "Manitoba") ~ "Prairies",
      province %in% c("New Brunswick", "Nova Scotia", "Prince Edward Island", 
                      "Newfoundland and Labrador") ~ "Atlantic"))
}

mutate_days <- function(x) {
  x |> 
    mutate(days = days_in_month(month(as.Date(month))))
}

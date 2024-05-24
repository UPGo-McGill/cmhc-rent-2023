#### 01 STARTUP ################################################################


# Optionally install packages from GitHub ---------------------------------

# remotes::install_github("UPGo-McGill/upgo")
# remotes::install_github("UPGo-McGill/strr")
# remotes::install_github("UPGo-McGill/matchr")
# remotes::install_github("jtextor/dagitty/r")


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
library(spmoran)
library(spdep)
library(ggfortify)
library(modelsummary)
library(dagitty)
library(brms)
library(bayesplot)
plan(multisession)
handlers(global = TRUE)


# Set global variables ----------------------------------------------------

# col_palette <-
#   c("#B8D6BE", "#73AE80", "#B5C0DA", "#6C83B5", "#2A5A5B", "#B58A6C", "#5B362A",
#     "#AE7673")

impute <- function(x) {
  x |> 
    inner_join(monthly_impute, by = c("id", "year", "rent", "universe", 
                                      "vacancy")) |> 
    mutate(rent = coalesce(rent, rent_new), 
           universe = coalesce(universe, univ_new),
           vacancy = coalesce(vacancy, vac_new)) |> 
    arrange(id, year) |> 
    mutate(rent_lag = lag(rent), .by = id) |> 
    mutate(rent_change_new = slide_dbl(rent, \(x) x[2] - x[1], .before = 1, 
                                       .complete = TRUE), .by = id) |> 
    mutate(rent_change = coalesce(rent_change, rent_change_new)) |> 
    mutate(univ_change_new = slide_dbl(universe, \(x) x[2] - x[1], .before = 1, 
                                       .complete = TRUE), .by = id) |> 
    mutate(universe_change = coalesce(universe_change, univ_change_new)) |> 
    mutate(vac_change_new = slide_dbl(vacancy, \(x) x[2] - x[1], .before = 1, 
                                      .complete = TRUE), .by = id) |> 
    mutate(vacancy_change = coalesce(vacancy_change, vac_change_new)) |> 
    select(-rent_new, -univ_new, -vac_new, -rent_change_new, -univ_change_new,
           -vac_change_new)
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

model_year <- function(start_year, end_year, ..., by_year = FALSE, 
                       use_FREH = TRUE, use_rev = TRUE, use_dummy = TRUE,
                       model = "sf") {
  
  # Setup
  stopifnot(exists("cmhc"))
  stopifnot(exists("cmhc_nbhd"))
  stopifnot(exists("mr"))
  mr <- get("mr")
  x <- dr$impute
  
  # Get coefficients
  if (by_year) {
    
    if (model == "l") {
      
      coefs <- map(mr[["l_year_both"]], \(x) {
        y <- summary(x)$coefficients
        tibble(
          effect_FREH = y[["FREH_log", "Estimate"]],
          effect_FREH_dummy = y[["FREH_dummyTRUE", "Estimate"]],
          effect_rev = y[["rev_log", "Estimate"]],
          effect_rev_dummy = y[["rev_dummyTRUE", "Estimate"]])}) |> 
        bind_rows() |> 
        mutate(year = as.character(2016:2022), .before = effect_FREH)
      
    } else if (model == "sf") {
      
      coefs <- map(mr[["sf_year"]], \(x) {
        y <- x$b
        tibble(
          effect_FREH = y[["FREH_log", "Estimate"]],
          effect_FREH_dummy = y[["FREH_dummy", "Estimate"]],
          effect_rev = y[["rev_log", "Estimate"]],
          effect_rev_dummy = y[["rev_dummy", "Estimate"]])}) |> 
        bind_rows() |> 
        mutate(year = as.character(2016:2022), .before = effect_FREH)
      
    } else stop("Unsupported model")
    
  } else {
    
    if (model == "l") {
      
      y <- summary(mr[["l_both"]])$coefficients
      coefs <- tibble(
        effect_FREH = y[["FREH_log", "Estimate"]],
        effect_FREH_dummy = y[["FREH_dummyTRUE", "Estimate"]],
        effect_rev = y[["rev_log", "Estimate"]],
        effect_rev_dummy = y[["rev_dummyTRUE", "Estimate"]])
      
    } else if (model == "sf") {
      
      y <- mr[["sf_both"]]$b
      coefs <- tibble(
        effect_FREH = y[["FREH_log", "Estimate"]],
        effect_FREH_dummy = y[["FREH_dummy", "Estimate"]],
        effect_rev = y[["rev_log", "Estimate"]],
        effect_rev_dummy = y[["rev_dummy", "Estimate"]])
      
    } else stop("Unsupported model")
    
  }
  
  # Get rent change from CMHC
  rent_change <- 
    cmhc |> 
    left_join(cmhc_nbhd, by = "id") |> 
    group_by(year, ...) |> 
    summarize(total_rent = sum(rent * tenant_count, na.rm = TRUE),
              .groups = "drop") |> 
    arrange(..., year) |> 
    group_by(...) |> 
    mutate(dif = slide_dbl(total_rent, \(x) x[2] - x[1], .before = 1)) |> 
    ungroup() |> 
    group_by(...) |> 
    summarize(j = "j",
              total_rent_dif = total_rent[year == end_year] - 
                total_rent[year == start_year], 
              .groups = "drop")
  
  # Apply treatment
  x1 <- 
    x |> 
    arrange(id, year) |> 
    mutate(FREH_log_c = FREH_log[year == start_year],
           rev_log_c = rev_log[year == start_year],
           FREH_dummy_c = FREH_dummy[year == start_year],
           rev_dummy_c = rev_dummy[year == start_year],
           .by = id, .after = rev) |> 
    filter(year == end_year) |> 
    select(id:province, rent_raw, rent_log, rent_log_raw, FREH_log, FREH_log_c, 
           rev_log, rev_log_c, FREH_dummy, FREH_dummy_c, rev_dummy, 
           rev_dummy_c)
  
  # Join to coefficients
  if (by_year) {
    x2 <- inner_join(x1, coefs, by = "year")
  } else x2 <- bind_cols(x1, coefs)
  
  # Calculate effects
  x3 <- 
    x2 |> 
    mutate(
      effect_real = 
        FREH_log * effect_FREH * use_FREH +
        FREH_dummy * effect_FREH_dummy * use_FREH * use_dummy +
        rev_log * effect_rev * use_rev +
        rev_dummy * effect_rev_dummy * use_rev * use_dummy,
      effect_c = 
        FREH_log_c * effect_FREH * use_FREH +
        FREH_dummy_c * effect_FREH_dummy * use_FREH * use_dummy +
        rev_log_c * effect_rev * use_rev +
        rev_dummy_c * effect_rev_dummy * use_rev * use_dummy)
  
  # Produce output
  x3 |> 
    mutate(
      dif = effect_c - effect_real, 
      rent_c = rent_log + dif,
      rent_cs = rent_c * sd(x$rent_log_raw) + mean(x$rent_log_raw),
      rent_raw_c = exp(rent_cs),
      .after = rent_log_raw) |> 
    select(id:rent_raw, rent_raw_c) |> 
    st_drop_geometry() |> 
    left_join(select(cmhc_nbhd, id, tenant_count), by = "id") |> 
    select(-geometry) |> 
    mutate(total_rent = rent_raw * tenant_count,
           total_rent_c = rent_raw_c * tenant_count) |> 
    group_by(...) |> 
    summarize(
      j = "j",
      total_rent = sum(total_rent),
      total_rent_c = sum(total_rent_c),
      dif = total_rent - total_rent_c,
      dif_pct = dif / total_rent, .groups = "drop") |> 
    left_join(rent_change) |> 
    mutate(rent_change_pct = dif / total_rent_dif) |> 
    mutate(start_year = start_year,
           end_year = end_year, .before = total_rent) |> 
    select(-j)
  
}

model_change_manual <- function(target_year, change_FREH, change_rev, ..., 
                                by_year = FALSE, use_FREH = TRUE, 
                                use_rev = TRUE, model = "sf") {
  
  # Setup
  stopifnot(exists("cmhc"))
  stopifnot(exists("cmhc_nbhd"))
  stopifnot(exists("mr"))
  mc <- get("mc")
  x <- 
    dc$impute |> 
    st_drop_geometry() |> 
    select(id:rev_change, rent_change_raw:rev_change_raw)
  
  # Get coefficients
  if (by_year) {
    
    if (model == "l") {
      
      coefs <- map(mc[["l_year_both"]], \(x) {
        y <- summary(x)$coefficients
        tibble(
          effect_FREH = y[["FREH_change", "Estimate"]],
          effect_rev = y[["rev_change", "Estimate"]])}) |> 
        bind_rows() |> 
        mutate(year = as.character(2017:2022), .before = effect_FREH)
      
    } else if (model == "sf") {
      
      coefs <- map(mc[["sf_year"]], \(x) {
        y <- x$b
        tibble(
          effect_FREH = y[["FREH_change", "Estimate"]],
          effect_rev = y[["rev_change", "Estimate"]])}) |> 
        bind_rows() |> 
        mutate(year = as.character(2017:2022), .before = effect_FREH)
      
    } else stop("Unsupported model")
    
  } else {
    
    if (model == "l") {
      
      y <- summary(mc[["l_both"]])$coefficients
      coefs <- tibble(
        effect_FREH = y[["FREH_change", "Estimate"]],
        effect_rev = y[["rev_change", "Estimate"]])
      
    } else if (model == "sf") {
      
      y <- mc[["sf_both"]]$b
      coefs <- tibble(
        effect_FREH = y[["FREH_change", "Estimate"]],
        effect_rev = y[["rev_change", "Estimate"]])
      
    } else stop("Unsupported model")
    
  }
  
  # Get rent change from CMHC
  rent_change <- 
    cmhc |> 
    left_join(cmhc_nbhd, by = "id") |> 
    group_by(year, ...) |> 
    summarize(total_rent = sum(rent * tenant_count, na.rm = TRUE),
              .groups = "drop") |> 
    arrange(..., year) |> 
    group_by(...) |> 
    mutate(dif = slide_dbl(total_rent, \(x) x[2] - x[1], .before = 1)) |> 
    ungroup() |> 
    group_by(...) |> 
    summarize(j = "j", total_rent_dif = dif[year == target_year], 
              .groups = "drop")
  
  # Apply treatment
  x1 <- 
    x |> 
    # Filter before applying treatment, so input can be, e.g. a vector of 
    # previous year's values
    filter(year == target_year) |> 
    mutate(FREH_change_c = change_FREH,
           rev_change_c = change_rev) |> 
    select(id:province, rent_change_raw, rent_change, rent_change_raw, 
           FREH_change, FREH_change_c, rev_change, rev_change_c)
  
  # Join to coefficients
  if (by_year) {
    x2 <- inner_join(x1, coefs, by = "year")
  } else x2 <- bind_cols(x1, coefs)
  
  # Calculate effects
  x3 <- 
    x2 |> 
    mutate(
      effect_real = 
        FREH_change * effect_FREH * use_FREH +
        rev_change * effect_rev * use_rev,
      effect_c = 
        FREH_change_c * effect_FREH * use_FREH +
        rev_change_c * effect_rev * use_rev)
  
  # Produce output
  x3 |> 
    select(-FREH_change_c, -rev_change_c, -effect_FREH, -effect_rev) |> 
    mutate(
      dif = effect_c - effect_real, 
      rent_change_c = rent_change + dif,
      rent_change_raw_c = rent_change_c * sd(x$rent_change_raw) + 
        mean(x$rent_change_raw),
      .after = rent_change_raw) |> 
    select(id:rent_change_raw, rent_change_raw_c) |> 
    left_join(select(cmhc_nbhd, id, tenant_count), by = "id") |> 
    select(-geometry) |> 
    mutate(total_change = rent_change_raw * tenant_count,
           total_change_c = rent_change_raw_c * tenant_count) |> 
    group_by(...) |>
    summarize(
      j = "j",
      total_change = sum(total_change),
      total_change_c = sum(total_change_c),
      dif = total_change - total_change_c,
      dif_pct = dif / total_change, .groups = "drop") |> 
    left_join(rent_change) |> 
    mutate(rent_change_pct = dif / total_rent_dif) |> 
    mutate(year = target_year, .before = total_change) |> 
    select(-j)
  
}

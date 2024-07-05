#### 01 STARTUP ################################################################


# Optionally install packages from GitHub ---------------------------------

# remotes::install_github("UPGo-McGill/upgo")
# remotes::install_github("UPGo-McGill/strr")
# remotes::install_github("UPGo-McGill/matchr")
# remotes::install_github("jtextor/dagitty/r")


# Load packages -----------------------------------------------------------

suppressPackageStartupMessages({
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
  library(ggdag)
  library(magick)
  library(did)
})

plan(multisession)
handlers(global = TRUE)


# Helper functions --------------------------------------------------------

impute <- function(x) {
  x |> 
    inner_join(monthly_impute, by = c("id", "year", "rent", "universe", 
                                      "vacancy")) |> 
    mutate(rent = coalesce(rent, rent_new), 
           universe = coalesce(universe, univ_new),
           vacancy = coalesce(vacancy, vac_new)) |> 
    arrange(id, year) |> 
    mutate(rent_lag = lag(rent), .by = id) |> 
    mutate(vacancy_lag = lag(vacancy), .by = id) |> 
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

model_change <- function(target_year, ..., change_FREH = 0, 
                         change_non_FREH = 0, change_price = 0, 
                         use_FREH = TRUE, use_non_FREH = TRUE, 
                         use_price = TRUE, whole_year = FALSE, 
                         model = "common.1") {
  
  # Setup
  stopifnot(exists("cmhc"))
  stopifnot(exists("cmhc_nbhd"))
  stopifnot(exists("mc"))
  stopifnot(exists("monthly_sept"))
  mc <- get("mc")
  
  x <- 
    dc$main |> 
    st_drop_geometry() |> 
    select(id:province, rent_change, FREH_change, non_FREH_change, price_change,
           rent_change_raw, FREH_change_raw, non_FREH_change_raw, 
           price_change_raw, rent_lag_raw)
  
  # Get coefficients
  y <- mc[[model]]$b
  
  coefs <- tibble(
    effect_FREH = y[["FREH_change", "Estimate"]],
    effect_non_FREH = y[["non_FREH_change", "Estimate"]],
    effect_price = y[["price_change", "Estimate"]])
  
  # Get tenant counts
  tenant_count <- 
    monthly_sept |> 
    st_drop_geometry() |> 
    # Impute missing values
    impute() |> 
    # Update tenant_count to reflect trend in universe
    mutate(tenant_count = universe * tenant_count[year == 2021] / 
             universe[year == 2021], .by = id) |> 
    select(id:province, rent, tenant_count)
  
  # Get lagged tenant counts
  tenant_count_lag <- 
    monthly_sept |> 
    st_drop_geometry() |> 
    # Impute missing values
    impute() |> 
    # Update tenant_count to reflect trend in universe
    mutate(tenant_count_lag = universe * tenant_count[year == 2021] / 
             universe[year == 2021], .by = id) |> 
    select(id, year, tenant_count_lag) |> 
    mutate(year = year + 1)
  
  # Drop additional columns from tenant_count
  tenant_count <- 
    tenant_count |> 
    select(id, year, rent, tenant_count)
  
  # Apply treatment
  x1 <- 
    x |> 
    # Filter before applying treatment, so input can be, e.g. a vector of 
    # previous year's values
    filter(year == target_year) |> 
    mutate(FREH_change_c = change_FREH,
           non_FREH_change_c = change_non_FREH,
           price_change_c = change_price) |> 
    select(id:province, rent_change_raw, rent_change, FREH_change, 
           FREH_change_c, non_FREH_change, non_FREH_change_c, price_change, 
           price_change_c, rent_lag_raw) |> 
    # Standardize treatment values
    mutate(FREH_change_c = (FREH_change_c - mean(x$FREH_change_raw)) / 
             sd(x$FREH_change_raw),
           non_FREH_change_c = (non_FREH_change_c - 
                                  mean(x$non_FREH_change_raw)) / 
             sd(x$non_FREH_change_raw),
           price_change_c = (price_change_c - mean(x$price_change_raw)) / 
             sd(x$price_change_raw))
  
  # Join to coefficients
  x2 <- bind_cols(x1, coefs)
  
  # Calculate effects
  x3 <- 
    x2 |> 
    mutate(
      effect_real = 
        FREH_change * effect_FREH * use_FREH +
        non_FREH_change * effect_non_FREH * use_non_FREH +
        price_change * effect_price * use_price,
      effect_c = 
        FREH_change_c * effect_FREH * use_FREH +
        non_FREH_change_c * effect_non_FREH * use_non_FREH +
        price_change_c * effect_price * use_price)
  
  # Produce output
  x4 <-
    x3 |> 
    select(-FREH_change_c, -non_FREH_change_c, -price_change_c, -effect_FREH, 
           -effect_non_FREH, -effect_price) |> 
    mutate(
      dif = effect_c - effect_real, 
      rent_change_c = rent_change + dif,
      rent_change_raw_c = rent_change_c * sd(x$rent_change_raw) + 
        mean(x$rent_change_raw),
      .after = rent_change_raw) |> 
    select(id:rent_change_raw, rent_change_raw_c, rent_lag_raw) |> 
    left_join(tenant_count, by = c("id", "year")) |> 
    left_join(tenant_count_lag, by = c("id", "year")) |> 
    mutate(total_before = rent_lag_raw * tenant_count_lag,
           total_before_adj = rent_lag_raw * tenant_count,
           total_after = (rent_lag_raw + rent_change_raw) * tenant_count,
           total_after_c = (rent_lag_raw + rent_change_raw_c) * tenant_count) |> 
    group_by(...) |>
    summarize(
      j = "j",
      total_before = sum(total_before),
      total_before_adj = sum(total_before_adj),
      total_after = sum(total_after),
      total_after_c = sum(total_after_c), 
      total_change = total_after - total_before_adj,
      total_change_non_adj = total_after - total_before,
      str_share = total_after - total_after_c,
      str_pct = str_share / total_change,
      str_pct_non_adj = str_share / total_change_non_adj, 
      .groups = "drop") |> 
    mutate(year = target_year, .before = total_before) |> 
    select(-j)
  
  # Multiply by 12 if whole_year is TRUE
  if (whole_year) x4 <- x4 |> 
    mutate(across(-c(year, str_pct), \(x) x * 12))
  
  x4
  
}

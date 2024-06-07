#### 08 DATA FOR MODELS ########################################################

source("R/01_startup.R")
monthly_sept <- qread("output/monthly_sept.qs")
qload("output/cmhc.qsm", nthreads = availableCores())


# Impute missing rent values for spatial panel ----------------------------

source("R/06_imputation.R")


# Produce dataset with rent_change as DV ----------------------------------

dc <- list()
change_vars <- c("id", "year", "CMA", "name_CMA", "province", "rent", 
                 "rent_change", "rent_lag", "FREH_change", "FREH_lag", 
                 "non_FREH_change", "non_FREH_lag", "price_change", "price_lag", 
                 "apart", "income", "tourism", "vacancy_lag", "tenant_count",
                 "universe", "universe_change")

# Main: imputed with vacancy_lag_log
dc$main <-
  monthly_sept |> 
  # Impute missing values
  impute() |> 
  # Filter to only complete observations
  filter(!is.na(rent_change), !is.na(FREH_change), year >= 2017) |> 
  # Select relevant variables
  select(all_of(change_vars)) |> 
  # Update tenant_count to reflect trend in universe
  mutate(tenant_count = universe * tenant_count[year == 2021] / 
           universe[year == 2021], .by = id) |> 
  # Replace zero FREH/non_FREH/price/vacancy with lowest non-zero values
  mutate(across(c(FREH_lag, non_FREH_lag, price_lag, vacancy_lag), 
                list(dummy = \(x) x == 0)), .before = rent) |> 
  mutate(across(c(FREH_lag, non_FREH_lag, price_lag, vacancy_lag), 
                \(x) if_else(x == 0, min(x[x > 0]), x))) |> 
  # Create logged versions of variables
  mutate(across(c(rent_lag, FREH_lag, non_FREH_lag, price_lag, vacancy_lag, 
                  apart, income, tourism), .fns = list(log = \(x) log(x))),
         .before = geometry) |> 
  # Normalize all variables
  mutate(across(c(rent:tourism_log), list(raw = \(x) x)),
         across(c(rent:tourism_log), \(x) as.numeric(scale(x))), 
         .before = geometry)

# No log
dc$no_log <- 
  monthly_sept |> 
  # Impute missing values
  impute() |> 
  # Filter to only complete observations
  filter(!is.na(rent_change), !is.na(FREH_change), year >= 2017) |> 
  # Select relevant variables
  select(all_of(change_vars)) |> 
  # Replace zero FREH/non_FREH/price with lowest non-zero values
  mutate(across(c(FREH_lag, non_FREH_lag, price_lag), 
                list(dummy = \(x) x == 0)), .before = rent_change) |> 
  mutate(across(c(FREH_lag, non_FREH_lag, price_lag), 
                \(x) if_else(x == 0, min(x[x > 0]), x))) |> 
  # Create logged versions of variables
  mutate(across(c(rent_lag, FREH_lag, non_FREH_lag, price_lag, apart, income, 
                  tourism), .fns = list(log = \(x) log(x))), 
         .before = geometry) |> 
  # Normalize all variables
  mutate(across(c(rent_change:tourism_log), list(raw = \(x) x)),
         across(c(rent_change:tourism_log), \(x) as.numeric(scale(x))),
         .before = geometry)

# No imputation
dc$no_imp <-
  monthly_sept |>
  # Filter to only complete observations
  filter(!is.na(rent_change), !is.na(rent_lag), !is.na(FREH_change), 
         !is.na(vacancy_lag), year >= 2017) |> 
  # Select relevant variables
  select(all_of(change_vars)) |> 
  # Replace zero FREH/non_FREH/price/vacancy with lowest non-zero values
  mutate(across(c(FREH_lag, non_FREH_lag, price_lag, vacancy_lag), 
                list(dummy = \(x) x == 0)), .before = rent_change) |> 
  mutate(across(c(FREH_lag, non_FREH_lag, price_lag, vacancy_lag), 
                \(x) if_else(x == 0, min(x[x > 0]), x))) |> 
  # Create logged versions of variables
  mutate(across(c(rent_lag, FREH_lag, non_FREH_lag, price_lag, vacancy_lag, 
                  apart, income, tourism), .fns = list(log = \(x) log(x))),
         .before = geometry) |> 
  # Normalize all variables
  mutate(across(c(rent_change:tourism_log), list(raw = \(x) x)),
         across(c(rent_change:tourism_log), \(x) as.numeric(scale(x))),
         .before = geometry)

# No vacancy
dc$no_vac <-
  monthly_sept |> 
  filter(!is.na(rent_change), !is.na(rent_lag), !is.na(FREH_change), 
         year >= 2017) |> 
  # Select relevant variables
  select(all_of(change_vars)) |> 
  # Replace zero FREH/non_FREH/price with lowest non-zero values
  mutate(across(c(FREH_lag, non_FREH_lag, price_lag), 
                list(dummy = \(x) x == 0)), .before = rent_change) |> 
  mutate(across(c(FREH_lag, non_FREH_lag, price_lag), 
                \(x) if_else(x == 0, min(x[x > 0]), x))) |> 
  # Create logged versions of variables
  mutate(across(c(rent_lag, FREH_lag, non_FREH_lag, price_lag, apart, income, 
                  tourism), .fns = list(log = \(x) log(x))),
         .before = geometry) |> 
  # Normalize all variables
  mutate(across(c(rent_change:tourism_log), list(raw = \(x) x)),
         across(c(rent_change:tourism_log), \(x) as.numeric(scale(x))),
         .before = geometry)


# Produce DiD dataset -----------------------------------------------------

dr <- list()

dr$main <- 
  monthly_sept |> 
  impute() |> 
  st_drop_geometry() |> 
  filter(!is.na(rent)) |> 
  select(id:province, rent, FREH, non_FREH, price, vacancy) |>
  mutate(across(c(FREH:vacancy), \(x) if_else(
    x == 0, min(x[x > 0], na.rm = TRUE), x))) |> 
  mutate(across(c(rent:vacancy), .fns = list(log = \(x) log(x)))) |> 
  mutate(across(c(rent:vacancy_log), \(x) as.numeric(scale(x))))

dr$no_imp <- 
  monthly_sept |> 
  st_drop_geometry() |> 
  filter(!is.na(rent)) |> 
  select(id:province, rent, FREH, non_FREH, price) |>
  mutate(across(c(FREH:price), \(x) if_else(
    x == 0, min(x[x > 0], na.rm = TRUE), x))) |> 
  mutate(across(c(rent:price), .fns = list(log = \(x) log(x)))) |> 
  mutate(across(c(rent:price_log), \(x) as.numeric(scale(x))))


# Clean up ----------------------------------------------------------------

rm(cmhc, cmhc_nbhd, monthly_impute, change_vars)

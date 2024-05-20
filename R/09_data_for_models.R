#### 09 DATA FOR MODELS ########################################################

source("R/01_startup.R")
monthly_sept <- qread("output/monthly_sept.qs")
qload("output/cmhc.qsm", nthreads = availableCores())


# Impute missing rent values for spatial panel ----------------------------

source("R/07_imputation.R")


# Produce dataset with rent as DV -----------------------------------------

dr <- list()
rent_vars <- c("id", "year", "CMA", "name_CMA", "province", "rent", "rent_lag", 
               "FREH", "FREH_lag", "non_FREH", "non_FREH_lag", "rev", "rev_lag", 
               "price", "price_lag", "tenant", "apart", "income", "tourism", 
               "vacancy", "universe_change")

# Main dataset: rent, rent_lag, FREH_lag, rev_lag, price_lag
dr$main <-
  monthly_sept |> 
  filter(!is.na(rent), !is.na(rent_lag), !is.na(FREH_lag), !is.na(rev_lag), 
         !is.na(price_lag)) |> 
  # Select relevant variables
  select(all_of(rent_vars)) |> 
  # Make year a character vector so it is treated as a factor
  mutate(year = as.character(year)) |> 
  # Replace zero-values of FREH/rev/price with lowest non-zero values
  mutate(FREH_lag_dummy = FREH_lag == 0, 
         non_FREH_lag_dummy = non_FREH_lag == 0, 
         rev_lag_dummy = rev_lag == 0, 
         .before = rent) |> 
  mutate(FREH_lag = if_else(FREH_lag == 0, min(FREH_lag[FREH_lag > 0]), 
                            FREH_lag),
         non_FREH_lag = if_else(non_FREH_lag == 0, min(
           non_FREH_lag[non_FREH_lag > 0]), non_FREH_lag),
         rev_lag = if_else(rev_lag == 0, min(rev_lag[rev_lag > 0]), rev_lag),
         price_lag = if_else(price_lag == 0, min(price_lag[price_lag > 0]), 
                             price_lag)) |> 
  # Create logged versions of all variables except for vacancy
  mutate(across(c(rent:tourism), .fns = list(log = \(x) log(x))), 
         .before = geometry) |> 
  # Normalize all variables
  mutate(across(c(rent:tourism_log), list(raw = \(x) x)),
         across(c(rent:tourism_log), \(x) as.numeric(scale(x))), 
         .before = geometry) |> 
  filter(year >= 2017)

# No lag dataset: rent, rent_lag, FREH, rev, price
dr$no_lag <-
  monthly_sept |> 
  filter(!is.na(rent), !is.na(rent_lag), !is.na(FREH), !is.na(rev), 
         !is.na(price)) |> 
  # Select relevant variables
  select(all_of(rent_vars)) |> 
  # Make year a character vector so it is treated as a factor
  mutate(year = as.character(year)) |> 
  # Replace zero-values of FREH/rev/price with lowest non-zero values
  mutate(FREH_dummy = FREH == 0, non_FREH_dummy = non_FREH == 0, 
         rev_dummy = rev == 0, .before = rent) |> 
  mutate(FREH = if_else(FREH == 0, min(FREH[FREH > 0]), FREH),
         non_FREH = if_else(non_FREH == 0, min(non_FREH[non_FREH > 0]), 
                            non_FREH),
         rev = if_else(rev == 0, min(rev[rev > 0]), rev),
         price = if_else(price == 0, min(price[price > 0]), price)) |> 
  # Create logged versions of all variables except for vacancy
  mutate(across(c(rent:tourism), .fns = list(log = \(x) log(x))), 
         .before = geometry) |> 
  # Normalize all variables
  mutate(across(c(rent:tourism_log), list(raw = \(x) x)),
         across(c(rent:tourism_log), \(x) as.numeric(scale(x))), 
         .before = geometry) |> 
  filter(year >= 2016)

# Imputed dataset
dr$impute <- 
  monthly_sept |> 
  # Do imputation
  impute() |>
  filter(!is.na(rent), !is.na(rent_lag), !is.na(FREH_lag), 
         !is.na(rev_lag), !is.na(price_lag)) |> 
  filter(year != 2016) |> 
  # Select relevant variables
  select(all_of(rent_vars)) |> 
  # Make year a character vector so it is treated as a factor
  mutate(year = as.character(year)) |> 
  # Replace zero-values of FREH/rev/price with lowest non-zero values
  mutate(FREH_dummy = FREH == 0, FREH_lag_dummy = FREH_lag == 0, 
         non_FREH_dummy = non_FREH == 0, non_FREH_lag_dummy = non_FREH_lag == 0, 
         rev_dummy = rev == 0, rev_lag_dummy = rev_lag == 0, 
         .before = rent) |> 
  mutate(FREH = if_else(FREH == 0, min(FREH[FREH > 0]), FREH),
         FREH_lag = if_else(FREH_lag == 0, min(FREH_lag[FREH_lag > 0]), 
                            FREH_lag),
         non_FREH = if_else(non_FREH == 0, min(non_FREH[non_FREH > 0]), 
                            non_FREH),
         non_FREH_lag = if_else(non_FREH_lag == 0, 
                                min(non_FREH_lag[non_FREH_lag > 0]), 
                                non_FREH_lag),
         rev = if_else(rev == 0, min(rev[rev > 0]), rev),
         rev_lag = if_else(rev_lag == 0, min(rev_lag[rev_lag > 0]), rev_lag),
         price = if_else(price == 0, min(price[price > 0]), price),
         price_lag = if_else(price_lag == 0, min(price_lag[price_lag > 0]), 
                             price_lag)) |> 
  # Create logged versions of all variables except for vacancy
  mutate(across(c(rent:tourism), .fns = list(log = \(x) log(x))), 
         .before = geometry) |> 
  # Normalize all variables
  mutate(across(c(rent:tourism_log), list(raw = \(x) x)),
         across(c(rent:tourism_log), \(x) as.numeric(scale(x))), 
         .before = geometry) |> 
  filter(year >= 2017)

# Vacancy lag dataset
dr$vacancy <-
  monthly_sept |> 
  filter(!is.na(rent), !is.na(rent_lag), !is.na(FREH_lag), !is.na(rev_lag), 
         !is.na(price_lag), !is.na(vacancy)) |> 
  # Select relevant variables
  select(all_of(rent_vars)) |> 
  # Make year a character vector so it is treated as a factor
  mutate(year = as.character(year)) |> 
  # Replace zero-values of FREH/rev/price with lowest non-zero values
  # Replace zero-values of FREH/rev/price with lowest non-zero values
  mutate(FREH_dummy = FREH == 0, FREH_lag_dummy = FREH_lag == 0, 
         non_FREH_dummy = non_FREH == 0, non_FREH_lag_dummy = non_FREH_lag == 0, 
         rev_dummy = rev == 0, rev_lag_dummy = rev_lag == 0, 
         .before = rent) |> 
  mutate(FREH = if_else(FREH == 0, min(FREH[FREH > 0]), FREH),
         FREH_lag = if_else(FREH_lag == 0, min(FREH_lag[FREH_lag > 0]), 
                            FREH_lag),
         non_FREH = if_else(non_FREH == 0, min(non_FREH[non_FREH > 0]), 
                            non_FREH),
         non_FREH_lag = if_else(non_FREH_lag == 0, 
                                min(non_FREH_lag[non_FREH_lag > 0]), 
                                non_FREH_lag),
         rev = if_else(rev == 0, min(rev[rev > 0]), rev),
         rev_lag = if_else(rev_lag == 0, min(rev_lag[rev_lag > 0]), rev_lag),
         price = if_else(price == 0, min(price[price > 0]), price),
         price_lag = if_else(price_lag == 0, min(price_lag[price_lag > 0]), 
                             price_lag)) |> 
  # Create logged versions of all variables except for vacancy
  mutate(across(c(rent:tourism), .fns = list(log = \(x) log(x))), 
         .before = geometry) |> 
  # Normalize all variables
  mutate(across(c(rent:tourism_log), list(raw = \(x) x)),
         across(c(rent:tourism_log), \(x) as.numeric(scale(x))), 
         .before = geometry) |> 
  filter(year >= 2017)

# Alternate dataset with arguable outliers removed
dr$outliers <-
  monthly_sept |> 
  filter(!is.na(rent), !is.na(rent_lag), !is.na(FREH_lag), !is.na(rev_lag), 
         !is.na(price_lag)) |> 
  filter(rent < 2700, rev_lag > 2.753645e-05, price_lag > 29.9641, 
         price_lag < 665.1416, abs(universe_change) < 1) |> 
  # Select relevant variables
  select(all_of(rent_vars)) |> 
  # Make year a character vector so it is treated as a factor
  mutate(year = as.character(year)) |> 
  # Replace zero-values of FREH/rev/price with lowest non-zero values
  mutate(FREH_lag_dummy = FREH_lag == 0, 
         non_FREH_lag_dummy = non_FREH_lag == 0, 
         rev_lag_dummy = rev_lag == 0, 
         .before = rent) |> 
  mutate(FREH_lag = if_else(FREH_lag == 0, min(FREH_lag[FREH_lag > 0]), 
                            FREH_lag),
         non_FREH_lag = if_else(non_FREH_lag == 0, min(
           non_FREH_lag[non_FREH_lag > 0]), non_FREH_lag),
         rev_lag = if_else(rev_lag == 0, min(rev_lag[rev_lag > 0]), rev_lag),
         price_lag = if_else(price_lag == 0, min(price_lag[price_lag > 0]), 
                             price_lag)) |> 
  # Create logged versions of all variables except for vacancy
  mutate(across(c(rent:tourism), .fns = list(log = \(x) log(x))), 
         .before = geometry) |> 
  # Normalize all variables
  mutate(across(c(rent:tourism_log), list(raw = \(x) x)),
         across(c(rent:tourism_log), \(x) as.numeric(scale(x))), 
         .before = geometry) |> 
  filter(year >= 2017)

# Alternate dataset with zeroes removed
dr$no_zero <-
  monthly_sept |> 
  filter(!is.na(rent), !is.na(rent_lag), !is.na(FREH_lag), !is.na(rev_lag), 
         !is.na(price_lag)) |> 
  # Select relevant variables
  select(all_of(rent_vars)) |> 
  # Make year a character vector so it is treated as a factor
  mutate(year = as.character(year)) |> 
  # Create logged versions of all variables except for vacancy
  mutate(across(c(rent:tourism), .fns = list(log = \(x) log(x))), 
         .before = geometry) |> 
  # Remove infinite values
  filter(!is.infinite(rent_log), !is.infinite(rent_lag_log), 
         !is.infinite(FREH_lag_log), !is.infinite(rev_lag_log),
         !is.infinite(price_lag_log)) |>
  # Normalize all variables
  mutate(across(c(rent:tourism_log), list(raw = \(x) x)),
         across(c(rent:tourism_log), \(x) as.numeric(scale(x))), 
         .before = geometry) |> 
  filter(year >= 2017)


# Clean up ----------------------------------------------------------------

rm(cmhc, cmhc_nbhd, monthly_impute, monthly_impute_rent, monthly_impute_univ,
   monthly_impute_vac, nn, nn_join, rent_vars, special_cases)

#### DATA FOR MODELS ###########################################################

source("R/01_startup.R")
monthly_sept <- qread("output/monthly_sept.qs")
monthly_sept_housing <- qread("output/monthly_sept_housing.qs")
qload("output/cmhc.qsm", nthreads = availableCores())


# Impute missing rent values for spatial panel ----------------------------

# nn <- nngeo::st_nn(cmhc_nbhd, cmhc_nbhd, k = 6, maxdist = 50000)
# qsave(nn, file = "output/nn.qs")
nn <- qread("output/nn.qs")

# Get list of nearest neighbours
nn_join <-
  cmhc_nbhd |> 
  st_drop_geometry() |> 
  select(id) |> 
  mutate(nn = !!nn) |> 
  mutate(nn = map(nn, \(x) cmhc_nbhd$id[x])) |> 
  mutate(nn = map2(nn, id, \(x, y) x[x != y]))

# Add nearest neighbours to monthly_sept
monthly_impute <- 
  monthly_sept |> 
  inner_join(nn_join, by = "id") |> 
  relocate(nn, .after = rent)

# Calculate rent/universe/vacancy values for nearest neighbours
monthly_impute <-
  monthly_impute |> 
  mutate(rent_nn = map2_dbl(nn, year, \(x, y) {
    monthly_impute |> 
      filter(id %in% x, year == y) |> 
      pull(rent) |> 
      mean(na.rm = TRUE)}), .before = nn) |> 
  mutate(univ_nn = map2_dbl(nn, year, \(x, y) {
    monthly_impute |> 
      filter(id %in% x, year == y) |> 
      pull(universe) |> 
      mean(na.rm = TRUE)}), .before = nn) |> 
  mutate(vac_nn = map2_dbl(nn, year, \(x, y) {
    monthly_impute |> 
      filter(id %in% x, year == y) |> 
      pull(vacancy) |> 
      mean(na.rm = TRUE)}), .before = nn) |> 
  # Special case 7650001 because no neighbours
  mutate(rent_nn = if_else(id == "7650001", rent, rent_nn),
         univ_nn = if_else(id == "7650001", universe, univ_nn),
         vac_nn = if_else(id == "7650001", vacancy, vac_nn))

# Calculate ratio of local values to nn values for each year
monthly_impute <-
  monthly_impute |> 
  mutate(rent_ratio = rent / rent_nn, .after = rent_nn) |> 
  mutate(univ_ratio = universe / univ_nn, .after = univ_nn) |> 
  mutate(vac_ratio = vacancy / vac_nn, .after = vac_nn)

# Fit per-neighbourhood linear models and predict missing values for rent
monthly_impute_rent <-
  monthly_impute |> 
  st_drop_geometry() |> 
  filter(sum(is.na(rent)) > 0, .by = id) |> 
  mutate(rent_lm = list(lm(rent_ratio ~ year, data = tibble(rent_ratio, year))),
         .after = rent_ratio, .by = id) |> 
  rowwise() |>
  mutate(new_ratio = predict(rent_lm, tibble(rent_ratio, year))) |> 
  ungroup() |> 
  mutate(rent_new = rent_nn * new_ratio) |> 
  filter(is.na(rent)) |> 
  select(id, year, rent_new) |> 
  # Suppress warnings about rank deficiency in models
  suppressWarnings()
  
# Fit per-neighbourhood linear models and predict missing values for universe
monthly_impute_univ <-
  monthly_impute |> 
  st_drop_geometry() |> 
  filter(sum(is.na(universe)) > 0, .by = id) |> 
  mutate(univ_lm = list(lm(univ_ratio ~ year, data = tibble(univ_ratio, year))),
         .after = univ_ratio, .by = id) |> 
  rowwise() |>
  mutate(new_ratio = predict(univ_lm, tibble(univ_ratio, year))) |> 
  ungroup() |> 
  mutate(univ_new = univ_nn * new_ratio) |> 
  filter(is.na(universe)) |> 
  select(id, year, univ_new)

# Fit per-neighbourhood linear models and predict missing values for vacancy
monthly_impute_vac <-
  monthly_impute |> 
  st_drop_geometry() |> 
  filter(sum(is.na(vacancy)) > 0, .by = id) |> 
  mutate(vac_ratio = if_else(is.infinite(vac_ratio), NA, vac_ratio)) |> 
  mutate(vac_nn_group = mean(vac_nn, na.rm = TRUE), .by = id, 
         .after = vac_nn) |> 
  mutate(vac_lm = list(
    if (sum(!is.na(vac_ratio)) > 0) {
      lm(vac_ratio ~ year, data = tibble(vac_ratio, year))  
    } else NA), .after = vac_ratio, .by = id) |>
  # Try to predict with model
  rowwise() |>
  mutate(new_ratio = if (is.logical(vac_lm[[1]])) NA_real_ else 
    predict(vac_lm, tibble(vac_ratio, year)), .after = vac_ratio) |> 
  ungroup() |> 
  # Use vac_nn_group is vac_nn is NA, and use vac_nn or vac_nn_group directly 
  # if there is no model
  mutate(vac_new = coalesce(vac_nn * new_ratio, vac_nn_group * new_ratio,
                            vac_nn, vac_nn_group), .after = vacancy) |> 
  filter(is.na(vacancy)) |>
  select(id, year, vac_new) |>
  # Suppress warnings about rank deficiency in models
  suppressWarnings()

# Coalesce new values
monthly_impute <-
  monthly_impute |> 
  left_join(monthly_impute_rent, by = c("id", "year")) |> 
  left_join(monthly_impute_univ, by = c("id", "year")) |> 
  left_join(monthly_impute_vac, by = c("id", "year")) |> 
  mutate(rent_new = coalesce(rent, rent_new)) |> 
  mutate(univ_new = coalesce(universe, univ_new)) |>
  mutate(vac_new = coalesce(vacancy, vac_new)) |> 
  select(id, year, rent, rent_new, universe, univ_new, vacancy, vac_new) |> 
  st_drop_geometry() 


# Produce dataset with rent as DV -----------------------------------------

dr <- list()

# Main dataset: rent, rent_lag, FREH_lag, rev_lag, price_lag
dr$main <-
  monthly_sept |> 
  filter(!is.na(rent), !is.na(rent_lag), !is.na(FREH), !is.na(FREH_lag), 
         !is.na(rev_lag), !is.na(price_lag)) |> 
  # Select relevant variables
  select(id, year, CMA, name_CMA, province, rent, rent_lag, FREH, FREH_lag, 
         rev, rev_lag, price, price_lag, universe, tenant, tourism, vacancy, 
         universe_change) |> 
  # Make year a character vector so it is treated as a factor
  mutate(year = as.character(year)) |> 
  # Replace zero-values of FREH/rev/price with lowest non-zero values
  mutate(FREH_dummy = FREH == 0, FREH_lag_dummy = FREH_lag == 0, 
         rev_dummy = rev == 0, rev_lag_dummy = rev_lag == 0, 
         .before = rent) |> 
  mutate(FREH = if_else(FREH == 0, min(FREH[FREH > 0]), FREH),
         FREH_lag = if_else(FREH_lag == 0, min(FREH_lag[FREH_lag > 0]), 
                            FREH_lag),
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
         .before = geometry)

# No lag dataset: rent, rent_lag, FREH, rev, price
dr$no_lag <-
  monthly_sept |> 
  filter(!is.na(rent), !is.na(rent_lag), !is.na(FREH), !is.na(rev), 
         !is.na(price)) |> 
  # Select relevant variables
  select(id, year, CMA, name_CMA, province, rent, rent_lag, FREH, FREH_lag, 
         rev, rev_lag, price, price_lag, universe, tenant, tourism, vacancy, 
         universe_change) |> 
  # Make year a character vector so it is treated as a factor
  mutate(year = as.character(year)) |> 
  # Replace zero-values of FREH/rev/price with lowest non-zero values
  mutate(FREH_dummy = FREH == 0, FREH_lag_dummy = FREH_lag == 0, 
         rev_dummy = rev == 0, rev_lag_dummy = rev_lag == 0, 
         .before = rent) |> 
  mutate(FREH = if_else(FREH == 0, min(FREH[FREH > 0]), FREH),
         FREH_lag = if_else(FREH_lag == 0, min(FREH_lag[FREH_lag > 0]), 
                            FREH_lag),
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
         .before = geometry)

# Vacancy dataset
dr$vacancy <-
  monthly_sept |> 
  filter(!is.na(rent), !is.na(vacancy)) |> 
  # Select relevant variables
  select(id, year, CMA, name_CMA, province, rent, rent_lag, FREH, FREH_lag, 
         rev, rev_lag, universe, tenant, tourism, vacancy, vacancy_lag, 
         universe_change) |> 
  # Make year a character vector so it is treated as a factor
  mutate(year = as.character(year)) |> 
  # Replace zero-values of FREH/rev with lowest non-zero values
  mutate(FREH_dummy = FREH == 0, FREH_lag_dummy = FREH_lag == 0, 
         rev_dummy = rev == 0, rev_lag_dummy = rev_lag == 0, .before = rent) |> 
  mutate(FREH = if_else(FREH == 0, min(FREH[FREH > 0]), FREH),
         FREH_lag = if_else(FREH_lag == 0, min(FREH_lag[FREH_lag > 0]), 
                            FREH_lag),
         rev = if_else(rev == 0, min(rev[rev > 0]), rev),
         rev_lag = if_else(rev_lag == 0, min(rev_lag[rev_lag > 0]), rev_lag)) |> 
  # Create logged versions of variables
  mutate(across(c(rent:tourism), .fns = list(log = \(x) log(x))), 
         .before = geometry) |> 
  # Normalize all variables
  mutate(across(c(rent:tourism_log), list(raw = \(x) x)),
         across(c(rent:tourism_log), \(x) as.numeric(scale(x))), 
         .before = geometry)

# Vacancy lag dataset
dr$vacancy_lag <-
  monthly_sept |> 
  filter(!is.na(rent), !is.na(rent_lag), !is.na(vacancy)) |> 
  # Select relevant variables
  select(id, year, CMA, name_CMA, province, rent, rent_lag, FREH, FREH_lag, 
         rev, rev_lag, universe, tenant, tourism, vacancy, universe_change) |> 
  # Make year a character vector so it is treated as a factor
  mutate(year = as.character(year)) |> 
  # Replace zero-values of FREH/rev with lowest non-zero values
  mutate(FREH_dummy = FREH == 0, FREH_lag_dummy = FREH_lag == 0, 
         rev_dummy = rev == 0, rev_lag_dummy = rev_lag == 0, .before = rent) |> 
  mutate(FREH = if_else(FREH == 0, min(FREH[FREH > 0]), FREH),
         FREH_lag = if_else(FREH_lag == 0, min(FREH_lag[FREH_lag > 0]), 
                            FREH_lag),
         rev = if_else(rev == 0, min(rev[rev > 0]), rev),
         rev_lag = if_else(rev_lag == 0, min(rev_lag[rev_lag > 0]), rev_lag)) |> 
  # Create logged versions of variables
  mutate(across(c(rent:tourism), .fns = list(log = \(x) log(x))), 
         .before = geometry) |> 
  # Normalize all variables
  mutate(across(c(rent:tourism_log), list(raw = \(x) x)),
         across(c(rent:tourism_log), \(x) as.numeric(scale(x))), 
         .before = geometry)

# Alternate dataset with arguable outliers removed
# dr$outliers <-
#   monthly_sept |> 
#   filter(!is.na(rent)) |> 
#   # Remove outliers
#   filter(rent < 2700, rent > 400, rev > 5.017468e-05) |> 
#   # Select relevant variables
#   select(id, year, CMA, name_CMA, province, rent, FREH, rev, universe, 
#          tenant, tourism, vacancy) |> 
#   # Make year a character vector so it is treated as a factor
#   mutate(year = as.character(year)) |> 
#   # Replace zero-values of FREH/rev with lowest non-zero values
#   mutate(FREH_dummy = FREH == 0, rev_dummy = rev == 0, .before = rent) |> 
#   mutate(FREH = if_else(FREH == 0, min(FREH[FREH > 0]), FREH),
#          rev = if_else(rev == 0, min(rev[rev > 0]), rev)) |> 
#   # Create logged versions of all variables except for vacancy
#   mutate(across(c(rent:tourism), .fns = list(log = \(x) log(x))), 
#          .before = geometry) |> 
#   # Normalize all variables
#   mutate(across(c(rent:tourism_log), list(raw = \(x) x)),
#          across(c(rent:tourism_log), \(x) as.numeric(scale(x))), 
#          .before = geometry)

# Alternate dataset with zeroes removed
# dr$no_zero <-
#   monthly_sept |> 
#   filter(!is.na(rent)) |> 
#   # Select relevant variables
#   select(id, year, CMA, name_CMA, province, rent, FREH, FREH_count, rev, 
#          rev_count, universe, tenant, tourism, vacancy) |> 
#   # Make year a character vector so it is treated as a factor
#   mutate(year = as.character(year)) |> 
#   # Create logged versions of all variables except for vacancy
#   mutate(across(c(rent:tourism), .fns = list(log = \(x) log(x))), 
#          .before = geometry) |> 
#   # Remove infinite values
#   filter(!is.infinite(rent_log), !is.infinite(FREH_log), 
#          !is.infinite(rev_log)) |> 
#   # Normalize all variables
#   mutate(across(c(rent:tourism_log), list(raw = \(x) x)),
#          across(c(rent:tourism_log), \(x) as.numeric(scale(x))), 
#          .before = geometry)

# Alternate with imputed rent/universe/vacancy for balanced panel
# dr$impute <-
#   monthly_sept |> 
#   # Do imputation
#   impute() |> 
#   # Select relevant variables
#   select(id, year, CMA, name_CMA, province, rent, FREH, FREH_count, rev, 
#          rev_count, universe, tenant, tourism, vacancy) |> 
#   # Make year a character vector so it is treated as a factor
#   mutate(year = as.character(year)) |> 
#   # Replace zero-values of FREH/rev with lowest non-zero values
#   mutate(FREH_dummy = FREH == 0, rev_dummy = rev == 0, .before = rent) |> 
#   mutate(FREH = if_else(FREH == 0, min(FREH[FREH > 0]), FREH),
#          rev = if_else(rev == 0, min(rev[rev > 0]), rev)) |> 
#   # Create logged versions of all variables except for vacancy
#   mutate(across(c(rent:tourism), .fns = list(log = \(x) log(x))), 
#          .before = geometry) |> 
#   # Normalize all variables
#   mutate(across(c(rent:tourism_log), list(raw = \(x) x)),
#          across(c(rent:tourism_log), \(x) as.numeric(scale(x))), 
#          .before = geometry)

# Alternate with FREH_3_share
dr$alt <-
  monthly_sept |> 
  filter(!is.na(rent)) |> 
  # Select relevant variables
  select(id, year, CMA, name_CMA, province, rent, rent_lag, FREH = FREH_3, 
         FREH_lag = FREH_3_lag, rev, rev_lag, universe, tenant, tourism, 
         vacancy, universe_change) |> 
  # Make year a character vector so it is treated as a factor
  mutate(year = as.character(year)) |> 
  # Replace zero-values of FREH/rev with lowest non-zero values
  mutate(FREH_dummy = FREH == 0, FREH_lag_dummy = FREH_lag == 0, 
         rev_dummy = rev == 0, rev_lag_dummy = rev_lag == 0, .before = rent) |> 
  mutate(FREH = if_else(FREH == 0, min(FREH[FREH > 0]), FREH),
         FREH_lag = if_else(FREH_lag == 0, min(FREH_lag[FREH_lag > 0]), 
                            FREH_lag),
         rev = if_else(rev == 0, min(rev[rev > 0]), rev),
         rev_lag = if_else(rev_lag == 0, min(rev_lag[rev_lag > 0]), rev_lag)) |> 
  # Create logged versions of all variables except for vacancy
  mutate(across(c(rent:tourism), .fns = list(log = \(x) log(x))), 
         .before = geometry) |> 
  # Normalize all variables
  mutate(across(c(rent:tourism_log), list(raw = \(x) x)),
         across(c(rent:tourism_log), \(x) as.numeric(scale(x))), 
         .before = geometry)

# Alternate with FREH_3_share and rent_lag
dr$alt_lag <-
  monthly_sept |> 
  filter(!is.na(rent), !is.na(rent_lag)) |> 
  # Select relevant variables
  select(id, year, CMA, name_CMA, province, rent, rent_lag, FREH = FREH_3, 
         FREH_lag = FREH_3_lag, rev, rev_lag, universe, tenant, tourism, 
         vacancy, universe_change) |> 
  # Make year a character vector so it is treated as a factor
  mutate(year = as.character(year)) |> 
  # Replace zero-values of FREH/rev with lowest non-zero values
  mutate(FREH_dummy = FREH == 0, FREH_lag_dummy = FREH_lag == 0, 
         rev_dummy = rev == 0, rev_lag_dummy = rev_lag == 0, .before = rent) |> 
  mutate(FREH = if_else(FREH == 0, min(FREH[FREH > 0]), FREH),
         FREH_lag = if_else(FREH_lag == 0, min(FREH_lag[FREH_lag > 0]), 
                            FREH_lag),
         rev = if_else(rev == 0, min(rev[rev > 0]), rev),
         rev_lag = if_else(rev_lag == 0, min(rev_lag[rev_lag > 0]), rev_lag)) |> 
  # Create logged versions of all variables except for vacancy
  mutate(across(c(rent:tourism), .fns = list(log = \(x) log(x))), 
         .before = geometry) |> 
  # Normalize all variables
  mutate(across(c(rent:tourism_log), list(raw = \(x) x)),
         across(c(rent:tourism_log), \(x) as.numeric(scale(x))), 
         .before = geometry)

# Alternate with only housing listings
# dr$housing <-
#   monthly_sept_housing |> 
#   filter(!is.na(rent)) |> 
#   # Select relevant variables
#   select(id, year, CMA, name_CMA, province, rent, FREH, rev, universe, 
#          tenant, tourism, vacancy) |> 
#   # Make year a character vector so it is treated as a factor
#   mutate(year = as.character(year)) |> 
#   # Replace zero-values of FREH/rev with lowest non-zero values
#   mutate(FREH_dummy = FREH == 0, rev_dummy = rev == 0, .before = rent) |> 
#   mutate(FREH = if_else(FREH == 0, min(FREH[FREH > 0]), FREH),
#          rev = if_else(rev == 0, min(rev[rev > 0]), rev)) |> 
#   # Create logged versions of all variables except for vacancy
#   mutate(across(c(rent:tourism), .fns = list(log = \(x) log(x))), 
#          .before = geometry) |> 
#   # Remove infinite values
#   # Normalize all variables
#   mutate(across(c(rent:tourism_log), list(raw = \(x) x)),
#          across(c(rent:tourism_log), \(x) as.numeric(scale(x))), 
#          .before = geometry)


# # Produce dataset with rent_change as DV ----------------------------------

dc <- list()

# Main dataset
dc$main <-
  monthly_sept |> 
  filter(!is.na(rent_change), year != 2015, year != 2023) |> 
  # Select relevant variables
  select(id, year, CMA, name_CMA, province, rent_change, FREH_change, 
         rev_change, universe, universe_change, tenant, tourism, vacancy,
         vacancy_change) |> 
  # Make year a character vector so it is treated as a factor
  mutate(year = as.character(year)) |> 
  # Create logged versions of universe and tourism
  mutate(across(c(universe, tourism), .fns = list(log = \(x) log(x))), 
         .before = geometry) |> 
  # Normalize all variables
  mutate(across(c(rent_change:tourism_log), list(raw = \(x) x)),
         across(c(rent_change:tourism_log), \(x) as.numeric(scale(x))), 
         .before = geometry)

# Vacancy dataset
dc$vacancy <-
  monthly_sept |> 
  filter(!is.na(rent_change), !is.na(vacancy_change), year != 2015, 
         year != 2023) |> 
  # Select relevant variables
  select(id, year, CMA, name_CMA, province, rent_change, FREH_change, 
         rev_change, universe, universe_change, tenant, tourism, vacancy,
         vacancy_change) |> 
  # Make year a character vector so it is treated as a factor
  mutate(year = as.character(year)) |> 
  # Create logged versions of universe and tourism
  mutate(across(c(universe, tourism), .fns = list(log = \(x) log(x))), 
         .before = geometry) |> 
  # Normalize all variables
  mutate(across(c(rent_change:tourism_log), list(raw = \(x) x)),
         across(c(rent_change:tourism_log), \(x) as.numeric(scale(x))), 
         .before = geometry)

# Alternate dataset with arguable outliers removed
dc$outliers <-
  monthly_sept |> 
  filter(!is.na(rent_change), year != 2015, year != 2023) |> 
  # Remove outliers
  filter(rent_change > -400, rent_change < 600,
         abs(FREH_change) < 0.01, abs(rev_change) < 0.2) |> 
# Select relevant variables
  select(id, year, CMA, name_CMA, province, rent_change, FREH_change, 
         rev_change, universe, universe_change, tenant, tourism, vacancy,
         vacancy_change) |> 
  # Make year a character vector so it is treated as a factor
  mutate(year = as.character(year)) |> 
  # Create logged versions of universe and tourism
  mutate(across(c(universe, tourism), .fns = list(log = \(x) log(x))), 
         .before = geometry) |> 
  # Normalize all variables
  mutate(across(c(rent_change:tourism_log), list(raw = \(x) x)),
         across(c(rent_change:tourism_log), \(x) as.numeric(scale(x))), 
         .before = geometry)

# Alternate with imputed rent/universe for balanced panel
dc$impute <-
  monthly_sept |> 
  # Do imputation
  impute() |> 
  filter(year != 2015, year != 2023) |> 
  # Select relevant variables
  select(id, year, CMA, name_CMA, province, rent_change, FREH_change, 
         rev_change, universe, universe_change, tenant, tourism, vacancy,
         vacancy_change) |> 
  # Make year a character vector so it is treated as a factor
  mutate(year = as.character(year)) |> 
  # Create logged versions of universe and tourism
  mutate(across(c(universe, tourism), .fns = list(log = \(x) log(x))), 
         .before = geometry) |> 
  # Normalize all variables
  mutate(across(c(rent_change:tourism_log), list(raw = \(x) x)),
         across(c(rent_change:tourism_log), \(x) as.numeric(scale(x))), 
         .before = geometry)

# Alternate with FREH_3_change 
dc$alt <-
  monthly_sept |> 
  filter(!is.na(rent_change), year != 2015, year != 2023) |> 
  # Select relevant variables
  select(id, year, CMA, name_CMA, province, rent_change, 
         FREH_change = FREH_3_change, rev_change, universe, universe_change, 
         tenant, tourism, vacancy, vacancy_change) |> 
  # Make year a character vector so it is treated as a factor
  mutate(year = as.character(year)) |> 
  # Create logged versions of universe and tourism
  mutate(across(c(universe, tourism), .fns = list(log = \(x) log(x))), 
         .before = geometry) |> 
  # Normalize all variables
  mutate(across(c(rent_change:tourism_log), list(raw = \(x) x)),
         across(c(rent_change:tourism_log), \(x) as.numeric(scale(x))), 
         .before = geometry)

# Alternate with FREH_count_change and rev_count_change
dc$count <- 
  monthly_sept |> 
  filter(!is.na(rent_change), year != 2015, year != 2023) |> 
  # Select relevant variables
  select(id, year, CMA, name_CMA, province, rent_change, 
         FREH_change = FREH_count_change, rev_change = rev_count_change, 
         universe, universe_change, tenant, tourism, vacancy, vacancy_change) |> 
  # Make year a character vector so it is treated as a factor
  mutate(year = as.character(year)) |> 
  # Create logged versions of universe and tourism
  mutate(across(c(universe, tourism), .fns = list(log = \(x) log(x))), 
         .before = geometry) |> 
  # Normalize all variables
  mutate(across(c(rent_change:tourism_log), list(raw = \(x) x)),
         across(c(rent_change:tourism_log), \(x) as.numeric(scale(x))), 
         .before = geometry)

# Alternate with only housing listings
dc$housing <-
  monthly_sept_housing |> 
  filter(!is.na(rent_change), year != 2015, year != 2023) |> 
  # Select relevant variables
  select(id, year, CMA, name_CMA, province, rent_change, FREH_change, 
         rev_change, universe, universe_change, tenant, tourism, vacancy,
         vacancy_change) |> 
  # Make year a character vector so it is treated as a factor
  mutate(year = as.character(year)) |> 
  # Create logged versions of universe and tourism
  mutate(across(c(universe, tourism), .fns = list(log = \(x) log(x))), 
         .before = geometry) |> 
  # Normalize all variables
  mutate(across(c(rent_change:tourism_log), list(raw = \(x) x)),
         across(c(rent_change:tourism_log), \(x) as.numeric(scale(x))), 
         .before = geometry)

# Alternate with change variables lagged by a year
dc$lag <-
  monthly_sept |> 
  filter(!is.na(rent_change), year != 2015, year != 2023) |> 
  filter(!is.na(rent_change), !is.na(rent_change_lag)) |> 
  # Select relevant variables
  select(id, year, CMA, name_CMA, province, rent_change, rent_change_lag,
         FREH_change, FREH_change_lag, rev_change, rev_change_lag, universe, 
         universe_change, tenant, tourism, vacancy, vacancy_change) |> 
  # Make year a character vector so it is treated as a factor
  mutate(year = as.character(year)) |> 
  # Create logged versions of universe and tourism
  mutate(across(c(universe, tourism), .fns = list(log = \(x) log(x))), 
         .before = geometry) |> 
  # Normalize all variables
  mutate(across(c(rent_change:tourism_log), list(raw = \(x) x)),
         across(c(rent_change:tourism_log), \(x) as.numeric(scale(x))), 
         .before = geometry)


# Clean up ----------------------------------------------------------------

rm(cmhc, cmhc_nbhd, monthly_impute, monthly_impute_rent, monthly_impute_univ,
   monthly_impute_vac, nn, nn_join)

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

# Calculate rent/universe values for nearest neighbours
monthly_impute <-
  monthly_impute |> 
  mutate(rent_nn = map2_dbl(nn, year, \(x, y) {
    monthly_impute |> 
      filter(id %in% x, year == y) |> 
      pull(rent) |> 
      mean(na.rm = TRUE)
  }), .before = nn) |> 
  mutate(univ_nn = map2_dbl(nn, year, \(x, y) {
    monthly_impute |> 
      filter(id %in% x, year == y) |> 
      pull(universe) |> 
      mean(na.rm = TRUE)
  }), .before = nn)

# Calculate ratio of local rent and universe to nn values for each year
monthly_impute <-
  monthly_impute |> 
  mutate(rent_ratio = rent / rent_nn, .after = rent_nn) |> 
  mutate(univ_ratio = universe / univ_nn, .after = univ_nn) |> 
  mutate(across(c(rent_ratio, univ_ratio), \(x) if_else(is.nan(x), 0, x)))
  
# Fit per-neighbourhood linear models and predict missing values
monthly_impute <- 
  monthly_impute |> 
  mutate(
    rent_lm = list(lm(rent_ratio ~ year, data = tibble(rent_ratio, year))),
    univ_lm = list(lm(univ_ratio ~ year, data = tibble(univ_ratio, year))),
    .by = id) |>
  rowwise() |>
  mutate(rent_ratio = coalesce(rent_ratio, 
                               predict(rent_lm, tibble(rent_ratio, year))),
         univ_ratio = coalesce(univ_ratio, 
                               predict(univ_lm, tibble(univ_ratio, year)))) |>
  ungroup() |> 
  # Suppress warnings about rank deficiency in models
  suppressWarnings()
  
# Calculate new rent/universe values from the model predictions
monthly_impute <-
  monthly_impute |> 
  mutate(rent_new = coalesce(rent, rent_nn * rent_ratio)) |> 
  mutate(univ_new = coalesce(universe, univ_nn * univ_ratio)) |> 
  select(id, year, rent, rent_new, universe, univ_new) |> 
  st_drop_geometry() 


# Produce dataset with rent as DV -----------------------------------------

dr <- list()

# Main dataset
dr$main <-
  monthly_sept |> 
  filter(!is.na(rent)) |> 
  # Select relevant variables
  select(id, year, CMA, name_CMA, province, rent, FREH, rev, universe, 
         tenant, tourism, vacancy) |> 
  # Make year a character vector so it is treated as a factor
  mutate(year = as.character(year)) |> 
  # Replace zero-values of FREH/rev with lowest non-zero values
  mutate(FREH_dummy = FREH == 0, rev_dummy = rev == 0, .before = rent) |> 
  mutate(FREH = if_else(FREH == 0, min(FREH[FREH > 0]), FREH),
         rev = if_else(rev == 0, min(rev[rev > 0]), rev)) |> 
  # Create logged versions of all variables except for vacancy
  mutate(across(c(rent:tourism), .fns = list(log = \(x) log(x))), 
         .before = geometry) |> 
  # Normalize all variables
  mutate(across(c(rent:tourism_log), list(raw = \(x) x)),
         across(c(rent:tourism_log), \(x) as.numeric(scale(x))), 
         .before = geometry)

# Alternate dataset with arguable outliers removed
dr$outliers <-
  monthly_sept |> 
  filter(!is.na(rent)) |> 
  # Remove outliers
  filter(rent < 2700, rent > 400, rev > 5.017468e-05) |> 
  # Select relevant variables
  select(id, year, CMA, name_CMA, province, rent, FREH, rev, universe, 
         tenant, tourism, vacancy) |> 
  # Make year a character vector so it is treated as a factor
  mutate(year = as.character(year)) |> 
  # Replace zero-values of FREH/rev with lowest non-zero values
  mutate(FREH_dummy = FREH == 0, rev_dummy = rev == 0, .before = rent) |> 
  mutate(FREH = if_else(FREH == 0, min(FREH[FREH > 0]), FREH),
         rev = if_else(rev == 0, min(rev[rev > 0]), rev)) |> 
  # Create logged versions of all variables except for vacancy
  mutate(across(c(rent:tourism), .fns = list(log = \(x) log(x))), 
         .before = geometry) |> 
  # Normalize all variables
  mutate(across(c(rent:tourism_log), list(raw = \(x) x)),
         across(c(rent:tourism_log), \(x) as.numeric(scale(x))), 
         .before = geometry)

# Alternate dataset with zeroes removed
dr$no_zero <-
  monthly_sept |> 
  filter(!is.na(rent)) |> 
  # Select relevant variables
  select(id, year, CMA, name_CMA, province, rent, FREH, FREH_count, rev, 
         rev_count, universe, tenant, tourism, vacancy) |> 
  # Make year a character vector so it is treated as a factor
  mutate(year = as.character(year)) |> 
  # Create logged versions of all variables except for vacancy
  mutate(across(c(rent:tourism), .fns = list(log = \(x) log(x))), 
         .before = geometry) |> 
  # Remove infinite values
  filter(!is.infinite(rent_log), !is.infinite(FREH_log), 
         !is.infinite(rev_log)) |> 
  # Normalize all variables
  mutate(across(c(rent:tourism_log), list(raw = \(x) x)),
         across(c(rent:tourism_log), \(x) as.numeric(scale(x))), 
         .before = geometry)

# Alternate with imputed rent/universe for balanced panel
dr$impute <-
  monthly_sept |> 
  # Do imputation
  impute() |> 
  # Select relevant variables
  select(id, year, CMA, name_CMA, province, rent, FREH, FREH_count, rev, 
         rev_count, universe, tenant, tourism, vacancy) |> 
  # Make year a character vector so it is treated as a factor
  mutate(year = as.character(year)) |> 
  # Replace zero-values of FREH/rev with lowest non-zero values
  mutate(FREH_dummy = FREH == 0, rev_dummy = rev == 0, .before = rent) |> 
  mutate(FREH = if_else(FREH == 0, min(FREH[FREH > 0]), FREH),
         rev = if_else(rev == 0, min(rev[rev > 0]), rev)) |> 
  # Create logged versions of all variables except for vacancy
  mutate(across(c(rent:tourism), .fns = list(log = \(x) log(x))), 
         .before = geometry) |> 
  # Normalize all variables
  mutate(across(c(rent:tourism_log), list(raw = \(x) x)),
         across(c(rent:tourism_log), \(x) as.numeric(scale(x))), 
         .before = geometry)

# Alternate with FREH_3_share
dr$alt <-
  monthly_sept |> 
  filter(!is.na(rent)) |> 
  # Select relevant variables
  select(id, year, CMA, name_CMA, province, rent, FREH = FREH_3, rev, 
         universe, tenant, tourism, vacancy) |> 
  # Make year a character vector so it is treated as a factor
  mutate(year = as.character(year)) |> 
  # Replace zero-values of FREH/rev with lowest non-zero values
  mutate(FREH_dummy = FREH == 0, rev_dummy = rev == 0, .before = rent) |> 
  mutate(FREH = if_else(FREH == 0, min(FREH[FREH > 0]), FREH),
         rev = if_else(rev == 0, min(rev[rev > 0]), rev)) |> 
  # Create logged versions of all variables except for vacancy
  mutate(across(c(rent:tourism), .fns = list(log = \(x) log(x))), 
         .before = geometry) |> 
  # Normalize all variables
  mutate(across(c(rent:tourism_log), list(raw = \(x) x)),
         across(c(rent:tourism_log), \(x) as.numeric(scale(x))), 
         .before = geometry)

# Alternate with only housing listings
dr$housing <-
  monthly_sept_housing |> 
  filter(!is.na(rent)) |> 
  # Select relevant variables
  select(id, year, CMA, name_CMA, province, rent, FREH, rev, universe, 
         tenant, tourism, vacancy) |> 
  # Make year a character vector so it is treated as a factor
  mutate(year = as.character(year)) |> 
  # Replace zero-values of FREH/rev with lowest non-zero values
  mutate(FREH_dummy = FREH == 0, rev_dummy = rev == 0, .before = rent) |> 
  mutate(FREH = if_else(FREH == 0, min(FREH[FREH > 0]), FREH),
         rev = if_else(rev == 0, min(rev[rev > 0]), rev)) |> 
  # Create logged versions of all variables except for vacancy
  mutate(across(c(rent:tourism), .fns = list(log = \(x) log(x))), 
         .before = geometry) |> 
  # Remove infinite values
  # Normalize all variables
  mutate(across(c(rent:tourism_log), list(raw = \(x) x)),
         across(c(rent:tourism_log), \(x) as.numeric(scale(x))), 
         .before = geometry)


# # Produce dataset with rent_change as DV ----------------------------------

dc <- list()

# Main dataset
dc$main <-
  monthly_sept |> 
  filter(!is.na(rent_change), year != 2016) |> 
  # Select relevant variables
  select(id, year, CMA, name_CMA, province, rent_change, FREH_change, 
         rev_change, universe, universe_change, tenant, tourism, vacancy) |> 
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
  filter(!is.na(rent_change), year != 2016) |> 
  # Remove outliers
  filter(rent_change > -400, rent_change < 600,
         abs(FREH_change) < 0.01, abs(rev_change) < 0.2) |> 
# Select relevant variables
  select(id, year, CMA, name_CMA, province, rent_change, FREH_change, 
         rev_change, universe, universe_change, tenant, tourism, vacancy) |> 
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
  filter(year != 2016) |> 
  # Select relevant variables
  select(id, year, CMA, name_CMA, province, rent_change, FREH_change, 
         rev_change, universe, universe_change, tenant, tourism, vacancy) |> 
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
  filter(!is.na(rent_change), year != 2016) |> 
  # Select relevant variables
  select(id, year, CMA, name_CMA, province, rent_change, 
         FREH_change = FREH_3_change, rev_change, universe, universe_change, 
         tenant, tourism, vacancy) |> 
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
  filter(!is.na(rent_change), year != 2016) |> 
  # Select relevant variables
  select(id, year, CMA, name_CMA, province, rent_change, 
         FREH_change = FREH_count_change, rev_change = rev_count_change, 
         universe, universe_change, tenant, tourism, vacancy) |> 
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
  filter(!is.na(rent_change), year != 2016) |> 
  # Select relevant variables
  select(id, year, CMA, name_CMA, province, rent_change, FREH_change, 
         rev_change, universe, universe_change, tenant, tourism, vacancy) |> 
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
  arrange(id, year) |> 
  mutate(across(c(FREH_change, rev_change, universe_change), 
                \(x) slide_dbl(x, \(y) y[1], .before = 1)), .by = id) |> 
  filter(!is.na(rent_change), !is.na(universe_change), year >= 2018) |> 
  # Select relevant variables
  select(id, year, CMA, name_CMA, province, rent_change, FREH_change, 
         rev_change, universe, universe_change, tenant, tourism, vacancy) |> 
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

rm(cmhc, cmhc_nbhd, monthly_impute, nn, nn_join)

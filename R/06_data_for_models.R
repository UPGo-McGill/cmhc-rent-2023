#### DATA FOR MODELS ###########################################################

source("R/01_startup.R")
monthly_sept <- qread("output/monthly_sept.qs")
monthly_sept_housing <- qread("output/monthly_sept_housing.qs")
qload("output/cmhc.qsm", nthreads = availableCores())


# Impute missing rent values for spatial panel ----------------------------

# nn <- nngeo::st_nn(cmhc_nbhd, cmhc_nbhd, k = 6, maxdist = 50000)
# qsave(nn, file = "output/nn.qs")
nn <- qread("output/nn.qs")

nn_join <-
  cmhc_nbhd |> 
  st_drop_geometry() |> 
  select(id) |> 
  mutate(nn = !!nn) |> 
  mutate(nn = map(nn, \(x) cmhc_nbhd$id[x])) |> 
  mutate(nn = map2(nn, id, \(x, y) x[x != y]))

monthly_impute <- 
  monthly_sept |> 
  inner_join(nn_join, by = "id") |> 
  relocate(nn, .after = rent)

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

monthly_impute <- 
  monthly_impute |> 
  mutate(rent_ratio = rent / rent_nn, .after = rent_nn) |> 
  mutate(rent_ratio = mean(rent_ratio, na.rm = TRUE), .by = id) |> 
  mutate(univ_ratio = universe / univ_nn, .after = univ_nn) |> 
  mutate(univ_ratio = mean(univ_ratio, na.rm = TRUE), .by = id)

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
  select(id, year, CMA, name_CMA, province, rent, FREH, rev, EH, universe, 
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
  select(id, year, CMA, name_CMA, province, rent, FREH, rev, EH, universe, 
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
         rev_count, EH, universe, tenant, tourism, vacancy) |> 
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
         rev_count, EH, universe, tenant, tourism, vacancy) |> 
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
  select(id, year, CMA, name_CMA, province, rent, FREH = FREH_3, rev, EH, 
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

# Alternate with imputed rent/universe for balanced panel and FREH_3_share
dr$impute_alt <-
  monthly_sept |> 
  # Do imputation
  impute() |> 
  # Select relevant variables
  select(id, year, CMA, name_CMA, province, rent, FREH = FREH_3, 
         FREH_count = FREH_3_count, rev, rev_count, EH, universe, tenant, 
         tourism, vacancy) |> 
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
  select(id, year, CMA, name_CMA, province, rent, FREH, rev, EH, universe, 
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

# Alternate with imputed rent/universe for balanced panel and only housing
dr$impute_housing <-
  monthly_sept_housing |> 
  # Do imputation
  impute() |> 
  # Select relevant variables
  select(id, year, CMA, name_CMA, province, rent, FREH, FREH_count, rev, 
         rev_count, EH, universe, tenant, tourism, vacancy) |> 
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


# # Produce dataset with rent_change as DV ----------------------------------
# 
# data_change <-
#   monthly_sept |> 
#   # Remove outliers
#   filter(rent_change > -400, rent_change < 600, abs(FREH_change) < 395,
#          abs(rev_change) < 5000000) |> 
# # Select relevant variables
#   select(id, year, CMA, name_CMA, province, rent, rent_change,
#          FREH_change, FREH_share_change, FREH_3_change, FREH_3_share_change,
#          FREH_share_u_change, FREH_3_share_u_change, rev_change, 
#          rev_share_change, rev_share_u_change, EH_share, universe, 
#          universe_change, tenant_share, tourism, vacancy) |> 
#   # Make year a character vector so it is treated as a factor
#   mutate(year = as.character(year)) |> 
#   # Make log version of universe and tourism
#   mutate(universe_log = log(universe),
#          tourism_log = log(tourism), 
#          .before = geometry) |> 
#   # Normalize all variables
#   mutate(across(c(rent:tourism_log), 
#                 .fns = list(nm = \(x) as.numeric(scale(x)))), 
#          .before = geometry)
# 
# # Alternate version with FREH_3_change
# data_change_alt <-
#   monthly_sept |> 
#   # Remove outliers
#   filter(rent_change > -400, rent_change < 600, abs(FREH_3_change) < 500,
#          abs(rev_change) < 5000000) |> 
#   # Select relevant variables
#   select(id, year, CMA, name_CMA, province, rent, rent_change,
#          FREH_change, FREH_share_change, FREH_3_change, FREH_3_share_change,
#          FREH_share_u_change, FREH_3_share_u_change, rev_change, 
#          rev_share_change, rev_share_u_change, EH_share, universe, 
#          universe_change, tenant_share, tourism, vacancy) |> 
#   # Make year a character vector so it is treated as a factor
#   mutate(year = as.character(year)) |> 
#   # Make log version of universe and tourism
#   mutate(universe_log = log(universe),
#          tourism_log = log(tourism), 
#          .before = geometry) |> 
#   # Normalize all variables
#   mutate(across(c(rent:tourism_log), 
#                 .fns = list(nm = \(x) as.numeric(scale(x)))), 
#          .before = geometry)
# 
# # Alternate version with FREH_share_change and rev_share_change
# data_change_share <-
#   monthly_sept |> 
#   # Remove outliers
#   filter(rent_change > -400, rent_change < 600, abs(rev_share_change) < 0.48) |> 
#   # Select relevant variables
#   select(id, year, CMA, name_CMA, province, rent, rent_change,
#          FREH_change, FREH_share_change, FREH_3_change, FREH_3_share_change,
#          FREH_share_u_change, FREH_3_share_u_change, rev_change, 
#          rev_share_change, rev_share_u_change, EH_share, universe, 
#          universe_change, tenant_share, tourism, vacancy) |> 
#   # Make year a character vector so it is treated as a factor
#   mutate(year = as.character(year)) |> 
#   # Make log version of universe and tourism
#   mutate(universe_log = log(universe),
#          tourism_log = log(tourism), 
#          .before = geometry) |> 
#   # Normalize all variables
#   mutate(across(c(rent:tourism_log), 
#                 .fns = list(nm = \(x) as.numeric(scale(x)))), 
#          .before = geometry)



# Clean up ----------------------------------------------------------------

rm(cmhc, cmhc_nbhd, monthly_impute, nn, nn_join, impute)

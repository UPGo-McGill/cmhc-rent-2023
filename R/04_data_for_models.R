#### DATA FOR MODELS ###########################################################

source("R/01_startup.R")
qload("output/cmhc.qsm", nthreads = availableCores())
monthly_april <- qread("output/monthly_april.qs", nthreads = availableCores())

data_rent <-
  monthly_april |> 
  inner_join(cmhc_nbhd, by = c("id", "year")) |> 
  st_as_sf() |> 
  filter(FREH_share < 1, rev_share < 1) |>
  mutate(year = as.character(year)) |> 
  mutate(across(c(rent, FREH_share, rev_share, universe, vacancy, pop_CMA, 
                  accommodation), .fns = list(log = log), 
                .names = "{.fn}_{.col}")) |> 
  select(id, year, CMA, province, rent, log_rent, FREH_share, log_FREH_share, 
         rev_share, log_rev_share, universe, log_universe, vacancy, log_vacancy,
         tenant_share, pop_CMA, log_pop_CMA, accommodation, 
         log_accommodation) |> 
  filter(!is.infinite(log_FREH_share), !is.infinite(log_rev_share),
         !is.infinite(log_universe), !is.infinite(log_vacancy),
         !is.infinite(log_pop_CMA), !is.infinite(log_accommodation)) |> 
  na.omit()

data_change <- 
  monthly_april |> 
  inner_join(cmhc_nbhd, by = c("id", "year")) |> 
  st_as_sf() |> 
  filter(!is.infinite(rent_change_pct), !is.infinite(FREH_change_pct), 
         !is.infinite(rev_change_pct)) |>
  filter(abs(FREH_change_pct) < 5, abs(rev_change_pct) < 5) |>
  mutate(year = as.character(year)) |> 
  mutate(across(c(universe, vacancy, pop_CMA, accommodation), 
                .fns = list(log = log), .names = "{.fn}_{.col}")) |> 
  select(id, year, CMA, province, rent_change_pct, FREH_change_pct, FREH_share,
         rev_change_pct, rev_share, universe, log_universe, vacancy, 
         log_vacancy, tenant_share, pop_CMA, log_pop_CMA, accommodation, 
         log_accommodation) |> 
  filter(!is.infinite(log_universe), !is.infinite(log_vacancy),
         !is.infinite(log_pop_CMA), !is.infinite(log_accommodation)) |> 
  na.omit()

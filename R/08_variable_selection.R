#### 08 VARIABLE SELECTION #####################################################

source("R/01_startup.R")
qload("output/cmhc.qsm", nthreads = availableCores())
monthly_sept <- qread("output/monthly_sept.qs", nthreads = availableCores())

skew <- function(x) mean(scale(x) ^ 3, na.rm = TRUE)


## Impute missing rent values for spatial panel ################################

source("R/07_imputation.R")


## Rent model dependent variables ##############################################

# `rent` ------------------------------------------------------------------

# `rent` is roughly normal, with or without imputation
monthly_sept |> 
  # impute() |> 
  ggplot(aes(rent)) +
  geom_histogram()

# Mild positive skew, with or without imputation
monthly_sept |> 
  # impute() |>
  pull(rent) |> 
  skew()

# Positive outliers above 2500?
monthly_sept |> 
  # impute() |>
  filter(rent < 2500) |> 
  ggplot(aes(rent)) +
  geom_histogram()

# Still somewhat positively skewed
monthly_sept |>
  # impute() |>
  filter(rent < 2500) |> 
  pull(rent) |> 
  skew()

# But `rent` is even more log normal
monthly_sept |> 
  # impute() |>
  mutate(rent_log = log(rent)) |> 
  ggplot(aes(rent_log)) +
  geom_histogram()

# Now very mild negative skew
monthly_sept |>
  # impute() |>
  mutate(rent_log = log(rent)) |> 
  pull(rent_log) |> 
  skew()

# Actually a bit worse with rent < 2500
monthly_sept |> 
  # impute() |>
  filter(rent < 2500) |> 
  mutate(rent_log = log(rent)) |> 
  ggplot(aes(rent_log)) +
  geom_histogram()

# Skew increases
monthly_sept |>
  # impute() |>
  filter(rent < 2500) |>
  mutate(rent_log = log(rent)) |> 
  pull(rent_log) |> 
  skew()

# So tentatively use logged version of rent but do not delete any outliers
# (Revisit in multivariate context)


## Rent model explanatory IVs ##################################################

# `FREH` ------------------------------------------------------------------

# FREH has extreme positive skew
monthly_sept |> 
  ggplot(aes(FREH)) +
  geom_histogram()

monthly_sept |> 
  pull(FREH) |> 
  skew()

# But FREH is log normal
monthly_sept |> 
  mutate(FREH_log = log(FREH)) |> 
  ggplot(aes(FREH_log)) +
  geom_histogram()

# Now with mild positive skew
monthly_sept |> 
  mutate(FREH_log = log(FREH)) |> 
  filter(!is.infinite(FREH_log)) |> 
  pull(FREH_log) |> 
  skew()

# To preserve zero values, shift all zero values to the minimum non-zero value,
# and add dummy
monthly_sept |> 
  mutate(FREH = if_else(FREH == 0, min(FREH[FREH > 0]), FREH)) |>
  mutate(FREH_log = log(FREH)) |> 
  ggplot(aes(FREH_log)) +
  geom_histogram()


# `FREH_3` ----------------------------------------------------------------

# FREH_3 has extreme positive skew
monthly_sept |> 
  ggplot(aes(FREH_3)) +
  geom_histogram()

monthly_sept |> 
  pull(FREH_3) |> 
  skew()

# But FREH_3 is log normal
monthly_sept |> 
  mutate(FREH_3_log = log(FREH_3)) |> 
  ggplot(aes(FREH_3_log)) +
  geom_histogram()

# Now with mild positive skew
monthly_sept |> 
  mutate(FREH_3_log = log(FREH_3)) |> 
  filter(!is.infinite(FREH_3_log)) |> 
  pull(FREH_3_log) |> 
  skew()

# To preserve zero values, shift all zero values to the minimum non-zero value,
# and add dummy
monthly_sept |> 
  mutate(FREH_3 = if_else(FREH_3 == 0, min(FREH_3[FREH_3 > 0]), FREH_3)) |>
  mutate(FREH_3_log = log(FREH_3)) |> 
  ggplot(aes(FREH_3_log)) +
  geom_histogram()


# `FREH` vs. `rent` -------------------------------------------------------

# No serious outliers in rent, but arguably could remove rent_log > 7.9, when 
# examined in a bivariate relationship with FREH
monthly_sept |> 
  mutate(FREH = if_else(FREH == 0, min(FREH[FREH > 0]), FREH)) |>
  mutate(FREH_log = log(FREH),
         rent_log = log(rent)) |>
  filter(!is.na(rent_log)) |> 
  filter(rent_log < 7.9) |> 
  ggplot(aes(FREH_log, rent_log)) +
  geom_point() +
  geom_smooth(method = "lm")

# Use FREH_log


# `FREH_3` vs. `rent` -----------------------------------------------------

# No serious outliers in rent, but arguably could remove rent_log > 7.9, when 
# examined in a bivariate relationship with FREH_3
monthly_sept |> 
  mutate(FREH_3 = if_else(FREH_3 == 0, min(FREH_3[FREH_3 > 0]), FREH_3)) |>
  mutate(FREH_3_log = log(FREH_3), rent_log = log(rent)) |>
  filter(!is.na(rent_log)) |>
  filter(rent_log < 7.9) |> 
  ggplot(aes(FREH_3_log, rent_log)) +
  geom_point() +
  geom_smooth(method = "lm")

# Use FREH_3_log


# `rev` -------------------------------------------------------------------

# rev has extreme positive skew
monthly_sept |> 
  ggplot(aes(rev)) +
  geom_histogram()

monthly_sept |> 
  pull(rev) |> 
  skew()

# But rev is log normal
monthly_sept |> 
  mutate(rev_log = log(rev)) |> 
  ggplot(aes(rev_log)) +
  geom_histogram()

# Now with mild negative skew
monthly_sept |> 
  mutate(rev_log = log(rev)) |> 
  filter(!is.infinite(rev_log)) |> 
  pull(rev_log) |> 
  skew()

# Removing extreme positive outliers may be theoretically preferable (cases 
# with more STR revenue than traditional rent are out of bounds for the model)
monthly_sept |> 
  filter(rev < 0.5) |> 
  ggplot(aes(rev)) +
  geom_histogram()

monthly_sept |> 
  filter(rev < 0.5) |> 
  pull(rev) |> 
  skew()

monthly_sept |> 
  filter(rev < 0.5) |>
  mutate(rev_log = log(rev)) |> 
  ggplot(aes(rev_log)) +
  geom_histogram()

# Skew is worse after removing outliers
monthly_sept |> 
  filter(rev < 0.5) |>
  mutate(rev_log = log(rev)) |> 
  filter(!is.infinite(rev_log)) |> 
  pull(rev_log) |> 
  skew()

# To preserve zero values, shift all zero values to the minimum non-zero value,
# and add dummy
monthly_sept |> 
  mutate(rev = if_else(rev == 0, min(rev[rev > 0]), rev)) |>
  mutate(rev_log = log(rev)) |> 
  ggplot(aes(rev_log)) +
  geom_histogram()


# `rev` vs. `rent` --------------------------------------------------------

# Arguably could remove rent_log > 7.9 and rent_log < 6, and rev_log < -9.9,
# when examining a bivariate relationship of rent_log vs. rev_log
monthly_sept |> 
  mutate(rev = if_else(rev == 0, min(rev[rev > 0]), rev)) |>
  mutate(rev_log = log(rev),
         rent_log = log(rent)) |>
  filter(rent_log < 7.9, rent_log > 6, rev_log > -9.9) |>
  filter(!is.infinite(rev_log), !is.na(rent_log)) |>
  ggplot(aes(rev_log, rent_log)) +
  geom_point() +
  geom_smooth(method = "lm")

# Use rev_log


# FREH and rev together? --------------------------------------------------

monthly_sept |> 
  st_drop_geometry() |> 
  select(FREH, rev) |> 
  mutate(FREH_log = log(FREH), rev_log = log(rev)) |> 
  filter(!is.infinite(FREH_log), !is.infinite(rev_log)) |> 
  cor(use = "complete.obs")

# Low correlation between FREH and rev, and reasonably low correlation between 
# FREH_log and rev_log, so (pending VIF) safe to use both variables together


# DV/IV outlier removal ---------------------------------------------------

# Remove rent where rent_log > 7.9 or rent_log < 6, which is > 2700 or < 400
exp(7.9); exp(6)

# Remove FREH > 1
# Remove FREH_3 > 1

# Remove rev where rev_log < -9.9, which is 5.017468e-05
exp(-9.9)

# Final filter? From 6192 complete obs to 6042, so 150 dropped obs (2.4%)
monthly_sept |> 
  filter(!is.na(rent), !is.na(FREH), !is.na(rev)) |> 
  filter(rent < 2700, rent > 400, rev > 5.017468e-05)

# Use this as an alternate specification, and default to no outlier removal
  

## Rent model control variables ################################################

# `EH` --------------------------------------------------------------------

# EH roughly normal, with inflated incidences at 0 and 1
monthly_sept |> 
  ggplot(aes(EH)) +
  geom_histogram()

# Moderate negative skew
monthly_sept |> 
  pull(EH) |> 
  skew()

# Use EH, no outliers


# `universe` --------------------------------------------------------------

# universe has extreme positive skew
monthly_sept |> 
  ggplot(aes(universe)) +
  geom_histogram()

monthly_sept |> 
  pull(universe) |> 
  skew()

# But universe is log normal
monthly_sept |> 
  mutate(universe_log = log(universe)) |> 
  ggplot(aes(universe_log)) +
  geom_histogram()

# Now with mild negative skew
monthly_sept |> 
  mutate(universe_log = log(universe)) |> 
  pull(universe_log) |> 
  skew()

# Use universe_log, and no obvious outliers to remove


# `tenant` ----------------------------------------------------------------

# tenant is very roughly normal
monthly_sept |> 
  ggplot(aes(tenant)) +
  geom_histogram()

# With minor positive skew
monthly_sept |> 
  pull(tenant) |> 
  skew()

# Not really log normal
monthly_sept |> 
  mutate(tenant_log = log(tenant)) |> 
  ggplot(aes(tenant_log)) +
  geom_histogram()

# Now with negative skew
monthly_sept |> 
  mutate(tenant_log = log(tenant)) |> 
  pull(tenant_log) |> 
  skew()

# Use tenant, and no outliers to remove


# `tourism` ---------------------------------------------------------------

# tourism is very roughly normal with positive skew
monthly_sept |> 
  ggplot(aes(tourism)) +
  geom_histogram()

monthly_sept |> 
  pull(tourism) |> 
  skew()

# But tourism is log normal
monthly_sept |> 
  mutate(tourism_log = log(tourism)) |> 
  ggplot(aes(tourism_log)) +
  geom_histogram()

# Still with a bit of positive skew
monthly_sept |> 
  mutate(tourism_log = log(tourism)) |> 
  pull(tourism_log) |> 
  skew()

# Skew diminishes with removal of positive outliers, but it is already minor
monthly_sept |> 
  mutate(tourism_log = log(tourism)) |> 
  filter(tourism_log < -1.5) |> 
  pull(tourism_log) |> 
  skew()

# Use tourism_log, and no outliers to remove


# `pop_CMA_log` -----------------------------------------------------------

# pop_CMA is discontinuous at the zone scale
monthly_sept |> 
  ggplot(aes(pop_CMA)) +
  geom_histogram()

# And even at the CMA scale it is not much better
monthly_sept |> 
  slice(1, .by = CMA) |> 
  ggplot(aes(pop_CMA)) +
  geom_histogram() +
  scale_x_log10()

# Log doesn't help much
monthly_sept |> 
  slice(1, .by = CMA) |> 
  ggplot(aes(pop_CMA)) +
  geom_histogram() +
  scale_x_log10()

# Best to exclude pop_CMA


# `vacancy` ---------------------------------------------------------------

# vacancy is roughly normal with positive skew
monthly_sept |> 
  ggplot(aes(vacancy)) +
  geom_histogram()

monthly_sept |> 
  pull(vacancy) |> 
  skew()

# Hard to apply log transformation because of zero values
monthly_sept |> 
  mutate(vacancy_log = log(vacancy + 2)) |> 
  ggplot(aes(vacancy_log)) +
  geom_histogram()

# Also many missing or suspect values
monthly_sept |> 
  st_drop_geometry() |> 
  summarize(missing = sum(is.na(vacancy)),
            bad = sum(vacancy_rel == "d"),
            good = n() - missing - bad)

# So don't include vacancy in main models


## Full variable specification for rent model ##################################

# Remove outliers
monthly_sept |> 
  filter(rent < 2700, rent > 400, FREH < 1, FREH_3 < 1, rev < 5, 
         rev > 1.67017e-05)

# Choose variables
# id, year, rent_log, FREH_log, FREH_3_log, rev_log, universe_log, tenant, 
# tourism_log


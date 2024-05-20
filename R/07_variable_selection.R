#### 05 VARIABLE SELECTION #####################################################

source("R/01_startup.R")
qload("output/cmhc.qsm", nthreads = availableCores())
monthly_sept <- qread("output/monthly_sept.qs", nthreads = availableCores())

skew <- function(x) mean(scale(x) ^ 3, na.rm = TRUE)


## Impute missing rent values for spatial panel ################################

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


################################################################################
################################################################################


## Rent change model dependent variables #######################################

# `rent_change` -----------------------------------------------------------

# `rent_change` is roughly normal with long symmetrical tails
monthly_sept |> 
  ggplot(aes(rent_change)) +
  geom_histogram()

# Mild positive skew
monthly_sept |> 
  pull(rent_change) |> 
  skew()

# Outliers above +/- 400?
monthly_sept |> 
  filter(abs(rent_change) < 400) |>
  ggplot(aes(rent_change)) +
  geom_histogram()

# Still somewhat positively skewed
monthly_sept |> 
  filter(abs(rent_change) < 400) |> 
  pull(rent_change) |> 
  skew()

# `rent_change` is not log normal
monthly_sept |> 
  mutate(rent_change_log = log(rent_change)) |> 
  ggplot(aes(rent_change_log)) +
  geom_histogram()

# So tentatively use non-logged rent_change with outliers > +/- 400 removed
# (Revisit in multivariate context)


## Rent change model explanatory IVs ###########################################

# `FREH_change` -----------------------------------------------------------

# FREH_change has long tails
monthly_sept |> 
  ggplot(aes(FREH_change)) +
  geom_histogram()

# Moderate positive skew
monthly_sept |> 
  pull(FREH_change) |> 
  skew()

# There is no reasonable outlier threshold that establishes normalcy, and no
# strong theoretical justification for removing extreme values
monthly_sept |> 
  # filter(abs(FREH_change) < 0) |>
  ggplot(aes(FREH_change)) +
  geom_histogram()


# `FREH_count_change` -----------------------------------------------------

# FREH_count_change has extreme outliers
monthly_sept |> 
  ggplot(aes(FREH_count_change)) +
  geom_histogram()

# Extreme negative skew
monthly_sept |> 
  pull(FREH_count_change) |> 
  skew()

# There is no reasonable outlier threshold that establishes normalcy
monthly_sept |> 
  filter(abs(FREH_count_change) < 100) |> 
  ggplot(aes(FREH_count_change)) +
  geom_histogram()


# `FREH_3_change` ---------------------------------------------------------

# FREH_3_change has long tails
monthly_sept |> 
  ggplot(aes(FREH_3_change)) +
  geom_histogram()

# No skew
monthly_sept |> 
  pull(FREH_3_change) |> 
  skew()

# No obvious need to remove values in the tails
monthly_sept |> 
  filter(abs(FREH_3_change) < 0.01) |> 
  ggplot(aes(FREH_3_change)) +
  geom_histogram()


# `FREH_3_count_change` ---------------------------------------------------

# FREH_3_count_change has very long tails
monthly_sept |> 
  ggplot(aes(FREH_3_count_change)) +
  geom_histogram()

# Extreme negative skew
monthly_sept |> 
  pull(FREH_3_count_change) |> 
  skew()

# There is no reasonable outlier threshold that establishes normalcy
monthly_sept |> 
  filter(abs(FREH_3_count_change) < 100) |> 
  ggplot(aes(FREH_3_count_change)) +
  geom_histogram()

# FREH_3_count_change has more extreme values than FREH_count_change
monthly_sept |> 
  ggplot(aes(FREH_count_change, FREH_3_count_change)) +
  geom_point()


# `FREH_change` vs. `rent_change` -----------------------------------------

monthly_sept |> 
  # Maybe rent_change < 400 is too strict? Instead could try -400/600
  # filter(abs(rent_change) < 400) |>
  filter(rent_change > -400, rent_change < 600) |>
  # Arguably could remove abs(FREH_change) < 0.01
  filter(abs(FREH_change) < 0.01) |>
  ggplot(aes(FREH_change, rent_change)) +
  geom_point() +
  geom_smooth(method = "lm")


# `FREH_count_change` vs. `rent_change` -----------------------------------

# No outliers
monthly_sept |> 
  filter(rent_change > -400, rent_change < 600) |>
  filter(abs(FREH_count_change) < 200) |>
  ggplot(aes(FREH_count_change, rent_change)) +
  geom_point() +
  geom_smooth(method = "lm")


# `FREH_3_change` vs. `rent_change` ---------------------------------------

monthly_sept |> 
  filter(rent_change > -400, rent_change < 600) |>
  # Could remove abs(FREH_3_change) < 0.015
  filter(abs(FREH_3_change) < 0.015) |>
  ggplot(aes(FREH_3_change, rent_change)) +
  geom_point() +
  geom_smooth(method = "lm")


# `FREH_3_count_change` vs. `rent_change` ---------------------------------

monthly_sept |> 
  filter(rent_change > -400, rent_change < 600) |>
  filter(abs(FREH_3_count_change) < 250) |> 
  ggplot(aes(FREH_3_count_change, rent_change)) +
  geom_point() +
  geom_smooth(method = "lm")


# `rev_change` ------------------------------------------------------------

# rev_change has long tails
monthly_sept |> 
  ggplot(aes(rev_change)) +
  geom_histogram()

# Minor positive skew
monthly_sept |> 
  pull(rev_change) |> 
  skew()

# There is no reasonable outlier threshold that establishes normalcy
monthly_sept |> 
  filter(abs(rev_change) < 0.2) |>
  ggplot(aes(rev_change)) +
  geom_histogram()


# `rev_count_change` ------------------------------------------------------

# rev_count_change has extreme outliers
monthly_sept |> 
  ggplot(aes(rev_count_change)) +
  geom_histogram()

# Heavy negative skew
monthly_sept |> 
  pull(rev_count_change) |> 
  skew()

# There is no reasonable outlier threshold that establishes normalcy
monthly_sept |> 
  filter(abs(rev_count_change) < 500000) |> 
  ggplot(aes(rev_count_change)) +
  geom_histogram()


# `rev_change` vs. `rent_change` ------------------------------------------

monthly_sept |> 
  filter(rent_change > -400, rent_change < 600) |>
  # Could remove abs(rev_change) < 0.2
  filter(abs(rev_change) < 0.2) |>
  ggplot(aes(rev_change, rent_change)) +
  geom_point() +
  geom_smooth(method = "lm")


# `rev_share_change` vs. `rent_change` ------------------------------------

monthly_sept |> 
  filter(rent_change > -400, rent_change < 600) |>
  # Could remove abs(rev_count_change) < 2500000
  filter(abs(rev_count_change) < 2500000) |>
  ggplot(aes(rev_count_change, rent_change)) +
  geom_point() +
  geom_smooth(method = "lm")


# DV/IV outlier removal ---------------------------------------------------

# Remove rent_change > -400, rent_change < 600
# Remove abs(FREH_change) < 0.01
# Remove abs(rev_change) < 0.2
# Remove abs(rev_count_change) < 2500000

# Final filter? From 5172 complete obs to 5150, so 22 dropped obs (0.4%)
monthly_sept |> 
  filter(!is.na(rent_change)) |> 
  filter(rent_change > -400, rent_change < 600,
         abs(FREH_change) < 0.01, abs(rev_change) < 0.2)


## Rent change model control variables #########################################

# `universe_change` -------------------------------------------------------

# universe_change has very long tails
monthly_sept |> 
  ggplot(aes(universe_change)) +
  geom_histogram()

# Positive skew
monthly_sept |> 
  pull(universe_change) |> 
  skew()

# There is no reasonable outlier threshold that establishes normalcy, and no
# strong theoretical justification for removing extreme values, but could clip
# at +- 500 or 300
monthly_sept |> 
  filter(abs(universe_change) < 500) |> 
  ggplot(aes(universe_change)) +
  geom_histogram()


## Full variable specification for rent_change model ###########################

# Remove outliers
monthly_sept |> 
  filter(!is.na(rent_change)) |> 
  filter(rent_change > -400, rent_change < 600,
         abs(FREH_change) < 0.01, abs(rev_change) < 0.2)

# Choose variables
# year, rent_change, FREH_change, FREH_count_change, FREH_3_change, 
# FREH_3_count_change, rev_change, rev_count_change, universe_change, 
# universe_log, tenant_share, tourism_log

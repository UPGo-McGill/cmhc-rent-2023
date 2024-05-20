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

# Now very mild positive skew
monthly_sept |>
  # impute() |>
  mutate(rent_log = log(rent)) |> 
  pull(rent_log) |> 
  skew()

# A bit better with rent < 2500
monthly_sept |> 
  # impute() |>
  filter(rent < 2500) |> 
  mutate(rent_log = log(rent)) |> 
  ggplot(aes(rent_log)) +
  geom_histogram()

# Skew decreases a small amount
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
  mutate(FREH = if_else(FREH == 0, min(FREH[FREH > 0], na.rm = TRUE), FREH)) |>
  ggplot(aes(FREH_log)) +
  geom_histogram()


# `non_FREH` --------------------------------------------------------------

# non_FREH has extreme positive skew
monthly_sept |> 
  ggplot(aes(non_FREH)) +
  geom_histogram()

monthly_sept |> 
  pull(non_FREH) |> 
  skew()

# But non_FREH is log normal
monthly_sept |> 
  mutate(non_FREH_log = log(non_FREH)) |> 
  ggplot(aes(non_FREH_log)) +
  geom_histogram()

# Now with mild positive skew
monthly_sept |> 
  mutate(non_FREH_log = log(non_FREH)) |> 
  filter(!is.infinite(non_FREH_log)) |> 
  pull(non_FREH_log) |> 
  skew()

# To preserve zero values, shift all zero values to the minimum non-zero value,
# and add dummy
monthly_sept |> 
  mutate(non_FREH = if_else(non_FREH == 0, min(
    non_FREH[non_FREH > 0], na.rm = TRUE), non_FREH)) |>
  mutate(non_FREH_log = log(non_FREH)) |> 
  ggplot(aes(non_FREH_log)) +
  geom_histogram()


# `FREH_lag` vs. `rent` ---------------------------------------------------

# No serious outliers in rent or FREH_lag
monthly_sept |> 
  mutate(FREH_lag = if_else(FREH_lag == 0, min(
    FREH_lag[FREH_lag > 0], na.rm = TRUE), FREH_lag)) |>
  mutate(FREH_lag_log = log(FREH_lag),
         rent_log = log(rent)) |>
  filter(!is.na(rent_log)) |>
  filter(rent_log < 7.9) |>
  ggplot(aes(FREH_lag_log, rent_log)) +
  geom_point() +
  geom_smooth(method = "lm")


# `non_FREH` vs. `rent` ---------------------------------------------------

# No serious outliers in rent, but arguably could remove rent_log > 7.9, when 
# examined in a bivariate relationship with non_FREH
monthly_sept |> 
  mutate(non_FREH = if_else(non_FREH == 0, min(
    non_FREH[non_FREH > 0], na.rm = TRUE), non_FREH)) |>
  mutate(non_FREH_log = log(non_FREH), rent_log = log(rent)) |> 
  filter(!is.na(rent_log)) |>
  # filter(rent_log < 7.9) |> 
  ggplot(aes(non_FREH_log, rent_log)) +
  geom_point() +
  geom_smooth(method = "lm")


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

# Skew is slightly worse after removing outliers
monthly_sept |> 
  filter(rev < 0.5) |>
  mutate(rev_log = log(rev)) |> 
  filter(!is.infinite(rev_log)) |> 
  pull(rev_log) |> 
  skew()

# To preserve zero values, shift all zero values to the minimum non-zero value,
# and add dummy
monthly_sept |> 
  mutate(rev = if_else(rev == 0, min(rev[rev > 0], na.rm = TRUE), rev)) |>
  mutate(rev_log = log(rev)) |> 
  ggplot(aes(rev_log)) +
  geom_histogram()


# `rev` vs. `rent` --------------------------------------------------------

# Arguably could remove rent_log > 7.9 and rev_log < -10.5,
# when examining a bivariate relationship of rent_log vs. rev_log
monthly_sept |> 
  mutate(rev = if_else(rev == 0, min(rev[rev > 0], na.rm = TRUE), rev)) |>
  mutate(rev_log = log(rev), rent_log = log(rent)) |>
  filter(rent_log < 7.9, rev_log > -10.5) |>
  filter(!is.infinite(rev_log), !is.na(rent_log)) |>
  ggplot(aes(rev_log, rent_log)) +
  geom_point() +
  geom_smooth(method = "lm")


# `price` -----------------------------------------------------------------

# price has moderate positive skew
monthly_sept |> 
  ggplot(aes(price)) +
  geom_histogram()

monthly_sept |> 
  pull(price) |> 
  skew()

# But price is log normal
monthly_sept |> 
  mutate(price_log = log(price)) |> 
  ggplot(aes(price_log)) +
  geom_histogram()

# Now with mild negative skew
monthly_sept |> 
  mutate(price_log = log(price)) |> 
  filter(!is.infinite(price_log)) |> 
  pull(price_log) |> 
  skew()

# To preserve zero values, shift all zero values to the minimum non-zero value,
# and add dummy
monthly_sept |> 
  mutate(price = if_else(price == 0, min(price[price > 0], na.rm = TRUE), 
                         price)) |>
  mutate(price_log = log(price)) |> 
  ggplot(aes(price_log)) +
  geom_histogram()


# `price` vs. `rent` ------------------------------------------------------

# Arguably could remove rent_log > 7.9 and price_log < 3.4 and > 6.5,
# when examining a bivariate relationship of rent_log vs. rev_log
monthly_sept |> 
  mutate(price = if_else(price == 0, min(price[price > 0], na.rm = TRUE), 
                         price)) |>
  mutate(price_log = log(price), rent_log = log(rent)) |>
  # filter(rent_log < 7.9, price_log > 3.4, price_log < 6.5) |>
  filter(!is.infinite(price_log), !is.na(rent_log)) |>
  ggplot(aes(price_log, rent_log)) +
  geom_point() +
  geom_smooth(method = "lm")



# FREH, rev and price together? -------------------------------------------

monthly_sept |> 
  st_drop_geometry() |> 
  select(FREH, rev, price) |> 
  mutate(FREH_log = log(FREH), rev_log = log(rev), price_log = log(price)) |> 
  filter(!is.infinite(FREH_log), !is.infinite(rev_log), 
         !is.infinite(price_log)) |> 
  cor(use = "complete.obs")

# Low correlation between unlogged variables, and reasonably low correlation 
# between logged ones, so (pending VIF) safe to use all variables together


# DV/IV outlier removal ---------------------------------------------------

# Remove rent where rent_log > 7.9, which is > 2700
exp(7.9)

# Remove FREH > 1

# Remove rev where rev_log < -10.5, which is 2.753645e-05
exp(-10.5)

# Remove price where price_log < 3.4 or > 6.5, which are 29.9641 and 665.1416
exp(3.4); exp(6.5)

# Final filter? From 7909 complete obs to 6804, so 1105 dropped obs (14.0%)
monthly_sept |> 
  filter(!is.na(rent), !is.na(rent_lag), !is.na(FREH_lag), !is.na(rev_lag),
         !is.na(price_lag)) |> 
  filter(rent < 2700, rev_lag > 2.753645e-05, price_lag > 29.9641, 
         price_lag < 665.1416)

# Use this as an alternate specification, and default to no outlier removal
  

## Rent model control variables ################################################

# `universe_change` -------------------------------------------------------

# universe_change has extreme outliers
monthly_sept |> 
  ggplot(aes(universe_change)) +
  geom_histogram()

# Extreme negative skew
monthly_sept |> 
  pull(universe_change) |> 
  skew()

# Zero-centered, so no possibility of log transformation

# Outliers?
monthly_sept |> 
  filter(abs(universe_change) < 1) |> 
  ggplot(aes(universe_change)) +
  geom_histogram()

# Only 7 with abs(universe_change) > 1, so not terrible to remove
monthly_sept |> 
  filter(abs(universe_change) >= 1) |> 
  filter(!is.na(rent), !is.na(rent_lag), !is.na(FREH_lag), !is.na(rev_lag),
         !is.na(price_lag), !is.na(non_FREH_lag))

# Use universe_change, remove outliers for alternate specification


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

# So don't include vacancy in main models, or only when imputing values


# `income` ----------------------------------------------------------------

# income is very roughly normal with positive skew
monthly_sept |> 
  ggplot(aes(income)) +
  geom_histogram()

monthly_sept |> 
  pull(income) |> 
  skew()

# But income is log normal
monthly_sept |> 
  mutate(income_log = log(income)) |> 
  ggplot(aes(income_log)) +
  geom_histogram()

# Still with a bit of positive skew
monthly_sept |> 
  mutate(income_log = log(income)) |> 
  pull(income_log) |> 
  skew()

# Skew diminishes with removal of positive outliers, but it is already minor
monthly_sept |> 
  mutate(income_log = log(income)) |> 
  filter(income_log < 12.5) |> 
  pull(income_log) |> 
  skew()

# Use income_log, and no outliers to remove


# `apart` -----------------------------------------------------------------

# apart is very roughly normal with positive skew
monthly_sept |> 
  ggplot(aes(apart)) +
  geom_histogram()

monthly_sept |> 
  pull(apart) |> 
  skew()

# Log doesn't improve normality
monthly_sept |> 
  mutate(apart_log = log(apart)) |> 
  ggplot(aes(apart_log)) +
  geom_histogram()

# Use apart, and no outliers to remove


## Full variable specification for rent model ##################################

# Remove outliers
monthly_sept |> 
  filter(!is.na(rent), !is.na(rent_lag), !is.na(FREH_lag), !is.na(rev_lag),
         !is.na(price_lag)) |> 
  filter(rent < 2700, rev_lag > 2.753645e-05, price_lag > 29.9641, 
         price_lag < 665.1416, abs(universe_change) < 1)

# Choose variables
# id, year, rent_log, rent_lag_log, FREH_lag_log, rev_lag_log, price_lag_log,
# universe_change, tourism_log, income_log, apart


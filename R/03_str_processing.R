#### STR PROCESSING ############################################################

source("R/01_startup.R")
qload("output/cmhc.qsm", nthreads = availableCores())


# Import raw monthly file -------------------------------------------------

monthly_raw <- read_csv("data/monthly_raw.csv")

monthly <- 
  monthly_raw |> 
  mutate(property_ID = `Property ID`,
         month = `Reporting Month`, 
         host_ID = coalesce(as.character(`Airbnb Host ID`), 
                            `HomeAway Property Manager`),
         listing_type = `Listing Type`,
         country = Country,
         region = State,
         city = City,
         R = `Reservation Days`,
         A = `Available Days`,
         B = `Blocked Days`,
         rev = `Revenue (USD)`,
         latitude = `Latitude`,
         longitude = `Longitude`,
         scraped = `Scraped During Month`,
         .keep = "none") |> 
  mutate(month = yearmonth(month)) |> 
  filter(!region %in% c("Idaho", "Vermont"))


# Fill in missing provinces -----------------------------------------------

missing_provinces <- 
  monthly |> 
  filter(is.na(region)) |> 
  slice(1, .by = property_ID) |> 
  strr_as_sf(3347) |>
  mutate(prov = unlist(nngeo::st_nn(geometry, st_transform(province, 3347)))) |> 
  mutate(prov = province$province[prov]) |> 
  st_drop_geometry() |> 
  select(property_ID, prov) |> 
  mutate(prov = if_else(prov == "Quebec", "Québec", prov))

monthly <- 
  monthly |> 
  left_join(missing_provinces, by = "property_ID") |> 
  mutate(region = coalesce(prov, region)) |> 
  select(-prov)

rm(missing_provinces)


# Trim file by last scraped date ------------------------------------------

monthly <- 
  monthly |> 
  filter(month <= max(month[scraped]), .by = property_ID)

# Additional clean up
monthly <- 
  monthly |> 
  filter(!is.na(R), !is.na(A), !is.na(B)) |> 
  mutate(rev = coalesce(rev, 0)) |> 
  arrange(property_ID, month)


# Convert currency --------------------------------------------------------

exchange_rates <- qread("data/exchange_rates.qs")

monthly <-
  monthly |>
  inner_join(mutate(exchange_rates, month = yearmonth(year_month)), 
            by = "month") |> 
  mutate(rev = rev * exchange_rate) |> 
  select(-year_month, -exchange_rate)

rm(exchange_rates)


# Save intermediate output ------------------------------------------------

qsave(monthly, file = "output/monthly.qs", nthreads = availableCores())
monthly <- qread("output/monthly.qs", nthreads = availableCores())


# Get monthly/CMHC correspondence -----------------------------------------

# prop_cmhc_id <-
#   monthly |>
#   distinct(property_ID, .keep_all = TRUE) |> 
#   strr_as_sf(3347) |>
#   select(property_ID, geometry) |>
#   st_join(st_transform(cmhc_nbhd, 3347)) |>
#   st_drop_geometry() |>
#   select(property_ID, year, id, province, CMA) |>
#   filter(!is.na(id)) |>
#   slice(1, .by = c(property_ID, year))
# 
# qsave(prop_cmhc_id, "output/prop_cmhc_id.qs", nthreads = availableCores())

prop_cmhc_id <- qread("output/prop_cmhc_id.qs", nthreads = availableCores())


# Join to monthly ---------------------------------------------------------

monthly_year <- 
  monthly |> 
  mutate(year = year(month)) |> 
  inner_join(prop_cmhc_id, by = c("property_ID", "year")) |> 
  inner_join(cmhc, by = c("id", "year"))

# Add FREH
monthly_year <- 
  monthly_year |> 
  arrange(property_ID, month) |> 
  mutate(FREH = slide2_lgl(A, R, \(A, R) sum(A) + sum(R) >= 183 & sum(R) > 90, 
                           .before = 11), .after = B)


# Make version for April data ---------------------------------------------

monthly_april <-
  monthly_year |> 
  filter(month(month) == 4) |> 
  mutate(year = year(month)) |> 
  summarize(
    active = sum(A + R) / 30,
    res = sum(R) / 30,
    rev = sum(rev), 
    FREH = sum(FREH),
    .by = c(id, year, rent, vacancy, universe)) |> 
  arrange(id, year) |> 
  mutate(
    active_share = active / universe,
    res_share = res / universe,
    rev_share = rev / (rent * universe),
    FREH_share = FREH / universe) |> 
  mutate(
    across(c(rent:FREH_share), \(x) slide_dbl(x, \(y) y[2] - y[1], .before = 1),
           .names = "{.col}_change"), .by = id) |> 
  mutate(rent_change_pct = rent_change / rent,
         active_change_pct = active_change / active,
         rev_change_pct = rev_change / rev,
         FREH_change_pct = FREH_change / FREH)

qsave(monthly_april, "output/monthly_april.qs", nthreads = availableCores())

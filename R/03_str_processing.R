#### 03 STR PROCESSING #########################################################

source("R/01_startup.R")
qload("output/cmhc.qsm", nthreads = availableCores())
province <- qread("output/province.qs", nthreads = availableCores())
CSD <- qread("output/CSD.qs", nthreads = availableCores())


# # Import raw monthly file -------------------------------------------------
# 
# monthly <- read_csv("data/monthly_raw.csv")
# 
# monthly <-
#   monthly |>
#   mutate(property_ID = `Property ID`,
#          month = `Reporting Month`,
#          host_ID = coalesce(as.character(`Airbnb Host ID`),
#                             `HomeAway Property Manager`),
#          listing_type = `Listing Type`,
#          property_type = `Property Type`,
#          country = Country,
#          region = State,
#          city = City,
#          R = `Reservation Days`,
#          A = `Available Days`,
#          B = `Blocked Days`,
#          rev = `Revenue (USD)`,
#          latitude = `Latitude`,
#          longitude = `Longitude`,
#          scraped = `Scraped During Month`,
#          .keep = "none") |>
#   mutate(month = yearmonth(month)) |>
#   filter(!region %in% c("Idaho", "Vermont"))
# 
# 
# # Remove Vrbo listings, because they aren't added until 2017 --------------
# 
# monthly <- 
#   monthly |> 
#   filter(str_starts(property_ID, "ab-"))
# 
# 
# # Fill in missing provinces -----------------------------------------------
# 
# missing_provinces <-
#   monthly |>
#   filter(is.na(region)) |>
#   slice(1, .by = property_ID) |>
#   strr_as_sf(3347) |>
#   mutate(prov = unlist(nngeo::st_nn(
#     geometry, st_transform(province, 3347)))) |>
#   mutate(prov = province$province[prov]) |>
#   st_drop_geometry() |>
#   select(property_ID, prov) |>
#   mutate(prov = if_else(prov == "Quebec", "Québec", prov))
# 
# monthly <-
#   monthly |>
#   left_join(missing_provinces, by = "property_ID") |>
#   mutate(region = coalesce(prov, region)) |>
#   select(-prov)
# 
# rm(missing_provinces)
# 
# 
# # Trim file by first and last scraped date --------------------------------
# 
# monthly <-
#   monthly |>
#   filter(month <= max(month[scraped]),
#          month >= min(month[scraped]),
#          .by = property_ID) |>
#   select(-scraped)
# 
# 
# # Fill in missing months, and clean up ------------------------------------
# 
# monthly <-
#   monthly |>
#   filter(!is.na(listing_type)) |>
#   as_tsibble(key = property_ID, index = month) |>
#   fill_gaps(
#     host_ID = first(host_ID),
#     listing_type = first(listing_type),
#     property_type = first(property_type),
#     country = first(country),
#     region = first(region),
#     city = first(city),
#     latitude = first(latitude),
#     longitude = first(longitude)) |>
#   as_tibble() |>
#   mutate(across(c(R, A, B, rev), \(x) coalesce(x, 0))) |>
#   arrange(property_ID, month) |> 
#   mutate(rev = if_else(R == 0, 0, rev))
# 
# 
# # Convert currency --------------------------------------------------------
# 
# exchange_rates <- qread("data/exchange_rates.qs")
# 
# monthly <-
#   monthly |>
#   inner_join(mutate(exchange_rates, month = yearmonth(year_month)),
#             by = "month") |>
#   mutate(rev = rev * exchange_rate) |>
#   select(-year_month, -exchange_rate)
# 
# rm(exchange_rates)
# 
# 
# # Calculate FREH ----------------------------------------------------------
# 
# # Add FREH
# monthly <-
#   monthly |>
#   arrange(property_ID, month) |>
#   mutate(FREH = listing_type == "Entire home/apt" & slide2_lgl(
#     A, R, \(A, R) sum(A) + sum(R) >= 183 & sum(R) > 90, .before = 11),
#     .by = property_ID, .after = B)
# 
# # Add non-FREH
# monthly <-
#   monthly |>
#   mutate(non_FREH = !FREH & A + R > 0, .after = FREH)
# 
# 
# # Save intermediate output ------------------------------------------------
# 
# qsave(monthly, file = "output/monthly.qs", nthreads = availableCores())
monthly <- qread("output/monthly.qs", nthreads = availableCores())


# Get monthly/CSD correspondence ------------------------------------------
# 
# # Get intersections with st_join
# prop_csd <-
#   monthly |>
#   distinct(property_ID, .keep_all = TRUE) |>
#   strr_as_sf(3347) |>
#   select(property_ID, geometry) |>
#   st_join(CSD) |>
#   arrange(property_ID, CSD) |>
#   slice(1, .by = property_ID) |>
#   select(property_ID, CSD)
# 
# # Use st_nn for non-intersecting points
# prop_csd_2 <-
#   prop_csd |>
#     filter(is.na(CSD)) |>
#     select(property_ID, geometry) |>
#     nngeo::st_nn(CSD, maxdist = 250, parallel = availableCores())
# 
# prop_csd_2 <-
#   prop_csd |>
#   filter(is.na(CSD)) |>
#   rename(CSD_new = CSD) |> 
#   mutate(CSD_new = map_chr(prop_csd_2, \(x) {
#     if (length(x) == 0) NA_character_ else CSD$CSD[x]})) |> 
#   rename(CSD = CSD_new)
# 
# prop_csd <-
#   prop_csd |>
#   st_drop_geometry() |>
#   filter(!is.na(CSD)) |>
#   bind_rows(st_drop_geometry(prop_csd_2)) |>
#   arrange(property_ID)
# 
# qsave(prop_csd, "output/prop_csd.qs", nthreads = availableCores())
prop_csd <- qread("output/prop_csd.qs", nthreads = availableCores())

# Join to monthly
monthly <-
  monthly |>
  inner_join(prop_csd, by = "property_ID") |>
  relocate(CSD, .after = city)


# Get monthly/CMHC correspondence -----------------------------------------

# # Start with only points within 250m of CMHC boundary
# monthly_close <-
#   monthly |>
#   distinct(property_ID, .keep_all = TRUE) |>
#   strr_as_sf(3347) |>
#   st_filter(st_buffer(st_union(cmhc_nbhd), 250))
# 
# # Get intersections with st_join
# prop_cmhc <-
#   monthly_close |>
#   select(property_ID, geometry) |>
#   st_join(cmhc_nbhd) |>
#   arrange(property_ID, id) |>
#   slice(1, .by = property_ID) |>
#   select(property_ID, id, province, CMA, name_CMA)
# 
# # Use st_nn for non-intersecting points
# prop_cmhc_2 <-
#   prop_cmhc |>
#     filter(is.na(id)) |>
#     select(property_ID, geometry) |>
#     mutate(id = cmhc_nbhd$id[unlist(nngeo::st_nn(geometry, cmhc_nbhd))]) |>
#   arrange(property_ID) |>
#   st_drop_geometry() |>
#   inner_join(st_drop_geometry(
#     select(cmhc_nbhd, id, province, CMA, name_CMA)), by = "id")
# 
# prop_cmhc <-
#   prop_cmhc |>
#   st_drop_geometry() |>
#   filter(!is.na(id)) |>
#   bind_rows(prop_cmhc_2) |>
#   arrange(property_ID)
# 
# qsave(prop_cmhc, "output/prop_cmhc.qs", nthreads = availableCores())
prop_cmhc <- qread("output/prop_cmhc.qs", nthreads = availableCores())

# Join to monthly
monthly_year <-
  monthly |> 
  mutate(year = year(month)) |>
  inner_join(prop_cmhc, by = "property_ID") |>
  left_join(cmhc, by = c("id", "year"))
  

# Make version for September data -----------------------------------------

monthly_sept <-
  monthly_year |> 
  # Filter to only EH listings
  filter(month(month) == 9, listing_type == "Entire home/apt") |> 
  summarize(
    across(c(rent, rent_rel, vacancy, vacancy_rel, universe), first),
    active_count = sum(A + R) / 30,
    rev_count = sum(rev),
    FREH_count = sum(FREH),
    non_FREH_count = sum(non_FREH),
    price = sum(rev) / sum(R),
    .by = c(id, year)) |> 
  arrange(id, year) |> 
  # Join to CMHC data
  full_join(select(cmhc, id, year, u2 = universe, r2 = rent, rr2 = rent_rel,
                   v2 = vacancy, vr2 = vacancy_rel), by = c("id", "year")) |> 
  mutate(rent = coalesce(rent, r2),
         rent_rel = coalesce(rent_rel, rr2),
         # Convert vacancy rate to proper percentage
         vacancy = coalesce(vacancy, v2) / 100,
         vacancy_rel = coalesce(vacancy_rel, vr2),
         universe = coalesce(universe, u2)) |> 
  select(-c(u2:vr2)) |> 
  # Add CMHC geometries, using full join to get all id/years even without data
  full_join((cmhc_nbhd |> 
               rowwise() |> 
               mutate(year = list(2015:2023)) |> 
               ungroup() |> 
               unnest(year)), by = c("id", "year")) |> 
  arrange(id, year) |> 
  mutate(across(c(active_count, rev_count, FREH_count, non_FREH_count, price), 
                \(x) coalesce(x, 0))) |> 
  mutate(
    active = active_count / dwellings,
    rev = rev_count / (rent_DA * dwellings * tenant + rev_count),
    FREH = FREH_count / dwellings,
    non_FREH = non_FREH_count / dwellings,
    STR = FREH + non_FREH,
    .after = price) |> 
  relocate(rent_rel, vacancy_rel, .after = last_col()) |> 
  arrange(id, year) |> 
  mutate(
    # Add YOY change variables
    across(c(rent:STR), 
           list(change = \(x) slide_dbl(x, \(y) y[2] - y[1], .before = 1))),
    # Add lag variables
    across(c(rent:STR, rent_change:STR_change), list(lag = lag)),
    .by = id) |> 
  # Normalize universe_change by total number of rental units
  mutate(universe_change = universe_change / universe) |> 
  st_as_sf() |> 
  arrange(id, year) |> 
  relocate(geometry, .after = last_col()) |> 
  relocate(name, province, CMA, name_CMA, pop, dwellings, .after = year) |> 
  # Set 2023 STR values to NA
  mutate(across(c(active_count:STR, active_count_change:STR_change),
                \(x) if_else(year == "2023", NA, x)))

qsave(monthly_sept, "output/monthly_sept.qs", nthreads = availableCores())

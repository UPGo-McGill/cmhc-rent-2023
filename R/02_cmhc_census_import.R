#### 02 CMHC AND CENSUS PROCESSING #############################################

source("R/01_startup.R")


# # Import census data ------------------------------------------------------
# 
# province <-
#   get_census("CA21", regions = list(C = 1), level = "PR",
#              geo_format = "sf") |>
#   st_transform(4326) |>
#   select(province = name, province_ID = GeoUID, geometry) |>
#   mutate(province = str_remove(province, " \\(.*\\)"))
# 
# CMA <-
#   get_census("CA21", regions = list(C = 1), level = "CMA",
#              geo_format = "sf") |>
#   st_transform(3347) |>
#   select(CMA = GeoUID, name_CMA = name, pop_CMA = Population,
#          dwellings_CMA = Dwellings, geometry) |>
#   mutate(name_CMA = str_remove(name_CMA, " \\(.*\\)"))
# 
# CSD <-
#   get_census("CA21", regions = list(C = 1), level = "CSD",
#              vectors = c(rent = "v_CA21_4318",
#                          tenants = "v_CA21_4313",
#                          owners = "v_CA21_4305",
#                          entertainment = "v_CA21_6657",
#                          accommodation = "v_CA21_6660",
#                          tourism_parent = "v_CA21_6606",
#                          apart_small = "v_CA21_439",
#                          apart_big = "v_CA21_440",
#                          apart_parent = "v_CA21_434",
#                          income = "v_CA21_915",
#                          income_parent = "v_CA21_914"),
#              geo_format = "sf") |>
#   as_tibble() |>
#   st_as_sf() |>
#   st_transform(3347)
# 
# CSD <-
#   CSD |>
#   select(CSD = GeoUID, name_CSD = name, CMA = CMA_UID, pop_CSD = Population,
#          households_CSD = Households, dwellings_CSD = Dwellings,
#          rent:income_parent, geometry) |>
#   mutate(name_CSD = str_remove(name_CSD, " \\(.*\\)")) |>
#   mutate(province_ID = substr(CSD, 1, 2)) |>
#   inner_join(st_drop_geometry(province), by = "province_ID") |>
#   relocate(province, province_ID, .after = CMA)
# 
# CSD <-
#   CSD |>
#   mutate(tourism = (entertainment + accommodation) / tourism_parent,
#          .after = dwellings_CSD) |>
#   mutate(tenant = tenants / (tenants + owners), .after = tenants) |>
#   rename(tenant_count = tenants) |>
#   mutate(apart = (apart_small + apart_big) / apart_parent,
#          .after = apart_parent) |> 
#   select(-c(owners, entertainment, accommodation, tourism_parent, apart_small,
#             apart_big, apart_parent, income_parent))
# 
# DA <-
#   map(c(35, 24, 59, 48, 46, 47, 12, 13, 10, 11, 61, 60, 62), \(x) {
#     get_census("CA21", regions = list(PR = x), level = "DA",
#                vectors = c(rent = "v_CA21_4318",
#                            tenants = "v_CA21_4313",
#                            owners = "v_CA21_4305",
#                            entertainment = "v_CA21_6657",
#                            accommodation = "v_CA21_6660",
#                            tourism_parent = "v_CA21_6606",
#                            apart_small = "v_CA21_439",
#                            apart_big = "v_CA21_440",
#                            apart_parent = "v_CA21_434",
#                            income = "v_CA21_915",
#                            income_parent = "v_CA21_914"),
#                geo_format = "sf")})
# 
# # Combine output and add area
# DA <-
#   DA |>
#   bind_rows() |>
#   as_tibble() |>
#   st_as_sf() |>
#   st_transform(3347) |>
#   mutate(tourism = entertainment + accommodation) |>
#   mutate(apart = apart_small + apart_big) |> 
#   select(DA = GeoUID, CMA = CMA_UID, pop = Population, dwellings = Dwellings,
#          tourism, tourism_parent, rent, tenants, owners, apart, apart_parent,
#          income, income_parent) |>
#   mutate(area_DA = units::drop_units(st_area(geometry)),
#          .before = geometry) |>
#   st_set_agr("constant")
# 
# DA <-
#   DA |>
#   mutate(province_ID = substr(DA, 1, 2)) |>
#   inner_join(st_drop_geometry(province), by = "province_ID") |>
#   relocate(province, province_ID, .after = CMA)
# 
# province <-
#   province |>
#   select(-province_ID)
# 
# DA_union <-
#   DA |>
#   group_by(province) |>
#   summarize()
# 
# water <-
#   read_sf("data/lhy_000c16a_e/lhy_000c16a_e.shp") |>
#   st_transform(3347) |>
#   mutate(province = case_when(
#     PRUID == 24 ~ "Quebec",
#     PRUID == 35 ~ "Ontario",
#     PRUID == 48 ~ "Alberta",
#     PRUID == 59 ~ "British Columbia")) |>
#   filter(!is.na(province))
# 
# 
# # Save census geometries --------------------------------------------------
# 
# qsave(DA, file = "output/DA.qs", nthreads = availableCores())
# qsave(DA_union, file = "output/DA_union.qs", nthreads = availableCores())
# qsave(CSD, file = "output/CSD.qs", nthreads = availableCores())
# qsave(CMA, file = "output/CMA.qs", nthreads = availableCores())
# qsave(province, file = "output/province.qs", nthreads = availableCores())
# qsave(water, file = "output/water.qs", nthreads = availableCores())

DA <- qread("output/DA.qs", nthreads = availableCores())
CSD <- qread("output/CSD.qs", nthreads = availableCores())
CMA <- qread("output/CMA.qs", nthreads = availableCores())
province <- qread("output/province.qs", nthreads = availableCores())


# Process neighbourhoods --------------------------------------------------

qload("data/cmhc_shp.qsm", nthreads = availableCores())

# 2017-2022 neighbourhoods are basically the same, with one exception which can
# be handled with imputation. So use 2022 geometries for all neighbourhoods to 
# facilitate spatial panel models
cmhc_nbhd <- 
  cmhc_nbhd_2022 |> 
  st_transform(3347) |> 
  mutate(id = paste0(METCODE, NBHDCODE)) |> 
  select(id, name = NBHDNAME_E, CMA = METCODE) |> 
  mutate(geometry = st_make_valid(geometry)) |> 
  st_set_agr("constant")

# Match 2016 geometries to the rest
cmhc_nbhd_2016 <- 
  cmhc_nbhd_2016 |> 
  st_transform(3347) |> 
  mutate(id = paste0(METCODE, NBHDCODE)) |> 
  select(id, name = NBHDNAME_E, CMA = METCODE) |> 
  mutate(geometry = st_make_valid(geometry))

cmhc_int <-
  cmhc_nbhd_2016 |> 
  mutate(area_2016 = as.numeric(st_area(geometry))) |> 
  st_set_agr("constant") |> 
  st_intersection(st_set_agr(mutate(cmhc_nbhd, area_2022 = as.numeric(
    st_area(geometry))), "constant")) |> 
  mutate(area = as.numeric(st_area(geometry))) |> 
  filter(area == max(area), .by = id.1)
  
cmhc_join_2015 <-
  cmhc_int |> 
  st_drop_geometry() |> 
  select(id_2016 = id, id_2022 = id.1, name_2016 = name) |> 
  # Special cases
  filter(id_2016 != "0340795" | id_2022 == "0340795") |> 
  filter(id_2016 != "3300900" | id_2022 == "3300900") |> 
  filter(id_2016 != "1700960" | id_2022 == "1700960") |> 
  filter(id_2016 != "5400450" | id_2022 == "5400450") |>
  add_row(id_2016 = "0340390", id_2022 = "0340390", 
          name_2016 = "Terwillegar/Rural Southwest") |> 
  add_row(id_2016 = "1700900", id_2022 = "1700900", name_2016 = "West") |> 
  add_row(id_2016 = "5400225", id_2022 = "5400225", name_2016 = "Downtown")

# Hold on to version with names for 2015 joining
cmhc_join <- 
  cmhc_join_2015 |> 
  select(-name_2016)

# Add provinces
cmhc_PR <- 
  cmhc_nbhd |> 
  st_set_agr("constant") |> 
  st_centroid() |> 
  nngeo::st_nn(st_transform(province, 3347))

cmhc_nbhd <- 
  cmhc_nbhd |> 
  mutate(province = !!province$province[as.numeric(cmhc_PR)], .after = name)

rm(cmhc_PR, cmhc_int)

rm(cmhc_nbhd_2016, cmhc_nbhd_2017, cmhc_nbhd_2018, cmhc_nbhd_2019, 
   cmhc_nbhd_2020, cmhc_nbhd_2021, cmhc_nbhd_2022)


# Add nbhd-level data -----------------------------------------------------

cmhc_nbhd <- 
  cmhc_nbhd |> 
  mutate(area_CMHC = units::drop_units(st_area(geometry)), 
         .before = geometry) |> 
  st_set_agr("constant")

cmhc_DA <-
  cmhc_nbhd |> 
  st_set_agr("constant") |> 
  st_intersection(st_set_agr(DA, "constant")) |> 
  mutate(area_int = units::drop_units(st_area(geometry)), .before = geometry)

cmhc_DA <- 
  cmhc_DA |> 
  st_drop_geometry() |> 
  summarize(
    pop = sum(pop * area_int / area_DA, na.rm = TRUE),
    dwellings = sum(dwellings * area_int / area_DA, na.rm = TRUE),
    tourism = sum(tourism * area_int / area_DA, na.rm = TRUE),
    tourism_parent = sum(tourism_parent * area_int / area_DA, na.rm = TRUE),
    apart = sum(apart * area_int / area_DA, na.rm = TRUE),
    apart_parent = sum(apart_parent * area_int / area_DA, na.rm = TRUE),
    income = sum(income * income_parent * area_int / area_DA, na.rm = TRUE) / 
      sum(income_parent * area_int / area_DA, na.rm = TRUE),
    rent_DA = sum(rent * tenants * area_int / area_DA, na.rm = TRUE) / 
      sum(tenants * area_int / area_DA, na.rm = TRUE),
    tenants_DA = sum(tenants + area_int / area_DA, na.rm = TRUE),
    owners_DA = sum(owners + area_int / area_DA, na.rm = TRUE),
    .by = c(id, name, province, CMA))

cmhc_DA <- 
  cmhc_DA |> 
  mutate(
    tourism = tourism / tourism_parent,
    tenant = tenants_DA / (tenants_DA + owners_DA),
    apart = apart / apart_parent) |>
  rename(tenant_count = tenants_DA) |> 
  select(-tourism_parent, -owners_DA, -apart_parent)

cmhc_nbhd <- 
  cmhc_nbhd |> 
  select(-area_CMHC) |> 
  inner_join(cmhc_DA, by = join_by(id, name, province, CMA)) |> 
  relocate(geometry, .after = last_col())

cmhc_CMA <- 
  cmhc_nbhd |> 
  summarize(geometry = st_union(geometry), .by = CMA) |> 
  rename(CMA_old = CMA) |> 
  st_set_agr("constant") |> 
  st_centroid() |> 
  mutate(CMA_id = unlist(nngeo::st_nn(geometry, CMA)),
         CMA_id = CMA$CMA[CMA_id]) |> 
  st_drop_geometry()

cmhc_nbhd <- 
  cmhc_nbhd |> 
  inner_join(cmhc_CMA, by = c("CMA" = "CMA_old")) |> 
  select(-CMA) |> 
  rename(CMA = CMA_id) |> 
  relocate(CMA, .after = province)

cmhc_join_2015 <- 
  cmhc_join_2015 |> 
  inner_join(st_drop_geometry(cmhc_nbhd), by = c(id_2022 = "id")) |> 
  select(id = id_2022, name = name_2016, name_2022 = name, CMA)

rm(cmhc_CMA)


# Process data ------------------------------------------------------------

qload("data/cmhc_data.qsm", nthreads = availableCores())

# Fix 2016 IDs
cmhc_2016 <- 
  cmhc_2016 |> 
  mutate(id = paste0(met_code, nbrhood_code)) |> 
  select(id, year = Year, universe, rent = average_rent, 
         rent_rel = average_rent_reliability, vacancy = vacancy_rate_percent,
         vacancy_rel = vacancy_rate_reliability)

cmhc <- bind_rows(cmhc_2017, cmhc_2018, cmhc_2019, cmhc_2020, cmhc_2021, 
                  cmhc_2022) |> 
  mutate(id = paste0(met_code, nbrhood_code)) |> 
  select(id, year = Year, universe, rent = average_rent, 
         rent_rel = average_rent_reliability, vacancy = vacancy_rate_percent,
         vacancy_rel = vacancy_rate_reliability)

# Join 2016 and 2017-2022 data
cmhc <- 
  cmhc_2016 |> 
  inner_join(cmhc_join, by = c(id = "id_2016")) |> 
  select(id = id_2022, year, universe, rent, rent_rel, vacancy, vacancy_rel) |> 
  bind_rows(cmhc)

# Plug gaps with cmhc_nbhd
cmhc <- 
  cmhc |> 
  full_join((cmhc_nbhd |> 
               st_drop_geometry() |> 
               select(id) |> 
               rowwise() |> 
               mutate(year = list(2016:2022)) |> 
               ungroup() |> 
               unnest(year)), by = c("id", "year"))

rm(cmhc_DA, cmhc_join, cmhc_2016, cmhc_2017, cmhc_2018, cmhc_2019, cmhc_2020, 
   cmhc_2021, cmhc_2022)


# Get updated 2015 data ---------------------------------------------------

# Start with all CMAs
CMA_nb <- sort(unique(cmhc_nbhd$CMA))
# Add missing CMAs by survey zone
CMA_sz <- c("11105", "12225", "24444", "35501", "35530", "35547", "35556",
            "46610", "59918", "59943")
CMA_nb <- CMA_nb[!CMA_nb %in% CMA_sz]
CMA_all <- c(CMA_nb, CMA_sz)

rent_2015_nb <- map(CMA_nb, \(x) {
  cmhc::get_cmhc(
    survey = "Rms",
    series = "Average Rent",
    dimension = "Bedroom Type",
    breakdown = "Neighbourhoods",
    geo_uid = x,
    year = 2015)})

rent_2015_sz <- map(CMA_sz, \(x) {
  cmhc::get_cmhc(
    survey = "Rms",
    series = "Average Rent",
    dimension = "Bedroom Type",
    breakdown = "Survey Zones",
    geo_uid = x,
    year = 2015) |> 
    rename(Neighbourhoods = `Survey Zones`)
}) 

vacancy_2015_nb <- map(CMA_nb, \(x) {
  cmhc::get_cmhc(
    survey = "Rms",
    series = "Vacancy Rate",
    dimension = "Bedroom Type",
    breakdown = "Neighbourhoods",
    geo_uid = x,
    year = 2015)})

vacancy_2015_sz <- map(CMA_sz, \(x) {
  cmhc::get_cmhc(
    survey = "Rms",
    series = "Vacancy Rate",
    dimension = "Bedroom Type",
    breakdown = "Survey Zones",
    geo_uid = x,
    year = 2015) |> 
    rename(Neighbourhoods = `Survey Zones`)
}) 

universe_2015_nb <- map(CMA_nb, \(x) {
  cmhc::get_cmhc(
    survey = "Rms",
    series = "Rental Universe",
    dimension = "Bedroom Type",
    breakdown = "Neighbourhoods",
    geo_uid = x,
    year = 2015)})

universe_2015_sz <- map(CMA_sz, \(x) {
  cmhc::get_cmhc(
    survey = "Rms",
    series = "Rental Universe",
    dimension = "Bedroom Type",
    breakdown = "Survey Zones",
    geo_uid = x,
    year = 2015) |> 
    rename(Neighbourhoods = `Survey Zones`)
})

rent_2015 <- c(rent_2015_nb, rent_2015_sz)
vacancy_2015 <- c(vacancy_2015_nb, vacancy_2015_sz)
universe_2015 <- c(universe_2015_nb, universe_2015_sz)


# Combine and clean 2015 data ---------------------------------------------

rent_2015 <- 
  rent_2015 |> 
  map2(CMA_all, \(x, y) {
    if (!is.null(x)) x |> 
      filter(`Bedroom Type` == "Total") |> 
      filter(!is.na(Neighbourhoods)) |> 
      mutate(CMA = y)
    }) |> 
  bind_rows()

vacancy_2015 <- 
  vacancy_2015 |> 
  map2(CMA_all, \(x, y) {
    if (!is.null(x)) x |> 
      filter(`Bedroom Type` == "Total") |> 
      filter(!is.na(Neighbourhoods)) |> 
      mutate(CMA = y)
  }) |> 
  bind_rows()

universe_2015 <- 
  universe_2015 |> 
  map2(CMA_all, \(x, y) {
    if (!is.null(x)) x |> 
      filter(`Bedroom Type` == "Total") |> 
      filter(!is.na(Neighbourhoods)) |> 
      mutate(CMA = y)
  }) |> 
  bind_rows()

cmhc_2015 <- 
  bind_rows(rent_2015, vacancy_2015, universe_2015) |> 
  mutate(Quality = case_when(
    Quality == "Excellent" ~ "a",
    Quality == "Very good" ~ "b",
    Quality == "Good" ~ "c",
    Quality == "Fair (Use with Caution)" ~ "d",
    .default = NA)) |> 
  select(name = Neighbourhoods, CMA, Value, Quality, year = Year, Series)

cmhc_2015 <- 
  cmhc_2015 |> 
  pivot_wider(names_from = Series, values_from = c(Value, Quality)) |>  
  set_names(c("name", "CMA", "year", "rent", "vacancy", "universe",
              "rent_rel", "vacancy_rel", "universe_rel")) |> 
  select(name:rent, rent_rel, vacancy, vacancy_rel, universe) |> 
  arrange(CMA, name)


# Manually add Halifax and Windsor (missing from HMIP) --------------------

rent_halifax <-
  readxl::read_xlsx("data/cmhc_Halifax_2015.xlsx", skip = 2) |> 
  slice(1:26) |> 
  select(1, 10:11) |> 
  set_names(c("name", "rent", "rent_rel")) |> 
  mutate(CMA = "12205", year = 2015, .after = name) |> 
  mutate(rent = as.numeric(rent)) |> 
  mutate(rent_rel = if_else(rent_rel %in% c("a", "b", "c", "d"), rent_rel, NA))

vac_halifax <- 
  readxl::read_xlsx("data/cmhc_Halifax_2015.xlsx", sheet = 4, skip = 2) |> 
  slice(1:26) |> 
  select(1, 10:11) |> 
  set_names(c("name", "vacancy", "vacancy_rel")) |> 
  mutate(CMA = "12205", year = 2015, .after = name) |> 
  mutate(vacancy = as.numeric(vacancy)) |> 
  mutate(vacancy_rel = if_else(vacancy_rel %in% c("a", "b", "c", "d"), 
                               vacancy_rel, NA))

univ_halifax <- 
  readxl::read_xlsx("data/cmhc_Halifax_2015.xlsx", sheet = 7, skip = 2) |> 
  slice(1:26) |> 
  select(1, 6) |> 
  set_names(c("name", "universe")) |> 
  mutate(CMA = "12205", year = 2015, .after = name)

cmhc_halifax <- 
  rent_halifax |> 
  inner_join(vac_halifax, by = c("name", "CMA", "year")) |> 
  inner_join(univ_halifax, by = c("name", "CMA", "year"))

rent_windsor <- 
  readxl::read_xlsx("data/cmhc_Windsor_2015.xlsx", skip = 2) |> 
  slice(1:15) |> 
  select(1, 10:11) |> 
  set_names(c("name", "rent", "rent_rel")) |> 
  mutate(CMA = "35559", year = 2015, .after = name) |> 
  mutate(rent = as.numeric(rent)) |> 
  mutate(rent_rel = if_else(rent_rel %in% c("a", "b", "c", "d"), rent_rel, NA))

vac_windsor <- 
  readxl::read_xlsx("data/cmhc_Windsor_2015.xlsx", sheet = 4, skip = 2) |> 
  slice(1:15) |> 
  select(1, 10:11) |> 
  set_names(c("name", "vacancy", "vacancy_rel")) |> 
  mutate(CMA = "35559", year = 2015, .after = name) |> 
  mutate(vacancy = as.numeric(vacancy)) |> 
  mutate(vacancy_rel = if_else(vacancy_rel %in% c("a", "b", "c", "d"), 
                               vacancy_rel, NA))

univ_windsor <- 
  readxl::read_xlsx("data/cmhc_Windsor_2015.xlsx", sheet = 7, skip = 2) |> 
  slice(1:15) |> 
  select(1, 6) |> 
  set_names(c("name", "universe")) |> 
  mutate(CMA = "35559", year = 2015, .after = name)

cmhc_windsor <- 
  rent_windsor |> 
  inner_join(vac_windsor, by = c("name", "CMA", "year")) |> 
  inner_join(univ_windsor, by = c("name", "CMA", "year"))

cmhc_2015 <- 
  cmhc_2015 |> 
  bind_rows(cmhc_halifax, cmhc_windsor)

rm(cmhc_halifax, cmhc_windsor, rent_halifax, rent_windsor, univ_halifax, 
   univ_windsor, vac_halifax, vac_windsor)


# Reconcile naming differences and identify missing entries ---------------

# Start by trying to match cmhc_2015 names to 2016 names, via cmhc_join_2015
cmhc_2015_matched <- 
  cmhc_2015 |> 
  inner_join(cmhc_join_2015, by = c("name", "CMA")) |> 
  relocate(id) |> 
  select(-name) |> 
  rename(name = name_2022) |> 
  relocate(name, .after = id)

cmhc_2015_unmatched <- 
  cmhc_2015 |> 
  anti_join(cmhc_join_2015, by = c("name", "CMA"))
  
# Fix remaining discrepancies by hand
cmhc_2015_unmatched <-
  cmhc_2015_unmatched |> 
  # CMA_all[5]
  add_row(
    name = c("Downtown Fredericton", "South-West Fredericton", 
             "Nashwaaksis", "Douglas/Bright", "Devon", "North-East Fredericton", 
             "South-East Fredericton", "Lincoln/New Maryland/Gladstone"),
    CMA = "13320",
    year = 2015,
    rent = c(830, 878, 817, NA, 774, 722, 823, NA),
    rent_rel = c("a", "a", "a", NA, "a", "a", "a", "NA"),
    vacancy = c(3.1, 11.4, 5.3, NA, 6.6, NA, 9.2, NA),
    vacancy_rel = c("b", "d", "c", NA, "b", NA, "b", NA),
    universe = c(3897, 396, 1388, 20, 638, 268, 1236, 15)) |> 
  # CMA_all[6]
  add_row(name = c("Rimouski"), CMA = "24404", year = 2015, rent = 609, 
          rent_rel = "a", vacancy = 4.2, vacancy_rel = "b", universe = 5189) |> 
  # CMA_all[11]
  add_row(
    name = c("Drummondville", "Peripheral Sectors"),
    CMA = "24447",
    year = 2015,
    rent = c(565, 687),
    rent_rel = "a",
    vacancy = c(3.65, 5.8),
    vacancy_rel = c("b", "d"),
    universe = c(8504, 308)) |> 
  # CMA_all[41]
  add_row(
    name = c("North", "South"),
    CMA = "48830",
    year = 2015,
    rent = c(1001, 998),
    rent_rel = "a",
    vacancy = c(4.5, 5.7),
    vacancy_rel = "a",
    universe = c(2555, 3704)) |> 
  # CMA_all[46]
  add_row(
    name = c("South Shore", "North Shore"),
    CMA = "59925",
    year = 2015,
    rent = c(920, 804),
    rent_rel = "a",
    vacancy = c(2.0, 3.0),
    vacancy_rel = c("a", "c"),
    universe = c(2094, 1805)) |> 
  # CMA_all[47]
  add_row(
    name = c("Village West/Chilliwack Mountain", "Sardis/Vedder", "Southeast", 
             "Northeast"),
    CMA = "59930",
    year = 2015,
    rent = c(708, 896, NA, NA),
    rent_rel = c("a", "a", NA, NA),
    vacancy = c(3.2, 4.1, NA, NA),
    vacancy_rel = c("a", "a", NA, NA),
    universe = c(2882, 171, 11, 71)) |> 
  # CMA_all[51]
  add_row(
    name = "Nanaimo (North & Periphery)", CMA = "59938", year = 2015, 
    rent = 787, rent_rel = "a", vacancy = 0.9, vacancy_rel = "a", 
    universe = 846) |> 
  mutate(name = case_when(
    # CMA_all[4]
    name == "Grand Bay/Westfield/Greenwich/Kingston" ~ "Eastern Saint John CMA",
    # CMA_all[7]
    name == "Chicoutimi Town Centre" ~ "Chicoutimi Downtown",
    name == "Jonquière Town Centre" ~ "Jonquière Downtown",
    # CMA_all[8]
    name == "Beaumont/Saint-Joseph-de-la-Pointe-de-Lévy/Pintendre" ~ 
      "Beaumont/St-Joseph-Pte-de-Lévy/Pintendre",
    name == "Beauport Sainte-Thérèse/Beauport Courville/Boischâtel" ~ 
      "Beauport Ste-Thérèse/Boischâtel etc.",
    name == "Cap-Blanc/Vieux-Québec/Saint-Jean-Baptiste" ~ 
      "Cap-Blanc/Vieux-Québec/St-Jean-Baptiste",
    name == paste0("Château-Richer/L'Ange-Gardien/", 
                   "Sainte-Brigitte-de-Laval/L'Île-d'Orléans") ~ 
      "Québec City East CMA",
    name == "Saint-Émile/Lac-Delage/Lac-Saint-Charles" ~
      "St-Émile/Lac-St-Charles",
    name == "Saint-Lambert-de-Lauzon/Saint-Étienne-de-Lauzon" ~ 
      "St-Lambert-de-Lauzon etc.",
    # CMA_all[9]
    name == "Rock-Forest-St-Elie-Deauville" ~ "Rock-Forest–St-Elie–Deauville",
    # CMA_all[10]
    name == "Université du Québec à Trois-Rivières Sector" ~ "UQTR Sector",
    # CMA_all[12]
    name == "Granby (City)" ~ "Granby V",
    # CMA_all[14]
    name == "Bellefeuille/Saint-Colomban/Gore" ~ 
      "Bellefeuille/St-Colomban/Gore",
    name == "Cote-Saint-Luc/Montreal West" ~ "Côte-Saint-Luc/Montreal West",
    name == "Coteau-du-Lac/Les Coteaux/Les Cèdres/Saint-Zotique" ~ 
      "Vaudreuil-Soulanges South",
    name == "L'Assomption/Saint-Sulpice" ~ "L'Assomption/St-Sulpice",
    name == paste0("Notre-Dame-de-l'Île-Perrot/Pincourt/", 
                   "Terrasse-Vaudreuil/L'Île-Perrot") ~ 
      "N.D.-Île-Perr./Pinc./T.-Vaudr./Île-Perr.",
    name == "Parish of L'Epiphanie/Ville de L'Epiphanie/Saint-Gerard-Majella" ~ 
      "L'Épiphanie/St-Gerard-Majella",
    name == "Plateau Mont-Royal" ~ "Plateau-Mont-Royal", 
    name == "Saint-Bruno-De-Montarville" ~ "Saint-Bruno-de-Montarville",
    name == "Saint-Constant/Saint-Mathieu/Saint-Philippe" ~ 
      "St-Constant/St-Mathieu/St-Philippe", 
    name == "Saint-Joseph-du-Lac/Pointe-Calumet/Oka" ~ 
      "St-Joseph-du-Lac/Pte-Calumet/Oka", 
    name == "Saint-Lazare/Hudson" ~ "St-Lazare/Hudson",
    name == "Saint-Mathias-sur-Richelieu" ~ "St-Mathias-sur-Richelieu", 
    name == "Sainte-Anne-de-Bellevue/Baie-d'Urfé" ~ 
      "Ste-Anne-de-Bellevue/Baie-d'Urfé",
    name == "Sainte-Julie/Saint-Mathieu-de-Beloeil" ~ 
      "Ste-Julie/St-Mathieu-de-Beloeil",
    name == "Sud-Ouest" ~ "South West",
    name == paste0("Vaudreuil-Dorion/Vaudreuil-sur-le-Lac/", 
                   "L'Île-Cadieux/Pointe-des-Cascades") ~ 
      "Vaudreuil-Soulanges North",
    # CMA_all[15]
    name == "Lac Deschênes" ~ "Lac-Deschênes",
    # CMA_all[16]
    name == "Eastern Orléans/Rural Eastern Ottawa" ~ 
      "Eastern Orléans/Eastern Ottawa",
    name == "New Edinburgh/Manor Park/Rockcliffe Park" ~
      "New Edinburgh/Manor Park etc",
    # CMA_all[17]
    name == "Loyalist/South Frontenac/Frontenac Islands" ~
      "Loyalist/S. Frontenac/Frontenac Islands",
    # CMA_all[19]
    name == "City Westside (Westmont/Byersville and Green Hill)" ~
      "City Westside",
    name == "Peterborough (Central Excluding Downtown Core)" ~ 
      "Peterborough(Central Excl Downtown Core)",
    name == paste0("West of Otonabee River (Smith-Ennismore-",
                   "Lakefield/Cavan-Millbrook-North Monaghan)") ~
      "West of Otonabee River",
    # CMA_all[21]
    name == "Cabbagetown-South St. James Town" ~ 
      "Cabbagetown-S. St. James Town",
    name == "Mount Olive-Silverstone-Jamestown/Thistletown" ~
      "Mt Olive-Jamestown etc.",
    name == "Waterfront Communities-The Island" ~ 
      "Waterfront Communities -The Island",
    name == "Yonge-St.Clair" ~ "Yonge-St. Clair",
    # CMA_all[23]
    name == "Chippewa Park Area" ~ "Chippawa Park Area",
    name == "Downtown St.Catharines" ~ "Downtown St. Catharines",
    name == "Niagara-On-The-Lake" ~ "Niagara-on-the-Lake",
    name == "Queensway Garden/Stamford Center" ~ 
      "Queensway Garden/Stamford Centre",
    # CMA_all[25]
    name == "Brant County" ~ "Brant",
    # CMA_all[26]
    name == "Townships" ~ "Guelph/Eramosa and Puslinch",
    # CMA_all[31]
    name == "Laurentian/Cedar Heights/Airport Heights" ~
      "Laurentian/Cedar Hts/Airport Hts",
    # CMA_all[32]
    name == "Rayside-Balfour/Azilda/Chelmsford" ~ "Rayside-Balfour",
    # CMA_all[33]
    name == "Rural Sault Ste Marie" ~ "Rural Sault Ste. Marie",
    # CMA_all[34]
    name == "College Heights/Grandview/Lakefront" ~ 
      "College Hts/Grandview/Lakefront",
    name == "College Park/Confederation College" ~ "Confederation College",
    name == "Forest Park/West End/Mariday/Downtown PA" ~ "Downtown/West End",
    name == "Edgewater/Northwood/McKellar Park/East End" ~ "East End",
    # CMA_all[35]
    name == "River Osborne" ~ "River-Osborne",
    # CMA_all[38]
    name == "Medicine Hat (Downtown)/South Flats" ~ 
      "Medicine Hat (Downtown)/S Flats",
    name == "Ranchlands/Parkview/NE Crescent Heights" ~ "North-East",
    name == "Riverside/Tower Estates/Light Industrial" ~ 
      "Riverside/Tower Estates",
    name == "Taylor/Turner/Southlands/Southridge/Saamis/Southvista" ~ "South",
    # CMA_all[39]
    name == paste0("Henderson Lake/Glendale/Sherring/",
                   "Victoria Park/Shakleford Industrial Parks") ~ "North-East",
    name == "Indian Battle Hts/The Crossings/West Highlands" ~ "West-North",
    name == "Legacy Ridge/Uplands/Hardieville" ~ "North-West",
    name == paste0("Paradise Canyon/River Stone/Mountain Heights/Copperwood/",
                   "Heritage Heights/Ridgewood Heights/SunRidge") ~ 
      "West-South",
    name == "Prairie Arbour Estates/Southgate/Fairmont" ~ "South-East",
    str_detect(name, "Scenic Heights/Chinook Heights/Park Royal/") ~ 
      "South-Centre",
    name == "Victoria Park/Downtown/London Road" ~ "Downtown",
    name == "Westminster/Dave Elton/West-North" ~ "North-Centre",
    # CMA_all[40]
    str_starts(name, "Altadore/South Calgary/CFB") ~
      "Altadore/South Calgary/CFB Currie etc.",
    name == "Bel-Aire/Mayfair/Kelvin Grove/Eagle Ridge/Chinook Park" ~
      "Bel-Aire/Mayfair/Kelvin Grove etc.",
    name == "Braeside/Bayview/Palliser/Pump Hill/Oakridge/Cedarbrae" ~
      "Braeside/Bayview/Palliser/Pump Hill etc.",
    str_starts(name, "Bridlewood/Evergreen/Shawnee Slope") ~
      "Bridlewood/Evergreen/Shawnee Slope etc.",
    str_starts(name, "Calgary CMA \\(Rural Calgary City/Rocky") ~ 
      "Calgary CMA Remainder",
    str_starts(name, "Canyon Meadows/Woodlands/Woodbine") ~
      "Canyon Meadows/Woodlands/Woodbine etc.",
    str_starts(name, "Dover/Southview/Inglewood/Alyth/Bonnybrook") ~ 
      "Dover/Southview/Inglewood/Alyth etc.",
    str_starts(name, "Falconridge/Castleridge/Coral Springs/Martindale") ~
      "Falconridge/Castleridge etc.",
    str_starts(name, "Forest Lawn/Forest Heights/Penbrooke Meadows") ~
      "Forest Lawn/Forest Heights etc.",
    str_starts(name, "Huntington Hills/Beddington Heights/Evanston/") ~
      "Huntington Hills/Evanston etc.",
    name == "Manchester Industrial/Roxboro/Erlton" ~ 
      "Manchester Ind./Roxboro/Erlton",
    str_starts(name, "McKenzie Towne/New Brighton/Copperfield/Cranston") ~ 
      "McKenzie Towne/New Brighton etc.",
    name == "Meridian/Fanklin/Albert Park/Radisson Heights" ~ 
      "Meridian/Franklin/Albert Pk/Radisson Hts",
    name == "Montgomery/Point McKay/University Heights" ~ 
      "Montgomery/Point McKay/University Hts",
    str_starts(name, "North Haven/Upper North Haven/Thorncliffe/Highwood") ~ 
      "N Haven/Upper N Haven/Thorncliffe etc.",
    str_starts(name, "Parkdale/St. Andrews Heights/Hounsfield Heights") ~ 
      "Parkdale/St. Andrews Hts etc.",
    name == "Ranchlands/Hawkwood/Arbour Lake/Citadel/Rocky Ridge/Royal Oak" ~ 
      "Ranchlands/Hawkwood/Citadel etc.",
    name == "Rosedale/Sunnyside/Crescent Heights/Renfrew" ~ 
      "Rosedale/Sunnyside/Crescent Hts/Renfrew",
    name == "Southeast Industrial Area and Surrounding Communities" ~ 
      "Southeast Industrial & Surrounding Comm.",
    str_starts(name, "Strathcona Park/Christie Park/Patterson/Coach Hill") ~ 
      "Strathcona Pk/Christie Pk/Patterson etc.",
    str_starts(name, "Vista Heights/Skyline East/Pegasus/McCall") ~ 
      "Vista Heights/Skyline East/Pegasus etc.",
    name == "Wildwood/Spruce Cliff/Shaganappi/Westgate/Rosscarrock" ~ 
      "Wildwood/Spruce Cliff/Shaganappi etc.",
    name == "Windsor Park/Meadowlark Park/Britania/Elboya" ~ 
      "Windsor Pk/Meadowlark Pk/Britannia etc.",
    name == "Winston Heights/Mountview/Tuxedo Park/Highland Park" ~ 
      "Winston Hts/Mountview/Tuxedo Park etc.",
    # CMA_all[42]
    name == "Bitannia" ~ "Britannia",
    name == "East Castledowns" ~ "East Castle Downs",
    name == "Strathcona County Remiander" ~ "Strathcona County Remainder",
    # CMA_all[44]
    name == "Wood Buffalo" ~ "Wood Buffalo CA",
    # CMA_all[49]
    name == "DT Eastside/Strathcona" ~ "Downtown Eastside/Strathcona",
    name == "Dundrave/West Vancouver Remainder" ~ 
      "Dundrave/W Vancouver Remainder",
    name == "Hastings/Sunrise/Grandview/Woodlands" ~ 
      "Hastings/Grandview/Woodlands",
    name == "Langley (City)" ~ "Langley CY",
    name == "Langley (District municipality)" ~ "Langley DM",
    name == "North Vancouver District Municipality East" ~ 
      "North Vancouver DM East",
    name == "North Vancouver District Municipality West" ~ 
      "North Vancouver DM West",
    # CMA_all[50]
    name == "James Bay (North)" ~ "James Bay North",
    name == "James Bay (South)" ~ "James Bay South",
    # CMA_all[54]
    name == "Sydney" ~ "Sydney City",
    # CMA_all[59]
    name == "Rest of Kent" ~ "Remainder of Kent",
    # Manual Halifax
    name == "Herring Cove/Terence Bay/St. Margaret's Bay/Prospect" ~
      "Herring Cove/Terence Bay/Prospect etc.",
    # Manual Windsor
    name == "Fontainbleu" ~ "Fontainebleau",
    .default = name)) 

cmhc_2015_unmatched <- 
  cmhc_2015_unmatched |> 
  inner_join(cmhc_join_2015, by = c("name", "CMA")) |> 
  relocate(id) |> 
  select(-name_2022)

cmhc_2015 <- 
  bind_rows(cmhc_2015_matched, cmhc_2015_unmatched) |> 
  select(-name, -CMA)

rm(cmhc_2015_matched, cmhc_2015_unmatched, cmhc_join_2015, rent_2015,
   rent_2015_nb, rent_2015_sz, universe_2015, universe_2015_nb,
   universe_2015_sz, vacancy_2015, vacancy_2015_nb, vacancy_2015_sz)


# Get updated 2023 data ---------------------------------------------------

# Start with all CMAs
CMA_nb <- sort(unique(cmhc_nbhd$CMA))
# Add missing CMAs by survey zone
CMA_sz <- c("11105", "12225", "24404", "24444", "35501", "35530", "35547",
            "35556", "46610", "59918", "59943")
CMA_nb <- CMA_nb[!CMA_nb %in% CMA_sz]
CMA_all <- c(CMA_nb, CMA_sz)

rent_2023_nb <- map(CMA_nb, \(x) {
  cmhc::get_cmhc(
    survey = "Rms",
    series = "Average Rent",
    dimension = "Bedroom Type",
    breakdown = "Neighbourhoods",
    geo_uid = x,
    year = 2023)})

rent_2023_sz <- map(CMA_sz, \(x) {
  cmhc::get_cmhc(
    survey = "Rms",
    series = "Average Rent",
    dimension = "Bedroom Type",
    breakdown = "Survey Zones",
    geo_uid = x,
    year = 2023) |> 
    rename(Neighbourhoods = `Survey Zones`)
}) 

vacancy_2023_nb <- map(CMA_nb, \(x) {
  cmhc::get_cmhc(
    survey = "Rms",
    series = "Vacancy Rate",
    dimension = "Bedroom Type",
    breakdown = "Neighbourhoods",
    geo_uid = x,
    year = 2023)})

vacancy_2023_sz <- map(CMA_sz, \(x) {
  cmhc::get_cmhc(
    survey = "Rms",
    series = "Vacancy Rate",
    dimension = "Bedroom Type",
    breakdown = "Survey Zones",
    geo_uid = x,
    year = 2023) |> 
    rename(Neighbourhoods = `Survey Zones`)
}) 

universe_2023_nb <- map(CMA_nb, \(x) {
  cmhc::get_cmhc(
    survey = "Rms",
    series = "Rental Universe",
    dimension = "Bedroom Type",
    breakdown = "Neighbourhoods",
    geo_uid = x,
    year = 2023)})

universe_2023_sz <- map(CMA_sz, \(x) {
  cmhc::get_cmhc(
    survey = "Rms",
    series = "Rental Universe",
    dimension = "Bedroom Type",
    breakdown = "Survey Zones",
    geo_uid = x,
    year = 2023) |> 
    rename(Neighbourhoods = `Survey Zones`)
})

rent_2023 <- c(rent_2023_nb, rent_2023_sz)
vacancy_2023 <- c(vacancy_2023_nb, vacancy_2023_sz)
universe_2023 <- c(universe_2023_nb, universe_2023_sz)


# Combine and clean 2023 data ---------------------------------------------

rent_2023 <- 
  rent_2023 |> 
  map2(CMA_all, \(x, y) {
    if (!is.null(x)) x |> 
      filter(`Bedroom Type` == "Total") |> 
      filter(!is.na(Neighbourhoods)) |> 
      mutate(CMA = y)
  }) |> 
  bind_rows()

vacancy_2023 <- 
  vacancy_2023 |> 
  map2(CMA_all, \(x, y) {
    if (!is.null(x)) x |> 
      filter(`Bedroom Type` == "Total") |> 
      filter(!is.na(Neighbourhoods)) |> 
      mutate(CMA = y)
  }) |> 
  bind_rows()

universe_2023 <- 
  universe_2023 |> 
  map2(CMA_all, \(x, y) {
    if (!is.null(x)) x |> 
      filter(`Bedroom Type` == "Total") |> 
      filter(!is.na(Neighbourhoods)) |> 
      mutate(CMA = y)
  }) |> 
  bind_rows()

cmhc_2023 <- 
  bind_rows(rent_2023, vacancy_2023, universe_2023) |> 
  mutate(Quality = case_when(
    Quality == "Excellent" ~ "a",
    Quality == "Very good" ~ "b",
    Quality == "Good" ~ "c",
    Quality == "Fair (Use with Caution)" ~ "d",
    .default = NA)) |> 
  select(name = Neighbourhoods, CMA, Value, Quality, year = Year, Series)

cmhc_2023 <- 
  cmhc_2023 |> 
  pivot_wider(names_from = Series, values_from = c(Value, Quality)) |>  
  set_names(c("name", "CMA", "year", "rent", "vacancy", "universe",
              "rent_rel", "vacancy_rel", "universe_rel")) |> 
  select(name:rent, rent_rel, vacancy, vacancy_rel, universe) |> 
  arrange(CMA, name)


# Reconcile naming differences and identify missing entries ---------------

# Start by trying to match cmhc_2023 names to 2016 names, via cmhc_join_2023
cmhc_2023_matched <-
  cmhc_2023 |> 
  inner_join(select(st_drop_geometry(cmhc_nbhd), id, name, CMA), 
             by = c("name", "CMA")) |> 
  relocate(id)

cmhc_2023_unmatched <- 
  cmhc_2023 |> 
  anti_join(cmhc_nbhd, by = c("name", "CMA"))

# Fix remaining discrepancies by hand
cmhc_2023_unmatched <- 
  cmhc_2023_unmatched |> 
  # CMA_all[2]
  filter(!(name == "Halifax" & CMA == "12205")) |> 
  # CMA_all[5]
  add_row(
    name = c("Devon", "Douglas/Bright", "Downtown Fredericton", 
             "Lincoln/New Maryland/Gladstone", "Nashwaaksis", 
             "North-East Fredericton", "South-East Fredericton", 
             "South-West Fredericton"),
    CMA = "13320",
    year = 2023,
    rent = c(1294, NA, 1247, NA, 1110, 1083, 1243, 1595),
    rent_rel = c("b", NA, "a", NA, "a", "a", "a", "a"),
    vacancy = c(0.8, NA, 0.9, NA, 1.5, 4.9, 2.2, 1.3),
    vacancy_rel = c("a", NA, "a", NA, "a", "c", "b", "a"),
    universe = c(854, 19, 4770, 18, 1731, 309, 1344, 1056)) |> 
  # CMA_all[10]
  add_row(
    name = c("Drummondville", "Peripheral Sectors"),
    CMA = "24447",
    year = 2023,
    rent = c(843, NA),
    rent_rel = c("a", NA),
    vacancy = c(0.5, NA),
    vacancy_rel = c("b", NA),
    universe = c(10808, 383)) |> 
  # CMA_all[40]
  add_row(
    name = c("North", "South"),
    CMA = "48830",
    year = 2023,
    rent = c(1126, 1192),
    rent_rel = "a",
    vacancy = c(1.1, 0.8),
    vacancy_rel = "a",
    universe = c(2473, 4551)) |> 
  # CMA_all[45]
  add_row(
    name = c("South Shore", "North Shore"),
    CMA = "59925",
    year = 2023,
    rent = c(1507, 1108),
    rent_rel = "a",
    vacancy = c(1.3, 1.4),
    vacancy_rel = "a",
    universe = c(2861, 1897)) |> 
  # CMA_all[46]
  add_row(
    name = c("Northeast", "Sardis/Vedder", "Southeast", 
             "Village West/Chilliwack Mountain"),
    CMA = "59930",
    year = 2023,
    rent = c(NA, 1485, NA, 1141),
    rent_rel = c(NA, "a", NA, "a"),
    vacancy = c(NA, 0.7, NA, 1.7),
    vacancy_rel = c(NA, "a", NA, "a"),
    universe = c(71, 1300, 21, 2676)) |> 
  # CMA_all[50]
  add_row(
    name = c("Nanaimo (Centre)", "Nanaimo (South)", 
             "Nanaimo (North & Periphery)"),
    CMA = "59938",
    year = 2023,
    rent = c(1380, 1259, 1723),
    rent_rel = "a",
    vacancy = c(3.0, 2.6, 2.2),
    vacancy_rel = c("a", "b", "a"),
    universe = c(2075, 1260, 1748)) |> 
  mutate(name = case_when(
    # CMA_all[2]
    name == "Herring Cove/Terence Bay/St. Margaret's Bay/Prospect" ~
      "Herring Cove/Terence Bay/Prospect etc.",
    # CMA_all[4]
    name == "Grand Bay/Westfield/Greenwich/Kingston" ~ 
      "Eastern Saint John CMA",
    # CMA_all[7]
    name == "Saint-Rédempteur/Saint-Apollinaire" ~ "Saint-Rédempteur",
    # CMA_all[8]
    name == "Rock-Forest-Saint-Elie-Deauville" ~ 
      "Rock-Forest–St-Elie–Deauville",
    # CMA_all[9]
    name == "Université du Québec à Trois-Rivières Sector" ~
      "UQTR Sector",
    # CMA_all[13]
    name == "Bellefeuille/Saint-Colomban/Gore" ~ 
      "Bellefeuille/St-Colomban/Gore",
    name == "Coteau-du-Lac/Les Coteaux/Les Cèdres/Saint-Zotique" ~ 
      "Vaudreuil-Soulanges South",
    name == "L'Assomption/Saint-Sulpice" ~ "L'Assomption/St-Sulpice",
    name == paste0("Notre-Dame-de-l'Île-Perrot/Pincourt/", 
                   "Terrasse-Vaudreuil/L'Île-Perrot") ~ 
      "N.D.-Île-Perr./Pinc./T.-Vaudr./Île-Perr.",
    name == "Parish of L'Epiphanie/Ville de L'Epiphanie/Saint-Gerard-Majella" ~ 
      "L'Épiphanie/St-Gerard-Majella",
    name == "Saint-Constant/Saint-Mathieu/Saint-Philippe" ~ 
      "St-Constant/St-Mathieu/St-Philippe", 
    name == "Saint-Joseph-du-Lac/Pointe-Calumet/Oka" ~ 
      "St-Joseph-du-Lac/Pte-Calumet/Oka", 
    name == "Saint-Lazare/Hudson" ~ "St-Lazare/Hudson",
    name == "Saint-Lin-Laurentides/Saint-Roch-de-l'Achigan" ~ 
      "Saint-Lin–Laurentides V",
    name == "Saint-Mathias-sur-Richelieu" ~ "St-Mathias-sur-Richelieu", 
    name == "Sainte-Anne-de-Bellevue/Baie-d'Urfé" ~ 
      "Ste-Anne-de-Bellevue/Baie-d'Urfé",
    name == "Sainte-Julie/Saint-Mathieu-de-Beloeil" ~ 
      "Ste-Julie/St-Mathieu-de-Beloeil",
    name == paste0("Vaudreuil-Dorion/Vaudreuil-sur-le-Lac/", 
                   "L'Île-Cadieux/Pointe-des-Cascades") ~ 
      "Vaudreuil-Soulanges North",
    # CMA_all[15]
    name == "Eastern Orléans/Rural Eastern Ottawa" ~ 
      "Eastern Orléans/Eastern Ottawa",
    name == "New Edinburgh/Manor Park/Rockcliffe Park" ~
      "New Edinburgh/Manor Park etc",
    # CMA_all[16]
    name == "Loyalist/South Frontenac/Frontenac Islands" ~
      "Loyalist/S. Frontenac/Frontenac Islands",
    # CMA_all[18]
    name == "City Westside (Westmont/Byersville and Green Hill)" ~
      "City Westside",
    name == "Peterborough (Central Excluding Downtown Core)" ~ 
      "Peterborough(Central Excl Downtown Core)",
    name == paste0("West of Otonabee River (Smith-Ennismore-",
                   "Lakefield/Cavan-Millbrook-North Monaghan)") ~
      "West of Otonabee River",
    name == "East of Otonabee River (Otonabee-South Monaghan/Douro-Dummer)" ~
      "East of Otonabee River",
    # CMA_all[20]
    name == "Cabbagetown-South St. James Town" ~ 
      "Cabbagetown-S. St. James Town",
    name == "Mount Olive-Silverstone-Jamestown/Thistletown" ~
      "Mt Olive-Jamestown etc.",
    name == "Waterfront Communities-The Island" ~ 
      "Waterfront Communities -The Island",
    # CMA_all[30]
    name == "Laurentian/Cedar Heights/Airport Heights" ~
      "Laurentian/Cedar Hts/Airport Hts",
    # CMA_all[31]
    name == "Rayside-Balfour/Azilda/Chelmsford" ~
      "Rayside-Balfour",
    # CMA_all[33]
    name == "College Heights/Grandview/Lakefront" ~ 
      "College Hts/Grandview/Lakefront",
    name == "College Park/Confederation College" ~ "Confederation College",
    name == "Edgewater/Northwood/McKellar Park/East End" ~ "East End",
    # CMA_all[37]
    name == "Medicine Hat (Downtown)/South Flats" ~ 
      "Medicine Hat (Downtown)/S Flats",
    name == "Ranchlands/Parkview/NE Crescent Heights" ~ "North-East",
    name == "Riverside/Tower Estates/Light Industrial" ~ 
      "Riverside/Tower Estates",
    name == "Taylor/Turner/Southlands/Southridge/Saamis/Southvista" ~ "South",
    # CMA_all[38]
    name == paste0("Henderson Lake/Glendale/Sherring/",
                   "Victoria Park/Shakleford Industrial Parks") ~ "North-East",
    name == "Indian Battle Hts/The Crossings/West Highlands" ~ "West-North",
    name == "Legacy Ridge/Uplands/Hardieville" ~ "North-West",
    name == paste0("Paradise Canyon/River Stone/Mountain Heights/Copperwood/",
                   "Heritage Heights/Ridgewood Heights/SunRidge") ~ 
      "West-South",
    name == "Prairie Arbour Estates/Southgate/Fairmont" ~ "South-East",
    str_detect(name, "Scenic Heights/Chinook Heights/Park Royal/") ~ 
      "South-Centre",
    name == "Victoria Park/Downtown/London Road" ~ "Downtown",
    name == "Westminster/Dave Elton/West-North" ~ "North-Centre",
    # CMA_all[39]
    str_starts(name, "Altadore/South Calgary/CFB") ~
      "Altadore/South Calgary/CFB Currie etc.",
    name == "Bel-Aire/Mayfair/Kelvin Grove/Eagle Ridge/Chinook Park" ~
      "Bel-Aire/Mayfair/Kelvin Grove etc.",
    name == "Braeside/Bayview/Palliser/Pump Hill/Oakridge/Cedarbrae" ~
      "Braeside/Bayview/Palliser/Pump Hill etc.",
    str_starts(name, "Bridlewood/Evergreen/Shawnee Slope") ~
      "Bridlewood/Evergreen/Shawnee Slope etc.",
    str_starts(name, "Calgary CMA \\(Rural Calgary City/Rocky") ~ 
      "Calgary CMA Remainder",
    str_starts(name, "Canyon Meadows/Woodlands/Woodbine") ~
      "Canyon Meadows/Woodlands/Woodbine etc.",
    str_starts(name, "Dover/Southview/Inglewood/Alyth/Bonnybrook") ~ 
      "Dover/Southview/Inglewood/Alyth etc.",
    str_starts(name, "Falconridge/Castleridge/Coral Springs/Martindale") ~
      "Falconridge/Castleridge etc.",
    str_starts(name, "Forest Lawn/Forest Heights/Penbrooke Meadows") ~
      "Forest Lawn/Forest Heights etc.",
    str_starts(name, "Huntington Hills/Beddington Heights/Evanston/") ~
      "Huntington Hills/Evanston etc.",
    name == "Manchester Industrial/Roxboro/Erlton" ~ 
      "Manchester Ind./Roxboro/Erlton",
    str_starts(name, "McKenzie Towne/New Brighton/Copperfield/Cranston") ~ 
      "McKenzie Towne/New Brighton etc.",
    name == "Meridian/Fanklin/Albert Park/Radisson Heights" ~ 
      "Meridian/Franklin/Albert Pk/Radisson Hts",
    name == "Montgomery/Point McKay/University Heights" ~ 
      "Montgomery/Point McKay/University Hts",
    str_starts(name, "North Haven/Upper North Haven/Thorncliffe/Highwood") ~ 
      "N Haven/Upper N Haven/Thorncliffe etc.",
    str_starts(name, "Parkdale/St. Andrews Heights/Hounsfield Heights") ~ 
      "Parkdale/St. Andrews Hts etc.",
    name == "Ranchlands/Hawkwood/Arbour Lake/Citadel/Rocky Ridge/Royal Oak" ~ 
      "Ranchlands/Hawkwood/Citadel etc.",
    name == "Rosedale/Sunnyside/Crescent Heights/Renfrew" ~ 
      "Rosedale/Sunnyside/Crescent Hts/Renfrew",
    name == "Southeast Industrial Area and Surrounding Communities" ~ 
      "Southeast Industrial & Surrounding Comm.",
    str_starts(name, "Strathcona Park/Christie Park/Patterson/Coach Hill") ~ 
      "Strathcona Pk/Christie Pk/Patterson etc.",
    str_starts(name, "Valley Ridge/Crestmont/Cougar Ridge") ~
      "Valley Ridge/Crestmont/Cougar Ridge etc.",
    str_starts(name, "Vista Heights/Skyline East/Pegasus/McCall") ~ 
      "Vista Heights/Skyline East/Pegasus etc.",
    name == "Wildwood/Spruce Cliff/Shaganappi/Westgate/Rosscarrock" ~ 
      "Wildwood/Spruce Cliff/Shaganappi etc.",
    name == "Windsor Park/Meadowlark Park/Britania/Elboya" ~ 
      "Windsor Pk/Meadowlark Pk/Britannia etc.",
    name == "Winston Heights/Mountview/Tuxedo Park/Highland Park" ~ 
      "Winston Hts/Mountview/Tuxedo Park etc.",
    # CMA_all[41]
    name == "Highlands/Alberta Avenue" ~ "Highlands/Alberta Ave.",
    # CMA_all[48]
    name == "Dundrave/West Vancouver Remainder" ~ 
      "Dundrave/W Vancouver Remainder",
    name == "Hastings/Sunrise/Grandview/Woodlands" ~ 
      "Hastings/Grandview/Woodlands",
    # CMA_all[49]
    name == "James Bay (North)" ~ "James Bay North",
    name == "James Bay (South)" ~ "James Bay South",
    # CMA_all[53]
    name == "Sydney" ~ "Sydney City",
    # CMA_all[55]
    name == "Shawinigan-South" ~ "Shawinigan-Sud",
    .default = name)) 

cmhc_2023_unmatched <- 
  cmhc_2023_unmatched |> 
  inner_join(select(st_drop_geometry(cmhc_nbhd), id, name, CMA), 
             by = c("name", "CMA")) |> 
  relocate(id)

cmhc_2023 <- 
  bind_rows(cmhc_2023_matched, cmhc_2023_unmatched) |> 
  select(-name, -CMA)

rm(cmhc_2023_matched, cmhc_2023_unmatched, rent_2023, rent_2023_nb, 
   rent_2023_sz, universe_2023, universe_2023_nb, universe_2023_sz, 
   vacancy_2023, vacancy_2023_nb, vacancy_2023_sz, CMA_all, CMA_nb, CMA_sz)


# Get CSD-level data ------------------------------------------------------

library(cmhc)

csd_pool <- 
  CSD |> 
  filter(is.na(CMA) | !CMA %in% cmhc_nbhd$CMA)

csd_to_download <- 
  cmhc_csd_translation_data_2023 |> 
  filter(!METCODE %in% substr(cmhc_nbhd$id, 1, 4)) |> 
  filter(GeoUID %in% csd_pool$CSD)

csd_rent <- 
  map(csd_to_download$GeoUID, \(x) {
    cmhc::get_cmhc(
      survey = "Rms",
      series = "Average Rent",
      dimension = "Bedroom Type",
      breakdown = "Historical Time Periods",
      geo_uid = x)})

csd_vacancy <- 
  map(csd_to_download$GeoUID, \(x) {
    cmhc::get_cmhc(
      survey = "Rms",
      series = "Vacancy Rate",
      dimension = "Bedroom Type",
      breakdown = "Historical Time Periods",
      geo_uid = x)})

csd_universe <- 
  map(csd_to_download$GeoUID, \(x) {
    cmhc::get_cmhc(
      survey = "Rms",
      series = "Rental Universe",
      dimension = "Bedroom Type",
      breakdown = "Historical Time Periods",
      geo_uid = x)})

csd_rent <- 
  csd_rent |> 
  bind_rows() |> 
  filter(Date >= "2015-10-01", `Bedroom Type` == "Total", GeoUID %in% CSD$CSD)

csd_vacancy <- 
  csd_vacancy |> 
  bind_rows() |> 
  filter(Date >= "2015-10-01", `Bedroom Type` == "Total", GeoUID %in% CSD$CSD)

csd_universe <- 
  csd_universe |> 
  bind_rows() |> 
  filter(Date >= "2015-10-01", `Bedroom Type` == "Total", GeoUID %in% CSD$CSD)

# Bind tables and join to METCODE
cmhc_csd <- 
  bind_rows(csd_rent, csd_vacancy, csd_universe) |> 
  mutate(year = year(Date), .after = GeoUID) |> 
  select(-Date, -DateString, -`Bedroom Type`, -Survey) |> 
  inner_join(cmhc_csd_translation_data_2023, by = "GeoUID") |> 
  relocate(METCODE, .after = GeoUID)

# Change quality
cmhc_csd <- 
  cmhc_csd |> 
  mutate(Quality = case_when(
    Quality == "Excellent" ~ "a",
    Quality == "Very good" ~ "b",
    Quality == "Good" ~ "c",
    Quality == "Fair (Use with Caution)" ~ "d",
    .default = NA))

cmhc_csd <- 
  cmhc_csd |> 
  pivot_wider(names_from = Series, values_from = c(Value, Quality)) |> 
  set_names(c("GeoUID", "METCODE", "year", "rent", "vacancy", "universe",
              "rent_rel", "vacancy_rel", "universe_rel")) |> 
  select(GeoUID:rent, rent_rel, vacancy, vacancy_rel, universe) |> 
  filter(sum(is.na(rent)) != n(), .by = GeoUID)

# Create IDs
cmhc_csd <- 
  cmhc_csd |> 
  arrange(GeoUID, year) |> 
  mutate(id = paste0(METCODE, "00", seq_along(GeoUID)), .by = c(METCODE, year),
         .before = GeoUID)

cmhc_csd_geom <- 
  cmhc_csd |> 
  select(id, GeoUID) |> 
  distinct()

cmhc_csd <- 
  cmhc_csd |> 
  select(id, year, universe, rent, rent_rel, vacancy, vacancy_rel)


# Add CSDs to cmhc_nbhd ---------------------------------------------------

cmhc_nbhd_csd <- 
  CSD |> 
  filter(CSD %in% cmhc_csd_geom$GeoUID) |> 
  select(CSD, name = name_CSD, province, CMA, pop = pop_CSD, 
         dwellings = dwellings_CSD, tourism, apart, income, rent_DA = rent,
         tenant_count, tenant, geometry)

# Remove overlap with existing neighbourhoods
cmhc_nbhd_csd <- 
  cmhc_nbhd_csd |> 
  st_set_agr("constant") |> 
  st_difference(st_intersection(st_union(cmhc_nbhd)))


# Combine tables ----------------------------------------------------------

cmhc <- 
  bind_rows(cmhc, cmhc_csd, cmhc_2015, cmhc_2023) |> 
  arrange(id, year)

cmhc_nbhd_csd <- 
  cmhc_nbhd_csd |> 
  inner_join(cmhc_csd_geom, by = c("CSD" = "GeoUID")) |> 
  relocate(id, .before = CSD) |>
  # Set CMA to the CSD value if there isn't one already
  mutate(CMA = coalesce(CMA, CSD)) |> 
  select(-CSD)

cmhc_nbhd <-
  bind_rows(cmhc_nbhd, cmhc_nbhd_csd) |>
  arrange(id)

rm(cmhc_2015, cmhc_2023, cmhc_csd, cmhc_csd_geom, cmhc_nbhd_csd, csd_pool, 
   csd_rent, csd_to_download, csd_universe, csd_vacancy)


# Add name_CMA ------------------------------------------------------------

cmhc_nbhd <- 
  cmhc_nbhd |> 
  left_join(select(st_drop_geometry(CMA), CMA, name_CMA), by = "CMA") |> 
  relocate(name_CMA, .after = CMA)

cmhc_nbhd <-
  cmhc_nbhd |> 
  mutate(name_CMA = coalesce(name_CMA, paste0(name, " (CSD)")))


# Adjust rents for inflation ----------------------------------------------

cpi <- 
  read_csv("data/CPI.csv", show_col_types = FALSE) |> 
  select(REF_DATE, GEO, product = `Products and product groups`, UOM, 
         value = VALUE) |> 
  suppressWarnings()

cpi <- 
  cpi |> 
  filter(product == "Shelter", GEO == "Canada", UOM == "2002=100") |> 
  mutate(month = yearmonth(REF_DATE)) |> 
  select(month, value) |> 
  filter(month >= yearmonth("2015-10"))

cpi_rent <- 
  cpi |> 
  filter(month(month) == 10) |> 
  mutate(year = year(month)) |> 
  select(year, value) |> 
  mutate(value = value / value[year == 2023])

cmhc <- 
  cmhc |> 
  inner_join(cpi_rent, by = "year") |> 
  mutate(rent = rent * value) |> 
  select(-value)

rm(cpi, cpi_rent)


# Harmonize cmhc and cmhc_nbhd re: missing data ---------------------------

cmhc <-
  cmhc |> 
  filter(sum(is.na(universe)) < 4, .by = id) |>
  filter(sum(is.na(rent)) < 7, .by = id) |>
  filter(n() >= 8, .by = id) |> 
  arrange(id, year)
  
cmhc_nbhd <- 
  cmhc_nbhd |> 
  filter(id %in% cmhc$id)

# Plug gaps one more time
cmhc <- 
  cmhc |> 
  full_join((cmhc_nbhd |> 
               st_drop_geometry() |> 
               select(id) |> 
               rowwise() |> 
               mutate(year = list(2015:2023)) |> 
               ungroup() |> 
               unnest(year)), by = c("id", "year")) |> 
  arrange(id, year)


# Save output -------------------------------------------------------------

qsavem(cmhc, cmhc_nbhd, file = "output/cmhc.qsm", nthreads = availableCores())


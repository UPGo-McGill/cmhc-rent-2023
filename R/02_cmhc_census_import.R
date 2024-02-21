#### CMHC AND CENSUS PROCESSING ################################################

source("R/01_startup.R")


# Import data -------------------------------------------------------------

qload("data/cmhc_data.qsm", nthreads = 10)
qload("data/cmhc_shp.qsm", nthreads = 10)

province <- 
  get_census("CA21", regions = list(C = 1), level = "PR", 
             geo_format = "sf") |> 
  st_transform(4326) |> 
  select(province = name, province_ID = GeoUID, geometry) |> 
  mutate(province = str_remove(province, " \\(.*\\)"))

CMA <- 
  get_census("CA21", regions = list(C = 1), level = "CMA", 
             geo_format = "sf") |> 
  st_transform(3347) |> 
  select(CMA = GeoUID, name_CMA = name, pop_CMA = Population, 
         dwellings_CMA = Dwellings, geometry) |> 
  mutate(name_CMA = str_remove(name_CMA, " \\(.*\\)"))

DA <- 
  map(c(35, 24, 59, 48, 46, 47, 12, 13, 10, 11, 61, 60, 62), \(x) {
    get_census("CA21", regions = list(PR = x), level = "DA",
               vectors = c(rent = "v_CA21_4318",
                           tenants = "v_CA21_4313",
                           owners = "v_CA21_4305",
                           entertainment = "v_CA21_6657",
                           accommodation = "v_CA21_6660",
                           accom_parent = "v_CA21_6606"), geo_format = "sf")})

# Combine output and add area
DA <- 
  DA |> 
  bind_rows() |> 
  as_tibble() |> 
  st_as_sf() |> 
  st_transform(3347) |> 
  mutate(accommodation = entertainment + accommodation) |> 
  select(DA = GeoUID, CMA = CMA_UID, pop = Population, dwellings = Dwellings,
         accommodation, accom_parent, rent, tenants, owners) |> 
  mutate(area_DA = units::drop_units(st_area(geometry)), .before = geometry) |> 
  st_set_agr("constant")

DA <- 
  DA |> 
  mutate(province_ID = substr(DA, 1, 2)) |> 
  inner_join(st_drop_geometry(province), by = "province_ID") |> 
  relocate(province, province_ID, .after = CMA)

province <-
  province |> 
  select(-province_ID)

DA_union <-
  DA |> 
  group_by(province) |> 
  summarize()
  

# Process neighbourhoods --------------------------------------------------

cmhc_nbhd <- 
  bind_rows(
    mutate(cmhc_nbhd_2016, year = 2016),
    mutate(cmhc_nbhd_2017, year = 2017),
    mutate(cmhc_nbhd_2018, year = 2018),
    mutate(cmhc_nbhd_2019, year = 2019),
    mutate(cmhc_nbhd_2020, year = 2020),
    mutate(cmhc_nbhd_2021, year = 2021),
    mutate(cmhc_nbhd_2022, year = 2022)
  ) |> 
  st_transform(4326) |> 
  mutate(id = paste0(METCODE, NBHDCODE)) |> 
  select(id, year, name = NBHDNAME_E, CMA = METCODE) |> 
  mutate(geometry = st_make_valid(geometry))

rm(cmhc_nbhd_2016, cmhc_nbhd_2017, cmhc_nbhd_2018, cmhc_nbhd_2019, 
   cmhc_nbhd_2020, cmhc_nbhd_2021, cmhc_nbhd_2022)

cmhc_PR <- 
  cmhc_nbhd |> 
  st_set_agr("constant") |> 
  st_centroid() |> 
  nngeo::st_nn(province)

cmhc_nbhd <- 
  cmhc_nbhd |> 
  mutate(province = !!province$province[as.numeric(cmhc_PR)], .after = name)

rm(cmhc_PR)


# Add nbhd- and CMA-level data --------------------------------------------

cmhc_nbhd <- 
  cmhc_nbhd |> 
  st_transform(3347) |> 
  mutate(area_CMHC = units::drop_units(st_area(geometry)), 
         .before = geometry) |> 
  st_set_agr("constant")

cmhc_DA <-
  cmhc_nbhd |> 
  st_set_agr("constant") |> 
  st_intersection(DA) |> 
  mutate(area_int = units::drop_units(st_area(geometry)), .before = geometry)

cmhc_DA <- 
  cmhc_DA |> 
  st_drop_geometry() |> 
  summarize(
    pop = sum(pop * area_int / area_DA, na.rm = TRUE),
    dwellings = sum(dwellings * area_int / area_DA, na.rm = TRUE),
    accommodation = sum(accommodation * area_int / area_DA, na.rm = TRUE),
    accom_parent = sum(accom_parent * area_int / area_DA, na.rm = TRUE),
    rent_DA = sum(rent * tenants * area_int / area_DA, na.rm = TRUE) / 
      sum(tenants + area_int / area_DA, na.rm = TRUE),
    tenants_DA = sum(tenants + area_int / area_DA, na.rm = TRUE),
    owners_DA = sum(owners + area_int / area_DA, na.rm = TRUE),
    .by = c(id, year, name, province, CMA)
  )

cmhc_DA <- 
  cmhc_DA |> 
  mutate(
    accommodation = accommodation / accom_parent,
    tenant_share = tenants_DA / (tenants_DA + owners_DA)) |> 
  select(-accom_parent, -tenants_DA, -owners_DA)

cmhc_nbhd <- 
  cmhc_nbhd |> 
  select(-area_CMHC) |> 
  inner_join(cmhc_DA, by = join_by(id, year, name, province, CMA)) |> 
  relocate(geometry, .after = last_col())

rm(cmhc_DA)

cmhc_CMA <- 
  cmhc_nbhd |> 
  summarize(geometry = st_union(geometry), .by = CMA) |> 
  rename(CMA_old = CMA) |> 
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

cmhc_nbhd <- 
  cmhc_nbhd |> 
  inner_join(st_drop_geometry(CMA), by = "CMA") |> 
  relocate(name_CMA, .after = CMA) |> 
  relocate(geometry, .after = last_col())

rm(cmhc_CMA)


# Process data ------------------------------------------------------------

cmhc <- bind_rows(cmhc_2016, cmhc_2017, cmhc_2018, cmhc_2019, cmhc_2020, 
                  cmhc_2021, cmhc_2022) |> 
  mutate(id = paste0(met_code, nbrhood_code)) |> 
  select(id, year = Year, universe, rent = average_rent, 
         rent_rel = average_rent_reliability, vacancy = vacancy_rate_percent,
         vacancy_rel = vacancy_rate_reliability) |> 
  arrange(id, year)

rm(cmhc_2016, cmhc_2017, cmhc_2018, cmhc_2019, cmhc_2020, cmhc_2021, cmhc_2022)


# Save output and clean up ------------------------------------------------

qsavem(cmhc, cmhc_nbhd, file = "output/cmhc.qsm", nthreads = availableCores())

qsave(DA, file = "output/DA.qs", nthreads = availableCores())
qsave(DA_union, file = "output/DA_union.qs", nthreads = availableCores())
qsave(CMA, file = "output/CMA.qs", nthreads = availableCores())
qsave(province, file = "output/province.qs", nthreads = availableCores())

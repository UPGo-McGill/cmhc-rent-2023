#### 10 RENT CHANGE MODELS #####################################################

source("R/06_data_for_models.R")
source("R/07_process_DAGs.R")


# Prepare eigenvectors ----------------------------------------------------

ec <- map(dc, \(x) {
  x |>
    st_transform(4326) |>
    st_set_agr("constant") |>
    st_centroid() |>
    st_coordinates() |>
    meigen(s_id = x$id)
})


# Prepare group effect tables ---------------------------------------------

gc <- map(dc, \(x) bind_cols(id = x$id, CMA_year = paste0(x$CMA, x$year)))


# RE-ESF ------------------------------------------------------------------

# Iterate over adjustment sets
mc <- map(ac, \(x) {
  resf(
    dc$main$rent_change,
    st_drop_geometry(dc$main[x]),
    gc$main,
    meig = ec$main
  )
}) |>
  set_names(names(ac))

# Additional scenarios on common adjustment set
mc$no_imp <- resf(
  dc$no_imp$rent_change,
  st_drop_geometry(dc$no_imp[
    ac$common.1
  ]),
  gc$no_imp,
  meig = ec$no_imp
)
mc$no_vac <- resf(
  dc$no_vac$rent_change,
  st_drop_geometry(dc$no_vac[
    setdiff(ac$common.1, "vacancy_lag_log")
  ]),
  gc$no_vac,
  meig = ec$no_vac
)

# Additional non-Gaussian variant on common adjustment set
mc$non_gauss <- resf(
  dc$main$rent_change,
  st_drop_geometry(dc$main[ac$common.1]),
  gc$main,
  meig = ec$main,
  nongauss = nongauss_y(tr_num = 2)
)

# Additional scenarios with FREH variants
mc$FREH_60 <- resf(
  dc$main$rent_change,
  st_drop_geometry(dc$main[
    str_replace(ac$common.1, "FREH_change", "FREH_60_change")
  ]),
  gc$main,
  meig = ec$main
)
mc$FREH_120 <- resf(
  dc$main$rent_change,
  st_drop_geometry(dc$main[
    str_replace(ac$common.1, "FREH_change", "FREH_120_change")
  ]),
  gc$main,
  meig = ec$main
)


# Pre/post-Covid ---------------------------------------------------------

ec$pre <-
  dc$main |>
  filter(year < 2020) |>
  st_transform(4326) |>
  st_set_agr("constant") |>
  st_centroid() |>
  st_coordinates() |>
  meigen(s_id = filter(dc$main, year < 2020)$id)

ec$post <-
  dc$main |>
  filter(year >= 2020) |>
  st_transform(4326) |>
  st_set_agr("constant") |>
  st_centroid() |>
  st_coordinates() |>
  meigen(s_id = filter(dc$main, year >= 2020)$id)


gc$pre <- bind_cols(
  id = filter(dc$main, year < 2020)$id,
  CMA_year = paste0(
    filter(dc$main, year < 2020)$CMA,
    filter(dc$main, year < 2020)$year
  )
)

gc$post <- bind_cols(
  id = filter(dc$main, year >= 2020)$id,
  CMA_year = paste0(
    filter(dc$main, year >= 2020)$CMA,
    filter(dc$main, year >= 2020)$year
  )
)

# Add pre-Covid models
mc$pre <- map(ac, \(x) {
  resf(
    filter(dc$main, year < 2020)$rent_change,
    st_drop_geometry(filter(dc$main, year < 2020)[x]),
    gc$pre,
    meig = ec$pre
  )
}) |>
  set_names(names(ac))

# Add post-Covid models
mc$post <- map(ac, \(x) {
  resf(
    filter(dc$main, year >= 2020)$rent_change,
    st_drop_geometry(filter(dc$main, year >= 2020)[x]),
    gc$post,
    meig = ec$post
  )
}) |>
  set_names(names(ac))


# S&NVC -------------------------------------------------------------------

mc$sn_common <- resf_vc(
  dc$main$rent_change,
  st_drop_geometry(dc$main[ac$common.1]),
  x_nvc = TRUE,
  xgroup = gc$main,
  meig = ec$main
)
mc$sn_common_force <- resf_vc(
  dc$main$rent_change,
  st_drop_geometry(dc$main[ac$common.1]),
  x_nvc = TRUE,
  xgroup = gc$main,
  meig = ec$main,
  x_sel = 1:4
)


# Save output -------------------------------------------------------------

qsave(mc, file = "output/mc.qs", nthreads = availableCores())

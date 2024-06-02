#### 13 RENT CHANGE MODELS #####################################################

source("R/05_process_DAGs.R")
source("R/09_data_for_models.R")

mc <- list()


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
mc <- map(ac, \(x) resf(dc$main$rent_change, st_drop_geometry(dc$main[x]),
                        gc$main, meig = ec$main)) |> 
  set_names(names(ac))

# Additional scenarios on common adjustment set
mc$no_log <- resf(dc$no_log$rent_change, st_drop_geometry(dc$no_log[
  c(ac$common.1[ac$common.1 != "vacancy_lag_log"], "vacancy_lag")]), gc$no_log, 
  meig = ec$no_log)
mc$no_imp <- resf(dc$no_imp$rent_change, st_drop_geometry(dc$no_imp[
  ac$common.1]), gc$no_imp, meig = ec$no_imp)
mc$no_vac <- resf(dc$no_vac$rent_change, st_drop_geometry(dc$no_vac[ac$common.1[
  ac$common.1 != "vacancy_lag_log"]]), gc$no_vac, meig = ec$no_vac)


# S&NVC -------------------------------------------------------------------

mc$sn_common <- resf_vc(dc$main$rent_change, 
                        st_drop_geometry(dc$main[ac$common.1]), 
                        x_nvc = TRUE, xgroup = gc$main, meig = ec$main)
mc$sn_common_force <- resf_vc(dc$main$rent_change, 
                              st_drop_geometry(dc$main[ac$common.1]), 
                              x_nvc = TRUE, xgroup = gc$main, meig = ec$main, 
                              x_sel = 1:4)


# Linear models -----------------------------------------------------------

mc$l_common <-
  ac$common.1 |> 
  paste(collapse = " + ") |> 
  paste0(x = "rent_change ~ ", y = _, " + id + CMA:year") |> 
  lm(data = dc$main)


# Save output -------------------------------------------------------------

qsave(mc, file = "output/mc.qs", nthreads = availableCores())

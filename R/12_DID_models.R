#### 12 DIFFERENCE-IN-DIFFERENCES MODELS #######################################

source("R/01_startup.R")
source("R/06_data_for_models.R")
qload("output/cmhc.qsm", nthreads = availableCores())


# Prepare data ------------------------------------------------------------

reg <-
  qread("data/reg.qs") |> 
  mutate(reg = if_else(reg == "TBD", FALSE, as.logical(reg))) |> 
  mutate(date = if_else(date >= "2023-01-02", NA, date)) |> 
  mutate(reg = if_else(is.na(date), FALSE, TRUE)) |> 
  inner_join(st_drop_geometry(cmhc_nbhd), by = c("id", "name")) |> 
  select(-c(pop:tenant))

dd <-
  map(dr, \(x) {
    x |> 
      inner_join(select(reg, id, date, reg), by = "id") |> 
      mutate(treat = if_else(is.na(date), 0, year(date - 1) + 1), 
             # Add prefix so leading 0s don't get removed
             id = as.numeric(paste0("1111", id))) |> 
      filter(treat == 0 | treat > 2017) |> 
      # Remove provinces with no treatment in the time period
      filter(!province %in% c("Manitoba", "Saskatchewan", "Nova Scotia", 
                              "Prince Edward Island", "Alberta",
                              "Newfoundland and Labrador"))
  })

dd$all <- 
  dr$main |> 
  inner_join(select(reg, id, date, reg), by = "id") |> 
  mutate(treat = if_else(is.na(date), 0, year(date - 1) + 1), 
         id = as.numeric(paste0("1111", id)))

# Variant with Vancouver treatment delayed by a year to account for slow
# regulatory progress
dd$van <- 
  dr$main |> 
  inner_join(select(mutate(reg, date = if_else(
    name_CSD == "Vancouver", as.Date("2019-08-31"), date)), id, date, reg), 
    by = "id") |> 
  mutate(treat = if_else(is.na(date), 0, year(date - 1) + 1), 
         id = as.numeric(paste0("1111", id))) |> 
  filter(treat == 0 | treat > 2017) |> 
  # Remove provinces with no treatment in the time period
  filter(!province %in% c("Manitoba", "Saskatchewan", "Nova Scotia", 
                          "Prince Edward Island", "Alberta",
                          "Newfoundland and Labrador"))


# Fit models --------------------------------------------------------------

md_vars <- c("rent_log", "FREH", "non_FREH", "price")

md <- map(dd, \(y) {
  md_vars |> 
    set_names(md_vars) |> 
    map(\(x) {
      anticip = if_else(x == "rent_log", 0, 1)
      att_gt(x, tname = "year", idname = "id", gname = "treat", 
                    allow_unbalanced_panel = TRUE, data = y)
      })}) |> 
  set_names(names(dd)) |> 
  suppressWarnings()


# Revert IDs --------------------------------------------------------------

dd <- map(dd, \(x) mutate(x, id = str_remove(as.character(id), "^1111")))


# Save output -------------------------------------------------------------

qsave(dd, "output/dd.qs")
qsave(md, "output/md.qs")

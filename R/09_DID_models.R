#### 09 DIFFERENCE-IN-DIFFERENCES MODELS #######################################

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
      inner_join(select(reg, id, date, reg, name_CSD, name_CMA), by = "id") |>
      mutate(
        treat = if_else(is.na(date), 0, year(date - 1) + 1),
        # Add prefix so leading 0s don't get removed
        id = as.numeric(paste0("1111", id))
      ) |>
      filter(treat == 0 | treat > 2017) |>
      # Remove provinces with no treatment in the time period
      filter(
        !province %in%
          c(
            "Manitoba",
            "Saskatchewan",
            "Nova Scotia",
            "Prince Edward Island",
            "Alberta",
            "Newfoundland and Labrador",
            "Northwest Territories"
          )
      )
  })


# Data variants ----------------------------------------------------------

# Variant with all provinces
dd$all <-
  dr$main |>
  inner_join(select(reg, id, date, reg, name_CSD, name_CMA), by = "id") |>
  mutate(
    treat = if_else(is.na(date), 0, year(date - 1) + 1),
    id = as.numeric(paste0("1111", id))
  )

# Variant with Vancouver treatment delayed by a year to account for slow
# regulatory progress
dd$van <-
  dr$main |>
  inner_join(
    select(
      mutate(
        reg,
        date = if_else(
          name_CSD == "Vancouver",
          as.Date("2019-08-31"),
          date
        )
      ),
      id,
      date,
      reg,
      name_CSD,
      name_CMA
    ),
    by = "id"
  ) |>
  mutate(
    treat = if_else(is.na(date), 0, year(date - 1) + 1),
    id = as.numeric(paste0("1111", id))
  ) |>
  filter(treat == 0 | treat > 2017) |>
  # Remove provinces with no treatment in the time period
  filter(
    !province %in%
      c(
        "Manitoba",
        "Saskatchewan",
        "Nova Scotia",
        "Prince Edward Island",
        "Alberta",
        "Newfoundland and Labrador",
        "Northwest Territories"
      )
  )

# Variant with paired treatment and non-treatment cities
top_309_no_treat <-
  dr$main |>
  inner_join(select(reg, id, date, reg, name_CSD, name_CMA), by = "id") |>
  mutate(
    treat = if_else(is.na(date), 0, year(date - 1) + 1),
    # Add prefix so leading 0s don't get removed
    id = as.numeric(paste0("1111", id))
  ) |>
  # Remove provinces with no treatment in the time period
  filter(
    !province %in%
      c(
        "Manitoba",
        "Saskatchewan",
        "Nova Scotia",
        "Prince Edward Island",
        "Alberta",
        "Newfoundland and Labrador",
        "Northwest Territories"
      )
  ) |>
  filter(treat == 0) |>
  summarize(rent = mean(rent), .by = id) |>
  arrange(-rent) |>
  slice(1:177) |>
  pull(id)

dd$pair <-
  dr$main |>
  inner_join(select(reg, id, date, reg, name_CSD, name_CMA), by = "id") |>
  mutate(
    treat = if_else(is.na(date), 0, year(date - 1) + 1),
    # Add prefix so leading 0s don't get removed
    id = as.numeric(paste0("1111", id))
  ) |>
  filter(treat > 2017 | id %in% top_309_no_treat)

# Variants with FREH_60 and FREH_120
dd$FREH_60 <-
  dr$main |>
  select(
    id:rent,
    FREH = FREH_60,
    non_FREH = non_FREH_60,
    price:rent_log,
    FREH_log = FREH_60_log,
    non_FREH_log = non_FREH_60_log,
    price_log:rent_raw,
    FREH_raw = FREH_60_raw,
    non_FREH_raw = non_FREH_60_raw,
    price_raw:rent_log_raw,
    FREH_log_raw = FREH_60_log_raw,
    non_FREH_log_raw = non_FREH_60_log_raw,
    price_log_raw
  ) |>
  inner_join(select(reg, id, date, reg, name_CSD, name_CMA), by = "id") |>
  mutate(
    treat = if_else(is.na(date), 0, year(date - 1) + 1),
    # Add prefix so leading 0s don't get removed
    id = as.numeric(paste0("1111", id))
  ) |>
  filter(treat == 0 | treat > 2017) |>
  # Remove provinces with no treatment in the time period
  filter(
    !province %in%
      c(
        "Manitoba",
        "Saskatchewan",
        "Nova Scotia",
        "Prince Edward Island",
        "Alberta",
        "Newfoundland and Labrador",
        "Northwest Territories"
      )
  )

dd$FREH_120 <-
  dr$main |>
  select(
    id:rent,
    FREH = FREH_120,
    non_FREH = non_FREH_120,
    price:rent_log,
    FREH_log = FREH_120_log,
    non_FREH_log = non_FREH_120_log,
    price_log:rent_raw,
    FREH_raw = FREH_120_raw,
    non_FREH_raw = non_FREH_120_raw,
    price_raw:rent_log_raw,
    FREH_log_raw = FREH_120_log_raw,
    non_FREH_log_raw = non_FREH_120_log_raw,
    price_log_raw
  ) |>
  inner_join(select(reg, id, date, reg, name_CSD, name_CMA), by = "id") |>
  mutate(
    treat = if_else(is.na(date), 0, year(date - 1) + 1),
    # Add prefix so leading 0s don't get removed
    id = as.numeric(paste0("1111", id))
  ) |>
  filter(treat == 0 | treat > 2017) |>
  # Remove provinces with no treatment in the time period
  filter(
    !province %in%
      c(
        "Manitoba",
        "Saskatchewan",
        "Nova Scotia",
        "Prince Edward Island",
        "Alberta",
        "Newfoundland and Labrador",
        "Northwest Territories"
      )
  )

# Post-Covid variant
dd$covid <-
  dr$main |>
  inner_join(select(reg, id, date, reg, name_CSD, name_CMA), by = "id") |>
  mutate(
    treat = if_else(is.na(date), 0, year(date - 1) + 1),
    # Add prefix so leading 0s don't get removed
    id = as.numeric(paste0("1111", id))
  ) |>
  filter(treat == 0 | treat > 2017) |>
  filter(year >= 2020) |>
  # Remove provinces with no treatment in the time period
  filter(
    !province %in%
      c(
        "Manitoba",
        "Saskatchewan",
        "Nova Scotia",
        "Prince Edward Island",
        "Alberta",
        "Newfoundland and Labrador",
        "Northwest Territories"
      )
  )

# Spillover variant
cmhc_adj <-
  cmhc_nbhd |>
  # Add prefix so leading 0s don't get removed
  mutate(id = as.numeric(paste0("1111", id)))

cmhc_adj <-
  cmhc_adj |>
  mutate(
    adj = map(st_touches(geometry), \(x) {
      cmhc_adj$id[x]
    })
  ) |>
  select(id, adj) |>
  st_drop_geometry()

dd$adj <-
  dr$main |>
  inner_join(select(reg, id, date, reg, name_CSD, name_CMA), by = "id") |>
  mutate(
    treat = if_else(is.na(date), 0, year(date - 1) + 1),
    # Add prefix so leading 0s don't get removed
    id = as.numeric(paste0("1111", id))
  ) |>
  filter(treat == 0 | treat > 2017) |>
  inner_join(cmhc_adj, by = join_by(id)) |>
  # Remove provinces with no treatment in the time period
  filter(
    !province %in%
      c(
        "Manitoba",
        "Saskatchewan",
        "Nova Scotia",
        "Prince Edward Island",
        "Alberta",
        "Newfoundland and Labrador",
        "Northwest Territories"
      )
  ) |>
  filter(treat == 0, lengths(adj) > 0) |>
  mutate(
    treat = map2_dbl(adj, year, \(x, y) {
      treats <-
        dd$main |>
        filter(year == y, id %in% x) |>
        pull(treat)

      if (max(treats != 0)) {
        min(treats[treats > 0])
      } else {
        min(treats)
      }
    })
  ) |>
  filter_out(is.infinite(treat)) |>
  suppressWarnings()

# Residualized rent variants
dd$resid_cma_year <-
  dd$main |>
  mutate(
    rent_log = resid(lm(
      rent_log ~ factor(name_CMA) + factor(year),
      data = dd$main
    ))
  )

dd$resid_cma_by_year <-
  dd$main |>
  mutate(
    rent_log = resid(lm(
      rent_log ~ factor(name_CMA):factor(year),
      data = dd$main
    ))
  )


# Fit models --------------------------------------------------------------

md_vars <- c("rent_log", "FREH_log", "non_FREH_log", "price_log")

md <- map(dd, \(y) {
  md_vars |>
    set_names(md_vars) |>
    map(\(x) {
      att_gt(
        x,
        tname = "year",
        idname = "id",
        gname = "treat",
        clustervars = "name_CMA",
        allow_unbalanced_panel = TRUE,
        data = y
      )
    })
}) |>
  set_names(names(dd)) |>
  suppressWarnings()

md$no_2023$rent_log <-
  att_gt(
    "rent_log",
    tname = "year",
    idname = "id",
    gname = "treat",
    allow_unbalanced_panel = TRUE,
    data = filter(dd$main, year <= 2022)
  )

md_no_pool <-
  map(dd, \(y) {
    md_vars |>
      set_names(md_vars) |>
      map(\(x) {
        att_gt(
          x,
          tname = "year",
          idname = "id",
          gname = "treat",
          allow_unbalanced_panel = TRUE,
          data = y
        )
      })
  }) |>
  set_names(names(dd)) |>
  suppressWarnings()

md_CSD <-
  map(dd, \(y) {
    md_vars |>
      set_names(md_vars) |>
      map(\(x) {
        att_gt(
          x,
          tname = "year",
          idname = "id",
          gname = "treat",
          clustervars = "name_CSD",
          allow_unbalanced_panel = TRUE,
          data = y
        )
      })
  }) |>
  set_names(names(dd)) |>
  suppressWarnings()


# Revert IDs --------------------------------------------------------------

dd <- map(dd, \(x) mutate(x, id = str_remove(as.character(id), "^1111")))


# Save output -------------------------------------------------------------

qsave(dd, "output/dd.qs")
qsave(md, "output/md.qs")
qsave(md_no_pool, "output/md_no_pool.qs")
qsave(md_CSD, "output/md_CSD.qs")

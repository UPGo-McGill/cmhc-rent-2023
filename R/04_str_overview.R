#### 04 STR OVERVIEW ###########################################################

source("R/01_startup.R")
monthly <- qread("output/monthly.qs", nthreads = availableCores())
qload("output/cmhc.qsm", nthreads = availableCores())
monthly_sept <- qread("output/monthly_sept.qs", nthreads = availableCores())
CSD <- qread("output/CSD.qs", nthreads = availableCores())
cpi <- 
  read_csv("data/CPI.csv") |> 
  select(REF_DATE, GEO, product = `Products and product groups`, UOM, 
         value = VALUE) |> 
  filter(product %in% c("All-items", "Shelter"), GEO == "Canada",
         UOM == "2002=100") |>
  mutate(month = yearmonth(REF_DATE)) |>
  select(month, product, value) |>
  filter(month >= yearmonth("2014-10"))


# Trends in rent ----------------------------------------------------------

fig_1 <- 
  cmhc |> 
  inner_join(st_drop_geometry(select(cmhc_nbhd, id, province, CMA))) |> 
  filter(!is.na(rent)) |> 
  mutate_region() |> 
  summarize(rent = mean(rent), .by = c(region, year)) |> 
  filter(!is.na(region)) |> 
  bind_rows(
    cmhc |> 
      inner_join(st_drop_geometry(select(cmhc_nbhd, id, province, CMA))) |> 
      filter(!is.na(rent)) |> 
      summarize(rent = mean(rent), .by = year) |> 
      mutate(region = "Canada total")) |> 
  mutate(CA = region == "Canada total") |> 
  mutate(region = factor(region, levels = c(
    "Canada total", "Atlantic", "British Columbia", "Ontario", "Prairies",
    "Quebec"))) |> 
  ggplot(aes(year, rent, colour = region, linetype = region, lwd = CA)) +
  geom_line() +
  scale_linewidth_manual(values = c("TRUE" = 2, "FALSE" = 1), 
                         guide = NULL) +
  scale_colour_brewer(name = NULL, palette = "Dark2") +
  scale_linetype(name = NULL) +
  scale_x_continuous(name = NULL) +
  scale_y_continuous(name = "Average monthly rent", labels = scales::dollar) +
  theme_minimal() +
  theme(text = element_text(family = "Futura"),
        legend.position = "bottom")

ggsave("output/figure_1.png", fig_1, width = 8, height = 4, units = "in")
  
# 2015-2023 rent change
cmhc |> 
  inner_join(st_drop_geometry(select(cmhc_nbhd, id, province, CMA))) |> 
  summarize(rent = sum(rent * universe, na.rm = TRUE) / 
              sum(universe, na.rm = TRUE), .by = year)

# Annualized increase: 4.4%
899 * 1.044 ^ 8

# 2015-2023 inflation
cpi |> 
  filter(month %in% c(yearmonth("2015-10"), yearmonth("2023-10")),
         product == "All-items") |> 
  summarize(change = value[2] / value[1] - 1)

# Annualized increase: 2.8%
1.028 ^ 8


# Trends in FREH ----------------------------------------------------------

# Active/FREH/FREH_3 listings by region
fig_2 <-
  monthly |> 
  rename(province = region) |> 
  mutate_region() |> 
  filter(!is.na(region)) |> 
  filter(month >= yearmonth("2015-10")) |> 
  summarize(active = sum(A + R), FREH = sum(FREH), .by = c(region, month)) |>
  bind_rows(
    monthly |> 
      filter(month >= yearmonth("2015-10")) |> 
      summarize(region = "Canada", active = sum(A + R), FREH = sum(FREH), 
                .by = c(month))) |> 
  mutate_days() |> 
  mutate(active = active / days) |> 
  select(-days) |> 
  mutate(region = factor(region, levels = c(
    "Canada", "Atlantic", "British Columbia", "Ontario", "Prairies", "Québec"
    ))) |> 
  rename(`Active listings` = active,
         `FREH listings` = FREH) |> 
  pivot_longer(`Active listings`:`FREH listings`) |> 
  ggplot(aes(month, value, linetype = name, colour = name)) +
  geom_line() +
  facet_wrap(~region, scales = "free_y") +
  scale_colour_brewer(name = NULL, palette = "Dark2") +
  scale_x_yearmonth(name = NULL) +
  scale_y_continuous(name = NULL, labels = scales::comma) +
  scale_linetype(name = NULL) +
  theme_minimal() +
  theme(text = element_text(family = "Futura"),
        legend.position = "bottom")

ggsave("output/figure_2.png", fig_2, width = 8, height = 4, units = "in")

# Sep 2022 figures
monthly |> 
  filter(month == max(month)) |> 
  summarize(active = sum(A + R) / 30,
            FREH = sum(FREH))

# Change from pre-pandemic peak
monthly |> 
  summarize(active = sum(A + R) / 30, FREH = sum(FREH), .by = month) |> 
  summarize(active = active[month == max(month)] / 
              max(active[month < yearmonth("2020-03")]) - 1,
            FREH = FREH[month == max(month)] / 
              max(FREH[month < yearmonth("2020-03")]) - 1)

# STR density by city
fig_3 <-
  monthly |> 
  inner_join(qread("output/prop_csd.qs", nthreads = availableCores()), 
             by = "property_ID") |>
  relocate(CSD, .after = city) |> 
  summarize(active = sum(A + R), .by = c(month, CSD)) |> 
  mutate_days() |> 
  mutate(active = active / days) |> 
  select(-days) |> 
  inner_join(CSD, by = "CSD") |> 
  mutate(active_households = active / households_CSD,
         active_dwellings = active / dwellings_CSD) |> 
  mutate(dwell_size = case_when(
    dwellings_CSD <= 2000 ~ "0-2,000",
    dwellings_CSD <= 5000 ~ "2,001-5,000",
    dwellings_CSD <= 10000 ~ "5,001-10,000",
    dwellings_CSD <= 50000 ~ "10,001-50,000",
    dwellings_CSD <= 100000 ~ "50,001-100,000",
    dwellings_CSD >= 100000 ~ "> 100,000")) |> 
  mutate(dwell_size = factor(dwell_size, levels = c(
    "0-2,000", "2,001-5,000", "5,001-10,000", "10,001-50,000", 
    "50,001-100,000", "> 100,000"))) |> 
  filter(!is.na(dwell_size)) |> 
  filter(!is.infinite(active_dwellings)) |> 
  summarize(active_dwellings = mean(active_dwellings, na.rm = TRUE), 
            .by = c(month, dwell_size)) |> 
  filter(month >= yearmonth("2015-10")) |> 
  ggplot(aes(month, active_dwellings, colour = dwell_size, 
             linetype = dwell_size)) +
  geom_line() +
  scale_x_yearmonth(name = NULL) +
  scale_y_continuous(name = "Active STRs as % of all dwellings",
                     labels = scales::percent) +
  scale_colour_brewer(name = "Community size", palette = "Dark2") +
  scale_linetype(name = "Community size") +
  theme_minimal() +
  theme(text = element_text(family = "Futura"),
        legend.position = "bottom")
  
ggsave("output/figure_3.png", fig_3, width = 8, height = 4, units = "in")

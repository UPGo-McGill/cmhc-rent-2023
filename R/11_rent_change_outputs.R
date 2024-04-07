#### RENT CHANGE MODEL OUTPUTS #################################################

source("R/10_rent_change_models.R")
DA_union <- qread("output/DA_union.qs", nthreads = availableCores())
province <- qread("output/province.qs", nthreads = availableCores())

# Get largest six CMAs to map residuals for
largest_CMAs <- 
  dc$main |> 
  st_drop_geometry() |> 
  count(CMA, sort = TRUE) |> 
  slice(1:6) |> 
  pull(CMA)


### Model summaries ############################################################

# Table 5: OLS and RE-ESF models ------------------------------------------

# OLS models
modelsummary(list(FREH = mc$l_FREH, rev = mc$l_rev, Both = mc$l_both), 
             coef_map = c("(Intercept)", "FREH_change", "rev_change", 
                          "universe_change", "universe_log", "tenant", 
                          "tourism_log"), stars = TRUE,
             add_rows = tibble(x = "Fixed effects",
                               y1 = "CMA by year",
                               y2 = "CMA by year",
                               y3 = "CMA by year") |>
               structure(position = 15),
             output = "kableExtra")

# RE-ESF model
mc$sf_both

# S&NVC model
mc$sn_both

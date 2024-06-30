#### 08 PROCESS DAGs ###########################################################

source("R/01_startup.R")


# Define DAGs -------------------------------------------------------------

hc <- list()

hc$common <- dagitty('dag {
bb="-2,-8.016,8.841,5.107"
"Competition with hotels" [latent,pos="2.730,-1.434"]
"Landlord STR decision" [latent,pos="2.459,-4.598"]
"Landlord mix" [latent,pos="5.628,-1.630"]
"Rent stickiness" [latent,pos="6.460,0.619"]
"Resident STR decision" [latent,pos="2.499,-2.604"]
Demand [latent,pos="6.355,-2.141"]
FREH_change [pos="4.258,-4.412"]
FREH_lag_log [pos="0.455,-5.514"]
Supply [latent,pos="6.418,-4.929"]
U1 [latent,pos="6.270,-6.180"]
U2 [latent,pos="6.270,-3.583"]
U3 [latent,pos="7.514,0.116"]
U4 [latent,pos="-0.957,-4.439"]
U5 [latent,pos="-1.031,-2.916"]
U6 [latent,pos="-1.062,-0.319"]
apart_log [pos="2.709,-6.316"]
income_log [pos="3.837,-6.710"]
non_FREH_change [pos="4.248,-3.229"]
non_FREH_lag_log [pos="0.434,-3.977"]
price_change [pos="4.279,-1.770"]
price_lag_log [pos="0.444,-2.386"]
Regulation [latent,pos="1.477,-6.575"]
rent_change [outcome,pos="7.859,-2.026"]
rent_lag_log [pos="0.455,0.497"]
tourism_log [pos="5.048,-6.343"]
universe_change [pos="7.661,-5.731"]
vacancy_lag_log [pos="0.465,-0.958"]
"Competition with hotels" -> price_change
"Landlord STR decision" -> FREH_change
"Landlord mix" -> "Rent stickiness"
"Rent stickiness" -> rent_change
"Resident STR decision" -> non_FREH_change
Demand -> rent_change
FREH_change -> Supply
FREH_lag_log -> FREH_change
Supply -> rent_change
U1 -> Supply
U2 -> Demand
U3 -> "Rent stickiness"
U4 -> FREH_lag_log
U4 -> non_FREH_lag_log
U4 -> price_lag_log
U5 -> rent_lag_log
U5 -> vacancy_lag_log
U6 -> FREH_lag_log
U6 -> non_FREH_lag_log
U6 -> price_lag_log
U6 -> rent_lag_log
U6 -> vacancy_lag_log
apart_log -> "Landlord mix"
apart_log -> "Resident STR decision"
apart_log -> income_log
apart_log -> non_FREH_lag_log
apart_log -> rent_lag_log
apart_log -> tourism_log
apart_log -> vacancy_lag_log
income_log -> Demand
income_log -> price_lag_log
income_log -> rent_lag_log
non_FREH_change -> Demand
non_FREH_lag_log -> non_FREH_change
price_change -> "Rent stickiness"
price_lag_log -> "Landlord STR decision"
price_lag_log -> "Resident STR decision"
price_lag_log -> price_change
Regulation -> "Landlord STR decision"
Regulation -> "Resident STR decision"
rent_lag_log -> "Landlord STR decision"
rent_lag_log -> "Rent stickiness"
rent_lag_log -> "Resident STR decision"
rent_lag_log -> price_change
tourism_log -> FREH_lag_log
tourism_log -> non_FREH_lag_log
tourism_log -> income_log
tourism_log -> price_change
tourism_log -> price_lag_log
universe_change -> "Rent stickiness"
universe_change -> Supply
vacancy_lag_log -> "Landlord STR decision"
vacancy_lag_log -> "Rent stickiness"
}')

hc$FREH <- 
  hc$common |> 
  setVariableStatus("exposure", "FREH_change")

hc$non_FREH <- 
  hc$common |> 
  setVariableStatus("exposure", "non_FREH_change")

hc$price <- 
  hc$common |> 
  setVariableStatus("exposure", "price_change")


# Adjustment sets ---------------------------------------------------------

ac_order <- c("FREH_change", "non_FREH_change", "price_change", "rent_lag_log",
              "FREH_lag_log", "non_FREH_lag_log", "price_lag_log",
              "vacancy_lag_log", "apart_log", "income_log")
ac <- list()
ac$FREH <- map(adjustmentSets(hc$FREH), \(x) sort(c(x, "FREH_change")))
ac$non_FREH <- map(adjustmentSets(hc$non_FREH), 
                   \(x) sort(c(x, "non_FREH_change")))
ac$price <- map(adjustmentSets(hc$price), \(x) sort(c(x, "price_change")))

# Find common adjustment set
ac$common <- intersect(intersect(ac$FREH, ac$non_FREH), ac$price)
stopifnot(length(ac$common) == 1)

# Remove common adjustment set from variable-specific sets
ac$FREH <- ac$FREH[!map_lgl(ac$FREH, \(x) all(x == ac$common[[1]]))] |> 
  suppressWarnings()
ac$non_FREH <- ac$non_FREH[
  !map_lgl(ac$non_FREH, \(x) all(x == ac$common[[1]]))] |>
  suppressWarnings()
ac$price <- ac$price[!map_lgl(ac$price, \(x) all(x == ac$common[[1]]))] |> 
  suppressWarnings()

# Produce unified list
ac <- unlist(ac, recursive = FALSE)
ac <- c(ac["common.1"], ac[-which(names(ac) == "common.1")])
ac <- map(ac, \(x) x[order(match(x, ac_order))])

# Remove additional duplicates
if (all(ac$FREH.2 == ac$non_FREH.2)) {
  ac$FREH_non_FREH.2 <- ac$FREH.2
  ac$non_FREH.2 <- NULL
  ac$FREH.2 <- NULL
}

if (all(ac$FREH.3 == ac$non_FREH.3)) {
  ac$FREH_non_FREH.3 <- ac$FREH.3
  ac$non_FREH.3 <- NULL
  ac$FREH.3 <- NULL
}

# Add dummy variables where needed
ac <- map(ac, \(x) {
  
  if (any("FREH_lag_log" %in% x)) x <- c(x, "FREH_lag_dummy")
  if (any("non_FREH_lag_log" %in% x)) x <- c(x, "non_FREH_lag_dummy")
  if (any("price_lag_log" %in% x)) x <- c(x, "price_lag_dummy")
  x
  
})


# Figure 5 ----------------------------------------------------------------

# fig_5 <-
#   hc$FREH |>
#   tidy_dagitty() |>
#   node_status() |>
#   node_ancestors("FREH_change") |>
#   select(everything(), FREH_anc = ancestor) |>
#   node_ancestors("rent_change") |>
#   mutate(status = case_when(
#     status == "exposure" ~ "b",
#     status == "outcome" ~ "a",
#     status == "latent" ~ "f",
#     name == "FREH_lag_log" ~ "c",
#     FREH_anc == "ancestor" & ancestor == "ancestor" ~ "e",
#     ancestor == "ancestor" ~ "d")) |>
#   mutate(label = case_when(
#     status == "a" ~ "Outcome",
#     status == "b" ~ "Exposure",
#     status == "c" ~ "Ancestor\n(treat.)",
#     status == "d" ~ "Ancestor\n(outcome)",
#     status == "e" ~ "Ancestor\n(both)",
#     status == "f" ~ "Latent")) |>
#   mutate(y = y * -1, yend = yend * -1) |>
#   ggplot(aes(x = x, y = y, xend = xend, yend = yend, colour = status)) +
#   geom_dag_edges(edge_width = 0.2) +
#   geom_dag_point(size = 16) +
#   geom_label(aes(label = name), family = "Futura", size = 3, 
#              show.legend = FALSE) +
#   scale_colour_manual(name = NULL, values = c(
#     a = "#1b9e77", b = "#d95f02", c = "#e6ab02", d = "#7570b3", e = "#e7298a",
#     f = "grey50"),
#     labels = c("Outcome", "Treatment", "Ancestor of treatment",
#                "Ancestor of outcome", "Ancestor of both", "Latent")) +
#   guides(colour = guide_legend(nrow = 1)) +
#   theme_dag() +
#   theme(plot.background = element_rect(colour = "transparent", fill = "white"),
#         text = element_text(family = "Futura"), legend.position = "bottom")
# 
# ggsave("output/figure_5.png", fig_5, width = 12, height = 7, units = "in")


# Clean up ----------------------------------------------------------------

rm(ac_order)

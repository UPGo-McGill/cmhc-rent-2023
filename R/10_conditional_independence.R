#### 10 CONDITIONAL INDEPENDENCE TESTS #########################################

source("R/05_process_DAGs.R")
source("R/08_data_for_models.R")


# Conditional independence tests ------------------------------------------

tc <- list()

tc <- map(1:4, \(x) {
  hc$common |> 
    localTests(data = select(st_drop_geometry(dc$main), 
                             -c(id:vacancy_lag_dummy)), 
               type = "cis", abbreviate.names = FALSE, 
               max.conditioning.variables = x, conf.level = 0.95) |> 
    as_tibble(rownames = "vars") |> 
    set_names(c("vars", "estimate", "p", "low", "high"))
}) |> 
  set_names(paste0("var_", 1:4))

# Test successes
map(tc, \(x) count(x, pass = low * high <= 0))

test_failures <- map(tc, \(x) {
  x |> 
    filter(low * high > 0) |> 
    pull(vars) |> 
    str_split(" \\_\\|\\|\\_ ") |> 
    unlist() |> 
    str_split(" \\| ") |> 
    unlist() |> 
    str_split(", ") |> 
    unlist() |> 
    table()
})


# Figure 6: Conditional independence --------------------------------------

fig_6 <- 
  tc$var_2 |> 
  ggplot() +
  geom_vline(xintercept = 0, colour = "grey70") +
  geom_pointrange(aes(x = estimate, 
                      y = vars,
                      xmin = low, xmax = high),
                  size = 0.2, linewidth = 0.8) +
  scale_y_discrete(name = NULL) +
  scale_x_continuous(name = "Pearson correlation coefficient", 
                     limits = c(-1, 1)) +
  theme_minimal() +
  theme(text = element_text(family = "futura"))

ggsave("output/figure_6.png", fig_6, width = 8, height = 5, units = "in")

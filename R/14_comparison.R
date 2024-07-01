#### Comparison of model results ###############################################

source("R/05_process_DAGs.R")
source("R/08_data_for_models.R")
qload("output/cmhc.qsm", nthreads = availableCores())
source("R/06_imputation.R")
dd <- qread("output/dd.qs")
mc <- qread("output/mc.qs", nthreads = availableCores())
md <- qread("output/md.qs")


# First year treatment effects from DiD -----------------------------------

treat_rent <- aggte(md$main$rent_log, "dynamic")
treat_FREH <- aggte(md$main$FREH, "dynamic")
treat_non_FREH <- aggte(md$main$non_FREH, "dynamic")

treat_table <- 
  bind_rows(
    tibble(var = "rent_log", time = treat_rent$egt, 
           effect = treat_rent$att.egt),
    tibble(var = "FREH", time = treat_FREH$egt, effect = treat_FREH$att.egt),
    tibble(var = "non_FREH", time = treat_non_FREH$egt, 
           effect = treat_non_FREH$att.egt)) |> 
  filter(time >= 0, time <= 3)

treat_table |> 
  pivot_wider(names_from = "var", values_from = "effect") |> 
  ggplot(aes(FREH, rent_log)) +
  geom_point()

# -0.132 FREH and -0.129 non_FREH -> -0.0461 rent_log
  
  

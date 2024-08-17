#### 14 MODEL COMPARISON #######################################################

source("R/06_data_for_models.R")
source("R/08_process_DAGs.R")
qload("output/cmhc.qsm", nthreads = availableCores())
source("R/05_imputation.R")
dd <- qread("output/dd.qs")
mc <- qread("output/mc.qs", nthreads = availableCores())
md <- qread("output/md.qs")


# Treatment effects from DiD ----------------------------------------------

# 2017-2022 average ATT
aggte(md$no_2023$rent_log, "simple")
aggte(md$main$FREH, "simple")
aggte(md$main$non_FREH, "simple")

# Average rent
exp(mean(dr$main$rent_log_raw, na.rm = TRUE))

# DiD treatment at average
exp(mean(dr$main$rent_log_raw, na.rm = TRUE)) - (
  exp(mean(dr$main$rent_log_raw, na.rm = TRUE) + 
        sd(dr$main$rent_log_raw, na.rm = TRUE) * 
        aggte(md$no_2023$rent_log, "simple")$overall.att))

# RE-ESF treatment at average
(aggte(md$main$FREH, "simple")$overall.att * mc$common.1$b$Estimate[2] +
  aggte(md$main$non_FREH, "simple")$overall.att * mc$common.1$b$Estimate[3])

# RE-ESF effect at average
(aggte(md$main$FREH, "simple")$overall.att * mc$common.1$b$Estimate[2] +
    aggte(md$main$non_FREH, "simple")$overall.att * mc$common.1$b$Estimate[3]) *
  sd(dr$main$rent_raw, na.rm = TRUE)

# Ratio of treatments
(exp(mean(dr$main$rent_log_raw, na.rm = TRUE)) - (
  exp(mean(dr$main$rent_log_raw, na.rm = TRUE) + 
        sd(dr$main$rent_log_raw, na.rm = TRUE) * 
        aggte(md$no_2023$rent_log, "simple")$overall.att))) / 
  (sd(dr$main$rent_raw, na.rm = TRUE) * (
    aggte(md$main$FREH, "simple")$overall.att * mc$common.1$b$Estimate[2] +
      aggte(md$main$non_FREH, "simple")$overall.att * 
      mc$common.1$b$Estimate[3]))

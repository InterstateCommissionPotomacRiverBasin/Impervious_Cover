





#==============================================================================
# Env x Env Correlation
#==============================================================================
corr_env_env <- function(all.df, scenario) {
  env.col <- c("AREA", "KARST", "PRECIP", "KFACT", "AVGKW", "FCODE", 
               "SLOPE", "SOIL")
  env.names <- c("Area", "Karst", "Precip", "Soil Erodibility Factor", 
                 "Mean Soil Erodibility", "Physiographic Province",
                 "Average Watershed Slope", "Soil Group")
  #------------------------------------------------------------------------------
  env.corr <- cor(all.df[, env.col], method = "spearman") 
  env.corr[upper.tri(env.corr, diag = FALSE)] <- NA
  env.corr <- data.frame(round(env.corr, 2))
  env.corr$WC1 <- row.names(env.corr)
  env.corr <- env.corr[, c(ncol(env.corr), 1:(ncol(env.corr) - 1))]
  #------------------------------------------------------------------------------
  final.corr <- env.corr
  names(final.corr) <- c("", env.names)
  final.corr[[1]] <- env.names
  final.corr[is.na(final.corr)] <- ""
  #------------------------------------------------------------------------------
  main.dir <- "output"
  todays.date <- Sys.Date()
  scenario.folder <- ifelse(scenario %in% "cur", "baseline_current",
                            ifelse(scenario %in% "imp", "baseline_impervious", "ERROR"))
  main.path <- file.path(main.dir, todays.date, "Corr", "Env_Env", scenario.folder)
  if (!dir.exists(main.path)){
    dir.create(main.path, recursive = TRUE)
  }
  #----------------------------------------------------------------------------
  file.name <- paste0(scenario, "_env_env_corr_", todays.date, ".csv")
  output.path <- file.path(main.path, file.name)
  write.csv(final.corr, output.path, row.names =  FALSE)
  #----------------------------------------------------------------------------
  invisible(final.corr)
}
#==============================================================================
# Metric x Metric Correlation
#==============================================================================
corr_metric_metric <- function(all.df, scenario) {
  metrics.col <- c("ALTERATION_MH21", "ALTERATION_DH17",
                   "ALTERATION_HIGH_PULSE_COUNT", "ALTERATION_FLASHINESS",
                   "ALTERATION_LOW_PULSE_DURATION", "ALTERATION_3_DAY_MAXIMUM",
                   "ALTERATION_3_DAY_MINIMUM", "ALTERATION_EXTREME_LOW_FLOW_FREQ",
                   "ALTERATION_EXTREME_LOW_FLOW_DURATION")
  metric.names <- c("High flow index, MH21 (days)",
                    "High flow duration, DH17 (days)", "High pulse count (#)",
                    "Flashiness (ratio)", "Low pulse duration (days)",
                    "3-day maximum (cfs/sqmi)", "3-day minimum (cfs/sqmi)",
                    "Extreme low flow frequency", "Extreme low flow duration")
  #------------------------------------------------------------------------------
  metric.corr <- cor(all.df[, metrics.col], method = "spearman") 
  metric.corr[upper.tri(metric.corr, diag = FALSE)] <- NA
  metric.corr <- data.frame(round(metric.corr, 2))
  metric.corr$WC1 <- row.names(metric.corr)
  metric.corr <- metric.corr[, c(ncol(metric.corr), 1:(ncol(metric.corr) - 1))]
  #------------------------------------------------------------------------------
  final.corr <- metric.corr
  names(final.corr) <- c("", metric.names)
  final.corr[[1]] <- metric.names
  final.corr[is.na(final.corr)] <- ""
  #------------------------------------------------------------------------------
  main.dir <- "output"
  todays.date <- Sys.Date()
  scenario.folder <- ifelse(scenario %in% "cur", "baseline_current",
                            ifelse(scenario %in% "imp", "baseline_impervious", "ERROR"))
  main.path <- file.path(main.dir, todays.date, "Corr", "Metric_Metric", scenario.folder)
  if (!dir.exists(main.path)){
    dir.create(main.path, recursive = TRUE)
  }
  #----------------------------------------------------------------------------
  file.name <- paste0(scenario, "_metric_metric_corr_", todays.date, ".csv")
  output.path <- file.path(main.path, file.name)
  write.csv(final.corr, output.path, row.names =  FALSE)
  #----------------------------------------------------------------------------
  invisible(final.corr)
}
#==============================================================================
# Metric x ENV Correlation
#==============================================================================
corr_metric_env <- function(all.df, scenario) {
  env.col <- c("AREA", "KARST", "PRECIP", "KFACT", "AVGKW", "FCODE", 
               "SLOPE", "SOIL")
  env.names <- c("Area", "Karst", "Precip", "Soil Erodibility Factor", 
                 "Mean Soil Erodibility", "Physiographic Province",
                 "Average Watershed Slope", "Soil Group")
  metrics.col <- c("ALTERATION_MH21", "ALTERATION_DH17",
                   "ALTERATION_HIGH_PULSE_COUNT", "ALTERATION_FLASHINESS",
                   "ALTERATION_LOW_PULSE_DURATION", "ALTERATION_3_DAY_MAXIMUM",
                   "ALTERATION_3_DAY_MINIMUM", "ALTERATION_EXTREME_LOW_FLOW_FREQ",
                   "ALTERATION_EXTREME_LOW_FLOW_DURATION")
  metric.names <- c("High flow index, MH21 (days)",
                    "High flow duration, DH17 (days)", "High pulse count (#)",
                    "Flashiness (ratio)", "Low pulse duration (days)",
                    "3-day maximum (cfs/sqmi)", "3-day minimum (cfs/sqmi)",
                    "Extreme low flow frequency", "Extreme low flow duration")
  #------------------------------------------------------------------------------
  metric.env.corr <- cor(all.df[, metrics.col], all.df[, env.col], method = "spearman") 
  metric.env.corr <- data.frame(round(metric.env.corr, 2))
  metric.env.corr$WC1 <- row.names(metric.env.corr)
  metric.env.corr <- metric.env.corr[, c(ncol(metric.env.corr),
                                         1:(ncol(metric.env.corr) - 1))]
  #------------------------------------------------------------------------------
  final.corr <- metric.env.corr
  names(final.corr) <- c("", env.names)
  final.corr[[1]] <- metric.names
  final.corr[is.na(final.corr)] <- ""
  #------------------------------------------------------------------------------
  main.dir <- "output"
  todays.date <- Sys.Date()
  scenario.folder <- ifelse(scenario %in% "cur", "baseline_current",
                            ifelse(scenario %in% "imp", "baseline_impervious", "ERROR"))
  main.path <- file.path(main.dir, todays.date, "Corr", "Metric_Env", scenario.folder)
  if (!dir.exists(main.path)){
    dir.create(main.path, recursive = TRUE)
  }
  #----------------------------------------------------------------------------
  file.name <- paste0(scenario, "_metric_env_corr_", todays.date, ".csv")
  output.path <- file.path(main.path, file.name)
  write.csv(final.corr, output.path, row.names =  FALSE)
  #----------------------------------------------------------------------------
  invisible(final.corr)
}

#==============================================================================
# Metric x Impervious Cover Correlation
#==============================================================================
corr_metric_imp <- function(all.df, scenario) {
  metrics.col <- c("ALTERATION_MH21", "ALTERATION_DH17",
                   "ALTERATION_HIGH_PULSE_COUNT", "ALTERATION_FLASHINESS",
                   "ALTERATION_LOW_PULSE_DURATION", "ALTERATION_3_DAY_MAXIMUM",
                   "ALTERATION_3_DAY_MINIMUM", "ALTERATION_EXTREME_LOW_FLOW_FREQ",
                   "ALTERATION_EXTREME_LOW_FLOW_DURATION")
  metric.names <- c("High flow index, MH21 (days)",
                    "High flow duration, DH17 (days)", "High pulse count (#)",
                    "Flashiness (ratio)", "Low pulse duration (days)",
                    "3-day maximum (cfs/sqmi)", "3-day minimum (cfs/sqmi)",
                    "Extreme low flow frequency", "Extreme low flow duration")
  #------------------------------------------------------------------------------
  metric.imp.corr <- cor(all.df[, metrics.col], all.df$IMPERV,
                         method = "spearman") 
  metric.imp.corr <- data.frame(round(metric.imp.corr, 2))
  metric.imp.corr$WC1 <- row.names(metric.imp.corr)
  metric.imp.corr <- metric.imp.corr[, c(ncol(metric.imp.corr),
                                         1:(ncol(metric.imp.corr) - 1))]
  #------------------------------------------------------------------------------
  final.corr <- metric.imp.corr
  names(final.corr) <- c("", "Impervious Cover (%)")
  final.corr[[1]] <- metric.names
  final.corr[is.na(final.corr)] <- ""
  #------------------------------------------------------------------------------
  main.dir <- "output"
  todays.date <- Sys.Date()
  scenario.folder <- ifelse(scenario %in% "cur", "baseline_current",
                            ifelse(scenario %in% "imp", "baseline_impervious", "ERROR"))
  main.path <- file.path(main.dir, todays.date, "Corr", "Metric_Impervious", scenario.folder)
  if (!dir.exists(main.path)){
    dir.create(main.path, recursive = TRUE)
  }
  #----------------------------------------------------------------------------
  file.name <- paste0(scenario, "_metric_impervious_corr_", todays.date, ".csv")
  output.path <- file.path(main.path, file.name)
  write.csv(final.corr, output.path, row.names =  FALSE)
  #----------------------------------------------------------------------------
  invisible(final.corr)
}

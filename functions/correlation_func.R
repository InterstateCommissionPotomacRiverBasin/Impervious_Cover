
spear_corr <- function(all.df, scenario, env.keep = NULL, metric.keep = NULL, type) {
  if (!is.null(env.keep)) {
    old.env <- c("AREA", "KARST", "PRECIP", "KFACT", "AVGKW", "FCODE", 
                 "SLOPE", "SOIL", "ECOREGION_C", "ECOREGION4", "ECOREGION3")
    new.env <- c("Area", "Karst", "Precip", "Soil Erodibility Factor", 
                 "Mean Soil Erodibility", "Physiographic Province",
                 "Average Watershed Slope", "Soil Group", "Ecoregion",
                 "Ecoregion4", "Ecoregion3")
    env.ref <- data.frame(OLD = old.env,
                          NEW = new.env,
                          stringsAsFactors = FALSE)
    env.ref <- env.ref %>% 
      filter(OLD %in% env.keep)
    env.col <- env.ref$OLD
    env.names <- env.ref$NEW
    all.df[, env.col] <- sapply(all.df[, env.col], as.numeric)
  }
  #------------------------------------------------------------------------------
  if (!is.null(metric.keep)) {
    old.alt <- c("ALTERATION_MH21", "ALTERATION_DH17",
                 "ALTERATION_HIGH_PULSE_COUNT", "ALTERATION_FLASHINESS",
                 "ALTERATION_LOW_PULSE_DURATION", "ALTERATION_3_DAY_MAXIMUM",
                 "ALTERATION_3_DAY_MINIMUM", "ALTERATION_EXTREME_LOW_FLOW_FREQ",
                 "ALTERATION_EXTREME_LOW_FLOW_DURATION")
    new.alt <- c("High flow index, MH21 (days)",
                 "High flow duration, DH17 (days)", "High pulse count (#)",
                 "Flashiness (ratio)", "Low pulse duration (days)",
                 "3-day maximum (cfs/sqmi)", "3-day minimum (cfs/sqmi)",
                 "Extreme low flow frequency", "Extreme low flow duration")
    metrics.ref <- data.frame(OLD = old.alt,
                              NEW = new.alt,
                              stringsAsFactors = FALSE)
    metrics.ref <- metrics.ref %>% 
      filter(OLD %in% metric.keep)
    metric.col <- metrics.ref$OLD
    metric.names <- metrics.ref$NEW
  }
  #------------------------------------------------------------------------------
  if (type %in% "env_env"){
    corr1 <- all.df[, env.col]
    corr2 <- all.df[, env.col]
    c.names <- env.names
    r.names <- env.names
  } 
  if (type %in% "alt_alt"){
    corr1 <- all.df[, metric.col]
    corr2 <- all.df[, metric.col]
    c.names <- metric.names
    r.names <- metric.names
  } 
  if (type %in% "alt_env"){
    corr1 <- all.df[, metric.col]
    corr2 <- all.df[, env.col]
    c.names <- env.names
    r.names <- metric.names
  } 
  if (type %in% "alt_imp"){
    corr1 <- all.df[, metric.col]
    corr2 <- all.df[, "IMPERV"]
    c.names <- "Impervious Cover (%)"
    r.names <- metric.names
  } 
  #------------------------------------------------------------------------------
  corr.df <- cor(corr1, corr2, method = "spearman") 
  if (type %in% c("env_env", "alt_alt")) {
    corr.df[upper.tri(corr.df, diag = FALSE)] <- NA
  }
  corr.df <- data.frame(round(corr.df, 2))
  corr.df$WC1 <- row.names(corr.df)
  corr.df <- corr.df[, c(ncol(corr.df), 1:(ncol(corr.df) - 1))]
  
  

  #------------------------------------------------------------------------------
  final.corr <- corr.df
  names(final.corr) <- c("", c.names)
  final.corr[[1]] <- r.names
  final.corr[is.na(final.corr)] <- ""
  #------------------------------------------------------------------------------
  main.dir <- "output"
  todays.date <- Sys.Date()
  scenario.folder <- ifelse(scenario %in% "cur", "baseline_current",
                            ifelse(scenario %in% "imp", "baseline_impervious", "ERROR"))
  main.path <- file.path(main.dir, todays.date, "Corr", type, scenario.folder)
  if (!dir.exists(main.path)){
    dir.create(main.path, recursive = TRUE)
  }
  #----------------------------------------------------------------------------
  file.name <- paste0(scenario, paste(type, type, sep = "_"), todays.date, ".csv")
  output.path <- file.path(main.path, file.name)
  write.csv(final.corr, output.path, row.names =  FALSE)
  #----------------------------------------------------------------------------
  invisible(final.corr)
}




#==============================================================================
# Env x Env Correlation
#==============================================================================
corr_env_env <- function(all.df, scenario, env.keep) {
  old.env <- c("AREA", "KARST", "PRECIP", "KFACT", "AVGKW", "PHYSIO", 
               "SLOPE", "SOIL", "ECOREGION_C", "ECOREGION4", "ECOREGION3")
  new.env <- c("Area", "Karst", "Precip", "Soil Erodibility Factor", 
               "Mean Soil Erodibility", "Physiographic Province",
               "Average Watershed Slope", "Soil Group", "Ecoregion",
               "Ecoregion4", "Ecoregion3")
  env.ref <- data.frame(OLD = old.env,
                            NEW = new.env,
                            stringsAsFactors = FALSE)
  env.ref <- env.ref %>% 
    filter(OLD %in% env.keep)
  env.col <- env.ref$OLD
  env.names <- env.ref$NEW
  all.df[, env.col] <- sapply(all.df[, env.col], as.numeric)
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
corr_metric_metric <- function(all.df, scenario, metric.keep) {
  old.names <- c("ALTERATION_MH21", "ALTERATION_DH17",
                   "ALTERATION_HIGH_PULSE_COUNT", "ALTERATION_FLASHINESS",
                   "ALTERATION_LOW_PULSE_DURATION", "ALTERATION_3_DAY_MAXIMUM",
                   "ALTERATION_3_DAY_MINIMUM", "ALTERATION_EXTREME_LOW_FLOW_FREQ",
                   "ALTERATION_EXTREME_LOW_FLOW_DURATION")
  new.names <- c("High flow index, MH21 (days)",
                    "High flow duration, DH17 (days)", "High pulse count (#)",
                    "Flashiness (ratio)", "Low pulse duration (days)",
                    "3-day maximum (cfs/sqmi)", "3-day minimum (cfs/sqmi)",
                    "Extreme low flow frequency", "Extreme low flow duration")
  metrics.ref <- data.frame(OLD = old.names,
                            NEW = new.names,
                            stringsAsFactors = FALSE)
  metrics.ref <- metrics.ref %>% 
    filter(OLD %in% metric.keep)
  metric.col <- metrics.ref$OLD
  metric.names <- metrics.ref$NEW
  #------------------------------------------------------------------------------
  metric.corr <- cor(all.df[, metric.col], method = "spearman") 
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
corr_metric_env <- function(all.df, scenario, env.keep, metric.keep) {
  old.env <- c("AREA", "KARST", "PRECIP", "KFACT", "AVGKW", "FCODE", 
               "SLOPE", "SOIL")
  new.env <- c("Area", "Karst", "Precip", "Soil Erodibility Factor", 
               "Mean Soil Erodibility", "Physiographic Province",
               "Average Watershed Slope", "Soil Group")
  env.ref <- data.frame(OLD = old.env,
                        NEW = new.env,
                        stringsAsFactors = FALSE)
  env.ref <- env.ref %>% 
    filter(OLD %in% env.keep)
  env.col <- env.ref$OLD
  env.names <- env.ref$NEW
  #------------------------------------------------------------------------------
  old.names <- c("ALTERATION_MH21", "ALTERATION_DH17",
                 "ALTERATION_HIGH_PULSE_COUNT", "ALTERATION_FLASHINESS",
                 "ALTERATION_LOW_PULSE_DURATION", "ALTERATION_3_DAY_MAXIMUM",
                 "ALTERATION_3_DAY_MINIMUM", "ALTERATION_EXTREME_LOW_FLOW_FREQ",
                 "ALTERATION_EXTREME_LOW_FLOW_DURATION")
  new.names <- c("High flow index, MH21 (days)",
                 "High flow duration, DH17 (days)", "High pulse count (#)",
                 "Flashiness (ratio)", "Low pulse duration (days)",
                 "3-day maximum (cfs/sqmi)", "3-day minimum (cfs/sqmi)",
                 "Extreme low flow frequency", "Extreme low flow duration")
  metrics.ref <- data.frame(OLD = old.names,
                            NEW = new.names,
                            stringsAsFactors = FALSE)
  metrics.ref <- metrics.ref %>% 
    filter(OLD %in% metric.keep)
  metric.col <- metrics.ref$OLD
  metric.names <- metrics.ref$NEW
  #------------------------------------------------------------------------------
  metric.env.corr <- cor(all.df[, metric.col], all.df[, env.col], method = "spearman") 
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
corr_metric_imp <- function(all.df, scenario, metric.keep) {
  old.names <- c("ALTERATION_MH21", "ALTERATION_DH17",
                 "ALTERATION_HIGH_PULSE_COUNT", "ALTERATION_FLASHINESS",
                 "ALTERATION_LOW_PULSE_DURATION", "ALTERATION_3_DAY_MAXIMUM",
                 "ALTERATION_3_DAY_MINIMUM", "ALTERATION_EXTREME_LOW_FLOW_FREQ",
                 "ALTERATION_EXTREME_LOW_FLOW_DURATION")
  new.names <- c("High flow index, MH21 (days)",
                 "High flow duration, DH17 (days)", "High pulse count (#)",
                 "Flashiness (ratio)", "Low pulse duration (days)",
                 "3-day maximum (cfs/sqmi)", "3-day minimum (cfs/sqmi)",
                 "Extreme low flow frequency", "Extreme low flow duration")
  metrics.ref <- data.frame(OLD = old.names,
                            NEW = new.names,
                            stringsAsFactors = FALSE)
  metrics.ref <- metrics.ref %>% 
    filter(OLD %in% metric.keep)
  metric.col <- metrics.ref$OLD
  metric.names <- metrics.ref$NEW
  #------------------------------------------------------------------------------
  metric.imp.corr <- cor(all.df[, metric.col], all.df$IMPERV,
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

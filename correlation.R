#==============================================================================
# Author: Zachary M. Smith
# Created: 3/13/2017
# Updated: 3/13/2017
# Maintained: Zachary M. Smith
# Purpose: 
# Output: 
#==============================================================================
# Set working directory
setwd("//Pike/data/Projects/ImperviousCover_802/Phase2")
#==============================================================================
# Source functions from impervious_functions.R
source("C:/Users/zsmith/Desktop/Impervious_Cover/Impervious_Cover/impervious_functions.R")
#==============================================================================
# Load package for importing individual excel sheets.
library(readxl)
# Import the master watershed table created by Jim Palmer (3-10-2017).
mwt <- read_excel("Master_Watershed_table_031017.xlsx", sheet = "Sheet1")
# The metrics should be represented as percentages (mult by 100).
mwt[, 13:ncol(mwt)] <- mwt[, 13:ncol(mwt)] * 100
mwt <- clean_up(mwt)
mwt$PHYSIO_PROV <- as.factor(mwt$PHYSIO_PROV)
mwt$SOIL_GROUP <- as.factor(mwt$SOIL_GROUP)
names(mwt)[names(mwt) %in% "AVG_WATERSHED_SLOPE_DEG"] <- "SLOPE"
names(mwt)[names(mwt) %in% "AREA_SQMI"] <- "AREA"
names(mwt)[names(mwt) %in% "PRECIP_IN"] <- "PRECIP"
names(mwt)[names(mwt) %in% "PCT_KARST"] <- "KARST"
names(mwt)[names(mwt) %in% "PHYSIO_PROV"] <- "PHYSIO"
#==============================================================================
# Env x Env Correlation
#==============================================================================
env.col <- c("AREA", "KARST", "PRECIP", "KFACT", "AVGKW", "FCODE", 
                      "SLOPE", "SOIL_GROUP")
mwt$SOIL_GROUP <- mwt$SOIL_GROUP_NUMB
mwt <- mwt[, !names(mwt) %in% "SOIL_GROUP_NUMB"]
#mwt$PHYSIO <- as.numeric(mwt$PHYSIO )
env.corr <- cor(mwt[, env.col], method = "spearman") 
env.corr[upper.tri(env.corr, diag = FALSE)] <- NA
env.corr <- data.frame(round(env.corr, 2))
env.corr$WC1 <- row.names(env.corr)
env.corr <- env.corr[, c(ncol(env.corr), 1:(ncol(env.corr) - 1))]
#------------------------------------------------------------------------------
main.dir <- "C:/Users/zsmith/Desktop/Impervious_Cover/Output/Corr/Env_Env"
todays.date <- Sys.Date()
if (!dir.exists(file.path(main.dir, todays.date ))){
  dir.create(file.path(main.dir, todays.date ))
} 
setwd(paste(main.dir, todays.date, sep = "/"))
#------------------------------------------------------------------------------
env.names <- c("Area", "Karst", "Precip", "Soil Erodibility Factor", 
                  "Mean Soil Erodibility", "Physiographic Province",
                  "Average Watershed Slope", "Soil Group")
#------------------------------------------------------------------------------
final.corr <- env.corr
names(final.corr) <- c("", env.names)
final.corr[[1]] <- env.names
final.corr[is.na(final.corr)] <- ""
#------------------------------------------------------------------------------
write.csv(final.corr, "env_env_corr.csv", row.names =  FALSE)
#==============================================================================
# Metric x Metric Correlation
#==============================================================================
metrics.col <- c("ALTERATION_MH21", "ALTERATION_DH17",
                    "ALTERATION_HIGH_PULSE_COUNT", "ALTERATION_FLASHINESS",
                    "ALTERATION_LOW_PULSE_DURATION", "ALTERATION_3_DAY_MAXIMUM",
                    "ALTERATION_3_DAY_MINIMUM", "ALTERATION_EXTREME_LOW_FLOW_FREQ",
                    "ALTERATION_EXTREME_LOW_FLOW_DURATION")
metric.corr <- cor(mwt[, metrics.col], method = "spearman") 
metric.corr[upper.tri(metric.corr, diag = FALSE)] <- NA
metric.corr <- data.frame(round(metric.corr, 2))
metric.corr$WC1 <- row.names(metric.corr)
metric.corr <- metric.corr[, c(ncol(metric.corr), 1:(ncol(metric.corr) - 1))]
#------------------------------------------------------------------------------
main.dir <- "C:/Users/zsmith/Desktop/Impervious_Cover/Output/Corr/Metric_Metric"
todays.date <- Sys.Date()
if (!dir.exists(file.path(main.dir, todays.date ))){
  dir.create(file.path(main.dir, todays.date ))
} 
setwd(paste(main.dir, todays.date, sep = "/"))
#------------------------------------------------------------------------------
metric.names <- c("High flow index, MH21 (days)",
                  "High flow duration, DH17 (days)", "High pulse count (#)",
                  "Flashiness (ratio)", "Low pulse duration (days)",
                  "3-day maximum (cfs/sqmi)", "3-day minimum (cfs/sqmi)",
                  "Extreme low flow frequency", "Extreme low flow duration")
#------------------------------------------------------------------------------
final.corr <- metric.corr
names(final.corr) <- c("", metric.names)
final.corr[[1]] <- metric.names
final.corr[is.na(final.corr)] <- ""
#------------------------------------------------------------------------------
write.csv(final.corr, "metric_metric_corr.csv", row.names =  FALSE)
#==============================================================================
# Metric x ENV Correlation
#==============================================================================
metric.env.corr <- cor(mwt[, metrics.col], mwt[, env.col], method = "spearman") 
metric.env.corr <- data.frame(round(metric.env.corr, 2))
metric.env.corr$WC1 <- row.names(metric.env.corr)
metric.env.corr <- metric.env.corr[, c(ncol(metric.env.corr), 1:(ncol(metric.env.corr) - 1))]
#------------------------------------------------------------------------------
main.dir <- "C:/Users/zsmith/Desktop/Impervious_Cover/Output/Corr/Metric_ENV"
todays.date <- Sys.Date()
if (!dir.exists(file.path(main.dir, todays.date ))){
  dir.create(file.path(main.dir, todays.date ))
} 
setwd(paste(main.dir, todays.date, sep = "/"))
#------------------------------------------------------------------------------
final.corr <- metric.env.corr
names(final.corr) <- c("", env.names)
final.corr[[1]] <- metric.names
final.corr[is.na(final.corr)] <- ""
#------------------------------------------------------------------------------
write.csv(final.corr, "metric_env_corr.csv", row.names =  FALSE)
#==============================================================================
# Metric x Impervious Cover Correlation
#==============================================================================
metric.imp.corr <- cor(mwt[, metrics.col], mwt$IMPERVIOUS_COVER_PRCENT,
                       method = "spearman") 
metric.imp.corr <- data.frame(round(metric.imp.corr, 2))
metric.imp.corr$WC1 <- row.names(metric.imp.corr)
metric.imp.corr <- metric.imp.corr[, c(ncol(metric.imp.corr), 1:(ncol(metric.imp.corr) - 1))]
#------------------------------------------------------------------------------
main.dir <- "C:/Users/zsmith/Desktop/Impervious_Cover/Output/Corr/Metric_Impervious"
todays.date <- Sys.Date()
if (!dir.exists(file.path(main.dir, todays.date ))){
  dir.create(file.path(main.dir, todays.date ))
} 
setwd(paste(main.dir, todays.date, sep = "/"))
#------------------------------------------------------------------------------
final.corr <- metric.imp.corr
names(final.corr) <- c("", "Impervious Cover (%)")
final.corr[[1]] <- metric.names
final.corr[is.na(final.corr)] <- ""
#------------------------------------------------------------------------------
write.csv(final.corr, "metric_impervious_corr.csv", row.names =  FALSE)





#==============================================================================
#==============================================================================
watershed.long <- tidyr::gather(env.corr, WC2, VALUE, 1:8)
watershed.long$VALUE[is.na(watershed.long$VALUE)] <- 100
watershed.long$WC1 <- factor(watershed.long$WC1, levels = env.col)
watershed.long$WC2 <- factor(watershed.long$WC2, levels = env.col)
watershed.long <- watershed.long[order(watershed.long$WC1, watershed.long$WC2), ]
#==============================================================================
pos.corr <- watershed.long[watershed.long$VALUE >= 0.5 & watershed.long$VALUE < 1, ]
neg.corr <- watershed.long[watershed.long$VALUE <= -0.5, ]
#==============================================================================
library(ggplot2)
final.plot <- ggplot(watershed.long, aes(WC1, WC2)) +
  theme(plot.title = element_text(hjust = 0.5, size = 12),
        plot.subtitle = element_text(hjust = 0.5, size = 11),
        text = element_text(size = 12),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10),
        #legend.position = "top",
        legend.title = element_blank(),
        legend.position = "none",

        #legend.background = element_rect(size = 0.5,
        #                                 linetype = "solid", 
        #                                 colour = "black"),
        #axis.line = element_line(colour = "black"),
        axis.line = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        #panel.border = element_blank(),
        #panel.border = element_rect(colour = "black", fill = NA, size = 5),
        panel.background = element_blank(),
        plot.margin = unit(c(5, 0, 5, 0), "mm")) +
  #scale_fill_gradient2(midpoint = 0, limits = c(-1, .9), low = "#D55E00", mid = "#009E73", high = "#D55E00", na.value = "white") +
  geom_tile(fill = "white", color = "black") +
  #geom_tile(data = pos.corr, aes(fill = "blue"), color = "black") +
  geom_tile(data = pos.corr, aes(fill = "#D55E00"), color = "black") +
  #geom_tile(data = neg.corr, aes(fill = "blue"),  color = "black") +
  geom_tile(data = neg.corr, aes(fill = "#009E73"),  color = "black") +
  geom_text( aes(label = ifelse(is.na(as.character(VALUE)) | VALUE %in% "NA" | VALUE ==100, "", VALUE)),
            size = 4)
final.plot

#999999

#file.name <- paste0("spearman_corr_", Sys.Date(), ".png")
#setwd("C:/Users/zsmith/Desktop/Impervious_Cover/Report/Figures")
#ggsave(file.name, final.plot, dpi = 300)


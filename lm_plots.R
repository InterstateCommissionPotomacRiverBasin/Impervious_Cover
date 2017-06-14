#==============================================================================
# Author: Zachary M. Smith
# Created: 05/15/2017
# Updated: 05/15/2017
# Maintained: Zachary M. Smith
# Purpose: 
# Output: 
#==============================================================================
source("C:/Users/zsmith/Desktop/Impervious_Cover/Impervious_Cover/prep_metrics.R")
#==============================================================================
library(ggplot2)
library(gridExtra)
#==============================================================================
all.df <- mwt
#==============================================================================
# set the seed for reporduciple results. Randomly generated with sample(100000, 1).
set.seed(91177)
all.df$IMP_GROUP <- kmeans(all.df$IMPERVIOUS_COVER_PRCENT, 3, nstart = 25)$cluster 
table(all.df$IMP_GROUP)
by(all.df$IMPERVIOUS_COVER_PRCENT, all.df$IMP_GROUP, range)
#==============================================================================
imp.ranges <- lapply(unique(all.df$IMP_GROUP), function(imp.x) {
  range.x <- round(range(all.df[all.df$IMP_GROUP == imp.x, "IMPERVIOUS_COVER_PRCENT"]), 1)
  range.x <- paste(range.x[1], range.x[2], sep = ", ")
  range.x
})
#==============================================================================
low.str <- paste0("Low (", imp.ranges[1], ")")
mid.str <- paste0("Mid (", imp.ranges[2], ")")
high.str <- paste0("High (", imp.ranges[3], ")")
all.df$IMP_CODE <- ifelse(all.df$IMP_GROUP == 1, mid.str, 
                          ifelse(all.df$IMP_GROUP == 2, low.str,
                                 ifelse(all.df$IMP_GROUP == 3, high.str, "ERROR")))
all.df$IMP_CODE <- factor(all.df$IMP_CODE, levels = c(high.str, mid.str, low.str))
all.df <- all.df[order(all.df$IMP_CODE), ]
#==============================================================================
metrics.df <- all.df
metrics.df$REF <- ifelse(metrics.df$IMPERVIOUS_COVER_PRCENT == 0, "REF", "ALT")
#==============================================================================
plot_lm <- function(metrics.df, metric, watershed.char) {
  x.lab <- paste0(substring(watershed.char, 1, 1),
                  tolower(substring(watershed.char, 2, nchar(watershed.char))))
  
  
  if (metric %in% "ALTERATION_LOW_PULSE_DURATION") {
    y.lab <- "Low Pulse Duration (days)"
  }
  
  if (metric %in% "ALTERATION_FLASHINESS") {
    y.lab <- "Flashiness (ratio)"
  }
  
p <- ggplot(metrics.df, aes_string(watershed.char, metric,
                              by = "IMP_CODE", color = "IMP_CODE", fill = "IMP_CODE")) + 
  #scale_fill_manual("%Impervious") +
  #labs(fill = "%Impervious") +
  geom_point(size = 1.5, pch = 21, alpha = 0.6, color = "black") + 
  #geom_smooth(method = 'lm', se = FALSE) +
  theme_bw() +
  #geom_smooth(method = "loess", color = "#0072B2") + #, span = 0.6
  theme(panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black"), 
        legend.title=element_blank(),
        legend.position = "bottom",
        legend.text = element_text(size = 6.7)) +
  
  xlab(x.lab) +
  ylab(y.lab) +
  scale_y_continuous(expand = c(0, 10)) +
  geom_hline(aes(yintercept = 0), colour = "black", linetype = "dashed")

if (!watershed.char %in% c("FCODE", "SOIL_GROUP_NUMB")) {
  p <- p + geom_smooth(method = 'lm', se = FALSE)
} 
p
}
#==============================================================================
metric.vec <- c("ALTERATION_FLASHINESS", "ALTERATION_LOW_PULSE_DURATION")
char.vec <- c("AREA", "KARST", "PRECIP", "SLOPE", "IMPERVIOUS_COVER_PRCENT", "FCODE",  "SOIL_GROUP_NUMB")
#==============================================================================
#==============================================================================
setwd("C:/Users/zsmith/Desktop/Impervious_Cover/Output/lm")
#==============================================================================
flash <- lapply(char.vec, function(char.x) {
  
  plot_lm(metrics.df, "ALTERATION_FLASHINESS", watershed.char =  char.x)
  
})
png("ALTERATION_FLASHINESS_lm_plots.png",
    width = 6.5, height = 10.5, units = "in", res = 800)
grid.arrange(flash[[1]], flash[[2]], flash[[3]], flash[[4]], flash[[5]])
dev.off()
#==============================================================================

low.pulse <- lapply(char.vec, function(char.x) {
  
  plot_lm(metrics.df, "ALTERATION_LOW_PULSE_DURATION", watershed.char =  char.x)
  
})
png("ALTERATION_LOW_PULSE_DURATION_lm_plots.png",
    width = 6.5, height = 10.5, units = "in", res = 800)
grid.arrange(low.pulse[[1]], low.pulse[[2]], low.pulse[[3]],
             low.pulse[[4]], low.pulse[[5]])
dev.off()
#==============================================================================


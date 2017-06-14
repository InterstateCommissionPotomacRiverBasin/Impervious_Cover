#==============================================================================
# Author: Zachary M. Smith
# Created: 05/04/2017
# Updated: 05/04/2017
# Maintained: Zachary M. Smith
# Purpose: 
# Output: 
#==============================================================================
source("C:/Users/zsmith/Desktop/Impervious_Cover/Impervious_Cover/prep_metrics.R")
#==============================================================================
library(ggplot2)
library(gridExtra)
#==============================================================================
metrics.df <- mwt
#metrics.df <- metrics.df[metrics.df$FCODE %in% c(41, 42), ]
metrics.df <- metrics.df[metrics.df$FCODE %in% c(51, 62, 84), ]
watershed.char <- "SLOPE"
metric <- "ALTERATION_FLASHINESS"
low.lim <- 0
high.lim <- 5
#mwt <- mwt[mwt$IMPERVIOUS_COVER_PRCENT >=2, ]
#==============================================================================
plot_loess <- function(metrics.df, metric, watershed.char,
                       low.lim = 0, high.lim = NULL) {
  x.lab <- paste0(substring(watershed.char, 1, 1),
                  tolower(substring(watershed.char, 2, nchar(watershed.char))))
  
  
  if (metric %in% "ALTERATION_LOW_PULSE_DURATION") {
    y.lab <- "Low Pulse Duration"
  }
  
  if (metric %in% "ALTERATION_FLASHINESS") {
    y.lab <- "Flashiness"
  }
  
  if (is.null(high.lim)) high.lim <- max(metrics.df$IMPERV)
  
  ggplot(metrics.df, aes_string(watershed.char, metric)) + 
    geom_point(aes(fill = metrics.df$IMPERV),
               size = 1.5, pch = 21, alpha = 0.7) + 
    scale_fill_gradientn(colors = c("white", "#D70000"),
                        limits = c(low.lim, high.lim),
                        breaks = c(low.lim, high.lim),
                        values = scales::rescale(c(low.lim, high.lim)),
                        oob = scales::squish,
                        labels = c(low.lim, high.lim)) +
    theme_bw() +
    geom_smooth(method = "loess", color = "#0072B2") + #, span = 0.6
    theme(panel.border = element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          axis.line = element_line(colour = "black"),
          legend.title=element_blank(),
          plot.margin = unit(c(0, 1, 1, 1), "cm")) +
    xlab(x.lab) +
    ylab(y.lab) +
    scale_y_continuous(expand = c(0, 10)) +
    geom_hline(aes(yintercept = 0), colour = "black", linetype = "dashed") + 
    facet_wrap( ~ day, ncol = 2)
  
  
}
#==============================================================================
metric.vec <- c("ALTERATION_FLASHINESS", "ALTERATION_LOW_PULSE_DURATION")
char.vec <- c("AREA", "KARST", "PRECIP", "FCODE", "SLOPE", "SOIL")
#mwt <- data.frame(mwt)
#mwt[, char.vec] <- as.numeric(as.character(mwt[, char.vec]))
#==============================================================================

lapply(metric.vec, function(metric.x) {
  pdf(paste(metric.x, "loess_plots.pdf", sep = "_"))
  lapply(char.vec, function(char.x) {
    plot_loess(mwt, metric.x, watershed.char =  char.x)
  })
  dev.off()
})

#==============================================================================
pdf(paste("ALTERATION_FLASHINESS", "loess_plots.pdf", sep = "_"))
lapply(char.vec, function(char.x) {

 plot_loess(mwt, "ALTERATION_FLASHINESS", watershed.char =  char.x)

})
dev.off()
#==============================================================================
pdf(paste("ALTERATION_LOW_PULSE_DURATION", "loess_plots.pdf", sep = "_"))
lapply(char.vec, function(char.x) {
 plot_loess(mwt, "ALTERATION_LOW_PULSE_DURATION", watershed.char =  char.x)

})
dev.off()




#==============================================================================
setwd("C:/Users/zsmith/Desktop/Impervious_Cover/Output/loess")
#==============================================================================
flash <- lapply(char.vec, function(char.x) {
  
  plot_loess(metrics.df, "ALTERATION_FLASHINESS", watershed.char =  char.x,
             low.lim = 0, high.lim = 5)
  
})
#png("ALTERATION_FLASHINESS_loess_plots.png", width = 6.5, height = 10.5, units = "in", res = 800)
grid.arrange(flash[[1]], flash[[2]], flash[[3]], flash[[4]], flash[[5]], flash[[6]])
#dev.off()
#==============================================================================

low.pulse <- lapply(char.vec, function(char.x) {
  
  plot_loess(metrics.df, "ALTERATION_LOW_PULSE_DURATION", watershed.char =  char.x)
  
})
#png("ALTERATION_LOW_PULSE_DURATION_loess_plots.png", width = 6.5, height = 10.5, units = "in", res = 800)
grid.arrange(low.pulse[[1]], low.pulse[[2]], low.pulse[[3]],
             low.pulse[[4]], low.pulse[[5]], low.pulse[[6]])
#dev.off()
#==============================================================================


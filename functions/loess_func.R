#==============================================================================
# Author: Zachary M. Smith
# Created: 05/04/2017
# Updated: 05/04/2017
# Maintained: Zachary M. Smith
# Purpose: 
# Output: 
#==============================================================================
library(ggplot2)
library(gridExtra)
#==============================================================================
plot_loess <- function(metrics.df, metric, watershed.char,
                       low.lim = 0, high.lim = NULL, color = FALSE) {
  x.lab <- paste0(substring(watershed.char, 1, 1),
                  tolower(substring(watershed.char, 2, nchar(watershed.char))))
  
  
  if (metric %in% "ALTERATION_LOW_PULSE_DURATION") {
    y.lab <- "Low Pulse Duration"
    y.min <- -85
    y.max <- 60
  }
  
  if (metric %in% "ALTERATION_FLASHINESS") {
    y.lab <- "Flashiness"
    y.min <- -10
    y.max <- 450
  }
  
  if (metric %in% "ALTERATION_DH17") {
    y.lab <- "DH17"
    y.min <- -85
    y.max <- 10
  }
  if (is.null(high.lim)) high.lim <- max(metrics.df$IMPERV)
  

  my.plot <- ggplot(metrics.df, aes_string(watershed.char, metric)) + 

    theme_bw() +
    
    theme(panel.border = element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          axis.line = element_line(colour = "black"),
          axis.title.x = element_text(size = 8),
          axis.title.y = element_text(size = 8),
          legend.title = element_blank(),
          plot.margin = unit(c(0.25, 0, 0.25, 0), "cm"),
          legend.key.height = unit(0.25, units = "cm")) +
    xlab(x.lab) +
    ylab(y.lab) +
    scale_y_continuous(limits = c(y.min, y.max)) +
    geom_hline(aes(yintercept = 0), colour = "black", linetype = "dashed")
  
  if (color == TRUE) {
    my.plot + 
      geom_point(aes(fill = metrics.df$IMPERV),
                 size = 1.5, pch = 21, alpha = 0.7) + 
      scale_fill_gradientn(colors = c("white", "#D70000"),
                                       limits = c(low.lim, high.lim),
                                       breaks = c(low.lim, high.lim),
                                       values = scales::rescale(c(low.lim, high.lim)),
                                       oob = scales::squish,
                                       labels = c(low.lim, high.lim)) +
      geom_smooth(method = "loess", color = "#0072B2")#, span = 0.6
  } else {
    my.plot + 
      geom_point(aes(fill = "Value"), size = 1.5, pch = 21, alpha = 0.7) +
      scale_fill_manual(values = "white") +
      geom_smooth(method = "loess", color = "#0072B2")
  }
  
  
}
#==============================================================================
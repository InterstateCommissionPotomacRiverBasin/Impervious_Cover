#==============================================================================
# Author: Zachary M. Smith
# Created: 3/13/2017
# Updated: 6/13/2017
# Maintained: Zachary M. Smith
# Purpose: 
# Output: 
#==============================================================================
#==============================================================================
# Random Forest
#==============================================================================
# Load package for random forest assessment.
library(randomForest)
library(ggplot2)
#==============================================================================


alt_loop <- function(train.df, alt.cols, tree.number = 10000) {
  #alt.cols <- names(train.df[, 13:ncol(train.df)])[1]
  alt.loop <- invisible(lapply(alt.cols, function(x){
    sub.train.df <- train.df[, !names(train.df) %in% alt.cols]
    sub.train.df$VAR <- unlist(train.df[, x])
    sub.train.df$VAR_NAME <- names(train.df[, x])
    fit <- randomForest(VAR ~ AREA + 
                          KARST + 
                          PRECIP + 
                          #KFACT + 
                          #AVGKW + 
                          #FCODE + 
                          SLOPE + 
                          SOIL +
                          PHYSIO,
                        data = sub.train.df,
                        importance = TRUE,
                        maxnodes = 4,
                        ntrees = tree.number)
    invisible(fit)
  }))
  #----------------------------------------------------------------------------
  names(alt.loop) <- alt.cols
  #----------------------------------------------------------------------------
  invisible(alt.loop)
}
#==============================================================================
plot_random_forest <- function(train.df, scenario, alt.cols, tree.number) {
  alt.loop <- alt_loop(train.df, alt.cols, tree.number)
  #----------------------------------------------------------------------------
  #x <- names(alt.loop)[9]
  plot.list <- lapply(names(alt.loop), function(x){
    my.df <- alt.loop[[x]]
    rf.important <- data.frame(my.df$importance)
    rf.important$ENV <- row.names(rf.important)
    #--------------------------------------------------------------------------
    mse.levels <- rf.important$ENV[order(rf.important$X.IncMSE,
                                          decreasing = FALSE)]
    node.levels <- rf.important$ENV[order(rf.important$IncNodePurity,
                                          decreasing = FALSE)]
    rf.important <- rf.important %>% 
      mutate(MSE_ENV = factor(rf.important$ENV, levels = mse.levels),
             NODE_ENV = factor(rf.important$ENV, levels = node.levels))
    #--------------------------------------------------------------------------
    title.x <- gsub("ALTERATION_", "", x)
    mse.plot <- ggplot(rf.important, aes(x = X.IncMSE, y = MSE_ENV)) +
      geom_point(size = 2) +
      theme(axis.title.y = element_blank(),
            plot.title = element_text(hjust = 0.5, size = 12),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            panel.background = element_blank(),
            panel.border = element_rect(colour = "black", fill = NA, size = 1)) +
      #axis.line = element_line(colour = "black")) +
      labs(x = "Percent Increase of MSE",
           title = title.x
      )
    #--------------------------------------------------------------------------
    
    node.plot <- ggplot(rf.important, aes(x = IncNodePurity, y = NODE_ENV)) +
      geom_point(size = 2) +
      theme(axis.title.y = element_blank(),
            plot.title = element_text(hjust = 0.5, size = 12),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            panel.background = element_blank(),
            panel.border = element_rect(colour = "black", fill = NA, size = 1)) +
      #axis.line = element_line(colour = "black")) +
      labs(x = "Increase in Node Purity",
           title = title.x
      )
    #--------------------------------------------------------------------------
    final.plot <- cowplot::plot_grid(mse.plot, node.plot, ncol = 2)
    #--------------------------------------------------------------------------
    main.dir <- "C:/Users/zsmith/Desktop/Impervious_Cover/Output/Rforest"
    todays.date <- Sys.Date()
    if (!dir.exists(file.path(main.dir, todays.date ))){
      dir.create(file.path(main.dir, todays.date ))
    }
    scenario.dir <- paste(main.dir, todays.date, scenario, sep = "/")
    if (!dir.exists(file.path(scenario.dir))){
      dir.create(file.path(scenario.dir))
    }
    #setwd(scenario.dir)
    #----------------------------------------------------------------------------
    prefix <- paste0(scenario, as.character(tree.number))
    file.name <- paste0(paste(prefix, x, Sys.Date(), sep = "_"), ".png")
    file.path <- paste(scenario.dir, file.name, sep = "/")
    #----------------------------------------------------------------------------
    cowplot::ggsave(file.name, final.plot, width = 5, height = 4,
                    units = "in")
    cowplot::ggdraw() +
      cowplot::draw_plot(mse.plot, 0, 0, 0.5, 1) +
      cowplot::draw_plot(node.plot, 0.5, 0, 0.5, 1)
  })
  for (i in 1:length(plot.list)) {
    print(plot.list[[i]])
  }

}
#==============================================================================

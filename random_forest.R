#==============================================================================
# Author: Zachary M. Smith
# Created: 3/13/2017
# Updated: 4/04/2017
# Maintained: Zachary M. Smith
# Purpose: 
# Output: 
#==============================================================================
source("C:/Users/zsmith/Desktop/Impervious_Cover/Impervious_Cover/prep_metrics.R")
#==============================================================================
# Create a training data set and testing data set.
#==============================================================================
# set the seed for reporduciple results. Randomly generated with sample(100000, 1).
set.seed(91177)
# Randomly sample 70% of the rows.
samp <- sample(nrow(mwt), 0.7 * nrow(mwt))
# Create the training set based on the 70% of data randomly selected above.
train <- mwt[samp, ]
# Create the test set based on the 30% of data NOT randomly selected above.
test <- mwt[-samp, ]
#==============================================================================
# Random Forest
#==============================================================================
# Load package for random forest assessment.
library(randomForest)
#==============================================================================
# NEED to remove if you want to only use a true training set.
train <- mwt
#==============================================================================
names(train)

#==============================================================================
x <- names(train[, 13:ncol(train)])[1]
alt.loop <- lapply(names(train[, 13:ncol(train)]), function(x){
  sub.train <- train[, 1:12]
  sub.train$VAR <- unlist(train[, x])
  sub.train$VAR_NAME <- names(train[, x])
  fit <- randomForest(VAR ~ AREA + 
                        KARST + 
                        PRECIP + 
                        #KFACT + 
                        #AVGKW + 
                        #FCODE + 
                        SLOPE + 
                        SOIL_GROUP_NUMB +
                        PHYSIO,
                      data = sub.train,
                      importance = TRUE, 
                      ntrees = 10000)
  return(fit)
})
names(alt.loop) <- names(train[, 13:ncol(train)])

#==============================================================================
test$pred.mh21 <- predict(alt.loop[[1]], test, OOB = TRUE, type = "class")
summary(abs(test$pred.mh21 - test$ALTERATION_MH21))
#==============================================================================
main.dir <- "C:/Users/zsmith/Desktop/Impervious_Cover/Output/Rforest"
todays.date <- Sys.Date()
if (!dir.exists(file.path(main.dir, todays.date ))){
  dir.create(file.path(main.dir, todays.date ))
} 
setwd(paste(main.dir, todays.date, sep = "/"))
#------------------------------------------------------------------------------
library(ggplot2)
x <- names(alt.loop)[9]
lapply(names(alt.loop), function(x){
  #test <- alt.loop[[2]]
  my.df <- alt.loop[[x]]
  rf.important <- data.frame(my.df$importance)
  rf.important$ENV <- row.names(rf.important)
  
  mse.important <- rf.important
  mse.important$ENV <- factor(mse.important$ENV, 
                              levels = mse.important$ENV[order(rf.important$X.IncMSE, decreasing = FALSE)])
  node.important <- rf.important
  node.important$ENV <- factor(node.important$ENV,
                               levels = node.important$ENV[order(node.important$IncNodePurity, decreasing = FALSE)])
  
  title.x <- gsub("ALTERATION_", "", x)
  mse.plot <- ggplot(mse.important, aes(x = X.IncMSE, y = ENV)) +
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
         )#,
         #y = "Watershed Characteristics")
  
  node.plot <- ggplot(node.important, aes(x = IncNodePurity, y = ENV)) +
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
         ) #,
         #y = "Watershed Characteristics")
  final.plot <- cowplot::plot_grid(mse.plot, node.plot, ncol = 2)
  file.name <- paste0(paste(x, Sys.Date(), sep = "_"), ".png")
  cowplot::ggsave(file.name, final.plot, width = 5, height = 4,
                  units = "in")
  
  
  cowplot::ggdraw() +
    cowplot::draw_plot(mse.plot, 0, 0, 0.5, 1) +
    cowplot::draw_plot(node.plot, 0.5, 0, 0.5, 1)
})

#==============================================================================
# Author: Zachary M. Smith
# Created: 05/02/2017
# Updated: 05/02/2017
# Maintained: Zachary M. Smith
# Purpose: 
# Output: 
#==============================================================================
source("C:/Users/zsmith/Desktop/Impervious_Cover/Impervious_Cover/prep_metrics.R")
#==============================================================================
# Create a training data set and testing data set.
#==============================================================================
# set the seed for reporduciple results. Randomly generated with sample(100000, 1).
#set.seed(91177)
# Randomly sample 70% of the rows.
samp <- sample(nrow(mwt), 0.7 * nrow(mwt))
# Create the training set based on the 70% of data randomly selected above.
train <- mwt[samp, ]
# Create the test set based on the 30% of data NOT randomly selected above.
test <- mwt[-samp, ]
#==============================================================================
# Rpart
#==============================================================================
# Load package for rpart assessment.
library(rpart)
library(ipred)
#==============================================================================
# NEED to remove if you want to only use a true training set.
all.df <- mwt
all.df <- as.data.frame(all.df)
#==============================================================================
metric.cols <- c("ALTERATION_MH21", "ALTERATION_DH17",
                 "ALTERATION_HIGH_PULSE_COUNT", "ALTERATION_FLASHINESS",
                 "ALTERATION_LOW_PULSE_DURATION", "ALTERATION_3_DAY_MAXIMUM",
                 "ALTERATION_3_DAY_MINIMUM", "ALTERATION_EXTREME_LOW_FLOW_FREQ",
                 "ALTERATION_EXTREME_LOW_FLOW_DURATION")
#==============================================================================
#------------------------------------------------------------------------------
main.dir <- "C:/Users/zsmith/Desktop/Impervious_Cover/Output/Bagging"
todays.date <- Sys.Date()
if (!dir.exists(file.path(main.dir, todays.date ))){
  dir.create(file.path(main.dir, todays.date ))
} 
setwd(paste(main.dir, todays.date, sep = "/"))
#------------------------------------------------------------------------------
#==============================================================================
# Iteratively sample the population and calculate the rpart for each metric.
samp.size = 1000
system.time(
metric.list <- lapply(metric.cols, function(metric.x) {
  print(metric.x)
  gbag <- bagging(all.df[, metric.x] ~ AREA + KARST + PRECIP + FCODE + SLOPE + SOIL_GROUP_NUMB,
                  data = all.df, coob = TRUE, nbagg = samp.size,
                 control = list(maxdepth = 2, minsplit = 20, maxcompete = 3))
  bag.list <- gbag$mtrees
  iter.list <- lapply(1:samp.size, function(i) {
    final.fit <- rpart.subrules.table(bag.list[[i]]$btree)
    final.fit <- as.data.frame(sapply(final.fit, as.character), stringsAsFactors = FALSE)
    final.fit$THRESH <- as.numeric(ifelse(!is.na(final.fit$Less), final.fit$Less, final.fit$Greater))
    final.fit$ITERATION <- i
    final.fit <- final.fit[grepl("L", final.fit$Subrule), c(7, 1, 2, 6)]
    return(final.fit)
  })
  final.metric <- do.call(rbind, iter.list)
})
)
names(metric.list) <- metric.cols
#------------------------------------------------------------------------------
# Summarize the iterative output.
library(plyr)
metric.summary <- lapply(metric.cols, function(metric.x) {
  cdata <- ddply(metric.list[[metric.x]], c("Subrule", "Variable"), summarise,
                 N    = sum(!is.na(THRESH)),
                 #PCT_FREQ = N / length(Subrule) * 100,
                 MEAN = mean(THRESH, na.rm = TRUE),
                 SD   = sd(THRESH, na.rm = TRUE),
                 SE   = SD / sqrt(N)
  )
  node.count <- as.data.frame(table(metric.list[[metric.x]]$Subrule))
  names(node.count) <- c("Subrule", "NODE_FREQ")
  final.df <- merge(cdata, node.count, by = "Subrule")
  final.df$PCT_FREQ <- final.df$N / final.df$NODE_FREQ * 100
  final.df$METRIC <- metric.x
  final.df <- final.df[, c(ncol(final.df), 1:ncol(final.df) - 1)]
  final.df <- final.df[order(final.df$Subrule, -final.df$N), ]
  final.df[, c("MEAN", "SD", "SE", "PCT_FREQ")] <- round(final.df[, c("MEAN", "SD", "SE", "PCT_FREQ")], 2)
  #final.df <- final.df[, !names(final.df) %in% "NODE_FREQ"]
  return(final.df)
})

summary.df <- do.call(rbind, metric.summary)
#------------------------------------------------------------------------------
file.name <- paste0(paste("bagging_summary", Sys.Date(), sep = "_"), ".csv")
write.csv(summary.df, file.name, row.names = FALSE)
#------------------------------------------------------------------------------

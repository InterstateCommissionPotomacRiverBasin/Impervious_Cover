#==============================================================================
# Author: Zachary M. Smith
# Created: 3/13/2017
# Updated: 04/04/2017
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
training <- mwt[samp, ]
# Create the test set based on the 30% of data NOT randomly selected above.
test <- mwt[-samp, ]
#==============================================================================
# Rpart
#==============================================================================
# Load package for rpart assessment.
library(rpart)
library(rpart.utils)
library(rpart.plot)
library(magrittr)
#==============================================================================
# NEED to remove if you want to only use a true training set.
all.df <- mwt
#==============================================================================
names(training)
#------------------------------------------------------------------------------
metric.cols <- c("ALTERATION_MH21", "ALTERATION_DH17",
                 "ALTERATION_HIGH_PULSE_COUNT", "ALTERATION_FLASHINESS",
                 "ALTERATION_LOW_PULSE_DURATION", "ALTERATION_3_DAY_MAXIMUM",
                 "ALTERATION_3_DAY_MINIMUM", "ALTERATION_EXTREME_LOW_FLOW_FREQ",
                 "ALTERATION_EXTREME_LOW_FLOW_DURATION", "IMPERV")
#------------------------------------------------------------------------------
# grow tree 
plot_rpart <- function(x, rpart.subset) {
  
  rpart.list <- lapply(metric.cols, function(metric.x) {
    my.formula <- paste(metric.x, "~ AREA + KARST + PRECIP + FCODE + SLOPE + SOIL_GROUP_NUMB")
    rpart(my.formula, data = x, control = list(maxdepth = 2, minsplit = 20, maxcompete = 3))
  })

  names(rpart.list) <- metric.cols
  #==============================================================================
  main.dir <- paste0("C:/Users/zsmith/Desktop/Impervious_Cover/Output/Rpart/", rpart.subset)
  todays.date <- Sys.Date()
  if (!dir.exists(file.path(main.dir, todays.date ))){
    dir.create(file.path(main.dir, todays.date ))
  } 
  setwd(paste(main.dir, todays.date, sep = "/"))
  #------------------------------------------------------------------------------
  lapply(metric.cols, function(metric.x) {
    file.name <- paste0(metric.x, ".png")
    png(file.name)
    rpart.plot(rpart.list[[metric.x]], main = metric.x, box.palette = 0)
    dev.off()
  })
  #==============================================================================
}
#==============================================================================
# Reproduce similar plots to Phase 1 (page 10)
#==============================================================================
# Plot all of the available data
# Remove coastal plain??????????
plot_rpart(all.df, "All")
#------------------------------------------------------------------------------
# Average watershed slope > 5
slope.5 <- all.df[all.df$SLOPE > 5, ]
plot_rpart(slope.5, "slope_greater_5")
#------------------------------------------------------------------------------
# Average watershed slope > 7.3
slope.7.3 <- all.df[all.df$SLOPE > 7.3, ]
plot_rpart(slope.7.3, "slope_greater_7.3")
#------------------------------------------------------------------------------
# Karst < 61
karst.61 <- all.df[all.df$KARST < 61, ]
plot_rpart(karst.61, "karst_less_61")
#------------------------------------------------------------------------------
# Include only Ridge/Valley and Appalachian platue provinces.
rvap <- all.df[all.df$FCODE %in% c(62, 84), ]
plot_rpart(rvap, "VR_AP")
#------------------------------------------------------------------------------
# Drainage area less than 1,348.
# Not applicable. All drainage areas less than 1,348.
#drain.1348 <- all.df[all.df$AREA < 1348, ]
#plot_rpart(drain.1348, "drainage_less_1348")
#------------------------------------------------------------------------------
# Precipitation < 45.
precip <- all.df[all.df$PRECIP < 45, ]
plot_rpart(precip, "precipitation_less_45")
#------------------------------------------------------------------------------
# slope < 3.255.
slope.3 <- all.df[all.df$SLOPE > 3.255, ]
plot_rpart(slope.3 , "slope_greater_3.255")




#==============================================================================
# Iteratively sample the population and calculate the rpart for each metric.
system.time(
metric.list <- lapply(metric.cols, function(metric.x) {
  iter.list <- lapply(1:1000, function(i) {
    print(paste("Iteration:", i))
    # Randomly sample 70% of the rows.
    samp <- sample(nrow(mwt), 0.7 * nrow(mwt))
    # Create the training set based on the 70% of data randomly selected above.
    training <- mwt[samp, ]
    # Run rpart.
    my.formula <- paste(metric.x, "~ AREA + KARST + PRECIP + FCODE + SLOPE + SOIL_GROUP_NUMB")
    fit <- rpart(my.formula, data = training, control = list(maxdepth = 2, minsplit = 20, maxcompete = 3))
    final.fit <- rpart.subrules.table(fit)
    final.fit <- as.data.frame(sapply(final.fit, as.character), stringsAsFactors = FALSE)
    final.fit$THRESH <- as.numeric(ifelse(!is.na(final.fit$Less), final.fit$Less, final.fit$Greater))
    final.fit$ITERATION <- i
    final.fit <- final.fit[grepl("L", final.fit$Subrule), c(7, 1, 2, 6)]
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
write.csv(summary.df, "jackknife_summary_5_2_17.csv", row.names = FALSE)
#==============================================================================

node.summary <- lapply(c("L1", "L2", "L3"), function(node.x) {
  metric.df <- metric.list$ALTERATION_MH21
  sub.df <- metric.df[metric.df$Subrule %in% node.x, ]
  sub.df$THRESH <- as.numeric(sub.df$THRESH)
  sub.tbl <- data.frame(table(sub.df$Variable))
  names(sub.tbl) <- c("VAR", "FREQ")
  sub.tbl$PCT_FREQ <- sub.tbl$FREQ / nrow(sub.df) * 100
  agg.df <- aggregate(THRESH ~ Variable, data = sub.df, function(x) c(mean = mean(x), sd = sd(x)))
  names(agg.df)[names(agg.df) %in% "THRESH"] <- "MEAN_THRESH"
  final.node <- merge(sub.tbl, agg.df, by.x = "VAR", by.y = "Variable")
  final.node$SPLIT <- ifelse(node.x %in% "L1", "First", 
                             ifelse(node.x %in% "L2", "Second (<)", 
                                    ifelse(node.x %in% "L3", "Second (>=)")))
  final.node[, c(ncol(final.node), 1:ncol(final.node) - 1)]
})

test <- do.call(rbind, node.summary)

test <- metric.list$ALTERATION_MH21






rpart_dig <- function() {
  subrules.df <- rpart.subrules.table(rpart.list$ALTERATION_MH21)
  metric.char <- as.character(subrules.df$Variable[1])
  metric.num <- as.character(subrules.df$Less[1]) %>% as.numeric()
  sub.less <- all.df[all.df[, metric.char] < metric.num, ]
  sub.great <- all.df[all.df[, metric.char] >= metric.num, ]
  plot_rpart(sub.less)
}


x <- sub.less
my.formula <- paste("ALTERATION_MH21", "~ AREA + KARST + PRECIP + FCODE + SLOPE + SOIL_GROUP_NUMB")
my.rpart <- rpart(my.formula, data = x, control = list(maxdepth= 100, minsplit = 20, maxcompete = 3))
rpart.plot(my.rpart, main = metric.x, box.palette = 0)



metric.x <- "ALTERATION_MH21"

# Run rpart.
my.formula <- paste(metric.x, "~ AREA + KARST + PRECIP + FCODE + SLOPE + SOIL_GROUP_NUMB")
fit <- rpart(my.formula, data = mwt, control = list(maxdepth = 100, minsplit = 20, maxcompete = 3))
rpart.plot(fit, extra = 1)
fit$splits

mwt.less <- mwt[mwt$SLOPE <= 10.13, ]
mwt.great <- mwt[mwt$SLOPE > 10.13, ]

fit.less <- rpart(my.formula, data = mwt.less, control = list(maxdepth = 100, minsplit = 20, maxcompete = 3))
rpart.plot(fit.less, extra = 1)

fit.great <- rpart(my.formula, data = mwt.great, control = list(maxdepth = 100, minsplit = 20, maxcompete = 3))
rpart.plot(fit.great, extra = 1)




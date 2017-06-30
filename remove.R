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
validation <- mwt[-samp, ]
#==============================================================================
# Rpart
#==============================================================================
# Load package for rpart assessment.
library(rpart)
library(rpart.utils)
library(rpart.plot)
library(magrittr)
library(caret)
#==============================================================================
tc <- trainControl(method = "repeatedcv", number = 10, repeats = 10)
rpart.grid <- expand.grid(.cp = 0.1)
#==============================================================================
metric.cols <- c("ALTERATION_MH21", "ALTERATION_DH17",
                 "ALTERATION_HIGH_PULSE_COUNT", "ALTERATION_FLASHINESS",
                 "ALTERATION_LOW_PULSE_DURATION", "ALTERATION_3_DAY_MAXIMUM",
                 "ALTERATION_3_DAY_MINIMUM", "ALTERATION_EXTREME_LOW_FLOW_FREQ",
                 "ALTERATION_EXTREME_LOW_FLOW_DURATION")

rpart.list <- lapply(metric.cols, function(metric.x) {
  my.formula <- as.formula(paste(metric.x, "~ AREA + KARST + PRECIP + FCODE + SLOPE + SOIL"))
  train.rpart <- train(my.formula,
                       data = training, method = "rpart", trControl = tc, tuneGrid = rpart.grid,
                       control = list(minsplit = 20, maxcompete = 3))
})
names(rpart.list) <- metric.cols
rpart.plot(rpart.list$ALTERATION_FLASHINESS$finalModel, extra = 1)


plotcp(rpart.list$ALTERATION_FLASHINESS)


rpart.plot(train.rpart$finalModel)

#==============================================================================
metric.x <- "ALTERATION_FLASHINESS"
my.formula <- paste(metric.x, "~ AREA + KARST + PRECIP + FCODE + SLOPE + SOIL")
rpart_model <- rpart(my.formula, data = training, method = "anova", control = list(minsplit = 20, maxcompete = 3,
                                                                                   xval = 1000))
rpart.plot(rpart_model, extra = 1)
printcp(rpart_model)
plotcp(rpart_model)
# get index of CP with lowest xerror
opt <- which.min(rpart_model$cptable[, "xerror"])
#get its value
cp <- rpart_model$cptable[opt, "CP"]
#prune tree
pruned_model <- prune(rpart_model, cp)
#plot tree
plot(pruned_model);text(pruned_model)
#summary(rpart_model)
pfit <- prune(rpart_model, cp=   rpart_model$cptable[which.min(rpart_model$cptable[,"xerror"]),"CP"])
pfit <- prune(rpart_model, cp=   0.1)
rpart.plot(pfit, extra = 1)
rsq.rpart(rpart_model)
rpart_predict <- predict(rpart_model, validation)
mean(rpart_predict == validation$ALTERATION_MH21)

test <- cbind(rpart_predict, validation$ALTERATION_MH21)

#…the moment of reckoning
rpart_test_predict <- predict(rpart_model, validation, type = "vector")
#calculate RMS error
rmsqe <- sqrt(mean((rpart_test_predict - validation$ALTERATION_MH21)^2))
rmsqe
#==============================================================================


train.cart <- predict(rpart.list$ALTERATION_MH21, newdata = training)
ptree <- prune(rpart.list$ALTERATION_MH21, cp = rpart.list$ALTERATION_MH21$cptable[which.min(rpart.list$ALTERATION_MH21$cptable[,"xerror"]),"CP"])
rpart.plot(ptree)
rpart.plot(rpart.list$ALTERATION_MH21)
pred.cart <- predict(rpart.list$ALTERATION_MH21, newdata = validation)
pred.cart <- predict(rpart.list$ALTERATION_MH21, newdata = training)
correct <- pred.cart == validation$ALTERATION_MH21
summary(train.cart)
training$ALTERATION_MH21
printcp(rpart.list$ALTERATION_MH21)
xpred.rpart(rpart.list$ALTERATION_MH21)
validate.rpart(rpart.list$ALTERATION_MH21)
plotcp(rpart.list$ALTERATION_MH21)

table(pred.cart)
rpart_model


test <- training
test2 <- validation
test$FLASH_CLASS <- ifelse(test$SLOPE >= 7.35, 1,
                           ifelse(test$SLOPE < 7.35 & test$SLOPE >= 4.16, 2,
                                  ifelse(test$SLOPE < 4.16 & test$AREA >= 7.8, 3,
                                         ifelse(test$SLOPE < 4.16 & test$AREA < 7.8, 4, 10))))
test$FLASH_CLASS2 <- ifelse(test$SLOPE >= 7.52, 1,
                           ifelse(test$SLOPE < 7.52 & test$SLOPE >= 4.2, 2,
                                  ifelse(test$SLOPE < 4.2 & test$PRECIP < 43.03, 3,
                                         ifelse(test$SLOPE < 4.2 & test$PRECIP >= 43.03, 4, 10))))
test$FLASH_CLASS <- as.factor(test$FLASH_CLASS)
test$FLASH_CLASS2 <- as.factor(test$FLASH_CLASS2)
test$PRED <- predict(rpart.list$ALTERATION_FLASHINESS, newdata = training)
group <- "FLASH_CLASS"
ggplot(test, aes(FLASH_CLASS, ALTERATION_FLASHINESS, color = FLASH_CLASS)) + 
  geom_boxplot(outlier.alpha = 0, color = "black") + 
  geom_jitter(alpha = 0.30) + 
  scale_colour_manual(values = c("green", "yellow", "orange", "red")) +
  theme_bw() +
  theme(panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black"),
        axis.title.x = element_text(size = 8),
        axis.title.y = element_text(size = 8),
        legend.title = element_blank(),
        plot.margin = unit(c(0.25, 0, 0.25, 0), "cm"),
        legend.key.height = unit(0.25, units = "cm")) 

ggplot(test, aes(FLASH_CLASS2, PRED, color = FLASH_CLASS2)) + 
  geom_boxplot(outlier.alpha = 0, color = "black") + 
  geom_jitter(alpha = 0.30) + 
  scale_colour_manual(values = c("green", "yellow", "orange", "red"))



test2$FLASH_CLASS2 <- ifelse(test2$SLOPE >= 7.52, 1,
                            ifelse(test2$SLOPE < 7.52 & test2$SLOPE >= 4.2, 2,
                                   ifelse(test2$SLOPE < 4.2 & test2$PRECIP < 43.03, 3,
                                          ifelse(test2$SLOPE < 4.2 & test2$PRECIP >= 43.03, 4, 10))))
test2$FLASH_CLASS2 <- as.factor(test2$FLASH_CLASS2)
test2$PRED <- predict(rpart.list$ALTERATION_FLASHINESS, newdata = validation)
ggplot(test2, aes(FLASH_CLASS2, PRED, color = FLASH_CLASS2)) + 
  geom_boxplot(outlier.alpha = 0, color = "black") + 
  geom_jitter(alpha = 0.30) + 
  scale_colour_manual(values = c("green", "yellow", "orange", "red"))





ggplot(test, aes(SLOPE, ALTERATION_FLASHINESS)) + geom_point() + geom_smooth()
ggplot(test, aes(PRECIP, ALTERATION_FLASHINESS)) + geom_point() + geom_smooth()
ggplot(test, aes(AREA, ALTERATION_FLASHINESS)) + geom_point() + geom_smooth()

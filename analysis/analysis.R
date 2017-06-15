#==============================================================================
# Author: Zachary M. Smith
# Created: 3/13/2017
# Updated: 6/13/2017
# Maintained: Zachary M. Smith
# Purpose: Quickly execute and summarize all statisitcal analyses associated
#          with the Impervious Cover (Phase 2) project.
#==============================================================================
#==============================================================================
# Prepare the watershed characteristic data.
source("prep/prep_metrics.R")
#==============================================================================
# Create a training data set and testing data set.
#==============================================================================
base.cur <- train_test(base.cur)
base.imp <- train_test(base.imp)
#==============================================================================
# Alteration columns.
alt.cols <- c("ALTERATION_MH21", "ALTERATION_DH17",
              "ALTERATION_HIGH_PULSE_COUNT", "ALTERATION_FLASHINESS",
              "ALTERATION_LOW_PULSE_DURATION", "ALTERATION_3_DAY_MAXIMUM",
              "ALTERATION_3_DAY_MINIMUM", "ALTERATION_EXTREME_LOW_FLOW_FREQ",
              "ALTERATION_EXTREME_LOW_FLOW_DURATION", "IMPERV")
#==============================================================================
# Spearman Correlation
#==============================================================================
# Import correlation functions.
source("functions/correlation_func.R")
#------------------------------------------------------------------------------
# Perform all correlations for the Baseline/Current Scenario.
corr_env_env(base.cur, "cur")
corr_metric_metric(base.cur, "cur")
corr_metric_env(base.cur, "cur")
corr_metric_imp(base.cur, "cur")
#------------------------------------------------------------------------------
# Perform all correlations for the Baseline/Current Scenario.
corr_env_env(base.imp, "imp")
corr_metric_metric(base.imp, "imp")
corr_metric_env(base.imp, "imp")
corr_metric_imp(base.imp, "imp")
#==============================================================================
# Random Forest
#==============================================================================
# Source the random forest functions.
source("functions/random_forest_func.R")
plot_random_forest(base.cur[base.cur$TYPE %in% "train", ], "cur", alt.cols, 100000)
plot_random_forest(base.imp[base.imp$TYPE %in% "train", ], "imp", alt.cols, 100000)
#==============================================================================
# Rpart
#==============================================================================
# Source the rpart functions.
source("functions/rpart_func.R")
plot_rpart_phase1(base.cur[base.cur$TYPE %in% "train", ], "cur", alt.cols)
plot_rpart_phase1(base.imp[base.imp$TYPE %in% "train", ], "imp", alt.cols)

#==============================================================================
cur <- base.cur %>% 
  select(WATERSHED, ALTERATION_MH21, ALTERATION_DH17, ALTERATION_HIGH_PULSE_COUNT,
         ALTERATION_FLASHINESS, ALTERATION_LOW_PULSE_DURATION, 
         ALTERATION_3_DAY_MAXIMUM, ALTERATION_3_DAY_MINIMUM,
         ALTERATION_EXTREME_LOW_FLOW_FREQ, ALTERATION_EXTREME_LOW_FLOW_DURATION) %>% 
  tidyr::gather(ALTERATION, VALUE_CUR, ALTERATION_MH21:ALTERATION_EXTREME_LOW_FLOW_DURATION)
imp <- base.imp %>% 
  select(WATERSHED, ALTERATION_MH21, ALTERATION_DH17, ALTERATION_HIGH_PULSE_COUNT,
         ALTERATION_FLASHINESS, ALTERATION_LOW_PULSE_DURATION, 
         ALTERATION_3_DAY_MAXIMUM, ALTERATION_3_DAY_MINIMUM,
         ALTERATION_EXTREME_LOW_FLOW_FREQ, ALTERATION_EXTREME_LOW_FLOW_DURATION) %>% 
  tidyr::gather(ALTERATION, VALUE_IMP, ALTERATION_MH21:ALTERATION_EXTREME_LOW_FLOW_DURATION)
test <- full_join(cur, imp, by = c("WATERSHED", "ALTERATION"))
test$DIFF <- test$VALUE_CUR - test$VALUE_IMP
test2 <- test %>%
  filter(DIFF == 0) %>% 
  group_by(ALTERATION) %>% 
  mutate(PERCENT = length(ALTERATION) / 445 * 100) %>% 
  select(ALTERATION, PERCENT) %>% 
  distinct()
table(test2$ALTERATION)

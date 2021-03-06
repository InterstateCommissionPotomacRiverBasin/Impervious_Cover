#==============================================================================
# Author: Zachary M. Smith
# Created: 3/13/2017
# Updated: 6/13/2017
# Maintained: Zachary M. Smith
# Purpose:
#==============================================================================
#==============================================================================
library(rpart)
library(rpart.utils)
library(rpart.plot)
library(magrittr)
library(partykit)
library(caret)
# grow tree 
plot_rpart <- function(x, scenario, alt.cols, rpart.subset) {
  tc <- trainControl(method = "repeatedcv", number = 10, repeats = 100)
  rpart.grid <- expand.grid(.cp = 0.01)
  rpart.grid <- expand.grid(maxdepth = 2)
  #----------------------------------------------------------------------------
  rpart.list <- lapply(alt.cols, function(metric.x) {
    #my.formula <- paste(metric.x, "~ AREA + KARST + PRECIP + FCODE + SLOPE + SOIL")
    my.formula <- as.formula(paste(metric.x, "~ AREA + KARST + PRECIP + FCODE + SLOPE + SOIL"))
    #rpart(my.formula, data = x, method = "anova",
    #      control = list(maxdepth = 2, minsplit = 20, maxcompete = 3, xval = 10000))
    train(my.formula,
          data = x, method = "rpart2",
          trControl = tc, tuneGrid = rpart.grid,
          control = list(minsplit = 20, maxcompete = 3))
  })
  names(rpart.list) <- alt.cols
  #============================================================================
  main.dir <- "output"
  todays.date <- Sys.Date()
  scenario.folder <- ifelse(scenario %in% "cur", "baseline_current",
                            ifelse(scenario %in% "imp", "baseline_impervious", "ERROR"))
  main.path <- file.path(main.dir, todays.date, "Rpart", "plots", scenario.folder)
  if (!dir.exists(main.path)){
    dir.create(main.path, recursive = TRUE)
  }
  #----------------------------------------------------------------------------
  metric.x <- alt.cols[[1]]
  lapply(alt.cols, function(metric.x) {
    #party.plot <- partykit::as.party(rpart.list[[metric.x]])
    file.name <- paste0(metric.x, ".png")
    output.path <- file.path(main.path, file.name)
    png(output.path)
    plot(partykit::as.party(rpart.list[[metric.x]]$finalModel))
    #invisible(rpart.plot(rpart.list[[metric.x]], main = metric.x, box.palette = 0))
    dev.off()
    #rpart.plot(rpart.list[[metric.x]], main = metric.x, box.palette = 0)
    plot(partykit::as.party(rpart.list[[metric.x]]$finalModel), main = metric.x)
  })
  #============================================================================
  return(rpart.list)
}
#==============================================================================
# Reproduce plots similar to Phase 1 (page 10)
#==============================================================================
plot_rpart_phase1 <- function(train.df, scenario, alt.cols) {
  # Plot all of the available data
  # Remove coastal plain??????????
  plot_rpart(train.df, scenario, alt.cols, "All")
  #------------------------------------------------------------------------------
  # Average watershed slope > 5
  slope.5 <- train.df[train.df$SLOPE > 5, ]
  plot_rpart(slope.5, scenario, alt.cols, "slope_greater_5")
  #------------------------------------------------------------------------------
  # Average watershed slope > 7.3
  slope.7.3 <- train.df[train.df$SLOPE > 7.3, ]
  plot_rpart(slope.7.3, scenario, alt.cols, "slope_greater_7.3")
  #------------------------------------------------------------------------------
  # Karst < 61
  karst.61 <- train.df[train.df$KARST < 61, ]
  plot_rpart(karst.61, scenario, alt.cols, "karst_less_61")
  #------------------------------------------------------------------------------
  # Include only Ridge/Valley and Appalachian platue provinces.
  rvap <- train.df[train.df$FCODE %in% c(62, 84), ]
  plot_rpart(rvap, scenario, alt.cols, "VR_AP")
  #------------------------------------------------------------------------------
  # Drainage area less than 1,348.
  # Not applicable. All drainage areas less than 1,348.
  #drain.1348 <- train.df[train.df$AREA < 1348, ]
  #plot_rpart(drain.1348, scenario, "drainage_less_1348")
  #------------------------------------------------------------------------------
  # Precipitation < 45.
  precip <- train.df[train.df$PRECIP < 45, ]
  plot_rpart(precip, scenario, alt.cols, "precipitation_less_45")
  #------------------------------------------------------------------------------
  # slope < 3.255.
  slope.3 <- train.df[train.df$SLOPE > 3.255, ]
  plot_rpart(slope.3, scenario, alt.cols, "slope_greater_3.255")
}

#==============================================================================
table_rpart_jackknife <- function(all.df, scenario, alt.cols, tree.number = 1000) {
  # Iteratively sample the population and calculate the rpart for each metric.
  system.time(
    metric.list <- lapply(alt.cols, function(metric.x) {
      iter.list <- lapply(1:tree.number, function(i) {
        print(paste("Iteration:", i))
        # Randomly sample 70% of the rows.
        samp <- sample(nrow(all.df), 0.7 * nrow(all.df))
        # Create the training set based on the 70% of data randomly selected above.
        training <- all.df[samp, ]
        # Run rpart.
        my.formula <- paste(metric.x, "~ AREA + KARST + PRECIP + FCODE + SLOPE + SOIL")
        fit <- rpart(my.formula, data = training, control = list(maxdepth = 2,
                                                                 minsplit = 20,
                                                                 maxcompete = 3))
        final.fit <- rpart.subrules.table(fit)
        final.fit <- as.data.frame(sapply(final.fit, as.character),
                                   stringsAsFactors = FALSE)
        final.fit$THRESH <- as.numeric(ifelse(!is.na(final.fit$Less),
                                              final.fit$Less,
                                              final.fit$Greater))
        final.fit$ITERATION <- i
        final.fit <- final.fit[grepl("L", final.fit$Subrule), c(7, 1, 2, 6)]
      })
      final.metric <- do.call(rbind, iter.list)
    })
  )
  names(metric.list) <- alt.cols
  #------------------------------------------------------------------------------
  # Summarize the iterative output.
  library(plyr)
  metric.summary <- lapply(alt.cols, function(metric.x) {
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
    summary.cols <- c("MEAN", "SD", "SE", "PCT_FREQ")
    final.df[, summary.cols] <- round(final.df[, summary.cols], 2)
    #final.df <- final.df[, !names(final.df) %in% "NODE_FREQ"]
    return(final.df)
  })
  #----------------------------------------------------------------------------
  summary.df <- do.call(rbind, metric.summary)
  #----------------------------------------------------------------------------
  main.dir <- "output"
  todays.date <- Sys.Date()
  scenario.folder <- ifelse(scenario %in% "cur", "baseline_current",
                            ifelse(scenario %in% "imp", "baseline_impervious", "ERROR"))
  main.path <- file.path(main.dir, todays.date, "Rpart", "jackknife", scenario.folder)
  if (!dir.exists(main.path)){
    dir.create(main.path, recursive = TRUE)
  }
  #----------------------------------------------------------------------------
  prefix <- paste0(scenario, tree.number)
  file.name <- paste0(prefix, "_jackknife_", todays.date, ".csv")
  output.path <- file.path(main.path, file.name)
  write.csv(summary.df, output.path, row.names = FALSE)
}

#==============================================================================

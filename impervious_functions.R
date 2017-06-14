library(dplyr)
#==============================================================================
# Data Clean Up Function
#==============================================================================
clean_up <- function(x) {
  # Change all names to uppercase and remove leading and trailing white space.
  names(x) <- trimws(toupper(names(x)))
  # Replace any spaces in column names with "_".
  names(x) <- gsub(" ", "_", names(x))
  names(x) <- gsub("-", "_", names(x))
  names(x) <- gsub("%", "PCT", names(x))
  # Identify columns of class character and factor.
  char.cols <- sapply(x, class) %in% c('character', 'factor')
  # Remove leading and trailing white space from characters and factors.
  x[, char.cols] <- apply(x[, char.cols], 2, trimws)
  return(x)
}

#==============================================================================
# 
#==============================================================================
prep_watershed_char <- function(x) {
  prefix <- paste("Baseline", "Current", "Impervious_",
                  sep = "|")
  rm.cols <- names(x[grepl(prefix, names(x))])
  rm.cols <- c(rm.cols, "HUC12_RefShed", "Extreme_Low_Flow_Freq",
               "Extreme_Low_Flow_Duration", "X__1", "X__2")
  # Remove unnecessary columns.
  x <- x[, !names(x) %in% rm.cols]
  # Remove any rows that contain NAs.
  x <- x[complete.cases(x), ] 
  #----------------------------------------------------------------------------
  # The metrics should be represented as percentages (mult by 100).
  x[, 13:ncol(x)] <- x[, 13:ncol(x)] * 100
  #----------------------------------------------------------------------------
  final.df <- clean_up(x) %>% 
    dplyr::mutate(PHYSIO_PROV = as.factor(PHYSIO_PROV),
           SOIL_GROUP = as.factor(SOIL_GROUP)) %>% 
    dplyr::rename(SLOPE = AVG_WATERSHED_SLOPE_DEG,
           AREA = AREA_SQMI,
           PRECIP = PRECIP_IN,
           KARST = PCT_KARST,
           PHYSIO = PHYSIO_PROV,
           IMPERV = IMPERVIOUS_COVER_PRCENT,
           SOIL = SOIL_GROUP_NUMB)
  #----------------------------------------------------------------------------
  return(final.df)
}
#==============================================================================
# Randomly divide the data into training and test samples.
train_test <- function(x, train.pct = 0.7) {
  # set the seed for reporduciple results. Randomly generated with sample(100000, 1).
  set.seed(91177)
  # Randomly sample 70% of the rows.
  samp <- sample(nrow(x), train.pct * nrow(x))
  # Create the training set based on the 70% of data randomly selected above.
  train <- x[samp, ]
  # Create the test set based on the 30% of data NOT randomly selected above.
  test <- x[-samp, ]
  # Create a "TYPE" column to identify the sample type (i.e., train or test).
  train$TYPE <- "train"
  test$TYPE <- "test"
  # Join the two data frames into a single dataframe.
  final.df <- dplyr::bind_rows(train, test)
  # Re-order the dataframe.
  final.df <- final.df[, c(ncol(final.df), 1:(ncol(final.df) - 1))]
  return(final.df)
}


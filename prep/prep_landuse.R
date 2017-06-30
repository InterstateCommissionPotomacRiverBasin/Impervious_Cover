#==============================================================================
# Author: Zachary M. Smith
# Created: 06/22/2017
# Updated: 06/22/2017
# Maintained: Zachary M. Smith
# Purpose: Import and prepare watershed land use data.
#==============================================================================
# Set working directory
#setwd("//Pike/data/Projects/ImperviousCover_802/Phase2/Watershed_Characteristics")
#------------------------------------------------------------------------------
# Source functions from impervious_functions.R
source("functions/impervious_functions.R")
#------------------------------------------------------------------------------
# Load package for importing individual excel sheets.
library(readxl)
# Load package for data manipulation
library(dplyr)
#------------------------------------------------------------------------------
# J. Palmer created two xlsx files containing watershed Characteristics:
# 1) Baseline/Current Scenario
#    file name = Phase2_Master_Watershed_Characteristics_values_032417.xlsx
#    date = 03-24-17
# 2) Baseline/Impervious Scenario
#    file name = Phase2_Watershed_Characteristics_Imprvous_values_061217.xlsx
#    date = 06-12-17
# These files were merged into one xlsx file by Z. Smith on 06-13-17.
#------------------------------------------------------------------------------
#file.path <- "//Pike/data/Projects/ImperviousCover_802/Phase2/Watershed_Characteristics"
file.path <- "data/Curr-Base_HUCRfshd_LUacs_052517.xlsx"
# Import the Baseline/Current Scenario sheet.
base.df <- read_excel(file.path, sheet = "BaselineScenario")
# Import the Baseline/Impervious Scenario sheet.
curr.df <- read_excel(file.path, sheet = "CurrentScenario")
# Import the Ecoregion sheet.
impr.df <- read_excel(file.path, sheet = "ImperviousScenario")
#------------------------------------------------------------------------------
clean_landuse <- function(x) {
  # Column names to be removed.
  rm.names <- c("X__1", "X__2", "X__3", "X__4",
                "%forest", "%impervious", "%urban")
  # Remove the specified column names and then remove any rows that contain NA.
  final.df <- x %>% 
    select_if(!names(.) %in% rm.names) %>% 
    na.omit()
  # Update the HUC12 reference column name.
  names(final.df)[names(final.df) %in% "HUC12-RefShd"] <- "HUC12_REF"
  # Return the new data frame.
  return(final.df)
}
#------------------------------------------------------------------------------
# Clean each of the landuse data frames.
base.df <- clean_landuse(base.df)
curr.df <- clean_landuse(curr.df)
impr.df <- clean_landuse(impr.df)
#------------------------------------------------------------------------------
imp.vec <- c("rid", "nid", "cid")
x <- base.df
y <- impr.df
plot_resid <- function(x, y) {
  clean.x <- x %>% 
    mutate(total_x = rowSums(.[, c("rid", "nid", "cid")]),
           pct_rid = case_when(rid == 0 | total_x == 0 ~ 0,
                               rid != 0 & total_x != 0 ~ rid / total_x * 100),
           pct_nid = case_when(nid == 0 | total_x == 0 ~ 0,
                               nid != 0 & total_x != 0 ~ nid / total_x * 100),
           pct_cid = case_when(cid == 0 | total_x == 0 ~ 0,
                               cid != 0 & total_x != 0 ~ cid / total_x * 100)) %>% 
    tidyr::gather(Impervious_Type, Percent_x, pct_rid:pct_cid) %>%
    select(UniqueID, HUC12_REF, total_x:Percent_x)
  
  clean.y <- y %>% 
    mutate(total_y = rowSums(.[, c("rid", "nid", "cid")]),
           pct_rid = case_when(rid == 0 | total_y == 0 ~ 0,
                               rid != 0 & total_y != 0 ~ rid / total_y * 100),
           pct_nid = case_when(nid == 0 | total_y == 0 ~ 0,
                               nid != 0 & total_y != 0 ~ nid / total_y * 100),
           pct_cid = case_when(cid == 0 | total_y == 0 ~ 0,
                               cid != 0 & total_y != 0 ~ cid / total_y * 100)) %>% 
    tidyr::gather(Impervious_Type, Percent_y, pct_rid:pct_cid) %>%
    select(UniqueID, HUC12_REF, total_y:Percent_y)
  
  
  clean.df <- full_join(clean.x, clean.y, by = c("UniqueID", "HUC12_REF", "Impervious_Type")) %>% 
    mutate(resid_total = total_x - total_y,
           resid_percent = Percent_x - Percent_y)
  
  k.test <- kruskal.test(resid_percent ~ Impervious_Type , data = clean.df)
  dunn.test::dunn.test(clean.df$resid_percent, clean.df$Impervious_Type)

  
    ggplot(clean.df, aes(Impervious_Type, resid_percent)) +
      geom_boxplot() +
      labs(title = paste("p-value:", round(k.test$p.value, 3),
                         "; test statistic =", round(k.test$statistic, 3))) +
      theme(plot.title = element_text(hjust = 0.5))
}
x <- base.df

plot_imp <- function(x) {
  clean.df <- x %>% 
    mutate(total = rowSums(x[, c("rid", "nid", "cid")]),
           pct_rid = case_when(total == 0 ~ 0,
                               total != 0 ~ rid / total * 100),
           pct_nid = case_when(total == 0 ~ 0,
                               total != 0 ~ nid / total * 100),
           pct_cid = case_when(total == 0 ~ 0,
                               total != 0 ~ cid / total * 100)) %>% 
    tidyr::gather(Impervious_Type, Percent, pct_rid:pct_cid) %>%
    select(UniqueID, HUC12_REF, c(total:Percent)) %>% 
    mutate(Impervious_Type = factor(Impervious_Type))

  k.test <- kruskal.test(clean.df$Percent, clean.df$Impervious_Type, data = clean.df)
  dunn.test::dunn.test(clean.df$Percent, clean.df$Impervious_Type)
  
  
  ggplot(clean.df, aes(Impervious_Type, Percent)) +
    geom_boxplot() +
    labs(title = paste("p-value:", round(k.test$p.value, 3),
                       "; test statistic =", round(k.test$statistic, 3))) +
    theme(plot.title = element_text(hjust = 0.5))
}

plot_imp(base.df)
plot_imp(curr.df)
plot_imp(impr.df)

base.df$nid - impr.df$nid
#------------------------------------------------------------------------------

test <- curr.df
test$total <- rowSums(test[, c("rid", "nid", "cid")])
test$pct_rid <- ifelse(test$total != 0, test$rid / test$total * 100, 0)

test2 <- base.df
test2$total <- rowSums(test2[, c("rid", "nid", "cid")])
test2$pct_rid <- ifelse(test2$total != 0, test2$rid / test2$total * 100, 0)
#------------------------------------------------------------------------------
test$pct_rid - test2$pct_rid


clean.b <- clean.base %>% 
  select(UniqueID, HUC12_REF, rid, nid, cid, total, pct_rid, pct_nid, pct_cid)

clean.c <- clean.curr %>% 
  select(UniqueID, HUC12_REF, rid, nid, cid, total, pct_rid, pct_nid, pct_cid)

test <- full_join(clean.b, clean.c, by = c("UniqueID", "HUC12_REF"))

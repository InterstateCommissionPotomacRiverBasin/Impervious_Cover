#==============================================================================
# Author: Zachary M. Smith
# Created: 3/13/2017
# Updated: 06/13/2017
# Maintained: Zachary M. Smith
# Purpose: Import and prepare watershed characteristic data.
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
file.path <- "data/final_watershed_characteristics_061617.xlsx"
# Import the Baseline/Current Scenario sheet.
base.cur <- read_excel(file.path, sheet = "Base_Current")
# Import the Baseline/Impervious Scenario sheet.
base.imp <- read_excel(file.path, sheet = "Base_Impervious")
# Import the Ecoregion sheet.
ecoregion <- read_excel(file.path, sheet = "Ecoregions")
#------------------------------------------------------------------------------
# Prepare ecoregion data.
ecoregion.df <- ecoregion %>% 
  rename(WATERSHED = Watershed, 
         ECOREGION4 = US_L4CODE) %>% 
  mutate(ECOREGION3 = substr(ECOREGION4, 1, 2),
         #Ecoregion_C; C for custom.
         ECOREGION_C = if_else(ECOREGION4 %in% c("45e", "64b",
                                                 "64c", "64d", "65n"), "PIEDMONT_UP",
                               if_else(ECOREGION4 %in% c("64a"), "PIEDMONT_LOW",
                               if_else(ECOREGION4 %in% c("66a", "66b"), "BLUE",
                                       if_else(ECOREGION4 %in% c("67a", "67b"), "VALLEY",
                                               if_else(ECOREGION4 %in% c("67c", "67d"), "RIDGE",
                                                       if_else(ECOREGION4 %in% c("69a", "69b"), "CA", "ERROR")))))),
         ECOREGION_C = if_else(ECOREGION_C %in% c("PIEDMONT_UP", "PIEDMONT_LOW", "VALLEY"), "VALLEY", ECOREGION_C),
         ECOREGION4 = as.factor(ECOREGION4),
         ECOREGION3 = as.factor(ECOREGION3),
         ECOREGION_C = as.factor(ECOREGION_C))
# Prepare the Baseline/Current Scenario.
base.cur <- prep_watershed_char(base.cur)
base.cur <- right_join(ecoregion.df, base.cur, by = "WATERSHED") %>% 
  mutate(FCODE = as.numeric(as.factor(PHYSIO)))
# Prepare the Baseline/Impervious Scenario.
base.imp <- prep_watershed_char(base.imp)
base.imp <- right_join(ecoregion.df, base.imp, by = "WATERSHED") %>% 
  mutate(FCODE = as.numeric(as.factor(PHYSIO)))
#------------------------------------------------------------------------------
# Remove functions that are no longer necessary.
rm(clean_up, prep_watershed_char, ecoregion.df)

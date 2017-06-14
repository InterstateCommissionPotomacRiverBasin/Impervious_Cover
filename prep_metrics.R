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
source("impervious_functions.R")
#------------------------------------------------------------------------------
# Load package for importing individual excel sheets.
library(readxl)
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
file.path <- "//Pike/data/Projects/ImperviousCover_802/Phase2/Watershed_Characteristics"
# Import the Baseline/Current Scenario sheet.
cur.file <- "final_watershed_characteristics_061317.xlsx"
cur.path <- paste(file.path, cur.file, sep = "/")
base.cur <- read_excel(cur.path, sheet = "Base_Current")
# Import the Baseline/Impervious Scenario sheet.
imp.file <- "final_watershed_characteristics_061317.xlsx"
imp.path <- paste(file.path, imp.file, sep = "/")
base.imp <- read_excel(imp.path, sheet = "Base_Impervious")
#------------------------------------------------------------------------------
# Prepare the Baseline/Current Scenario.
base.cur <- prep_watershed_char(base.cur)
# Prepare the Baseline/Impervious Scenario.
base.imp <- prep_watershed_char(base.imp)
#------------------------------------------------------------------------------
# Remove functions that are no longer necessary.
rm(clean_up, prep_watershed_char)

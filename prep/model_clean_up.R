#==============================================================================
# Author: Zachary M. Smith
# Created: 1/6/17
# Updated: 3/4/17
# Maintained: Zachary M. Smith
# Purpose: The script was written to import and process Edge of Field (EOF)
#          model output.
# Output: A time series of total EOF flow by HUC12.
#==============================================================================
# Set working directory
setwd("C:/Users/zsmith/Desktop/Impervious_Cover/Data/UNIKID_LRS_HUC12RfShds")
huc12 <- read.csv("UNIKID_LRS_HUC12RfShds_010517.csv")
names(huc12)[1] <- "UniqueID"
huc12 <- unique(huc12[, 1:3])
#==============================================================================
remove.cols <- c("LRSeg", "%forest", "%impervious", "%urban")
#==============================================================================
library(readxl)
setwd("//Pike/data/Projects/ImperviousCover_802/Phase2/LandUse_acres")
base.import <- read_excel("Curr-Base_LRS_LUacs_030317.xlsx", sheet = "BaselineScenario")
base.import <- base.import[, !names(base.import) %in% remove.cols]
base.import[is.na(base.import)] <- 0

curr.import <- read_excel("Curr-Base_LRS_LUacs_030317.xlsx", sheet = "CurrentScenario")
curr.import <- curr.import[, !names(curr.import) %in% remove.cols]
curr.import[is.na(curr.import)] <- 0
#==============================================================================
# Merge the base scenario land-use/acre with huc12 info
base.df <- merge(base.import, huc12, by = "UniqueID")
base.df <- tidyr::gather(base.df, LANDUSE, ACRE, 3:33)
base.df <- base.df[, c("UniqueID", "UNIK_ID_landuse_122916_LRS",
                       "HUC12.RefShd",  "LANDUSE", "ACRE")]
names(base.df) <- c("UniqueID", "FILENAME", "HUC12", "LANDUSE", "ACRE")
base.df$LANDUSE <- ifelse(base.df$LANDUSE %in% "for.", "for", base.df$LANDUSE)
#==============================================================================
curr.df <- merge(curr.import, huc12, by = "UniqueID")
curr.df <- tidyr::gather(curr.df, LANDUSE, ACRE, 3:33)
curr.df <- curr.df[, c("UniqueID", "UNIK_ID_landuse_122916_LRS",
                       "HUC12.RefShd",  "LANDUSE", "ACRE")]
names(curr.df) <- c("UniqueID", "FILENAME", "HUC12", "LANDUSE", "ACRE")
curr.df$LANDUSE <- ifelse(curr.df$LANDUSE %in% "for.", "for", curr.df$LANDUSE)
#==============================================================================
# Set working directory
setwd("//Pike/data/Projects/ImperviousCover_802/Phase2/EOF Flows")
# Get a list of files ending in ".day"
dfiles <- list.files(pattern = glob2rx("*.day"))
# Remove the last 8-characters of the file name to extract the UNIK_ID.
#nfiles <- unique(substr(dfiles, 1, nchar(dfiles) - 8))
#=============================================================================
# Remove the last 8-characters of the file name to extract the River-Segment.
# Remove the "_to_" from the file name to easily merge with the huc12 file.
# Create a list of River-Segments, which correspond to the filenames by HUC12.

nfiles <- data.frame(FILE = unique(substr(dfiles, 1, nchar(dfiles) - 8)))
nfiles$FILENAME <- gsub("_to_", "", nfiles$FILE)
huc <- huc12
names(huc) <- c("UniqueID", "FILENAME", "HUC12")
t.merge <- merge(huc, nfiles, by = "FILENAME")

nfiles <- split(t.merge$FILE, t.merge$HUC12, sep = ",")
#=============================================================================
# The nested loop below imports the files in chunks defined by the HUC12.
# There are 30 land-use types for each Unique-ID and River-Segment. Some of
# the land-use types were not applicable for some Unique-IDs and
# River-Segments. These files did not contain any data and were omitted. The 
# remaining files represented flows associated with a distinct land-use for a
# Unique-ID and River-Segment. The baseline and current land-use acreage for 
# each River-Segment was merged with the flow output. The ACRE column was 
#multiplied by the FLOW column in each row. All of the files were merged 
# together to create a single data frame representing a River-Segment.  The 
# FLOW column was then aggregated (Summed) by HUC12, YEAR, MONTH, and DAY 
# columns to create the Total Daily Flow column. This process was performed
# for each HUC12..
#=============================================================================

# Create an open list to store each HUC12 chunk.
#final.list <- list()
# This loop cycles through each HUC12
system.time(
  for (i in seq_along(nfiles)){
    setwd("//Pike/data/Projects/ImperviousCover_802/Phase2/EOF Flows")
    # Find all of the files with this iterations UNIK_ID.
    #sub.files <- list.files(pattern = nfiles[i])
    sub.files <- list.files(pattern = paste(unlist(nfiles[i]), collapse = "|"))
    
    # Create an open list to store each land-use data frame for 
    # the baseline and current HUC12 specified in this iteration.
    base.list <- list()
    curr.list <- list()
    # This loop cycles through each land-use data frame for
    # the HUC12 specified in this iteration.
    for(j in seq_along(sub.files)){
      # Files with less than 10 bytes are excluded because they do not contain
      # any information.
      if (file.size(sub.files[j]) <= 10) {
        print(paste("skip", sub.files[j], sep = " "))
        next
      } 
      # Print file to track progress while running.
      print(sub.files[j])
      
      #read csv file
      temp.df <- read.csv(sub.files[j], stringsAsFactors = FALSE)
      
      # Keep only necessary columns
      temp.df <- temp.df[, names(temp.df) %in% c("YEAR", "MONTH", "DAY", "FLOW")]
      
      # Isolate the file name
      file.name <- as.character(sub.files[j])
      
      # Use the file name to create a new column specifying the River-Segment.
      temp.df$FILENAME <- substr(file.name, 1, nchar(file.name) - 8)
      temp.df$FILENAME <- gsub("_to_", "", temp.df$FILENAME)
      
      # Add the file name to the data frame
      #temp.df$FILE_NAME <- file.name
      # Remove the ".day"
      file.name <- gsub("*.day", "", file.name)
      
      # Divide the file string into 6 list elements by the character "_"
      split.name <- strsplit(file.name, "*_")
      
      # Use split.name to create necessary columns
      #temp.df$SEGMENT_NAME <- paste(split.name[[1]][1], split.name[[1]][3], sep = "_")
      #temp.df$SEGMENT_CODE <- paste(split.name[[1]][4], split.name[[1]][5], sep = "_")
      # Specify the land-use.
      temp.df$LANDUSE <- split.name[[1]][6]
      #==========================================================================
      # Bring in the base
      base.j <- base.df[base.df$FILENAME %in% unique(temp.df$FILENAME), ]
      base.j <- base.j[base.j$HUC12 %in% names(nfiles[i]), ]
      merged.base <- merge(base.j, temp.df, by = c("FILENAME", "LANDUSE"))
      
      # Multiply the FLOW by ACRE to get the land-use flow for that Unique ID.
      merged.base$SUB_FLOW <- merged.base$FLOW * merged.base$ACRE
      
      # Store each land-use data.frame in a list.
      base.list[[j]] <- merged.base
      #==========================================================================
      # Bring in the current
      curr.j <- curr.df[curr.df$FILENAME %in% unique(temp.df$FILENAME), ]
      curr.j <- curr.j[curr.j$HUC12 %in% names(nfiles[i]), ]
      merged.curr <- merge(curr.j, temp.df, by = c("FILENAME", "LANDUSE"))
      
      # Multiply the FLOW by ACRE to get the land-use flow for that Unique ID.
      merged.curr$SUB_FLOW <- merged.curr$FLOW * merged.curr$ACRE
      
      # Store each land-use data.frame in a list.
      curr.list[[j]] <- merged.curr
      
    }# end loop j
    #=============================================================================
    # Merge all of the land-use data frames for the FILENAME specified by iteration i.
    bind.base <- do.call(rbind, base.list)
    # Use data.table in hopes that the aggregation will be faster than the aggregate function.
    bind.base <- data.table::data.table(bind.base)
    # Aggregate (sum) the land-use flow (SUB_FLOW) by HUC12, YEAR, MONTH, and DAY to calculate TOTAL_FLOW.
    agg.base <- bind.base[, list(TOTAL_FLOW = sum(SUB_FLOW)), by = list(HUC12, YEAR, MONTH, DAY)]
    agg.base$SCENARIO <- "Baseline"
    
    #Store the final data frame in a list.
    #final.list[[i]] <- agg.base
    #setwd("C:/Users/zsmith/Desktop/Impervious_Cover/Data/Output/January_2017")
    setwd("//Pike/data/Projects/ImperviousCover_802/Phase2/R_Output/3_04_17")
    write.csv(agg.base, paste(paste("BASE", unique(agg.base$HUC12), format(Sys.Date(), "%m_%d_%Y"), sep = "_"),
                              ".csv", sep = ""), row.names = FALSE)
    #=============================================================================
    # Merge all of the land-use data frames for the FILENAME specified by iteration i.
    bind.curr <- do.call(rbind, curr.list)
    # Use data.table in hopes that the aggregation will be faster than the aggregate function.
    bind.curr <- data.table::data.table(bind.curr)
    # Aggregate (sum) the land-use flow (SUB_FLOW) by HUC12, YEAR, MONTH, and DAY to calculate TOTAL_FLOW.
    agg.curr <- bind.curr[, list(TOTAL_FLOW = sum(SUB_FLOW)), by = list(HUC12, YEAR, MONTH, DAY)]
    agg.curr$SCENARIO <- "Current"
    
    #Store the final data frame in a list.
    #final.list[[i]] <- agg.curr
    #setwd("C:/Users/zsmith/Desktop/Impervious_Cover/Data/Output/January_2017")
    setwd("//Pike/data/Projects/ImperviousCover_802/Phase2/R_Output/3_04_17")
    write.csv(agg.curr, paste(paste("CURR", unique(agg.curr$HUC12), format(Sys.Date(), "%m_%d_%Y"), sep = "_"),
                              ".csv", sep = ""), row.names = FALSE)
    
  }# end loop i
) # End system.time
#test <- do.call(rbind, final.list)


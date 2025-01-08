# Clay Ford
# jcf2d@virginia.edu

# getting and munging Albemarle county homes data

# set year when running script
year <- "2025"

# Albemarle county
# Office of Geographic Data Services
# https://www.albemarle.org/government/information-technology/geographic-information-system-gis-mapping/gis-data

# Real Estate Information
# Under PARCELS - Primary Card Level Data - this file includes data
# such as year built, finished square footage, number of rooms, and condition. 
# https://gisweb.albemarle.org/gisdata/CAMA/GIS_CardLevelData_new_TXT.zip

if(!dir.exists("data"))dir.create("data")
setwd("data/")
link <- "https://gisweb.albemarle.org/gisdata/CAMA/GIS_CardLevelData_new_TXT.zip"
download.file(link, destfile = basename(link))
unzip(basename(link)) # extract file to working directory


# Real Estate Information - Parcel Level Data.  This file contains information
# about the parcel itself such as owner information, deed acreage value, and
# assessed value

# https://gisweb.albemarle.org/gisdata/CAMA/GIS_View_Redacted_ParcelInfo_TXT.zip

link2 <- "https://gisweb.albemarle.org/gisdata/CAMA/GIS_View_Redacted_ParcelInfo_TXT.zip"
download.file(link2, destfile = basename(link2))
unzip(basename(link2)) # extract file to working directory

# Other Parcel Characteristics

# This file contains other parcel information that is managed in our development
# tracking system (e.g. Zoning, School Districts, Jurisdictional Areas, etc.).

link3 <- "https://gisweb.albemarle.org/gisdata/CAMA/CityView_View_OtherParcelCharacteristics_TXT.zip"
download.file(link3, destfile = basename(link3))
unzip(basename(link3)) # extract file to working directory

file.remove("GIS_View_Redacted_ParcelInfo_TXT.zip")
file.remove("GIS_CardLevelData_new_TXT.zip")
file.remove("CityView_View_OtherParcelCharacteristics_TXT.zip")

card_level <- read.csv("GIS_CardLevelData_new.txt", 
                       na.strings = "NULL")
parcel_level <- read.csv("GIS_View_Redacted_ParcelInfo.txt", 
                         na.strings = "NULL")
other_parcel <- read.csv("CityView_View_OtherParcelCharacteristics.txt", 
                         na.strings = c("NULL", "N/A"))

file.remove("CityView_View_OtherParcelCharacteristics.txt")
file.remove("GIS_CardLevelData_new.txt")
file.remove("GIS_View_Redacted_ParcelInfo.txt")

# card_level list of variables to keep
card_vars <- c("TMP", "CardType", "YearBuilt", "YearRemodeled", 
               "UseCode", "Condition", "FinSqFt", "Cooling", 
               "FP_Open", "Bedroom", "FullBath")
card <- card_level[,card_vars]

# parcel_level list of variables to keep
parcel_vars <- c("ParcelID", "LotSize", "TotalValue", "LastSalePrice",
                 "LastSaleDate1", "Cards")
parcel <- parcel_level[,parcel_vars]

# other_parcel list of variables to keep
other_vars <- c("ParcelID", "ESDistrict", "MSDistrict", "HSDistrict",
                "CensusTract")
other <- other_parcel[,other_vars]

# clean up
rm(card_level, parcel_level, other_parcel, card_vars, other_vars, parcel_vars,
   link, link2, link3)


# Merge other data with parcel data 
# all.x = TRUE means keep all parcel records
parcel <- merge(x = parcel, y = other, by = "ParcelID", all.x = TRUE)

# Merge card data with parcel data
# This keeps records that have matching ParcelID/TMP in both data frames
homes <- merge(x = parcel, y = card, by.x = "ParcelID", by.y = "TMP")

# Done with merging

# make names lower case
names(homes) <- tolower(names(homes))

# sort(table(homes$usecode), decreasing = TRUE)
# keep only detached residential home records 
# (e.g., not businesses, apartment complexes)
res <- c("Single Family", "Doublewide", "Duplex")
homes <- subset(homes, usecode %in% res & cardtype == "R")

# keep records with only one card associated with a parcel
homes <- subset(homes, cards < 2)

# keep homes with totalvalue greater than 0 and not NA
homes <- subset(homes, totalvalue > 0 & !is.na(totalvalue))
summary(homes$totalvalue)

# keeps homes with finsqft greater than 0 and not missing
homes <- subset(homes, finsqft > 0 & !is.na(finsqft))
summary(homes$finsqft)

# keeps homes with fullbath not missing
homes <- subset(homes, !is.na(fullbath))
table(homes$fullbath, useNA = "ifany")

# Keep homes with an assigned census tract
homes <- subset(homes, !is.na(censustract))
table(homes$censustract, useNA = "ifany")

# keep homes with yearbuilt not missing
homes <- subset(homes, !is.na(yearbuilt))
summary(homes$yearbuilt)

# create age of home
homes$age <- lubridate::year(Sys.Date()) - homes$yearbuilt
summary(homes$age)

# look at school districts
# table(homes$hsdistrict, useNA = "ifany")
# subset(homes, hsdistrict == "Unassigned")

# keep homes that are assigned to a school district
homes <- subset(homes, hsdistrict != "Unassigned")

# set some columns to factor
facvar <- c("esdistrict", "msdistrict", "hsdistrict", "censustract")
homes[,facvar] <- lapply(homes[,facvar], factor)
table(homes$hsdistrict)

# sale date should be date
# "%m/%d/%Y" = date pattern
# see help(strptime)
homes$lastsaledate1 <- lubridate::mdy(homes$lastsaledate1)

# Create month sold column
homes$month_sold <- lubridate::month(homes$lastsaledate1, label = TRUE)

# condition

# re-order levels of factor
# set order of condition levels
cond_levels <- c("Very Poor", "Poor", "Fair", "Average", 
                 "Average Plus", "Good", "Excellent") 

homes$condition <- factor(homes$condition, levels = cond_levels, ordered = TRUE)

# keep rows with condition not missing
homes <- subset(homes, !is.na(condition))
summary(homes$condition)

# yearremodeled -> remodel indicator
# summary(homes$yearremodeled) # NA not remodeled; 
homes$remodeled <- ifelse(!is.na(homes$yearremodeled), 1, 0)
table(homes$remodeled)

# cooling
# fix factor -- assume 00 and "" are no air
table(homes$cooling, useNA = "ifany") 
homes$cooling <- factor(homes$cooling)
levels(homes$cooling) <- c("No Central Air", "No Central Air", "Central Air", 
                           "Central Air", "Central Air", "No Central Air")
summary(homes$cooling)

# drop rows with missing cooling
homes <- subset(homes, !is.na(cooling))

# fp_open (these are characters)
# make a binary indicator, 0 and Null are none
table(homes$fp_open, useNA = "ifany")
homes$fp <- ifelse(homes$fp_open > 0, 1, 0)
homes$fp <- factor(homes$fp, labels = c("No", "Yes"))

# drop rows with missing fp
homes <- subset(homes, !is.na(fp))
summary(homes$fp)

# drop selected variables
homes$parcelid <- NULL
homes$cards <- NULL
homes$fp_open <- NULL
homes$cardtype <- NULL

# reset row numbers
rownames(homes) <- NULL

# tidy up
rm(res, facvar, cond_levels)

# save everything to working directory
date <- Sys.Date()
save.image(file = paste0("albemarle_homes_", date, ".Rdata")) 

# save just the homes data frame 
saveRDS(homes, file = paste0("albemarle_homes_", date, ".rds")) 

# save a csv file of the homes data; will lose factor levels!
write.csv(homes, file = paste0("albemarle_homes_", date, ".csv"), 
          row.names = FALSE) 

setwd("..")
## To run from command line

# Note that Rscript is not by default in the PATH on Windows
# Rscript session_02.R


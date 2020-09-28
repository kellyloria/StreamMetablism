## ---------------------------
## Data aggregation of historical/online data for Sagehen Creek
##
## Author: Kelly A. Loria
## Date Created: 2020-08-31
## Email: kelly.loria@nevada.unr.edu
##
## ---------------------------
## Load packages:
#install.packages("dataRetrieval")
#library(remotes)
#install_github("USGS-R/dataRetrieval", 
#               build_opts = c("--no-resave-data", "--no-manual"),
#               build_vignettes = TRUE)

library(dataRetrieval)
## ---------------------------
# data found via: https://sagehen.ucnrs.org/research/resources-data/#water
# nutrient data maybe under: https://nwis.waterdata.usgs.gov/nwis/qwdata/?site_no=10343500&agency_cd=USGS&param_group=NUT&format=rdb

## ---------------------------
# Value codes for water chemistry: The following parameters are included:
#  00600  - Total nitrogen [nitrate + nitrite + ammonia + organic-N], water, unfiltered, milligrams per liter
#  00602  - Total nitrogen [nitrate + nitrite + ammonia + organic-N], water, filtered, milligrams per liter
#  00605  - Organic nitrogen, water, unfiltered, milligrams per liter as nitrogen
#  00607  - Organic nitrogen, water, filtered, milligrams per liter as nitrogen
#  00608  - Ammonia (NH3 + NH4+), water, filtered, milligrams per liter as nitrogen
#  00610  - Ammonia (NH3 + NH4+), water, unfiltered, milligrams per liter as nitrogen
#  00613  - Nitrite, water, filtered, milligrams per liter as nitrogen
#  00618  - Nitrate, water, filtered, milligrams per liter as nitrogen
#  00623  - Ammonia plus organic nitrogen, water, filtered, milligrams per liter as nitrogen
#  00625  - Ammonia plus organic nitrogen, water, unfiltered, milligrams per liter as nitrogen
#  00630  - Nitrate plus nitrite, water, unfiltered, milligrams per liter as nitrogen
#  00631  - Nitrate plus nitrite, water, filtered, milligrams per liter as nitrogen
#  00650  - Phosphate, water, unfiltered, milligrams per liter as PO4
#  00660  - Orthophosphate, water, filtered, milligrams per liter as PO4
#  00665  - Phosphorus, water, unfiltered, milligrams per liter as phosphorus
#  00666  - Phosphorus, water, filtered, milligrams per liter as phosphorus
#  00671  - Orthophosphate, water, filtered, milligrams per liter as phosphorus
#  49570  - Particulate nitrogen, suspended in water, milligrams per liter
#  62854  - Total nitrogen [nitrate + nitrite + ammonia + organic-N], water, filtered, analytically determined, milligrams per liter
#  70507  - Orthophosphate, water, unfiltered, milligrams per liter as phosphorus
#  71845  - Ammonia (NH3 + NH4+), water, unfiltered, milligrams per liter as NH4
#  71846  - Ammonia (NH3 + NH4+), water, filtered, milligrams per liter as NH4
#  71851  - Nitrate, water, filtered, milligrams per liter as nitrate
#  71856  - Nitrite, water, filtered, milligrams per liter as nitrite
#  71886  - Phosphorus, water, unfiltered, milligrams per liter as PO4
#  71887  - Total nitrogen [nitrate + nitrite + ammonia + organic-N], water, unfiltered, milligrams per liter as nitrate

#  00060 -	Discharge
#  00065	- Gage Height
#  00010	- Temperature
#  00400	- pH


## ---------------------------
# Example workflow from: https://github.com/USGS-R/dataRetrieval
# Choptank River near Greensboro, MD
siteNumber <- "01491000" 
ChoptankInfo <- readNWISsite(siteNumber)
parameterCd <- "00060" # Discharge

#Raw daily data:
rawDailyData <- readNWISdv(siteNumber,parameterCd,
                           "1980-01-01","2010-01-01")

# Sample data Nitrate:
parameterCd <- "00618"
qwData <- readNWISqw(siteNumber,parameterCd,
                     "1980-01-01","2010-01-01")

pCode <- readNWISpCode(parameterCd)

# Water Quality Portal
specificCond <- readWQPqw(siteNumbers = 'WIDNR_WQX-10032762',
                          parameterCd = 'Specific conductance',
                          startDate = '2011-05-01',
                          endDate = '2011-09-30')
## ---------------------------
## Attempt to load in Sagehen data 10343500
siteNumber <- "10343500" 
SagehenInfo <- readNWISsite(siteNumber)
parameterCd <- "00060"

#Raw daily data:
rawDailyData <- readNWISdv(siteNumber,parameterCd)
plot(rawDailyData$Date, rawDailyData$X_00060_00003)

## Figuring out what is what at Sagehen?
# Sample data Nitrate:
parameterCd <- "00618"
qwData <- readNWISqw(siteNumber,parameterCd)
hist(qwData$result_va) # Histogram of Nitrate values
summary(qwData)
# Nitrate data resolution
plot(qwData$sample_dt, qwData$result_va)

# Sample data other...Nitrate:
parameterCd <- "00613"
qwData <- readNWISqw(siteNumber,parameterCd)
hist(qwData$result_va) # Histogram of Nitrate values 0 are probably more like NA
summary(qwData)
# Nitrate data resolution
plot(qwData$sample_dt, qwData$result_va) # not great distribution... looks like some sort of rounding error

# I will repeat with all parameters

pCode <- readNWISpCode(parameterCd)

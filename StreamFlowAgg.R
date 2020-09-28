## ---------------------------
## Data aggregation of all stream gauge data
##
## Author: Kelly A. Loria
## Date Created: 2020-09-26
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
library(ggplot2)
library(tidyverse)
library(zoo)

## ---------------------------
# Water Quality Portal
## ---------------------------
## Attempt to load in Sagehen data 10343500

# Sagehen
siteNumber <- "10343500" 
SagehenInfo <- readNWISsite(siteNumber)
parameterCd <- "00060" # Discharge

#Raw daily data:
qwData_Sagehen <- readNWISdv(siteNumber,parameterCd,
                           "1980-01-01","2020-01-01")

plot(qwData_Sagehen$Date, qwData_Sagehen$X_00060_00003)
qwData_Sagehen$site <- "Sagehen"


#pCode <- readNWISpCode(parameterCd)

# Ward creek "10336676" 
siteNumber <- "10336676" 
SagehenInfo <- readNWISsite(siteNumber)
parameterCd <- "00060" # Discharge
#Raw daily data:
qwData_ward <- readNWISdv(siteNumber,parameterCd,
                     "1980-01-01","2020-01-01")
plot(qwData_ward$Date, qwData_ward$X_00060_00003)
qwData_ward$site <- "Ward"


# Blackwood canyon "10336660"
siteNumber <- "10336660" 
SagehenInfo <- readNWISsite(siteNumber)
parameterCd <- "00060" # Discharge

#Raw daily data:
qwData_BlackW <- readNWISdv(siteNumber,parameterCd,
                          "1980-01-01","2020-01-01")
plot(qwData_BlackW$Date, qwData_BlackW$X_00060_00003)
qwData_BlackW$site <- "Blackwood"

AllFlow <- rbind(qwData_Sagehen, qwData_ward, qwData_BlackW)

p <- ggplot(AllFlow, aes(x=Date, y=(X_00060_00003), colour =as.factor(site))) +
  geom_line()  +
  theme_classic() + facet_wrap(~site)


# Average by month
library(tidyverse)
library(zoo)

AllFlow_ave=AllFlow %>% 
  mutate(month=lubridate::month(Date))%>%
  mutate(year=lubridate::year(Date)) %>%
  arrange(site, month, year)%>%
  group_by(site, month, year)%>% #this will get the nearest 15, but could be fewer if some are missing OR >35C, I think (?) the 35 are bogus so that is ok but you could
  mutate(mnD=rollapply(X_00060_00003, width = 3, FUN = mean, fill=NA),           # also filter out the NAs and >35s if you wanted to always have 15 values in your rolling window after removing bad values
         sdD=rollapply(X_00060_00003, width = 3, FUN = sd, fill=NA)) %>%
  mutate(loD=mnD- (3*sdD), hiD=mnD+ (3*sdD))%>%
  full_join(., AllFlow)

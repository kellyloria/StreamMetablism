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
library(scales)
library(tidyverse)
library(zoo)


## ---------------------------
# Water Quality Portal
  # List of all values:
  # https://help.waterdata.usgs.gov/code/parameter_cd_query?fmt=rdb&inline=true&group_cd=%
## ---------------------------
## Attempt to load in Sagehen data 10343500

# Sagehen
siteNumber <- "10343500" 
SagehenInfo <- readNWISsite(siteNumber)
parameterCd <- c("00060", #discharge 
                 "00010")  # Wtemp
                 #"62854")  # Organic nitrogen, water

#Raw daily data:
qwData_Sagehen <- readNWISdv(siteNumber,parameterCd,
                           "1980-01-01","2020-01-01")

plot(qwData_Sagehen$Date, qwData_Sagehen$X_00060_00003_cd)
qwData_Sagehen$site <- "Sagehen"
summary(qwData_Sagehen)

#pCode <- readNWISpCode(parameterCd)

# Ward creek "10336676" 
siteNumber <- "10336676" 
SagehenInfo <- readNWISsite(siteNumber)
parameterCd <- c("00060", "00010")
#Raw daily data:
qwData_ward <- readNWISdv(siteNumber,parameterCd,
                     "1980-01-01","2020-01-01")
plot(qwData_ward$Date, qwData_ward$X_00010_00003)
qwData_ward$site <- "Ward"


# Blackwood canyon "10336660"
siteNumber <- "10336660" 
SagehenInfo <- readNWISsite(siteNumber)
parameterCd <- c("00060", "00010")

#Raw daily data:
qwData_BlackW <- readNWISdv(siteNumber,parameterCd,
                          "1980-01-01","2020-01-01")
plot(qwData_BlackW$Date, qwData_BlackW$X_00060_00003)
qwData_BlackW$site <- "Blackwood"

#10336645
# General "10336645"
siteNumber <- "10336645" 
SagehenInfo <- readNWISsite(siteNumber)
parameterCd <- c("00060", "00010")

#Raw daily data:
qwData_Gen <- readNWISdv(siteNumber,parameterCd,
                            "1980-01-01","2020-01-01")
plot(qwData_Gen$Date, qwData_Gen$X_00060_00003)
qwData_Gen$site <- "General"


AllFlow <- rbind(qwData_Sagehen, qwData_ward, qwData_BlackW, qwData_Gen)

p <- ggplot(AllFlow, aes(x=Date, y=(X_00060_00003), colour =as.factor(site))) +
  geom_line()  + ylab("Discharge") +
  theme_classic() + facet_wrap(~site)


p2 <- ggplot(AllFlow, aes(x=Date, y=(X_00010_00003), colour =as.factor(site))) +
  geom_line()  + ylab("Water TempC") + 
  scale_x_date(date_breaks = "6 month", 
               labels=date_format("%b-%Y"),
               limits = as.Date(c('2016-01-01','2020-01-01'))) +
  theme_classic() + facet_wrap(~site)


# East side?
# Nutrient species?
# Span of snow data?

# Lets get some precipitation data:

## ---------------------------
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

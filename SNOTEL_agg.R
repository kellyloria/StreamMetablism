## ---------------------------
## Data aggregation of SNOTEL data for the area
##
## Author: Kelly A. Loria
## Date Created: 2020-10-23
## Email: kelly.loria@nevada.unr.edu

# Station links:
# https://wcc.sc.egov.usda.gov/nwcc/site?sitenum=848
# https://wcc.sc.egov.usda.gov/nwcc/site?sitenum=539
# https://wcc.sc.egov.usda.gov/reportGenerator/edit/customSingleStationReport/daily/start_of_period/724:CA:SNTL%7Cid=%22%22%7Cname/-22,0/WTEQ::value,SNWD::value,PREC::value,TOBS::value,TMAX::value,TMIN::value,TAVG::value?fitToScreen=false

## ---------------------------
# File path setup:
if (dir.exists('/Users/kellyloria/Documents/UNR_2020/Fall2020Projects/SNOTEL')){
  inputDir<- '/Users/kellyloria/Documents/UNR_2020/Fall2020Projects/SNOTEL'
  outputDir<- '/Users/kellyloria/Documents/UNR_2020/Fall2020Projects/SNOTEL' 
}

## ---------------------------
library(ggplot2)

## ---------------------------
WardSNOTEL <- read.delim(paste0(inputDir, "/WardSNOTEL.TXT"), header=T, sep = ',')
summary(WardSNOTEL)
names(WardSNOTEL)
WardSNOTEL$Date
# attempting to convert dt1$date dateTime string to R date structure (date or POSIXct)                                
tmpDateFormat<-"%Y-%m-%d"
tmp1date<-as.Date(WardSNOTEL$Date,format=tmpDateFormat)
# Keep the new dates only if they all converted correctly
if(length(tmp1date) == length(tmp1date[!is.na(tmp1date)])){WardSNOTEL$Date <- tmp1date } else {print("Date conversion failed for dt1$date. Please inspect the data and do the date conversion yourself.")}                                                                    
rm(tmpDateFormat,tmp1date) 

#need to rename some columns:
names(WardSNOTEL)[2] <- "PrecipAccum.ave"
names(WardSNOTEL)[3] <- "AirTempF.ave"
names(WardSNOTEL)[4] <- "AirTempF.max"
names(WardSNOTEL)[5] <- "AirTempF.min"
names(WardSNOTEL)[6] <- "SnowDepth"
names(WardSNOTEL)[7] <- "SWEin"
names(WardSNOTEL)[8] <- "SWEin.max"
WardSNOTEL$Site <- 'Ward.ST'
WardSNOTEL$Elevation.ft <- c(6745)
WardSNOTEL$Lat <- c(39.133)
WardSNOTEL$Long <- c(-120.217)

qplot(Date, PrecipAccum.ave, data = WardSNOTEL.Q, geom="line") +
  theme(axis.text.x = element_text(angle = 25, vjust = 1.0, hjust = 1.0))

qplot(Date, SWEin, data = WardSNOTEL.Q, geom="line") +
  theme(axis.text.x = element_text(angle = 25, vjust = 1.0, hjust = 1.0))

qplot(Date, SnowDepth, data = WardSNOTEL.Q, geom="line") +
  theme(axis.text.x = element_text(angle = 25, vjust = 1.0, hjust = 1.0))

WardSNOTEL.Q <- subset(WardSNOTEL, Date >= ('2000-10-01') & 
                         Date <= ('2020-10-01'))
summary(WardSNOTEL.Q)


## ---------------------------
RubiconSNOTEL <- read.delim(paste0(inputDir, "/RubiconSNOTEL_dat.TXT"), header=T, sep = ',')
summary(RubiconSNOTEL)
names(RubiconSNOTEL)
RubiconSNOTEL$Date
# attempting to convert dt1$date dateTime string to R date structure (date or POSIXct)                                
tmpDateFormat<-"%Y-%m-%d"
tmp1date<-as.Date(RubiconSNOTEL$Date,format=tmpDateFormat)
# Keep the new dates only if they all converted correctly
if(length(tmp1date) == length(tmp1date[!is.na(tmp1date)])){RubiconSNOTEL$Date <- tmp1date } else {print("Date conversion failed for dt1$date. Please inspect the data and do the date conversion yourself.")}                                                                    
rm(tmpDateFormat,tmp1date) 

#need to rename some columns:
names(RubiconSNOTEL)[2] <- "PrecipAccum.ave" # Not in this data set :(
names(RubiconSNOTEL)[3] <- "AirTempF.ave"
names(RubiconSNOTEL)[4] <- "AirTempF.max"
names(RubiconSNOTEL)[5] <- "AirTempF.min"
names(RubiconSNOTEL)[6] <- "SnowDepth"
names(RubiconSNOTEL)[7] <- "SWEin"
names(RubiconSNOTEL)[8] <- "SWEin.max" # Not in this data set :(
RubiconSNOTEL$Site <- 'Rubicon.ST'
RubiconSNOTEL$Elevation.ft <- c(7619)
RubiconSNOTEL$Lat <- c(	39.00)
RubiconSNOTEL$Long <- c(-120.13)


qplot(Date, PrecipAccum.ave, data = RubiconSNOTEL.Q, geom="line") +
  theme(axis.text.x = element_text(angle = 25, vjust = 1.0, hjust = 1.0))

qplot(Date, SWEin, data = RubiconSNOTEL.Q, geom="line") +
  theme(axis.text.x = element_text(angle = 25, vjust = 1.0, hjust = 1.0))

qplot(Date, SnowDepth, data = RubiconSNOTEL.Q, geom="line") +
  theme(axis.text.x = element_text(angle = 25, vjust = 1.0, hjust = 1.0))

qplot(Date, AirTempF.ave, data = RubiconSNOTEL.Q, geom="line") +
  theme(axis.text.x = element_text(angle = 25, vjust = 1.0, hjust = 1.0))

RubiconSNOTEL.Q <- subset(RubiconSNOTEL, Date >= ('2000-10-01') & 
                         Date <= ('2020-10-01'))


## ---------------------------
Indi2SNOTEL <- read.delim(paste0(inputDir, "/IndependenceSNOTEL2_dat.TXT"), header=T, sep = ',')
summary(Indi2SNOTEL)
names(Indi2SNOTEL)
Indi2SNOTEL$Date
# attempting to convert dt1$date dateTime string to R date structure (date or POSIXct)                                
tmpDateFormat<-"%Y-%m-%d"
tmp1date<-as.Date(Indi2SNOTEL$Date,format=tmpDateFormat)
# Keep the new dates only if they all converted correctly
if(length(tmp1date) == length(tmp1date[!is.na(tmp1date)])){Indi2SNOTEL$Date <- tmp1date } else {print("Date conversion failed for dt1$date. Please inspect the data and do the date conversion yourself.")}                                                                    
rm(tmpDateFormat,tmp1date) 

#need to rename some columns:
names(Indi2SNOTEL)[2] <- "PrecipAccum.ave" # Not in this data set :(
names(Indi2SNOTEL)[3] <- "AirTempF.ave"
names(Indi2SNOTEL)[4] <- "AirTempF.max"
names(Indi2SNOTEL)[5] <- "AirTempF.min"
names(Indi2SNOTEL)[6] <- "SnowDepth"
names(Indi2SNOTEL)[7] <- "SWEin"
names(Indi2SNOTEL)[8] <- "SWEin.max" # Not in this data set :(

Indi2SNOTEL$Site <- 'IndiCamp.ST'
Indi2SNOTEL$Elevation.ft <- c(6980)
Indi2SNOTEL$Lat <- c(39.450)
Indi2SNOTEL$Long <- c(-120.300)

qplot(Date, PrecipAccum.ave, data = Indi2SNOTEL.Q, geom="line") +
  theme(axis.text.x = element_text(angle = 25, vjust = 1.0, hjust = 1.0))

qplot(Date, SWEin, data = Indi2SNOTEL.Q, geom="line") +
  theme(axis.text.x = element_text(angle = 25, vjust = 1.0, hjust = 1.0))

qplot(Date, SnowDepth, data = Indi2SNOTEL.Q, geom="line") +
  theme(axis.text.x = element_text(angle = 25, vjust = 1.0, hjust = 1.0))

qplot(Date, AirTempF.ave, data = Indi2SNOTEL.Q, geom="line") +
  theme(axis.text.x = element_text(angle = 25, vjust = 1.0, hjust = 1.0))

Indi2SNOTEL.Q <- subset(Indi2SNOTEL, Date >= ('2000-10-01') & 
                         Date <= ('2020-10-01'))
summary(Indi2SNOTEL.Q)



## ---------------------------
#IndiSNOTEL <- read.delim(paste0(inputDir, "/IndependenceSNOTEL_dat.TXT"), header=T, sep = ',')
#summary(IndiSNOTEL)
#names(IndiSNOTEL)
#IndiSNOTEL$Date
## attempting to convert dt1$date dateTime string to R date structure (date or POSIXct)                                
#tmpDateFormat<-"%Y-%m-%d"
#tmp1date<-as.Date(IndiSNOTEL$Date,format=tmpDateFormat)
## Keep the new dates only if they all converted correctly
#if(length(tmp1date) == length(tmp1date[!is.na(tmp1date)])){IndiSNOTEL$Date <- tmp1date } else {print("Date conversion failed for dt1$date. Please inspect the data and do the date conversion yourself.")}                                                                    
#rm(tmpDateFormat,tmp1date) 

##need to rename some columns:
#names(IndiSNOTEL)[2] <- "PrecipAccum.ave" # Not in this data set :(
#names(IndiSNOTEL)[3] <- "AirTempF.ave"
#names(IndiSNOTEL)[4] <- "AirTempF.max"
#names(IndiSNOTEL)[5] <- "AirTempF.min"
#names(IndiSNOTEL)[6] <- "SnowDepth"
#names(IndiSNOTEL)[7] <- "SWEin"
#names(IndiSNOTEL)[8] <- "SWEin.max" # Not in this data set :(
#IndiSNOTEL$Site <- 'Indi.ST'

#Indi2SNOTEL$Elevation.ft <- c(8338)
#Indi2SNOTEL$Lat <- c(39.433)
#Indi2SNOTEL$Long <- c(-120.37)

#qplot(Date, PrecipAccum.ave, data = IndiSNOTEL.Q, geom="line") +
#  theme(axis.text.x = element_text(angle = 25, vjust = 1.0, hjust = 1.0))

#qplot(Date, SWEin, data = IndiSNOTEL.Q, geom="line") +
#  theme(axis.text.x = element_text(angle = 25, vjust = 1.0, hjust = 1.0))

#qplot(Date, SnowDepth, data = IndiSNOTEL.Q, geom="line") +
#  theme(axis.text.x = element_text(angle = 25, vjust = 1.0, hjust = 1.0))

#qplot(Date, AirTempF.ave, data = IndiSNOTEL.Q, geom="line") +
#  theme(axis.text.x = element_text(angle = 25, vjust = 1.0, hjust = 1.0))

#IndiSNOTEL.Q <- subset(IndiSNOTEL, Date >= ('2000-10-01') & 
#                         Date <= ('2020-10-01'))

# Agg all snow data 
SNOTEL_agg <- rbind(WardSNOTEL.Q, RubiconSNOTEL.Q, Indi2SNOTEL.Q)

# plot 
qplot(Date, AirTempF.ave, data = SNOTEL_agg, geom="line",  colour =as.factor(Site)) +
  theme(axis.text.x = element_text(angle = 25, vjust = 1.0, hjust = 1.0))

p2 <- ggplot(SNOTEL_agg, aes(x=Date, y=(AirTempF.ave), colour =as.factor(Site))) +
  geom_line() + theme_classic() + facet_wrap(~Site)

p2 <- ggplot(SNOTEL_agg, aes(x=Date, y=(SWEin), colour =as.factor(Site))) +
  geom_line() + theme_classic() + facet_wrap(~Site)





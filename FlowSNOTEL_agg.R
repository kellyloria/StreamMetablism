## ---------------------------
# File path setup:
if (dir.exists('/Users/kellyloria/Documents/UNR_2020/Fall2020Projects/SNOTEL')){
  inputDir<- '/Users/kellyloria/Documents/UNR_2020/Fall2020Projects/SNOTEL'
  outputDir<- '/Users/kellyloria/Documents/UNR_2020/Fall2020Courses/EvenStats/FinalPrioject' 
}

## ----
# Load packages
library(tidyverse)
library(lme4)
library(lmerTest)
library(MuMIn)
library(mosaic)
## ---------------------------
# I. Get data:

# Restrict date range from flow script, see 'StreamFlowAgg.R' object: qwData_Sagehen
qwData_Sagehen

flow_Sagehen <- subset(qwData_Sagehen, Date >= ('1999-10-01') & 
                         Date <= ('2020-10-01'))
summary(flow_Sagehen)
#rename the columns
names(flow_Sagehen)[4] <- "WtempC"
names(flow_Sagehen)[6] <- "discharge"
names(flow_Sagehen)[8] <- "GaugeSite"
summary(flow_Sagehen)

# Select relevant columns:
flow_Sagehen.q <- subset(flow_Sagehen, select=c(GaugeSite, Date, WtempC, discharge))
summary(flow_Sagehen.q)

# add in WS characteristics from: https://streamstatsags.cr.usgs.gov/gagepages/html/10343500.htm
flow_Sagehen.q$Relative_Relief <- 129.8
flow_Sagehen.q$Relief_T <- 2380
flow_Sagehen.q$Per_Forest <- 57.6
flow_Sagehen.q$Per_Impervious <- 0.11
flow_Sagehen.q$Contrib_Drainage_Area <- 10.60

# Convert temps to C from snotel script, see 'SNOTEL_agg.R' object: Indi2SNOTEL.Q
Indi2SNOTEL.Q$AirTempCave <- c((Indi2SNOTEL.Q$AirTempF.ave - 32) * (5/9))
Indi2SNOTEL.Q$AirTempCmax <- c((Indi2SNOTEL.Q$AirTempF.max - 32) * (5/9))
Indi2SNOTEL.Q$AirTempCmin <- c((Indi2SNOTEL.Q$AirTempF.min - 32) * (5/9))
summary(Indi2SNOTEL.Q)
# Select relevant columns:
Indi2SNOTEL.Q2 <- subset(Indi2SNOTEL.Q, select=c(Site, Date, PrecipAccum.aveR, SnowDepth, SWEin,
                                                 AirTempCave, AirTempCmax, AirTempCmin, PrecipIncrement, SnowRainRatio))
summary(Indi2SNOTEL.Q2)


# Left join the 2 data frames together:
Sagehen_Dat <- left_join(flow_Sagehen.q, Indi2SNOTEL.Q2[c("Date", "PrecipAccum.aveR", "SnowDepth", "SWEin",
                                                          "AirTempCave", "AirTempCmax", "AirTempCmin", "PrecipIncrement", "SnowRainRatio")],
                         by = c("Date" = "Date"))
summary(Sagehen_Dat)

# Check to make sure this all makes sense
qplot(WtempC, AirTempCave, data = Sagehen_Dat, geom="point") +
  theme(axis.text.x = element_text(angle = 25, vjust = 1.0, hjust = 1.0))

qplot(discharge, PrecipAccum.aveR, data = Sagehen_Dat, geom="point") +
  theme(axis.text.x = element_text(angle = 25, vjust = 1.0, hjust = 1.0))

##----
## Preform the same for Ward:
# Restrict date range from flow script, see 'StreamFlowAgg.R' object: qwData_Sagehen
qwData_Sagehen

flow_Ward <- subset(qwData_ward, Date >= ('1999-10-01') & 
                      Date <= ('2020-10-01'))
summary(flow_Ward)
#rename the columns
names(flow_Ward)[4] <- "WtempC"
names(flow_Ward)[6] <- "discharge"
names(flow_Ward)[8] <- "GaugeSite"
summary(flow_Ward)

# Select relevant columns:
flow_Ward.q <- subset(flow_Ward, select=c(GaugeSite, Date, WtempC, discharge))
summary(flow_Ward.q)

# add in WS characteristics from: https://streamstatsags.cr.usgs.gov/gagepages/html/10336676.htm
flow_Ward.q$Relative_Relief <- 145.7
flow_Ward.q$Relief_T <- 2598
flow_Ward.q$Per_Forest <- 56.7
flow_Ward.q$Per_Impervious <- 0.34
flow_Ward.q$Contrib_Drainage_Area <- 9.53

# Convert temps to C from snotel script, see 'SNOTEL_agg.R' object: Indi2SNOTEL.Q
WardSNOTEL.Q$AirTempCave <- c((WardSNOTEL.Q$AirTempF.ave - 32) * (5/9))
WardSNOTEL.Q$AirTempCmax <- c((WardSNOTEL.Q$AirTempF.max - 32) * (5/9))
WardSNOTEL.Q$AirTempCmin <- c((WardSNOTEL.Q$AirTempF.min - 32) * (5/9))
summary(WardSNOTEL.Q)
# Select relevant columns:
WardSNOTEL.Q2 <- subset(WardSNOTEL.Q, select=c(Site, Date, PrecipAccum.aveR, SnowDepth, SWEin,
                                               AirTempCave, AirTempCmax, AirTempCmin, 
                                               PrecipIncrement, SnowRainRatio))
summary(WardSNOTEL.Q2)

# Left join the 2 data frames together:
Ward_Dat <- left_join(flow_Ward.q, WardSNOTEL.Q2[c("Date", "PrecipAccum.aveR", "SnowDepth", "SWEin",
                                                   "AirTempCave", "AirTempCmax", "AirTempCmin",
                                                   "PrecipIncrement", "SnowRainRatio")],
                      by = c("Date" = "Date"))
summary(Ward_Dat)

# Check to make sure this all makes sense
qplot(WtempC, AirTempCave, data = Ward_Dat, geom="point") +
  theme(axis.text.x = element_text(angle = 25, vjust = 1.0, hjust = 1.0))

qplot(discharge, PrecipAccum.ave, data = Ward_Dat, geom="point") +
  theme(axis.text.x = element_text(angle = 25, vjust = 1.0, hjust = 1.0))


##----
## Preform the same for Blackwood:
# Restrict date range from flow script, see 'StreamFlowAgg.R' object: qwData_Sagehen
qwData_Sagehen

flow_Blackwood <- subset(qwData_BlackW, Date >= ('1999-10-01') & 
                           Date <= ('2020-10-01'))
summary(flow_Blackwood)
#rename the columns
names(flow_Blackwood)[4] <- "WtempC"
names(flow_Blackwood)[6] <- "discharge"
names(flow_Blackwood)[8] <- "GaugeSite"
summary(flow_Blackwood)

# Select relevant columns:
flow_Blackwood.q <- subset(flow_Blackwood, select=c(GaugeSite, Date, WtempC, discharge))
summary(flow_Blackwood.q)

# add in WS characteristics from: https://streamstatsags.cr.usgs.gov/gagepages/html/10336676.htm
flow_Blackwood.q$Relative_Relief <- NA
flow_Blackwood.q$Relief_T <- c((7241-6234)*2)
flow_Blackwood.q$Per_Forest <- 98.0
flow_Blackwood.q$Per_Impervious <- NA
flow_Blackwood.q$Contrib_Drainage_Area <- 11.9

# Convert temps to C from snotel script, see 'SNOTEL_agg.R' object: Indi2SNOTEL.Q
WardSNOTEL.Q$AirTempCave <- c((WardSNOTEL.Q$AirTempF.ave - 32) * (5/9))
WardSNOTEL.Q$AirTempCmax <- c((WardSNOTEL.Q$AirTempF.max - 32) * (5/9))
WardSNOTEL.Q$AirTempCmin <- c((WardSNOTEL.Q$AirTempF.min - 32) * (5/9))
summary(WardSNOTEL.Q)

# Select relevant columns:
WardSNOTEL.Q2 <- subset(WardSNOTEL.Q, select=c(Site, Date, PrecipAccum.aveR, SnowDepth, SWEin,
                                               AirTempCave, AirTempCmax, AirTempCmin, 
                                               PrecipIncrement, SnowRainRatio))
summary(WardSNOTEL.Q2)

# Left join the 2 data frames together:
Blackwood_Dat <- left_join(flow_Blackwood.q, WardSNOTEL.Q2[c("Date", "PrecipAccum.aveR", "SnowDepth", "SWEin",
                                                             "AirTempCave", "AirTempCmax", "AirTempCmin",
                                                             "PrecipIncrement", "SnowRainRatio")],
                           by = c("Date" = "Date"))
summary(Blackwood_Dat)

# Check to make sure this all makes sense
qplot(WtempC, AirTempCave, data = Blackwood_Dat, geom="point") +
  theme(axis.text.x = element_text(angle = 25, vjust = 1.0, hjust = 1.0))

qplot(discharge, PrecipAccum.ave, data = Blackwood_Dat, geom="point") +
  theme(axis.text.x = element_text(angle = 25, vjust = 1.0, hjust = 1.0))


##----
## Preform the same for General:
# Restrict date range from flow script, see 'StreamFlowAgg.R' object: 

qwData_Gen

flow_General <- subset(qwData_Gen, Date >= ('1999-10-01') & 
                         Date <= ('2020-10-01'))
summary(flow_General)
#rename the columns
names(flow_General)[4] <- "WtempC"
names(flow_General)[6] <- "discharge"
names(flow_General)[8] <- "GaugeSite"
summary(flow_General)

# Select relevant columns:
flow_General.q <- subset(flow_General, select=c(GaugeSite, Date, WtempC, discharge))
summary(flow_General.q)

# add in WS characteristics from: https://streamstatsags.cr.usgs.gov/gagepages/html/10336645.htm
flow_General.q$Relative_Relief <- NA
flow_General.q$Relief_T <- c((7184-6250)*2)
flow_General.q$Per_Forest <- NA
flow_General.q$Per_Impervious <- NA
flow_General.q$Contrib_Drainage_Area <- 7.54

# Convert temps to C from snotel script, see 'SNOTEL_agg.R' object: Indi2SNOTEL.Q
RubiconSNOTEL.Q$AirTempCave <- c((RubiconSNOTEL.Q$AirTempF.ave - 32) * (5/9))
RubiconSNOTEL.Q$AirTempCmax <- c((RubiconSNOTEL.Q$AirTempF.max - 32) * (5/9))
RubiconSNOTEL.Q$AirTempCmin <- c((RubiconSNOTEL.Q$AirTempF.min - 32) * (5/9))
summary(RubiconSNOTEL.Q)
# Select relevant columns:
RubiconSNOTEL.Q2 <- subset(RubiconSNOTEL.Q, select=c(Site, Date, PrecipAccum.aveR, SnowDepth, SWEin,
                                                     AirTempCave, AirTempCmax, AirTempCmin,
                                                     PrecipIncrement, SnowRainRatio))
summary(RubiconSNOTEL.Q2)

# Left join the 2 data frames together:
General_Dat <- left_join(flow_General.q, RubiconSNOTEL.Q2[c("Date", "PrecipAccum.aveR", "SnowDepth", "SWEin",
                                                            "AirTempCave", "AirTempCmax", "AirTempCmin",
                                                            "PrecipIncrement", "SnowRainRatio")],
                         by = c("Date" = "Date"))
summary(General_Dat)

# Check to make sure this all makes sense
qplot(WtempC, AirTempCave, data = General_Dat, geom="point") +
  theme(axis.text.x = element_text(angle = 25, vjust = 1.0, hjust = 1.0))

qplot(discharge, PrecipAccum.aveR, data = General_Dat, geom="point") + # not in data set so need to use SWE
  theme(axis.text.x = element_text(angle = 25, vjust = 1.0, hjust = 1.0))

###---
#Save all data in 1 file:
NRES710_MSM_dat <- rbind(General_Dat, Blackwood_Dat, Ward_Dat, Sagehen_Dat)

# Add in date variables 
d <- transform(NRES710_MSM_dat,
               year = as.numeric(format(Date, '%Y')),
               week = as.numeric(format(Date, '%U')),
               nmonth = as.numeric(format(Date, '%m')),
               doy = as.numeric(format(Date, '%j')),
               day = as.numeric(format(Date, '%d')))


# Add in date variables + column for water year: W_yr
wtr_yr <- function(dates, start_month = 10) {
  # Convert possible character vector into date
  d1 = as.Date(dates)
  # Year offset
  offset = ifelse(as.integer(format(d1, "%m")) < start_month, 0, 1)
  # Water year
  adj.year = as.integer(format(d1, "%Y")) + offset
  # Return the water year
  return(adj.year)
}

d$wtr_yr <- wtr_yr(d$Date)
d <- d %>%
  group_by(wtr_yr) %>% 
  mutate(wtr_day = 
           (as.integer(difftime(Date,ymd(paste0(wtr_yr - 1 ,'-09-30')), units = "days")))) %>%
  mutate(
    winter = 
      case_when(
        nmonth < 11 | nmonth > 3 ~ 'summer',
        nmonth > 11 | nmonth < 3 ~ 'winter'))



summary(d$winter)
# columns for whether it will rain or snow
#  a * [tanh(b *( Mean Temp – c))] – d
# Snow Probability = -50 * [tanh(0.4 *( Mean Temp – 1.75))] – 1 
###
# dq <- d %>%
#   mutate(
#     prob_Snow=
#       (
#       -50*(tanh(0.4*(AirTempCave-1.75)))-1))# %>%
#   mutate(
#     Snow=
#       case_when(
#         prob_Snow < 0&!is.na(prob_Snow) & 
#           PrecipIncrement > 0&!is.na(PrecipIncrement)  & 
#           nmonth <11 | nmonth > 4 ~ 'rain' ,
#         prob_Snow > 0&!is.na(prob_Snow) & 
#           PrecipIncrement > 0&!is.na(PrecipIncrement) & 
#           nmonth > 11 | nmonth < 4 ~ 'snow')) %>%
#   mutate(
#     Snow_bi=
#       case_when(
#         prob_Snow < 0&!is.na(prob_Snow) & 
#           PrecipIncrement > 0&!is.na(PrecipIncrement)  & 
#           nmonth <11 | nmonth > 4 ~ 0,
#         prob_Snow > 0&!is.na(prob_Snow) & 
#           PrecipIncrement > 0&!is.na(PrecipIncrement) & 
#           nmonth > 11 | nmonth < 4 ~ 1)) %>%
#   mutate(
#     ROS=
#       case_when(
#         prob_Snow < 0&!is.na(prob_Snow) & 
#           PrecipIncrement > 3&!is.na(PrecipIncrement) & 
#           nmonth <11 | nmonth > 4 ~ 0,
#         prob_Snow > 0&!is.na(prob_Snow) & 
#           PrecipIncrement > 3&!is.na(PrecipIncrement) &
#           nmonth > 11 | nmonth < 4 ~ 1)) 

p <- ggplot(dq, aes(x=wtr_day, y=prob_Snow, colour =as.factor(Snow_bi), shape =as.factor(GaugeSite))) + 
  geom_point() + 
  theme_classic() + facet_wrap(~wtr_yr)

p <- ggplot(dq, aes(x=Snow_bi, y=prob_Snow, colour =as.factor(Snow_bi), shape =as.factor(GaugeSite))) + 
  geom_point() + 
  theme_classic() + facet_wrap(~wtr_yr)

p <- ggplot(dq, aes(x=wtr_day, y=PrecipIncrement, colour =as.factor(ROS), shape =as.factor(GaugeSite))) + 
  geom_point() + 
  theme_classic() + facet_wrap(~wtr_yr)

#
df <- read.csv(paste0(inputDir, "/NRES710_MSM_dat.csv"), header=T, sep = ',')
tmpDateFormat<-"%Y-%m-%d"
tmp1date<-as.Date(df$Date,format=tmpDateFormat)
# Keep the new dates only if they all converted correctly
if(length(tmp1date) == length(tmp1date[!is.na(tmp1date)])){df$Date <- tmp1date } else {print("Date conversion failed for dt1$date. Please inspect the data and do the date conversion yourself.")}                                                                    
rm(tmpDateFormat,tmp1date) 
summary(df)

datcheck <- subset(df, Snow_0NY == 1) # looks good. 
summary(datcheck)
range(datcheck$Snow)

p <- ggplot(df, aes(x=wtr_day, y=PrecipIncrement, colour =as.factor(ROS_0NY), shape =as.factor(GaugeSite))) + 
  geom_point() + 
  theme_classic() + facet_wrap(~wtr_yr)

mod <- lmer( discharge ~ scale(ROS_0NY) + scale(PrecipIncrement) + scale(wtr_yr) + (1|GaugeSite), data =df) 
summary(mod) # sig  
hist(residuals(mod)) # likely much more zero inflated... need to figureout how to standardize with lag


#write.csv(dq,  paste0(outputDir,"/NRES710_MSM_dat.csv")) # complied data file of all RBR sensors along buoy line

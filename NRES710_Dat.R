#NRES701_Dat

## ---------------------------
# File path setup:
if (dir.exists('/Users/kellyloria/Documents/UNR_2020/Fall2020Projects/SNOTEL')){
  inputDir<- '/Users/kellyloria/Documents/UNR_2020/Fall2020Projects/SNOTEL'
  outputDir<- '/Users/kellyloria/Documents/UNR_2020/Fall2020Courses/EvenStats/FinalPrioject' 
}

## ---------------------------
library(tidyverse)

# Restrict date range from flow script, see 'StreamFlowAgg.R' object: qwData_Sagehen
qwData_Sagehen

flow_Sagehen <- subset(qwData_Sagehen, Date >= ('2000-10-01') & 
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
flow_Sagehen.q


# Convert temps to C from snotel script, see 'SNOTEL_agg.R' object: Indi2SNOTEL.Q
Indi2SNOTEL.Q$AirTempCave <- c((Indi2SNOTEL.Q$AirTempF.ave - 32) * (5/9))
Indi2SNOTEL.Q$AirTempCmax <- c((Indi2SNOTEL.Q$AirTempF.max - 32) * (5/9))
Indi2SNOTEL.Q$AirTempCmin <- c((Indi2SNOTEL.Q$AirTempF.min - 32) * (5/9))
summary(Indi2SNOTEL.Q)
# Select relevant columns:
Indi2SNOTEL.Q2 <- subset(Indi2SNOTEL.Q, select=c(Site, Date, PrecipAccum.ave, SnowDepth, SWEin,
                                                 AirTempCave, AirTempCmax, AirTempCmin))
summary(Indi2SNOTEL.Q2)


# Left join the 2 data frames together:
Sagehen_Dat <- left_join(flow_Sagehen.q, Indi2SNOTEL.Q2[c("Date", "PrecipAccum.ave", "SnowDepth", "SWEin",
                                           "AirTempCave", "AirTempCmax", "AirTempCmin")],
                       by = c("Date" = "Date"))
summary(Sagehen_Dat)

# Check to make sure this all makes sense
qplot(WtempC, AirTempCave, data = Sagehen_Dat, geom="point") +
  theme(axis.text.x = element_text(angle = 25, vjust = 1.0, hjust = 1.0))

qplot(discharge, PrecipAccum.ave, data = Sagehen_Dat, geom="point") +
  theme(axis.text.x = element_text(angle = 25, vjust = 1.0, hjust = 1.0))

# Want to pull out winter months:
# December - March 

library(lubridate)
Sagehen_Dat <-transform(Sagehen_Dat,
                              month = as.numeric(format(Date, '%m')))

Sagehen_winterDat <- subset(Sagehen_Dat, month >= 12 | month <= 3)
summary(Sagehen_winterDat)
  
qplot(discharge, PrecipAccum.ave, data = Sagehen_winterDat, geom="point") +
  theme(axis.text.x = element_text(angle = 25, vjust = 1.0, hjust = 1.0))

qplot(discharge, SWEin, data = Sagehen_winterDat, geom="point") +
  theme(axis.text.x = element_text(angle = 25, vjust = 1.0, hjust = 1.0))

qplot(discharge, SnowDepth, data = Sagehen_winterDat, geom="point") +
  theme(axis.text.x = element_text(angle = 25, vjust = 1.0, hjust = 1.0))

qplot(discharge, AirTempCave, data = Sagehen_winterDat, geom="point") +
  theme(axis.text.x = element_text(angle = 25, vjust = 1.0, hjust = 1.0))


##----
## Preform the same for Ward:
# Restrict date range from flow script, see 'StreamFlowAgg.R' object: qwData_Sagehen
qwData_Sagehen

flow_Ward <- subset(qwData_ward, Date >= ('2000-10-01') & 
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

# Convert temps to C from snotel script, see 'SNOTEL_agg.R' object: Indi2SNOTEL.Q
WardSNOTEL.Q$AirTempCave <- c((WardSNOTEL.Q$AirTempF.ave - 32) * (5/9))
WardSNOTEL.Q$AirTempCmax <- c((WardSNOTEL.Q$AirTempF.max - 32) * (5/9))
WardSNOTEL.Q$AirTempCmin <- c((WardSNOTEL.Q$AirTempF.min - 32) * (5/9))
summary(WardSNOTEL.Q)
# Select relevant columns:
WardSNOTEL.Q2 <- subset(WardSNOTEL.Q, select=c(Site, Date, PrecipAccum.ave, SnowDepth, SWEin,
                                                 AirTempCave, AirTempCmax, AirTempCmin))
summary(WardSNOTEL.Q2)

# Left join the 2 data frames together:
Ward_Dat <- left_join(flow_Ward.q, WardSNOTEL.Q2[c("Date", "PrecipAccum.ave", "SnowDepth", "SWEin",
                                                          "AirTempCave", "AirTempCmax", "AirTempCmin")],
                         by = c("Date" = "Date"))
summary(Ward_Dat)

# Check to make sure this all makes sense
qplot(WtempC, AirTempCave, data = Ward_Dat, geom="point") +
  theme(axis.text.x = element_text(angle = 25, vjust = 1.0, hjust = 1.0))

qplot(discharge, PrecipAccum.ave, data = Ward_Dat, geom="point") +
  theme(axis.text.x = element_text(angle = 25, vjust = 1.0, hjust = 1.0))

# Want to pull out winter months:
# December - March 

library(lubridate)
Ward_Dat <-transform(Ward_Dat,
                        month = as.numeric(format(Date, '%m')))

Ward_winterDat <- subset(Ward_Dat, month >= 12 | month <= 3)
summary(Ward_winterDat)

qplot(discharge, PrecipAccum.ave, data = Ward_winterDat, geom="point") +
  theme(axis.text.x = element_text(angle = 25, vjust = 1.0, hjust = 1.0))

qplot(discharge, SWEin, data = Ward_winterDat, geom="point") +
  theme(axis.text.x = element_text(angle = 25, vjust = 1.0, hjust = 1.0))

qplot(discharge, SnowDepth, data = Ward_winterDat, geom="point") +
  theme(axis.text.x = element_text(angle = 25, vjust = 1.0, hjust = 1.0))

qplot(discharge, AirTempCave, data = Ward_winterDat, geom="point") +
  theme(axis.text.x = element_text(angle = 25, vjust = 1.0, hjust = 1.0))



##----
## Preform the same for Blackwood:
# Restrict date range from flow script, see 'StreamFlowAgg.R' object: qwData_Sagehen
qwData_Sagehen

flow_Blackwood <- subset(qwData_BlackW, Date >= ('2000-10-01') & 
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

# Convert temps to C from snotel script, see 'SNOTEL_agg.R' object: Indi2SNOTEL.Q
WardSNOTEL.Q$AirTempCave <- c((WardSNOTEL.Q$AirTempF.ave - 32) * (5/9))
WardSNOTEL.Q$AirTempCmax <- c((WardSNOTEL.Q$AirTempF.max - 32) * (5/9))
WardSNOTEL.Q$AirTempCmin <- c((WardSNOTEL.Q$AirTempF.min - 32) * (5/9))
summary(WardSNOTEL.Q)
# Select relevant columns:
WardSNOTEL.Q2 <- subset(WardSNOTEL.Q, select=c(Site, Date, PrecipAccum.ave, SnowDepth, SWEin,
                                               AirTempCave, AirTempCmax, AirTempCmin))
summary(WardSNOTEL.Q2)

# Left join the 2 data frames together:
Blackwood_Dat <- left_join(flow_Blackwood.q, WardSNOTEL.Q2[c("Date", "PrecipAccum.ave", "SnowDepth", "SWEin",
                                                   "AirTempCave", "AirTempCmax", "AirTempCmin")],
                      by = c("Date" = "Date"))
summary(Blackwood_Dat)

# Check to make sure this all makes sense
qplot(WtempC, AirTempCave, data = Blackwood_Dat, geom="point") +
  theme(axis.text.x = element_text(angle = 25, vjust = 1.0, hjust = 1.0))

qplot(discharge, PrecipAccum.ave, data = Blackwood_Dat, geom="point") +
  theme(axis.text.x = element_text(angle = 25, vjust = 1.0, hjust = 1.0))

# Want to pull out winter months:
# December - March 

library(lubridate)
Blackwood_Dat <-transform(Blackwood_Dat,
                     month = as.numeric(format(Date, '%m')))

Blackwood_winterDat <- subset(Blackwood_Dat, month >= 12 | month <= 3)
summary(Blackwood_winterDat)

qplot(discharge, PrecipAccum.ave, data = Blackwood_winterDat, geom="point") +
  theme(axis.text.x = element_text(angle = 25, vjust = 1.0, hjust = 1.0))

qplot(discharge, SWEin, data = Blackwood_winterDat, geom="point") +
  theme(axis.text.x = element_text(angle = 25, vjust = 1.0, hjust = 1.0))

qplot(discharge, SnowDepth, data = Blackwood_winterDat, geom="point") +
  theme(axis.text.x = element_text(angle = 25, vjust = 1.0, hjust = 1.0))

qplot(discharge, AirTempCave, data = Blackwood_winterDat, geom="point") +
  theme(axis.text.x = element_text(angle = 25, vjust = 1.0, hjust = 1.0))


##----
## Preform the same for General:
# Restrict date range from flow script, see 'StreamFlowAgg.R' object: 

qwData_Gen

flow_General <- subset(qwData_Gen, Date >= ('2000-10-01') & 
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

# Convert temps to C from snotel script, see 'SNOTEL_agg.R' object: Indi2SNOTEL.Q
RubiconSNOTEL$AirTempCave <- c((RubiconSNOTEL$AirTempF.ave - 32) * (5/9))
RubiconSNOTEL$AirTempCmax <- c((RubiconSNOTEL$AirTempF.max - 32) * (5/9))
RubiconSNOTEL$AirTempCmin <- c((RubiconSNOTEL$AirTempF.min - 32) * (5/9))
summary(RubiconSNOTEL)
# Select relevant columns:
RubiconSNOTEL.Q2 <- subset(RubiconSNOTEL, select=c(Site, Date, PrecipAccum.ave, SnowDepth, SWEin,
                                               AirTempCave, AirTempCmax, AirTempCmin))
summary(RubiconSNOTEL.Q2)

# Left join the 2 data frames together:
General_Dat <- left_join(flow_General.q, RubiconSNOTEL.Q2[c("Date", "PrecipAccum.ave", "SnowDepth", "SWEin",
                                                             "AirTempCave", "AirTempCmax", "AirTempCmin")],
                           by = c("Date" = "Date"))
summary(General_Dat)

# Check to make sure this all makes sense
qplot(WtempC, AirTempCave, data = General_Dat, geom="point") +
  theme(axis.text.x = element_text(angle = 25, vjust = 1.0, hjust = 1.0))

qplot(discharge, PrecipAccum.ave, data = General_Dat, geom="point") +
  theme(axis.text.x = element_text(angle = 25, vjust = 1.0, hjust = 1.0))

# Want to pull out winter months:
# December - March 

library(lubridate)
General_Dat <-transform(General_Dat,
                          month = as.numeric(format(Date, '%m')))

General_winterDat <- subset(General_Dat, month >= 12 | month <= 3)
summary(General_winterDat)

qplot(discharge, PrecipAccum.ave, data = General_winterDat, geom="point") +
  theme(axis.text.x = element_text(angle = 25, vjust = 1.0, hjust = 1.0))

qplot(discharge, SWEin, data = General_winterDat, geom="point") +
  theme(axis.text.x = element_text(angle = 25, vjust = 1.0, hjust = 1.0))

qplot(discharge, SnowDepth, data = General_winterDat, geom="point") +
  theme(axis.text.x = element_text(angle = 25, vjust = 1.0, hjust = 1.0))

qplot(discharge, AirTempCave, data = General_winterDat, geom="point") +
  theme(axis.text.x = element_text(angle = 25, vjust = 1.0, hjust = 1.0))

#Save all data in 1 file:
NRES710_MSM_dat <- rbind(General_Dat, Blackwood_Dat, Ward_Dat, Sagehen_Dat)
summary(NRES710_MSM_dat$month)
summary(General_Dat)
summary(Blackwood_Dat)
summary(Ward_Dat)
summary(Sagehen_Dat)
summary(Blackwood_winterDat)

#write.csv(NRES710_MSM_dat,  paste0(outputDir,"/NRES710_MSM_dat.csv")) # complied data file of all RBR sensors along buoy line

NRES710_MSM_dat <-transform(NRES710_MSM_dat,
                            doy = as.numeric(format(Date, '%j')))

d <- transform(NRES710_MSM_dat,
               year = as.numeric(format(Date, '%Y')),
               week = as.numeric(format(Date, '%U')),
               nmonth = as.numeric(format(Date, '%m')),
               doy = as.numeric(format(Date, '%j')))

# Average all data averaged by week-year
d$weekyr <- paste(d$week, d$year)
summary(d)
d1 <- d %>%
  group_by(GaugeSite, weekyr, year, month, week) %>%
  summarise("WtempC.ave.wyr" = mean(WtempC, na.rm = TRUE), 
            "discharge.ave.wyr"= mean(discharge, na.rm = TRUE),
            "PrecipAccum.ave.wyr"= mean(PrecipAccum.ave, na.rm = TRUE),
            "SnowDepth.ave.wyr"= mean(SnowDepth, na.rm = TRUE),
            "SWEin.ave.wyr"= mean(SWEin, na.rm = TRUE),
            "AirTempC.ave.wyr"= mean(AirTempCave, na.rm = TRUE),
            "AirTempCMax.ave.wyr"= mean(AirTempCmax, na.rm = TRUE),
            "AirTempCMin.ave.wyr"= mean(AirTempCmin, na.rm = TRUE)) 

d2 <- d1 %>%
  group_by(GaugeSite) %>% 
  mutate(
    "Wtemp_delta" = ((WtempC.ave.wyr/lead(WtempC.ave.wyr) - 1) * 100), 
    "discharge_delta" = ((discharge.ave.wyr/lead(discharge.ave.wyr) - 1) * 100),
    "PrecipAccum_delta" = ((PrecipAccum.ave.wyr/lead(PrecipAccum.ave.wyr) - 1) * 100),
    "SnowDepth_delta" = ((SnowDepth.ave.wyr/lead(SnowDepth.ave.wyr) - 1) * 100),
    "SWE_delta" = ((SnowDepth.ave.wyr/lead(SnowDepth.ave.wyr) - 1) * 100),
    "AirtempAve_delta" = ((AirTempC.ave.wyr/lead(AirTempC.ave.wyr) - 1) * 100),
    "AirtempMax_delta" = ((AirTempCMax.ave.wyr/lead(AirTempCMax.ave.wyr) - 1) * 100),
    "AirtempMin_delta" = ((AirTempCMin.ave.wyr/lead(AirTempCMin.ave.wyr) - 1) * 100))


?lag
d2$discharge_delta


p1 <- ggplot(d2, aes(x=week, y=(AirtempAve_delta), colour =as.factor(GaugeSite))) +
  geom_point() + 
  #scale_x_date(date_breaks = "6 month", 
  #             labels=date_format("%b-%Y"),
  #             limits = as.Date(c('2016-01-01','2020-01-01'))) +
  theme_classic() + facet_wrap(~month)

# need to calculate date when SnowDepth.ave.wyr > 0.01
# lets look at data spread
hist(d1$SnowDepth.ave.wyr)
hist(d1$SWEin.ave.wyr)
hist(d1$discharge.ave.wyr)


# GLM to address how correlated year is with day of first

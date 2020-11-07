#NRES701_Dat

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

### add in some rain!
#  a * [tanh(b *( Mean Temp – c))] – d
# Snow Probability = -50 * [tanh(0.4 *( Mean Temp – 1.75))] – 1 
NRES710_MSM_dat$prob_Snow <- -50*(tanh(0.4*(NRES710_MSM_dat$AirTempCave-1.75)))-1
plot(NRES710_MSM_dat$AirTempCave, NRES710_MSM_dat$prob_Snow)

# columns for whether it will rain or snow
NRES710_MSM_dat.q <- NRES710_MSM_dat %>%
  mutate(
    prob_Snow=
      -50*(tanh(0.4*(AirTempCave-1.75)))-1) %>%
  mutate(
    Snow=
      case_when( #may as well add the m in here since your metadata days that flag was used
        prob_Snow<0 & PrecipIncrement > 0 ~ 'rain',
        prob_Snow>0 & PrecipIncrement > 0 ~ 'snow')) %>%
  mutate(
    Snow_bi=
      case_when( #may as well add the m in here since your metadata days that flag was used
        prob_Snow<0 & PrecipIncrement > 0 ~ 0,
        prob_Snow>0 & PrecipIncrement > 0 ~ 1)) 

# Add in date variables 
d <- transform(NRES710_MSM_dat.q,
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
summary(d)

p <- ggplot(d, aes(x=Date, y=prob_Snow, colour =as.factor(Snow_bi), shape =as.factor(GaugeSite))) + 
  geom_point() + 
  theme_classic() + facet_wrap(~wtr_yr)

summary(d)
summary(General_Dat)
summary(Blackwood_Dat)
summary(Ward_Dat)
summary(Sagehen_Dat)
summary(Blackwood_winterDat)

#write.csv(d,  paste0(outputDir,"/NRES710_MSM_dat.csv")) # complied data file of all RBR sensors along buoy line

## ---------------------------
# IIa. Statistical tests: WS differences 

# test for site based differences: Contrib_Drainage_Area, Relief_T, AirTempCave, PrecipAccum.ave
summary(d)



## ---------------------------
# IIb. Statistical tests: SWE and Discharge

# Average all data averaged by week-year
d$weekyr <- paste(d$week, d$wtr_yr)
summary(d)
d1 <- d %>%
  group_by(GaugeSite, weekyr, wtr_yr, nmonth, week, Relief_T, Contrib_Drainage_Area) %>%
  summarise("WtempC.ave.wyr" = mean(WtempC, na.rm = TRUE), 
            "discharge.ave.wyr"= mean(discharge, na.rm = TRUE),
            "PrecipAccum.ave.wyr"= mean(PrecipAccum.aveR, na.rm = TRUE),
            "PrecipIncrem.ave.wyr"= mean(PrecipIncrement, na.rm = TRUE),
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
    "PrecipIncrem_delta" = ((PrecipIncrem.ave.wyr/lead(PrecipIncrem.ave.wyr) - 1) * 100),
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
  theme_classic() + facet_wrap(~nmonth)

# need to calculate date when SnowDepth.ave.wyr > 0.01
# lets look at data spread
hist(d1$SnowDepth.ave.wyr)
hist(d1$SWEin.ave.wyr)
hist(d1$discharge.ave.wyr)


# How does max snow depth or swe correlate to max dishcharge and water temp
# Average all data averaged by week-year
dmax <- d1 %>%
  group_by(GaugeSite, wtr_yr, Relief_T, Contrib_Drainage_Area) %>%
  summarise("WtempC.max.yr" = max(WtempC.ave.wyr, na.rm = TRUE), 
            "discharge.max.yr"= max(discharge.ave.wyr, na.rm = TRUE),
            "PrecipAccum.max.yr"= max(PrecipAccum.ave.wyr, na.rm = TRUE),
            "PrecipIncrem.max.yr"= max(PrecipIncrem.ave.wyr, na.rm = TRUE),
            "SnowDepth.max.yr"= max(SnowDepth.ave.wyr, na.rm = TRUE),
            "SWE.max.yr"= max(SWEin.ave.wyr, na.rm = TRUE),
            "AirTempC.max.yr"= max(AirTempC.ave.wyr, na.rm = TRUE),
            "AirTempCMax.max.yr"= max(AirTempCMax.ave.wyr, na.rm = TRUE)) 

# Remove all -Inf values:
summary(dmax)
is.na(dmax)<-sapply(dmax, is.infinite)
dmax[is.na(dmax)]<-0

#library(PerformanceAnalytics) 
my_cor <- dmax[, c(5:10)] # select the columns with discrete variables only 
chart.Correlation(my_cor, histogram=TRUE, pch=19) # prints the cor plot

# looks like strongest relationship between max Discharge and SWE...
# prelim models: dmax
hist(dmax$discharge.max.wyr) # skewed but not terribly 
hist(log10(dmax$discharge.max.wyr +1))

# model building 
dischargemax.Fullmod <- lmer(discharge.max.wyr ~ scale(SWE.max.wyr) + scale(PrecipAccum.max.wyr) + 
                               scale(Contrib_Drainage_Area) +
                               scale(wtr_yr) + (1|GaugeSite), data = dmax) 
summary(dischargemax.Fullmod) # sig  
hist(residuals(dischargemax.Fullmod)) # pretty normally distributed and best fit model 
r.squaredGLMM(dischargemax.Fullmod)

dischargemax.mod <- lmer(discharge.max.wyr ~ scale(SWE.max.wyr) + scale(PrecipAccum.max.wyr) 
                         + scale(Contrib_Drainage_Area) + (1|GaugeSite), data = dmax) 
summary(dischargemax.mod)

dischargemax.mod1 <- lmer(discharge.max.wyr ~ scale(SWE.max.wyr) + scale(PrecipAccum.max.wyr) 
                         + (1|GaugeSite), data = dmax) 
summary(dischargemax.mod1)

dischargemax.mod2 <- lmer(discharge.max.wyr ~ scale(SWE.max.wyr)
                          + (1|GaugeSite), data = dmax) 
summary(dischargemax.mod2)

AIC(dischargemax.Fullmod, dischargemax.mod, dischargemax.mod1, dischargemax.mod2)


### repeat for averages:
dmean <- d1 %>%
  group_by(GaugeSite, wtr_yr, Relief_T, Contrib_Drainage_Area) %>%
  summarise("WtempC.mean.wyr" = mean(WtempC.ave.wyr, na.rm = TRUE), 
            "discharge.mean.wyr"= mean(discharge.ave.wyr, na.rm = TRUE),
            "PrecipAccum.mean.wyr"= mean(PrecipAccum.ave.wyr, na.rm = TRUE),
            "SnowDepth.mean.wyr"= mean(SnowDepth.ave.wyr, na.rm = TRUE),
            "SWE.mean.wyr"= mean(SWEin.ave.wyr, na.rm = TRUE),
            "AirTempC.mean.wyr"= mean(AirTempC.ave.wyr, na.rm = TRUE),
            "AirTempCMax.mean.wyr"= mean(AirTempCMax.ave.wyr, na.rm = TRUE)) 

# Remove all -Inf values:
summary(dmean)
is.na(dmax)<-sapply(dmax, is.infinite)
dmax[is.na(dmax)]<-0

my_cor <- dmean[, c(5:9)] # select the columns with discrete variables only 
chart.Correlation(my_cor, histogram=TRUE, pch=19) # prints the cor plot


dischargemean.Fullmod <- lmer(WtempC.mean.wyr ~ scale(SWE.mean.wyr) + scale(PrecipAccum.mean.wyr) + 
                               scale(Contrib_Drainage_Area) + scale(Relief_T) +
                               scale(wyear) + (1|GaugeSite), data = dmean) 
summary(dischargemean.Fullmod) # sig  
hist(residuals(dischargemean.Fullmod)) # Max looks much better than mean. 

## SWE relationship not detected with current WS characteristics Relief_T or Contrib_Drainage_Area
# model building 
SWEmax.Fullmod <- lmer(SWE.max.wyr ~ scale(Relief_T) + scale(Contrib_Drainage_Area) + 
                         (1|GaugeSite), data = dmax) 
summary(SWEmax.Fullmod)  
hist(residuals(SWEmax.Fullmod)) # Not sig 
r.squaredGLMM(SWEmax.Fullmod)

## ---------------------------
# IIc. Statistical tests: R-O-S events
summary(d)

# Maybe think about this as occupancy 
# lets start by sub setting for winter months only 
d.winter <- d %>%
  subset(nmonth >= 12 | nmonth <= 3)  %>%
  subset(wtr_yr < 2020)
summary(d.winter)

d2.winter <- d.winter %>%
  group_by(wtr_yr) %>% 
  mutate(wtr_day = (as.integer(difftime(Date,ymd(paste0(wtr_yr - 1 ,'-09-30')), units = "days"))))
?ymd

summary(d2.winter)

p2 <- ggplot(d2.winter, aes(x=wtr_day, y=PrecipAccum.aveR, colour =(Snow), shape =(GaugeSite))) + 
  geom_point() +  
  theme_classic() + 
  labs(x ="Day of year (water year)", y = "Precip Accumulation (in)") +
  facet_wrap(~wtr_yr)

p3 <- ggplot(d2.winter, aes(x=wtr_day, y=PrecipIncrement, colour =(Snow), shape =(GaugeSite))) + 
  geom_point() +  
  theme_classic() + 
  labs(x ="Day of year (water year)", y = "Precip Accumulation (in)") +
  facet_wrap(~wtr_yr)

#ggsave(paste0(outputDir,"/MSM_SnowProbabilityRoughPrecp.pdf"), p2, scale = 1.5, width = 18, height =12, units = c("cm"), dpi = 500)



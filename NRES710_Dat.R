## ---------------------------
## NRES701 Supplemental Code
##
## Author: Kelly A. Loria
## Date Created: 2020-11-11
## Email: kelly.loria@nevada.unr.edu
##
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
library(PerformanceAnalytics) 
se <- function(x) sd(x) / sqrt(length(x))
## ---------------------------
# I. Read in data: df
df <- read.csv(paste0(inputDir, "/NRES710_MSM_Wdat.csv"), header=T, sep = ',')
# Make sure date is formatted correctly: 
tmpDateFormat<-"%Y-%m-%d"
tmp1date<-as.Date(df$Date,format=tmpDateFormat)
# Keep the new dates only if they all converted correctly
if(length(tmp1date) == length(tmp1date[!is.na(tmp1date)])){df$Date <- tmp1date } else {print("Date conversion failed for dt1$date. Please inspect the data and do the date conversion yourself.")}                                                                    
rm(tmpDateFormat,tmp1date) 
summary(df)

# Double check column organization to make sure data is true
datcheck <- subset(df, Snow_0NY == 1) # looks good. 
summary(datcheck)
range(datcheck$Snow)

# p <- ggplot(df, aes(x=wtr_day, y=PrecipIncrement, colour =as.factor(ROS_0NY), shape =as.factor(GaugeSite))) + 
#   geom_point() + 
#   theme_classic() + facet_wrap(~wtr_yr)
# 
# mod <- lmer( discharge ~ scale(ROS_0NY) + scale(PrecipIncrement) + scale(wtr_yr) + (1|GaugeSite), data =df) 
# summary(mod) # sig  
# hist(residuals(mod)) # likely much more zero inflated... need to figureout how to standardize with lag

# p <- ggplot(dq, aes(x=wtr_day, y=prob_Snow, colour =as.factor(Snow_bi), shape =as.factor(GaugeSite))) + 
#   geom_point() + 
#   theme_classic() + facet_wrap(~wtr_yr)
# 
# p <- ggplot(dq, aes(x=Snow_bi, y=prob_Snow, colour =as.factor(Snow_bi), shape =as.factor(GaugeSite))) + 
#   geom_point() + 
#   theme_classic() + facet_wrap(~wtr_yr)
# 
# p <- ggplot(dq, aes(x=wtr_day, y=PrecipIncrement, colour =as.factor(ROS), shape =as.factor(GaugeSite))) + 
#   geom_point() + 
#   theme_classic() + facet_wrap(~wtr_yr)


## ---------------------------
# II. Statistical tests: is there a relationship between SWE and Discharge?
summary(df)

df_ave <- df %>%
  # Create grouping variable for water year "week-year"
  mutate(
    w_wkyr= paste(df$week, df$w_wkyr)) %>%
  # Average all data averaged by week-year
  group_by(GaugeSite, w_wkyr, wtr_yr, nmonth, Relief_T, Contrib_Drainage_Area) %>%
  summarise("WtempC.ave" = mean(WtempC, na.rm = TRUE), 
            "discharge.ave"= mean(discharge, na.rm = TRUE),
            "PrecipAccum.ave"= mean(PrecipAccum.aveR, na.rm = TRUE),
            "PrecipIncrem.ave"= mean(PrecipIncrement, na.rm = TRUE),
            "SnowDepth.ave"= mean(SnowDepth, na.rm = TRUE),
            "SWEin.ave"= mean(SWEin, na.rm = TRUE),
            "AirTempC.ave"= mean(AirTempCave, na.rm = TRUE),
            "AirTempCMax.ave"= mean(AirTempCmax, na.rm = TRUE),
            "AirTempCMin.ave"= mean(AirTempCmin, na.rm = TRUE),
            "AirTempCMin.ave"= mean(AirTempCmin, na.rm = TRUE),
            "WeeklyLikelySnow"= mean(prob_snowbi, na.rm = TRUE),
            "WeeklyLikelyROS"= sum(ROS_0NY, na.rm = TRUE)) 

# d2 <- d1 %>%
#   group_by(GaugeSite) %>% 
#   mutate(
#     "Wtemp_delta" = ((WtempC.ave.wyr/lead(WtempC.ave.wyr) - 1) * 100), 
#     "discharge_delta" = ((discharge.ave.wyr/lead(discharge.ave.wyr) - 1) * 100),
#     "PrecipAccum_delta" = ((PrecipAccum.ave.wyr/lead(PrecipAccum.ave.wyr) - 1) * 100),
#     "PrecipIncrem_delta" = ((PrecipIncrem.ave.wyr/lead(PrecipIncrem.ave.wyr) - 1) * 100),
#     "SnowDepth_delta" = ((SnowDepth.ave.wyr/lead(SnowDepth.ave.wyr) - 1) * 100),
#     "SWE_delta" = ((SnowDepth.ave.wyr/lead(SnowDepth.ave.wyr) - 1) * 100),
#     "AirtempAve_delta" = ((AirTempC.ave.wyr/lead(AirTempC.ave.wyr) - 1) * 100),
#     "AirtempMax_delta" = ((AirTempCMax.ave.wyr/lead(AirTempCMax.ave.wyr) - 1) * 100),
#     "AirtempMin_delta" = ((AirTempCMin.ave.wyr/lead(AirTempCMin.ave.wyr) - 1) * 100))

# Look at data spread:
hist(df_ave$WtempC.ave)
hist(df_ave$discharge.ave)
hist(df_ave$SnowDepth.ave)
hist(df_ave$PrecipIncrem.ave)
hist(df_ave$SWEin.ave)
hist(df_ave$AirTempC.ave)
# Lot of expected 0s from the seasonality of snow (not being observed in summer months)

# Lets start by buffing out variables to the annual scale: MAX
dmax <- df_ave %>%
  group_by(GaugeSite, wtr_yr, Relief_T, Contrib_Drainage_Area) %>%
  summarise("WtempC.max.yr" = max(WtempC.ave, na.rm = TRUE), 
            "discharge.max.yr"= max(discharge.ave, na.rm = TRUE),
            "PrecipAccum.max.yr"= max(PrecipAccum.ave, na.rm = TRUE),
            "PrecipIncrem.max.yr"= max(PrecipIncrem.ave, na.rm = TRUE),
            "SnowDepth.max.yr"= max(SnowDepth.ave, na.rm = TRUE),
            "SWE.max.yr"= max(SWEin.ave, na.rm = TRUE),
            "AirTempC.max.yr"= max(AirTempC.ave, na.rm = TRUE),
            "AirTempCMax.max.yr"= max(AirTempCMax.ave, na.rm = TRUE),
            "LikelySnow_max"= sum(WeeklyLikelySnow, na.rm = TRUE),
            "LikelyROS_max"= sum(WeeklyLikelyROS, na.rm = TRUE)) 

# Remove all -Inf values:
summary(dmax)
is.na(dmax)<-sapply(dmax, is.infinite)
dmax[is.na(dmax)]<-0

# Get an idea of transformed data shape.
my_cor <- dmax[, c(5:14)] # select the columns with discrete variables only 
chart.Correlation(my_cor, histogram=TRUE, pch=19) # prints the cor plot

# looks like strongest relationship between max Discharge and SWE...
# prelim models: dmax
hist(dmax$discharge.max.yr) # skewed but not terribly 
hist(dmax$PrecipAccum.max.yr)
hist(dmax$PrecipIncrem.max.yr)
hist(dmax$SWE.max.yr)
hist(dmax$LikelySnow_max)
hist(dmax$LikelyROS_max)
summary(dmax)
# Model building: Full model that isn't over correlated
Flowmax.Fullmod <- lmer(discharge.max.yr ~ scale(SWE.max.yr) + 
                               scale(Contrib_Drainage_Area) + scale(Relief_T) + scale(AirTempCMax.max.yr) +
                               scale(wtr_yr) + (1|GaugeSite), data = dmax) 
summary(Flowmax.Fullmod) # sig  
hist(residuals(Flowmax.Fullmod)) # pretty normally distributed and best fit model 
r.squaredGLMM(Flowmax.Fullmod) # r2c = 0.767


Flowmax.mod1 <- lmer(discharge.max.yr ~ scale(SWE.max.yr) + scale(AirTempCMax.max.yr) +
                               scale(wtr_yr) + (1|GaugeSite), data = dmax) 
summary(Flowmax.mod1) # sig  
hist(residuals(Flowmax.mod1)) # pretty normally distributed and best fit model 
r.squaredGLMM(Flowmax.mod1)

Flowmax.mod2 <- lmer(discharge.max.yr ~ scale(SWE.max.yr) + scale(AirTempCMax.max.yr) + 
                       (1|GaugeSite), data = dmax) 
summary(Flowmax.mod2) # sig  
hist(residuals(Flowmax.mod2)) # pretty normally distributed and best fit model 
r.squaredGLMM(Flowmax.mod2)


Flowmax.mod3 <- lmer(discharge.max.yr ~ scale(SWE.max.yr) + 
                       (1|GaugeSite), data = dmax) 
summary(Flowmax.mod3) # sig  
hist(residuals(Flowmax.mod3)) # pretty normally distributed and best fit model 
r.squaredGLMM(Flowmax.mod3)

AIC(Flowmax.Fullmod, Flowmax.mod1, Flowmax.mod2, Flowmax.mod3)




## ---------------------------
# III. Statistical tests: is there a relationship between flow and ROS?
# Again buff out variables to the annual scale: MAX -- 
    # not great results maybe need to figure out when ROS is greater than average ROS
# summary(dmax)
# 
# # Model building: Full model that isn't over correlated
# ROSmax.fullmod <- lmer(LikelyROS_max ~ scale(discharge.max.yr) + 
#                          scale(Contrib_Drainage_Area) + scale(Relief_T) + scale(AirTempCMax.max.yr) +
#                          scale(SnowDepth.max.yr) + scale(PrecipIncrem.max.yr) +
#                          scale(wtr_yr) + (1|GaugeSite), data = dmax) 
# summary(ROSmax.fullmod) # sig  
# hist(residuals(ROSmax.fullmod)) # pretty normally distributed and best fit model 
# r.squaredGLMM(ROSmax.fullmod) # r2c = 0.767
# 
# 
# # Model building: Full model that isn't over correlated
# ROSmax.fullmod <- lmer(LikelyROS_max ~ scale(wtr_yr) + (1|GaugeSite), data = dmax) 
# summary(ROSmax.fullmod) # sig  
# hist(residuals(ROSmax.fullmod)) # pretty normally distributed and best fit model 
# r.squaredGLMM(ROSmax.fullmod) # r2c = 0.767


# try to create a df for extreme years 
df_ave2 <- df %>%
  # Create grouping variable for water year "week-year + month-year"
  mutate(
    w_wkyr= paste(df$week, df$wtr_yr)) %>%
  mutate(
    w_mhyr= paste(df$nmonth, df$wtr_yr)) %>%
  # Average all data averaged by week-year
  group_by(GaugeSite, wtr_yr, Relief_T, Contrib_Drainage_Area) %>%
  ##### YOU ARE HERE ###
  summarise("WtempC.ave" = mean(WtempC, na.rm = TRUE), 
            "discharge.ave"= mean(discharge, na.rm = TRUE),
            "PrecipAccum.ave"= mean(PrecipAccum.aveR, na.rm = TRUE),
            "PrecipIncrem.ave"= mean(PrecipIncrement, na.rm = TRUE),
            "SnowDepth.ave"= mean(SnowDepth, na.rm = TRUE),
            "SWEin.ave"= mean(SWEin, na.rm = TRUE),
            "AirTempC.ave"= mean(AirTempCave, na.rm = TRUE),
            "AirTempCMax.ave"= mean(AirTempCmax, na.rm = TRUE),
            "AirTempCMin.ave"= mean(AirTempCmin, na.rm = TRUE),
            "AirTempCMin.ave"= mean(AirTempCmin, na.rm = TRUE),
            "LikelySnow"= sum(prob_snowbi, na.rm = TRUE),
            "LikelyROS"= sum(ROS_0NY, na.rm = TRUE))


dmax <- df_ave %>%
  group_by(GaugeSite, wtr_yr, Relief_T, Contrib_Drainage_Area) %>%
  summarise("WtempC.max.yr" = max(WtempC.ave, na.rm = TRUE), 
            "discharge.max.yr"= max(discharge.ave, na.rm = TRUE),
            "PrecipAccum.max.yr"= max(PrecipAccum.ave, na.rm = TRUE),
            "PrecipIncrem.max.yr"= max(PrecipIncrem.ave, na.rm = TRUE),
            "SnowDepth.max.yr"= max(SnowDepth.ave, na.rm = TRUE),
            "SWE.max.yr"= max(SWEin.ave, na.rm = TRUE),
            "AirTempC.max.yr"= max(AirTempC.ave, na.rm = TRUE),
            "AirTempCMax.max.yr"= max(AirTempCMax.ave, na.rm = TRUE),
            "Snow_max"= sum(WeeklyLikelySnow, na.rm = TRUE),
            "ROS_max"= sum(WeeklyLikelyROS, na.rm = TRUE)) 

summary(dmax)


ROSmax.fullmod <- lmer(LikelyROS ~ scale(discharge.ave) +
                         scale(Contrib_Drainage_Area) + scale(Relief_T) + scale(AirTempCMax.ave) +
                         scale(SnowDepth.ave) + scale(PrecipAccum.ave) +
                         scale(wtr_yr) + (1|GaugeSite), data = df_ave2)
summary(ROSmax.fullmod) # sig
hist(residuals(ROSmax.fullmod)) # pretty normally distributed and best fit model
r.squaredGLMM(ROSmax.fullmod) # r2c = 0.767


ROSmax.fullmod <- lmer(discharge.ave ~ scale(LikelyROS) + (1|GaugeSite), data = df_ave2)
summary(ROSmax.fullmod) # sig
hist(residuals(ROSmax.fullmod)) # pretty normally distributed and best fit model
r.squaredGLMM(ROSmax.fullmod) 





plot(df_ave2$discharge.ave, df_ave2$LikelyROS)

p3 <- ggplot(df_ave2, aes(x=discharge.ave, y=LikelyROS, shape =(GaugeSite))) + 
  geom_point() +  
  theme_classic() + 
  labs(x ="Day of year (water year)", y = "Precip Accumulation (in)") +
  facet_wrap(~wtr_yr)


ROSmax.fullmod <- lmer(discharge.max.yr ~ scale(ROS_max) + (1|GaugeSite), data = dmax)
summary(ROSmax.fullmod) # sig
hist(residuals(ROSmax.fullmod)) # pretty normally distributed and best fit model
r.squaredGLMM(ROSmax.fullmod) 

plot(dmax$discharge.max.yr, dmax$ROS_max)


# limit for winter only
dmax_winter <- df_ave %>%
    subset(nmonth > 1 | nmonth < 5) %>%
  group_by(GaugeSite, wtr_yr, Relief_T, Contrib_Drainage_Area) %>%
  summarise("WtempC.max.yr" = max(WtempC.ave, na.rm = TRUE), 
            "discharge.max.yr"= max(discharge.ave, na.rm = TRUE),
            "PrecipAccum.max.yr"= max(PrecipAccum.ave, na.rm = TRUE),
            "PrecipIncrem.max.yr"= max(PrecipIncrem.ave, na.rm = TRUE),
            "SnowDepth.max.yr"= max(SnowDepth.ave, na.rm = TRUE),
            "SWE.max.yr"= max(SWEin.ave, na.rm = TRUE),
            "AirTempC.max.yr"= max(AirTempC.ave, na.rm = TRUE),
            "AirTempCMax.max.yr"= max(AirTempCMax.ave, na.rm = TRUE),
            "Snow_max"= sum(WeeklyLikelySnow, na.rm = TRUE),
            "ROS_max"= sum(WeeklyLikelyROS, na.rm = TRUE)) 

summary(dmax_winter)

is.na(dmax_winter)<-sapply(dmax_winter, is.infinite)
dmax_winter[is.na(dmax_winter)]<-0


## Looks alright maybe lets look at it on the raw data.. and maybe weekly
ROSmax.fullmod <- lmer(discharge.max.yr ~ scale(ROS_max)+ (1|GaugeSite), data = dmax_winter)
summary(ROSmax.fullmod) # sig
hist(residuals(ROSmax.fullmod)) # pretty normally distributed and best fit model
r.squaredGLMM(ROSmax.fullmod) 

ROSmax.fullmod <- lmer(discharge.max.yr ~ scale(ROS_max)+ scale(PrecipIncrem.max.yr) + 
                         scale(ROS_max*PrecipIncrem.max.yr)+ (1|GaugeSite), data = dmax_winter)
summary(ROSmax.fullmod) # sig
hist(residuals(ROSmax.fullmod)) # pretty normally distributed and best fit model
r.squaredGLMM(ROSmax.fullmod) 
range(na.omit(dmax_winter$ROS_max))

plot(dmax_winter$discharge.max.yr, dmax_winter$ROS_max)

# Is there a stronger snow signal 
SOSmax.fullmod <- lmer(discharge.max.yr ~ scale(Snow_max)+ (1|GaugeSite), data = dmax_winter)
summary(SOSmax.fullmod) # sig
hist(residuals(SOSmax.fullmod)) # pretty normally distributed and best fit model
r.squaredGLMM(SOSmax.fullmod) 

## ---------------------------
# IIc. Statistical tests: R-O-S events
df_winter <- (subset(df, nmonth > 11 | nmonth < 5)) 
summary(df_winter)

# probably dealing with zero inflation lets buff up to winter week maximums. 
ROSmax.fullmod <- lmer(discharge ~ scale(ROS_0NY)+ (1|GaugeSite), data = df_winter)
summary(ROSmax.fullmod) 
hist(residuals(ROSmax.fullmod)) # pretty normally distributed and best fit model
r.squaredGLMM(ROSmax.fullmod) 
range(na.omit(df_winter$ROS_0NY))

ROSmax.fullmod <- lmer(discharge ~ scale(ROS2_YN)+ (1|GaugeSite), data = df_winter)
summary(ROSmax.fullmod) 
hist(residuals(ROSmax.fullmod)) # pretty normally distributed and best fit model
r.squaredGLMM(ROSmax.fullmod) 
range(na.omit(df_winter$ROS_0NY))

ROSmax.fullmod <- lmer(discharge ~ scale(ROS3_YN)+ (1|GaugeSite), data = df_winter)
summary(ROSmax.fullmod) 
hist(residuals(ROSmax.fullmod)) # pretty normally distributed and best fit model
r.squaredGLMM(ROSmax.fullmod) 
range(na.omit(df_winter$ROS_0NY))


ROSmax.fullmod <- lmer(discharge ~ scale(ROS_temp1)+ (1|GaugeSite), data = df_winter)
summary(ROSmax.fullmod) 
hist(residuals(ROSmax.fullmod)) # pretty normally distributed and best fit model
r.squaredGLMM(ROSmax.fullmod) 
range(na.omit(df_winter$ROS_0NY))

df_winter_ave <- df_winter %>%
  # Create grouping variable for water year "week-year"
  # Create grouping variable for water year "week-year + month-year"
  mutate(
    w_wkyr= paste(df_winter$week, df$wtr_yr)) %>%
  mutate(
    w_mhyr= paste(df_winter$nmonth, df$wtr_yr)) %>%
  # Average all data averaged by week-year
  group_by(GaugeSite, w_mhyr, wtr_yr, nmonth, Relief_T, Contrib_Drainage_Area) %>%
  summarise("WtempC.max" = max(WtempC, na.rm = TRUE), 
            "discharge.max"= max(discharge, na.rm = TRUE),
            "PrecipAccum.max"= max(PrecipAccum.aveR, na.rm = TRUE),
            "PrecipIncrem.max"= max(PrecipIncrement, na.rm = TRUE),
            "SnowDepth.max"= max(SnowDepth, na.rm = TRUE),
            "SWEin.max"= max(SWEin, na.rm = TRUE),
            "AirTempC.max"= max(AirTempCave, na.rm = TRUE),
            "AirTempCMax.max"= max(AirTempCmax, na.rm = TRUE),
            "AirTempCMin.max"= max(AirTempCmin, na.rm = TRUE),
            "AirTempCMin.max"= max(AirTempCmin, na.rm = TRUE),
            "WeeklyLikelySnow"= sum(prob_snowbi, na.rm = TRUE),
            "WeeklyLikelyROS"= sum(ROS_0NY, na.rm = TRUE)) 

# Remove all -Inf values:
summary(df_winter_ave)
is.na(df_winter_ave)<-sapply(df_winter_ave, is.infinite)
df_winter_ave[is.na(df_winter_ave)]<-0


# probably dealing with zero inflation lets buff up to winter week maximums. 
ROSmax.fullmod <- lmer(discharge.max ~ scale(WeeklyLikelyROS)+ scale(wtr_yr)+ scale(nmonth)+(1|GaugeSite), data = df_winter_ave)
summary(ROSmax.fullmod) 
hist(residuals(ROSmax.fullmod)) # pretty normally distributed and best fit model
r.squaredGLMM(ROSmax.fullmod) 



## ---------------------------
# IIc. Statistical tests: R-O-S events
summary(df)

# Maybe think about this as occupancy 
# lets start by sub setting for winter months only 
#dq.winter <- dq %>%
#  subset(nmonth >= 12 | nmonth <= 3)  %>%
#  subset(wtr_yr < 2020)
#summary(dq.winter)

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

p4 <- ggplot(dq, aes(x=discharge, y=ROS, colour =(Snow), shape =(GaugeSite))) + 
  geom_point() +  
  theme_classic() + 
  #labs(x ="Day of year (water year)", y = "Precip Accumulation (in)") +
  facet_wrap(~GaugeSite)

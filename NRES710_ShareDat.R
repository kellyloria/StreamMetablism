## ---------------------------
## NRES701 Supplemental Code
##
## Author: Kelly A. Loria
## Date Created: 2020-11-17
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
library(car)
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

## ---------------------------
# IIa. Statistical tests: What parameters best describe the relationship between snow pack and discharge?
    # df is in daily obs for 20 years so some data gaps exist.
    # instead of infilling I will average at the week level for each year.

df_ave <- df %>%
  # Create grouping variable for water year "week-year"
  mutate(w_wkyr= paste(df$week, df$wtr_yr)) %>%
  # Average all data averaged by week-year
  group_by(GaugeSite, w_wkyr, wtr_yr, nmonth, Relief_T, Contrib_Drainage_Area, SNOTELSite) %>%
  summarise("WtempC" = mean(WtempC, na.rm = TRUE), 
            "discharge"= mean(discharge, na.rm = TRUE),
            "PrecipAccum"= mean(PrecipAccum.aveR, na.rm = TRUE),
            "PrecipIncrem"= mean(PrecipIncrement, na.rm = TRUE),
            "SnowDepth"= mean(SnowDepth, na.rm = TRUE),
            "SWEin"= mean(SWEin, na.rm = TRUE),
            "AirTempC"= mean(AirTempCave, na.rm = TRUE),
            "AirTempCMax"= mean(AirTempCmax, na.rm = TRUE),
            "AirTempCMin"= mean(AirTempCmin, na.rm = TRUE),
            "sumROS"= sum(ROS2_YN, na.rm = TRUE)) 


## ---------------------------
# group by mean annually: _AveA
df_AveA <- df_ave %>%
  group_by(GaugeSite, wtr_yr, Relief_T, Contrib_Drainage_Area, SNOTELSite) %>%
  summarise("WtempC" = mean(WtempC, na.rm = TRUE), 
            "discharge"= mean(discharge, na.rm = TRUE),
            "PrecipAccum"= mean(PrecipAccum, na.rm = TRUE),
            "PrecipIncrem"= mean(PrecipIncrem, na.rm = TRUE),
            "SnowDepth"= mean(SnowDepth, na.rm = TRUE),
            "SWEin"= mean(SWEin, na.rm = TRUE),
            "AirTempC"= mean(AirTempCMax, na.rm = TRUE)) 

# remove -inf values 
summary(df_AveA)
is.na(df_AveA)<-sapply(df_AveA, is.infinite)
df_AveA[is.na(df_AveA)]<-0


## ---------------------------
# group by max annually: _MaxA
df_MaxA <- df_ave %>%
  subset(nmonth > 11 | nmonth < 5) %>%
  group_by(GaugeSite, wtr_yr, Relief_T, Contrib_Drainage_Area, SNOTELSite) %>%
  summarise("WtempC" = max(WtempC, na.rm = TRUE), 
            "discharge"= max(discharge, na.rm = TRUE),
            "PrecipAccum"= max(PrecipAccum, na.rm = TRUE),
            "PrecipIncrem"= max(PrecipIncrem, na.rm = TRUE),
            "SnowDepth"= max(SnowDepth, na.rm = TRUE),
            "SWEin"= max(SWEin, na.rm = TRUE),
            "AirTempC"= max(AirTempCMax, na.rm = TRUE),
            "sumROS"= sum(sumROS, na.rm = TRUE)) 

# remove -inf values 
summary(df_MaxA)
is.na(df_MaxA)<-sapply(df_MaxA, is.infinite)
df_MaxA[is.na(df_MaxA)]<-0


## ---------------------------
# IIb. Statistical tests: lm: y ~ x
# 1. discharge ~ SWE 
# 2. discharge ~ snow depth
# 3. discharge ~ precip accumulation
# 4. discharge ~ precip increment 
# 5. discharge ~ AirTemp 
# 6. discharge ~ Water year  

# Are these relationships best explained by df_MaxA or df_AveA
summary(df_MaxA)
summary(df_AveA)
    # *NOTE: Slopes were more extreme for df_MaxA so only shown here.


#layout(matrix(1:4,2,byrow = T))

# 1. discharge ~ SWE 
lm_MaxA1 <- lm(discharge ~  + scale(SWEin), data = df_MaxA) 
summary(lm_MaxA1) # sig steeper slope than ave. 
hist(residuals(lm_MaxA1)) # fairly normal
#plot(lm_MaxA1) # pretty normally distributed and best fit model 

# 2. discharge ~ snow depth
lm_MaxA2 <- lm(discharge ~  + scale(SnowDepth), data = df_MaxA) 
summary(lm_MaxA2) # sig . 
hist(residuals(lm_MaxA2)) # some skew
#plot(lm_MaxA2) 

# 3. discharge ~ precip accumulation
lm_MaxA3 <- lm(discharge ~ scale(PrecipAccum), data = df_MaxA) 
summary(lm_MaxA3) # sig  
hist(residuals(lm_MaxA3)) # normal
#plot(lm_MaxA3) #

# 4. discharge ~ precip increment 
lm_MaxA4 <- lm(discharge ~  scale(PrecipIncrem), data = df_MaxA) 
summary(lm_MaxA4) # sig  
hist(residuals(lm_MaxA4)) # more more than precip accum.
#plot(lm_MaxA4)

# 5. discharge ~ AirTemp 
lm_MaxA5 <- lm(discharge ~  scale(AirTempC), data = df_MaxA) 
summary(lm_MaxA5) # sig  
hist(residuals(lm_MaxA5)) # scew
#plot(lm_MaxA5)

lm_MaxA6 <- lm(discharge ~  scale(wtr_yr), data = df_MaxA) 
summary(lm_MaxA6) # NOT sig  
hist(residuals(lm_MaxA6)) # also skew.
#plot(lm_MaxA6)

# All parameters appear to be related to discharge aside from Water year. 
# So need to see which predictors we could include together in a model

## ---------------------------
# Correlation of predictors for glmm model selection:
  # Get an idea of transformed data shape and level of correlation
dim(df_MaxA)
my_cor <- df_MaxA[, c(6:12)] # select the columns with discrete variables only 
chart.Correlation(my_cor, histogram=TRUE, pch=19) # prints the cor plot
    # SWE and precip increment are most strongly correlated as predictors so want to include in glmm
    # Snow depth, precip accum, and precip increment are all <0.65 related so will avoid including in same glmm.

## ---------------------------
# II. Trying to figure out the scale: Annual, weekly, daily?
    # Statistical tests: lmer: y ~ x + (1|Gauge Site) as explained by df_MaxA 

# Annual scale:
glmm_MaxGlobal2 <- lmer(discharge ~  scale(SWEin) + 
                          scale(AirTempC) + 
                          scale(PrecipIncrem) + 
                          scale(sumROS) +
                          (1|GaugeSite) + 
                          (1|SNOTELSite), data = df_MaxA)
summary(glmm_MaxGlobal2) 
hist(residuals(glmm_MaxGlobal2))
r.squaredGLMM(glmm_MaxGlobal2) 
vif(glmm_MaxGlobal2) # variance looks equally shared among predictors 

GSVar <- 282.87
SLVar <- 61.02
ResVar <- 970.51
TotalVar <- (GSVar + SLVar + ResVar)

((GSVar + SLVar)/TotalVar) *100
(GSVar/TotalVar) *100

df_delete <- df %>%
  subset(nmonth > 11 | nmonth < 5) 
summary(df_delete)

sd(na.omit(df_delete$SWEin))
mean(na.omit(df_delete$PrecipIncrement))


sd(na.omit(df_delete$AirTempCave))



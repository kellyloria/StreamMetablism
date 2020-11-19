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


## ---------------------------
# IIa. Statistical tests: What parameters best describe the relationship between snow pack and discharge?
    # df is in daily obs for 20 so some data gaps exist.
    # un-tested thought but could (1) keep discharge data separate then left join winter based mean and max 
    # or (2) only look at these transformed/summarized data for winter

df_ave <- df %>%
  # Create grouping variable for water year "week-year"
  mutate(
    w_wkyr= paste(df$week, df$wtr_yr)) %>%
  # Average all data averaged by week-year
  group_by(GaugeSite, w_wkyr, wtr_yr, nmonth, Relief_T, Contrib_Drainage_Area) %>%
  summarise("WtempC" = mean(WtempC, na.rm = TRUE), 
            "discharge"= mean(discharge, na.rm = TRUE),
            "PrecipAccum"= mean(PrecipAccum.aveR, na.rm = TRUE),
            "PrecipIncrem"= mean(PrecipIncrement, na.rm = TRUE),
            "SnowDepth"= mean(SnowDepth, na.rm = TRUE),
            "SWEin"= mean(SWEin, na.rm = TRUE),
            "AirTempC"= mean(AirTempCave, na.rm = TRUE),
            "AirTempCMax"= mean(AirTempCmax, na.rm = TRUE),
            "AirTempCMin"= mean(AirTempCmin, na.rm = TRUE)) 


## ---------------------------
# group by mean annually: _AveA
df_AveA <- df_ave %>%
  group_by(GaugeSite, wtr_yr, Relief_T, Contrib_Drainage_Area) %>%
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

# Get an idea of transformed data shape and level of correlation
dim(df_AveA)
my_cor <- df_AveA[, c(5:11)] # select the columns with discrete variables only 
chart.Correlation(my_cor, histogram=TRUE, pch=19) # prints the cor plot



## ---------------------------
# group by max annually: _MaxA
df_MaxA <- df_ave %>%
  group_by(GaugeSite, wtr_yr, Relief_T, Contrib_Drainage_Area) %>%
  summarise("WtempC" = max(WtempC, na.rm = TRUE), 
            "discharge"= max(discharge, na.rm = TRUE),
            "PrecipAccum"= max(PrecipAccum, na.rm = TRUE),
            "PrecipIncrem"= max(PrecipIncrem, na.rm = TRUE),
            "SnowDepth"= max(SnowDepth, na.rm = TRUE),
            "SWEin"= max(SWEin, na.rm = TRUE),
            "AirTempC"= max(AirTempCMax, na.rm = TRUE)) 

# remove -inf values 
summary(df_MaxA)
is.na(df_MaxA)<-sapply(df_MaxA, is.infinite)
df_MaxA[is.na(df_MaxA)]<-0

# Get an idea of transformed data shape and level of correlation
dim(df_MaxA)
my_cor <- df_MaxA[, c(5:11)] # select the columns with discrete variables only 
chart.Correlation(my_cor, histogram=TRUE, pch=19) # prints the cor plot



## ---------------------------
# IIb. Statistical tests: lm: y ~ x
    # 1. discharge ~ SWE 
    # 2. discharge ~ snow depth
    # 3. discharge ~ precip accumulation
    # 4. discharge ~ AirTemp 

 # Are these relationships best explained by df_MaxA or df_AveA
summary(df_MaxA)
summary(df_AveA)

layout(matrix(1:4,2,byrow = T))

# 1. discharge ~ SWE 
lm_AveA1 <- lm(discharge ~ scale(SWEin), data = df_AveA) 
summary(lm_AveA1) # sig  
hist(residuals(lm_AveA1))
plot(lm_AveA1) # pretty normally distributed and best fit model 

# 2. discharge ~ snow depth
lm_AveA2 <- lm(discharge ~ scale(SnowDepth), data = df_AveA) 
summary(lm_AveA2) # sig  
hist(residuals(lm_AveA2))
plot(lm_AveA2) # 

# 3. discharge ~ precip accumulation
lm_AveA3 <- lm(discharge ~ scale(PrecipAccum), data = df_AveA) 
summary(lm_AveA3) # sig  
hist(residuals(lm_AveA3))
plot(lm_AveA3) #

# 4. discharge ~ AirTemp 
lm_AveA4 <- lm(discharge ~  scale(AirTempC), data = df_AveA) 
summary(lm_AveA4) # sig  
hist(residuals(lm_AveA4))
plot(lm_AveA4) #



# 1. discharge ~ SWE 
lm_MaxA1 <- lm(discharge ~  + scale(SWEin), data = df_MaxA) 
summary(lm_MaxA1) # sig steeper slope than ave. 
hist(residuals(lm_MaxA1))
plot(lm_MaxA1) # pretty normally distributed and best fit model 

# 2. discharge ~ snow depth
lm_MaxA2 <- lm(discharge ~  + scale(SnowDepth), data = df_MaxA) 
summary(lm_MaxA2) # sig steeper slope than ave. 
hist(residuals(lm_MaxA2))
plot(lm_MaxA2) # pretty normally distributed and best fit model 

# 3. discharge ~ precip accumulation
lm_MaxA3 <- lm(discharge ~ scale(PrecipAccum), data = df_MaxA) 
summary(lm_MaxA3) # sig  
hist(residuals(lm_MaxA3))
plot(lm_MaxA3) #

# 4. discharge ~ AirTemp 
lm_MaxA4 <- lm(discharge ~  scale(AirTempC), data = df_MaxA) 
summary(lm_MaxA4) # sig  
hist(residuals(lm_MaxA4))
plot(lm_MaxA4)

# Discharge slopes are more extreme for df_MaxA


## ---------------------------
# IIc. Statistical tests: lmer: y ~ x + (1|Gauge Site) as explained by df_MaxA 

glmm_MaxGlobal <- lmer(discharge ~  + scale(SWEin) + 
                         scale(WtempC) + 
                         scale(AirTempC) + 
                         scale(PrecipIncrem) + 
                         scale(wtr_yr) +
                         (1|GaugeSite), data = df_MaxA)
summary(glmm_MaxGlobal) 
hist(residuals(glmm_MaxGlobal))
r.squaredGLMM(glmm_MaxGlobal) 


glmm_Max1 <- lmer(discharge ~  + scale(SWEin) + 
                         scale(WtempC) + 
                         scale(AirTempC) + 
                         scale(PrecipIncrem) + 
                         #scale(wtr_yr) +
                         (1|GaugeSite), data = df_MaxA)
summary(glmm_Max1) 
hist(residuals(glmm_Max1))
r.squaredGLMM(glmm_Max1) 


glmm_Max2 <- lmer(discharge ~  + scale(SWEin) + 
                    #scale(WtempC) + 
                    scale(AirTempC) + 
                    scale(PrecipIncrem) + 
                    #scale(wtr_yr) +
                    (1|GaugeSite), data = df_MaxA)
summary(glmm_Max2) 
hist(residuals(glmm_Max2))
r.squaredGLMM(glmm_Max2) 


glmm_Max3 <- lmer(discharge ~  + scale(SWEin) + 
                    #scale(WtempC) + 
                    #scale(AirTempC) + 
                    scale(PrecipIncrem) + 
                    #scale(wtr_yr) +
                    (1|GaugeSite), data = df_MaxA)
summary(glmm_Max3) 
hist(residuals(glmm_Max3))
r.squaredGLMM(glmm_Max3) 

AIC(glmm_MaxGlobal, # global still looks the best. 
    glmm_Max1, 
    glmm_Max2, 
    glmm_Max3) 



## ---------------------------
# III. Statistical tests: Discharge ~ R-O-S events

# Isolate for winter obs only from original df
df_winter <- (subset(df, nmonth > 12 | nmonth < 5)) 
summary(df_winter)

# probably dealing with zero inflation lets buff up to winter week maximums. 
lm.ROS1 <- lm(discharge ~ scale(ROS_0NY) + 
                scale(wtr_yr), data = df_winter)
summary(lm.ROS1) 
hist(residuals(lm.ROS1)) 
r.squaredGLMM(lm.ROS1) 

lm.ROS2 <- lm(discharge ~ scale(ROS2_YN) + 
                scale(wtr_yr), data = df_winter)
summary(lm.ROS2) 
hist(residuals(lm.ROS2)) 
r.squaredGLMM(lm.ROS2) 

lm.ROS3 <- lmer(discharge ~ scale(ROS3_YN) + 
                  scale(wtr_yr) + (1|GaugeSite), data = df_winter)
summary(lm.ROS3) 
hist(residuals(lm.ROS3)) 
r.squaredGLMM(lm.ROS3) 

lm.ROS4 <- lmer(discharge ~ scale(ROS_temp1) +  
                  scale(wtr_yr) + (1|GaugeSite), data = df_winter)
summary(lm.ROS4) 
hist(residuals(lm.ROS4)) 
r.squaredGLMM(lm.ROS4) 

# not sure how best to move forward the might pause for some visualizations
Fig_SWEFlow <- ggplot(df, aes(x=ROS3_YN, y=discharge, color=GaugeSite)) +
  geom_point(aes(x=ROS3_YN, y=discharge, color=GaugeSite),shape =1, size =2) +
  theme_classic() + 
  scale_color_manual(values=c("#3B6064", "#C9E4CA", "#87BBA2", "#55828B")) +
  #ylab('Stream discharge ('~ft^3~s^-1*')') + xlab("SWE (in)") + 
  guides(fill = guide_legend(override.aes = list(color = NA)), 
         color = FALSE,
         shape = FALSE) +
  facet_wrap(~GaugeSite)

# so it looks like rain on snow is just a binary level and so not sure how best to analyze 
# for those areas that experienced a rain on snow event and compare the dishcharge before and after. 

inds = which(df$ROS2_YN == 1)
# We use lapply() to get all rows for all indices, result is a list
rows <- lapply(inds, function(x) (x-5):(x+5))
# With unlist() you get all relevant rows
df_ROS <- df[unlist(rows),]

# write.csv(df_ROS, paste0(outputDir, "df_ROS.csv")) 




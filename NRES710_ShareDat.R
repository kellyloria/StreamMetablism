## ---------------------------
## NRES701 Supplemental Code
##
## Author: Kelly A. Loria
## Date Created: 2020-11-25
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
library(lmerTest) # for p value in glmm
library(MuMIn) # for glmm r squared 
library(PerformanceAnalytics) # for corrlation 
library(car) # for lm analysis
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
# II. Data organization: What parameters best describe the relationship between snow pack and discharge?
    # df is in daily obs for 20 years so some data gaps exist.
    # instead of infilling I will average at the week level for each year.

df_ave <- df %>%
  # Create grouping variable for water year "week-year"
  mutate(w_wkyr= paste(df$week, df$wtr_yr)) %>%
  # subset for winter months
  subset(nmonth > 11 | nmonth < 5) %>%
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


dim(df_ave)
#my_cor[is.na(my_cor)] <- NA
my_cor <- df_ave[, c(8:16)] # select the columns with discrete variables only 
df_ave(is.na(NaN))
chart.Correlation(my_cor, histogram=TRUE, pch=19)# horsible distribution of discharge

 
# group by max annually: _MaxA
df_MaxA <- df_ave %>%
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
df_MaxA2[is.na(df_MaxA)]<-0


my_cor <- df_MaxA[, c(6:12)] # select the columns with discrete variables only 
chart.Correlation(my_cor, histogram=TRUE, pch=19) # much more normal response distribution. 

## ---------------------------
# III. Statistical tests: lm: y ~ x
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
hist(residuals(lm_MaxA4)) # more normal distribution than precip accum.
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
chart.Correlation(my_cor, histogram=TRUE, pch=19)  
# Correlation of look at of significant predictors for glmm model selection:
    # SWE and precip increment are most strongly correlated as predictors so want to include in glmm
    # Snow depth, precip accum, and precip increment are all <0.65 related so will avoid including in same glmm.

## ---------------------------
# III Statistical tests: lmer: y ~ x + (1|Gauge Site) as explained by df_MaxA 

# Annual scale:
glmm_MaxGlobal <- lmer(discharge ~  scale(SWEin) + 
                          scale(AirTempC) + 
                          scale(PrecipIncrem) + 
                          scale(sumROS) +
                          (1|GaugeSite) + 
                          (1|SNOTELSite), data = df_MaxA)
summary(glmm_MaxGlobal) 
hist(residuals(glmm_MaxGlobal))
r.squaredGLMM(glmm_MaxGlobal) 
vif(glmm_MaxGlobal) # variance looks equally shared among predictors 

# explore the amount of variance attributed to random intercepts
GSVar <- 282.87
SLVar <- 61.02
ResVar <- 970.51
TotalVar <- (GSVar + SLVar + ResVar)

((GSVar + SLVar)/TotalVar) *100
(GSVar/TotalVar) *100





## ---------------------------
# Figures

## ---------------------------
# Figure 2: hydrologic regime 

# Value used to transform second y axis to fit
coeff <- 0.25

# A few constants
QColor <- "#224b66"
SWEcolor <- "#68acd9"

fig2 <- ggplot(df_MaxA, aes(x=wtr_yr)) +
  geom_line( aes(y=discharge ), size=0.5, color=QColor) +
  geom_point(aes(y=discharge), shape =19, color=QColor) +
  geom_line( aes(y=(SWEin)/coeff), size=0.5, color=SWEcolor) +
  geom_point( aes(y=(SWEin)/coeff), shape =19, color=SWEcolor) +
  
  scale_y_continuous(
    # Features of the first axis
    name = 'Stream discharge ('~ft^3~s^-1*')',
    # Add a second axis and specify its features
    sec.axis = sec_axis(~.*coeff, name="SWE (in)")
  ) + 
  xlab("Water year") +
  scale_x_continuous(limits = c(2000, 2020), breaks=seq(2000,2020,4)) +
  theme_bw() +
  
  theme(
    axis.title.y = element_text(color = QColor, size=13),
    axis.title.y.right = element_text(color = SWEcolor, size=13)
  )  + facet_wrap(~GaugeSite)

# save plot:
#ggsave(paste0(outputDir,"/MSM_winterHydroregimeNRES710.pdf"), fig1, scale = 1.5, width = 12, height = 6, units = c("cm"), dpi = 500)

## ---------------------------
# Figure 3: best model predictor 

# Create column for ROS that is just 0 or no

###
# Daily scale
df_plot <- df_ave %>%
  subset(nmonth > 11 | nmonth < 5) %>%
  mutate(
    ROS_lab=
      case_when(
        is.na(sumROS) ~ "No",
        sumROS<0 ~ "No",
        sumROS>0 ~ "Yes", 
        TRUE ~ as.character(sumROS)))

# Annual scale 
df_plot2 <- df_MaxA %>%
  mutate(
    ROS_lab=
      case_when(
        is.na(sumROS) ~ "No",
        sumROS<0 ~ "No",
        sumROS>0 ~ "Yes", 
        TRUE ~ as.character(sumROS)))

###
# Daily scale
Fig4_SWEFlow <- ggplot(df_plot, aes(x=PrecipIncrem, y=discharge, color=GaugeSite)) +
  geom_point(aes(x=PrecipIncrem, y=discharge, color=GaugeSite, shape=as.factor(ROS_lab)), size =2) +
  stat_smooth(method="lm", se=T, colour="#636363", level = 0.95) +
  scale_x_continuous(limits = c(0, 3), breaks=seq(0,3,0.75)) +
  scale_y_continuous(limits = c(0, 355), breaks=seq(0, 350, 70)) +
  theme_classic() + 
  scale_color_manual(
    values=c("#3B6064", "#C9E4CA", "#87BBA2", "#55828B")) +
  ylab('Stream discharge ('~ft^3~s^-1*')') + xlab("Precipitation increment (in)")  +
  guides(fill = guide_legend(override.aes = list(color = NA)),
         color = FALSE,
         shape = FALSE) +
  annotate("text", x = 1, y = 352, label = "Daily observations (12 hr)")

# Annual scale
Fig5_SWEFlow <- ggplot(df_plot2, aes(x=PrecipIncrem, y=discharge, color=GaugeSite)) +
  geom_point(aes(x=PrecipIncrem, y=discharge, color=GaugeSite, shape=as.factor(ROS_lab)), size =2) +
  stat_smooth(method="lm", se=T, colour="#636363", level = 0.95) +
  scale_x_continuous(limits = c(0, 3), breaks=seq(0,3,0.75)) +
  scale_y_continuous(limits = c(0, 355), breaks=seq(0, 350, 70)) +
  theme_classic() + 
  scale_color_manual(name = "Gauge site",
                     values=c("#3B6064", "#C9E4CA", "#87BBA2", "#55828B")) +
  ylab('Stream discharge ('~ft^3~s^-1*')') + xlab("Precipitation increment (in)")  + 
  scale_shape_discrete(name = "R-O-S event") +
  annotate("text", x = 1.2, y = 352, label = "Maximum annual observations")

library(gridExtra)
pannel_b <- grid.arrange(Fig4_SWEFlow, Fig5_SWEFlow,
                         nrow = 1,
                         widths = c(1.5,2))

#ggsave(paste0(outputDir,"/MSM_PIflowROS_NRES710_v3.pdf"), pannel_b, scale = 1, width =22, height = 8, units = c("cm"), dpi = 500)



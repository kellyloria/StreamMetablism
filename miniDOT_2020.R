## ---------------------------
## Data aggregation of all miniDOT data
##
## Author: Kelly A. Loria
## Date Created: 2020-10-12
## Email: kelly.loria@nevada.unr.edu

## ---------------------------
#Load packages
library(plyr)
library(dplyr)
library(ggplot2)
library(cowplot)
library(streamMetabolizer)
library(lubridate)
library(reshape2)
library(LakeMetabolizer)

## ---------------------------
# File path setup:
if (dir.exists('/Users/kellyloria/Documents/UNR_2020/Fall2020Projects/miniDOT_2020PrelimDat')){
  inputDir<- '/Users/kellyloria/Documents/UNR_2020/Fall2020Projects/miniDOT_2020PrelimDat'
  outputDir<- '/Users/kellyloria/Documents/UNR_2020/Fall2020Projects/miniDOT_2020PrelimDat/Clean_PrelimDat' 
}

## ---------------------------
# Calibration offsets for all sensors
# load in raw data
cal_547404 <- read.delim(paste0(inputDir, "/SeptemberMiniDOTCalibration/547404/Cat_copy.TXT"), header=T, sep = ',')
summary(cal_547404)
cal_547404$Seiral <- "547404"

cal_555348 <- read.delim(paste0(inputDir, "/SeptemberMiniDOTCalibration/555348/Cat_copy.TXT"), header=T, sep = ',')
summary(cal_555348)
cal_555348$Seiral <- "555348"

cal_617000 <- read.delim(paste0(inputDir, "/SeptemberMiniDOTCalibration/617000/Cat_copy.TXT"), header=T, sep = ',')
summary(cal_617000)
cal_617000$Seiral <- "617000"

cal_666671 <- read.delim(paste0(inputDir, "/SeptemberMiniDOTCalibration/666671/Cat_copy.TXT"), header=T, sep = ',')
summary(cal_666671)
cal_666671$Seiral <- "666671"

cal_714094 <- read.delim(paste0(inputDir, "/SeptemberMiniDOTCalibration/714094/Cat_copy.TXT"), header=T, sep = ',')
summary(cal_714094)
cal_714094$Seiral <- "714094"

# Aggregate all calibration data
Sept_Cal <- rbind(cal_547404, cal_555348, cal_617000, cal_666671, cal_714094)
summary(Sept_Cal)
head(Sept_Cal)

# attempting to convert dt1$timestamp dateTime string to R date structure (date or POSIXct)                                
tmpDateFormat<-"%Y-%m-%d %H:%M:%S" 
tmp1timestamp<-as.POSIXct(Sept_Cal$Pacific.Standard.Time,format=tmpDateFormat)
# Keep the new dates only if they all converted correctly
if(length(tmp1timestamp) == length(tmp1timestamp[!is.na(tmp1timestamp)])){Sept_Cal$timestamp <- tmp1timestamp } else {print("Date conversion failed for dt1$timestamp. Please inspect the data and do the date conversion yourself.")}    

qplot(timestamp, Dissolved.Oxygen.Saturation, data = Sept_Cal, geom="line", ylab = "Sat", color = factor(Seiral)) +
  theme(axis.text.x = element_text(angle = 25, vjust = 1.0, hjust = 1.0))

# restrict time window to 
Sept_CalQ <- subset(Sept_Cal,timestamp >= as.POSIXct('2020-09-14 13:30:00') & 
                     timestamp <= as.POSIXct('2020-09-16 10:00:00'))
range(Sept_CalQ$timestamp)

qplot(timestamp, Dissolved.Oxygen.Saturation, data = Sept_CalQ, geom="line", ylab = "Sat", color = factor(Seiral)) +
  theme(axis.text.x = element_text(angle = 25, vjust = 1.0, hjust = 1.0))

# write.csv(Sept_CalQ, paste0(outputDir,"Sept2020_CalDatQ.csv")) # complied data file of all RBR sensors along buoy line


## ---------------------------
# Aggregate data by site 

# Blackwood:
Blackwood_prelim <- read.delim(paste0(inputDir, "/Blackwood_7450-617000/Cat_copy.TXT"), header=T, sep = ',')
summary(Blackwood_prelim)
Blackwood_prelim$Site <- "Blackwood"
Blackwood_prelim$Seiral <- "617000"

# attempting to convert dt1$timestamp dateTime string to R date structure (date or POSIXct)                                
tmpDateFormat<-"%Y-%m-%d %H:%M:%S" 
tmp1timestamp<-as.POSIXct(Blackwood_prelim$Pacific.Standard.Time,format=tmpDateFormat)
# Keep the new dates only if they all converted correctly
if(length(tmp1timestamp) == length(tmp1timestamp[!is.na(tmp1timestamp)])){Blackwood_prelim$timestamp <- tmp1timestamp } else {print("Date conversion failed for dt1$timestamp. Please inspect the data and do the date conversion yourself.")}    

Blackwood_prelimQ <- subset(Blackwood_prelim,timestamp >= as.POSIXct('2020-09-27 15:00:00') & 
                      timestamp <= as.POSIXct('2020-10-11 10:30:00'))
range(Sept_CalQ$timestamp)

qplot(timestamp, Dissolved.Oxygen.Saturation, data = Blackwood_prelimQ, geom="line", ylab = "Sat", color = factor(Site)) +
  theme(axis.text.x = element_text(angle = 25, vjust = 1.0, hjust = 1.0))

# Add in new download: 2020-10-29
Blackwood_prelim2 <- read.delim(paste0(inputDir, "/Blackwood_7450-617000/20201029/7450-617000/BWCatCopy.txt"), header=T, sep = ',')
summary(Blackwood_prelim2)
Blackwood_prelim2$Site <- "Blackwood"
Blackwood_prelim2$Seiral <- "617000"

# attempting to convert dt1$timestamp dateTime string to R date structure (date or POSIXct)                                
tmpDateFormat<-"%Y-%m-%d %H:%M:%S" 
tmp1timestamp<-as.POSIXct(Blackwood_prelim2$Pacific.Standard.Time,format=tmpDateFormat)
# Keep the new dates only if they all converted correctly
if(length(tmp1timestamp) == length(tmp1timestamp[!is.na(tmp1timestamp)])){Blackwood_prelim2$timestamp <- tmp1timestamp } else {print("Date conversion failed for dt1$timestamp. Please inspect the data and do the date conversion yourself.")}    

Blackwood_prelim2Q <- subset(Blackwood_prelim2,timestamp >= as.POSIXct('2020-10-11 11:30:00') & 
                              timestamp <= as.POSIXct('2020-10-29 12:00:00'))
range(Blackwood_prelim2Q$timestamp)

Blackwood_prelim3Q <- rbind(Blackwood_prelimQ, Blackwood_prelim2Q)

plot_grid(
  ggplot(Blackwood_prelim3Q, aes(timestamp, Dissolved.Oxygen)) + geom_point(),
  ggplot(Blackwood_prelim3Q, aes(timestamp, Temperature)) + geom_point(),
  ggplot(Blackwood_prelim3Q, aes(timestamp, Q)) + geom_point(),
  ncol=1, align="hv")

# write.csv(Blackwood_prelim3Q, (paste0(outputDir,"/Blackwood_2020prelimQ.csv"))) # complied data file

## ---------------------------
# General:
General_prelim <- read.delim(paste0(inputDir, "/General_7450-547404/Cat_copy.TXT"), header=T, sep = ',')
summary(General_prelim)
General_prelim$Site <- "General"
General_prelim$Seiral <- "547404"

# attempting to convert dt1$timestamp dateTime string to R date structure (date or POSIXct)                                
tmpDateFormat<-"%Y-%m-%d %H:%M:%S" 
tmp1timestamp<-as.POSIXct(General_prelim$Pacific.Standard.Time,format=tmpDateFormat)
# Keep the new dates only if they all converted correctly
if(length(tmp1timestamp) == length(tmp1timestamp[!is.na(tmp1timestamp)])){General_prelim$timestamp <- tmp1timestamp } else {print("Date conversion failed for dt1$timestamp. Please inspect the data and do the date conversion yourself.")}    

General_prelimQ <- subset(General_prelim,timestamp >= as.POSIXct('2020-09-30 16:00:00') & 
                              timestamp <= as.POSIXct('2020-10-10 10:00:00'))
range(General_prelimQ$timestamp)

qplot(timestamp, Dissolved.Oxygen.Saturation, data = General_prelimQ, geom="line", ylab = "Sat", color = factor(Site)) +
  theme(axis.text.x = element_text(angle = 25, vjust = 1.0, hjust = 1.0))


# Add in new download: 2020-10-29
General_prelim2 <- read.delim(paste0(inputDir, "/General_7450-547404/20201029/7450-547404/GCCatCopy.txt"), header=T, sep = ',')
summary(General_prelim2)
General_prelim2$Site <- "General"
General_prelim2$Seiral <- "547404"

# attempting to convert dt1$timestamp dateTime string to R date structure (date or POSIXct)                                
tmpDateFormat<-"%Y-%m-%d %H:%M:%S" 
tmp1timestamp<-as.POSIXct(General_prelim2$Pacific.Standard.Time,format=tmpDateFormat)
# Keep the new dates only if they all converted correctly
if(length(tmp1timestamp) == length(tmp1timestamp[!is.na(tmp1timestamp)])){General_prelim2$timestamp <- tmp1timestamp } else {print("Date conversion failed for dt1$timestamp. Please inspect the data and do the date conversion yourself.")}    

General_prelim2Q <- subset(General_prelim2,timestamp >= as.POSIXct('2020-10-10 11:00:00') & 
                            timestamp <= as.POSIXct('2020-10-29 11:30:00'))
range(General_prelim2Q$timestamp)

General_prelim3Q <- rbind(General_prelimQ, General_prelim2Q)

plot_grid(
  ggplot(General_prelim3Q, aes(timestamp, Dissolved.Oxygen)) + geom_point(),
  ggplot(General_prelim3Q, aes(timestamp, Temperature)) + geom_point(),
  ggplot(General_prelim3Q, aes(timestamp, Q)) + geom_point(),
  ncol=1, align="hv")

# write.csv(General_prelim3Q, (paste0(outputDir,"/General_prelimQ.csv"))) # complied data file 

## ---------------------------
# Ward:
Ward_prelim <- read.delim(paste0(inputDir, "/Ward_7450-555348/Cat_copy.TXT"), header=T, sep = ',')
summary(Ward_prelim)
Ward_prelim$Site <- "Ward"
Ward_prelim$Seiral <- "555348"

# attempting to convert dt1$timestamp dateTime string to R date structure (date or POSIXct)                                
tmpDateFormat<-"%Y-%m-%d %H:%M:%S" 
tmp1timestamp<-as.POSIXct(Ward_prelim$Pacific.Standard.Time,format=tmpDateFormat)
# Keep the new dates only if they all converted correctly
if(length(tmp1timestamp) == length(tmp1timestamp[!is.na(tmp1timestamp)])){Ward_prelim$timestamp <- tmp1timestamp } else {print("Date conversion failed for dt1$timestamp. Please inspect the data and do the date conversion yourself.")}  

Ward_prelimQ <- subset(Ward_prelim,timestamp >= as.POSIXct('2020-09-26 19:00:00') & 
                            timestamp < as.POSIXct('2020-10-10 11:00:00'))
range(Ward_prelimQ$timestamp)

Ward_prelim0 <- read.delim(paste0(inputDir, "/Ward_7450-555348/Ward_PilotData/7450-714094/Cat_copy.TXT"), header=T, sep = ',')
summary(Ward_prelim0)
Ward_prelim0$Site <- "Ward"
Ward_prelim0$Seiral <- "714094"

# attempting to convert dt1$timestamp dateTime string to R date structure (date or POSIXct)                                
tmpDateFormat<-"%Y-%m-%d %H:%M:%S" 
tmp1timestamp<-as.POSIXct(Ward_prelim0$Pacific.Standard.Time,format=tmpDateFormat)
# Keep the new dates only if they all converted correctly
if(length(tmp1timestamp) == length(tmp1timestamp[!is.na(tmp1timestamp)])){Ward_prelim0$timestamp <- tmp1timestamp } else {print("Date conversion failed for dt1$timestamp. Please inspect the data and do the date conversion yourself.")}  

Ward_prelim0Q <- subset(Ward_prelim0,timestamp >= as.POSIXct('2020-09-18 10:30:00') & 
                         timestamp < as.POSIXct('2020-09-26 18:20:00'))
range(Ward_prelim0Q$timestamp)

qplot(timestamp, Dissolved.Oxygen.Saturation, data = Ward_prelim0Q, geom="line", ylab = "Sat", color = factor(Site)) +
  theme(axis.text.x = element_text(angle = 25, vjust = 1.0, hjust = 1.0))

Ward_prelimQ2 <- rbind(Ward_prelim0Q, Ward_prelimQ)
summary(Ward_prelimQ2)

# Add in new download: 2020-10-29
Ward_prelim2 <- read.delim(paste0(inputDir, "/Ward_7450-555348/20201029/7450-555348/WCCatCopy.txt"), header=T, sep = ',')
summary(Ward_prelim2)
Ward_prelim2$Site <- "Ward"
Ward_prelim2$Seiral <- "555348"

# attempting to convert dt1$timestamp dateTime string to R date structure (date or POSIXct)                                
tmpDateFormat<-"%Y-%m-%d %H:%M:%S" 
tmp1timestamp<-as.POSIXct(Ward_prelim2$Pacific.Standard.Time,format=tmpDateFormat)
# Keep the new dates only if they all converted correctly
if(length(tmp1timestamp) == length(tmp1timestamp[!is.na(tmp1timestamp)])){Ward_prelim2$timestamp <- tmp1timestamp } else {print("Date conversion failed for dt1$timestamp. Please inspect the data and do the date conversion yourself.")}  

Ward_prelim2_Q <- subset(Ward_prelim2,timestamp >= as.POSIXct('2020-10-10 12:00:00') & 
                          timestamp < as.POSIXct('2020-10-29 13:00:00'))
range(Ward_prelim2_Q$timestamp)


Ward_prelim3Q <- rbind(Ward_prelimQ2, Ward_prelim2_Q)

plot_grid(
  ggplot(Ward_prelim3Q, aes(timestamp, Dissolved.Oxygen)) + geom_point(),
  ggplot(Ward_prelim3Q, aes(timestamp, Temperature)) + geom_point(),
  ggplot(Ward_prelim3Q, aes(timestamp, Q)) + geom_point(),
  ncol=1, align="hv")


# write.csv(Ward_prelim3Q, (paste0(outputDir,"/Ward_prelimQ.csv")))

## ---------------------------
# Franktown

setwd("~/Documents/UNR_2020/Fall2020Projects/miniDOT_2020PrelimDat/Franktown_7450-666671/2020_10_05miniDOT_JB")
getwd()
# Set wd to proper place using "Files"
raw <- ldply(list.files(pattern = "txt"), function(filename) {
  dum = read.table(filename, sep = ",", skip=3, header=F)
  return(dum)
})

colnames(raw) <- c("Time","V","Temp","DO","Q")
raw$Time <- as.POSIXct(raw$Time, origin="1970-01-01")
head(raw, lines=5)

sapply(raw, class)

## Select desired datetime range
range(raw$Time)
raw1 <- subset(raw, Time > "2020-09-23 17:00:00" & Time < "2020-10-05 14:00:00")
raw2 <- subset(raw, Time > "2020-10-05 15:30:00" & Time < "2020-10-15 12:00:00")

Franktown3Q <- rbind(raw1, raw2)


## Visualize
plot_grid(
  ggplot(Franktown3Q, aes(Time, DO)) + geom_point(),
  ggplot(Franktown3Q, aes(Time, Temp)) + geom_point(),
  ggplot(Franktown3Q, aes(Time, Q)) + geom_point(),
  ncol=1, align="hv")

Franktown3Q$Site <- "Franktown"
Franktown3Q$Seiral <- "66671"

#write.csv(Franktown3Q, (paste0(outputDir,"/Franktown_prelimQ.csv")))


## ---------------------------
# Check out patterns by site
summary(Ward_prelim3Q)
summary(Franktown3Q)

prelimDOpart_2020 <- rbind(Ward_prelim3Q, General_prelim3Q, Blackwood_prelim3Q)
prelimDOpart1_2020 <- subset(prelimDOpart_2020,
                             select=c(Temperature, Dissolved.Oxygen, 
                                      Q, Site, Seiral, timestamp))

Franktownpart1_2020 <- subset(Franktown3Q,
                             select=c(Temp, DO, 
                                      Q, Site, Seiral, Time))

names(prelimDOpart1_2020)[1] <- "Temp"
names(prelimDOpart1_2020)[2] <- "DO"
names(prelimDOpart1_2020)[6] <- "Time"

prelimDO_2020 <- rbind(prelimDOpart1_2020, Franktownpart1_2020)
prelimDO_2020_2 <- subset(prelimDO_2020, Time > "2020-10-01 12:00:00" & Time < "2020-10-15 12:00:00")


## visualize this:
# Value used to transform the data
coeff <- 1.75

# A few constants
DOColor <- "#68acd9"
#rgb(0.2, 0.6, 0.9, 1)
TempColor <- "#224b66"

fig2 <- ggplot(prelimDO_2020_2, aes(x=Time)) +
  geom_line( aes(y=DO ), size=0.5, color=DOColor) +
  geom_line( aes(y=(Temp)/coeff), size=0.5, color=TempColor) +
  scale_y_continuous(
    # Features of the first axis
    name = 'DO ('~mg~L^-1*')',
    # Add a second axis and specify its features
    sec.axis = sec_axis(~.*coeff, name="Temp (degreeC)")
  ) + 
  #scale_x_continuous(limits = c(2000, 2020), breaks=seq(2000,2020,4)) +
  theme_bw() +
 
  theme(
    axis.title.y = element_text(color = DOColor, size=13),
    axis.title.y.right = element_text(color = TempColor, size=13)
  )  + facet_wrap(~Site)

#ggsave(paste0(outputDir,"/MSM_DOcomparison.jpeg"), fig2, scale = 1.5, width = 12, height = 6, units = c("cm"), dpi = 500)



p <- ggplot(prelimDO_2020, aes(x=Time, y=(DO), colour =as.factor(Site), shape=(Seiral))) +
  geom_line()  + ylab("DO") +
  theme_classic() + facet_wrap(~Site)


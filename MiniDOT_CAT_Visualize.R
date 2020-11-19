## Checking MiniDOT outputs
library(plyr)
library(dplyr)
library(ggplot2)
library(cowplot)
library(streamMetabolizer)
library(lubridate)

## ---------------------------
## Set wd and read in raw .txt files

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

## Visualize
plot_grid(
ggplot(raw, aes(Time, DO)) + geom_point(),
ggplot(raw, aes(Time, Temp)) + geom_point(),
ggplot(raw, aes(Time, Q)) + geom_point(),
ncol=1, align="hv")

## Select desired datetime range
range(raw$Time)
raw1 <- subset(raw, Time > "2020-09-23 15:00:00" & Time < "2020-10-05 12:00:00")

## Write compiled csv -- reset wd
setwd("/Users/kellyloria/Documents/UNR_2020/Fall2020Projects/miniDOT_2020PrelimDat/Franktown_7450-666671")
write.csv(raw1, "20201005_FRANKTWON_DOcompiled_JB.csv")




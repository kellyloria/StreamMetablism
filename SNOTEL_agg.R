# directions from https://github.com/bluegreen-labs/snotelr

#install.packages("snotelr")
library(DT)
library(plotly)
library(shinydashboard)
library(leaflet)
library(snotelr)
snotel_explorer()


# returns the site info as snotel_metadata.txt in the current working directory
snotel_info(path = ".") 

# export to data frame
meta_data <- snotel_info(path = NULL)

snotel_download(site_id = 924)

# show some lines of the data frame
head(meta_data)


## ---------------------------
# File path setup:
if (dir.exists('/Users/kellyloria/Documents/UNR_2020/Fall2020Projects/SNOTEL')){
  inputDir<- '/Users/kellyloria/Documents/UNR_2020/Fall2020Projects/SNOTEL'
  outputDir<- '/Users/kellyloria/Documents/UNR_2020/Fall2020Projects/SNOTEL' 
}

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

qplot(timestamp, Dissolved.Oxygen.Saturation, data = WardSNOTEL, geom="line", ylab = "Sat", color = factor(Site)) +
  theme(axis.text.x = element_text(angle = 25, vjust = 1.0, hjust = 1.0))



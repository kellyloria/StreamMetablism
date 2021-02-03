## ---------------------------
## Practice space for stream metabolizer package used in Appling et al. 2018
##
## Author: Kelly A. Loria
## Date Created: 2020-11-19
## Email: kelly.loria@nevada.unr.edu
##
## ---------------------------
# Installing streamMetabolizer
#remotes::install_github('appling/unitted')
#remotes::install_github("USGS-R/streamMetabolizer")

## ---------------------------
## Load packages:
library(StreamMetabolism)
library(streamMetabolizer)
library(dplyr)
library(tidyr)
library(ggplot2)
library(lme4)
library(rstan)
library(unitted)
library(zoo)
library(lubridate)
library(dataRetrieval)

##
## ---------------------------
# File path setup:
if (dir.exists('/Users/kellyloria/Documents/UNR_2020/Fall2020Projects')){
  inputDir<- '/Users/kellyloria/Documents/UNR_2020/Fall2020Projects/StreamMetablismPractice_Data'
  outputDir<- '/Users/kellyloria/Documents/UNR_2020/Fall2020Projects/StreamMetablismPractice_Output' 
}

## ---------------------------
# Weather station data

# KTRK.2020-11-01.csv
# Weather station data: KTVL
# STATION NAME: South Lake Tahoe	 Lake Tahoe Airport				
# LATITUDE: 38.89836					
# LONGITUDE: -119.99615					
# ELEVATION [ft]: 6257	
# available from the NCEI (https://www.ncdc.noaa.gov/customer-support/certification-data)

KTVL <- read_csv("~/Documents/UNR_2020/Fall2020Projects/StreamMetablismPractice_Data/KTVL.2020-11-01.csv")
summary(KTVL)


# KTVL <- read.csv("~/Documents/UNR_2020/Fall2020Projects/StreamMetablismPractice_Data/KTVL.2020-12-01.csv")
# summary(KTVL)
# KTVL$Time <- as.POSIXct(KTVL$Date_Time, origin="1970-01-01")

# Left join flow data with DO dat: Ward_prelim3Q
summary(General_prelim3Q)
General_prelim3Q$timestampQ <- as.POSIXct(round_date(General_prelim3Q$timestamp, hour,unit="5 minutes"))


# Left join flow data with DO dat: 
Q_General <- left_join(General_prelim3Q, KTVL[c("Date_Time", "air_temp_C", "pressure_pascals")],
                    by = c("timestampQ" = "Date_Time"))
summary(Q_General)

## ---------------------------
# Flow data from USGS:

# General "10336645"
siteNumber <- "10336645" 
SagehenInfo <- readNWISsite(siteNumber)
parameterCd <- c("00060", "00010")

#Raw daily data:
qwData_Gen <- readNWISdv(siteNumber,parameterCd,
                         "2020-09-01","2020-11-20")
plot(qwData_Gen$Date, qwData_Gen$X_00060_00003)
qwData_Gen$site <- "General"


Q_General2 <- left_join(Q_General, qwData_Gen[c("Date", "X_00060_00003")],
                     by = c("date" = "Date"))
summary(Q_General2)

# infill some means
Q_General2$pressureMean <- rollapply(Q_General2$pressure_pascals, width=300,
                                  FUN=function(x) mean(x, na.rm=TRUE), by=1,
                                  by.column=TRUE, partial=TRUE, fill=NA, align="center")

Q_General2$pressure_pascals <- as.numeric(ifelse(is.na(Q_General2$pressure_pascals), 
                                              (Q_General2$pressureMean),
                                              (Q_General2$pressure_pascals)))
summary(Q_General2)

Q_General2$pressure_millibar <- c(Q_General2$pressure_pascals * 0.01)
Q_General2$Qs <- Q_General2$X_00060_00003


Q_General2$depth <- calc_depth(Q=u(Q_General2$X_00060_00003, "m^3 s^-1"), f=u(0.36))

# calc light example
latitude <- c(39.052008)
longitude <- c(-120.119056) 

Q_General2$solar.time <- calc_solar_time(Q_General2$timestamp, longitude)

Q_General2$light <- calc_light(
  Q_General2$solar.time,
  latitude,
  longitude,
  max.PAR = u(2326, "umol m^-2 s^-1"),
  attach.units = is.unitted(Q_General2$solar.time)
)

?calc_solar_time
?calc_light
?calc_DO_sat

Q_General2$DO.sat <- calc_DO_sat(Q_General2$Temperature, 
                                 Q_General2$pressure_millibar, sal=0) # still need to get barometric pressure data

# Get data in correct name and column form
names(Q_General2)

colnames(Q_General2)[5] <- "temp.water"
colnames(Q_General2)[6] <- "DO.obs"
colnames(Q_General2)[16] <- "discharge"

# New named df
GCdat <- subset(Q_General2, select= c(solar.time, DO.obs, DO.sat, depth, temp.water, light, discharge))

## ---------------------------
# with chaining & customization
# fit a basic MLE model

mm <- metab(specs(mm_name('mle')), data=GCdat, info='my info')
predict_metab(mm)
get_info(mm)
get_fitting_time(mm)

## SAVE mm model output:
Gen_mmOutput <- get_params(mm)[c('date','K600.daily','GPP.daily','ER.daily')]
plot(Gen_mmOutput$K600.daily, Gen_mmOutput$GPP.daily)
plot(Gen_mmOutput$K600.daily, Gen_mmOutput$ER.daily)

# write.csv(Gen_mmOutput, paste0(outputDir, "/Gen_mmOutput.csv")) # complied data file 



## ---------------------------
# Final figure formatting 

# Panel plot for raw data
GC_RawPlot1 <- GCdat %>% unitted::v() %>%
  mutate(DO.pctsat = 100 * (DO.obs / DO.sat)) %>%
  select(solar.time, starts_with('DO')) %>%
  gather(type, DO.value, starts_with('DO')) %>%
  mutate(units=ifelse(type == 'DO.pctsat', 'DO\n(% sat)', 'DO\n(mg/L)')) %>%
  ggplot(aes(x=solar.time, y=DO.value, color=type)) + geom_line() +
  facet_grid(units ~ ., scale='free_y') + theme_bw() + xlab("") + ggtitle("General Creek") +
  ylab("") +
  scale_color_manual(values = c("#B9C252", "#0B322F", "#1f838c")) 

labels <- c(depth='depth\n(m)', temp.water='water temp\n(deg C)', light='PAR\n(umol m^-2 s^-1)')
GC_RawPlot2 <- GCdat %>% unitted::v() %>%
  select(solar.time, depth, temp.water, light) %>%
  gather(type, value, depth, temp.water, light) %>%
  mutate(
    type=ordered(type, levels=c('depth','temp.water','light')),
    units=ordered(labels[type], unname(labels))) %>%
  ggplot(aes(x=solar.time, y=value, color=type)) + geom_line() +
  facet_grid(units ~ ., scale='free_y') + theme_bw() + xlab("Solar time") +
  ylab("") +
  scale_color_manual(values = c("#C1666B", "#3c709e", "#D4B483")) 

#library(gridExtra)
RawDat <- grid.arrange(GC_RawPlot1, GC_RawPlot2,
                       nrow = 2, heights=c(1.5,2))
#ggsave(paste0(outputDir,"/MSM_GeneralRawModelData.pdf"), RawDat, scale = 1, width =15, height = 20, units = c("cm"), dpi = 500)

# Panel plot for Modeled data
GC_ModelDOPlot <- plot_DO_preds(predict_DO(mm))

GC_ModelMetabPlot <- plot_metab_preds(mm)


ModelDat <- grid.arrange(GC_ModelDOPlot, GC_ModelMetabPlot,
                         nrow = 2, heights=c(2,1.5))
#ggsave(paste0(outputDir,"/MSM_GeneralMetabModelData.pdf"), ModelDat, scale = 1, width =15, height = 20, units = c("cm"), dpi = 500)



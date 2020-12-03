library(dataRetrieval)
library(ggplot2)
library(scales)
#library(tidyverse)
library(zoo)
library(unitted)
library(dplyr)
library(dygraphs)
library(StreamMetabolism)
library(streamMetabolizer)
library(tidyr)
library(lme4)
library(rstan)
library(unitted)

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


## ---------------------------
qwData_Gen$Qs <- rollapply(qwData_Gen$X_00060_00003, width=3, 
                              FUN=function(x) mean(x, na.rm=TRUE), by=1, 
                              by.column=TRUE, partial=TRUE, fill=NA, align="center")


qwData_ward$Qs <- replace(qwData_ward$flow, qwData_ward$flow==0,0.001)

plot(qwData_Gen$Date, qwData_Gen$X_00060_00003) # some gaps 
plot(qwData_Gen$Date, qwData_Gen$Qs)

summary(qwData_Gen$X_00060_00003)


# Left join flow data with DO dat: Ward_prelim3Q
General_prelim3Q$date <- as.Date(General_prelim3Q$timestamp)
Q_Gen <- left_join(General_prelim3Q, qwData_Gen[c("Date", "X_00060_00003", "Qs")],
                    by = c("date" = "Date"))
summary(Q_Gen)

Q_Gen$depth <- calc_depth(Q=u(Q_Gen$X_00060_00003, "m^3 s^-1"), f=u(0.36))

# calc light example
latitude <- c(39.052008)
longitude <- c(-120.119056) 

Q_Gen$solar.time <- calc_solar_time(Q_Gen$timestamp, longitude)

Q_Gen$light <- calc_light(
  Q_Gen$solar.time,
  latitude,
  longitude,
  max.PAR = u(2326, "umol m^-2 s^-1"),
  attach.units = is.unitted(Q_Gen$solar.time)
)

?calc_solar_time
?calc_light

Q_Gen$DO_sat <- calc_DO_sat(Q_Gen$Temperature, 
                             press=1000.1, sal=0) # still need to get barometric pressure data

# Check the input data format:
?mm_data
metab_inputs('mle', 'data')

###
# Get data in correct name and column form
names(Q_Gen)

colnames(Q_Gen)[5] <- "temp.water"
colnames(Q_Gen)[6] <- "DO.obs"
colnames(Q_Gen)[7] <- "DO.sat"
colnames(Q_Gen)[13] <- "discharge"

# New named df
GCdat <- subset(Q_Gen, select= c(solar.time, DO.obs, DO.sat, depth, temp.water, light, discharge))

# GPP visualization + modeling
GCdat %>% unitted::v() %>%
  mutate(DO.pctsat = 100 * (DO.obs / DO.sat)) %>%
  select(solar.time, starts_with('DO')) %>%
  gather(type, DO.value, starts_with('DO')) %>%
  mutate(units=ifelse(type == 'DO.pctsat', 'DO\n(% sat)', 'DO\n(mg/L)')) %>%
  ggplot(aes(x=solar.time, y=DO.value, color=type)) + geom_line() +
  facet_grid(units ~ ., scale='free_y') + theme_bw() +
  scale_color_discrete('variable')

labels <- c(depth='depth\n(m)', temp.water='water temp\n(deg C)', light='PAR\n(umol m^-2 s^-1)')
GCdat %>% unitted::v() %>%
  select(solar.time, depth, temp.water, light) %>%
  gather(type, value, depth, temp.water, light) %>%
  mutate(
    type=ordered(type, levels=c('depth','temp.water','light')),
    units=ordered(labels[type], unname(labels))) %>%
  ggplot(aes(x=solar.time, y=value, color=type)) + geom_line() +
  facet_grid(units ~ ., scale='free_y') + theme_bw() +
  scale_color_discrete('variable')

mm <- metab(specs(mm_name('mle')), data=GCdat, info='my info')
predict_metab(mm)
get_info(mm)
get_fitting_time(mm)

mm <- mm_name('mle', ode_method='euler') %>%
  specs(init.GPP.daily=40) %>%
  metab(data=GCdat)
predict_metab(mm)
## Not run: 
plot_DO_preds(predict_DO(mm))
plot_DO_preds(predict_DO(mm), y_var='pctsat', style='dygraphs')

plot_metab_preds(mm)



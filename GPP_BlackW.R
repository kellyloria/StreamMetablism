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
summary(Blackwood_prelim3Q)
Blackwood_prelim3Q$timestampQ <- as.POSIXct(round_date(Blackwood_prelim3Q$timestamp, hour,unit="5 minutes"))


# Left join flow data with DO dat: 
Q_BlackW <- left_join(Blackwood_prelim3Q, KTVL[c("Date_Time", "air_temp_C", "pressure_pascals")],
                       by = c("timestampQ" = "Date_Time"))
summary(Q_BlackW)

## ---------------------------
# Flow data from USGS:

# Blackwood canyon "10336660"
siteNumber <- "10336660" 
SagehenInfo <- readNWISsite(siteNumber)
parameterCd <- c("00060", "00010")
#Raw daily data:
watQ_BlackW <- readNWISdv(siteNumber,parameterCd,
                       "2020-09-27","2020-10-29") 

Q_BlackW2 <- left_join(Q_BlackW, watQ_BlackW[c("Date", "X_00060_00003")],
                        by = c("timestampQ" = "Date"))
summary(Q_BlackW2)

# infill some means
Q_BlackW2$pressureMean <- rollapply(Q_BlackW2$pressure_pascals, width=300,
                                     FUN=function(x) mean(x, na.rm=TRUE), by=1,
                                     by.column=TRUE, partial=TRUE, fill=NA, align="center")

Q_BlackW2$pressure_pascals <- as.numeric(ifelse(is.na(Q_BlackW2$pressure_pascals), 
                                                 (Q_BlackW2$pressureMean),
                                                 (Q_BlackW2$pressure_pascals)))
summary(Q_BlackW2)

Q_BlackW2$pressure_millibar <- c(Q_BlackW2$pressure_pascals * 0.01)


# infill some means
Q_BlackW2$dischargemean <- rollapply(Q_BlackW2$X_00060_00003, width=460,
                                    FUN=function(x) mean(x, na.rm=TRUE), by=1,
                                    by.column=TRUE, partial=TRUE, fill=NA, align="center")

Q_BlackW2$X_00060_00003 <- as.numeric(ifelse(is.na(Q_BlackW2$X_00060_00003), 
                                                (Q_BlackW2$dischargemean),
                                                (Q_BlackW2$X_00060_00003)))
summary(Q_BlackW2)
Q_BlackW2$Qs <- Q_BlackW2$X_00060_00003


Q_BlackW2$depth <- calc_depth(Q=u(Q_BlackW2$X_00060_00003, "m^3 s^-1"), f=u(0.36))
#?calc_depth
# calc light example
latitude <- c(39.107055)
longitude <- c(-120.163089) 

Q_BlackW2$solar.time <- calc_solar_time(Q_BlackW2$timestamp, longitude)
Q_BlackW2$light <- calc_light(Q_BlackW2$solar.time,
                                  latitude,
                                  longitude,
                                  max.PAR = u(2326, "umol m^-2 s^-1"),
                                  attach.units = is.unitted(Q_BlackW2$solar.time)
)

#?calc_light
Q_BlackW2$DO.sat <- calc_DO_sat(Q_BlackW2$Temperature, 
                                Q_BlackW2$pressure_millibar, sal=0) # still need to get barometric pressure data


###
# Get data in correct name and column form
names(Q_BlackW2)
colnames(Q_BlackW2)[5] <- "temp.water"
colnames(Q_BlackW2)[6] <- "DO.obs"
colnames(Q_BlackW2)[16] <- "discharge"

# New named df
BWdat <- subset(Q_BlackW2, select= c(solar.time, DO.obs, DO.sat, depth, temp.water, light, discharge))

# write.csv(BWdat, paste0(outputDir, "/BWdat.csv")) # complied data file 

## ---------------------------
# with chaining & customization
# fit a basic MLE model

mm <- metab(specs(mm_name('mle')), data=BWdat, info='my info')
predict_metab(mm)
get_info(mm)
get_fitting_time(mm)

get_params(mm)[c('date','K600.daily','GPP.daily','ER.daily')]

## SAVE mm model output:
BW_mmOutput <- get_params(mm)[c('date','K600.daily','GPP.daily','ER.daily')]
plot(BW_mmOutput$K600.daily, BW_mmOutput$GPP.daily)
plot(BW_mmOutput$K600.daily, BW_mmOutput$ER.daily)

# write.csv(BW_mmOutput, paste0(outputDir, "/BW_mmOutput.csv")) # complied data file 



## ---------------------------

# GPP visualization + modeling
BWdat %>% unitted::v() %>%
  mutate(DO.pctsat = 100 * (DO.obs / DO.sat)) %>%
  select(solar.time, starts_with('DO')) %>%
  gather(type, DO.value, starts_with('DO')) %>%
  mutate(units=ifelse(type == 'DO.pctsat', 'DO\n(% sat)', 'DO\n(mg/L)')) %>%
  ggplot(aes(x=solar.time, y=DO.value, color=type)) + geom_line() +
  facet_grid(units ~ ., scale='free_y') + theme_bw() +
  scale_color_discrete('variable')

labels <- c(depth='depth\n(m)', temp.water='water temp\n(deg C)', light='PAR\n(umol m^-2 s^-1)')
BWdat %>% unitted::v() %>%
  select(solar.time, depth, temp.water, light) %>%
  gather(type, value, depth, temp.water, light) %>%
  mutate(
    type=ordered(type, levels=c('depth','temp.water','light')),
    units=ordered(labels[type], unname(labels))) %>%
  ggplot(aes(x=solar.time, y=value, color=type)) + geom_line() +
  facet_grid(units ~ ., scale='free_y') + theme_bw() +
  scale_color_discrete('variable')



## ---------------------------
# with chaining & customization
library(dplyr)
library(dygraphs)


# mm <- mm_name('mle', ode_method='euler') %>%
#   specs(init.GPP.daily=40) %>%
#   metab(data=BWdat1)
# predict_metab(mm)
## Not run: 

# Panel plot for raw data
BW_RawPlot1 <- BWdat %>% unitted::v() %>%
  mutate(DO.pctsat = 100 * (DO.obs / DO.sat)) %>%
  select(solar.time, starts_with('DO')) %>%
  gather(type, DO.value, starts_with('DO')) %>%
  mutate(units=ifelse(type == 'DO.pctsat', 'DO\n(% sat)', 'DO\n(mg/L)')) %>%
  ggplot(aes(x=solar.time, y=DO.value, color=type)) + geom_line() +
  facet_grid(units ~ ., scale='free_y') + theme_bw() + xlab("") + ggtitle("Blackwood Creek") +
  ylab("") +
  scale_color_manual(values = c("#B9C252", "#0B322F", "#1f838c")) 

labels <- c(depth='depth\n(m)', temp.water='water temp\n(deg C)', light='PAR\n(umol m^-2 s^-1)')
BW_RawPlot2 <- BWdat %>% unitted::v() %>%
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
RawDat <- grid.arrange(BW_RawPlot1, BW_RawPlot2,
                       nrow = 2, heights=c(1.5,2))
#ggsave(paste0(outputDir,"/MSM_BlackwoodRawModelData.pdf"), RawDat, scale = 1, width =15, height = 20, units = c("cm"), dpi = 500)

# Panel plot for Modeled data
BW_ModelDOPlot <- plot_DO_preds(predict_DO(mm))
#?plot_DO_preds
BW_ModelMetabPlot <- plot_metab_preds(mm)

ModelDat <- grid.arrange(BW_ModelDOPlot, BW_ModelMetabPlot,
                         nrow = 2, heights=c(2,1.5))
#ggsave(paste0(outputDir,"/MSM_BlackwoodMetabModelData.pdf"), ModelDat, scale = 1, width =15, height = 20, units = c("cm"), dpi = 500)































################3
# you are here ##
# For this example, we will specify a Bayesian model with both observation error and process error. 
# We won’t pool K600 here because we don’t have many days of data, but pooling is one feature that makes Bayesian models better than MLE models in general. 
# Another great feature of Bayesian models is that they produce more accurate and nuanced confidence intervals.
bayes_name <- mm_name(type='bayes', pool_K600='none', err_obs_iid=TRUE, err_proc_iid=TRUE)
bayes_name

# Set the specifications:

# We now pass the model name to specs() to get a list of default specifications for this model.
bayes_specs <- specs(bayes_name)
bayes_specs
#At this point we can alter some of the specifications if desired...

# one way to alter specifications: call specs() again
bayes_specs <- specs(bayes_name, burnin_steps=100, saved_steps=200, n_cores=1, GPP_daily_mu=3, GPP_daily_sigma=2)
# another way: use revise()
bayes_specs <- revise(bayes_specs, burnin_steps=100, saved_steps=200, n_cores=1, GPP_daily_mu=3, GPP_daily_sigma=2)

# Fitting the model:
#   Once a model has been configured, you can fit the model to data with metab(). 
#   Bayesian models take a while to run, so be patient. Or switch to an MLE model if you can afford to sacrifice some accuracy for speed. 
#   (This small example usually takes about 30 seconds on my computer.)

mm_bay <- metab(bayes_specs, data=BWdat) 


# Here are the daily metabolism predictions from the model:
predict_metab(mm)

plot_metab_preds(mm)

# You can inspect more of the fitted daily parameters, including K600, with get_params():
get_params(mm)

# Here are the first few dissolved oxygen predictions from the model (DO.mod). 
# They are returned along with the input data for convenience.

predict_DO(mm) %>% head()
plot_DO_preds(mm)

# For Bayesian models only, you can dig even deeper using get_mcmc, which returns a stanfit object that 
# can be inspected using the rstan package. 
# (These traceplots are pretty bad because we used so few MCMC iterations. 
# You should strive for better in your final models.)

mcmc <- get_mcmc(mm)
rstan::traceplot(mcmc, pars='K600_daily', nrow=3)

# The get_fit() function returns a list of data.frames, 
# one per temporal resolution, containing all fitted values and details about their distributions and convergence. 
# Here are just the overall metrics of model convergence (Rhats, or potential scale reduction statistics; see Gelman and Rubin 1992 or Brooks and Gelman 1998):

get_fit(mm)$overall %>%
  select(ends_with('Rhat'))

# And here is a list of all column names available through get_fit():
get_fit(mm) %>%
  lapply(names)





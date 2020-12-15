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

##
## ---------------------------
# File path setup:
if (dir.exists('/Users/kellyloria/Documents/UNR_2020/Fall2020Projects')){
  inputDir<- '/Users/kellyloria/Documents/UNR_2020/Fall2020Projects/StreamMetablismPractice_Data'
  outputDir<- '/Users/kellyloria/Documents/UNR_2020/Fall2020Projects/StreamMetablismPractice_Output' 
}

## ---------------------------
# Data Preparation: From the tutorial
# http://usgs-r.github.io/streamMetabolizer/articles/data_prep.html

dat <- data_metab(num_days='3', res='15', attach.units=TRUE)

# Quick exploration:
dat %>% unitted::v() %>%
  mutate(DO.pctsat = 100 * (DO.obs / DO.sat)) %>%
  select(solar.time, starts_with('DO')) %>%
  gather(type, DO.value, starts_with('DO')) %>%
  mutate(units=ifelse(type == 'DO.pctsat', 'DO\n(% sat)', 'DO\n(mg/L)')) %>%
  ggplot(aes(x=solar.time, y=DO.value, color=type)) + geom_line() +
  facet_grid(units ~ ., scale='free_y') + theme_bw() +
  scale_color_discrete('variable')

labels <- c(depth='depth\n(m)', temp.water='water temp\n(deg C)', light='PAR\n(umol m^-2 s^-1)')
dat %>% unitted::v() %>%
  select(solar.time, depth, temp.water, light) %>%
  gather(type, value, depth, temp.water, light) %>%
  mutate(
    type=ordered(type, levels=c('depth','temp.water','light')),
    units=ordered(labels[type], unname(labels))) %>%
  ggplot(aes(x=solar.time, y=value, color=type)) + geom_line() +
  facet_grid(units ~ ., scale='free_y') + theme_bw() +
  scale_color_discrete('variable')

# Check the input data format:
metab_inputs('mle', 'data')

##
# package that might hekp with other data preparation:
# ?calc_depth()
# ?calc_DO_sat()
# ?calc_light()
# ?convert_date_to_doyhr()
# ?convert_localtime_to_UTC()
# ?convert_UTC_to_solartime()
# ?convert_k600_to_kGAS()
# ?convert_PAR_to_SW()

# streamMetabolizer Quickstart Guide:

# Preparing the input data:
#dat <- data_metab(num_days='3', res='15', day_start=4, day_end=28, attach.units=TRUE)

# Configuring the model: 

# There are two steps to configuring a metabolism model in streamMetabolizer.
#     a. Identify the name of the model structure you want using mm_name().
#     b. Set the specifications for the model using defaults fromspecs() as a starting point

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

mm <- metab(bayes_specs, data=dat) 


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


## ---------------------------
# Kelly Data Preparation: For Blackwood: Blackwood_prelim3Q

# DO data: Blackwood:
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

?calc_depth
# Example:
# Qs <- seq(1,9,2)
# calc_depth(Q=Qs)
# calc_depth(Q=Qs, f=0.4)
# library(unitted)
# calc_depth(Q=u(Qs, "m^3 s^-1"), c=u(40,"cm"))
# calc_depth(Q=u(Qs, "m^3 s^-1"), f=u(0.36))

## ---------------------------
# Flow data from USGS:
library(dataRetrieval)
  # Blackwood canyon "10336660"
siteNumber <- "10336660" 
SagehenInfo <- readNWISsite(siteNumber)
parameterCd <- c("00060", "00010")
  #Raw daily data:
Q_BlackW <- readNWISdv(siteNumber,parameterCd,
                       "2020-09-27","2020-10-29")

# Q_BlackW$Qs <- rollapply(Q_BlackW$X_00060_00003, width=3, 
#                            FUN=function(x) mean(x, na.rm=TRUE), by=1, 
#                            by.column=TRUE, partial=TRUE, fill=NA, align="center")

# Q_BlackW$Qs <- replace(Q_BlackW$flow, Q_BlackW$flow==0,0.001)

plot(Q_BlackW$Date, Q_BlackW$X_00060_00003) # some gaps 

# Left join flow data with DO dat
Q_BlackW1 <- left_join(Q_BlackW, qwData_wardQ[c("Date", "baroRawMiliBar")],
                         by = c("Date" = "Date"))

Blackwood_prelim3Q$date <- as.Date(Blackwood_prelim3Q$timestamp)
Blackwood_MSM <- left_join(Blackwood_prelim3Q, Q_BlackW1[c("Date", "X_00060_00003", "baroRawMiliBar")],
                           by = c("date" = "Date"))
summary(Blackwood_MSM)

Blackwood_MSM$depth <- calc_depth(Q=u(Blackwood_MSM$X_00060_00003, "m^3 s^-1"), f=u(0.36))
#?calc_depth
# calc light example
latitude <- c(39.107055)
longitude <- c(-120.163089) 

Blackwood_MSM$solar.time <- calc_solar_time(Blackwood_MSM$timestamp, longitude)
Blackwood_MSM$light <- calc_light(Blackwood_MSM$solar.time,
                                  latitude,
                                  longitude,
                                  max.PAR = u(2326, "umol m^-2 s^-1"),
                                  attach.units = is.unitted(Blackwood_MSM$solar.time)
)

#?calc_light
Blackwood_MSM$DO_sat <- calc_DO_sat(Blackwood_MSM$Temperature, 
                                    Blackwood_MSM$baroRawMiliBar, sal=0) # still need to get barometric pressure data

# Check the input data format:
?mm_data
metab_inputs('mle', 'data')

###
# Get data in correct name and column form
names(Blackwood_MSM)

colnames(Blackwood_MSM)[5] <- "temp.water"
colnames(Blackwood_MSM)[6] <- "DO.obs"
colnames(Blackwood_MSM)[13] <- "discharge"
colnames(Blackwood_MSM)[18] <- "DO.sat"

# New named df
BWdat <- subset(Blackwood_MSM, select= c(solar.time, DO.obs, DO.sat, depth, temp.water, light, discharge))

# write.csv(BWdat, paste0(outputDir, "/BWdat.csv")) # complied data file 

## ---------------------------
# Incase we need to re-read in clean data 
# BWdat <- read.csv("BWdat.csv")

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

?data_metab

## ---------------------------
# with chaining & customization
library(dplyr)
library(dygraphs)

# lets cutout the 26th 
#BWdat1 <- subset(BWdat, solar.time <= as.POSIXct('2020-10-28 10:59:40 UTC'))

# fit a basic MLE model
?metab
mm <- metab(specs(mm_name('mle')), data=BWdat, info='my info')
predict_metab(mm)
get_info(mm)
get_fitting_time(mm)

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

mm <- metab(bayes_specs, data=BWdat) 


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





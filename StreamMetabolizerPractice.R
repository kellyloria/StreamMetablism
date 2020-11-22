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
##
## ---------------------------
# File path setup:
if (dir.exists('/Users/kellyloria/Documents/UNR_2020/Fall2020Projects')){
  inputDir<- '/Users/kellyloria/Documents/UNR_2020/Fall2020Projects/StreamMetablismPractice_Data'
  outputDir<- '/Users/kellyloria/Documents/UNR_2020/Fall2020Projects/StreamMetablismPractice_Output' 
}

## ---------------------------
# Data Preparation: 
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
# Prepare the timestamps

# Starting with numeric datetimes, e.g., from PMEs
num.time <- 1471867200
(posix.time.localtz <- as.POSIXct(num.time, origin='1970-01-01', tz='UTC'))
# If you have datetimes stored in seconds since 1/1/1970 at Laramie, WY (i.e., in MST, no daylight savings):
num.time <- 1471867200
(posix.time.nominalUTC <- as.POSIXct(num.time, origin='1970-01-01', tz='UTC')) # the numbers get treated as UTC no matter what tz you request
(posix.time.localtz <- lubridate::force_tz(posix.time.nominalUTC, 'Etc/GMT+7')) # +7 = mountain standard time

# Starting with text timestamps:
#   If you have datetimes stored as text timestamps in UTC, you can bypass the conversion to local time and just start with UTC. 
#   Then rather than using calc_solar_time() in Step 2, you’ll use convert_UTC_to_solartime()
text.time <- '2016-08-22 12:00:00'
(posix.time.utc <- as.POSIXct(text.time, tz='UTC'))

text.time <- '2016-08-22 12:00:00'
(posix.time.localtz <- as.POSIXct(text.time, format="%Y-%m-%d %H:%M:%S", tz='America/New_York'))

text.time <- '2016-08-22 12:00:00'
(posix.time.localtz <- as.POSIXct(text.time, format="%Y-%m-%d %H:%M:%S", tz='Etc/GMT+5'))

# Starting with chron datetimes:
#   If you have datetimes stored in the chron time format in EST (no daylight savings):

chron.time <- chron::chron('08/22/16', '12:00:00')
time.format <- "%Y-%m-%d %H:%M:%S"
text.time <- format(chron.time, time.format) # direct as.POSIXct time works poorly
(posix.time.localtz <- as.POSIXct(text.time, format=time.format, tz='Etc/GMT+5'))

# Step 2: Solar time:
lubridate::tz(posix.time.localtz) # yep, we want and have the code for EST
(posix.time.solar <- streamMetabolizer::calc_solar_time(posix.time.localtz, longitude=-106.3))


##
# Other data preparation:
?calc_depth()
?calc_DO_sat()
?calc_light()
?convert_date_to_doyhr()
?convert_localtime_to_UTC()
?convert_UTC_to_solartime()
?convert_k600_to_kGAS()
?convert_PAR_to_SW()

## ---------------------------
# streamMetabolizer Quickstart Guide:

# Preparing the input data:
dat <- data_metab(num_days='3', res='15', day_start=4, day_end=28, attach.units=TRUE)

## ---------------------------
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
# Data Preparation: For Blackwood: Blackwood_prelim3Q
range(Blackwood_prelim3Q$timestamp)

?calc_depth
# Example:
# Qs <- seq(1,9,2)
# calc_depth(Q=Qs)
# calc_depth(Q=Qs, f=0.4)
# library(unitted)
# calc_depth(Q=u(Qs, "m^3 s^-1"), c=u(40,"cm"))
# calc_depth(Q=u(Qs, "m^3 s^-1"), f=u(0.36))

# Flow data from USGS:
library(dataRetrieval)
  # Blackwood canyon "10336660"
siteNumber <- "10336660" 
SagehenInfo <- readNWISsite(siteNumber)
parameterCd <- c("00060", "00010")
  #Raw daily data:
Q_BlackW <- readNWISdv(siteNumber,parameterCd,
                       "2020-09-27","2020-10-29")
plot(Q_BlackW$Date, Q_BlackW$X_00060_00003)

Qs <- c(Q_BlackW$X_00060_00003 * 0.0283168)
calc_depth(Q=Qs)
calc_depth(Q=Qs, f=0.4)
library(unitted)
calc_depth(Q=u(Qs, "m^3 s^-1"), c=u(40,"cm"))
calc_depth(Q=u(Qs, "m^3 s^-1"), f=u(0.36))


Blackwood_prelim3Q

?calc_DO_sat
# Example
calc_DO_sat(temp=21, press=1000.1, sal=0) # no units checking if no units provided
library(unitted)
calc_DO_sat(temp=u(21,"degC"), press=u(1000.1,"mb"), sal=u(0,"PSU")) # units are checked



?calc_light
?convert_date_to_doyhr
#?convert_localtime_to_UTC
?convert_UTC_to_solartime
?convert_k600_to_kGAS
?convert_PAR_to_SW




# need light dat + depth dat
Blackwood_prelim3Q
#Rename values to better fit within df
names(WardSNOTEL_ex)[2] <- "PrecipAccum.aveR" 
names(Blackwood_prelim3Q)[6] <- 

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


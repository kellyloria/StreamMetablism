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

# Ward creek "10336676" 
siteNumber <- "10336676" 
SagehenInfo <- readNWISsite(siteNumber)
parameterCd <- c("00060", 
                 "00025") #barometric pressure not looking like any of it works 
#Raw daily data:
qwData_ward <- readNWISdv(siteNumber, parameterCd,
                          "2020-09-01","2020-11-20")
plot(qwData_ward$Date, qwData_ward$X_00060_00003)
qwData_ward$site <- "Ward"

# infill some means
# qwData_ward$flow <- rollapply(qwData_ward$X_00060_00003, width=3, 
#                               FUN=function(x) mean(x, na.rm=TRUE), by=1, 
#                               by.column=TRUE, partial=TRUE, fill=NA, align="center")

qwData_ward$Qs <- replace(qwData_ward$X_00060_00003, qwData_ward$X_00060_00003==0,0.001)

plot(qwData_ward$Date, qwData_ward$Qs)
# Coarse vector for baro pressure:"2020-09-18" "2020-10-29" Pressure (Hg)
qwData_wardQ$baroRaw <- c(23.9, 23.9, 23.9, 24, 23.9, 23.9, 23.9, 24.1, 24.1, 24.1, 
                         24.1, 24.1, 24.1, 24, 24, 24.1, 24, 23.9, 23.9, 23.9, 
                         23.9, 24, 24.1, 24.1, 24.1, 24.1, 24.1, 24.1, 24, 24, 
                         24, 23.9, 23.9, 23.9, 23.9, 23.8, 23.8, 24, 24, 24, 24,
                         24)
qwData_wardQ$baroRawMiliBar <- qwData_wardQ$baroRaw * 33.8639
dim(qwData_wardQ)

# Left join flow data with DO dat: Ward_prelim3Q
Ward_prelim3Q$date <- as.Date(Ward_prelim3Q$timestamp)
Q_Ward <- left_join(Ward_prelim3Q, qwData_wardQ[c("Date", "X_00060_00003", "Qs", "baroRawMiliBar")],
                           by = c("date" = "Date"))
summary(Q_Ward)

Q_Ward$depth <- calc_depth(Q=u(Q_Ward$Qs, "m^3 s^-1"), f=u(0.36))

# calc light example
latitude <- c(39.133221)
longitude <- c(-120.158530) 

Q_Ward$solar.time <- calc_solar_time(Q_Ward$timestamp, longitude)

Q_Ward$light <- calc_light(
  Q_Ward$solar.time,
  latitude,
  longitude,
  max.PAR = u(2326, "umol m^-2 s^-1"),
  attach.units = is.unitted(Q_Ward$solar.time)
)

# ?calc_solar_time
# ?calc_light
# ?calc_DO_sat

Q_Ward$DO_sat <- calc_DO_sat(Q_Ward$Temperature, 
                                    press=Q_Ward$baroRawMiliBar, sal=0) # still need to get barometric pressure data

# Check the input data format:
?mm_data
metab_inputs('mle', 'data')

###
# Get data in correct name and column form
names(Q_Ward)

colnames(Q_Ward)[5] <- "temp.water"
colnames(Q_Ward)[6] <- "DO.obs"
colnames(Q_Ward)[19] <- "DO.sat"
colnames(Q_Ward)[14] <- "discharge"

# New named df
WCdat <- subset(Q_Ward, select= c(solar.time, DO.obs, DO.sat, depth, temp.water, light, discharge))

# write.csv(BWdat, paste0(outputDir, "/BWdat.csv")) # complied data file 

## ---------------------------
# GPP visualization + modeling
WCdat %>% unitted::v() %>%
  mutate(DO.pctsat = 100 * (DO.obs / DO.sat)) %>%
  select(solar.time, starts_with('DO')) %>%
  gather(type, DO.value, starts_with('DO')) %>%
  mutate(units=ifelse(type == 'DO.pctsat', 'DO\n(% sat)', 'DO\n(mg/L)')) %>%
  ggplot(aes(x=solar.time, y=DO.value, color=type)) + geom_line() +
  facet_grid(units ~ ., scale='free_y') + theme_bw() +
  scale_color_discrete('variable')

labels <- c(depth='depth\n(m)', temp.water='water temp\n(deg C)', light='PAR\n(umol m^-2 s^-1)')
WCdat %>% unitted::v() %>%
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
# fit a basic MLE model
mm <- metab(specs(mm_name('mle')), data=WCdat, info='my info')
predict_metab(mm)
get_info(mm)
get_fitting_time(mm)

# mm <- mm_name('mle', ode_method='euler') %>%
#   specs(init.GPP.daily=40) %>%
#   metab(data=WCdat)
# predict_metab(mm)

# Panel plot for raw data
WC_RawPlot1<- WCdat %>% unitted::v() %>%
  mutate(DO.pctsat = 100 * (DO.obs / DO.sat)) %>%
  select(solar.time, starts_with('DO')) %>%
  gather(type, DO.value, starts_with('DO')) %>%
  mutate(units=ifelse(type == 'DO.pctsat', 'DO\n(% sat)', 'DO\n(mg/L)')) %>%
  ggplot(aes(x=solar.time, y=DO.value, color=type)) + geom_line() +
  facet_grid(units ~ ., scale='free_y') + theme_bw() + xlab("") + ggtitle("Ward Creek") +
  ylab("") +
  scale_color_manual(values = c("#B9C252", "#0B322F", "#1f838c")) 

labels <- c(depth='depth\n(m)', temp.water='water temp\n(deg C)', light='PAR\n(umol m^-2 s^-1)')
WC_RawPlot2 <- WCdat %>% unitted::v() %>%
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
RawDat <- grid.arrange(WC_RawPlot1, WC_RawPlot2,
                         nrow = 2, heights=c(1.5,2))
ggsave(paste0(outputDir,"/MSM_WardRawModelData.pdf"), RawDat, scale = 1, width =15, height = 20, units = c("cm"), dpi = 500)

# Panel plot for Modeled data
WC_ModelDOPlot <- plot_DO_preds(predict_DO(mm))

WC_ModelMetabPlot <- plot_metab_preds(mm)

ModelDat <- grid.arrange(WC_ModelDOPlot, WC_ModelMetabPlot,
                       nrow = 2, heights=c(2,1.5))
#ggsave(paste0(outputDir,"/MSM_WardMetabModelData.pdf"), ModelDat, scale = 1, width =15, height = 20, units = c("cm"), dpi = 500)



## ---------------------------
# At General






























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
#bayes_specs <- specs(bayes_name, burnin_steps=100, saved_steps=200, n_cores=1, GPP_daily_mu=3, GPP_daily_sigma=2)
# another way: use revise()
bayes_specs <- revise(bayes_specs, burnin_steps=100, saved_steps=200, n_cores=1, GPP_daily_mu=3, GPP_daily_sigma=2)

# Fitting the model:
#   Once a model has been configured, you can fit the model to data with metab(). 
#   Bayesian models take a while to run, so be patient. Or switch to an MLE model if you can afford to sacrifice some accuracy for speed. 
#   (This small example usually takes about 30 seconds on my computer.)

mm2 <- metab(bayes_specs, data=WCdat) 


# Here are the daily metabolism predictions from the model:
predict_metab(mm2)

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

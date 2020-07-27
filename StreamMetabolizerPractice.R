## ---------------------------
## Practice space for stream metabolizer package used in Appling et al. 2018
## http://usgs-r.github.io/streamMetabolizer/articles/get_started.html
##
## Author: Kelly A. Loria
## Date Created: 2020-07-26
## Email: kelly.loria@nevada.unr.edu
##
## ---------------------------
# Installing streamMetabolizer
remotes::install_github('appling/unitted')
remotes::install_github("USGS-R/streamMetabolizer")
#
## ---------------------------
## Load packages:
library(StreamMetabolism)
library(dplyr)
##
## ---------------------------
# File path setup:
if (dir.exists('/Users/kellyloria/Documents/UNR_2020/Fall2020Projects')){
  inputDir<- '/Users/kellyloria/Documents/UNR_2020/Fall2020Projects/StreamMetablismPractice_Data'
  outputDir<- '/Users/kellyloria/Documents/UNR_2020/Fall2020Projects/StreamMetablismPractice_Output' 
}

## ---------------------------
# Data Preparation:
library(streamMetabolizer)
dat <- data_metab(num_days='3', res='15', attach.units=TRUE)





## ---------------------------
# Preparing the input data:
dat <- data_metab(num_days='3', res='15', day_start=4, day_end=28, attach.units=TRUE)

## ---------------------------
# Configuring the model: 

# There are two steps to configuring a metabolism model in streamMetabolizer.
#     a. Identify the name of the model structure you want using mm_name().
#     b. Set the specifications for the model using defaults fromspecs() as a starting point

# Find the name of a model by its features
#     Find function at: https://github.com/USGS-R/streamMetabolizer/blob/master/R/mm_name.R
mm_name <- function(
  type=c('mle','bayes','night','Kmodel','sim'), 
  #pool_GPP='none', pool_ER='none', pool_eoi='alldays', pool_epc='alldays', pool_epi='alldays',
  pool_K600=c('none',
              'normal','normal_sdzero','normal_sdfixed',
              'linear','linear_sdzero','linear_sdfixed',
              'binned','binned_sdzero','binned_sdfixed',
              'complete'),
  err_obs_iid=c(TRUE, FALSE),
  err_proc_acor=c(FALSE, TRUE),
  err_proc_iid=c(FALSE, TRUE),
  err_proc_GPP=c(FALSE, TRUE),
  ode_method=c('trapezoid','euler','rk2','lsoda','lsode','lsodes','lsodar','vode','daspk',
               'rk4','ode23','ode45','radau','bdf','bdf_d','adams','impAdams','impAdams_d',
               'Euler','pairmeans','NA'),
  GPP_fun=c('linlight','satlight','satlightq10temp','NA'),
  ER_fun=c('constant','q10temp','NA'),
  deficit_src=c('DO_mod','DO_obs','DO_obs_filter','NA'),
  engine=c('stan','nlm','lm','mean','loess','rnorm'),
  check_validity=TRUE) {
  
  # determine type
  type <- match.arg(type)
  
  # set type-specific defaults where values weren't specified
  . <- '.dplyr.var'
  if(type != 'Kmodel') {
    relevant_args <- names(formals(mm_name)) %>% .[!(. %in% c('type','check_validity'))]
  } else {
    # only one argument allowed for Kmodel
    relevant_args <- 'engine' 
    # directly specify all the rest
    pool_K600='complete'
    pool_all='complete'
    err_obs_iid=FALSE
    err_proc_acor=FALSE
    err_proc_iid=FALSE
    err_proc_GPP=FALSE
    ode_method='NA'
    GPP_fun='NA'
    ER_fun='NA'
    deficit_src='NA'
  }
  given_args <- names(match.call()[-1])
  missing_args <- relevant_args[!(relevant_args %in% given_args)]
  if(length(missing_args) > 0) {
    default_args <- mm_parse_name(mm_valid_names(type)[1])
    for(ms in missing_args) {
      assign(ms, default_args[[ms]])
    }
  }
  
  # check arguments and throw errors as needed. these checks define the names 
  # that are possible to create; will be supplemented by call to mm_valid_names 
  # to see if a specific arg combo is actually implemented
  if(type != 'Kmodel') {
    pool_K600 <- match.arg(pool_K600)
    pool_all <- if(pool_K600 == 'none') 'none' else 'partial'
    if(!is.logical(err_obs_iid) || length(err_obs_iid) != 1) stop("need err_obs_iid to be a logical of length 1")
    if(!is.logical(err_proc_acor) || length(err_proc_acor) != 1) stop("need err_proc_acor to be a logical of length 1")
    if(!is.logical(err_proc_iid) || length(err_proc_iid) != 1) stop("need err_proc_iid to be a logical of length 1")
    if(!is.logical(err_proc_GPP) || length(err_proc_GPP) != 1) stop("need err_proc_GPP to be a logical of length 1")
    ode_method <- match.arg(ode_method)
    if(ode_method %in% c('Euler','pairmeans'))
      warning("for ode_method, 'Euler' and 'pairmeans' are deprecated in favor of 'euler' and 'trapezoid'")
    GPP_fun <- match.arg(GPP_fun)
    ER_fun <- match.arg(ER_fun)
    deficit_src <- match.arg(deficit_src)
  } else {
    if(any(!(given_args %in% c('type','engine','check_validity'))))
      stop("for Kmodel, only type, engine, and check_validity may be specified")
  }
  engine <- match.arg(engine)
  if(!(engine %in% list(bayes='stan', mle='nlm', night='lm', Kmodel=c('mean','lm','loess'), sim='rnorm')[[type]]))
    stop("mismatch between type (",type,") and engine (",engine,")")
  
  # make the name
  mmname <- paste0(
    c(bayes='b', mle='m', night='n', Kmodel='K', sim='s')[[type]], '_',
    c(none='', normal='Kn', linear='Kl', binned='Kb', complete='Kc')[[strsplit(pool_K600, '_')[[1]][[1]]]],
    c(none_or_fitted='', sdzero='0', sdfixed='x')[[tryCatch(strsplit(pool_K600, '_')[[1]][[2]], error=function(e) 'none_or_fitted')]],
    c(none='np', partial='', complete='')[[pool_all]], '_',
    if(err_obs_iid) 'oi', if(err_proc_acor) 'pc', if(err_proc_iid) 'pi', if(err_proc_GPP) 'pp', '_',
    c(Euler='Eu', pairmeans='pm', trapezoid='tr', rk2='r2', 
      lsoda='o1', lsode='o2', lsodes='o3', lsodar='o4', vode='o5', daspk='o6', euler='eu', rk4='o8', 
      ode23='o9', ode45='o10', radau='o11', bdf='o12', bdf_d='o13', adams='o14', impAdams='o15', impAdams_d='o16',
      'NA'='')[[ode_method]], '_',
    c(linlight='pl', satlight='ps', satlightq10temp='pq', 'NA'='')[[GPP_fun]],
    c(constant='rc', q10temp='rq', 'NA'='')[[ER_fun]],
    c(DO_mod='km', DO_obs='ko', DO_obs_filter='kf', 'NA'='')[[deficit_src]], 
    '.', engine)
  
  # check validity if requested
  check_validity <- if(!is.logical(check_validity)) stop("need check_validity to be a logical of length 1") else check_validity[1]
  if(isTRUE(check_validity)) mm_validate_name(mmname)
  
  # return
  mmname
}


## ---------------------------
# Need to define specs function: https://github.com/USGS-R/streamMetabolizer/blob/master/R/specs.R
specs <- function(
  
  ## All or several models
  
  model_name = mm_name(),
  engine,
  
  # inheritParams mm_model_by_ply
  day_start = 4,
  day_end = 28,
  
  # inheritParams mm_is_valid_day
  day_tests=c('full_day', 'even_timesteps', 'complete_data', 'pos_discharge', 'pos_depth'),
  required_timestep=NA,
  
  
  ## MLE
  
  # initial values
  init.GPP.daily = 8, 
  init.Pmax = 10,
  init.alpha = 0.0001,
  init.ER.daily = -10, 
  init.ER20 = -10,
  init.K600.daily = 10,
  
  
  ## Bayes
  
  # model setup
  split_dates,
  keep_mcmcs = TRUE,
  keep_mcmc_data = TRUE,
  
  # hyperparameters for non-hierarchical GPP & ER
  GPP_daily_mu = 3.1,
  GPP_daily_lower = -Inf,
  GPP_daily_sigma = 6.0,
  alpha_meanlog = -4.6,
  alpha_sdlog = 0.5,
  Pmax_mu = 10,
  Pmax_sigma = 7,
  ER_daily_mu = -7.1,
  ER_daily_upper = Inf,
  ER_daily_sigma = 7.1,
  
  # hyperparameters for non-hierarchical K600
  K600_daily_meanlog = log(12),
  
  # hyperparameters for hierarchical K600 - normal
  K600_daily_meanlog_meanlog = log(12),
  K600_daily_meanlog_sdlog = 1.32,
  
  # hyperparameters for hierarchical K600 - linear. defaults should be
  # reasonably constrained, not too wide
  lnK600_lnQ_intercept_mu = 2,
  lnK600_lnQ_intercept_sigma = 2.4,
  lnK600_lnQ_slope_mu = 0,
  lnK600_lnQ_slope_sigma = 0.5,
  
  # hyperparameters for hierarchical K600 - binned. K600_daily ~ 
  # lognormal(K600_daily_nodes_meanlog[lnQ_bin], 
  # K600_daily_nodes_sdlog[lnQ_bin]) with linear interpolation among bins before
  # exponentiating. nodes_meanlog and nodes_sdlog may be length b = 
  # length(K600_daily_lnQ_nodes) or length 1 (to be replicated to length b). 
  # -8:6 covers almost all points in Raymond et al. 2012 and will therefore 
  # always be too broad a range for a single stream. -3:3 will catch some
  # streams to rivers as a first cut, though users should still modify
  K600_lnQ_nodes_centers = -3:3, # the x=lnQ values for the nodes
  K600_lnQ_nodediffs_sdlog = 0.5, # for centers 1 apart; for centers 0.2 apart, use 1/5 of this
  K600_lnQ_nodes_meanlog = rep(log(12), length(K600_lnQ_nodes_centers)), # distribs for the y=K600 values of the nodes
  K600_lnQ_nodes_sdlog = rep(1.32, length(K600_lnQ_nodes_centers)),
  
  # hyperparameters for any K pooling or non-pooling strategy
  K600_daily_sdlog = switch(mm_parse_name(model_name)$pool_K600, none=1, normal_sdfixed=0.05, NA),
  K600_daily_sigma = switch(mm_parse_name(model_name)$pool_K600, linear_sdfixed=10, binned_sdfixed=5, NA),
  K600_daily_sdlog_sigma = switch(mm_parse_name(model_name)$pool_K600, normal=0.05, NA),
  K600_daily_sigma_sigma = switch(mm_parse_name(model_name)$pool_K600, linear=1.2, binned=0.24, NA),
  # normal_sdzero, linear_sdzero, and binned_sdzero all have no parameters for this
  
  # hyperparameters for error terms
  err_obs_iid_sigma_scale = 0.03,
  err_proc_iid_sigma_scale = 5,
  err_proc_acor_phi_alpha = 1,
  err_proc_acor_phi_beta = 1,
  err_proc_acor_sigma_scale = 1,
  err_mult_GPP_sdlog_sigma = 1,
  
  # vector of hyperparameters to include as MCMC data
  params_in,
  
  # inheritParams runstan_bayes
  params_out,
  n_chains = 4,
  n_cores = 4,
  burnin_steps = 500,
  saved_steps = 500,
  thin_steps = 1,
  verbose = FALSE,
  
  
  ## Kmodel
  
  #inheritParams prepdata_Kmodel
  weights = c("K600/CI"), # 'K600/CI' is argued for in stream_metab_usa issue #64
  filters = c(CI.max=NA, discharge.daily.max=NA, velocity.daily.max=NA),
  
  #inheritParams Kmodel_allply
  predictors = c("discharge.daily"), 
  transforms = c(K600='log', date=NA, velocity.daily="log", discharge.daily="log"),
  other_args = c(),
  
  
  ## Sim
  
  # multi-day simulation parameters. already above for bayes:
  # K600_lnQ_nodes_centers, K600_lnQ_nodediffs_sdlog
  K600_lnQ_cnode_meanlog = log(6), # distrib for the y=K600 values of the middle (or just past middle) node
  K600_lnQ_cnode_sdlog = 1, # distrib for the y=K600 values of the middle (or just past middle) node
  K600_lnQ_nodediffs_meanlog = 0.2, # non-zero introduces a trend in K ~ Q
  lnK600_lnQ_nodes = function(K600_lnQ_nodes_centers, K600_lnQ_cnode_meanlog, K600_lnQ_cnode_sdlog,
                              K600_lnQ_nodediffs_meanlog, K600_lnQ_nodediffs_sdlog, ...) {
    sim_Kb(K600_lnQ_nodes_centers, K600_lnQ_cnode_meanlog, K600_lnQ_cnode_sdlog,
           K600_lnQ_nodediffs_meanlog, K600_lnQ_nodediffs_sdlog)
  },
  
  # daily simulation parameters
  discharge_daily = function(n, ...) rnorm(n, 20, 3),
  DO_mod_1 = NULL,
  K600_daily = function(n, K600_daily_predlog=log(10), ...) pmax(0, rnorm(n, K600_daily_predlog, 4)),
  GPP_daily = function(n, ...) pmax(0, rnorm(n, 8, 4)),
  Pmax = function(n, ...) pmax(0, rnorm(n, 10, 2)),
  alpha = function(n, ...) pmax(0, rnorm(n, 0.0001, 0.00002)),
  ER_daily = function(n, ...) pmin(0, rnorm(n, -10, 5)),
  ER20 = function(n, ...) pmin(0, rnorm(n, -10, 4)),
  
  # sub-daily simulation parameters
  err_obs_sigma = 0.01,
  err_obs_phi = 0,
  err_proc_sigma = 0.2,
  err_proc_phi = 0,
  err_round = NA,
  
  # simulation replicability
  sim_seed = NA
  
) {
  
  # make it easier to enter custom specs by creating the type-specific default if model_name %in% 'mle', etc.
  if(model_name %in% eval(formals(mm_name)$type))
    model_name <- mm_name(type=model_name)
  
  # check the validity of the model_name against the list of officially accepted model names
  mm_validate_name(model_name)
  
  # parse the model_name
  features <- mm_parse_name(model_name, expand=TRUE)
  
  # collect info about the arguments
  required <- 'model_name'
  all_possible <- names(formals(specs))
  not_missing <- names(as.list(match.call())[-1]) # the arguments that were given explicitly
  yes_missing <- all_possible[!(all_possible %in% not_missing)]
  prefer_missing <- setdiff(all_possible[sapply(formals(specs), is.symbol)], 'params_out') # the arguments w/o defaults, mostly
  prefer_not_missing <- if(features$type == 'bayes' && features$GPP_fun == 'satlight') {
    c('alpha_meanlog', 'alpha_sdlog', 'Pmax_mu', 'Pmax_sigma') 
  } else {
    c() # could be made more extensive
  }
  
  # argument checks
  if(any(required %in% yes_missing))
    stop("missing and required argument: ", paste(required[required %in% yes_missing], collapse=", "))
  if(any(prefer_not_missing %in% yes_missing)) {
    warn_about <- prefer_not_missing[prefer_not_missing %in% yes_missing]
    warning("you should specify site-appropriate values for all parameters and especially ", paste(warn_about, collapse=", "))
  }
  redundant <- not_missing[not_missing %in% prefer_missing]
  if('engine' %in% redundant) {
    warning("'engine' should be specified in mm_name() rather than specs()")
    redundant <- redundant[redundant != 'engine']
  }
  if(length(redundant) > 0) {
    warning("argument[s] that should usually be specified in revise() rather than specs(): ", paste(redundant, collapse=", "))
  }
  
  # collect the defaults + directly specified arguments
  all_specs <- as.list(environment())
  
  # copy/calculate arguments as appropriate to the model
  specs <- list()
  switch(
    features$type,
    'bayes' = {
      
      # list the specs that will make it all the way to the Stan model as data
      all_specs$params_in <- c(
        switch(
          features$GPP_fun,
          linlight=c('GPP_daily_mu','GPP_daily_lower','GPP_daily_sigma'),
          satlight=c('alpha_meanlog', 'alpha_sdlog', 'Pmax_mu', 'Pmax_sigma')),
        c('ER_daily_mu','ER_daily_upper','ER_daily_sigma'),
        switch(
          features$pool_K600_type,
          none=c('K600_daily_meanlog'),
          normal=c('K600_daily_meanlog_meanlog', 'K600_daily_meanlog_sdlog'),
          linear=c('lnK600_lnQ_intercept_mu', 'lnK600_lnQ_intercept_sigma', 'lnK600_lnQ_slope_mu', 'lnK600_lnQ_slope_sigma'),
          binned=c('K600_lnQ_nodediffs_sdlog', 'K600_lnQ_nodes_meanlog', 'K600_lnQ_nodes_sdlog')),
        switch(
          features$pool_K600_sd,
          zero=c(),
          fixed=switch(features$pool_K600_type, none=, normal='K600_daily_sdlog', linear=, binned='K600_daily_sigma'),
          fitted=switch(features$pool_K600_type, normal='K600_daily_sdlog_sigma', linear=, binned='K600_daily_sigma_sigma')
        ),
        if(features$err_obs_iid) 'err_obs_iid_sigma_scale',
        if(features$err_proc_acor) c('err_proc_acor_phi_alpha', 'err_proc_acor_phi_beta', 'err_proc_acor_sigma_scale'),
        if(features$err_proc_iid) 'err_proc_iid_sigma_scale',
        if(features$err_proc_GPP) 'err_mult_GPP_sdlog_sigma'
      )
      
      # list all needed arguments
      included <- c(
        # model setup
        'model_name', 'engine', 'split_dates', 'keep_mcmcs', 'keep_mcmc_data',
        
        # date ply day_tests
        'day_start', 'day_end', 'day_tests', 'required_timestep',
        
        # discharge binning parameters are not params_in, though they're 
        # conceptually related and therefore colocated in formals(specs)
        if(features$pool_K600_type == 'binned') c('K600_lnQ_nodes_centers'),
        
        # params_in is both a vector of specs to include and a vector to include in specs
        all_specs$params_in, 'params_in',
        
        # inheritParams runstan_bayes
        'params_out', 'n_chains', 'n_cores', 
        'burnin_steps', 'saved_steps', 'thin_steps', 'verbose'
      )
      
      # compute some arguments
      if('engine' %in% yes_missing) {
        all_specs$engine <- features$engine
      }
      if('split_dates' %in% yes_missing) {
        all_specs$split_dates <- switch(
          features$pool_K600_type,
          'none' = FALSE, # pretty sure FALSE is faster. also allows hierarchical error terms
          'normal'=, 'linear'=, 'binned' = FALSE, 
          stop("unknown pool_K600; unsure how to set split_dates"))
      }
      if(features$pool_K600_type == 'binned') {
        # defaults are for linear pool_K600 & need adjustment for binned method
        all_specs$K600_daily_beta_mu <- rep(10, length(all_specs$K600_daily_lnQ_nodes))
        all_specs$K600_daily_beta_sigma <- rep(10, length(all_specs$K600_daily_lnQ_nodes))
      }
      if('params_out' %in% yes_missing) {
        all_specs$params_out <- c(
          c('GPP', 'ER', 'DO_R2'),
          switch(
            features$GPP_fun,
            linlight=c('GPP_daily'),
            satlight=c('alpha', 'Pmax')),
          c('ER_daily', 'K600_daily'),
          switch(
            features$pool_K600_type,
            none=c(),
            normal=c('K600_daily_predlog'),
            linear=c('K600_daily_predlog', 'lnK600_lnQ_intercept', 'lnK600_lnQ_slope'),
            binned=c('K600_daily_predlog', 'lnK600_lnQ_nodes')), 
          if(features$pool_K600_sd == 'fitted')
            switch(
              features$pool_K600_type,
              normal='K600_daily_sdlog',
              linear=, binned='K600_daily_sigma'),
          if(features$err_obs_iid) c('err_obs_iid_sigma', 'err_obs_iid'),
          if(features$err_proc_acor) c('err_proc_acor', 'err_proc_acor_phi', 'err_proc_acor_sigma'),
          if(features$err_proc_iid) c('err_proc_iid_sigma', 'err_proc_iid'),
          if(features$err_proc_GPP) c('err_proc_GPP', 'GPP_pseudo_R2'))
      }
      
      # check for errors/inconsistencies
      model_path <- tryCatch(
        mm_locate_filename(model_name), 
        error=function(e) {
          warning(e)
          return(model_name)
        })
      if(features$engine == "NA") 
        stop('engine must be specified for Bayesian models')
      
    },
    'mle' = {
      # determine which init values will be needed
      . <- '.dplyr.var'
      init.needs <- paste0('init.', get_param_names(model_name)$required)
      
      # list all needed arguments
      included <- c('model_name', 'day_start', 'day_end', 'day_tests', 'required_timestep', init.needs)
      
    }, 
    'night' = {
      # list all needed arguments
      included <- c('model_name', 'day_start', 'day_end', 'day_tests', 'required_timestep')
      
      # some different defaults for night relative to other models
      if('day_start' %in% yes_missing) {
        all_specs$day_start <- 12
      }
      if('day_end' %in% yes_missing) {
        all_specs$day_end <- 36
      }
      if('day_tests' %in% yes_missing) {
        all_specs$day_tests <- c(day_tests, 'include_sunset')
      }
      
    }, 
    'Kmodel' = {
      # list all needed arguments
      included <- c(
        'model_name', 'engine', 'day_start', 'day_end', 'day_tests', 'required_timestep',
        'weights', 'filters', 'predictors', 'transforms', 'other_args')
      
      if('engine' %in% yes_missing) {
        all_specs$engine <- features$engine
      }
      
      # some different defaults for each engine, because no one set of defaults
      # makes sense for all engines
      #if('weights' %in% yes_missing) all_specs$weights <- c("K600/CI") # same for all, so use default as in Usage
      switch(
        all_specs$engine,
        mean={
          if('filters' %in% yes_missing) all_specs['filters'] <- list(c()) # need special syntax to assign c(). see https://stackoverflow.com/a/7945259/3203184
          if('predictors' %in% yes_missing) all_specs['predictors'] <- list(c())
          if('transforms' %in% yes_missing) all_specs$transforms <- c(K600='log')
          if('other_args' %in% yes_missing) all_specs$other_args <- list(possible_args=NULL)
        },
        lm={
          if('filters' %in% yes_missing) all_specs$filters <- c(CI.max=NA, discharge.daily.max=NA)
          if('predictors' %in% yes_missing) all_specs$predictors <- c("discharge.daily")
          if('transforms' %in% yes_missing) all_specs$transforms <- c(K600='log', discharge.daily="log")
          if('other_args' %in% yes_missing) all_specs$other_args <- list(possible_args=names(formals(lm))[-which(names(formals(lm)) %in% c('formula','data','weights'))])
        },
        loess={
          if('filters' %in% yes_missing) all_specs$filters <- c(CI.max=NA, discharge.daily.max=NA, velocity.daily.max=NA)
          if('predictors' %in% yes_missing) all_specs$predictors <- c("date", "discharge.daily")
          if('transforms' %in% yes_missing) all_specs$transforms <- c(K600='log', date=NA, velocity.daily="log", discharge.daily="log")
          if('other_args' %in% yes_missing) all_specs$other_args <- list(possible_args=names(formals('loess'))[-which(names(formals('loess')) %in% c('formula','data','weights'))])
        }
      )
      
    },
    'sim' = {
      # determine which daily parameters will be needed
      par_needs <- gsub('\\.', '_', unlist(get_param_names(model_name)[c('optional','required')]))
      
      # list all needed arguments
      included <- c(
        'model_name', 'day_start', 'day_end', 'day_tests', 'required_timestep',
        switch(
          features$pool_K600,
          none=c(),
          normal=stop("pool_K600='normal' unavailable for now; try 'binned' instead"),
          linear=stop("pool_K600='linear' unavailable for now; try 'binned' instead"), # 'discharge_daily', etc.
          binned=c('K600_lnQ_nodes_centers', 
                   'K600_lnQ_cnode_meanlog', 'K600_lnQ_cnode_sdlog', 'K600_lnQ_nodediffs_meanlog', 'K600_lnQ_nodediffs_sdlog',
                   'lnK600_lnQ_nodes')),
        par_needs, 'err_round', 'sim_seed')
      
      if(features$pool_K600 == 'binned') {
        if('K600_lnQ_nodes_centers' %in% yes_missing) # override the default, which is for 'bayes' rather than 'sim'
          all_specs$K600_lnQ_nodes_centers <- function(discharge.daily, ...) calc_bins(log(discharge.daily), 'width', width=0.2)$bounds
      }
    }
  )
  
  # stop if truly irrelevant arguments were given
  if(length(irrelevant <- not_missing[!(not_missing %in% included)]) > 0) 
    stop("irrelevant argument: ", paste(irrelevant, collapse=", "))
  
  # return just the arguments we actually need
  add_specs_class(all_specs[included])
  
}

bayes_specs <- specs(bayes_name)
bayes_specs




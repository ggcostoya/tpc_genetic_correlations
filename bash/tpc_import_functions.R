#### tpc_import_functions.R
#### trevor faske
#### 11/05/2021

## Load necessary packages

## load packages & register cores for parallel running ------------------------

suppressWarnings(suppressPackageStartupMessages(library(tidyverse)))
suppressWarnings(suppressPackageStartupMessages(library(parallel)))
suppressWarnings(suppressPackageStartupMessages(library(foreach)))
suppressWarnings(suppressPackageStartupMessages(library(nlstools)))
suppressWarnings(suppressPackageStartupMessages(library(data.table)))
suppressWarnings(suppressPackageStartupMessages(library(DescTools)))
suppressWarnings(suppressPackageStartupMessages(library(iterators)))

##########################################################################

## Randomize TPTs

randomize_tpts <- function(tpts, corr, s){
  
  # build basic genetic correlations matrix
  gmtx <- diag(length(tpts))
  
  # modify genetic correlations matrix depending on tradeoff
  
  # gsto
  if(corr %in% c("gsto", "both")){gmtx[5,1] <- gmtx[1,5] <- gmtx[5,2] <- gmtx[2,5] <- s}
  if(corr %in% c("gsto", "both")){gmtx[5,4] <- gmtx[4,5] <- -s}
  
  # tde
  if(corr %in% c("tde", "both")){gmtx[5,3] <- gmtx[3,5] <- s}
  
  # both
  if(corr == "both"){gmtx[3,1] <- gmtx[1,3] <- gmtx[3,2] <- gmtx[2,3] <- s}
  if(corr == "both"){gmtx[4,3] <- gmtx[3,4] <- -s}
  
  # initial amount of change in tpts
  initial_change <- c(rnorm(4), rnorm(1, sd = 1/3))
  
  # amount of change due to genetic correlations
  change <- initial_change %*% gmtx
  
  # new tpt values
  new_tpts <- as.vector(tpts + change)
  
  # correct for impossible values
  
  # mid
  new_tpts[2] <- ifelse(new_tpts[2] > new_tpts[3] - 1, new_tpts[3] - 1, new_tpts[2])
  
  # ctmin
  new_tpts[1] <- ifelse(new_tpts[1] > new_tpts[2] - 1, new_tpts[1] - 1, new_tpts[1])
  
  # ctmax
  new_tpts[4] <- ifelse(new_tpts[4] < new_tpts[3] + 1, new_tpts[3] + 1, new_tpts[4])
  
  # round final values to two decimal places
  new_tpts <- round(new_tpts, digits = 2)
  
  return(new_tpts)
  
}

## Build TPD

build_tpd <- function(tpts){
  
  # temperature
  t <- round(tpts[1:4], 1)
  
  # performance
  pmax <- tpts[5]
  p <- c(0, pmax/2, pmax, 0)
  
  return(data.frame(t,p))
  
}

## Generate a TPC

generate_tpc <- function(tpd){
  
  # temperature
  t <- round(seq(0, 50, by = 0.1), digits = 1)
  
  # holder performance
  p <- rep(NA, length(t))
  
  # set zero p for below and above ctmin and ctmax
  p[1:which(t == tpd$t[1])] <- p[which(t == tpd$t[4]):length(t)] <- 0
  
  # get indexes for critical points
  ctmin <- which(t == tpd$t[1])
  mid <- which(t == tpd$t[2])
  topt <- which(t == tpd$t[3])
  ctmax <- which(t == tpd$t[4])
  
  # set performance values for each vector
  p[ctmin:mid] <- seq(0, tpd$p[2], length.out = length(p[ctmin:mid]))
  p[mid:topt] <- seq(tpd$p[2], tpd$p[3], length.out = length(p[mid:topt]))
  p[topt:ctmax] <- seq(tpd$p[3], 0, length.out = length(p[topt:ctmax]))
  
  return(tibble(t = t, p = p))
  
}

## Generate a Population

generate_population <- function(n, tpts, corr, s){
  
  # loop top generate population
  pop <- foreach(i = 1:n,
                 combine = rbind,
                 .packages = "tidyverse",
                 .export = c("randomize_tpts", "build_tpd", "generate_tpc")) %dopar% {
                   
                   tpc <- nest(generate_tpc(build_tpd(randomize_tpts(tpts = tpts, corr = corr, s = s))))
                   
                 }
  
  # reshape population object
  pop <- do.call(rbind, pop) %>% mutate(tpc = data) %>% select(tpc)
  
  return(pop)
}

## Generate a Temperature Sequence

generate_tseq <- function(gen, days, mean_0, breath_0, mean_c, breath_c, burnin){
  
  # sequence of mean temperature values
  means <- seq(mean_0, mean_0 + mean_c, length.out = gen)
  burnin_means <- rep(mean_0, burnin)
  means <- c(burnin_means, means)
  
  # determine standard deviation from breath
  sds <- seq(breath_0, breath_0 + breath_c, length.out = gen) / 6
  burnin_sds <- rep(breath_0 / 6, burnin)
  sds <- c(burnin_sds, sds)
  
  # loop to generate thermal sequence
  tseq <- foreach(i  = 1:(gen + burnin)) %do% {
    round(rnorm(n = days, mean = means[i], sd = sds[i]), 1)}
  
  return(tseq)
}

## Simulation

simulation <- function(pop, tseq){
  
  # get the initial population size and set that as cap
  popsize_cap <- nrow(pop)
  
  # set initial population as current population
  current_pop <- pop %>% mutate(gen = rep(0, nrow(.)), r = rep(0, nrow(.)))
  
  # object to store list of population data each generation
  sim_data <- list(current_pop)
  
  # start of the generation loop
  for(i in 1:length(tseq)){
    
    # select the temperature sequence belonging to the current generation & index it
    idx_t <- round((tseq[[i]]) / 0.1)
    
    # loop to determine next population
    next_pop <- foreach(k = 1:nrow(current_pop)) %do% {
      
      # extract individual tpc
      tpc <- sim_data[[i]][[1]][[k]]
      
      # determine daily performance at the env temperature through indexing
      p_et <- tpc$p[idx_t]
      
      # determine daily probability of survival
      p_s <- 1/(1 + exp(-(-5 + 1 * p_et)))
      
      # determine daily survival
      days_s <- as.numeric(runif(length(p_s)) < p_s)
      
      # determine day of death (minimum day with survival = 0)
      min_idx <- suppressWarnings(min(which(days_s == 0)))
      
      # reshape days survived after individual death
      if(min_idx != Inf){days_s[(min_idx + 1):length(days_s)] <- 0}
      
      # determine the sum of days alive
      days_alive <- sum(days_s, na.rm = T)
      
      # determine offspring produced based on days alive
      n_off <- floor(days_alive/10)
      
      # record reproductive output
      sim_data[[i]]$r[[k]] <- n_off
      
      # nest individual tpc data
      tpc <- tpc %>% nest(tpc = c(t,p))
      
      # generate individual offspring
      off <- do.call("rbind", replicate(n_off, tpc, simplify = FALSE))
      
    }
    
    # reshape the next population object
    next_pop <- as_tibble(rbindlist(next_pop))
    
    # break loop if length of the next population is zero (extinction)
    if(nrow(next_pop) == 0){break}
    
    # if the population exceeds the carrying capacity select up to it
    if(nrow(next_pop) > popsize_cap){next_pop <- sample_n(next_pop, popsize_cap)}
    
    # update current population
    current_pop <- next_pop
    
    # add generation and reproduction columns to new offspring
    next_pop <- next_pop %>% mutate(gen = rep(i,), r = rep(0,))
    
    # bind to simulation data
    sim_data <- c(sim_data, list(next_pop))
    
  }
  
  # reshape simulation data output
  sim_data <- suppressWarnings(as_tibble(rbindlist(sim_data)))
  
  # set starting values for TPTs
  topt <- ctmin <- ctmax <- pmax <- mid <- NA
  
  # generate holder tpt columns
  sim_data <- mutate(sim_data, topt, ctmin, ctmax, pmax, mid)
  
  # loop to assign tpt values from TPC
  for(i in 1:nrow(sim_data)){
    
    # extract TPC
    tpc <- sim_data[[1]][[i]]
    
    # extract tpts
    sim_data$pmax[i] <- max(tpc$p)
    sim_data$topt[i] <- tpc$t[tpc$p == max(tpc$p)]
    sim_data$ctmin[i] <- max(tpc$t[tpc$p == 0 & tpc$t < tpc$t[tpc$p == max(tpc$p)]])
    sim_data$ctmax[i] <- min(tpc$t[tpc$p == 0 & tpc$t > tpc$t[tpc$p == max(tpc$p)]])
    mid <- suppressWarnings(tpc$t[tpc$p == Closest(tpc$p, max(tpc$p) / 2)])
    sim_data$mid[i] <- mid[1]
    
  }
  
  # indicate columns to keep
  sim_data <- sim_data %>% select(topt, pmax, ctmin, ctmax, mid, gen, r)
  
  return(sim_data)
  
}

## Predict optimal tpc

predict_optimal <- function(gen0, tseq, replicates){
  
  # get temperature indexes
  indexes <- round(tseq / 0.1)
  
  # add predicted r column
  gen0 <- gen0 %>% mutate(predicted_r = rep(NA, nrow(.)))
  
  # start of loop to predict fitness
  for(i in 1:nrow(gen0)){
    
    # extract TPC
    tpc <- gen0$tpc[[i]]
    
    # determine performance at environmental temperature
    p_et <- tpc$p[indexes]
    
    # determine survival probabilities
    p_s <- 1/(1 + exp(-(-5 + 1 * p_et)))
    
    # days survived holder
    days <- 0
    
    # loop to simulate survival
    for(j in 1:replicates){
      
      # determine days survived
      days_s <- as.numeric(runif(length(p_s)) < p_s)
      
      # select min index with no survival
      min_idx <- suppressWarnings(min(which(days_s == 0)))
      
      # reshape days survived if individual dies
      if(min_idx != Inf){days_s[(min_idx + 1):length(days_s)] <- 0}
      
      # determine days alive
      days_alive <- sum(days_s, na.rm = T)
      
      # add days alive to holder
      days <- days + days_alive
      
    }
    
    # get the mean survival across replicates
    mean_survived_days <- days/replicates
    
    # get the mean offspring produced
    gen0$predicted_r[i] <- mean_survived_days/10
    
  }
  
  return(gen0)
  
  
}

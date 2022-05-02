
### Simulation Functions ####

## Packages needed ----

library(tidyverse)
library(parallel)
library(foreach)
library(doParallel)
library(data.table)
library(DescTools)

registerDoParallel(detectCores()) # Register parallel cores

## Complete simulation function ----

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
      tpc <- sim_data[[i]]$tpc[[k]]

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
    tpc <- sim_data$tpc[[i]]

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

## Summary simulation function ----

simulation_sum <- function(pop, tseq){

  # get initial popsize and set as cap
  popsize_cap <- nrow(pop)

  # setinitial population as current population
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
      tpc <- sim_data[[i]]$tpc[[k]]

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

  # reshape simulation output
  sim_data <- suppressWarnings(as_tibble(rbindlist(sim_data)))

  # count rows per generation
  sim_data <- sim_data %>%
    group_by(gen) %>%
    summarise(as.data.frame(table(gen))) %>%
    ungroup() %>%
    mutate(n = Freq, gen = as.numeric(gen) - 1) %>%
    select(gen, n)

  # complete columns with popsize 0
  gens <- tibble(gen = seq(0,85, by = 1))
  sim_data <- merge(sim_data, gens, by = "gen", all = TRUE)
  sim_data$n <- ifelse(is.na(sim_data$n), 0, sim_data$n)

  return(sim_data)
}

## Function to run multiple sims in parallel Complete ----

parallel_simulation <- function(pops, tseqs, corr_name, tseq_name){

  # define n_k
  n_k <- nrow(pops[1])

  # simulation function
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
        tpc <- sim_data[[i]]$tpc[[k]]

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
      tpc <- sim_data$tpc[[i]]

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

  # define packages needed
  packages <- c("tidyverse", 'parallel', "doParallel",
                "foreach", "data.table", "DescTools")

  # get raw simulation output
  raw_sim <- foreach(i = 1:length(pops), .packages = packages) %dopar% {

    foreach(j = 1:length(tseqs)) %do% {

      sim <- simulation(pop = pops[[i]], tseq = tseqs[[j]])

      sim %>% mutate(pop_n = rep(i, nrow(.)), tseq_n = rep(j, nrow(.)))

    }
  }

  # post process raw sim object
  sim <- do.call(rbind, do.call(c, raw_sim)) %>%
    mutate(corr = rep(corr_name, nrow(.)),
           tseq = rep(tseq_name, nrow(.)),
           n_k = rep(n_k, nrow(.)))

  return(sim)

}

## Function to run multiple sims in parallel Complete ----

parallel_simulation_sum <- function(pops, tseqs, corr_name, tseq_name){

  # define n_k
  n_k <- nrow(pops[1])

  # simulation function
  simulation_sum <- function(pop, tseq){

    # get initial popsize and set as cap
    popsize_cap <- nrow(pop)

    # setinitial population as current population
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
        tpc <- sim_data[[i]]$tpc[[k]]

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

    # reshape simulation output
    sim_data <- suppressWarnings(as_tibble(rbindlist(sim_data)))

    # count rows per generation
    sim_data <- sim_data %>%
      group_by(gen) %>%
      summarise(as.data.frame(table(gen))) %>%
      ungroup() %>%
      mutate(n = Freq, gen = as.numeric(gen) - 1) %>%
      select(gen, n)

    # complete columns with popsize 0
    gens <- tibble(gen = seq(0,85, by = 1))
    sim_data <- merge(sim_data, gens, by = "gen", all = TRUE)
    sim_data$n <- ifelse(is.na(sim_data$n), 0, sim_data$n)

    return(sim_data)
  }

  # define packages needed
  packages <- c("tidyverse", 'parallel', "doParallel",
                "foreach", "data.table", "DescTools")

  # get raw simulation output
  raw_sim <- foreach(i = 1:length(pops), .packages = packages) %dopar% {

    foreach(j = 1:length(tseqs)) %do% {

      sim <- simulation_sum(pop = pops[[i]], tseq = tseqs[[j]])

      sim %>% mutate(pop_n = rep(i, nrow(.)), tseq_n = rep(j, nrow(.)))

    }
  }

  # post process raw sim object
  sim <- do.call(rbind, do.call(c, raw_sim)) %>%
    mutate(corr = rep(corr_name, nrow(.)),
           tseq = rep(tseq_name, nrow(.)),
           n_k = rep(n_k, nrow(.)))

  return(sim)

}



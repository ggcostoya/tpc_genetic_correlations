
## Generate Population Data

## Packages ----

library(tidyverse)
library(parallel)
library(foreach)
library(doParallel)
library(data.table)
library(DescTools)

registerDoParallel(detectCores()) # Register parallel cores

## Functions ----

# randomization of base tpt values
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

# build thermal-performance dataset from tpts
build_tpd <- function(tpts){

  # temperature
  t <- round(tpts[1:4], 1)

  # performance
  pmax <- tpts[5]
  p <- c(0, pmax/2, pmax, 0)

  return(data.frame(t,p))

}

# generate a tpc from thermal-performance dataset
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

# generate a population combining the previous 3 functions
generate_population <- function(reps, n, tpts, corr, s){

  # Loop for all replicates
  pops <- foreach(i = 1:reps) %do% {

    # loop for each replicate
    pop <- foreach(j = 1:n,
                   combine = rbind,
                   .packages = "tidyverse",
                   .export = c("randomize_tpts", "build_tpd", "generate_tpc")) %dopar% {

                     tpc <- nest(generate_tpc(build_tpd(randomize_tpts(tpts = tpts, corr = corr, s = s))))

                   }

    # reshape population object
    do.call(rbind, pop) %>% mutate(tpc = data) %>% select(tpc)

  }

  return(pops)
}

## Constant parameters ----

reps <- 10 # Number of replicates

topt <- 28 # Thermal optimum

pmax <- 10 # Maximum performance

ctmin <- 10 # Critical Thermal Minium

ctmax <- 35 # Critic Thermal Maximum

tpts <- c(ctmin, mean(c(topt, ctmin)), topt, ctmax, pmax) # Thermal Performance Traits

scorr <- 0.75 # Strength of genetic correlation

## Populations ----

# N0 = 50
none_50 <- generate_population(reps, tpts = tpts, n = 50, corr = "none", s = 0)
gsto_50 <- generate_population(reps, tpts = tpts, n = 50, corr = "gsto", s = scorr)
tde_50 <- generate_population(reps, tpts = tpts, n = 50, corr = "tde", s = scorr)
both_50 <- generate_population(reps, tpts = tpts, n = 50, corr = "both", s = scorr)

# No = 500
none_500 <- generate_population(reps, tpts = tpts, n = 500, corr = "none", s = 0)
gsto_500 <- generate_population(reps, tpts = tpts, n = 500, corr = "gsto", s = scorr)
tde_500 <- generate_population(reps, tpts = tpts, n = 500, corr = "tde", s = scorr)
both_500 <- generate_population(reps, tpts = tpts, n = 500, corr = "both", s = scorr)

# No = 5000
none_5000 <- generate_population(reps, tpts = tpts, n = 5000, corr = "none", s = 0)
gsto_5000 <- generate_population(reps, tpts = tpts, n = 5000, corr = "gsto", s = scorr)
tde_5000 <- generate_population(reps, tpts = tpts, n = 5000, corr = "tde", s = scorr)
both_5000 <- generate_population(reps, tpts = tpts, n = 5000, corr = "both", s = scorr)

## Save Data ----

# Set appropiate directory first

save(none_50, file = "none_50.RData")
save(gsto_50, file = "gsto_50.RData")
save(tde_50, file = "tde_50.RData")
save(both_50, file = "both_50.RData")
save(none_500, file = "none_500.RData")
save(gsto_500, file = "gsto_500.RData")
save(tde_500, file = "tde_500.RData")
save(both_500, file = "both_500.RData")
save(none_5000, file = "none_5000.RData")
save(gsto_5000, file = "gsto_5000.RData")
save(tde_5000, file = "tde_5000.RData")
save(both_5000, file = "both_5000.RData")

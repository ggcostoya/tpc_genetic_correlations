## Generate Temperature Sequences

## Packages ----

library(tidyverse)
library(parallel)
library(foreach)
library(doParallel)
library(data.table)
library(DescTools)

## Function for temperature sequence generation ----

generate_tseq <- function(reps, gen, days, mean_0, sd_0, mean_c, sd_c, burnin){

  # sequence of mean temperature values
  means <- seq(mean_0, mean_0 + mean_c, length.out = gen)
  burnin_means <- rep(mean_0, burnin)
  means <- c(burnin_means, means)

  # determine standard deviation from breath
  sds <- seq(sd_0, sd_0 + sd_c, length.out = gen)
  burnin_sds <- rep(sd_0, burnin)
  sds <- c(burnin_sds, sds)

  # loop for all replicates
  tseqs <- foreach(i = 1:reps) %do% {

    # loop for each replicate
    foreach(j  = 1:(gen + burnin)) %do% {

      round(rnorm(n = days, mean = means[j], sd = sds[j]), 1)}

  }

  return(tseqs)
}

## Constant parameters ----

reps <- 10 # Number of replicates

gen <- 80 # Number of generations

days <- 150 # Number of days per

mean_0 <- 28 # Initial env. mean temperature

sd_0 <- 1 # Initial SD

burnin <- 5 # Burn-in period

sd_rel_factor <- 0.15 # SD change relative to Mean change factor

## Temperature Sequences ----

# Control -- No change in Mean and No change in SD
control <- generate_tseq(reps, gen, days, mean_0, sd_0, burnin, mean_c = 0, sd_c = 0)

# RCP4.5 -- + 1.44 C in Mean
rcp4.5 <- generate_tseq(reps, gen, days, mean_0, sd_0, burnin, mean_c = 1.44, sd_c = sd_rel_factor * 1.44)

# RCP6 -- + 1.76 C in Mean
rcp6 <- generate_tseq(reps, gen, days, mean_0, sd_0, burnin, mean_c = 1.76, sd_c = sd_rel_factor * 1.76)

# RCP8.5 -- + 2.96 C in Mean
rcp8.5 <- generate_tseq(reps, gen, days, mean_0, sd_0, burnin, mean_c = 2.96, sd_c = sd_rel_factor * 2.96)

# RCP 6, Only Change in Mean
rcp6_m <- generate_tseq(reps, gen, days, mean_0, sd_0, burnin, mean_c = 1.76, sd_c = 0)

# RCP 6, Only Change in SD
rcp6_sd <- rcp6 <- generate_tseq(reps, gen, days, mean_0, sd_0, burnin, mean_c = 0, sd_c = sd_rel_factor * 1.76)

# RCP 8.5 Only Change in Mean
rcp8.5_m <- generate_tseq(reps, gen, days, mean_0, sd_0, burnin, mean_c = 2.96, sd_c = 0)

# RCP 8.5 Only Change in SD
rcp8.5_sd <- generate_tseq(reps, gen, days, mean_0, sd_0, burnin, mean_c = 0, sd_c = sd_rel_factor * 2.96)

# RCP 8.5 with x2 initial SD
rcp8.5_2 <- generate_tseq(reps, gen, days, mean_0, sd_0 * 2, burnin, mean_c = 2.96, sd_c = sd_rel_factor * 2.96)

# RCP 8.5 with x2 initial SD, Only change in SD
rcp8.5_sd2 <- generate_tseq(reps, gen, days, mean_0, sd_0 * 2, burnin, mean_c = 0, sd_c = sd_rel_factor * 2.96)

## Save Data ---

# Set appropiate directory

# Save datasets
save(control, file = "control.RData")
save(rcp4.5, file = "rcp4.5.RData")
save(rcp6, file = "rcp6.RData")
save(rcp8.5, file = "rcp8.5.RData")
save(rcp6_m, file = "rcp6_m.RData")
save(rcp6_sd, file = "rcp6_sd.RData")
save(rcp8.5_m, file = "rcp8.5_m.RData")
save(rcp8.5_sd, file = "rcp8.5_sd.RData")
save(rcp8.5_2, file = "rcp8.5_2.RData")
save(rcp8.5_sd2, file = "rcp8.5_sd2.RData")

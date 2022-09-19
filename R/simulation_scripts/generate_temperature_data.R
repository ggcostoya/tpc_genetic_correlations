
## Generate temperature sequences ----

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

mean_0 <- 25 # Initial env. mean temperature

sd_0 <- 1 # Initial SD

burnin <- 5 # Burn-in period

sd_rel_factor <- 0.15 # SD change relative to Mean change factor

## Temperature Sequences ----

# Control -- No change in Mean and No change in SD
control <- generate_tseq(reps, gen, days, mean_0, sd_0, burnin, mean_c = 0, sd_c = 0)

# Control -- Double initial SD
control_2sd <- generate_tseq(reps, gen, days, mean_0, sd_0 * 2, burnin, mean_c = 0, sd_c = 0)

# RCP4.5 -- + 1.44 C in Mean
low <- generate_tseq(reps, gen, days, mean_0, sd_0, burnin, mean_c = 1.44, sd_c = sd_rel_factor * 1.44)

# RCP6 -- + 1.76 C in Mean
mid <- generate_tseq(reps, gen, days, mean_0, sd_0, burnin, mean_c = 1.76, sd_c = sd_rel_factor * 1.76)

# RCP8.5 -- + 2.96 C in Mean
high <- generate_tseq(reps, gen, days, mean_0, sd_0, burnin, mean_c = 2.96, sd_c = sd_rel_factor * 2.96)

# RCP 8.5 Only Change in Mean
high_m <- generate_tseq(reps, gen, days, mean_0, sd_0, burnin, mean_c = 2.96, sd_c = 0)

# RCP 8.5 Only Change in SD
high_sd <- generate_tseq(reps, gen, days, mean_0, sd_0, burnin, mean_c = 0, sd_c = sd_rel_factor * 2.96)

# RCP 8.5 with x2 initial SD
high_2sd <- generate_tseq(reps, gen, days, mean_0, sd_0 * 2, burnin, mean_c = 2.96, sd_c = sd_rel_factor * 2.96)

# RCP 8.5 with x2 initial SD, Only change in SD
high_2sd_sd <- generate_tseq(reps, gen, days, mean_0, sd_0 * 2, burnin, mean_c = 0, sd_c = sd_rel_factor * 2.96)

## Save data ----

save(control, file = "control.RData")
save(control_2sd, file = "control_2sd.RData")
save(low, file = "low.RData")
save(mid, file = "mid.RData")
save(high, file = "high.RData")
save(high_m, file = "high_m.RData")
save(high_sd, file = "high_sd.RData")
save(high_2sd, file = "high_2sd.RData")
save(high_2sd_sd, file = "high_2sd_sd.RData")



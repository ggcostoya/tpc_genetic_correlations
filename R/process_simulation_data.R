
## Process Raw Simulation Data ##

## Packages ----

library(tidyverse)

## Process No & K = 50 / No & K = 5000 simulation data ----

## No & K = 50 --

# Load all raw datasets from the No & K = 50 folder #

# combine all K50 datasets
k50 <- sapply(.GlobalEnv, is.data.frame)
k50 <- do.call(rbind, mget(names(k50))[k50])
rownames(k50) <- NULL

# add column for N0 & K
k50$n0_k <- rep(50, nrow(k50))

# save datset
save(k50, file = "k50.RData")

# clean environment
rm(list = ls())

## No & K = 5000 --

# Load all raw datasets from the No & K = 50 folder

# combine all K50 datasets
k5000 <- sapply(.GlobalEnv, is.data.frame)
k5000 <- do.call(rbind, mget(names(k5000))[k5000])
rownames(k5000) <- NULL

# add column for N0 & K
k5000$n0_k <- rep(5000, nrow(k5000))

# save datset
save(k5000, file = "k5000.RData")

# clean environment
rm(list = ls())

## Process No & K = 500 data ----

## Complete Information Datasets --

# load all complete information datasets ("complete" folder) #

# combine all K500 complete datasets
k500_complete <- sapply(.GlobalEnv, is.data.frame)
k500_complete <- do.call(rbind, mget(names(k500_complete))[k500_complete])

# get mean trait characteristics
k500_traits <- k500_complete %>%
  group_by(corr, tseq, gen) %>%
  summarise(topt = mean(topt, na.rm = T),
            pmax = mean(pmax, na.rm = T),
            ctmin = mean(ctmin, na.rm = T),
            ctmax = mean(ctmax, na.rm = T),
            mid = mean(mid, na.rm = T)) %>%
  ungroup()

# save dataset with mean trait characteristics
save(k500_traits, file = "k500_traits.RData")

# get population size per generation & add column for N0 & K
k500_n <- k500_complete %>%
  group_by(corr, tseq, pop_n, tseq_n) %>%
  summarise(as.data.frame(table(gen))) %>%
  ungroup() %>%
  mutate(n = Freq) %>%
  mutate(gen = as.numeric(gen)) %>%
  mutate(n0_k = rep(500, nrow(.))) %>%
  select(gen, n, pop_n, tseq_n, corr, tseq, n0_k)

# save dataset with population sizes
save(k500_n, file = "k500_n.RData")

# clean environment
rm(list = ls())

## Summary Information Datasets --

# load all summary information datasets ("summarized" folder) #

# combine all K500 summarised datasets
k500_sum <- sapply(.GlobalEnv, is.data.frame)
k500_sum <- do.call(rbind, mget(names(k500_sum))[k500_sum])
rownames(k500_sum) <- NULL

# add column for N0 & K
k500_sum$n0_k <- rep(500, nrow(k500_sum))

# save dataset with population sizes
save(k500_sum, file = "k500_sum.RData")

# clean environment
rm(list = ls())

## Combine Complete and Summary Datasets for N0 & K = 500 ----

# load k500_n & k500_sum files #

# combine datasets
k500 <- rbind(k500_n, k500_sum)

# save dataset
save(k500, file = "k500.RData")

## Combine all N0 & K datasets ----

# load k50, k500 & k5000 files #

# combine datasets
complete_sim_results <- rbind(k50, k500, k5000)

# save dataset with population sizes
save(complete_sim_results, file = "complete_sim_results.RData")

# summarise complete simulation results
sum_sim_results <- complete_sim_results %>%
  group_by(n0_k, corr, tseq, gen) %>%
  summarise(n = mean(n, na.rm = T)) %>%
  ungroup()

# save dataset with summarized simulation results
save(sum_sim_results, file = "sum_sim_results.RData")









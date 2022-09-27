
## Get Tables S1 and S2 ##

## Packages ----

library(tidyverse)

## Load data ----

# load sum_sim_results.RData file into the environment

## Get tables ----

sum_sim_results %>%
  filter(gen == 85) %>%
  mutate(percentage = (100 - (n / n0_k) * 100)) %>%
  print(n = 68) %>%
  as.data.frame()



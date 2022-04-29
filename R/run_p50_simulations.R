## Example on how to run a simulation

## Packages needed ----

library(tidyverse)
library(parallel)
library(foreach)
library(doParallel)
library(data.table)
library(DescTools)

registerDoParallel(detectCores()) # Register parallel cores

## Simulation Function ----

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

## Running Simulations

## Population Size 50

# None - control
p50_nc <- foreach(i = 1:length(none_50),
                  .packages = c("tidyverse", 'parallel', "doParallel", "foreach", "data.table", "DescTools")) %dopar% {

  foreach(j = 1:length(control)) %do% {

    sim <- simulation(pop = none_50[[i]], tseq = control[[j]])

    sim %>% mutate(pop_n = rep(i, nrow(.)), tseq_n = rep(j, nrow(.)))

  }
}
p50_nc <- do.call(rbind,do.call(c,p50_nc)) %>%
  mutate(corr = rep("none", nrow(.)),
         tseq = rep("control", nrow(.)),
         n_k = rep(50, nrow(.)))

# None - low
p50_nl <- foreach(i = 1:length(none_50),
                  .packages = c("tidyverse", 'parallel', "doParallel", "foreach", "data.table", "DescTools")) %dopar% {

  foreach(j = 1:length(low)) %do% {

    sim <- simulation(pop = none_50[[i]], tseq = low[[j]])

    sim %>% mutate(pop_n = rep(i, nrow(.)), tseq_n = rep(j, nrow(.)))

  }
}
p50_nl <- do.call(rbind,do.call(c,p50_nl)) %>%
  mutate(corr = rep("none", nrow(.)),
         tseq = rep("low", nrow(.)),
         n_k = rep(50, nrow(.)))

# None - mid
p50_nm <- foreach(i = 1:length(none_50),
                  .packages = c("tidyverse", 'parallel', "doParallel", "foreach", "data.table", "DescTools")) %dopar% {

                    foreach(j = 1:length(mid)) %do% {

                      sim <- simulation(pop = none_50[[i]], tseq = mid[[j]])

                      sim %>% mutate(pop_n = rep(i, nrow(.)), tseq_n = rep(j, nrow(.)))

                    }
                  }
p50_nm <- do.call(rbind,do.call(c,p50_nm)) %>%
  mutate(corr = rep("none", nrow(.)),
         tseq = rep("mid", nrow(.)),
         n_k = rep(50, nrow(.)))

# None - high
p50_nh <- foreach(i = 1:length(none_50),
                  .packages = c("tidyverse", 'parallel', "doParallel", "foreach", "data.table", "DescTools")) %dopar% {

                    foreach(j = 1:length(high)) %do% {

                      sim <- simulation(pop = none_50[[i]], tseq = high[[j]])

                      sim %>% mutate(pop_n = rep(i, nrow(.)), tseq_n = rep(j, nrow(.)))

                    }
                  }
p50_nh <- do.call(rbind,do.call(c,p50_nh)) %>%
  mutate(corr = rep("none", nrow(.)),
         tseq = rep("high", nrow(.)),
         n_k = rep(50, nrow(.)))

# GSTO - control
p50_gc <- foreach(i = 1:length(gsto_50),
                  .packages = c("tidyverse", 'parallel', "doParallel", "foreach", "data.table", "DescTools")) %dopar% {

                    foreach(j = 1:length(control)) %do% {

                      sim <- simulation(pop = gsto_50[[i]], tseq = control[[j]])

                      sim %>% mutate(pop_n = rep(i, nrow(.)), tseq_n = rep(j, nrow(.)))

                    }
                  }
p50_gc <- do.call(rbind,do.call(c,p50_gc)) %>%
  mutate(corr = rep("gsto", nrow(.)),
         tseq = rep("control", nrow(.)),
         n_k = rep(50, nrow(.)))

# GSTO - low
p50_gl <- foreach(i = 1:length(gsto_50),
                  .packages = c("tidyverse", 'parallel', "doParallel", "foreach", "data.table", "DescTools")) %dopar% {

                    foreach(j = 1:length(low)) %do% {

                      sim <- simulation(pop = gsto_50[[i]], tseq = low[[j]])

                      sim %>% mutate(pop_n = rep(i, nrow(.)), tseq_n = rep(j, nrow(.)))

                    }
                  }
p50_gl <- do.call(rbind,do.call(c,p50_gl)) %>%
  mutate(corr = rep("gsto", nrow(.)),
         tseq = rep("low", nrow(.)),
         n_k = rep(50, nrow(.)))

# GSTO - mid
p50_gm <- foreach(i = 1:length(gsto_50),
                  .packages = c("tidyverse", 'parallel', "doParallel", "foreach", "data.table", "DescTools")) %dopar% {

                    foreach(j = 1:length(mid)) %do% {

                      sim <- simulation(pop = gsto_50[[i]], tseq = mid[[j]])

                      sim %>% mutate(pop_n = rep(i, nrow(.)), tseq_n = rep(j, nrow(.)))

                    }
                  }
p50_gm <- do.call(rbind,do.call(c,p50_gm)) %>%
  mutate(corr = rep("gsto", nrow(.)),
         tseq = rep("mid", nrow(.)),
         n_k = rep(50, nrow(.)))

# GSTO - high
p50_gh <- foreach(i = 1:length(gsto_50),
                  .packages = c("tidyverse", 'parallel', "doParallel", "foreach", "data.table", "DescTools")) %dopar% {

                    foreach(j = 1:length(high)) %do% {

                      sim <- simulation(pop = gsto_50[[i]], tseq = high[[j]])

                      sim %>% mutate(pop_n = rep(i, nrow(.)), tseq_n = rep(j, nrow(.)))

                    }
                  }
p50_gh <- do.call(rbind,do.call(c,p50_gh)) %>%
  mutate(corr = rep("gsto", nrow(.)),
         tseq = rep("high", nrow(.)),
         n_k = rep(50, nrow(.)))

##### --- TDE ----


# Both - control
p50_bc <- foreach(i = 1:length(both_50),
                  .packages = c("tidyverse", 'parallel', "doParallel", "foreach", "data.table", "DescTools")) %dopar% {

                    foreach(j = 1:length(control)) %do% {

                      sim <- simulation(pop = both_50[[i]], tseq = control[[j]])

                      sim %>% mutate(pop_n = rep(i, nrow(.)), tseq_n = rep(j, nrow(.)))

                    }
                  }
p50_bc <- do.call(rbind,do.call(c,p50_bc)) %>%
  mutate(corr = rep("both", nrow(.)),
         tseq = rep("control", nrow(.)),
         n_k = rep(50, nrow(.)))

# GSTO - low
p50_bl <- foreach(i = 1:length(both_50),
                  .packages = c("tidyverse", 'parallel', "doParallel", "foreach", "data.table", "DescTools")) %dopar% {

                    foreach(j = 1:length(low)) %do% {

                      sim <- simulation(pop = both_50[[i]], tseq = low[[j]])

                      sim %>% mutate(pop_n = rep(i, nrow(.)), tseq_n = rep(j, nrow(.)))

                    }
                  }
p50_bl <- do.call(rbind,do.call(c,p50_bl)) %>%
  mutate(corr = rep("both", nrow(.)),
         tseq = rep("low", nrow(.)),
         n_k = rep(50, nrow(.)))

# GSTO - mid
p50_bm <- foreach(i = 1:length(both_50),
                  .packages = c("tidyverse", 'parallel', "doParallel", "foreach", "data.table", "DescTools")) %dopar% {

                    foreach(j = 1:length(mid)) %do% {

                      sim <- simulation(pop = both_50[[i]], tseq = mid[[j]])

                      sim %>% mutate(pop_n = rep(i, nrow(.)), tseq_n = rep(j, nrow(.)))

                    }
                  }
p50_bm <- do.call(rbind,do.call(c,p50_bm)) %>%
  mutate(corr = rep("both", nrow(.)),
         tseq = rep("mid", nrow(.)),
         n_k = rep(50, nrow(.)))

# GSTO - high
p50_bh <- foreach(i = 1:length(both_50),
                  .packages = c("tidyverse", 'parallel', "doParallel", "foreach", "data.table", "DescTools")) %dopar% {

                    foreach(j = 1:length(high)) %do% {

                      sim <- simulation(pop = both_50[[i]], tseq = high[[j]])

                      sim %>% mutate(pop_n = rep(i, nrow(.)), tseq_n = rep(j, nrow(.)))

                    }
                  }
p50_bh <- do.call(rbind,do.call(c,p50_bh)) %>%
  mutate(corr = rep("both", nrow(.)),
         tseq = rep("high", nrow(.)),
         n_k = rep(50, nrow(.)))



testp50 <- rbind(p50_nc, p50_nl, p50_nm, p50_nh,
                 p50_gc, p50_gl, p50_gm, p50_gh,
                 p50_bc, p50_bl, p50_bm, p50_bh)

testp50 %>%
  group_by(gen, corr, tseq) %>%
  summarise(as.data.frame(table(gen, corr, tseq))) %>%
  ungroup() %>%
  mutate(mean_n = Freq/100, gen = as.numeric(gen)) %>%
  ggplot(aes(x = gen, y = mean_n, col = corr)) +
  geom_line() +
  geom_vline(xintercept = 5) +
  facet_grid(cols = vars(tseq))



















## PLotting

## Packages ----

library(tidyverse)
library(parallel)
library(foreach)
library(doParallel)
library(gridExtra)

## Load & Process Data ----

simdata <- p500
tseq <- rcp8.5

# subset simulation data for only tseq of interest
simdata <- simdata %>%
  filter(tseq == "rcp8.5") %>%
  select(corr, topt, pmax, ctmin, ctmax, mid, gen, r)

# add tseq_n and generation numbers to temperature sequence
tseq <- foreach(i = 1:length(tseq)) %do% {

  foreach(j = 1:length(tseq[[i]])) %do% {

    # define range of values
    ts <- tseq[[i]][[j]]

    tibble(t = ts,
           tseq_n = rep(i, length(ts)),
           gen = rep(j, length(ts)))
  }
}

# reshape raw data object
tseq <- do.call(rbind, (do.call(c,tseq)))

# select generations of interest
gens <- c(5, 60, 75)

## Plot temperature distribution ----

tseq %>%
  filter(gen %in% gens) %>%
  ggplot(aes(x = t)) +
  geom_density(color = NA, fill = "orange", alpha = 0.5) +
  facet_grid(cols = vars(gen))

## Plot mean TPC per generation ----

# sumarrise simdata
simdata_sum <- simdata %>%
  select(corr, gen, topt, pmax, ctmin, ctmax, mid) %>%
  filter(gen %in% gens) %>%
  group_by(corr, gen) %>%
  summarise(topt = mean(topt),
            pmax = mean(pmax),
            ctmin = mean(ctmin),
            ctmax = mean(ctmax),
            mid = mean(mid, na.rm = T)) %>%
  ungroup()

# function to build thermal-performance dataset
build_tpd <- function(tpts){

  # temperature
  t <- round(tpts[1:4], 1)

  # performance
  pmax <- tpts[5]
  p <- c(0, pmax/2, pmax, 0)

  return(data.frame(t,p))

}

# add tpd column
simdata_sum$tpd <- rep(NA, nrow(simdata_sum))

# generate tpd for every row
for(i in 1:nrow(simdata_sum)){

  # get tpts
  tpts <- c(simdata_sum$ctmin[i],
            simdata_sum$mid[i],
            simdata_sum$topt[i],
            simdata_sum$ctmax[i],
            simdata_sum$pmax[i])

  # get tpd
  tpd <- build_tpd(tpts = tpts) %>% nest(cols = c(t,p))

  # add columns
  simdata_sum$tpd[i] <- tpd

}

# reshape tpd column
simdata_sum <- simdata_sum %>% unnest(cols = c(tpd))

# simdata_sum for plot
simdata_sum_sub <- simdata_sum %>% unnest(cols = c(tpd))

# prepare tseq dataset

# name plot
tseq_plot <- tseq %>%
  filter(gen %in% gens) %>%
  ggplot(aes(x = t)) +
  geom_density(color = NA, fill = "orange", alpha = 0.5) +
  facet_grid(cols = vars(gen))

# extract plot data
plot_data <- ggplot_build(tseq_plot)$data[[1]]

# reshape plot data
plot_data <- plot_data %>%
  select(x,y, PANEL) %>%
  mutate(t = x, density = y, gen = PANEL) %>%
  select(t, density, gen)

# get maximum performance on the simdata
max_perf <- max(simdata_sum_sub$p)

# get maximum density for temperature data
max_dens <- max(plot_data$density)

# get multiplication factor
factor <- max_perf/max_dens

# rescale density column
plot_data$density <- plot_data$density * factor

# reshape generation column
plot_data$gen <- ifelse(plot_data$gen == 1, gens[1],
                        ifelse(plot_data$gen == 2, gens[2], gens[3]))

# plot no zoom
ggplot() +
  geom_area(data = plot_data, aes(x = t, y = density), color = NA, fill = "orange", alpha = 0.5) +
  geom_line(data = simdata_sum_sub, aes(x = t, y = p, col = corr), lwd = 1) +
  scale_y_continuous(sec.axis = sec_axis(~. / factor, name = "density")) +
  facet_grid(cols = vars(gen))

# plot with zoom
ggplot() +
  geom_area(data = plot_data, aes(x = t, y = density), color = NA, fill = "orange", alpha = 0.5) +
  geom_line(data = simdata_sum_sub, aes(x = t, y = p, col = corr), lwd = 1) +
  scale_y_continuous(sec.axis = sec_axis(~. / factor, name = "density")) +
  #coord_cartesian(ylim = c(5,max_perf), xlim = c(20,35)) +
  coord_cartesian(xlim = c(20,35)) +
  facet_grid(cols = vars(gen), scales = "free")

# plot with geom_raster instead
ggplot() +
  geom_raster(data = plot_data, aes(x = t, fill = density))

### Tiles plot ----

# subset of tseq data
tseq_sub <- tseq %>% filter(gen %in% gens)

#

# tile plot
ggplot() +
  geom_tile(data = plot_data, aes(x = t, y = max_perf/2, fill = density, height = max_perf), alpha = 0.5, color = NA) +
  scale_fill_gradient(low = "white", high = "darkorange") +
  geom_line(data = simdata_sum_sub, aes(x = t, y = p, col = corr), lwd = 1.5) +
  facet_grid(cols = vars(gen)) +
  theme_classic()

simdata %>%
  filter(gen %in% gens) %>%
  ggplot(aes(x = r, col = corr)) +
  geom_freqpoly(binwidth = 1, position = "identity", alpha = 1) +
  facet_grid(cols = vars(gen))


## Calculate population size

popsizes <- simdata %>%
  filter(gen %in% gens) %>%
  summarise(as_tibble(table(gen,corr)))

## simdata

# function to generate tpd & tpc
build_tpd <- function(tpts){

  # temperature
  t <- round(tpts[1:4], 1)

  # performance
  pmax <- tpts[5]
  p <- c(0, pmax/2, pmax, 0)

  return(data.frame(t,p))

}
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

# add columns for tpd and tpc
simdata_sum$tpd <- rep(NA, nrow(simdata_sum))
simdata_sum$tpc <- rep(NA, nrow(simdata_sum))

# generate TPD and TPC
for(i in 1:nrow(simdata_sum)){

  # get tpts
  tpts <- c(simdata_sum$ctmin[i],
            simdata_sum$mid[i],
            simdata_sum$topt[i],
            simdata_sum$ctmax[i],
            simdata_sum$pmax[i])

  # get tpd
  tpd <- build_tpd(tpts = tpts)
  tpd_nested <- tpd %>% nest(cols = c(t,p))

  # get tpc
  tpc <- generate_tpc(tpd = tpd) %>% nest(tpc = c(t,p))

  # add columns
  simdata_sum$tpd[i] <- tpd_nested
  simdata_sum$tpc[i] <- tpc

}

# reshape tpd and tpc columns
simdata_sum <- simdata_sum %>% unnest(cols = c(tpc)) %>% unnest(cols = c(tpd))

# function to predict performance
predict_performance <- function(sim, tseq){

  # get temperature indeces
  indexes <- round(tseq/0.1)

  # add predicted performance column
  sim_pred <- sim %>% mutate(pred_p = rep(NA, nrow(.)))

  # start loop to pedict performance
  for(i in 1:nrow(sim_pred)){

    # extract TPC
    tpc <- sim_pred$tpc[[i]]

    # determine performance at environmental temperature
    sim_pred$pred_p[i] <- list(tpc$p[indexes])

  }

  return(sim_pred)

}

# add predicted_performance column
simdata_sum$pred_perf <- rep(NA, nrow(simdata_sum))

# loop to calculate predicted performance
for(i in 1:nrow(simdata_sum)){

  # extract the tpc
  tpc <- simdata_sum$tpc[[i]]

  # extract the temperature sequences for that generation
  temps <- tseq %>% filter(gen == simdata_sum$gen[[i]]) %>%
    select(t)
  temps <- temps$t

  # get indeces
  indexes <- round(temps/0.1)

  # predict performance
  simdata_sum$pred_perf[i] <- list(tpc$p[indexes])

}

# add column of population size
simdata_sum$N <- popsizes$n

# plot
dens_plot_data <- simdata_sum %>%
  select(corr, gen, N, pred_perf) %>%
  unnest(pred_perf) %>%
  ggplot(aes(x = pred_perf, fill = corr), col = NA) +
  geom_density(col = NA, alpha = 0.5) +
  scale_fill_manual(values = c("red","blue","green","yellow")) +
  xlim(5,15) +
  facet_grid(cols = vars(gen)) +
  theme_minimal() +
  theme(panel.border = element_rect(size = 0.5, fill = NA, color = "black"),
        legend.position = "top")


dens_plot_data <- ggplot_build(dens_plot_data)$data[[1]]

dens_plot_data <- dens_plot_data %>%
  select(fill, y, x, PANEL) %>%
  mutate(density = y, t = x, corr = fill, gen = PANEL) %>%
  select(density, t, corr, gen)

dens_plot_data$corr <- ifelse(dens_plot_data$corr == "red", "both",
                              ifelse(dens_plot_data$corr == "blue", "gsto",
                                     ifelse(dens_plot_data$corr == "green", "none", "tde")))

dens_plot_data$gen <- ifelse(dens_plot_data$gen == 1, 5,
                             ifelse(dens_plot_data$gen == 2, 60, 75))


dens_plot_data <- merge(dens_plot_data, popsizes, by = c("gen", "corr"), all = TRUE)

dens_plot_data %>%
  ggplot(aes(x = t, y = density * n, fill = corr), col = NA) +
  geom_area(alpha = 0.5, position = "identity") +
  facet_grid(cols = vars(gen))















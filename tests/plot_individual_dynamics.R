
## Plotting Individual Dynamics

## Packages ----

library(tidyverse)
library(ggExtra)
library(parallel)
library(foreach)
library(doParallel)
library(gridExtra)

## Load & Process Data ---

## Simulation data

simdata <- p500
rm(p500)

# Subset data for only simulations of interest
simdata <- simdata %>%
  filter(tseq == "rcp8.5") %>%
  select(corr, topt, pmax, ctmin, ctmax, mid, gen, r)

## Temperature data

tseq <- rcp8.5

# get thermal characteristics summary
tseq_summary <- foreach(i = 1:length(tseq)) %do% {

  foreach(j = 1:length(tseq[[i]])) %do% {

    # define range of values
    ts <- tseq[[i]][[j]]

    tibble(mean = mean(ts),
           sd = sd(ts),
           min = min(ts),
           max = max(ts),
           tseq_n = i,
           gen = j)
  }
}

# reshape tseq summary
tseq_summary <- do.call(rbind, do.call(c, tseq_summary))
tseq_summary <- tseq_summary %>% mutate(gen = gen - 1)

# reshape raw data object I
tseq <- foreach(i = 1:length(tseq)) %do% {

  foreach(j = 1:length(tseq[[i]])) %do% {

    # define range of values
    ts <- tseq[[i]][[j]]

    tibble(t = ts,
           tseq_n = rep(i, length(ts)),
           gen = rep(j, length(ts)))
  }
}

# reshape raw data object II
tseq <- do.call(rbind, (do.call(c,tseq)))

## Plot individual dynamics ----

# summarize tseq summary
tseq_summary_2 <- tseq_summary %>%
  group_by(gen) %>%
  summarize(mean = mean(mean),
            sd = mean(sd),
            mean_min = mean(min),
            mean_max = mean(max),
            min_min = min(min),
            max_max = max(max)) %>%
  mutate(max_range = max_max - min_min,
         mean_range = mean_max - mean_min)

# add column for relative reproduction on simdata
simdata <- simdata %>%
  select(corr, topt, pmax, ctmin, ctmax, mid, gen, r) %>%
  group_by(corr, gen, topt, pmax, ctmin, ctmax, mid) %>%
  summarize(r = mean(r)) %>%
  ungroup() %>%
  group_by(corr, gen) %>%
  mutate(rel_r = ifelse(r > mean(r), 1, 0)) %>%
  ungroup()

# generations of interest
gens <- c(5,25,50,75)

# filter temperature summary for generations of interest
tseq_summary_2_plot <- tseq_summary_2 %>% filter(gen %in% gens)

# plot
simdata %>%
  filter(gen %in% gens) %>%
  ggplot(aes(x = topt, y = pmax, col = as.factor(rel_r))) +
  geom_point(alpha = 0.5) +
  scale_color_manual(values = c("darkgray", "palegreen3"), guide = "none") +
  geom_vline(data = tseq_summary_2_plot, aes(xintercept = mean), lwd = 0.5, lty = "longdash") +
  #geom_hline(data = tseq_summary_2_plot, aes(yintercept = mean_max), lwd = 0.5, lty = "longdash") +
  #geom_hline(data = tseq_summary_2_plot, aes(yintercept = max_max), lwd = 0.5, lty = "longdash", col = "darkred") +
  #geom_hline(data = tseq_summary_2_plot, aes(yintercept = mean_min), lwd = 0.5, lty = "longdash") +
  facet_grid(cols = vars(gen), rows = vars(corr)) +
  theme_minimal() +
  theme(panel.border = element_rect(size = 0.5, fill = NA, color = "black"),
        panel.grid = element_blank())

## Plot TPC match with Temperature sequence ----

## Plot temperature sequence histogram

tseq_dist_plot <- tseq %>%
  filter(gen %in% gens) %>%
  ggplot(aes(x = t)) +
  geom_vline(data = tseq_summary_2_plot, aes(xintercept = mean), lwd = 1, col = "darkorange") +
  geom_vline(data = tseq_summary_2_plot, aes(xintercept = mean_min), lwd = 0.5, lty = 2, col = "darkorange") +
  geom_vline(data = tseq_summary_2_plot, aes(xintercept = mean_max), lwd = 0.5, lty = 2, col = "darkorange") +
  geom_density(color = "darkorange", fill = "orange", lwd = 1, alpha = 0.5) +
  xlim(9.5, 38) +
  facet_grid(cols = vars(gen)) +
  theme_minimal() +
  theme(panel.border = element_rect(size = 0.5, fill = NA, color = "black"),
        panel.grid = element_blank(),
        strip.text.x = element_blank())

## Plot mean TPC per generation ----

# summarise simdataset
simdata_summary <- simdata %>%
  select(corr, gen, topt, pmax, ctmin, ctmax, mid) %>%
  group_by(corr, gen) %>%
  summarise(topt = mean(topt),
            pmax = mean(pmax),
            ctmin = mean(ctmin),
            ctmax = mean(ctmax),
            mid = mean(mid, na.rm = T)) %>%
  filter(gen %in% gens) %>%
  ungroup()

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
simdata_summary$tpd <- rep(NA, nrow(simdata_summary))
simdata_summary$tpc <- rep(NA, nrow(simdata_summary))

# generate TPD and TPC
for(i in 1:nrow(simdata_summary)){

  # get tpts
  tpts <- c(simdata_summary$ctmin[i],
            simdata_summary$mid[i],
            simdata_summary$topt[i],
            simdata_summary$ctmax[i],
            simdata_summary$pmax[i])

  # get tpd
  tpd <- build_tpd(tpts = tpts)
  tpd_nested <- tpd %>% nest(cols = c(t,p))

  # get tpc
  tpc <- generate_tpc(tpd = tpd) %>% nest(tpc = c(t,p))

  # add columns
  simdata_summary$tpd[i] <- tpd_nested
  simdata_summary$tpc[i] <- tpc

}

# reshape tpd and tpc columns
simdata_summary <- simdata_summary %>% unnest(cols = c(tpc)) %>% unnest(cols = c(tpd))

# plot
tpc_plot <- simdata_summary %>%
  select(corr, gen, tpd) %>%
  unnest(cols = c(tpd)) %>%
  ggplot(aes(x = t, y = p, col = corr)) +
  geom_vline(data = tseq_summary_2_plot, aes(xintercept = mean), lwd = 0.5, col = "darkorange") +
  geom_rect(data = tseq_summary_2_plot,
            aes(NULL, NULL, color = NULL, xmin = mean_min, xmax = mean_max, ymin = 0, ymax = 16),
            col = NA, fill = "orange", alpha = 0.25) +
  geom_point(size = 2) +
  geom_line(lwd = 1.25) +
  xlim(9.5,38) +
  facet_grid(cols = vars(gen), rows = vars(corr)) +
  theme_minimal() +
  theme(panel.border = element_rect(size = 0.5, fill = NA, color = "black"),
        panel.grid = element_blank(),
        legend.position = "none")

## Combine plots ----

grid.arrange(tpc_plot, tseq_dist_plot, ncol = 1, nrow = 2, heights = c(4,1.5))

## Plot Performance PDFs ----

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
simdata_summary$pred_perf <- rep(NA, nrow(simdata_summary))

# loop to calculate predicted performance
for(i in 1:nrow(simdata_summary)){

  # extract the tpc
  tpc <- simdata_summary$tpc[[i]]

  # extract the temperature sequences for that generation
  temps <- tseq %>% filter(gen == simdata_summary$gen[[i]]) %>%
    select(t)
  temps <- temps$t

  # get indeces
  indexes <- round(temps/0.1)

  # predict performance
  simdata_summary$pred_perf[i] <- list(tpc$p[indexes])

}

# plot
simdata_summary %>%
  select(corr, gen, pred_perf) %>%
  unnest(pred_perf) %>%
  ggplot(aes(x = pred_perf, fill = corr), col = NA) +
  geom_histogram(col = NA, alpha = 0.5, position = "identity") +
  xlim(5,15) +
  facet_grid(cols = vars(gen)) +
  theme_minimal() +
  theme(panel.border = element_rect(size = 0.5, fill = NA, color = "black"),
        legend.position = "top")

# plot ## IN PROGRESS VITCH

simdata_summary_bb <- simdata_summary %>% filter(gen == 75)
simdata_summary_bb$N <- c(3200,700,625,4500)

simdata_summary_bb %>%
  select(corr, gen, pred_perf, N) %>%
  unnest(pred_perf) %>%
  mutate(pred_bb = pred_perf * N) %>%
  ggplot(aes(x = pred_bb, fill = corr), col = NA) +
  geom_histogram(col = NA, alpha = 0.5, position = "identity") +
  #xlim(5,15) +
  #facet_grid(cols = vars(gen)) +
  theme_minimal() +
  theme(panel.border = element_rect(size = 0.5, fill = NA, color = "black"),
        legend.position = "top")




## Plot performance PDF for entire population ----

# prepare dataset
simdata_pdf_plot <- simdata %>%
  filter(!is.na(mid)) %>%
  select(corr, gen, topt, pmax, ctmin, ctmax, mid) %>%
  filter(gen %in% gens) %>%
  mutate(tpc = rep(NA, nrow(.)),
         tpd = rep(NA, nrow(.)),
         pred_perf = rep(NA, nrow(.)))

# get tpcs for all individuals
for(i in 1:nrow(simdata_pdf_plot)){

  # get tpts
  tpts <- c(simdata_pdf_plot$ctmin[i],
            simdata_pdf_plot$mid[i],
            simdata_pdf_plot$topt[i],
            simdata_pdf_plot$ctmax[i],
            simdata_pdf_plot$pmax[i])

  # get tpd
  tpd <- build_tpd(tpts = tpts)
  tpd_nested <- tpd %>% nest(cols = c(t,p))

  # get tpc
  tpc <- generate_tpc(tpd = tpd) %>% nest(tpc = c(t,p))

  # add columns
  simdata_pdf_plot$tpd[i] <- tpd_nested
  simdata_pdf_plot$tpc[i] <- tpc

}

# reshape tpd and tpc columns
simdata_pdf_plot <- simdata_pdf_plot %>% unnest(cols = c(tpc)) %>% unnest(cols = c(tpd))

# loop to calculate predicted performance
for(i in 1:nrow(simdata_pdf_plot)){

  # extract the tpc
  tpc <- simdata_pdf_plot$tpc[[i]]

  # extract the temperature sequences for that generation
  temps <- tseq %>% filter(gen == simdata_pdf_plot$gen[[i]]) %>%
    select(t)
  temps <- temps$t

  # get indeces
  indexes <- round(temps/0.1)

  # predict performance
  simdata_pdf_plot$pred_perf[i] <- list(tpc$p[indexes])

}

# plot
simdata_pdf_plot %>%
  filter(gen == 5) %>%
  select(corr, gen, pred_perf) %>%
  unnest(pred_perf) %>%
  ggplot(aes(x = pred_perf, fill = corr), col = NA) +
  #geom_density(col = NA, alpha = 0.5) +
  geom_histogram(col = NA, alpha = 0.5, position = "identity", binwidth = 0.1) +
  #facet_grid(cols = vars(gen)) +
  theme_minimal() +
  theme(panel.border = element_rect(size = 0.5, fill = NA, color = "black"),
        legend.position = "top")

check <- simdata_pdf_plot %>%
  select(corr, gen, pred_perf) %>%
  unnest(pred_perf) %>%
  group_by(corr) %>%
  summarise(as.data.frame(table(corr, gen)))











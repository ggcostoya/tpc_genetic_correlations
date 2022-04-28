
## PLotting

## Packages ----

library(tidyverse)
library(parallel)
library(foreach)
library(doParallel)
library(gridExtra)
library(ggridges)
library(see)

## Load & Process Data ----

p500
rcp8.5

# assign new data objects
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
gens <- c(5, 30, 55, 75)

## Plot temperature distribution ----

# get thermal characteristics summary
tseq_summary <- tseq %>%
  filter(gen %in% gens) %>%
  group_by(gen) %>%
  summarise(mean_t = mean(t),
            max_t = max(t),
            min_t = min(t))

# plot itself
tseq_dist_plot <- tseq %>%
  filter(gen %in% gens) %>%
  ggplot(aes(x = t)) +
  geom_density(color = NA, fill = "orange", alpha = 0.5) +
  geom_vline(data = tseq_summary, aes(xintercept = mean_t), col = "darkorange", alpha = 0.75) +
  geom_text(data = tseq_summary, aes(x = 12, y = 0.17, label = gen), size = 6) +
  scale_y_continuous(expand = c(0,0), breaks = seq(0,0.4, by = 0.03)) +
  xlim(9.5,37) +
  ylab("Performance") +
  facet_grid(cols = vars(gen)) +
  theme_minimal() +
  theme(panel.border = element_blank(),
        panel.grid = element_blank(),
        strip.text.x = element_blank(),
        axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.title.y = element_text(color = "white"),
        axis.text.y = element_text(color =  "white"),
        #axis.line.x = element_line(colour = "black"),
        plot.margin = unit(c(1,1,-0.1,1), "lines"))

## Plot mean TPC per generation with temperature distribution as tile ----

## process simulation data

# summarize simulation data
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

# function to build thermal-performance data set
build_tpd <- function(tpts){

  # temperature
  t <- round(tpts[1:4], 1)

  # performance
  pmax <- tpts[5]
  p <- c(0, pmax/2, pmax, 0)

  return(data.frame(t,p))

}

# add an empty TPD column
simdata_sum$tpd <- rep(NA, nrow(simdata_sum))

# generate TPD for every row
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

# reshape and unnest TPD column
simdata_sum <- simdata_sum %>% unnest(cols = c(tpd))
simdata_sum_sub <- simdata_sum %>% unnest(cols = c(tpd))

## prepare temperature sequence dataset

# name density plot for temperature distributions
tseq_plot <- tseq %>%
  filter(gen %in% gens) %>%
  ggplot(aes(x = t)) +
  geom_density(color = NA, fill = "orange", alpha = 0.5) +
  facet_grid(cols = vars(gen))

# extract plot data
tseq_plot_data <- ggplot_build(tseq_plot)$data[[1]]

# reshape plot data
tseq_plot_data <- tseq_plot_data %>%
  select(x,y, PANEL) %>%
  mutate(t = x, density = y, gen = as.numeric(PANEL)) %>%
  select(t, density, gen)

# reshape generation column
tseq_plot_data$gen <- ifelse(tseq_plot_data$gen == 1, gens[1],
                        ifelse(tseq_plot_data$gen == 2, gens[2],
                               ifelse(tseq_plot_data$gen == 3, gens[3], gens[4])))

# get maximum performance on the simdata
max_perf <- max(simdata_sum_sub$p)


# reset factor levels of summary sim dataset
simdata_sum_sub$corr <- ifelse(simdata_sum_sub$corr == "both", "GSTO + TDE",
                               ifelse(simdata_sum_sub$corr == "gsto", "GSTO",
                                      ifelse(simdata_sum_sub$corr == "tde", "TDE", "None")))

# reorder factor levels of summry sim dataset
simdata_sum_sub$corr <- factor(simdata_sum_sub$corr, levels = c("None", "GSTO", "TDE", "GSTO + TDE"))

# set color scale for different tradeoffs
colors <- c("None" = "#009E73", "GSTO" = "#56B4E9", "TDE" = "#CC79A7", "GSTO + TDE" = "#D55E00")

# tile plot all data
tpc_plot <- ggplot() +
  geom_tile(data = tseq_plot_data, aes(x = t, y = max_perf/2, fill = density, height = max_perf + 1), alpha = 0.25, color = NA) +
  scale_fill_gradient(low = "white", high = "darkorange", guide = "none") +
  geom_vline(data = tseq_summary, aes(xintercept = mean_t), col = "darkorange", alpha = 0.75) +
  geom_vline(data = tseq_summary, aes(xintercept = max_t), col = "gray", lty = 1) +
  geom_vline(data = tseq_summary, aes(xintercept = min_t), col = "gray", lty = 1) +
  geom_line(data = simdata_sum_sub, aes(x = t, y = p, col = corr), lwd = 0.75) +
  geom_point(data = simdata_sum_sub, aes(x = t, y = p, col = corr), size = 1.5) +
  scale_color_manual(values = colors) +
  #geom_text(data = simdata_sum_sub, aes(x = 11.5, y = 13.5, label = gen), size = 6) +
  #geom_rect(data = tseq_summary, aes(xmin = 24, xmax = 36, ymin = 5, ymax = max_perf + 0.2), fill = NA, color = "black", linetype = "dashed", size = 0.3) +
  #geom_rect(data = tseq_summary, aes(xmin = 30, xmax = 37, ymin = 0, ymax = 6), fill = NA, color = "black", linetype = "dashed", size = 0.3) +
  scale_y_continuous(expand = c(0,0), breaks = seq(0,15, by = 2.5)) +
  coord_cartesian(xlim = c(9.5,37)) +
  xlab("Temperature") + ylab("Performance") +
  facet_grid(cols = vars(gen)) +
  theme_minimal() +
  theme(legend.position = "none",
        axis.title.y = element_text(color = "white"),
        strip.text = element_blank(),
        axis.title.x = element_blank(),
        panel.border = element_rect(size = 0.5, fill = NA, color = "black"),
        panel.grid = element_blank(),
        plot.margin = unit(c(-0.1,1,0.5,1), "lines"))


# combine plot with tseq plot
grid.arrange(tseq_dist_plot, tpc_plot, nrow = 2,
             heights = c(1,4))

# tile plot zoomed
tpc_plot_zoomed <- ggplot() +
  geom_tile(data = tseq_plot_data, aes(x = t, y = max_perf/2, fill = density, height = max_perf + 1), alpha = 0.25, color = NA) +
  scale_fill_gradient(low = "white", high = "darkorange", guide = "none") +
  geom_vline(data = tseq_summary, aes(xintercept = mean_t), col = "darkorange", alpha = 0.75, lwd = 1.5) +
  geom_vline(data = tseq_summary, aes(xintercept = max_t), col = "gray", lty = 1, lwd = 1.5) +
  geom_vline(data = tseq_summary, aes(xintercept = min_t), col = "gray", lty = 1, lwd = 1.5) +
  geom_line(data = simdata_sum_sub, aes(x = t, y = p, col = corr), lwd = 1.5, alpha = 0.75) +
  geom_point(data = simdata_sum_sub, aes(x = t, y = p, col = corr), size = 3, alpha = 0.75) +
  scale_color_manual(values = colors) +
  geom_text(data = simdata_sum_sub, aes(x = 11, y = 14, label = gen), size = 4) +
  scale_y_continuous(expand = c(0,0.2), breaks = seq(7.5,14.5, by = 1)) +
  coord_cartesian(ylim = c(7, max_perf + 0.2), xlim = c(24, 36)) +
  xlab("Temperature") + ylab("Performance") +
  facet_grid(cols = vars(gen)) +
  theme_minimal() +
  theme(legend.position = "none",
        strip.text = element_blank(),
        axis.title.x = element_blank(),
        panel.border = element_rect(size = 0.5, fill = NA, color = "black"),
        panel.grid = element_blank(),
        plot.margin = unit(c(0,1,0.5,1), "lines"))

# combine the three plots
grid.arrange(tseq_dist_plot, tpc_plot, tpc_plot_zoomed, nrow = 3,
             heights = c(1.5,4,4.5))

## CTmax panel ----
tpc_plot_zoomed_ctmax <- ggplot() +
  geom_tile(data = tseq_plot_data, aes(x = t, y = max_perf/2, fill = density, height = max_perf + 1), alpha = 0.25, color = NA) +
  scale_fill_gradient(low = "white", high = "darkorange", guide = "none") +
  geom_vline(data = tseq_summary, aes(xintercept = mean_t), col = "darkorange", alpha = 0.75, lwd = 1.5) +
  geom_vline(data = tseq_summary, aes(xintercept = max_t), col = "gray", lty = 1, lwd = 1.5) +
  geom_vline(data = tseq_summary, aes(xintercept = min_t), col = "gray", lty = 1) +
  geom_line(data = simdata_sum_sub, aes(x = t, y = p, col = corr), lwd = 1.5, alpha = 0.75) +
  geom_point(data = simdata_sum_sub, aes(x = t, y = p, col = corr), size = 3, alpha = 0.75) +
  scale_color_manual(values = colors) +
  scale_y_continuous(expand = c(0.05,0), breaks = seq(0,5, by = 1.25)) +
  guides(colour = guide_legend(override.aes = list(size=3))) +
  coord_cartesian(ylim = c(-0.1, 6), xlim = c(30, 37)) +
  xlab("Temperature") + ylab("Performance") +
  facet_grid(cols = vars(gen)) +
  theme_minimal() +
  theme(legend.position = "bottom",
        legend.title = element_blank(),
        legend.text = element_text(size = 10),
        axis.title.y = element_text(color = "white"),
        strip.text = element_blank(),
        panel.border = element_rect(size = 0.5, fill = NA, color = "black"),
        panel.grid = element_blank(),
        plot.margin = unit(c(0,1,1,1), "lines"))

# combine the 4 plots plots
grid.arrange(tseq_dist_plot,
             tpc_plot,
             tpc_plot_zoomed,
             tpc_plot_zoomed_ctmax,
             nrow = 4,
             heights = c(1.5,4,4,5))


## Plot PDF of performance ----

# calculate population sizes
popsizes <- simdata %>%
  filter(gen %in% gens) %>%
  summarise(as_tibble(table(gen,corr)))

# function to generate a TPC
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

# add columns for TPC storage
simdata_sum$tpc <- rep(NA, nrow(simdata_sum))

# generate TPD and TPC
for(i in 1:nrow(simdata_sum)){

  # get TPC
  tpc <- generate_tpc(tpd = simdata_sum$tpd[[i]]) %>% nest(tpc = c(t,p))

  # add column
  simdata_sum$tpc[i] <- tpc

}

# reshape TPC column
simdata_sum <- simdata_sum %>% unnest(cols = c(tpc))

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

# assign denstiy plot object
perf_dens_plot_data <- simdata_sum %>%
  select(corr, gen, N, pred_perf) %>%
  unnest(pred_perf) %>%
  ggplot(aes(x = pred_perf, fill = corr), col = NA) +
  geom_density(col = NA, alpha = 0.5) +
  scale_fill_manual(values = c("red","blue","green","yellow")) +
  facet_grid(cols = vars(gen))

# extract density plot data
perf_dens_plot_data <- ggplot_build(perf_dens_plot_data)$data[[1]]

# reshape performance density plot data object
perf_dens_plot_data <- perf_dens_plot_data %>%
  select(fill, y, x, PANEL) %>%
  mutate(density = y, t = x, corr = fill, gen = PANEL) %>%
  select(density, t, corr, gen)

# reshape correlations variable
perf_dens_plot_data$corr <- ifelse(perf_dens_plot_data$corr == "red", "both",
                              ifelse(perf_dens_plot_data$corr == "blue", "gsto",
                                     ifelse(perf_dens_plot_data$corr == "green", "none", "tde")))
# reshape generation variable
perf_dens_plot_data$gen <- ifelse(perf_dens_plot_data$gen == 1, gens[1],
                             ifelse(perf_dens_plot_data$gen == 2, gens[2],
                                    ifelse(perf_dens_plot_data$gen ==3, gens[3], gens[4])))

# merge density plot data with population size data
perf_dens_plot_data <- merge(perf_dens_plot_data, popsizes, by = c("gen", "corr"), all = TRUE)

# reset factor levels of summary sim dataset
perf_dens_plot_data$corr <- ifelse(perf_dens_plot_data$corr == "both", "GSTO + TDE",
                               ifelse(perf_dens_plot_data$corr == "gsto", "GSTO",
                                      ifelse(perf_dens_plot_data$corr == "tde", "TDE", "None")))

# reorder factor levels of summry sim dataset
perf_dens_plot_data$corr <- factor(perf_dens_plot_data$corr, levels = c("None", "GSTO", "TDE", "GSTO + TDE"))

# plot
pdf_plot <- perf_dens_plot_data %>%
  ggplot(aes(x = t, y = density * (n/100), fill = corr, col = corr)) +
  geom_area(alpha = 0.5, position = "identity", lwd = 0.5) +
  geom_vline(xintercept = 5, lty = 2) +
  scale_color_manual(values = colors) +
  scale_fill_manual(values = colors) +
  coord_cartesian(ylim = c(3,max(perf_dens_plot_data$t))) +
  ylab("Scaled Density") +
  xlab("Performance") +
  scale_y_continuous(expand = expansion(mult = c(0,0.01))) +
  scale_x_continuous(expand = c(0,0), breaks = seq(0,14, by = 2)) +
  facet_grid(cols = vars(gen)) +
  theme_minimal() +
  theme(legend.position = "bottom",
        legend.title = element_blank(),
        strip.text = element_text(size = 12),
        panel.border = element_rect(size = 0.5, fill = NA, color = "black"),
        panel.grid = element_blank(),
        plot.margin = unit(c(1,1,1,1), "lines")) +
  coord_flip()

## Getting performance metrics

perf_data <- perf_dens_plot_data

perf_data <- perf_data %>% select(gen = gen, corr = corr, density = density,
                    performance = t, n = n)

perf_data[rep(seq_along(perf_data$density))]

dsplot[rep(seq_along(dsplot$density), dsplot$density),]

perf_dens_plot_data %>%
  select(gen, corr, scaled_density) %>%
  group_by(gen, corr) %>%
  summarise(scaled_density = sum(scaled_density)) %>%
  ungroup() %>%
  as.data.frame()

# mean performance
simdata_sum$m_pred_perf <- rep(NA, nrow(simdata_sum))

for(i in 1:nrow(simdata_sum)){

  simdata_sum$m_pred_perf[i] <- mean(simdata_sum$pred_perf[[i]])

}

# population size * performance
simdata_sum$n_p <- simdata_sum$m_pred_perf * (simdata_sum$N / 100)

# times performance was below X
simdata_sum$times_zero <- rep(NA, nrow(simdata_sum))
simdata_sum$times_below <- rep(NA, nrow(simdata_sum))

for(i in 1:nrow(simdata_sum)){

  perfs <- simdata_sum$pred_perf[[i]]

  simdata_sum$times_zero[i] <- (length(perfs[perfs == 0])/length(perfs)) * 100
  simdata_sum$times_below[i] <- (length(perfs[perfs < 5])/length(perfs)) * 100

}

## Ridges plot trial

# add scaled density variable
perf_dens_plot_data <- perf_dens_plot_data %>% mutate(scaled_density = density * (n/100))

# data for densities plot
dsplot <- perf_dens_plot_data %>% select(gen = gen, corr = corr, performance = t, scaled_density = scaled_density)

# rescale density variable
dsplot$density <- round((dsplot$sdensity / max(dsplot$sdensity)) * 1000)

# reshape dataset
dsplot2 <- dsplot[rep(seq_along(dsplot$density), dsplot$density),]

dsplot2 %>%
  ggplot(aes(x = performance, y = corr, fill = corr, col = corr)) +
  geom_density_ridges() +
  xlim(5,max_perf) +
  facet_grid(cols = vars(gen)) +
  theme_minimal() +
  theme(legend.position = "bottom")


library(bayesplot)

dsplot2 %>%
  ggplot(aes(x = ))

dsplot %>%
  ggplot(aes(x = performance, y = corr, height = scaled_density, group = corr, col = corr, fill = corr)) +
  geom_density_ridges(stat = "identity" , scale = 5, alpha = 0.5, lwd = 0.5 ) +
  scale_color_manual(values = colors) +
  scale_fill_manual(values = colors) +
  geom_vline(xintercept = 5, lty = 2) +
  facet_grid(cols = vars(gen)) +
  xlab("Performance") +
  scale_x_continuous(expand = c(0,0), breaks = seq(0,14, by = 2)) +
  theme_minimal() +
  theme(legend.position = "bottom",
        legend.title = element_blank(),
        strip.text = element_text(size = 12),
        axis.text.x = element_blank(),
        axis.title.x = element_blank(),
        panel.border = element_rect(size = 0.5, fill = NA, color = "black"),
        panel.grid = element_blank(),
        plot.margin = unit(c(1,1,1,1), "lines")) +
  coord_flip()

## Barplot for area under the curve

# make data
barplot_data <- tibble(gen = rep(c(5,30,55,75),4),
                       corr = c(rep("None",4), rep("GSTO",4), rep("TDE",4), rep("GSTO + TDE",4)),
                       area = c(4835,4965,4387,746,5550,5950,5315,654,5150,5550,5603,5020,5350,5900,6250,4458))

# reorder factor levels
barplot_data$corr <- factor(barplot_data$corr, levels = c("None", "GSTO", "TDE", "GSTO + TDE"))

# plot itself
barplot_data %>%
  ggplot(aes(x = as.factor(gen), y = area, fill = factor(corr), col = factor(corr))) +
  geom_bar(stat = "identity", position = "dodge", alpha = 0.5) +
  scale_color_manual(values = colors) +
  scale_fill_manual(values = colors) +
  xlab("Generation") +
  ylab("Combined Performance (Area)") +
  scale_y_continuous(expand = c(0,0), breaks = seq(1000,6000, by = 1000)) +
  theme_minimal() +
  theme(legend.position = "bottom",
        legend.title = element_blank(),
        axis.text.x = element_text(size = 12),
        panel.grid = element_blank(),
        axis.line = element_line(color = "black", size = 1))

## Plot for population size over time

# rename dataset
simdata_2 <- p500

# get the summary data set
simdata_2 <- simdata_2 %>%
  filter(tseq != "control") %>%
  group_by(gen,corr,tseq) %>%
  summarise(as.data.frame(table(gen, corr, tseq))) %>%
  ungroup() %>%
  mutate(mean_n = Freq / 100) %>%
  mutate(gen = as.numeric(gen))

# rename & reorder factor levels corr
levels(simdata_2$corr) <- c("GSTO + TDE", "GSTO", "None", "TDE")
simdata_2$corr <- factor(simdata_2$corr, levels = c("None", "GSTO", "TDE", "GSTO + TDE"))

# rename & reorder factor levels tseq
levels(simdata_2$tseq) <- c("RCP 4.5", "RCP 6", "RCP 8.5")

# plot itself
simdata_2 %>%
  ggplot(aes(x = gen, y = mean_n, col = corr)) +
  geom_hline(aes(yintercept = 500), lty = 2) +
  geom_line(lwd = 1.1) +
  scale_color_manual(values = colors, name = "Genetic Correlation") +
  scale_x_continuous(breaks = seq(0, 80, by = 20)) +
  xlab("Generation") + ylab("Mean Population Size") +
  facet_grid(cols = vars(tseq)) +
  theme_minimal() +
  theme(legend.position = c(0.12,0.20),
        strip.text = element_text(size = 12),
        panel.grid.minor = element_blank(),
        strip.text.x = element_text(size = 12, face = "bold"),
        panel.border = element_rect(size = 0.5, fill = NA, color = "black"))

## Plot trait dynamic over time

# get tseq characteristics per generation
tseq_means <- tseq %>%
  group_by(gen) %>%
  summarise(mean_t = mean(t),
            max_t = max(t),
            min_t = min(t))

# get trait characteristics
simdata_traits <- simdata %>%
  group_by(corr, gen) %>%
  summarise(topt = mean(topt),
            ctmax = mean(ctmax),
            ctmin = mean(ctmin))

# trying to plot
simdata_traits %>%
  ggplot(aes(x = gen, y = topt, col = corr)) +
  geom_line() +
  geom_line(data = tseq_means, aes(x = gen, y = mean_t))

ggplot()+
  geom_ribbon(data = tseq_means, aes(x = gen, ymin = min_t, ymax = max_t), alpha = 0.5) +
  geom_line(data = tseq_means, aes(x = gen, y = mean_t)) +
  geom_ribbon(data = simdata_traits, aes(x = gen, ymin = ctmin, ymax = ctmax, fill = corr, col = corr), alpha = 0.25) +
  geom_line(data = simdata_traits, aes(x = gen, y = topt, col = corr), lwd = 1) +
  facet_grid(cols = vars(corr)) +
  theme(legend.position = "top")












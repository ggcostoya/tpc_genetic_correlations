
## Plot Alternative Figure 3 ##

## Packages ----

library(tidyverse)
library(parallel)
library(foreach)
library(doParallel)
library(gridExtra)
library(ggridges)
library(see)

## TPC plot -----

## Load and process data ----

## select generations of interest
gens <- c(5,30,55,75)

## add tseq_n and generation numbers to tseq
tseq <- foreach(i = 1:length(high)) %do% {

  foreach(j = 1:length(high[[i]])) %do% {

    # define range of values
    ts <- high[[i]][[j]]

    tibble(t = ts,
           tseq_n = rep(i, length(ts)),
           gen = rep(j, length(ts)))
  }
}
tseq <- do.call(rbind, (do.call(c,tseq)))

## get tseq summary
tseq_summary <- tseq %>%
  filter(gen %in% gens) %>%
  group_by(gen) %>%
  summarise(mean_t = mean(t),
            max_t = max(t),
            min_t = min(t)) %>%
  mutate(gen_label = c("Gen. 5", "Gen. 30", "Gen. 55", "Gen. 75"))

## process simulation data
summary <- p500 %>%
  filter(gen %in% gens) %>%
  filter(tseq == "+2.96°C") %>%
  group_by(corr, tseq, gen) %>%
  summarise(topt = mean(topt, na.rm = T),
            pmax = mean(pmax, na.rm = T),
            ctmin = mean(ctmin, na.rm = T),
            ctmax = mean(ctmax, na.rm = T),
            mid = mean(mid, na.rm = T)) %>%
  ungroup()

## function to build thermal-performance data set
build_tpd <- function(tpts){

  # temperature
  t <- round(tpts[1:4], 1)

  # performance
  pmax <- tpts[5]
  p <- c(0, pmax * 0.6, pmax, 0)

  return(data.frame(t,p))

}

## add an empty TPD column
summary$tpd <- rep(NA, nrow(summary))

## generate TPD for every row
for(i in 1:nrow(summary)){

  # get tpts
  tpts <- c(summary$ctmin[i],
            summary$mid[i],
            summary$topt[i],
            summary$ctmax[i],
            summary$pmax[i])

  # get tpd
  tpd <- build_tpd(tpts = tpts) %>% nest(cols = c(t,p))

  # add columns
  summary$tpd[i] <- tpd

}

## reshape and unnest TPD column
summary_plot <- summary %>% unnest(cols = c(tpd)) %>% unnest(cols = c(tpd))


## Plotting ----

# set color scale for different tradeoffs
colors <- c("None" = "#009E73", "GSTO" = "#56B4E9", "TDE" = "#CC79A7", "GSTO + TDE" = "#D55E00")

## label data for tpc plot
tpc_plot_label_data <- data.frame(
  label = c("a) Gen. 5", "b) Gen. 30", "c) Gen. 55", "d) Gen. 75"),
  gen = gens,
  x = rep(14,4), y = rep(14,4))

## tpc plot
tpc_plot <- ggplot() +
  geom_rect(data = tseq_summary, aes(xmin = min_t, xmax = max_t, ymin = -5, ymax = 16),
            fill = "orange", alpha = 0.25) +
  geom_vline(data = tseq_summary, aes(xintercept = mean_t), col = "black") +
  geom_line(data = summary_plot, aes(x = t, y = p, col = corr), lwd = 1.25, alpha = 0.75) +
  scale_color_manual(values = colors) +
  scale_y_continuous(breaks = seq(0,15, by = 2.5)) +
  xlab("Temperature") + ylab("Performance") +
  coord_cartesian(xlim = c(9.5,35), ylim = c(0,15)) +
  facet_grid(rows = vars(gen)) +
  theme_minimal() +
  theme(legend.position = "none",
        axis.title.y = element_text(size = 12),
        strip.text = element_blank(),
        axis.title.x = element_text(size = 12),
        axis.line.x = element_line(size = 0.5, color = "black"),
        axis.line.y = element_line(size = 0.5, color = "black"),
        panel.grid.major.x = element_line(size = 0.25),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.grid.major.y = element_line(size = 0.25),
        plot.margin = unit(c(1,0,1,1), "lines"))
tpc_plot <- tpc_plot + geom_text(data = tpc_plot_label_data, aes(x = x, y = y, label = label), size = 4)


## Distribution Plot ----

## Processing data ----

## function to generate a tpc from thermal-performance dataset
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

## add empty TPC column
summary$tpc <- rep(NA, nrow(summary))

## generate TPC for every row
for(i in 1:nrow(summary)){

  # get tpts
  tpts <- c(summary$ctmin[i],
            summary$mid[i],
            summary$topt[i],
            summary$ctmax[i],
            summary$pmax[i])

  # get tpd
  tpd <- build_tpd(tpts = tpts)

  # get TPC
  tpc <- generate_tpc(tpd = tpd) %>% nest(cols = c(t,p))

  # add columns
  summary$tpc[i] <- tpc

}

## reshape TPC column
summary <- summary %>% unnest(cols = c(tpc))

## add row of predicted performance per TPC
summary <- summary %>%
  mutate(pred_p = rep(NA, nrow(.)),
         sum_p = rep(NA, nrow(.)))

## function to predict performance
predict_performance <- function(summary, tseq){

  # filter temperatures of that generation
  tseq_gen <- tseq %>% filter(gen == summary$gen)

  # get temperature indexes
  indexes <- round(tseq_gen$t/0.1)

  # extract TPC
  tpc <- summary$tpc[[1]]

  # predict performance
  pred_p <- tpc$p[indexes]

  # extract densities
  p <- density(pred_p)$x
  freq <- density(pred_p)$y

  # nest pred_p object
  pred_p <- tibble(p = p, freq = freq) %>% nest(pred_p = c(p,freq))

  return(pred_p)

}

## loop to predict performance
for(i in 1:nrow(summary)){

  # predict performance
  summary$pred_p[i] <- predict_performance(summary = summary[i,], tseq = tseq)

}

## reshape predicted performance object
summary <- summary %>% unnest(cols = c(pred_p))

## rename summary object
summary1 <- summary

## load summary simulations data
simdata <- summary

## rename back summary object
summary <- summary1

## extract population sizes of generations of interest from simdata
popsizes <- simdata %>%
  filter(gen %in% gens) %>%
  filter(tseq == "+2.96°C") %>%
  filter(k == "K = 500") %>%
  select(n)

## add N column to summary dataset
summary <- summary %>% mutate(n = popsizes$n)

## Plotting ----

## label data for tpc plot
pdf_plot_label_data <- data.frame(
  label = c("e)", "f)", "g)", "h)"),
  gen = gens,
  x = rep(250,4), y = rep(14,4))


## Performance distribution plot
pdf_plot <- summary %>%
  select(corr,gen,pred_p,n) %>%
  unnest(cols = c(pred_p)) %>%
  mutate(scaled_freq = freq * n) %>%
  ggplot(aes(x = p, y = scaled_freq, col = corr, fill = corr)) +
  geom_area(position = "identity", alpha = 0.4) +
  scale_color_manual(values = colors) +
  scale_fill_manual(values = colors) +
  scale_y_continuous(expand = c(0,0), breaks = seq(0, 300, by = 100)) +
  scale_x_continuous(breaks = seq(0, 15, by = 2.5), limits = c(0,15)) +
  #geom_hline(aes(yintercept = 0), lwd = 1.25) +
  ylab("Number of individuals") +
  xlab("Performance") +
  coord_cartesian(ylim = c(0.5,15)) +
  facet_grid(rows = vars(gen)) +
  theme_minimal() +
  theme(legend.position = "none",
        legend.title = element_blank(),
        axis.title.x = element_text(size = 12),
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.grid.major.y = element_line(size = 0.25),
        axis.line.x = element_line(size = 0.5, color = "black"),
        axis.line.y = element_line(size = 0.5, color = "black"),
        strip.text = element_blank(),
        plot.margin = unit(c(1,1,1,0), "lines")) +
  coord_flip()

#pdf_plot <- pdf_plot + geom_text(data = pdf_plot_label_data, aes(x = x, y = y, label = label), size = 3.5)

grid.arrange(tpc_plot, pdf_plot, ncol = 2,
             widths = c(4,2))


## zoomed plots

tpc_plot_zoom <- tpc_plot + coord_cartesian(xlim = c(30,35), ylim = c(0,5))

pdf_plot_zoom <- pdf_plot + coord_cartesian(xlim = c(0,5), ylim = c(0,8))







## Plot Figure 4 ##

## Packages ----

library(tidyverse)
library(parallel)
library(foreach)
library(doParallel)
library(gridExtra)
library(ggridges)
library(see)
library(plyr) # careful

## Load and process data ----

## select generations of interest
gens <- c(5, 30, 55, 75)

## process simulations data
summary <- p500 %>%
  group_by(corr, tseq, gen) %>%
  summarise(topt = mean(topt, na.rm = T),
            pmax = mean(pmax, na.rm = T),
            ctmin = mean(ctmin, na.rm = T),
            ctmax = mean(ctmax, na.rm = T),
            mid = mean(mid, na.rm = T)) %>%
  ungroup() %>%
  filter(gen %in% gens) %>% # subset data for generations of interest
  filter(tseq == "+2.96°C")

## process temperature data

# add tseq_n and generation numbers to temperature sequence
tseq <- foreach(i = 1:length(high)) %do% {

  foreach(j = 1:length(high[[i]])) %do% {

    # define range of values
    ts <- high[[i]][[j]]

    tibble(t = ts,
           tseq_n = rep(i, length(ts)),
           gen = rep(j, length(ts)))
  }
}

# reshape raw data object
tseq <- do.call(rbind, (do.call(c,tseq)))

## Generate TPC per generation ----

## functions to generate TPD and TPC

# function to build a TPD
build_tpd <- function(tpts){

  # temperature
  t <- round(tpts[1:4], 1)

  # performance
  pmax <- tpts[5]
  p <- c(0, pmax * 0.6, pmax, 0)

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

## generate a TPD and TPC for each generation

# empty store variables
summary$tpc <- rep(NA, nrow(summary))

# generate TPD for every row
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

# reshape TPC column
summary <- summary %>% unnest(cols = c(tpc))

## Predict performance per generation ----

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

## Add information about population size ----

# rename summary object
summary1 <- summary

# load summary simulations data
simdata <- summary

# rename back summary object
summary <- summary1

# extract population sizes of generations of interest from simdata
popsizes <- simdata %>%
  filter(gen %in% gens) %>%
  filter(tseq == "+2.96°C") %>%
  filter(k == "K = 500") %>%
  select(n)

## add column to summary dataset
summary <- summary %>% mutate(n = popsizes$n)


## PLotting ----

## Generate generation naming variable
summary$gen_name <- ifelse(summary$gen == 5, "Gen. 5",
                           ifelse(summary$gen == 30, "Gen. 30",
                                  ifelse(summary$gen == 55,
                                         "Gen. 55", "Gen. 75")))

## reorder factor levels
summary$gen_name <- factor(summary$gen_name,
                           levels = c("Gen. 5", "Gen. 30", "Gen. 55", "Gen. 75"))

## set colors
colors <- c("None" = "#009E73", "GSTO" = "#56B4E9", "TDE" = "#CC79A7", "GSTO + TDE" = "#D55E00")

## Performance distribution plot

pdf_plot <- summary %>%
  select(corr,gen_name,pred_p,n) %>%
  unnest(cols = c(pred_p)) %>%
  mutate(scaled_freq = freq * n) %>%
  ggplot(aes(x = p, y = scaled_freq, col = corr, fill = corr)) +
  geom_area(position = "identity", alpha = 0.4) +
  scale_color_manual(values = colors) +
  scale_fill_manual(values = colors) +
  scale_y_continuous(expand = c(0,0), breaks = seq(0, 200, by = 100)) +
  scale_x_continuous(expand = c(0,0), breaks = seq(0, 15, by = 2.5), limits = c(0,15)) +
  geom_hline(aes(yintercept = 0), lwd = 1.25) +
  ylab("Number of individuals") +
  xlab("Performance") +
  facet_grid(cols = vars(gen_name)) +
  theme_minimal() +
  theme(legend.position = "none",
        legend.title = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.grid.major.y = element_line(size = 0.25),
        axis.line.x = element_line(size = 0.5, color = "black"),
        axis.line.y = element_line(size = 0.5, color = "black"),
        strip.text = element_text(size = 12)) +
  coord_flip()

## Area under the curve

area_plot <- summary %>%
  select(corr, gen, pred_p, n) %>%
  unnest(cols = c(pred_p)) %>%
  mutate(scaled_freq = freq * n) %>%
  mutate(p = ifelse(p < 0, 0, p)) %>%
  select(corr, gen, scaled_freq) %>%
  group_by(corr, gen) %>%
  summarise(area = sum(scaled_freq)/10000) %>%
  ungroup() %>%
  ggplot(aes(x = as.factor(corr), y = area, fill = corr, col = corr)) +
  geom_bar(stat = "identity", position = "dodge", alpha = 0.5) +
  scale_color_manual(values = colors) +
  scale_fill_manual(values = colors) +
  scale_y_continuous(expand = c(0,0)) +
  geom_vline(aes(xintercept = 0)) +
  ylab("Area under the curve (x 10^4)") +
  facet_grid(cols = vars(gen)) +
  theme_minimal() +
  theme(legend.position = "bottom",
        legend.title = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_text(margin = margin(r = 15)),
        panel.grid = element_blank(),
        strip.text.x = element_blank(),
        axis.line.x = element_line(size = 0.5, color = "black"),
        axis.line.y = element_line(size = 0.5, color = "black"))

## Combine plots

grid.arrange(pdf_plot, area_plot, nrow = 2,
             heights = c(4,3))
























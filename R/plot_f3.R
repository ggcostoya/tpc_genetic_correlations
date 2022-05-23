
## Plot Figure 2 ##

## Packages ----

library(tidyverse)
library(parallel)
library(foreach)
library(doParallel)
library(gridExtra)
library(ggridges)
library(see)

## Load and process data ----

## process simulations data
summary <- p500 %>%
  group_by(corr, tseq, gen) %>%
  summarise(topt = mean(topt, na.rm = T),
            pmax = mean(pmax, na.rm = T),
            ctmin = mean(ctmin, na.rm = T),
            ctmax = mean(ctmax, na.rm = T),
            mid = mean(mid, na.rm = T)) %>%
  ungroup()

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

# select generations of interest
gens <- c(5, 30, 55, 75)

# get temperature summary
tseq_summary <- tseq %>%
  filter(gen %in% gens) %>%
  group_by(gen) %>%
  summarise(mean_t = mean(t),
            max_t = max(t),
            min_t = min(t)) %>%
  mutate(gen_label = c("Gen. 5", "Gen. 30", "Gen. 55", "Gen. 75"))

## Plot temperature distribution ---

tseq_dist_plot <- tseq %>%
  filter(gen %in% gens) %>%
  ggplot(aes(x = t)) +
  geom_density(color = NA, fill = "orange", alpha = 0.5) +
  geom_vline(data = tseq_summary, aes(xintercept = mean_t), col = "darkorange", lwd = 1.25, alpha = 0.75) +
  geom_text(data = tseq_summary, aes(x = 14, y = 0.05, label = gen_label), size = 4) +
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
        plot.margin = unit(c(1,1,-0.1,1), "lines"))

## Plot full TPC ----

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
summary$tpd <- rep(NA, nrow(summary))

# generate TPD for every row
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

# reshape and unnest TPD column
summary <- summary %>% unnest(cols = c(tpd)) %>% unnest(cols = c(tpd))

# set color scale for different tradeoffs
colors <- c("None" = "#009E73", "GSTO" = "#56B4E9", "TDE" = "#CC79A7", "GSTO + TDE" = "#D55E00")

summary_subset <- summary %>%
  filter(gen %in% gens) %>%
  filter(tseq == "+2.96°C")

tpc_plot <- ggplot() +
  geom_rect(data = tseq_summary, aes(xmin = min_t, xmax = max_t, ymin = -5, ymax = 15),
            fill = "orange", alpha = 0.25) +
  geom_vline(data = tseq_summary, aes(xintercept = mean_t), col = "black") +
  geom_line(data = summary_subset, aes(x = t, y = p, col = corr), lwd = 1.25, alpha = 0.75) +
  scale_color_manual(values = colors) +
  coord_cartesian(xlim = c(9.5,35), ylim = c(0.5,13.5)) +
  facet_grid(cols = vars(gen)) +
  theme_minimal() +
  theme(legend.position = "none",
        axis.title.y = element_text(color = "white"),
        strip.text = element_blank(),
        axis.title.x = element_blank(),
        panel.border = element_rect(size = 0.5, fill = NA, color = "black"),
        panel.grid = element_blank(),
        plot.margin = unit(c(-0.1,1,0.5,1), "lines"))

tpc_plot + coord_cartesian(xlim = c(30, 34), ylim = c(0.5,7))

tpc_plot + coord_cartesian(xlim = c(22,30.5), ylim = c(8,13.5))



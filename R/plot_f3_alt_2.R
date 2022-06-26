
## Plot Figure 3 ##

## Packages ----

library(tidyverse)
library(parallel)
library(foreach)
library(doParallel)
library(gridExtra)

## Load and process data ----

## For trait information, load "k500_traits.RData" file"

# filtet climate change scenario of interest
traits <- k500_traits %>%
  filter(tseq == "high")

## Proces tmperature sequence information --

# load "high" tseq file

# add tseq_n and generation numbers to tseq
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

# summarise thermal characteristics per generation
tseq_sum <- tseq %>%
  group_by(gen) %>%
  summarise(mean_t = mean(t), max_t = max(t), min_t = min(t))

## Plotting ----

## rename and relevel factor levels

# genetic correlation
traits$corr <- as.factor(traits$corr)
levels(traits$corr) <- c("GSTO + TDE", "GSTO", "None", "TDE")
traits$corr <- factor(traits$corr, levels = c("None", "GSTO","TDE","GSTO + TDE"))

# set color scale for different tradeoffs
colors <- c("None" = "#009E73", "GSTO" = "#56B4E9", "TDE" = "#CC79A7", "GSTO + TDE" = "#D55E00")

## Topt and Mean Temperature --

topt_plot <- ggplot() +
  geom_line(data = tseq_sum %>% filter(gen > 4),
            aes(x = gen, y = mean_t),
            lwd = 1.25, alpha = 0.75) +
  geom_line(data = traits %>% filter(gen > 4),
            aes(x = gen, y = topt, col = corr),
            lwd = 1.25, alpha = 0.75) +
  annotate("text", label = "a)", y = 25.2, x = 80, size = 5) +
  annotate("text", label = expression(T[opt]), y = 25.2, x = 70, size = 5) +
  scale_color_manual(values = colors) +
  scale_x_continuous(expand = c(0,0)) +
  ylab("Mean temperature (°C)") +
  theme_minimal() +
  theme(axis.title.x = element_blank(),
        legend.position = "top",
        legend.title = element_blank(),
        legend.text = element_text(size =  10),
        strip.text = element_blank(),
        axis.line = element_line(),
        axis.title = element_text(size = 12),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_line(size = 0.25))

## CTmax and Max Temperature

ctmax_plot <- ggplot() +
  geom_line(data = tseq_sum %>% filter(gen > 4),
            aes(x = gen, y = max_t),
            lwd = 1.25, alpha = 0.75) +
  geom_line(data = traits %>% filter(gen > 4),
            aes(x = gen, y = ctmax, col = corr),
            lwd = 1.25, alpha = 0.75) +
  annotate("text", label = "b)", y = 28.4, x = 80, size = 5) +
  annotate("text", label = expression(CT[max]), y = 28.4, x = 70, size = 5) +
  scale_color_manual(values = colors) +
  scale_x_continuous(expand = c(0,0)) +
  ylab("Maximum temperature (°C)") +
  theme_minimal() +
  theme(axis.title.x = element_blank(),
        legend.position = "none",
        strip.text = element_blank(),
        axis.line = element_line(),
        axis.title = element_text(size = 12),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_line(size = 0.25))

## Reproductive success

r_plot <- ggplot() +
  geom_line(data = traits %>% filter(gen %in% c(5:84)),
            aes(x = gen, y = r, col = corr),
            lwd = 1.25, alpha = 0.75) +
  scale_color_manual(values = colors) +
  scale_x_continuous(expand = c(0,0),
                     limits = c(5, 85)) +
  scale_y_continuous(expand = c(0,0),
                     breaks = seq(0,15, by = 3),
                     limits = c(0,15)) +
  annotate("text", label = "c)", y = 14.8, x = 80, size = 5) +
  ylab("Mean reproductive sucess") +
  xlab("Generation") +
  theme_minimal() +
  theme(legend.position = "none",
        strip.text = element_blank(),
        axis.line = element_line(),
        axis.title = element_text(size = 12),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_line(size = 0.25))

## Combine plots

grid.arrange(topt_plot, ctmax_plot, r_plot, ncol = 1, nrow = 3,
             heights = c(4.1,4,4.1))


























































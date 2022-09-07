
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
  filter(tseq == "mid")

## Proces tmperature sequence information --

# load "high" tseq file

# add tseq_n and generation numbers to tseq ## Changed to Mid
tseq <- foreach(i = 1:length(mid)) %do% {

  foreach(j = 1:length(mid[[i]])) %do% {

    # define range of values
    ts <- mid[[i]][[j]]

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

## Reproductive success plot

r_plot <- ggplot() +
  geom_line(data = traits %>% filter(gen > 4, gen < 85),
            aes(x = gen, y = r, col = corr),
            lwd = 1.25, alpha = 0.75) +
  geom_hline(yintercept = 1, size = 1, alpha = 0.75, lty = 2) +
  annotate("text",label = expression(paste(lambda," " ,"= 1")), x = 80, y = 2, size = 6) +
  scale_color_manual(values = colors) +
  scale_x_continuous(expand = c(0,0),
                     limits = c(5, 85),
                     breaks = c(5,20,40,60,80)) +
  scale_y_continuous(expand = c(0,0),
                     breaks = c(0,1,seq(3,15, by = 3)),
                     limits = c(0,16)) +
  annotate("text", label = "A.", y = 15, x = 8, size = 4) +
  ylab("Mean reproductive sucess") +
  xlab("Generation") +
  theme_minimal() +
  theme(legend.position = c(0.15,0.275),
        legend.title = element_blank(),
        legend.text = element_text(size = 10),
        strip.text = element_blank(),
        axis.line = element_line(),
        axis.title = element_text(size = 12),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_line(size = 0.25))

## CTmax & Maximum Temperature plot --

ctmax_plot <- ggplot() +
  geom_line(data = tseq_sum %>% filter(gen > 4),
            aes(x = gen, y = max_t),
            lwd = 1.25, alpha = 0.75) +
  geom_line(data = traits %>% filter(gen > 4),
            aes(x = gen, y = ctmax, col = corr),
            lwd = 1.25, alpha = 0.75) +
  annotate("text", label = "B.", y = 35, x = 11, size = 4) +
  annotate("text", label = expression(CT[max]), y = 35, x = 23.5, size = 5) +
  scale_color_manual(values = colors) +
  scale_x_continuous(expand = c(0,0)) +
  scale_y_continuous(expand = c(0,0), limits = c(27.5,35.5)) +
  ylab("Maximum temperature (°C)") +
  theme_minimal() +
  theme(legend.position = "none",
        legend.title = element_blank(),
        legend.text = element_text(size = 11),
        strip.text = element_blank(),
        axis.line = element_line(),
        axis.title = element_text(size = 12),
        axis.title.x = element_blank(),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_line(size = 0.25))

## Topt and Mean Temperature --

topt_plot <- ggplot() +
  geom_line(data = tseq_sum %>% filter(gen > 4),
            aes(x = gen, y = mean_t),
            lwd = 1.25, alpha = 0.75) +
  geom_line(data = traits %>% filter(gen > 4),
            aes(x = gen, y = topt, col = corr),
            lwd = 1.25, alpha = 0.75) +
  annotate("text", label = "C.", y = 28, x = 11, size = 4) +
  annotate("text", label = expression(T[opt]), y = 28, x = 19, size = 5) +
  scale_color_manual(values = colors) +
  scale_x_continuous(expand = c(0,0)) +
  scale_y_continuous(expand = c(0,0), limits = c(24.5,28.25)) +
  ylab("Mean temperature (°C)") +
  theme_minimal() +
  theme(axis.title.x = element_blank(),
        legend.position = "none",
        legend.title = element_blank(),
        legend.text = element_text(size =  10),
        strip.text = element_blank(),
        axis.line = element_line(),
        axis.title = element_text(size = 12),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_line(size = 0.25))

## CTmin and Min Temperature --

ctmin_plot <- ggplot() +
  geom_line(data = tseq_sum %>% filter(gen > 4),
            aes(x = gen, y = min_t),
            lwd = 1.25, alpha = 0.75) +
  geom_line(data = traits %>% filter(gen > 4),
            aes(x = gen, y = ctmin, col = corr),
            lwd = 1.25, alpha = 0.75) +
  annotate("text", label = "D.", y = 27, x = 11, size = 4) +
  annotate("text", label = expression(CT[min]), y = 27, x = 23, size = 5) +
  scale_color_manual(values = colors) +
  scale_x_continuous(expand = c(0,0)) +
  scale_y_continuous(expand = c(0,0), limits = c(9,28)) +
  ylab("Minimum temperature (°C)") +
  xlab("Generation") +
  theme_minimal() +
  theme(legend.position = "none",
        legend.title = element_blank(),
        strip.text = element_blank(),
        axis.line = element_line(),
        axis.title = element_text(size = 12),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_line(size = 0.25))

## Pmax plot --

pmax_plot <- ggplot() +
  geom_line(data = traits %>% filter(gen > 4),
            aes(x = gen, y = pmax, col = corr),
            lwd = 1.25, alpha = 0.75) +
  annotate("text", label = "E.", y = 14, x = 11, size = 4) +
  annotate("text", label = expression(P[max]), y = 14, x = 21, size = 5) +
  scale_color_manual(values = colors) +
  scale_x_continuous(expand = c(0,0)) +
  scale_y_continuous(expand = c(0,0), limits = c(10,14.25)) +
  ylab("Maximum performance") +
  xlab("Generation") +
  theme_minimal() +
  theme(legend.position = "none",
        legend.title = element_blank(),
        legend.text = element_text(size =  10),
        strip.text = element_blank(),
        axis.line = element_line(),
        axis.title = element_text(size = 12),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_line(size = 0.25))

## Combine plots

lay <- rbind(c(1,1),c(2,3),c(4,5))

grid.arrange(r_plot,ctmax_plot,topt_plot,ctmin_plot,pmax_plot,
             layout_matrix = lay,
             heights = c(4.4,4,4.2))


## Get summary data

traits %>% filter(gen %in% c(0,5,84))

















































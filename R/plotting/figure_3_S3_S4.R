
## Plot Figures 3, S3, S4 ##

## Packages ----

library(tidyverse)
library(parallel)
library(foreach)
library(doParallel)
library(gridExtra)

## Load and process data ----

## For trait info ,load "simulations > processed_data > k500_traits.RData"

# filter climte change scenario of interest
traits <- k500_traits %>% filter(tseq == "high") #"high" F3, "low" S3, "mid" S4

# rename and relevel genetic correlations factors
traits$corr <- as.factor(traits$corr)
levels(traits$corr) <- c("GSTO + TDE", "GSTO", "None", "TDE")
traits$corr <- factor(traits$corr, levels =c("None","GSTO","TDE","GSTO + TDE"))

## For temperature sequence information, go to "temperature_sequences" and
## load "high.RData" for F3, "mid.RData" for S3 or "low.RData" for S4

# rename temperature sequence
tseq_raw <- high

# add tseq and generation numbers
tseq <- foreach(i = 1:length(tseq_raw)) %do% {

  foreach(j = 1:length(tseq_raw[[i]])) %do% {

    # define range of values
    ts <- tseq_raw[[i]][[j]]

    # construct dataset
    tibble(t = ts, tseq_n = rep(i, length(ts)), gen = rep(j, length(ts)))

  }
}

# reshape tseq object
tseq <- do.call(rbind, (do.call(c,tseq)))

# summarize thermal characteristics per generation
tseq_sum <- tseq %>% group_by(gen) %>%
  summarise(mean_t = mean(t), max_t = max(t), min_t = min(t))

## Plotting ---

# set color scale for gentic correlations
colors <- c("None" = "#009E73", "GSTO" = "#56B4E9",
            "TDE" = "#CC79A7", "GSTO + TDE" = "#D55E00")

# reproductive success plot
r_plot <- ggplot() +
  geom_line(data = traits %>% filter(gen > 4, gen < 85), # > 4 is after burn-in
            aes(x = gen, y = r, col = corr),
            lwd = 1.25, alpha = 0.75) +
  geom_hline(yintercept = 1, size = 1, alpha = 0.75, lty = 2) +
  geom_hline(yintercept = 15, size = 1, alpha = 0.75, col = "lightgray") +
  annotate("text", label = expression(paste(lambda," " ,"= 1")),
           x = 20, y = 2, size = 6) + # for Fig. S3. & S4, set to x = 80
  annotate("text", label = "A.",
           x = 8, y = 14, size = 4) +
  xlab("Generation") +
  ylab("Mean reproductive sucess") +
  scale_color_manual(values = colors) +
  scale_x_continuous(expand = c(0, 0),
                     limits = c(5, 85),
                     breaks = c(5, 20, 40, 60, 80)) +
  scale_y_continuous(expand = c(0, 0),
                     breaks = c(0, 1, seq(3,15, by = 3)),
                     limits = c(0, 16)) +
  theme_minimal() +
  theme(legend.position = c(0.9,0.75), # for Fig.S3 & S4 set to c(0.15, 0.275)
        legend.title = element_blank(),
        legend.text = element_text(size = 10),
        strip.text = element_blank(),
        axis.line = element_line(),
        axis.title = element_text(size = 12),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_line(size = 0.25))

# ctmax & maximum temperature plot
ctmax_plot <- ggplot() +
  geom_line(data = tseq_sum %>% filter(gen > 4),
            aes(x = gen, y = max_t),
            lwd = 1.25, alpha = 0.75) +
  geom_line(data = traits %>% filter(gen > 5),
            aes(x = gen, y = ctmax, col = corr),
            lwd = 1.25, alpha = 0.75) +
  annotate("text", label = expression("B." ~ CT[max]),
           y = 34.5, x = 20, size = 4) +
  scale_color_manual(values = colors) +
  scale_x_continuous(expand = c(0,0),
                     breaks = c(5, 20, 40, 60, 80)) +
  scale_y_continuous(expand = c(0,0),
                     limits = c(28,35)) +
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

# topt & mean temperature plot
topt_plot <- ggplot() +
  geom_line(data = tseq_sum %>% filter(gen > 4),
            aes(x = gen, y = mean_t),
            lwd = 1.25, alpha = 0.75) +
  geom_line(data = traits %>% filter(gen > 5),
            aes(x = gen, y = topt, col = corr),
            lwd = 1.25, alpha = 0.75) +
  annotate("text", label = expression("C." ~ T[opt]),
           y = 27.75, x = 20, size = 4) +
  scale_color_manual(values = colors) +
  scale_x_continuous(expand = c(0,0),
                     breaks = c(5, 20, 40, 60, 80)) +
  scale_y_continuous(expand = c(0,0),
                     limits = c(25,28)) +
  ylab("Mean Temperature (°C)") +
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

# ctmin & min temperature plot
ctmin_plot <- ggplot() +
  geom_line(data = tseq_sum %>% filter(gen > 4),
            aes(x = gen, y = min_t),
            lwd = 1.25, alpha = 0.75) +
  geom_line(data = traits %>% filter(gen > 5),
            aes(x = gen, y = ctmin, col = corr),
            lwd = 1.25, alpha = 0.75) +
  annotate("text", label = expression("D." ~ CT[min]),
           y = 25, x = 20, size = 4) +
  scale_color_manual(values = colors) +
  scale_x_continuous(expand = c(0,0),
                     breaks = c(5, 20, 40, 60, 80)) +
  scale_y_continuous(expand = c(0,0),
                     limits = c(9,26)) +
  ylab("Minimum Temperature (°C)") +
  xlab("Generation") +
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

# pmax plot
pmax_plot <- ggplot() +
  geom_line(data = traits %>% filter(gen > 5),
            aes(x = gen, y = pmax, col = corr),
            lwd = 1.25, alpha = 0.75) +
  annotate("text", label = expression("E." ~ P[max]),
           y = 13.75, x = 20, size = 4) +
  scale_color_manual(values = colors) +
  scale_x_continuous(expand = c(0,0)) +
  scale_y_continuous(expand = c(0,0), limits = c(10,14)) +
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

# let layout for combining plots
lay <- rbind(c(1,1),c(2,3),c(4,5))

# combine plots and final output
grid.arrange(r_plot,ctmax_plot,topt_plot,ctmin_plot,pmax_plot,
             layout_matrix = lay,
             heights = c(4.4,4,4.2))








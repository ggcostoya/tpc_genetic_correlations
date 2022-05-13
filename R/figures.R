
### Plot Figures ####

## Packages ----

library(tidyverse)
library(parallel)
library(foreach)
library(doParallel)
library(gridExtra)
library(ggridges)
library(see)


### Figure 1a: Polygon TPC vs usual TPC ----

# build base data
base <- tibble(t = c(10, 17.5, 25, 33.5), p = c(0,6,10,0))

plot(base, type = "l")

# build example TPC
ts <- seq(0,50, by = 1)
s <- 10
topt <- 25
b <- 33
c <- 7.5
# weibull function
ps <- ((s*(((c-1)/c)^((1-c)/c))*((((ts-topt)/b)+(((c-1)/c)^(1/c)))^(c-1))*(exp(-((((ts-topt)/b)+(((c-1)/c)^(1/c)))^c)+((c-1)/c)))))
tpc <- tibble(t = ts, p = ps)

# build plot
base %>%
  ggplot(aes(x = t, y = p)) +
  geom_line(data = tpc, aes(x = t, y = p), lwd = 3, col = "lightgray") +
  geom_point(lwd = 3) +
  geom_path(lwd = 1) +
  geom_segment(aes(x = 0, y = 10, xend = 25, yend = 10), lty = 2) +
  geom_segment(aes(x = 25, y = -1, xend = 25, yend = 10), lty = 2) +
  geom_segment(aes(x = 10, y = 0, xend = 10, yend = -1), lty = 2) +
  geom_segment(aes(x = 33.5, y = 0, xend = 33.5, yend = -1), lty = 2) +
  annotate("text", label = expression(P[max]), x = 5.5, y = 10.5, size = 5) +
  annotate("text", label = expression(CT[min]), x = 7.5, y = -0.5, size = 5) +
  annotate("text", label = expression(CT[max]), x = 31, y = -0.5, size = 5) +
  annotate("text", label = expression(T[opt]), x = 23, y = -0.5, size = 5) +
  annotate("text", label = "a)", x = 33, y = 10.5, size = 4) +
  coord_cartesian(ylim = c(-0.5,11), xlim = c(5,35)) +
  xlab("Temperature") +
  ylab("Performance") +
  theme_minimal() +
  theme(axis.line = element_line(size = 1),
        axis.text = element_blank(),
        axis.title = element_text(size = 15),
        panel.grid = element_blank())

### Figure 1b -- Polygon TPC and Genetic Correlations --

# build base gsto data
base_gsto <- base %>% mutate(type = rep("GSTO",4), tpc = rep("base",4))
high_gsto <- base %>% mutate(t = t + c(3, 1.5, 0, -3),
                             p = p + c(0,1.5,3,0),
                             type = rep("GSTO",4), tpc = rep("high",4))
low_gsto <- base %>% mutate(t = t + c(-3, -1.5, 0, 3),
                             p = p + c(0,-1.5,-3,0),
                             type = rep("GSTO",4), tpc = rep("low",4))
gsto <- rbind(base_gsto, high_gsto, low_gsto)

# build base tde data
base_tde <- base %>% mutate(type = rep("TDE",4), tpc = rep("base",4))
high_tde <- base %>% mutate(t = t + c(0, 0.5, 3, 0),
                             p = p + c(0,1.5,3,0),
                             type = rep("TDE",4), tpc = rep("high",4))
low_tde <- base %>% mutate(t = t + c(0, -0.5, -3, 0),
                            p = p + c(0,-1.5,-3,0),
                            type = rep("TDE",4), tpc = rep("low",4))
tde <- rbind(base_tde, high_tde, low_tde)

# build base both data
base_both <- base %>% mutate(type = rep("GSTO + TDE",4), tpc = rep("base",4))
high_both <- base %>% mutate(t = t + c(3, 1.5, 3, -3),
                            p = p + c(0,1.5,3,0),
                            type = rep("GSTO + TDE",4), tpc = rep("high",4))
low_both <- base %>% mutate(t = t + c(-3, -1.5, -3, 3),
                           p = p + c(0,-1.5,-3,0),
                           type = rep("GSTO + TDE",4), tpc = rep("low",4))
both <- rbind(base_both, high_both, low_both)

# combine data
corrs <- rbind(gsto, tde, both)

# reshape factor levels
corrs$type <- factor(corrs$type,  levels = c("GSTO","TDE", "GSTO + TDE"))

# assign colors
colors_corr <- c("None" = "#009E73", "GSTO" = "#56B4E9", "TDE" = "#CC79A7", "GSTO + TDE" = "#D55E00")

# get text data
text_data <- data.frame(
  label = c("b) GSTO", "c) TDE", "d) GSTO + TDE"),
  type = c("GSTO", "TDE", "GSTO + TDE"),
  x = c(11,11,14),
  y = c(13,13,13))
text_data$type <- factor(text_data$type,  levels = c("GSTO","TDE", "GSTO + TDE"))

# plot

corrs %>%
  ggplot(aes(x = t, y = p, col = type)) +
  geom_path(aes(group = tpc), lwd = 1.25, alpha = 0.75) +
  geom_point(lwd = 2.5, alpha = 0.75) +
  scale_color_manual(values = colors_corr)+
  geom_point(data = base, aes(x = t, y = p), color = "black", lwd = 2.5) +
  geom_path(data = base, aes(x = t, y = p), color = "black", lwd = 1.25) +
  geom_text(data = text_data, aes(x = x, y = y, label = label), size = 4, col = "black") +
  xlab("Temperature") + ylab("Performance") +
  facet_grid(cols = vars(type)) +
  theme_minimal() +
  theme(legend.position = "none",
        axis.line = element_line(size = 1),
        axis.text = element_blank(),
        panel.grid = element_blank(),
        strip.text.x = element_blank())



### Figure 2 & S1: Population Size over time ----

## load p50, p500 and p5000 simulation files from supporting data

# process p50 data into a summary
summary_p50 <- p50 %>%
  group_by(gen, corr, tseq) %>%
  summarise(as.data.frame(table(gen, corr, tseq))) %>% # count
  ungroup() %>%
  mutate(mean_n = Freq / 100, # add descriptive columns
         gen = as.numeric(gen),
         n_k = rep(50, nrow(.)))

# process p500 data into a summary
summary_p500 <- p500 %>%
  group_by(gen, corr, tseq) %>%
  summarise(as.data.frame(table(gen, corr, tseq))) %>% # count
  ungroup() %>%
  mutate(mean_n = Freq / 100, # add descriptive columns
         gen = as.numeric(gen),
         n_k = rep(500, nrow(.)))

# due to its sheer size, p5000 is already summarized in this format

## combine datasets & reshape variable names

# combine datasets
summary <- rbind(summary_p50, summary_p500)

# rename & reorder factor levels of genetic correlation
levels(summary$corr) <- c("GSTO + TDE", "GSTO", "None", "TDE")
summary$corr <- factor(summary$corr, levels = c("None", "GSTO", "TDE", "GSTO + TDE"))

# rename and reorder factor levels of tseq
levels(summary$tseq) <- c("+ 0 C", "+ 1.44 C", "+ 1.76 C", "+ 2.96 C")

# set color values for genetic correlation
colors_corr <- c("None" = "#009E73", "GSTO" = "#56B4E9", "TDE" = "#CC79A7", "GSTO + TDE" = "#D55E00")

## processing thermal data

## function to process temperature data

process_tseq <- function(tseq){

  # loop to add extra information to raw tseq file
  tseq_processed <- foreach(i = 1:length(tseq)) %do% {

    foreach(j = 1:length(tseq[[i]])) %do% {

      tibble(t = tseq[[i]][[j]],
             tseq_n = rep(i, length(tseq[[i]][[j]])),
             gen = rep(j, length(tseq[[i]][[j]])))

    }
  }

  # reshape raw tseq file
  tseq_processed <- do.call(rbind, do.call(c, tseq_processed))

  return(tseq_processed)

}

## process tseq objects

p_control <- process_tseq(control) %>% mutate(tseq = rep("+ 0 C", nrow(.)))
p_rcp4.5 <-  process_tseq(rcp4.5) %>% mutate(tseq = rep("+ 1.44 C", nrow(.)))
p_rcp6 <-  process_tseq(rcp6) %>% mutate(tseq = rep("+ 1.76 C", nrow(.)))
p_rcp8.5 <-  process_tseq(rcp8.5) %>% mutate(tseq = rep("+ 2.96 C", nrow(.)))

## combine tseq objects

tseqs <- rbind(p_control, p_rcp4.5, p_rcp6, p_rcp8.5)

## summarise tseq objects info

summary_tseqs <- tseqs %>%
  group_by(tseq, gen) %>%
  summarise(mean_t = mean(t),
            sd_t = sd(t),
            max_t = max(t),
            min_t = min(t)) %>%
  ungroup()

## ploting figure S1
fig_S1 <- summary %>%
  ggplot(aes(x = gen, y = mean_n, col = corr)) +
  geom_line(lwd = 1.1, alpha = 0.75) +
  geom_hline(yintercept = 0, lty = 2) +
  scale_color_manual(values = colors_corr, name = "Genetic Correlation") +
  scale_x_continuous(breaks = seq(0, 80, by = 20)) +
  xlab("Generation") +
  ylab("Mean Population Size") +
  facet_grid(cols = vars(tseq), rows = vars(n_k), scales = "free") +
  theme_minimal() +
  theme(legend.position = c(0.11, 0.15),
        legend.title = element_text(size = 10),
        legend.text = element_text(size = 8),
        strip.text = element_text(size = 12, face = "bold"),
        panel.border = element_rect(size = 0.5, fill = NA),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank())

summary %>%
  ggplot(aes(x = gen, y = mean_n, col = corr)) +
  geom_line() +
  g
  facet_grid(cols = vars(tseq), rows = vars(n_k), scales = "free")

ggplot() +
  geom_line(data = summary, aes(x = gen, y = mean_n, col = corr)) +
  geom_line(data = summary_tseqs, aes(x = gen, y = mean_t)) +
  scale_y_continuous(name = "Mean population size",
                     sec.axis = sec_axis(~. /50 + 28,
                                         name = "Temperature")) +
  facet_grid(cols = vars(tseq), rows = vars(n_k), scales = "free")


subset_data <- summary %>%
  filter(n_k > 50, tseq != "+ 0 C")




## plotting figure 2
fig_2 <- summary %>%
  filter(n_k == 500, tseq != "+ 0 C") %>%
  ggplot(aes(x = gen, y = mean_n, col = corr)) +
  geom_hline(yintercept = c(0,500), lty = 2) +
  geom_line(lwd = 1.1, alpha = 0.75) +
  scale_color_manual(values = colors_corr, name = "Genetic Correlation") +
  scale_x_continuous(breaks = seq(0, 80, by = 20)) +
  xlab("Generation") +
  ylab("Mean Population Size") +
  facet_grid(cols = vars(tseq)) +
  theme_minimal() +
  theme(legend.position = c(0.11, 0.22),
        legend.title = element_text(size = 10),
        legend.text = element_text(size = 8),
        strip.text = element_text(size = 12, face = "bold"),
        panel.border = element_rect(size = 0.5, fill = NA),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank())

### Figure S3 Temperature Sequence Dynamics ----

## load control, rcp4.5, rcp8.5, rcp8.5 files

## function to process temperature data

process_tseq <- function(tseq){

  # loop to add extra information to raw tseq file
  tseq_processed <- foreach(i = 1:length(tseq)) %do% {

    foreach(j = 1:length(tseq[[i]])) %do% {

      tibble(t = tseq[[i]][[j]],
             tseq_n = rep(i, length(tseq[[i]][[j]])),
             gen = rep(j, length(tseq[[i]][[j]])))

    }
  }

  # reshape raw tseq file
  tseq_processed <- do.call(rbind, do.call(c, tseq_processed))

  return(tseq_processed)

}

## process tseq objects

p_control <- process_tseq(control) %>% mutate(tseq = rep("+ 0 C", nrow(.)))
p_rcp4.5 <-  process_tseq(rcp4.5) %>% mutate(tseq = rep("+ 1.44 C", nrow(.)))
p_rcp6 <-  process_tseq(rcp6) %>% mutate(tseq = rep("+ 1.76 C", nrow(.)))
p_rcp8.5 <-  process_tseq(rcp8.5) %>% mutate(tseq = rep("+ 2.96 C", nrow(.)))

## combine tseq objects

tseqs <- rbind(p_control, p_rcp4.5, p_rcp6, p_rcp8.5)

## summarise tseq objects info

summary_tseqs <- tseqs %>%
  group_by(tseq, gen) %>%
  summarise(mean_t = mean(t),
            sd_t = sd(t),
            max_t = max(t),
            min_t = min(t)) %>%
  ungroup()

## plot

summary_tseqs %>%
  ggplot(aes(x = gen, y = mean_t)) +
  geom_line() +
  geom_ribbon(aes(ymin = mean_t - sd_t, ymax = mean_t + sd_t), alpha = 0.5) +
  facet_grid(cols = vars(tseq))





















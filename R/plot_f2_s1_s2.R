
## Exploration of simulation results

## Packages ----

library(tidyverse)
library(parallel)
library(foreach)
library(doParallel)
library(gridExtra)
library(ggridges)
library(see)

## Figure 2 ----

## Reshape data ----

## p50

# load and reshape data
p50 <- sapply(.GlobalEnv, is.data.frame)
p50 <- do.call(rbind, mget(names(p50)[p50]))
rownames(p50) <- NULL

# rename factor levels
p50$corr <- as.factor(p50$corr)
levels(p50$corr) <- c("GSTO + TDE", "GSTO", "None", "TDE")
p50$corr <- factor(p50$corr, levels = c("None", "GSTO","TDE","GSTO + TDE"))
p50$tseq <- as.factor(p50$tseq)
levels(p50$tseq) <- c("+0°C","+2.96°C","+1.44°C","+1.76°C")
p50$tseq <- factor(p50$tseq, levels = c("+0°C","+1.44°C","+1.76°C","+2.96°C"))

# add column for carrying capacity
p50$k <- rep("50", nrow(p50))

# save p50 file
save(p50, file = "p50.RData")

## p5000

# load and reshape data
p5000 <- sapply(.GlobalEnv, is.data.frame)
p5000 <- do.call(rbind, mget(names(p5000)[p5000]))
rownames(p5000) <- NULL

# rename factor levels
p5000$corr <- as.factor(p5000$corr)
levels(p5000$corr) <- c("GSTO + TDE", "GSTO", "None", "TDE")
p5000$corr <- factor(p5000$corr, levels = c("None", "GSTO","TDE","GSTO + TDE"))
p5000$tseq <- as.factor(p5000$tseq)
levels(p5000$tseq) <- c("+0°C","+2.96°C","+1.44°C","+1.76°C")
p5000$tseq <- factor(p5000$tseq, levels = c("+0°C","+1.44°C","+1.76°C","+2.96°C"))

# add column for carrying capacity
p5000$k <- rep("5000", nrow(p5000))

# save p50 file
save(p5000, file = "p5000.RData")

## p500

# load and reshape data
p500 <- sapply(.GlobalEnv, is.data.frame)
p500 <- do.call(rbind, mget(names(p500)[p500]))
rownames(p500) <- NULL

# rename factor levels
p500$corr <- as.factor(p500$corr)
levels(p500$corr) <- c("GSTO + TDE", "GSTO", "None", "TDE")
p500$corr <- factor(p500$corr, levels = c("None", "GSTO","TDE","GSTO + TDE"))
p500$tseq <- as.factor(p500$tseq)
levels(p500$tseq) <- c("+0°C","+2.96°C","+1.44°C","+1.76°C")
p500$tseq <- factor(p500$tseq, levels = c("+0°C","+1.44°C","+1.76°C","+2.96°C"))

# save p50 file
save(p500, file = "p500.RData")

# count rows per unique gen, corr and tseq combination
summary_p500 <- p500 %>%
  group_by(corr, tseq) %>%
  summarise(as.data.frame(table(gen))) %>%
  ungroup() %>%
  mutate(mean_n = Freq / 100) %>%
  mutate(gen = as.numeric(gen)) %>%
  mutate(k = rep(500, nrow(.))) # add column for carrying capacity

# reshape output
summary_p500 <- summary_p500 %>%
  mutate(n = mean_n) %>%
  select(gen, n, corr, tseq, k)

# reshape p50 output
summary_p50 <- p50 %>%
  group_by(k, corr, tseq, gen) %>%
  summarise(n = mean(n)) %>%
  ungroup() %>%
  select(gen, n, corr, tseq, k)

# reshape p5000 output
summary_p5000 <- p5000 %>%
  group_by(k, corr, tseq, gen) %>%
  summarise(n = mean(n)) %>%
  ungroup() %>%
  select(gen, n, corr, tseq, k)

# combine datasets
summary <- rbind(summary_p50, summary_p500, summary_p5000)

# reshape k variable
summary$k <- as.factor(summary$k)
levels(summary$k) <- c("K = 50", "K = 500", "K = 5000")

# set color scale for different tradeoffs
colors <- c("None" = "#009E73", "GSTO" = "#56B4E9", "TDE" = "#CC79A7", "GSTO + TDE" = "#D55E00")

# Build label data sets
text_data <- data.frame(
  label = c("a)","b)","c)","d)","e)","f)","g)","h)","i)"),
  tseq = c("+1.44°C","+1.76°C", "+2.96°C","+1.44°C","+1.76°C", "+2.96°C","+1.44°C","+1.76°C", "+2.96°C"),
  k = c("K = 50","K = 50","K = 50","K = 500","K = 500","K = 500", "K = 5000","K = 5000","K = 5000"),
  x = rep(9,9),
  y = c(46,46,46,460,460,460,4600,4600,4600))
text_data$tseq <- factor(text_data$tseq,  levels = c("+1.44°C","+1.76°C", "+2.96°C"))

# plot t
summary %>%
  filter(gen > 4) %>%
  filter(tseq != "+0°C") %>%
  ggplot(aes(x = gen, y = n, col = corr)) +
  geom_line(lwd = 1, alpha = 0.75) +
  scale_color_manual(values = colors, name = "Genetic correlation") +
  geom_hline(aes(yintercept = 0), lty = 2) +
  scale_x_continuous(breaks = seq(5,85, by = 20)) +
  xlab("Generation") + ylab("Mean population size") +
  facet_grid(cols = vars(tseq), rows = vars(k), scales = "free") +
  geom_text(data = text_data, aes(x = x, y = y, label = label), size = 3, col = "black") +
  theme_minimal() +
  theme(legend.position = c(0.16, 0.12),
        legend.text = element_text(size = 9),
        legend.title = element_blank(),
        strip.text = element_text(size = 12),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_line(size = 0.25),
        panel.border = element_rect(size = 0.5, fill = NA, color = "black"))

### Figure S1 & S2 ----

# load and reshape data
p500s <- sapply(.GlobalEnv, is.data.frame)
p500s <- do.call(rbind, mget(names(p500s)[p500s]))
rownames(p500s) <- NULL

# rename factor levels for genetic correlation
p500s$corr <- as.factor(p500s$corr)
levels(p500s$corr) <- c("GSTO + TDE", "GSTO", "None", "TDE")
p500s$corr <- factor(p500s$corr, levels = c("None", "GSTO","TDE","GSTO + TDE"))

# summarize p500s dataset
summary_p500s <- p500s %>%
  group_by(corr, tseq, gen) %>%
  summarise(n = mean(n)) %>%
  ungroup()
summary_p500s$tseq <- as.factor(summary_p500s$tseq)

# generate Initial SD variable
summary_p500s$initial_sd <- ifelse(summary_p500s$tseq %in%  c("control_2sd", "high_2sd", "high_2sd_sd"),
                                   "Initial SD = 2°C", "Initial SD = 1°C")

# generate change in Mean and SD variable
summary_p500s$climate_model <- ifelse(summary_p500s$tseq == "control_2sd", "control",
                                      ifelse(summary_p500s$tseq == "high_2sd", "high",
                                             ifelse(summary_p500s$tseq == "high_m", "high_m", "high_sd")))

# extract observations of interest from summary data
extract_summary <- summary %>%
  filter(k == "K = 500", tseq %in% c("+0°C","+2.96°C"))

# generate new columns for extract summary
extract_summary$initial_sd <- rep("Initial SD = 1°C", nrow(extract_summary))
extract_summary$climate_model <- ifelse(extract_summary$tseq == "+0°C", "control", "high")

# reshape extract summary object
extract_summary <- extract_summary %>%
  select(corr,tseq,gen,n,initial_sd,climate_model)

# combine datasets
summary_p500s <- rbind(extract_summary, summary_p500s)

## figure S1

# reshape data for the figure
s1_data <- summary_p500s %>% filter(climate_model %in% c("control", "high"))
s1_data$climate_model <- ifelse(s1_data$climate_model == "control", "+0°C", "+2.96°C")

# generate text data
text_data_s1 <- data.frame(
  label = c("a)","b)","c)", "d)"),
  climate_model = c("+0°C","+0°C", "+2.96°C","+2.96°C"),
  initial_sd = c("Initial SD = 1°C", "Initial SD = 2°C", "Initial SD = 1°C", "Initial SD = 2°C"),
  x = rep(10,4),
  y = c(475,425,475,425))

s1_data %>%
  filter(gen > 4) %>%
  ggplot(aes(x = gen, y = n, col = corr)) +
  geom_hline(aes(yintercept = 0), lty = 2) +
  geom_line(lwd = 1, alpha = 0.75) +
  scale_color_manual(values = colors, name = "Genetic correlation") +
  scale_x_continuous(breaks = seq(5,85, by = 20)) +
  xlab("Generation") + ylab("Mean population size") +
  facet_grid(cols = vars(climate_model), rows = vars(initial_sd)) +
  geom_text(data = text_data_s1, aes(x = x, y = y, label = label), size = 3, col = "black") +
  theme_minimal() +
  theme(legend.position = c(0.2, 0.15),
        legend.text = element_text(size = 9),
        legend.title = element_blank(),
        strip.text = element_text(size = 12),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_line(size = 0.25),
        panel.border = element_rect(size = 0.5, fill = NA, color = "black"))


## figure S2

# subset data for figure
s2_data <- summary_p500s %>%
  filter(climate_model != "control", initial_sd == "Initial SD = 1°C")

# reshape climate model variable
s2_data$climate_model <- ifelse(s2_data$climate_model == "high", "M. +2.96°C, SD +0.44°C",
                                ifelse(s2_data$climate_model == "high_m",
                                       "M. +2.96°C, SD +0°C",
                                       "M. +0°C, SD +0.44°C"))
s2_data$climate_model <- as.factor(s2_data$climate_model)
s2_data$climate_model <- factor(s2_data$climate_model,
                                levels = c("M. +2.96°C, SD +0.44°C",
                                           "M. +2.96°C, SD +0°C",
                                           "M. +0°C, SD +0.44°C"))

# generate text data
text_data_s2 <- data.frame(
  label = c("a)","b)","c)"),
  climate_model = c("M. +2.96°C, SD +0.44°C",
                    "M. +2.96°C, SD +0°C",
                    "M. +0°C, SD +0.44°C"),
  x = rep(10,3),
  y = rep(475,3))
text_data_s2$climate_model <- as.factor(text_data_s2$climate_model)
text_data_s2$climate_model <- factor(text_data_s2$climate_model,
                                levels = c("M. +2.96°C, SD +0.44°C",
                                           "M. +2.96°C, SD +0°C",
                                           "M. +0°C, SD +0.44°C"))


s2_data %>%
  filter(gen > 4) %>%
  ggplot(aes(x = gen, y = n, col = corr)) +
  geom_hline(aes(yintercept = 0), lty = 2) +
  geom_line(lwd = 1.25, alpha = 0.75) +
  scale_color_manual(values = colors, name = "Genetic correlation") +
  scale_x_continuous(breaks = seq(5,85, by = 20)) +
  xlab("Generation") + ylab("Mean population size") +
  facet_wrap(~ climate_model, strip.position = "top") +
  geom_text(data = text_data_s2, aes(x = x, y = y, label = label), size = 4, col = "black") +
  theme_minimal() +
  theme(legend.position = c(0.13, 0.28),
        legend.text = element_text(size = 9),
        legend.title = element_blank(),
        strip.text = element_text(size = 10),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_line(size = 0.25),
        panel.border = element_rect(size = 0.5, fill = NA, color = "black"))





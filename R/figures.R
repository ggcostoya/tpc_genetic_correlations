
### Plot Figures ####

## Packages ----

library(tidyverse)
library(parallel)
library(foreach)
library(doParallel)
library(gridExtra)
library(ggridges)
library(see)

### Figure 2 & S1 - Population Size over time ----

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





















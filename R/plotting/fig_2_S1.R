
## Plot Figures 2 & S1 ##

## Packages ----

library(tidyverse)
library(gridExtra)

## Load "sum_sim_results.RData" for summary of all simulations

## Figure 2 ----

## Prepare data set

# filter data of interest
fig2_data <- sum_sim_results %>% filter(tseq %in% c("low", "mid", "high"))

# rename genetic correlation factor levels
fig2_data$corr <- as.factor(fig2_data$corr)
levels(fig2_data$corr) <- c("GSTO + TDE", "GSTO", "None", "TDE")
fig2_data$corr <- factor(fig2_data$corr,
                         levels = c("None", "GSTO","TDE","GSTO + TDE"))

# rename climate change scenario factor levels
fig2_data$tseq <- as.factor(fig2_data$tseq)
levels(fig2_data$tseq) <- c("RCP 8.5 (~ +3°C)",
                            "RCP 4.5 (~ +1.5°C)",
                            "RCP 6 (~ +2°C)")
fig2_data$tseq <- factor(fig2_data$tseq,
                         levels = c("RCP 4.5 (~ +1.5°C)",
                                    "RCP 6 (~ +2°C)",
                                    "RCP 8.5 (~ +3°C)"))

# build label dataset
text_fig2 <- data.frame(
  label = c("A.","B.","C.","D.","E.","F.","G.","H.","I."),
  tseq = rep(c("RCP 4.5 (~ +1.5°C)","RCP 6 (~ +2°C)","RCP 8.5 (~ +3°C)"), 3),
  n0_k = rep(c(50,500,5000), each = 3),
  x = rep(11,9),
  y = c(46,46,46,460,460,460,4600,4600,4600))

## Plotting

# set genetic correlation colors
colors <- c("None" = "#009E73", "GSTO" = "#56B4E9","TDE" = "#CC79A7",
            "GSTO + TDE" = "#D55E00")

# plot
fig2_data %>%
  filter(gen > 4) %>% # only showing period after burn-in
  ggplot(aes(x = gen, y = n, col = corr)) +
  geom_line(lwd = 1, alpha = 0.75) +
  geom_hline(aes(yintercept = 0), col = "black", lty = 2) +
  geom_text(data = text_fig2, aes(x = x, y = y, label = label),
            size = 3, col = "black") +
  xlab("Generation") +
  ylab("Mean population size (N)") +
  scale_color_manual(values = colors) +
  scale_x_continuous(expand = c(0,2),
                     breaks = seq(20,80, by = 20)) +
  facet_grid(as.factor(n0_k) ~ tseq, scales = "free") +
  theme_minimal() +
  theme(legend.position = c(0.16, 0.12),
        legend.title = element_blank(),
        legend.text = element_text(size = 10),
        strip.text.x = element_text(size = 12, vjust = 1),
        strip.text.y = element_blank(),
        axis.title = element_text(size = 12),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_line(size = 0.25),
        panel.border = element_rect(fill = NA))

## Figure S1 ----

## Prepare dataset

# filter data of interest
figS1_data <- sum_sim_results %>%
  filter(tseq %in%
           c("control", "control_2sd", "high_m", "high_sd", "high_2sd")) %>%
  filter(n0_k == 500)

# rename genetic correlation factor levels
figS1_data$corr <- as.factor(figS1_data$corr)
levels(figS1_data$corr) <- c("GSTO + TDE", "GSTO", "None", "TDE")
figS1_data$corr <- factor(figS1_data$corr,
                          levels = c("None", "GSTO","TDE","GSTO + TDE"))

# rename climate change scenario factor levels
figS1_data$tseq <- as.factor(figS1_data$tseq)
levels(figS1_data$tseq) <- c("A. Control, SD = 1°C", "B. Control, SD = 2°C",
                             "C. RCP 8.5, SD = 2°C",
                             "D. RCP 8.5, Mean change", "E. RCP 8.5, SD change")
## Plotting

figS1_data %>%
  filter(gen > 4) %>%
  ggplot(aes(x = gen, y = n, col = corr)) +
  geom_line(lwd = 1, alpha = 0.75) +
  scale_color_manual(values = colors) +
  geom_hline(aes(yintercept = 0), col = "black", lty = 2) +
  scale_x_continuous(expand = c(0,2), breaks = seq(20,80, by = 20)) +
  xlab("Generation") +
  ylab("Mean population size (N)") +
  facet_wrap(~tseq, scales = "free_x") +
  theme_minimal() +
  theme(legend.position = c(0.85, 0.38),
        legend.title = element_blank(),
        legend.text = element_text(size = 11),
        strip.text = element_text(size = 10, vjust = 1),
        axis.title = element_text(size = 12),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_line(size = 0.25),
        panel.border = element_rect(fill = NA))










## Plot Figure 2, Figure S1 & Figure S2 ##

## Packages ----

library(tidyverse)
library(gridExtra)

## Figure 2 ----

## filter data of interest
fig2_data <- sum_sim_results %>%
  filter(tseq %in% c("low", "mid", "high"))

## rename and relevel factor levels

# genetic correlation
fig2_data$corr <- as.factor(fig2_data$corr)
levels(fig2_data$corr) <- c("GSTO + TDE", "GSTO", "None", "TDE")
fig2_data$corr <- factor(fig2_data$corr,
                         levels = c("None", "GSTO","TDE","GSTO + TDE"))

# climate change scenario
fig2_data$tseq <- as.factor(fig2_data$tseq)
levels(fig2_data$tseq) <- c("RCP 8.5 (+2.96°C)",
                            "RCP 4.5 (+1.44°C)",
                            "RCP 6 (+1.76°C)")
fig2_data$tseq <- factor(fig2_data$tseq,
                         levels = c("RCP 4.5 (+1.44°C)",
                                    "RCP 6 (+1.76°C)",
                                    "RCP 8.5 (+2.96°C)"))

# initial population size (N0) and Carrying capacity (K)
fig2_data$n0_k <- as.factor(fig2_data$n0_k)
levels(fig2_data$n0_k) <- c("No & K = 50", "No & K = 500", "No & K = 5000")

## set colors
colors <- c("None" = "#009E73", "GSTO" = "#56B4E9",
            "TDE" = "#CC79A7", "GSTO + TDE" = "#D55E00")

# Build label data sets
text_fig2 <- data.frame(
  label = c("A.","B.","C.","D.","E.","F.","G.","H.","I."),
  tseq = as.factor(c("RCP 4.5 (+1.44°C)","RCP 6 (+1.76°C)","RCP 8.5 (+2.96°C)",
                     "RCP 4.5 (+1.44°C)","RCP 6 (+1.76°C)","RCP 8.5 (+2.96°C)",
                     "RCP 4.5 (+1.44°C)","RCP 6 (+1.76°C)","RCP 8.5 (+2.96°C)")),
  n0_k = as.factor(c("No & K = 50","No & K = 50","No & K = 50",
        "No & K = 500","No & K = 500","No & K = 500",
        "No & K = 5000","No & K = 5000","No & K = 5000")),
  x = rep(11,9),
  y = c(46,46,46,460,460,460,4600,4600,4600))
#text_data$tseq <- factor(text_data$tseq,  levels = c("+1.44°C","+1.76°C", "+2.96°C"))

## plot
fig2_data %>%
  filter(gen > 4) %>%
  ggplot(aes(x = gen, y = n, col = corr)) +
  geom_line(lwd = 0.75, alpha = 0.75) +
  scale_color_manual(values = colors) +
  geom_hline(aes(yintercept = 0), col = "black", lty = 2) +
  scale_x_continuous(expand = c(0,2), breaks = seq(20,80, by = 20)) +
  xlab("Generation") +
  ylab("Mean population size (N)") +
  facet_grid(n0_k ~ tseq, scales = "free") +
  geom_text(data = text_fig2, aes(x = x, y = y, label = label), size = 3, col = "black") +
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

## filter data of interest
figS1_data <- sum_sim_results %>%
  filter(tseq %in% c("control", "control_2sd", "high_m", "high_sd", "high_2sd")) %>%
  filter(n0_k == 500)

## rename and relevel factor levels

# genetic correlation
figS1_data$corr <- as.factor(figS1_data$corr)
levels(figS1_data$corr) <- c("GSTO + TDE", "GSTO", "None", "TDE")
figS1_data$corr <- factor(figS1_data$corr,
                         levels = c("None", "GSTO","TDE","GSTO + TDE"))

# climate change scenario
figS1_data$tseq <- as.factor(figS1_data$tseq)
levels(figS1_data$tseq) <- c("A. Co., SD = 1°C", "B. Co., SD = 2°C",
                             "C. RCP 8.5, SD = 2°C",
                             "D. RCP 8.5, Mean", "E. RCP 8.5, SD")

## plot
figS1_data %>%
  filter(gen > 4) %>%
  ggplot(aes(x = gen, y = n, col = corr)) +
  geom_line(lwd = 0.75, alpha = 0.75) +
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


## Extract of Figure 2 for Figure 3 ----

extract <- fig2_data %>%
  filter(n0_k == "No & K = 500", tseq == "RCP 8.5 (+2.96°C)") %>%
  filter(gen > 4) %>%
  ggplot(aes(x = gen, y = n, col = corr)) +
  geom_line(lwd = 0.75, alpha = 0.75) +
  scale_color_manual(values = colors) +
  geom_hline(aes(yintercept = 0), col = "black", lty = 2) +
  scale_x_continuous(expand = c(0,2), breaks = seq(20,80, by = 20)) +
  scale_y_continuous(expand = c(0,10)) +
  xlab("Generation") +
  ylab("Mean population size (N)") +
  theme_minimal() +
  theme(legend.position = c(0.30, 0.25),
        legend.title = element_blank(),
        legend.text = element_text(size = 10),
        strip.text.x = element_text(size = 12, vjust = 1),
        strip.text.y = element_blank(),
        axis.title = element_text(size = 12),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_line(size = 0.25),
        axis.line = element_line())




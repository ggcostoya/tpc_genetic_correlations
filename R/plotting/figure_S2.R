
## Plot Figure S2 ##

## Packages ----

library(tidyverse)
library(gridExtra)

## Load and process data ----

# load "k500_initial_traits.RData" into environment

# rename dataset
traits <- k500_initial_traits

# relevel genetic correlation factor
traits$corr <- as.factor(traits$corr)
levels(traits$corr) <- c("GSTO + TDE", "GSTO", "None", "TDE")
traits$corr <- factor(traits$corr, levels = c("None", "GSTO","TDE","GSTO + TDE"))

# obtain summarized dataset
traits_sum <- traits %>%
  group_by(gen, corr) %>%
  summarise(topt = mean(topt, na.rm = T),
            ctmax = mean(ctmax, na.rm = T),
            ctmin = mean(ctmin, na.rm = T),
            pmax = mean(pmax, na.rm = T)) %>%
  ungroup()

## Plotting

# set genetic correlation colors
colors <- c("None" = "#009E73", "GSTO" = "#56B4E9",
            "TDE" = "#CC79A7", "GSTO + TDE" = "#D55E00")

## create text for labelling

# ctmin vs pmax plot
ctmin_text <- data.frame(label = c("A.","D.","G.","J."),
                         corr = c("None","GSTO","TDE","GSTO + TDE"))
ctmin_text$corr <- as.factor(ctmin_text$corr)
ctmin_text$corr <- factor(ctmin_text$corr,
                          levels = c("None", "GSTO","TDE","GSTO + TDE"))

# topt vs pmax plot
topt_text <- data.frame(label = c("B.","E.","H.","K."),
                         corr = c("None","GSTO","TDE","GSTO + TDE"))
topt_text$corr <- as.factor(topt_text$corr)
topt_text$corr <- factor(topt_text$corr,
                          levels = c("None", "GSTO","TDE","GSTO + TDE"))

# topt vs pmax plot
ctmax_text <- data.frame(label = c("C.","F.","I.","L."),
                        corr = c("None","GSTO","TDE","GSTO + TDE"))
ctmax_text$corr <- as.factor(ctmax_text$corr)
ctmax_text$corr <- factor(ctmax_text$corr,
                         levels = c("None", "GSTO","TDE","GSTO + TDE"))

# topt vs pmax plot
topt <- ggplot() +
  geom_vline(xintercept = 25) +
  geom_point(data = traits %>% sample_n(1000),
             aes(x = topt, y = pmax, col = corr, shape = as.factor(gen)),
             size = 1, alpha = 0.15) +
  geom_point(data = traits_sum,
             aes(x = topt, y = pmax, col = corr, shape = as.factor(gen)),
             size = 4) +
  geom_smooth(data = traits,
              aes(x = topt, y = pmax, col = corr,
                  fill = corr, lty = as.factor(gen)),
              method = "lm", alpha = 0.25) +
  geom_text(data = topt_text, aes(label = label),
            x = 28, y = 7, size = 4) +
  geom_hline(yintercept = 6.1) +
  scale_color_manual(values = colors) +
  scale_y_continuous(expand = c(0,0), limits = c(6.1,14)) +
  xlab(expression(T[opt])) +
  facet_grid(rows = vars(corr)) +
  theme_minimal() +
  theme(legend.position = "none",
        strip.text = element_blank(),
        axis.line.y = element_line(),
        axis.title = element_text(size = 20),
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_line(size = 0.25))

# ctmax vs pmax plot
ctmax <- ggplot() +
  geom_point(data = traits %>% sample_n(1000),
             aes(x = ctmax, y = pmax, col = corr, shape = as.factor(gen)),
             size = 1, alpha = 0.1) +
  geom_point(data = traits_sum,
             aes(x = ctmax, y = pmax, col = corr, shape = as.factor(gen)),
             size = 4) +
  geom_smooth(data = traits,
              aes(x = ctmax, y = pmax, col = corr,
                  fill = corr, lty = as.factor(gen)),
              method = "lm", alpha = 0.25) +
  geom_text(data = ctmax_text, aes(label = label),
            x = 37.5, y = 7, size = 4) +
  geom_hline(yintercept = 6.1) +
  scale_color_manual(values = colors) +
  scale_y_continuous(expand = c(0,0), limits = c(6.1,14)) +
  xlab(expression(CT[max])) +
  facet_grid(rows = vars(corr)) +
  theme_minimal() +
  theme(legend.position = "none",
        strip.text = element_blank(),
        axis.title = element_text(size = 20),
        axis.line.y = element_line(),
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_line(size = 0.25))

# ctmin vs pmax
ctmin <- ggplot() +
  geom_point(data = traits %>% sample_n(1000),
             aes(x = ctmin, y = pmax, col = corr, shape = as.factor(gen)),
             size = 1, alpha = 0.1) +
  geom_point(data = traits_sum,
             aes(x = ctmin, y = pmax, col = corr, shape = as.factor(gen)),
             size = 4) +
  geom_smooth(data = traits,
              aes(x = ctmin, y = pmax, col = corr,
                  fill = corr, lty = as.factor(gen)),
              method = "lm", alpha = 0.25) +
  geom_text(data = ctmin_text, aes(label = label),
            x = 13.5, y = 7, size = 4) +
  geom_hline(yintercept = 6.1) +
  scale_color_manual(values = colors) +
  scale_y_continuous(expand = c(0,0), limits = c(6.1,14)) +
  xlab(expression(CT[min])) +
  ylab(expression(P[max])) +
  facet_grid(rows = vars(corr)) +
  theme_minimal() +
  theme(legend.position = "none",
        strip.text = element_blank(),
        axis.title = element_text(size = 20),
        axis.line.y = element_line(),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_line(size = 0.25))

# combining plots
grid.arrange(ctmin, topt, ctmax, ncol = 3, widths = c(4.5, 4, 4))




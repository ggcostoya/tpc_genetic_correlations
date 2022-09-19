
## Plot Figure 1 ##

## Packages ----

library(tidyverse)
library(gridExtra)

## Figure 1a: Polygon TPC vs Equation TPC ----

# build base data
base <- tibble(t = c(10, 17.5, 24, 34), p = c(0,6,10,0))

# build example TPC
ts <- seq(0,50, by = 1)
s <- 10
topt <- 24
b <- 29
c <-6.5
# weibull function
ps <- ((s*(((c-1)/c)^((1-c)/c))*((((ts-topt)/b)+(((c-1)/c)^(1/c)))^(c-1))*(exp(-((((ts-topt)/b)+(((c-1)/c)^(1/c)))^c)+((c-1)/c)))))
tpc <- tibble(t = ts, p = ps)

# build plot
base %>%
  ggplot(aes(x = t, y = p)) +
  geom_line(data = tpc, aes(x = t, y = p), lwd = 3, col = "lightgray") +
  geom_point(lwd = 3) +
  geom_path(lwd = 1) +
  geom_segment(aes(x = 0, y = 10, xend = 24, yend = 10), lty = 2) +
  geom_segment(aes(x = 25, y = -1, xend = 24, yend = 10), lty = 2) +
  geom_segment(aes(x = 10, y = 0, xend = 10, yend = -1), lty = 2) +
  geom_segment(aes(x = 34, y = 0, xend = 34, yend = -1), lty = 2) +
  annotate("text", label = expression(P[max]), x = 7.5, y = 10.75, size = 7) +
  annotate("text", label = expression(CT[min]), x = 7, y = -0.25, size = 7) +
  annotate("text", label = expression(CT[max]), x = 31, y = -0.25, size = 7) +
  annotate("text", label = expression(T[opt]), x = 22.5, y = -0.25, size = 7) +
  annotate("text", label = "Mid", x = 15.5, y = 6.5, size = 7) +
  annotate("text", label = "A.", x = 5, y = 12, size = 5) +
  coord_cartesian(ylim = c(-0.5,12), xlim = c(5,35)) +
  xlab("Temperature") +
  ylab("Performance") +
  theme_minimal() +
  theme(axis.line = element_line(size = 1),
        axis.text = element_blank(),
        axis.title = element_text(size = 16),
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
high_both <- base %>% mutate(t = t + c(3, 1.5, 2.5, -2.5),
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
  label = c("B. GSTO", "C. TDE", "D. GSTO + TDE"),
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


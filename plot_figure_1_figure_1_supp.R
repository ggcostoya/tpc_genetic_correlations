
## Plot supplementary Figure 1

## Packages ----

library(tidyverse)

## Process data ----

summary_p50 <- p50 %>%
  group_by(gen, corr, tseq) %>%
  summarise(as.data.frame(table(gen, corr, tseq))) %>%
  ungroup() %>%
  mutate(mean_n = Freq / 100) %>%
  mutate(gen = as.numeric(gen)) %>%
  mutate(N = rep(50, nrow(.)))

summary_p500 <- p500 %>%
  group_by(gen, corr, tseq) %>%
  summarise(as.data.frame(table(gen, corr, tseq))) %>%
  ungroup() %>%
  mutate(mean_n = Freq / 100) %>%
  mutate(gen = as.numeric(gen)) %>%
  mutate(N = rep(500, nrow(.)))

## Combine datasets & process data

summary <- rbind(summary_p50, summary_p500)

# rename & reorder factor levels corr
levels(summary$corr) <- c("GSTO + TDE", "GSTO", "None", "TDE")
summary$corr <- factor(summary$corr, levels = c("None", "GSTO", "TDE", "GSTO + TDE"))

# rename & reorder factor levels tseq
levels(summary$tseq) <- c("+ 0 C", "+ 1.44 C", "+ 1.76 C", "+ 2.96 C")

# set color scale for different tradeoffs
colors <- c("None" = "#009E73", "GSTO" = "#56B4E9", "TDE" = "#CC79A7", "GSTO + TDE" = "#D55E00")

# plot itself
summary %>%
  ggplot(aes(x = gen, y = mean_n, col = corr)) +
  #geom_hline(aes(yintercept = c(50,500)), lty = 2) +
  geom_line(lwd = 1.1) +
  scale_color_manual(values = colors, name = "Genetic Correlation") +
  scale_x_continuous(breaks = seq(0, 80, by = 20)) +
  xlab("Generation") + ylab("Mean Population Size") +
  facet_grid(cols = vars(tseq), rows = vars(N), scales = "free") +
  theme_minimal() +
  theme(legend.position = c(0.11,0.13),
        legend.text = element_text(size = 8),
        legend.title = element_text(size = 10),
        strip.text = element_text(size = 12),
        panel.grid.minor = element_blank(),
        strip.text.x = element_text(size = 12, face = "bold"),
        strip.text.y = element_text(size = 12, face = "bold"),
        panel.border = element_rect(size = 0.5, fill = NA, color = "black"))


# figure 2

summary %>%
  filter(N > 50, tseq != "+ 0 C") %>%
  ggplot(aes(x = gen, y = mean_n, col = corr)) +
  geom_hline(aes(yintercept = 500), lty = 2) +
  geom_line(lwd = 1.1) +
  scale_color_manual(values = colors, name = "Genetic Correlation") +
  scale_x_continuous(breaks = seq(0, 80, by = 20)) +
  xlab("Generation") + ylab("Mean Population Size") +
  facet_grid(cols = vars(tseq)) +
  theme_minimal() +
  theme(legend.position = c(0.11,0.19),
        legend.text = element_text(size = 10),
        legend.title = element_text(size = 10),
        strip.text = element_text(size = 12),
        panel.grid.minor = element_blank(),
        strip.text.x = element_text(size = 12, face = "bold"),
        strip.text.y = element_text(size = 12, face = "bold"),
        panel.border = element_rect(size = 0.5, fill = NA, color = "black"))

## Plot Figure 1

base_gsto <- tibble(t = c(10,19,28,35), p = c(0,5,10,0), type = rep("GSTO", 4), extreme = rep("mid", 4))

gsto <- tibble(t = c(7,18,28,38,13,20,28,32), p = c(0,3.5,7,0,0,6.5,13,0), type = rep("GSTO", 8), extreme = c(rep("low",4), rep("high",4)))

gsto <- rbind(base_gsto,gsto)

base_tde <- tibble(t = c(10,19,28,35), p = c(0,5,10,0), type = rep("TDE", 4), extreme = rep("mid", 4))

tde <- tibble(t = c(10,18,25,35,10,20,31,35), p = c(0,3.5,7,0,0,6.5,13,0), type = rep("TDE", 8), extreme = c(rep("low",4), rep("high", 4)))

tde <- rbind(base_tde, tde)

base_both <- tibble(t = c(10,19,28,35), p = c(0,5,10,0), type = rep("GSTO + TDE", 4), extreme = rep("mid", 4))

both <- tibble(t = c(7,16,25,38,13,21.5,30,32), p = c(0,3.5,7,0,0,6.5,13,0), type = rep("GSTO + TDE", 8), extreme = c(rep("low",4), rep("high", 4)))

both <- rbind(base_both, both)

data <- rbind(gsto, tde, both)

data$type <- factor(data$type, levels = c("GSTO", "TDE", "GSTO + TDE"))

base <- tibble(t = c(10,19,28,35), p = c(0,5,10,0))

data %>%
  ggplot(aes(x = t, y = p, col = extreme)) +
  geom_vline(aes(xintercept = 28), lty = 2, alpha = 0.25) +
  geom_hline(aes(yintercept = 10), lty = 2, alpha = 0.25) +
  geom_point(lwd = 2.5, alpha = 0.75) +
  geom_path(lwd = 1.25, alpha = 0.75) +
  geom_point(data = base, aes(x = t, y = p), color = "black", lwd = 2.5) +
  geom_path(data = base, aes(x = t, y = p), color = "black", lwd = 1.25) +
  scale_color_manual(values = c("indianred1", "indianred1", "indianred1")) +
  facet_grid(cols = vars(type)) +
  xlab("Temperature") +
  ylab("Performance") +
  theme_minimal() +
  theme(legend.position = "none",
        axis.text = element_blank(),
        panel.grid = element_blank(),
        strip.text = element_text(size = 10, face = "bold"))


base1 <- tibble(t = c(10,19,28,35), p = c(1,6,11.5,1))

base1 %>%
  ggplot(aes(x = t, y = p)) +
  geom_point(lwd = 4) +
  geom_path(lwd = 2.5) +
  geom_segment(aes(x = 0, y = 11.5, xend = 28, yend = 11.5), lty = 2) +
  geom_segment(aes(x = 28, y = 0, xend = 28, yend = 11.5), lty = 2) +
  geom_segment(aes(x = 10, y = 0, xend = 10, yend = 1), lty = 2) +
  geom_segment(aes(x = 35, y = 0, xend = 35, yend = 1), lty = 2) +
  annotate("text", label = "Pmax", x = 8.5, y = 11, size = 5) +
  annotate("text", label = "CTmin", x = 12.5, y = 0.6, size = 5) +
  annotate("text", label = "CTmax", x = 32.5, y = 0.6, size = 5) +
  annotate("text", label = "Topt", x = 26, y = 0.6, size = 5) +
  scale_y_continuous(expand = c(0,0)) +
  coord_cartesian(ylim = c(0,12), xlim = c(8, 37)) +
  xlab("Temperature") +
  ylab("Performance") +
  theme_minimal() +
  theme(legend.position = "none",
        axis.line = element_line(size = 1),
        axis.text = element_blank(),
        panel.grid = element_blank())

## Plot other experimental figures

# summarize data by simulation replicate and gen
sum_p50 <- p50 %>%
  group_by(gen,r,pop_n,tseq_n,corr,tseq) %>%
  summarise(topt = mean(topt),
            pmax = mean(pmax),
            ctmin = mean(ctmin),
            ctmax = mean(ctmax))

# plotting traits over time
sample <- sum_p50[sample(nrow(sum_p50), 10000),]

sample %>%
  ggplot(aes(x = gen, y = ctmax - ctmin, col = corr)) +
  geom_point(size = 0.1, alpha = 0.5) +
  geom_smooth() +
  facet_wrap(~tseq)

sum_p50 %>%
  filter(gen %in% c(5,30,55,75)) %>%
  ggplot(aes(x = r, y = pmax, col = corr)) +
  geom_point(size = 0.1, alpha = 0.5) +
  geom_smooth(method = "lm") +
  facet_wrap(~gen)

sample %>%
  ggplot(aes(x = gen, y = r, col = corr)) +
  geom_point(size = 0.5, alpha = 0.5) +
  geom_smooth(method = "lm") +
  facet_wrap(~ tseq)






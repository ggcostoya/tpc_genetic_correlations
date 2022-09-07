
## Statistical Analysis ##

## Packages ----

library(tidyverse)
library(parallel)
library(foreach)
library(doParallel)
library(gridExtra)
library(MASS)

## Obtain analysis data

a_data <- complete_sim_results %>%
  group_by(pop_n, tseq_n, corr, tseq, n0_k) %>%
  filter(gen == max(gen)) %>%
  ungroup() %>%
  filter(tseq %in% c("control", "low", "mid", "high")) %>%
  mutate(rel_n = 1 - n / n0_k) %>%
  mutate(surv = floor(rel_n))

## Plot data

a_data %>%
  ggplot(aes(x = corr, y = rel_n, col = as.factor(n0_k))) +
  geom_boxplot() +
  geom_jitter(width = 0.1, alpha = 0.25) +
  facet_grid(cols = vars(tseq))

## Run statistics

model_lm <- glm(rel_n ~ corr * tseq * as.factor(n0_k), data = a_data)
model_glm <- glm(rel_n ~ corr * tseq * as.factor(n0_k), data = a_data, family = binomial("logit"))

summary(model_lm)
summary(model_glm)
summary(model_glm2)


shapiro.test((mean(a_data$rel_n) - a_data$rel_n/ sd(a_data$rel_n)))

shapiro.test(a_data$n)

model_glm2 <- glm(surv ~ corr + tseq + as.factor(n0_k), data = a_data, family = binomial("logit"))



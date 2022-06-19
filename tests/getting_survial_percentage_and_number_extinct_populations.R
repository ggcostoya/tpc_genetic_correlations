
## Estimating Extinction Probabilities ##

library(tidyverse)

table <- summary %>%
  group_by(k, corr, tseq) %>%
  filter(gen == max(gen)) %>%
  ungroup()

table$percentage <- ifelse(table$k == "K = 50", 1 - table$n/50,
                           ifelse(table$k == "K = 500", 1 - table$n/500, 1 - table$n/5000))

## Determining how many populations became extinct

p50 %>%
  group_by(corr, tseq, k, pop_n, tseq_n) %>%
  filter(gen == max(gen)) %>%
  ungroup() %>%
  select(gen, n, corr, tseq) %>%
  group_by(corr, tseq) %>%
  summarise(as.data.frame(table(gen, n))) %>%
  ungroup() %>%
  filter(n == 0)


p50 %>%
  group_by(corr, tseq, k, pop_n, tseq_n) %>%
  #filter(gen == max(gen)) %>%
  ungroup() %>%
  select(gen, n, corr, tseq) %>%
  group_by(corr, tseq) %>%
  summarise(as.data.frame(table(gen, n))) %>%
  ungroup() %>%
  filter(n == 0) %>%
  ggplot(aes(x = as.numeric(gen), y = as.numeric(Freq), col = corr)) +
  geom_path() +
  facet_grid(cols = vars())


## Exploration of simulation results

## Packages ----

library(tidyverse)
library(parallel)
library(foreach)
library(doParallel)
library(gridExtra)
library(ggridges)
library(see)

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
p500$corr <- as.factor(p5000$corr)
levels(p500$corr) <- c("GSTO + TDE", "GSTO", "None", "TDE")
p500$corr <- factor(p500$corr, levels = c("None", "GSTO","TDE","GSTO + TDE"))
p500$tseq <- as.factor(p500$tseq)
levels(p500$tseq) <- c("+0°C","+2.96°C","+1.44°C","+1.76°C")
p500$tseq <- factor(p500$tseq, levels = c("+0°C","+1.44°C","+1.76°C","+2.96°C"))

# add column for carrying capacity
p500$k <- rep("500", nrow(p500))

# save p50 file
save(p500, file = "p500.RData")

# count rows per unique gen, corr and tseq combination
summary_p500 <- p500 %>%
  group_by(gen, corr, tseq) %>%
  summarise(as.data.frame(table(gen, corr, tseq))) %>%
  ungroup() %>%
  mutate(mean_n = Freq / 100) %>%
  mutate(gen = as.numeric(gen)) %>%
  mutate(k = rep(500, nrow(.)))

# reshape output
summary_p500 <- summary_p500 %>%
  mutate(n = mean_n) %>%
  select(gen, n, corr, tseq, k)





p50 %>%
  group_by(corr, tseq, gen) %>%
  summarise(n = mean(n)) %>%
  ungroup() %>%
  ggplot(aes(x = gen, y = n, col = corr)) +
  geom_line() +
  facet_grid(cols = vars(tseq))




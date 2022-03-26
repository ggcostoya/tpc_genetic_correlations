
## Plotting Population Size Dynamics

## Packages ----

library(tidyverse)

## Load data ---

simdata <- rbind(p50, p500)

## Simulation Dynamics ----

# Process data

## Get last alive per generation
last_alive_data <- simdata %>%
  group_by(popsize, tseq, corr, tseq_n, pop_n) %>% # group data
  filter(gen == max(gen)) %>% # filter for max generation
  summarise(last_alive = mean(gen)) %>% # get last generation alive
  ungroup()

# Get summary of percentage alive data
per_alive_data <- last_alive_data %>%
  group_by(popsize, tseq, corr) %>% # group data
  select(popsize, tseq, corr, last_alive) %>% # select specific columns
  summarise(table = as_tibble(table(last_alive))) %>% # table last generation alive
  mutate(gen = as.numeric(table$last_alive),
         per_alive = 100 - cumsum(table$n)) %>% # calculate percentage alive per gen
  select(popsize, tseq, corr, gen, per_alive) %>%
  ungroup()

## Generate grid to complete data
popsize <- c(50, 500)
corr <- c("both", "gsto", "tde", "none")
tseq <- c("control", "rcp4.5", "rcp6", "rcp8.5")
gen <- c(0:85)
grid <- expand.grid(popsize, tseq, corr, gen) %>%
  tibble(popsize = Var1, tseq = Var2, corr = Var3, gen = Var4) %>%
  select(popsize, tseq, corr, gen)

## Merge percentage alive data with grid
per_alive_data <- merge(x = per_alive_data, y = grid,
                        by = c("popsize", "tseq", "corr", "gen"),
                        all.x = TRUE, all.y = TRUE)

## Loop to fill missing observations
for(i in 1:nrow(per_alive_data)){

  per_alive_data$per_alive[i] <- ifelse(per_alive_data$gen[i] == 0, 100, per_alive_data$per_alive[i])

  per_alive_data$per_alive[i] <- ifelse(is.na(per_alive_data$per_alive[i]), per_alive_data$per_alive[i-1], per_alive_data$per_alive[i])

  per_alive_data$per_alive[i] <- ifelse(per_alive_data$gen[i] == 85, per_alive_data$per_alive[i-1], per_alive_data$per_alive[i])

}

## Get population size data
popsize_data <- simdata %>%
  group_by(popsize, tseq, corr, tseq_n, pop_n) %>%
  summarise(as.data.frame(table(gen))) %>%
  rename(n = Freq) %>%
  ungroup()

## Get standardized population size
popsize_data$n_s <- ifelse(popsize_data$popsize == 50,
                           popsize_data$n / 50,
                           popsize_data$n / 500)

## Get summary of population size data
popsize_data_sum <- popsize_data %>%
  group_by(popsize, tseq, corr, gen) %>%
  summarise(mean_ns = mean(n_s)) %>%
  ungroup()

## Add percentage alive column
popsize_data_sum <- merge(x = popsize_data_sum, y = per_alive_data,
                        by = c("popsize", "tseq", "corr", "gen"),
                        all.x = TRUE, all.y = TRUE)

## Get the actual population size considering percentage alive
popsize_data_sum <- popsize_data_sum %>% mutate(ns = mean_ns * (per_alive/100))

## Change NAs for zero
popsize_data_sum$ns <- ifelse(is.na(popsize_data_sum$ns), 0, popsize_data_sum$ns)

# Plotting

# mean population size over time
popsize_data_sum %>%
  filter(as.numeric(gen) > 4) %>%
  mutate(gen = as.numeric(gen) - 5) %>%
  ggplot(aes(x = as.numeric(gen), y = ns, col = corr)) +
  geom_line(lwd = 1.25) +
  geom_hline(aes(yintercept = 1), lwd = 0.5, lty = "dashed") +
  xlab("Generation") +
  ylab("N/K") +
  facet_grid(cols = vars(tseq), rows = vars(popsize)) +
  theme_minimal() +
  theme(legend.position = "top",
        panel.border = element_rect(size = 0.5, fill = NA, color = "black"))

# mean population size over time -- only rcp8.5
popsize_data_sum %>%
  filter(as.numeric(gen) > 4) %>%
  mutate(gen = as.numeric(gen) - 5) %>%
  filter(tseq == "rcp8.5") %>%
  ggplot(aes(x = as.numeric(gen), y = ns, col = corr)) +
  geom_line(lwd = 1.25) +
  geom_hline(aes(yintercept = 1), lwd = 0.5, lty = "dashed") +
  xlab("Generation") +
  ylab("N/K") +
  facet_grid(cols = vars(popsize)) +
  theme_minimal() +
  theme(legend.position = "top",
        panel.border = element_rect(size = 0.5, fill = NA, color = "black"))



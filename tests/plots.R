
## Plotting

## Packages ----

library(tidyverse)

## Load data ---

simdata <- rbind(p50, p500)

#simdata <- simdata %>% filter(mid < topt)

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
  ggplot(aes(x = as.numeric(gen), y = ns, col = corr)) +
  geom_line(lwd = 1.25) +
  geom_hline(aes(yintercept = 1), lwd = 0.5, lty = "dashed") +
  facet_grid(cols = vars(tseq), rows = vars(popsize)) +
  theme_minimal() +
  theme(legend.position = "top",
        panel.border = element_rect(size = 0.5, fill = NA, color = "black"))


## Individual dynamics -----

g50 <- rcp8.5[[1]][[50]]

g80 <- rcp8.5[[1]][[80]]

ggdata <- tibble(gen = c(rep(50,length(g50)), rep(80,length(g80))),
                 temp = c(g50,g80))

ggplot(ggdata, aes(x = temp, fill = as.factor(gen))) +
  geom_histogram(alpha = 0.5, position = "identity")


ggplot(ggdata, aes(x = temp, fill = as.factor(gen))) +
  geom_density(alpha = 0.5) +
  xlim(25,38)

## Getting mean curve per generation

tdata <- tibble(t = g50)

simdata %>%
  filter(tseq == "rcp8.5") %>%
  filter(popsize == 50) %>%
  filter(gen == 50) %>%
  ggplot(aes(x = topt, fill = as.factor(corr))) +
  geom_density(position = "identity", binwidth = 0.1, alpha = 0.5) +
  geom_vline(aes(xintercept = c(mean(g50))))


simdata %>%
  filter(tseq == "rcp8.5") %>%
  filter(popsize == 50) %>%
  filter(gen %in% c(5,25,45,65,80)) %>%
  ggplot(aes(x = r, fill = as.factor(corr))) +
  geom_histogram(position = "identity", binwidth = 1, alpha = 0.5) +
  facet_grid(cols = vars(gen), rows = vars(corr))


## Predict performance ----

predict_performance <- function(sim, tseq){

  # get temperature indeces
  indexes <- round(tseq/0.1)

  # add predicted performance column
  sim_pred <- sim %>% mutate(pred_p = rep(NA, nrow(.)))

  # start loop to pedict performance
  for(i in 1:nrow(sim_pred)){

    # extract TPC
    tpc <- sim_pred$tpc[[i]]

    # determine performance at environmental temperature
    sim_pred$pred_p[i] <- list(tpc$p[indexes])

  }

return(sim_pred)

}


test_simdata <- simdata %>%
  filter(tseq == "rcp8.5",
         gen == 80,
         popsize == 50) %>%
  select(corr, topt, pmax, ctmin, ctmax, mid) %>%
  group_by(corr, topt, pmax, ctmin, ctmax, mid) %>%
  summarise(count = table(.)) %>%
  ungroup() %>%
  unique()


build_tpd <- function(tpts){

  # temperature
  t <- round(tpts[1:4], 1)

  # performance
  pmax <- tpts[5]
  p <- c(0, pmax/2, pmax, 0)

  return(data.frame(t,p))

}

generate_tpc <- function(tpd){

  # temperature
  t <- round(seq(0, 50, by = 0.1), digits = 1)

  # holder performance
  p <- rep(NA, length(t))

  # set zero p for below and above ctmin and ctmax
  p[1:which(t == tpd$t[1])] <- p[which(t == tpd$t[4]):length(t)] <- 0

  # get indexes for critical points
  ctmin <- which(t == tpd$t[1])
  mid <- which(t == tpd$t[2])
  topt <- which(t == tpd$t[3])
  ctmax <- which(t == tpd$t[4])

  # set performance values for each vector
  p[ctmin:mid] <- seq(0, tpd$p[2], length.out = length(p[ctmin:mid]))
  p[mid:topt] <- seq(tpd$p[2], tpd$p[3], length.out = length(p[mid:topt]))
  p[topt:ctmax] <- seq(tpd$p[3], 0, length.out = length(p[topt:ctmax]))

  return(tibble(t = t, p = p))

}

test_simdata$tpc <- rep(NA, nrow(test_simdata))

# generate TPCs
for(i in 1:nrow(test_simdata)){

  # get tpts
  tpts <- c(test_simdata$ctmin[i], test_simdata$mid[i], test_simdata$topt[i], test_simdata$ctmax[i], test_simdata$pmax[i])

  # get tpd
  tpd <- build_tpd(tpts = tpts)

  # get tpc
  tpc <- generate_tpc(tpd = tpd) %>% nest(tpc = c(t,p))

  # get tpc
  test_simdata$tpc[i] <- tpc

}

test_simdata <- test_simdata %>% unnest(cols = c(tpc))

test_tseq <- rcp8.5[[1]][[80]]

maybe <- predict_performance(sim = test_simdata, tseq = test_tseq)


maybe %>%
  select(corr, pred_p) %>%
  unnest(cols = c(pred_p)) %>%
  ggplot(aes(x = pred_p, fill = as.factor(corr))) +
  geom_density(position = "identity", bindiwdth = 0.1, alpha = 0.5)

maybe %>%
  select(corr, pred_p) %>%
  unnest(cols = c(pred_p)) %>%
  group_by(corr) %>%
  summarise(p = sum(pred_p))















































## Simulation Data Post Processing

## Packages ----

library(tidyverse)

## Loading data ----

## Population size 50

# Both control
bc_50 <- simulation_p50_both_control
bc_50 <- do.call(rbind, do.call(c, bc_50))
bc_50 <- mutate(bc_50, corr = rep("both", nrow(bc_50)),
                tseq = rep("control", nrow(bc_50)),
                popsize = rep(50,nrow(bc_50)))

# Both rcp4.5
b45_50 <- simulation_p50_both_rcp4.5
b45_50 <- do.call(rbind, do.call(c, b45_50))
b45_50 <- mutate(b45_50, corr = rep("both", nrow(b45_50)),
                 tseq = rep("rcp4.5", nrow(b45_50)),
                 popsize = rep(50,nrow(b45_50)))

# Both rcp6
b6_50 <- simulation_p50_both_rcp6
b6_50 <- do.call(rbind, do.call(c, b6_50))
b6_50 <- mutate(b6_50, corr = rep("both", nrow(b6_50)),
                tseq = rep("rcp6", nrow(b6_50)),
                popsize = rep(50,nrow(b6_50)))

# Both rcp8.5
b85_50 <- simulation_p50_both_rcp8.5
b85_50 <- do.call(rbind, do.call(c, b85_50))
b85_50 <- mutate(b85_50, corr = rep("both", nrow(b85_50)),
                 tseq = rep("rcp8.5", nrow(b85_50)),
                 popsize = rep(50,nrow(b85_50)))

# GSTO control
gc_50 <- simulation_p50_gsto_control
gc_50 <- do.call(rbind, do.call(c, gc_50))
gc_50 <- mutate(gc_50, corr = rep("gsto", nrow(gc_50)),
                tseq = rep("control", nrow(gc_50)),
                popsize = rep(50,nrow(gc_50)))

# GSTO rcp4.5
g45_50 <- simulation_p50_gsto_rcp4.5
g45_50 <- do.call(rbind, do.call(c, g45_50))
g45_50 <- mutate(g45_50, corr = rep("gsto", nrow(g45_50)),
                 tseq = rep("rcp4.5", nrow(g45_50)),
                 popsize = rep(50,nrow(g45_50)))

# GSTO rcp6
g6_50 <- simulation_p50_gsto_rcp6
g6_50 <- do.call(rbind, do.call(c, g6_50))
g6_50 <- mutate(g6_50, corr = rep("gsto", nrow(g6_50)),
                tseq = rep("rcp6", nrow(g6_50)),
                popsize = rep(50,nrow(g6_50)))

# GSTO rcp8.5
g85_50 <- simulation_p50_gsto_rcp8.5
g85_50 <- do.call(rbind, do.call(c, g85_50))
g85_50 <- mutate(g85_50, corr = rep("gsto", nrow(g85_50)),
                 tseq = rep("rcp8.5", nrow(g85_50)),
                 popsize = rep(50,nrow(g85_50)))

# TDE control
tc_50 <- simulation_p50_tde_control
tc_50 <- do.call(rbind, do.call(c, tc_50))
tc_50 <- mutate(tc_50, corr = rep("tde", nrow(tc_50)),
                tseq = rep("control", nrow(tc_50)),
                popsize = rep(50,nrow(tc_50)))

# TDE rcp4.5
t45_50 <- simulation_p50_tde_rcp4.5
t45_50 <- do.call(rbind, do.call(c, t45_50))
t45_50 <- mutate(t45_50, corr = rep("tde", nrow(t45_50)),
                 tseq = rep("rcp4.5", nrow(t45_50)),
                 popsize = rep(50,nrow(t45_50)))

# TDE rcp6
t6_50 <- simulation_p50_tde_rcp6
t6_50 <- do.call(rbind, do.call(c, t6_50))
t6_50 <- mutate(t6_50, corr = rep("tde", nrow(t6_50)),
                tseq = rep("rcp6", nrow(t6_50)),
                popsize = rep(50,nrow(t6_50)))

# TDE rcp8.5
t85_50 <- simulation_p50_tde_rcp8.5
t85_50 <- do.call(rbind, do.call(c, t85_50))
t85_50 <- mutate(t85_50, corr = rep("tde", nrow(t85_50)),
                 tseq = rep("rcp8.5", nrow(t85_50)),
                 popsize = rep(50,nrow(t85_50)))

# None control
nc_50 <- simulation_p50_none_control
nc_50 <- do.call(rbind, do.call(c, nc_50))
nc_50 <- mutate(nc_50, corr = rep("none", nrow(nc_50)),
                tseq = rep("control", nrow(nc_50)),
                popsize = rep(50,nrow(nc_50)))

# None rcp4.5
n45_50 <- simulation_p50_none_rcp4.5
n45_50 <- do.call(rbind, do.call(c, n45_50))
n45_50 <- mutate(n45_50, corr = rep("none", nrow(n45_50)),
                 tseq = rep("rcp4.5", nrow(n45_50)),
                 popsize = rep(50,nrow(n45_50)))

# None rcp6
n6_50 <- simulation_p50_none_rcp6
n6_50 <- do.call(rbind, do.call(c, n6_50))
n6_50 <- mutate(n6_50, corr = rep("none", nrow(n6_50)),
                tseq = rep("rcp6", nrow(n6_50)),
                popsize = rep(50,nrow(n6_50)))

# None rcp8.5
n85_50 <- simulation_p50_none_rcp8.5
n85_50 <- do.call(rbind, do.call(c, n85_50))
n85_50 <- mutate(n85_50, corr = rep("none", nrow(n85_50)),
                 tseq = rep("rcp8.5", nrow(n85_50)),
                 popsize = rep(50,nrow(n85_50)))

## Population size 500 --

# Both control
bc_500 <- simulation_p500_both_control
bc_500 <- do.call(rbind, do.call(c, bc_500))
bc_500 <- mutate(bc_500, corr = rep("both", nrow(bc_500)),
                 tseq = rep("control", nrow(bc_500)),
                 popsize = rep(500,nrow(bc_500)))

# Both rcp4.5
b45_500 <- simulation_p500_both_rcp4.5
b45_500 <- do.call(rbind, do.call(c, b45_500))
b45_500 <- mutate(b45_500, corr = rep("both", nrow(b45_500)),
                  tseq = rep("rcp4.5", nrow(b45_500)),
                  popsize = rep(500,nrow(b45_500)))

# Both rcp6
b6_500 <- simulation_p500_both_rcp6
b6_500 <- do.call(rbind, do.call(c, b6_500))
b6_500 <- mutate(b6_500, corr = rep("both", nrow(b6_500)),
                 tseq = rep("rcp6", nrow(b6_500)),
                 popsize = rep(500,nrow(b6_500)))

# Both rcp8.5
b85_500 <- simulation_p500_both_rcp8.5
b85_500 <- do.call(rbind, do.call(c, b85_500))
b85_500 <- mutate(b85_500, corr = rep("both", nrow(b85_500)),
                  tseq = rep("rcp8.5", nrow(b85_500)),
                  popsize = rep(500,nrow(b85_500)))

# GSTO control
gc_500 <- simulation_p500_gsto_control
gc_500 <- do.call(rbind, do.call(c, gc_500))
gc_500 <- mutate(gc_500, corr = rep("gsto", nrow(gc_500)),
                 tseq = rep("control", nrow(gc_500)),
                 popsize = rep(500,nrow(gc_500)))

# GSTO rcp4.5
g45_500 <- simulation_p500_gsto_rcp4.5
g45_500 <- do.call(rbind, do.call(c, g45_500))
g45_500 <- mutate(g45_500, corr = rep("gsto", nrow(g45_500)),
                  tseq = rep("rcp4.5", nrow(g45_500)),
                  popsize = rep(500,nrow(g45_500)))

# GSTO rcp6
g6_500 <- simulation_p500_gsto_rcp6
g6_500 <- do.call(rbind, do.call(c, g6_500))
g6_500 <- mutate(g6_500, corr = rep("gsto", nrow(g6_500)),
                 tseq = rep("rcp6", nrow(g6_500)),
                 popsize = rep(500,nrow(g6_500)))

# GSTO rcp8.5
g85_500 <- simulation_p500_gsto_rcp8.5
g85_500 <- do.call(rbind, do.call(c, g85_500))
g85_500 <- mutate(g85_500, corr = rep("gsto", nrow(g85_500)),
                  tseq = rep("rcp8.5", nrow(g85_500)),
                  popsize = rep(500,nrow(g85_500)))

# TDE control
tc_500 <- simulation_p500_tde_control
tc_500 <- do.call(rbind, do.call(c, tc_500))
tc_500 <- mutate(tc_500, corr = rep("tde", nrow(tc_500)),
                 tseq = rep("control", nrow(tc_500)),
                 popsize = rep(500,nrow(tc_500)))

# TDE rcp4.5
t45_500 <- simulation_p500_tde_rcp4.5
t45_500 <- do.call(rbind, do.call(c, t45_500))
t45_500 <- mutate(t45_500, corr = rep("tde", nrow(t45_500)),
                  tseq = rep("rcp4.5", nrow(t45_500)),
                  popsize = rep(500,nrow(t45_500)))

# TDE rcp6
t6_500 <- simulation_p500_tde_rcp6
t6_500 <- do.call(rbind, do.call(c, t6_500))
t6_500 <- mutate(t6_500, corr = rep("tde", nrow(t6_500)),
                 tseq = rep("rcp6", nrow(t6_500)),
                 popsize = rep(500,nrow(t6_500)))

# TDE rcp8.5
t85_500 <- simulation_p500_tde_rcp8.5
t85_500 <- do.call(rbind, do.call(c, t85_500))
t85_500 <- mutate(t85_500, corr = rep("tde", nrow(t85_500)),
                  tseq = rep("rcp8.5", nrow(t85_500)),
                  popsize = rep(500,nrow(t85_500)))

# None control
nc_500 <- simulation_p500_none_control
nc_500 <- do.call(rbind, do.call(c, nc_500))
nc_500 <- mutate(nc_500, corr = rep("none", nrow(nc_500)),
                 tseq = rep("control", nrow(nc_500)),
                 popsize = rep(500,nrow(nc_500)))

# None rcp4.5
n45_500 <- simulation_p500_none_rcp4.5
n45_500 <- do.call(rbind, do.call(c, n45_500))
n45_500 <- mutate(n45_500, corr = rep("none", nrow(n45_500)),
                  tseq = rep("rcp4.5", nrow(n45_500)),
                  popsize = rep(500,nrow(n45_500)))

# None rcp6
n6_500 <- simulation_p500_none_rcp6
n6_500 <- do.call(rbind, do.call(c, n6_500))
n6_500 <- mutate(n6_500, corr = rep("none", nrow(n6_500)),
                 tseq = rep("rcp6", nrow(n6_500)),
                 popsize = rep(500,nrow(n6_500)))

# None rcp8.5
n85_500 <- simulation_p500_none_rcp8.5
n85_500 <- do.call(rbind, do.call(c, n85_500))
n85_500 <- mutate(n85_500, corr = rep("none", nrow(n85_500)),
                  tseq = rep("rcp8.5", nrow(n85_500)),
                  popsize = rep(500,nrow(n85_500)))


## Population size 5000 --

# Both control
bc_5000 <- simulation_p5000_both_control
bc_5000 <- do.call(rbind, do.call(c, bc_5000))
bc_5000 <- mutate(bc_5000, corr = rep("both", nrow(bc_5000)),
                 tseq = rep("control", nrow(bc_5000)),
                 popsize = rep(5000,nrow(bc_5000)))

# Both rcp4.5
b45_5000 <- simulation_p5000_both_rcp4.5
b45_5000 <- do.call(rbind, do.call(c, b45_5000))
b45_5000 <- mutate(b45_5000, corr = rep("both", nrow(b45_5000)),
                  tseq = rep("rcp4.5", nrow(b45_5000)),
                  popsize = rep(5000,nrow(b45_5000)))
rm(simulation_p5000_both_rcp4.5)

# Both rcp6
b6_5000 <- simulation_p5000_both_rcp6
b6_5000 <- do.call(rbind, do.call(c, b6_5000))
b6_5000 <- mutate(b6_5000, corr = rep("both", nrow(b6_5000)),
                 tseq = rep("rcp6", nrow(b6_5000)),
                 popsize = rep(5000,nrow(b6_5000)))
rm(simulation_p5000_both_rcp6)

# Both rcp8.5
b85_5000 <- simulation_p5000_both_rcp8.5
b85_5000 <- do.call(rbind, do.call(c, b85_5000))
b85_5000 <- mutate(b85_5000, corr = rep("both", nrow(b85_5000)),
                  tseq = rep("rcp8.5", nrow(b85_5000)),
                  popsize = rep(5000,nrow(b85_5000)))
rm(simulation_p5000_both_rcp8.5)

# GSTO control
gc_5000 <- simulation_p5000_gsto_control
gc_5000 <- do.call(rbind, do.call(c, gc_5000))
gc_5000 <- mutate(gc_5000, corr = rep("gsto", nrow(gc_5000)),
                 tseq = rep("control", nrow(gc_5000)),
                 popsize = rep(5000,nrow(gc_5000)))
rm(simulation_p5000_gsto_control)

# GSTO rcp4.5
g45_5000 <- simulation_p5000_gsto_rcp4.5
g45_5000 <- do.call(rbind, do.call(c, g45_5000))
g45_5000 <- mutate(g45_5000, corr = rep("gsto", nrow(g45_5000)),
                  tseq = rep("rcp4.5", nrow(g45_5000)),
                  popsize = rep(5000,nrow(g45_5000)))
rm(simulation_p5000_gsto_rcp4.5)

# GSTO rcp6
g6_5000 <- simulation_p5000_gsto_rcp6
g6_5000 <- do.call(rbind, do.call(c, g6_5000))
g6_5000 <- mutate(g6_5000, corr = rep("gsto", nrow(g6_5000)),
                 tseq = rep("rcp6", nrow(g6_5000)),
                 popsize = rep(5000,nrow(g6_5000)))
rm(simulation_p5000_gsto_rcp6)

# GSTO rcp8.5
g85_5000 <- simulation_p5000_gsto_rcp8.5
g85_5000 <- do.call(rbind, do.call(c, g85_5000))
g85_5000 <- mutate(g85_5000, corr = rep("gsto", nrow(g85_5000)),
                  tseq = rep("rcp8.5", nrow(g85_5000)),
                  popsize = rep(5000,nrow(g85_5000)))
rm(simulation_p5000_gsto_rcp8.5)

# TDE control
tc_5000 <- simulation_p5000_tde_control
tc_5000 <- do.call(rbind, do.call(c, tc_5000))
tc_5000 <- mutate(tc_5000, corr = rep("tde", nrow(tc_5000)),
                 tseq = rep("control", nrow(tc_5000)),
                 popsize = rep(5000,nrow(tc_5000)))

# TDE rcp4.5
t45_5000 <- simulation_p5000_tde_rcp4.5
t45_5000 <- do.call(rbind, do.call(c, t45_5000))
t45_5000 <- mutate(t45_5000, corr = rep("tde", nrow(t45_5000)),
                  tseq = rep("rcp4.5", nrow(t45_5000)),
                  popsize = rep(5000,nrow(t45_5000)))

# TDE rcp6
t6_5000 <- simulation_p5000_tde_rcp6
t6_5000 <- do.call(rbind, do.call(c, t6_5000))
t6_5000 <- mutate(t6_5000, corr = rep("tde", nrow(t6_5000)),
                 tseq = rep("rcp6", nrow(t6_5000)),
                 popsize = rep(5000,nrow(t6_5000)))

# TDE rcp8.5
t85_5000 <- simulation_p500_tde_rcp8.5
t85_5000 <- do.call(rbind, do.call(c, t85_5000))
t85_5000 <- mutate(t85_5000, corr = rep("tde", nrow(t85_5000)),
                  tseq = rep("rcp8.5", nrow(t85_5000)),
                  popsize = rep(5000,nrow(t85_5000)))

# None control
nc_5000 <- simulation_p500_none_control
nc_5000 <- do.call(rbind, do.call(c, nc_5000))
nc_5000 <- mutate(nc_5000, corr = rep("none", nrow(nc_5000)),
                 tseq = rep("control", nrow(nc_5000)),
                 popsize = rep(5000,nrow(nc_5000)))

# None rcp4.5
n45_5000 <- simulation_p5000_none_rcp4.5
n45_5000 <- do.call(rbind, do.call(c, n45_5000))
n45_5000 <- mutate(n45_5000, corr = rep("none", nrow(n45_5000)),
                  tseq = rep("rcp4.5", nrow(n45_5000)),
                  popsize = rep(5000,nrow(n45_5000)))

# None rcp6
n6_5000 <- simulation_p5000_none_rcp6
n6_5000 <- do.call(rbind, do.call(c, n6_5000))
n6_5000 <- mutate(n6_5000, corr = rep("none", nrow(n6_5000)),
                 tseq = rep("rcp6", nrow(n6_5000)),
                 popsize = rep(5000,nrow(n6_5000)))

# None rcp8.5
n85_5000 <- simulation_p5000_none_rcp8.5
n85_5000 <- do.call(rbind, do.call(c, n85_5000))
n85_5000 <- mutate(n85_5000, corr = rep("none", nrow(n85_5000)),
                  tseq = rep("rcp8.5", nrow(n85_5000)),
                  popsize = rep(5000,nrow(n85_5000)))

## Combining data ----

## Population size 50

p50 <- rbind(bc_50, b45_50, b6_50, b85_50,
             gc_50, g45_50, g6_50, g85_50,
             tc_50, t45_50, t6_50, t85_50,
             nc_50, n45_50, n6_50, n85_50)

## Population size 500

p500 <- rbind(bc_500, b45_500, b6_500, b85_500,
              gc_500, g45_500, g6_500, g85_500,
              tc_500, t45_500, t6_500, t85_500,
              nc_500, n45_500, n6_500, n85_500)

## Population size 5000

p5000 <- rbind(bc_5000, b45_5000, b6_5000, b85_5000,
               gc_5000, g45_5000, g6_5000, g85_5000,
               tc_5000, t45_5000, t6_5000, t85_5000,
               nc_5000, n45_5000, n6_5000, n85_5000)

## Save data ----

setwd("C:/Users/ggarc/projects/tpcgencorr_data/simulations")

save(p50, file = "p50.RData")
save(p500, file = "p500.RData")
save(p5000, file = "p5000.RData")

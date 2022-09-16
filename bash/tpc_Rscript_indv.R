#!/usr/bin/env Rscript

## how to run 
# Rscript tpc_Rscript.R pop_size gen rcp cpus

## keep track of time
start.time <- Sys.time()

## accept argument from the command line. 
args <- commandArgs(trailingOnly=TRUE)

## argument needs to be the population size
if (length(args) != 4) {
  stop("Arguments for population size, genetic tradeoff, climate scenerio, 
       number of cpurs and must be provided", call.=FALSE)
}

## set all necessary file/variable names for this run using command args
pop_size <- args[1]
gen <- args[2]
rcp <- args[3]
cpus <- as.numeric(args[4])

## check right inputs
if (!(gen %in% c('none','gsto','tde','both'))){
  stop("Second argument must be genetic tradeoff: none, gsto, tde, or both", call.=FALSE)
}

if (!(rcp %in% c('control','rcp4.5','rcp6','rcp6_m','rcp6_sd','rcp8.5'))){
  stop("Third argument must be climate change scenerio: control, rcp4.5, rcp6, rcp6_m, rcp6_sd, or rcp8.5", call.=FALSE)
}

### detect cpus and set 
if (cpus > parallel::detectCores()){
  stop("Number of cpus (fourth argument) greater than cpus available", call.=FALSE)
}else{
  doParallel::registerDoParallel(cpus) # Detect computer cores and register them for use
}

outfile <- paste0('simulation_p',pop_size,'_',gen,'_',rcp)

### output for user
writeLines(paste0('Running simulations for...\n',
    'population size = ',pop_size,'\n',
    'genetic tradeoff = ',gen,'\n',
    'climate scenerio = ',rcp,'\n\n'))

## source all simualtion functions
source('tpc_import_functions.R')

## load tseq data

tseqs <- get(load(paste0('data/tseqs/',rcp,'.RData')))

## load populations

pops <- get(load(paste0('data/pops/',gen,'_',pop_size,'.RData')))

##############################################################################

## Run simulations ------------------------------------------------------------

writeLines(paste0('Running simulations......\n\n'))
# Using just the first replicate population and tseq

sim1 <- simulation(pop = pops[[1]], tseq = tseqs[[1]])

# Using all replicates

simulation_out <- foreach(i = 1:length(pops), .packages = c("tidyverse", 'parallel', "doParallel", "foreach", "data.table", "DescTools")) %dopar% {
  
  foreach(j = 1:length(tseqs)) %do% {
    
    sim <- simulation(pop = pops[[i]], tseq = tseqs[[j]])
    
    sim %>% mutate(pop_n = rep(i, nrow(.)), tseq_n = rep(j, nrow(.)))
    
  }
}

# As long as the file name specifies the populations and the tseqs used,
# returning a simulation results object in the format as the one above this works
# just fine.

#rename variable and delete other
assign(outfile,simulation_out)
rm(simulation_out)

#save sim as Rdata file
save(list=outfile, file = paste0('data/output/',outfile,'.Rdata'))
writeLines(paste0('Simulation output saved as:\n......',outfile,'\n\n'))
writeLines(paste0('This is before running add_simulation_numbers! \n\n'))

# run.time
run.time <- round(Sys.time() - start.time,3)
print(run.time)


#print(cat('Summarizing simulations......\n\n'))
#simulation_out <- add_simulation_numbers(do.call(c, simulation_out))

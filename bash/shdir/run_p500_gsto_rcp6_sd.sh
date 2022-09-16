#!/usr/bin/env bash

#SBATCH --account=cpu-s1-bionres-0
#SBATCH --partition=cpu-s1-bionres-0
#SBATCH --time=8-00:00:00
#SBATCH --ntasks 1
#SBATCH --cpus-per-task 64
#SBATCH --mem-per-cpu=2500
#SBATCH --job-name p500_gsto_rcp6_sd
#SBATCH --output out_p500_gsto_rcp6_sd
#SBATCH --mail-type=FAIL
#SBATCH --mail-user=tfaske@nevada.unr.edu

# change directory
cd /data/gpfs/assoc/denovo/tfaske/guille/tpc/pronghorn_final

# run Rscript
Rscript tpc_Rscript_indv.R 500 gsto rcp6_sd 64


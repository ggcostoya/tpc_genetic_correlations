#### tpc_slurmout.R
#### Trevor Faske
#### 03/07/2022

#### Simulation input

pops <- c(50,500,5000)
gens <- c('none','gsto','tde','both')
temps <- c('control','rcp4.5','rcp6','rcp6_m','rcp6_sd','rcp8.5')

#### Slurm input 
account = 'cpu-s1-bionres-0'
partition = 'cpu-s1-bionres-0'
time = '8-00:00:00' #time limit 8 days
ntasks = 1
cpus = 64
mem_cpu = 2500
email = 'tfaske@nevada.unr.edu'

path = '/data/gpfs/assoc/denovo/tfaske/guille/tpc/pronghorn_final'

#### write slurm

for (p in pops){
  for (g in gens) {
    for (t in temps) { 
      
      jobname <-  paste0('p',p,'_',g,'_',t)
      
      slurmout <- paste0('#!/usr/bin/env bash\n\n',
                      '#SBATCH --account=',account,'\n',
                      '#SBATCH --partition=',partition,'\n',
                      '#SBATCH --time=',time,'\n',
                      '#SBATCH --ntasks ',ntasks,'\n',
                      '#SBATCH --cpus-per-task ',cpus,'\n',
                      '#SBATCH --mem-per-cpu=',mem_cpu,'\n',
                      '#SBATCH --job-name ',jobname,'\n',
                      '#SBATCH --output out_',jobname,'\n',
                      '#SBATCH --mail-type=FAIL\n',
                      '#SBATCH --mail-user=tfaske@nevada.unr.edu\n\n',
                      
                      '# change directory\ncd ',path,'\n\n',
                      
                      '# run Rscript\n',
                      'Rscript tpc_Rscript_indv.R ',p,' ',g,' ',t,' ',cpus,'\n\n')
      
      #write slurmfile
      outfile <- paste0('shdir/run_',jobname,'.sh')
      cat(slurmout,file=outfile)
      
    }
  }
}

#### make run_sbatch scripts for each population combo

files_50 <- list.files(path='shdir',pattern='^run_p50_')
files_500 <- list.files(path='shdir',pattern='^run_p500_')
files_5000 <- list.files(path='shdir',pattern='^run_p5000_')


# files with p50
cat('#!/usr/bin/env bash\n\n',file='shdir/sbatch_run_p50.sh')
for (f in files_50) {
  bashout <- paste0('sbatch ',f,'\n')
  cat(bashout,file='shdir/sbatch_run_p50.sh',append=TRUE)
}

# files with p500
cat('#!/usr/bin/env bash\n\n',file='shdir/sbatch_run_p500.sh')
for (f in files_500) {
  bashout <- paste0('sbatch ',f,'\n')
  cat(bashout,file='shdir/sbatch_run_p500.sh',append=TRUE)
}

# files with p5000
cat('#!/usr/bin/env bash\n\n',file='shdir/sbatch_run_p5000.sh')
for (f in files_5000) {
  bashout <- paste0('sbatch ',f,'\n')
  cat(bashout,file='shdir/sbatch_run_p5000.sh',append=TRUE)
}


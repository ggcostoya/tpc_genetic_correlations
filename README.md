# Evolutionary constraints mediate extinction risk under climate change 

**Authors**: Guillermo Garcia-Costoya<sup>1</sup>, Claire E. Williamns<sup>1</sup>, Trevor M. Faske<sup>1</sup>, Jacob D. Moorman<sup>2</sup>, Michael L. Logan<sup>1</sup>

**Affiliations**: <sup>1</sup>University of Nevada, Reno, NV, USA. <sup>2</sup>University of California, Los Angeles, CA, USA. 

**Full manuscript available at**: https://onlinelibrary.wiley.com/doi/full/10.1111/ele.14173

### **Abstract**

Mounting evidence suggests that evolutionary adaptation may rescue some organisms from rapid climate change. However, evolutionary constraints might hinder this process, especially when different aspects of environmental change generate antagonistic selection on genetically correlated traits. Here, we use individual-based simulations to explore how genetic correlations underlying the thermal physiology of ectotherms might influence their responses to the two major components of climate change—increases in mean temperature and thermal variability—that are happening concurrently in nature. We found that genetic correlations can influence population dynamics under climate change, with declines in population size varying three-fold depending on the type of genetic correlation present. Surprisingly, populations whose thermal performance curves were constrained by genetic correlations often declined less rapidly than those populations which were unconstrained. Our results suggest that accurate forecasts of the impact of climate change on ectotherms will require an understanding of the genetic architecture of the traits under selection

### **Repository structure**

All `R` and `bash` code within this repository can be found in the corresponding folders. Within the `R` folder:

- The subfolder `simulation_scripts` contains scripts (1) to generate populations of varying initial population sizes and subject to different kinds of genetic correlations, (2) to generate temperature sequences following climate change scenarios with specified changes in mean and thermal variability, (3) describing the basic simulation function with and without parallelization, (4) to process the data output from simulation runs. 

- The subfolder `plotting` contains all code necessary to replicate all figures presented in the manuscript. 

Within the `bash` folder: 

### **Data Availability**

All data is available at: https://drive.google.com/drive/folders/1nxoNiDcqxyInwjXeWqUe5b4KYCWmzzBf?usp=sharing

**All metadata for the provided dataset is explained next**. Within the folder above there are 3 folders:

- The subfolder `thermal_environments` contains the raw temperature data for all thermal environments representing different climate change scenarios. Each climate change scenario is abbreviated as follows: 

  - `control`: Control scenario, no change in mean or standard deviation of temperature.
  - `low`: RCP 4.5, +1.44 C in Mean with changes in standard deviation of temperature.
  - `mid`: RCP 6, +1.76 C in Mean with changes in standard deviation of temperature.
  - `high`: RCP 8.5, +2.96 C in Mean with changes in standard deviation of temperature.
  - `control_2sd`: Control scenario, no change in mean or standard deviation (SD) of temperature but double the initial SD.
  - `high_2sd`: RCP 8.5, +2.96 C in Mean with changes in standard deviation of temperature but double the initial SD
  - `high_m`: RCP 8.5, but only changes in mean temperature.
  - `high_sd`: RCP 8.5, but only changes in standard deviation of temperature.

- The subfolder `starting_populations` contains the raw data for all individuals generated as part of the initial populations faced with climate change scenarios in our simulations. In all cases, each file indicates genetic correlation and initial population size (and carrying capacity) in the format `geneticcorrelation_populationsize.RData`. Initial population sizes are either `50`,`500`, or `5000`. Genetic correlations are abbreviated as:

  - `none`: For no genetic correlations. 
  - `gsto`: For the generalist-specialist trade-off (GSTO). 
  - `tde`: For the thermodynamic effect (TDE).
  - `both`: For both the GSTO and the TDE acting together. 

- The subfolder `simulations` contains all raw and processed data from simulation runs. 

  - The subfolder `raw_data` contains all simulation raw data for each of the 3 initial population sizes: `50`, `500` or `5000`. In all cases the file format is `simulation_pN0_geneticcorrelation_thermalenvironment` followed by either `sum` for simulations only containing population size at each generation or `full` for simulations containing all individual information each generation. 
  
  - The subfolder `processed_data` contains processed and summarized data obtained from the raw datasets using the `process_simulation_data.R` script. The files contained in this folder are: 
  
    - `k50` : Population size every generation for all simulations with initial population size and carrying capacity of = 50
    - `k500` : Population size every generation for all simulations with initial population size and carrying capacity of = 500
    - `k500` : Population size every generation for all simulations with initial population size and carrying capacity of 5000
    - `k500_traits` : Average trait values for every generation across simulations with initial population size and carrying capacity of 500 that were exposed to the Control, RCP 4.5, RCP 6 & RCP 8.5 climate change scenarios 
    - `complete_sim_results` : Population size every generation for all simulations.
    - `sum_sim_results` : Average Population size every generation across all simulations of the same N0 & K and exposed to the same climate change scenario.








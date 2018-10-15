October 2018
# Data and code for the manuscript "Estimates of vital rates and predictions of population dynamics change along a long-term monitoring program"


## 1. Data

The function `data_prep.r` reads tag-recapture data and produces a plot of cumulative number of tagged fish over the monitoring program.

## 2. Code

R scripts are in the folder `scripts`. `NameOfPopulation` can be `loidri`, `uppidri`, `rtidri`, `uppvol`.    

The script `Plot_density.r` produces the plot (`Plots/ Plot_dens_all.pdf`) of observed densities and their 95% CI. Density data is the files `data/density.all.idri.RDS` for all the Idrijca populations and `data/density.uppvol.RDS` for Upper Volaja.   

The scripts `NameOfPopulation_marked_06_14.r` estimate survival models for the population `NameOfPopulation` with the R package `marked` using data up to 2006, 2008, 2010, 2012, and 2014 and store them in the RDS list `data/NameOfPopulation_surv_06_14.list.RDS`. It is RAM intensive and it should take quite a few hours to run for each population.  The script `Plot_survival.r` produces the plot of avg, mean, and max survival in sampling interval along the monitoring program.

The scripts `NameOfPopulation_growth.r` estimate individual random-effect and standard nls von Bertalanffy growth models using data up to 2005, 2006, 2007, 2008, 2009, 2010, 2011, 2012, 2013, and 2014). It uses _ADBM_ for the random-effects models and _standard R_ for nls. The data frame with estimates of the parameters, sample size etc. are stored in the RDS files `data/data/NameOfPopulation_growth_all_df.RDS`.  
   
The script `Plot_growth.r` plots the changes through time of _L_inf_ estimated with the random-effects and the nls models (`Plots/Plot_linf_all.pdf`) and the predicted growth trajectories using the random-effects model with data up to 2006 and 2014 (`Plots/Plot_tr_all.pdf`).   

The script `demo_limit_sim.r` simulates the population dynamics of the salmonid populations. Since 500 simulations are run for each populations, the simulations are run in parallel with the script `init_val_demo.r`. The script `post_sim.r` takes the simulation results, summarizes them, and produces the data frame (`data/res_df_mean.RDS`) that is used in the script `Plot_mean_dens_sim.r` to produce the plot of mean simulated densities (`Plots/Plot_dens_sim.pdf`) and in the script `Plot_mean_cv_sim.r` to produce the plot of CV of simulated densities (`Plots/Plot_cv_sim.pdf`).

## 3. Manuscript

The manuscript is currently under review 
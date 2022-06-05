# #######################################################################################################################

# Author of code: Antonia D. Tsvetanova

# This is code for a simulation study presented in a manuscript entitled: 
# Impact of inconsistencies in missing data handlign across the pipeline 
# of a prediction model on estimated predictive performance: A simulation study
# Authors:
#   Antonia Tsvetanova
#   Matthew Sperrin
#   Niels Peek
#   David Jenkins
#   Iain Buchan
#   Stephanie Hyland
#   Glen P. Martin

# #######################################################################################################################




source(here::here("00_Simulation_functions.R")) #main functions to run the sim

#install.packages("tidyverse")

library(tidyverse)
library(furrr)


sims_parameters <- crossing(
  n_iter = 200, 
  N = 500000,
  N_dev = 100000,
  N_imp = 300000, 
  N_val = 100000, 
  Y_prev = c(0.1, 0.5),
  X_categorical = c(TRUE,FALSE), 
  R_prev = c(0.1, 0.2, 0.5),
  beta_x1 = c(0, 0.5, 1), 
  beta_x2 = c(0, 0.5, 1), 
  beta_U = c(0, 0.5, 1),  
  gamma_x1 = c(0, 0.5, 1), 
  gamma_x2 = c(0, 0.5, 1), 
  gamma_U = c(0, 0.5, 1)
)


#run the main simulation study:

plan(multiprocess, workers = (availableCores()-1)) 
###stores results alongside parameter values in a nested dataset (retaining names of outputs)

for(i in 1:length(sims_parameters$n_iter)){
  
  mutate(Results = future_pmap(.1 = list(n_iter = sims_parameters$n_iter[i], 
                                         N_dev = sims_parameters$N_dev[i],
                                         N_imp = sims_parameters$N_imp[i], 
                                         N_val = sims_parameters$N_val[i], 
                                         Y_prev = sims_parameters$Y_prev[i],
                                         X_categorical = sims_parameters$X_categorical[i], 
                                         R_prev = sims_parameters$R_prev[i],
                                         beta_x1 = sims_parameters$beta_x1[i], 
                                         beta_x2 = sims_parameters$beta_x2[i], 
                                         beta_U = sims_parameters$beta_U[i],  
                                         gamma_x1 = sims_parameters$gamma_x1[i], 
                                         gamma_x2 = sims_parameters$gamma_x2[i], 
                                         gamma_U = sims_parameters$gamma_U[i]), 
                               .f =  simulation_nrun_fnc,
                               .progress = TRUE,
                               .options = future_options(seed = as.integer(1234)))
  )
  
  write_rds(Results, path = here::here("Results_", i, "RDS"))
  
}

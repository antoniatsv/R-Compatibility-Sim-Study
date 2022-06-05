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


source(here::here("00_Simulation_functions.R"))

install.packages("tidyverse")

library(tidyverse)
library(furrr)

#Define a dataset that includes all combinations of simulation parameters (i.e. simulation cases) 

sims_parameters <- crossing(
  n_iter = 200, 
  N = 500000,
  N_dev = 100000,
  N_imp = 300000, 
  N_val = 100000, 
  Y_prev = c(0.1, 0.5),
  X_categorical = c(TRUE,FALSE), 
  R_prev = c(0.05, 0.1, 0.2, 0.5),
  beta_x1 = c(0, 0.1, 0.5, 1), 
  beta_x2 = c(0, 0.1, 0.5, 1), 
  beta_U = c(0, 0.1, 0.5, 1),  
  gamma_x1 = c(0, 0.5, 1), 
  gamma_x2 = c(0, 0.5, 1), 
  gamma_U = c(0, 0.5, 1)
)


#run the main simulation study:

plan(multiprocess, workers = (availableCores()-1))

for(i in 1:length(sims_parameters$n_iter)){
  
  mutate(Results = future_pmap(.1 = list(N_dev = N_dev,
                                         N_imp = N_imp,
                                         N_val = N_val,
                                         Y_prev = Y_prev,
                                         X_categorical = X_categorical,
                                         R_prev =  R_prev), 
                               .f =  simulation_nrun_fnc,
                               n_iter = n_iter,
                               beta_x1 = ,
                               beta_x2 = ,
                               beta_U = ,
                               gamma_x1 = ,
                               gamma_x2 = ,
                               gamma_U = ,
                               .progress = TRUE,
                               .options = future_options(seed = as.integer(1234)))
  )
  
  write_rds(Results, path = here::here("Results_", i, "RDS"))
  
}


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



####---------------------------------------------------------
## This script runs the simulations across all scenarios 
####---------------------------------------------------------

#load the simulation functions


source("./00_Simulation_Functions.R") #for CSF

#source(here::here("00_Simulation_Functions.R")) #for local run

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


args <- commandArgs(trailingOnly = T) #pull in all arguments from the qsub file
s <- as.numeric(args[1]) #Note that we need to specify what class the argument is

#run the main simulation study:

#plan(multiprocess, workers = (availableCores()-1)) #
###stores results alongside parameter values in a nested dataset (retaining names of outputs)

simulation_results <- simulation_nrun_fnc(
                    n_iter = sims_parameters$n_iter[s],
                    N_dev = sims_parameters$N_dev[s],
                    N_imp = sims_parameters$N_imp[s],
                    N_val = sims_parameters$N_val[s],
                    Y_prev = sims_parameters$Y_prev[s],
                    X_categorical = sims_parameters$X_categorical[s],
                    R_prev = sims_parameters$R_prev[s],
                    beta_x1 = sims_parameters$beta_x1[s],
                    beta_x2 = sims_parameters$beta_x2[s],
                    beta_U = sims_parameters$beta_U[s],
                    gamma_x1 = sims_parameters$gamma_x1[s],
                    gamma_x2 = sims_parameters$gamma_x2[s],
                    gamma_U = sims_parameters$gamma_U[s])

write_rds(simulation_results, file = paste0("Results_", s,".rds"))

warnings()

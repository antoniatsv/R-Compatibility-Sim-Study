# ##############################################################################

# Author of code: Antonia D. Tsvetanova & Glen P. Martin

# This is code for a simulation study presented in a manuscript entitled: 
# Compatibility of missing data handling methods across validation and 
# deployment of a Clinical Prediction Model
# Authors:
#   Antonia Tsvetanova
#   Matthew Sperrin
#   Niels Peek
#   David Jenkins
#   Iain Buchan
#   Stephanie Hyland
#   Glen P. Martin

# ##############################################################################

####----------------------------------------------------------------------------
## This script runs the simulations across all scenarios: implemented using the 
## computational shared facility (CSF) at University of Manchester
####----------------------------------------------------------------------------

#Load the simulation functions
source("./00_Simulation_Functions.R") 

library(tidyverse)

# Define a dataset that includes all combinations of simulation parameters:
sims_parameters <- tidyr::crossing(
  Y_prev = c(0.2),
  X_categorical = c(TRUE, FALSE), 
  R_prev = c(0.9, 0.8, 0.5), #10%, 20% and 50% missingness, respectively
  beta_x1_dev = c(0, 0.5), 
  beta_x2_dev = c(0, 0.5), 
  beta_U_dev = c(0, 0.5),  
  beta_x1_val = c(0, 0.5), 
  beta_x2_val = c(0, 0.5), 
  beta_U_val = c(0, 0.5),  
  rho_X = c(0, 0.75),
  gamma_x1 = c(0, 0.5), 
  gamma_x2 = 0.5, 
  gamma_U = c(0, 0.5)
)

args <- commandArgs(trailingOnly = T) #pull in all arguments from the qsub file
s <- as.numeric(args[1]) #Note that we need to specify what class the argument is

# number of repeats per scenario
n_rep <- 100
#Define the size of development, validation and implementation, respectively:
N_dev <- 50000
N_val <- 50000 

set.seed(465475 * s)

#run the simulation study:
simulation_results <- simulation_nrun_fnc(n_iter = n_rep, 
                                          N_dev = N_dev,
                                          N_val = N_val, 
                                          Y_prev = sims_parameters$Y_prev[s],
                                          X_categorical = sims_parameters$X_categorical[s], 
                                          R_prev = sims_parameters$R_prev[s],
                                          beta_x1_dev = sims_parameters$beta_x1_dev[s], 
                                          beta_x2_dev = sims_parameters$beta_x2_dev[s], 
                                          beta_U_dev = sims_parameters$beta_U_dev[s], 
                                          beta_x1_val = sims_parameters$beta_x1_val[s], 
                                          beta_x2_val = sims_parameters$beta_x2_val[s],  
                                          beta_U_val = sims_parameters$beta_U_val[s], 
                                          rho_X = sims_parameters$rho_X[s],
                                          gamma_x1 = sims_parameters$gamma_x1[s], 
                                          gamma_x2 = sims_parameters$gamma_x2[s], 
                                          gamma_U = sims_parameters$gamma_U[s])

#attach the simulation parameters for this scenario to the simulation results:
simulation_results <- simulation_results %>%
  dplyr::mutate("Simulation_Scenario" = s,
                "Y_prev" = sims_parameters$Y_prev[s],
                "X_categorical" = sims_parameters$X_categorical[s], 
                "R_prev" = sims_parameters$R_prev[s],
                "beta_x1_dev" = sims_parameters$beta_x1_dev[s], 
                "beta_x2_dev" = sims_parameters$beta_x2_dev[s], 
                "beta_U_dev" = sims_parameters$beta_U_dev[s], 
                "beta_x1_val" = sims_parameters$beta_x1_val[s], 
                "beta_x2_val" = sims_parameters$beta_x2_val[s],  
                "beta_U_val" = sims_parameters$beta_U_val[s], 
                "rho_X" = sims_parameters$rho_X[s],
                "gamma_x1" = sims_parameters$gamma_x1[s], 
                "gamma_x2" = sims_parameters$gamma_x2[s], 
                "gamma_U" = sims_parameters$gamma_U[s],
                .before = "Iteration")

readr::write_rds(simulation_results, 
                 file = paste("./simulation_results_", s, 
                              ".RDS", sep = ""))

warnings()
library(tidyverse)
library(ggh4x)

## Load the data 
simulation_results_all <- read_rds(here::here("Outputs", 
                                              "simulation_results_all.RDS"))


##### Add the missingness mechanism to each simulation scenario (can differ
##### across development and validation sets):
simulation_results_all <- simulation_results_all %>% 
  dplyr::mutate(
    Missing_Mech_Dev = case_when(
      beta_x1_dev == 0 & beta_x2_dev == 0 & beta_U_dev == 0 ~ "MCAR",
      
      beta_x1_dev == 0 & beta_x2_dev != 0 & beta_U_dev == 0 ~ "MAR", 
      
      beta_x1_dev != 0 & beta_x2_dev != 0 & beta_U_dev == 0 ~ "MNAR_X",
      beta_x1_dev != 0 & beta_x2_dev == 0 & beta_U_dev == 0 ~ "MNAR_X",
      
      beta_x1_dev == 0 & beta_x2_dev != 0 & beta_U_dev != 0 ~ "MNAR_Y",
      beta_x1_dev == 0 & beta_x2_dev == 0 & beta_U_dev != 0 ~ "MNAR_Y",
      
      beta_x1_dev != 0 & beta_x2_dev != 0 & beta_U_dev != 0 ~ "MNAR_XY",
      beta_x1_dev != 0 & beta_x2_dev == 0 & beta_U_dev != 0 ~ "MNAR_XY",
      TRUE ~ as.character("error")
    )
    ,
    Missing_Mech_Val = case_when(
      beta_x1_val == 0 & beta_x2_val == 0 & beta_U_val == 0 ~ "MCAR",
      
      beta_x1_val == 0 & beta_x2_val != 0 & beta_U_val == 0 ~ "MAR", 
      
      beta_x1_val != 0 & beta_x2_val != 0 & beta_U_val == 0 ~ "MNAR_X",
      beta_x1_val != 0 & beta_x2_val == 0 & beta_U_val == 0 ~ "MNAR_X",
      
      beta_x1_val == 0 & beta_x2_val != 0 & beta_U_val != 0 ~ "MNAR_Y",
      beta_x1_val == 0 & beta_x2_val == 0 & beta_U_val != 0 ~ "MNAR_Y",
      
      beta_x1_val != 0 & beta_x2_val != 0 & beta_U_val != 0 ~ "MNAR_XY",
      beta_x1_val != 0 & beta_x2_val == 0 & beta_U_val != 0 ~ "MNAR_XY",
      TRUE ~ as.character("error")
    )
  )


##### Summarise the results across all iterations for each simulation scenario
simulation_results_summarised <- simulation_results_all %>%
  dplyr::group_by(Simulation_Scenario, Missing_Mech_Dev, Missing_Mech_Val, 
                  Y_prev, X_categorical, R_prev,
                  beta_x1_dev, beta_x2_dev, beta_U_dev, 
                  beta_x1_val, beta_x2_val, beta_U_val,
                  rho_X,
                  gamma_x1, gamma_x2, gamma_U,
                  CPM,
                  Validation_Dataset) %>%
  dplyr::summarise(CalInt_Mean = mean(CalInt_est),
                   CalInt_withinSE = sqrt(mean(CalInt_var)),
                   CalInt_acrossSE = (sum(CalInt_est - CalInt_Mean))/(max(Iteration)-1),
                   CalInt_totalSE = CalInt_withinSE + CalInt_acrossSE + (CalInt_acrossSE/max(Iteration)),
                   CalInt_quantileLower = quantile(CalInt_est, 0.025),
                   CalInt_quantileUpper = quantile(CalInt_est, 0.975),
                   
                   CalSlope_Mean = mean(CalSlope_est),
                   CalSlope_withinSE = sqrt(mean(CalSlope_var)),
                   CalSlope_acrossSE = (sum(CalSlope_est - CalSlope_Mean))/(max(Iteration)-1),
                   CalSlope_totalSE = CalSlope_withinSE + CalSlope_acrossSE + (CalSlope_acrossSE/max(Iteration)),
                   CalSlope_quantileLower = quantile(CalSlope_est, 0.025),
                   CalSlope_quantileUpper = quantile(CalSlope_est, 0.975),
                   
                   AUC_Mean = mean(AUC_est),
                   AUC_withinSE = sqrt(mean(AUC_var)),
                   AUC_acrossSE = (sum(AUC_est - AUC_Mean))/(max(Iteration)-1),
                   AUC_totalSE = AUC_withinSE + AUC_acrossSE + (AUC_acrossSE/max(Iteration)),
                   AUC_quantileLower = quantile(AUC_est, 0.025),
                   AUC_quantileUpper = quantile(AUC_est, 0.975),
                   
                   Brier_Mean = mean(Brier_est),
                   Brier_withinSE = sqrt(mean(Brier_var)),
                   Brier_acrossSE = (sum(Brier_est - Brier_Mean))/(max(Iteration)-1),
                   Brier_totalSE = Brier_withinSE + Brier_acrossSE + (Brier_acrossSE/max(Iteration)),
                   Brier_quantileLower = quantile(Brier_est, 0.025),
                   Brier_quantileUpper = quantile(Brier_est, 0.975),
                   
                   .groups = "drop") %>%
  dplyr::select(-CalInt_withinSE,
                -CalInt_acrossSE,
                -CalSlope_withinSE,
                -CalSlope_acrossSE,
                -AUC_withinSE,
                -AUC_acrossSE,
                -Brier_withinSE,
                -Brier_acrossSE)

write_rds(simulation_results_summarised,
          file = here::here("outputs", "simulation_results_summarised.RDS"))
# simulation_results_summarised <- read_rds(here::here("outputs", 
#                                                      "simulation_results_summarised.RDS"))


simulation_results_summarised <- simulation_results_summarised %>%
  dplyr::mutate("Scenario" = paste(paste(Missing_Mech_Dev, "Dev", sep=" "), 
                                   paste(Missing_Mech_Val, "Val", sep=" "), 
                                   sep = " -\n "),
                
                Scenario = forcats::fct_relevel(Scenario,
                                                "MCAR Dev -\n MCAR Val",
                                                "MAR Dev -\n MAR Val",
                                                "MNAR_X Dev -\n MNAR_X Val",
                                                "MNAR_Y Dev -\n MNAR_Y Val",
                                                "MNAR_XY Dev -\n MNAR_XY Val",
                                                "MCAR Dev -\n MAR Val",
                                                "MCAR Dev -\n MNAR_X Val",
                                                "MCAR Dev -\n MNAR_Y Val",
                                                "MCAR Dev -\n MNAR_XY Val",
                                                "MAR Dev -\n MNAR_X Val",
                                                "MAR Dev -\n MNAR_Y Val",
                                                "MAR Dev -\n MNAR_XY Val",
                                                "MNAR_X Dev -\n MNAR_Y Val",
                                                "MNAR_X Dev -\n MNAR_XY Val"),
                
                Validation_Dataset = forcats::fct_recode(Validation_Dataset,
                                                         "CCA" = "CCA_val",
                                                         "MI no Y (refit)" = "MI_val_noY_newimputation",
                                                         "MI no Y (transported)" = "MI_val_noY_transportedimputation",
                                                         "MI with Y (refit)" = "MI_val_withY_newimputation",
                                                         "MI with Y (transported)" = "MI_val_withY_transportedimputation",
                                                         "RI (refit)" = "RI_val_newimputation",
                                                         "RI (transported)" = "RI_val_transportedimputation",
                                                         "All Data Required" = "fullyobserved_val",
                                                         "Mean Imputation" = "mean_val",
                                                         "Risk Absent Imputation" ="zero_val",
                                                         "Pattern Sub-Model" = "observed_val")) 



####----------------------------------------------------------------------------
### Select which estimand we want to produce plots for:
target_performance <- "All Data Required"
# target_performance <- "Mean Imputation"
# target_performance <- "RI (transported)"
# target_performance <- "RI (refit)"
# target_performance <- "MI no Y (transported)"
# target_performance <- "MI no Y (refit)"
# target_performance <- "MI with Y (transported)"
# target_performance <- "MI with Y (refit)"
# target_performance <- "Pattern Sub-Model"

### The below plots look at scenarios where X_1 is continuous, contains 50%
### missing data, and where gamma_1=gamma_2=gamma_3=0.5. Results were quantitively
### similar in other scenarios 
# Define scenario numbers of interest. We focus on 50% missingness and 
# continuous X1 (results similar over other scenarios)
#Some code to extract scenario numbers as follows:
# simulation_results_summarised %>% 
#   filter(rho_X == 0.75, X_categorical == FALSE, R_prev == 0.5, 
#          gamma_x1 == 0, gamma_U == 0, 
#          Missing_Mech_Dev == "MNAR_Y" & Missing_Mech_Val == "MNAR_XY") %>% 
#   select(Simulation_Scenario) %>% 
#   pull() %>% 
#   unique()
####----------------------------------------------------------------------------

### Function to plot the predictive performance of each CPM:
performance_plotting_fnc <- function(df,
                                     scenario_number,
                                     target_performance_imputation_method = c("All Data Required",
                                                                              "CCA",
                                                                              "Mean Imputation",
                                                                              "RI (refit)",
                                                                              "RI (transported)",
                                                                              "MI no Y (refit)",
                                                                              "MI no Y (transported)",
                                                                              "MI with Y (refit)",
                                                                              "MI with Y (transported)",
                                                                              "Pattern Sub-Model")) {
  
  target_performance_imputation_method <- as.character(match.arg(target_performance_imputation_method))
  
  df <- df %>%
    dplyr::mutate("combinations" = case_when(
      CPM == "PR_fullyobserved" |
        CPM == "PR_CCA" |
        CPM == "PR_mean" ~ Validation_Dataset %in% c("All Data Required",
                                                     "CCA",
                                                     "Mean Imputation",
                                                     "MI with Y (refit)",
                                                     "MI no Y (refit)",
                                                     "RI (refit)"),
      CPM == "PR_RI" ~ Validation_Dataset %in% c("All Data Required",
                                                 "CCA",
                                                 "Mean Imputation",
                                                 "MI with Y (refit)",
                                                 "MI no Y (refit)",
                                                 "RI (transported)",
                                                 "RI (refit)"),
      CPM == "PR_MIwithY" ~ Validation_Dataset %in% c("All Data Required",
                                                      "CCA",
                                                      "Mean Imputation",
                                                      "MI with Y (transported)",
                                                      "MI with Y (refit)",
                                                      "MI no Y (refit)",
                                                      "RI (refit)"),
      CPM == "PR_MInoY" ~ Validation_Dataset %in% c("All Data Required",
                                                    "CCA",
                                                    "Mean Imputation",
                                                    "MI with Y (refit)",
                                                    "MI no Y (transported)",
                                                    "MI no Y (refit)",
                                                    "RI (refit)"),
      
      CPM == "PR_patternsubmodel" ~ Validation_Dataset %in% c("All Data Required",
                                                              "CCA",
                                                              "Mean Imputation",
                                                              "MI with Y (refit)",
                                                              "MI no Y (refit)",
                                                              "RI (refit)",
                                                              "Pattern Sub-Model"),
      .default = NA)) %>%
    dplyr::filter(combinations == 1)
  
  scenario_df <- df %>%
    dplyr::filter(Simulation_Scenario %in% scenario_number) %>%
    dplyr::filter(Validation_Dataset == target_performance_imputation_method) %>%
    dplyr::select(Simulation_Scenario, Scenario,
                  CPM, Validation_Dataset,
                  contains("_Mean"),
                  contains("_quantileLower"),
                  contains("_quantileUpper")) %>%
    pivot_longer(cols = c(contains("_Mean"),
                          contains("_quantileLower"),
                          contains("_quantileUpper"))) %>%
    separate_wider_delim(name,
                         delim = "_",
                         names = c("Metric", "Summary_Type")) %>%
    pivot_wider(id_cols = c("Simulation_Scenario", "Scenario",
                            "CPM", "Validation_Dataset", "Metric"),
                names_from = "Summary_Type",
                values_from = "value") 
  
  plot_df <- scenario_df %>%
    dplyr::mutate(Metric = forcats::fct_recode(Metric,
                                               "Brier Score" = "Brier",
                                               "Calibration Intercept" = "CalInt",
                                               "Calibration Slope" = "CalSlope")) %>%
    mutate(CPM = stringr::str_remove(CPM, "PR_"),
           
           CPM = forcats::fct_recode(CPM,
                                     "CCA" = "CCA",
                                     "RI" = "RI",
                                     "MI no Y" = "MInoY",
                                     "MI with Y" = "MIwithY",
                                     "Fully Observed" = "fullyobserved",
                                     "Mean Imputation" = "mean",
                                     "PSM" = "patternsubmodel"),
           
           CPM = forcats::fct_relevel(CPM,
                                      "Fully Observed",
                                      "CCA",
                                      "Mean Imputation",
                                      "RI",
                                      "MI with Y",
                                      "MI no Y",
                                      "PSM"))
  
  plot_df %>%
    dplyr::mutate(Validation_Dataset = forcats::fct_recode(Validation_Dataset,
                                                           "Fully Observed Data" = "All Data Required"),
                  Validation_Dataset = forcats::fct_relevel(Validation_Dataset,
                                                            "Fully Observed Data",
                                                            "CCA",
                                                            "Mean Imputation",
                                                            "RI (refit)",
                                                            "RI (transported)",
                                                            "MI with Y (refit)",
                                                            "MI with Y (transported)",
                                                            "MI no Y (refit)",
                                                            "MI no Y (transported)",
                                                            "Pattern Sub-Model")) %>%
    ggplot(aes(x = Mean,
               y = CPM)) +
    geom_point() +
    geom_errorbar(aes(xmin = quantileLower, 
                      xmax = quantileUpper), width=.1) +
    xlab("Predictive Performance") +
    ylab("Developed CPM") +
    geom_vline(data = data.frame("Metric" = c("Calibration Intercept",
                                              "Calibration Slope"),
                                 "Mean" = c(0,1)),
               aes(xintercept = Mean),
               linetype = "dashed") +
    ggh4x::facet_grid2(Scenario ~ Metric, scales = "free") +
    ggtitle(paste("Targetting ", target_performance_imputation_method, " Performance", sep = "")) +
    theme_minimal(base_size = 12) +
    theme(legend.position = "bottom",
          panel.background = element_rect(fill = "gray90"),  
          panel.spacing.x = unit(0.5, "lines"),
          axis.text.x = element_text(angle = 45, vjust = 1, hjust=1),
          plot.title = element_text(size = 14, hjust = 0.5),
          panel.border = element_rect(color = "black", fill = NA, linewidth = 0.5)) 
}

## Consistent missingness mechanism plots:
#rho = 0 
performance_plotting_fnc(df = simulation_results_summarised,
                         scenario_number = c(4, 148),
                         target_performance_imputation_method = target_performance)

performance_plotting_fnc(df = simulation_results_summarised,
                         scenario_number = c(436, 220, 508),
                         target_performance_imputation_method = target_performance)

#rho = 0.75 
performance_plotting_fnc(df = simulation_results_summarised,
                         scenario_number = c(8, 152),
                         target_performance_imputation_method = target_performance)

performance_plotting_fnc(df = simulation_results_summarised,
                         scenario_number = c(440, 224, 512),
                         target_performance_imputation_method = target_performance)

## Inconsistent missingness mechanism plots:
#rho = 0
#MCAR+something
performance_plotting_fnc(df = simulation_results_summarised,
                         scenario_number = c(20, 52, 28, 60),
                         target_performance_imputation_method = target_performance)
#MAR+something
performance_plotting_fnc(df = simulation_results_summarised,
                         scenario_number = c(180, 156, 188),
                         target_performance_imputation_method = target_performance)
#MNAR+something
performance_plotting_fnc(df = simulation_results_summarised,
                         scenario_number = c(412, 444, 252),
                         target_performance_imputation_method = target_performance)

#rho = 0.75
#MCAR+something
performance_plotting_fnc(df = simulation_results_summarised,
                         scenario_number = c(24, 56, 32, 64),
                         target_performance_imputation_method = target_performance)
#MAR+something
performance_plotting_fnc(df = simulation_results_summarised,
                         scenario_number = c(184, 161, 193),
                         target_performance_imputation_method = target_performance)
#MNAR-X+something
performance_plotting_fnc(df = simulation_results_summarised,
                         scenario_number = c(416, 448, 256),
                         target_performance_imputation_method = target_performance)



### Function to plot the bias in validation results:
plotting_fnc <- function(df,
                         scenario_number,
                         target_performance_imputation_method = c("All Data Required",
                                                                  "CCA",
                                                                  "Mean Imputation",
                                                                  "RI (refit)",
                                                                  "RI (transported)",
                                                                  "MI no Y (refit)",
                                                                  "MI no Y (transported)",
                                                                  "MI with Y (refit)",
                                                                  "MI with Y (transported)",
                                                                  "Pattern Sub-Model")) {
  
  target_performance_imputation_method <- as.character(match.arg(target_performance_imputation_method))
  
  df <- df %>%
    dplyr::mutate("combinations" = case_when(
      CPM == "PR_fullyobserved" |
        CPM == "PR_CCA" |
        CPM == "PR_mean" ~ Validation_Dataset %in% c("All Data Required",
                                                     "CCA",
                                                     "Mean Imputation",
                                                     "MI with Y (refit)",
                                                     "MI no Y (refit)",
                                                     "RI (refit)"),
      CPM == "PR_RI" ~ Validation_Dataset %in% c("All Data Required",
                                                 "CCA",
                                                 "Mean Imputation",
                                                 "MI with Y (refit)",
                                                 "MI no Y (refit)",
                                                 "RI (transported)",
                                                 "RI (refit)"),
      CPM == "PR_MIwithY" ~ Validation_Dataset %in% c("All Data Required",
                                                      "CCA",
                                                      "Mean Imputation",
                                                      "MI with Y (transported)",
                                                      "MI with Y (refit)",
                                                      "MI no Y (refit)",
                                                      "RI (refit)"),
      CPM == "PR_MInoY" ~ Validation_Dataset %in% c("All Data Required",
                                                    "CCA",
                                                    "Mean Imputation",
                                                    "MI with Y (refit)",
                                                    "MI no Y (transported)",
                                                    "MI no Y (refit)",
                                                    "RI (refit)"),
      
      CPM == "PR_patternsubmodel" ~ Validation_Dataset %in% c("All Data Required",
                                                              "CCA",
                                                              "Mean Imputation",
                                                              "MI with Y (refit)",
                                                              "MI no Y (refit)",
                                                              "RI (refit)",
                                                              "Pattern Sub-Model"),
      .default = NA)) %>%
    dplyr::filter(combinations == 1)
  
  scenario_df <- df %>%
    dplyr::filter(Simulation_Scenario %in% scenario_number) %>%
    dplyr::select(Simulation_Scenario, Scenario,
                  CPM, Validation_Dataset,
                  contains("_Mean"),
                  contains("_quantileLower"),
                  contains("_quantileUpper")) %>%
    pivot_longer(cols = c(contains("_Mean"),
                          contains("_quantileLower"),
                          contains("_quantileUpper"))) %>%
    separate_wider_delim(name,
                         delim = "_",
                         names = c("Metric", "Summary_Type")) %>%
    pivot_wider(id_cols = c("Simulation_Scenario", "Scenario",
                            "CPM", "Validation_Dataset", "Metric"),
                names_from = "Summary_Type",
                values_from = "value")
  
  results_using_for_implementation <- df %>%
    dplyr::filter(Simulation_Scenario %in% scenario_number) %>%
    dplyr::filter(Validation_Dataset == target_performance_imputation_method) %>%
    dplyr::select(Simulation_Scenario, Scenario,
                  CPM, Validation_Dataset,
                  contains("_Mean"),
                  contains("_quantileLower"),
                  contains("_quantileUpper")) %>%
    pivot_longer(cols = c(contains("_Mean"),
                          contains("_quantileLower"),
                          contains("_quantileUpper"))) %>%
    separate_wider_delim(name,
                         delim = "_",
                         names = c("Metric", "Summary_Type")) %>%
    pivot_wider(id_cols = c("Simulation_Scenario", "Scenario",
                            "CPM", "Validation_Dataset", "Metric"),
                names_from = "Summary_Type",
                values_from = "value") %>%
    dplyr::rename("Imp_Data" = Validation_Dataset,
                  "Imp_Mean" = Mean,
                  "Imp_quantileLower" = quantileLower,
                  "Imp_quantileUpper" = quantileUpper)
  
  plot_df <- scenario_df %>%
    dplyr::left_join(results_using_for_implementation,
                     by = c("Simulation_Scenario",
                            "Scenario",
                            "CPM",
                            "Metric")) %>%
    dplyr::mutate("Bias" = Mean - Imp_Mean,
                  "Bias_Lower" = quantileLower - Imp_quantileLower,
                  "Bias_Upper" = quantileUpper - Imp_quantileUpper) %>%
    dplyr::mutate(Metric = forcats::fct_recode(Metric,
                                               "Brier Score" = "Brier",
                                               "Calibration Intercept" = "CalInt",
                                               "Calibration Slope" = "CalSlope")) %>%
    mutate(CPM = stringr::str_remove(CPM, "PR_"),
           
           CPM = forcats::fct_recode(CPM,
                                     "CCA \n developed CPM" = "CCA",
                                     "RI \n developed CPM" = "RI",
                                     "MI no Y \n developed CPM" = "MInoY",
                                     "MI with Y \n developed CPM" = "MIwithY",
                                     "Fully Observed \n developed CPM" = "fullyobserved",
                                     "Mean Imputation \n developed CPM" = "mean",
                                     "PSM \n developed CPM" = "patternsubmodel"),
           
           CPM = forcats::fct_relevel(CPM,
                                      "Fully Observed \n developed CPM",
                                      "CCA \n developed CPM",
                                      "Mean Imputation \n developed CPM",
                                      "RI \n developed CPM",
                                      "MI with Y \n developed CPM",
                                      "MI no Y \n developed CPM",
                                      "PSM \n developed CPM"))
  
  plot_df %>%
    tidyr::drop_na() %>%
    dplyr::mutate(Validation_Dataset = forcats::fct_recode(Validation_Dataset,
                                                           "Fully Observed Data" = "All Data Required"),
                  Validation_Dataset = forcats::fct_relevel(Validation_Dataset,
                                                            "Fully Observed Data",
                                                            "CCA",
                                                            "Mean Imputation",
                                                            "RI (refit)",
                                                            "RI (transported)",
                                                            "MI with Y (refit)",
                                                            "MI with Y (transported)",
                                                            "MI no Y (refit)",
                                                            "MI no Y (transported)",
                                                            "Pattern Sub-Model")) %>%
    ggplot(aes(x = Bias,
               y = Validation_Dataset,
               color = Metric)) +
    geom_point(aes(shape = Metric)) +
    geom_errorbar(aes(xmin = Bias_Lower, 
                      xmax = Bias_Upper), width=.1) +
    scale_color_brewer(palette = "Set1") +
    xlab("Bias") +
    ylab("Validation Data Imputation Method") +
    geom_vline(data = data.frame("Metric" = c("AUC",
                                              "Brier Score",
                                              "Calibration Intercept",
                                              "Calibration Slope"),
                                 "Bias" = c(0,0,0,0)),
               aes(xintercept = Bias),
               linetype = "dashed") +
    ggh4x::facet_grid2(Scenario ~ CPM, scales = "fixed") +
    ggtitle(paste("Targetting ", target_performance_imputation_method, " Performance", sep = "")) +
    theme_minimal(base_size = 12) +
    theme(legend.position = "bottom",
          panel.background = element_rect(fill = "gray90"),  
          panel.spacing.x = unit(0.5, "lines"),
          axis.text.x = element_text(angle = 45, vjust = 1, hjust=1),
          plot.title = element_text(size = 14, hjust = 0.5),
          panel.border = element_rect(color = "black", fill = NA, linewidth = 0.5)) 
}

## Consistent missingness mechanism plots:
#rho = 0 
plotting_fnc(df = simulation_results_summarised,
             scenario_number = c(4, 148),
             target_performance_imputation_method = target_performance)

plotting_fnc(df = simulation_results_summarised,
             scenario_number = c(436, 220, 508),
             target_performance_imputation_method = target_performance)

#rho = 0.75 
plotting_fnc(df = simulation_results_summarised,
             scenario_number = c(8, 152),
             target_performance_imputation_method = target_performance)

plotting_fnc(df = simulation_results_summarised,
             scenario_number = c(440, 224, 512),
             target_performance_imputation_method = target_performance)

## Inconsistent missingness mechanism plots:
#rho = 0
#MCAR+something
plotting_fnc(df = simulation_results_summarised,
             scenario_number = c(20, 52, 28, 60),
             target_performance_imputation_method = target_performance)
#MAR+something
plotting_fnc(df = simulation_results_summarised,
             scenario_number = c(180, 156, 188),
             target_performance_imputation_method = target_performance)
#MNAR+something
plotting_fnc(df = simulation_results_summarised,
             scenario_number = c(412, 444, 252),
             target_performance_imputation_method = target_performance)

#rho = 0.75
#MCAR+something
plotting_fnc(df = simulation_results_summarised,
             scenario_number = c(24, 56, 32, 64),
             target_performance_imputation_method = target_performance)
#MAR+something
plotting_fnc(df = simulation_results_summarised,
             scenario_number = c(184, 161, 193),
             target_performance_imputation_method = target_performance)
#MNAR-X+something
plotting_fnc(df = simulation_results_summarised,
             scenario_number = c(416, 448, 256),
             target_performance_imputation_method = target_performance)




###----------------------------------------------------------------------------
## Now look at a few X_categorical cases to check similar results

plotting_fnc <- function(df,
                         scenario_number,
                         target_performance_imputation_method = c("All Data Required",
                                                                  "CCA",
                                                                  "Risk Absent Imputation",
                                                                  "RI (refit)",
                                                                  "RI (transported)",
                                                                  "MI no Y (refit)",
                                                                  "MI no Y (transported)",
                                                                  "MI with Y (refit)",
                                                                  "MI with Y (transported)",
                                                                  "Pattern Sub-Model")) {
  
  target_performance_imputation_method <- as.character(match.arg(target_performance_imputation_method))
  
  df <- df %>%
    dplyr::mutate("combinations" = case_when(
      CPM == "PR_fullyobserved" |
        CPM == "PR_CCA" |
        CPM == "PR_zero" ~ Validation_Dataset %in% c("All Data Required",
                                                     "CCA",
                                                     "Risk Absent Imputation",
                                                     "MI with Y (refit)",
                                                     "MI no Y (refit)",
                                                     "RI (refit)"),
      CPM == "PR_RI" ~ Validation_Dataset %in% c("All Data Required",
                                                 "CCA",
                                                 "Risk Absent Imputation",
                                                 "MI with Y (refit)",
                                                 "MI no Y (refit)",
                                                 "RI (transported)",
                                                 "RI (refit)"),
      CPM == "PR_MIwithY" ~ Validation_Dataset %in% c("All Data Required",
                                                      "CCA",
                                                      "Risk Absent Imputation",
                                                      "MI with Y (transported)",
                                                      "MI with Y (refit)",
                                                      "MI no Y (refit)",
                                                      "RI (refit)"),
      CPM == "PR_MInoY" ~ Validation_Dataset %in% c("All Data Required",
                                                    "CCA",
                                                    "Risk Absent Imputation",
                                                    "MI with Y (refit)",
                                                    "MI no Y (transported)",
                                                    "MI no Y (refit)",
                                                    "RI (refit)"),
      
      CPM == "PR_patternsubmodel" ~ Validation_Dataset %in% c("All Data Required",
                                                              "CCA",
                                                              "Risk Absent Imputation",
                                                              "MI with Y (refit)",
                                                              "MI no Y (refit)",
                                                              "RI (refit)",
                                                              "Pattern Sub-Model"),
      .default = NA)) %>%
    dplyr::filter(combinations == 1)
  
  scenario_df <- df %>%
    dplyr::filter(Simulation_Scenario %in% scenario_number) %>%
    dplyr::select(Simulation_Scenario, Scenario,
                  CPM, Validation_Dataset,
                  contains("_Mean"),
                  contains("_quantileLower"),
                  contains("_quantileUpper")) %>%
    pivot_longer(cols = c(contains("_Mean"),
                          contains("_quantileLower"),
                          contains("_quantileUpper"))) %>%
    separate_wider_delim(name,
                         delim = "_",
                         names = c("Metric", "Summary_Type")) %>%
    pivot_wider(id_cols = c("Simulation_Scenario", "Scenario",
                            "CPM", "Validation_Dataset", "Metric"),
                names_from = "Summary_Type",
                values_from = "value")
  
  results_using_for_implementation <- df %>%
    dplyr::filter(Simulation_Scenario %in% scenario_number) %>%
    dplyr::filter(Validation_Dataset == target_performance_imputation_method) %>%
    dplyr::select(Simulation_Scenario, Scenario,
                  CPM, Validation_Dataset,
                  contains("_Mean"),
                  contains("_quantileLower"),
                  contains("_quantileUpper")) %>%
    pivot_longer(cols = c(contains("_Mean"),
                          contains("_quantileLower"),
                          contains("_quantileUpper"))) %>%
    separate_wider_delim(name,
                         delim = "_",
                         names = c("Metric", "Summary_Type")) %>%
    pivot_wider(id_cols = c("Simulation_Scenario", "Scenario",
                            "CPM", "Validation_Dataset", "Metric"),
                names_from = "Summary_Type",
                values_from = "value") %>%
    dplyr::rename("Imp_Data" = Validation_Dataset,
                  "Imp_Mean" = Mean,
                  "Imp_quantileLower" = quantileLower,
                  "Imp_quantileUpper" = quantileUpper)
  
  plot_df <- scenario_df %>%
    dplyr::left_join(results_using_for_implementation,
                     by = c("Simulation_Scenario",
                            "Scenario",
                            "CPM",
                            "Metric")) %>%
    dplyr::mutate("Bias" = Mean - Imp_Mean,
                  "Bias_Lower" = quantileLower - Imp_quantileLower,
                  "Bias_Upper" = quantileUpper - Imp_quantileUpper) %>%
    dplyr::mutate(Metric = forcats::fct_recode(Metric,
                                               "Brier Score" = "Brier",
                                               "Calibration Intercept" = "CalInt",
                                               "Calibration Slope" = "CalSlope")) %>%
    mutate(CPM = stringr::str_remove(CPM, "PR_"),
           
           CPM = forcats::fct_recode(CPM,
                                     "CCA \n developed CPM" = "CCA",
                                     "RI \n developed CPM" = "RI",
                                     "MI no Y \n developed CPM" = "MInoY",
                                     "MI with Y \n developed CPM" = "MIwithY",
                                     "Fully Observed \n developed CPM" = "fullyobserved",
                                     "Risk Absent Imputation \n developed CPM" = "zero",
                                     "Pattern Sub-Model \n developed CPM" = "patternsubmodel"),
           
           CPM = forcats::fct_relevel(CPM,
                                      "Fully Observed \n developed CPM",
                                      "CCA \n developed CPM",
                                      "Risk Absent Imputation \n developed CPM",
                                      "RI \n developed CPM",
                                      "MI with Y \n developed CPM",
                                      "MI no Y \n developed CPM",
                                      "Pattern Sub-Model \n developed CPM"))
  
  plot_df %>%
    tidyr::drop_na() %>%
    dplyr::mutate(Validation_Dataset = forcats::fct_recode(Validation_Dataset,
                                                           "Fully Observed Data" = "All Data Required"),
                  Validation_Dataset = forcats::fct_relevel(Validation_Dataset,
                                                            "Fully Observed Data",
                                                            "CCA",
                                                            "Risk Absent Imputation",
                                                            "RI (refit)",
                                                            "RI (transported)",
                                                            "MI with Y (refit)",
                                                            "MI with Y (transported)",
                                                            "MI no Y (refit)",
                                                            "MI no Y (transported)",
                                                            "Pattern Sub-Model")) %>%
    ggplot(aes(x = Bias,
               y = Validation_Dataset,
               color = Metric)) +
    geom_point(aes(shape = Metric)) +
    geom_errorbar(aes(xmin = Bias_Lower, 
                      xmax = Bias_Upper), width=.1) +
    scale_color_brewer(palette = "Set1") +
    xlab("Bias") +
    ylab("Validation Data Imputation Method") +
    geom_vline(data = data.frame("Metric" = c("AUC",
                                              "Brier Score",
                                              "Calibration Intercept",
                                              "Calibration Slope"),
                                 "Bias" = c(0,0,0,0)),
               aes(xintercept = Bias),
               linetype = "dashed") +
    ggh4x::facet_grid2(Scenario ~ CPM, scales = "fixed") +
    ggtitle(paste("Targetting ", target_performance_imputation_method, " Performance", sep = "")) +
    theme_minimal() +
    theme(legend.position = "bottom",
          panel.background = element_rect(fill = "gray90"),  
          panel.spacing.x = unit(0.5, "lines"),
          axis.title.y = element_text(size=14),
          axis.text.x = element_text(angle = 45, vjust = 1, hjust=1),
          plot.title = element_text(size = 14, hjust = 0.5),
          panel.border = element_rect(color = "black", fill = NA, linewidth = 0.5)) 
}


plotting_fnc(df = simulation_results_summarised,
             scenario_number = c(3023),
             target_performance_imputation_method = "All Data Required")

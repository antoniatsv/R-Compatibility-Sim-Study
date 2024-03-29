
### MCAR ***ALL DATA REQUIRED***
MCAR_ALLDATA_BIAS <- subset(df_imp, DAG_type == "MCAR" & dataset == "all_data_imp") %>%
  rename_at(vars("estimates"), function(x) paste0("true_", x)) %>%
  left_join(df_val, MCAR_ALLDATA_BIAS, 
            multiple = "all", 
            by = c("Iteration",
                   "mod",
                   "target_measures",
                   "n_iter",          
                   "N",
                   "N_dev",
                   "N_imp",
                   "N_val", 
                   "Y_prev",          
                   "X_categorical",   
                   "R_prev",
                   "gamma_x1",       
                   "gamma_x2",       
                   "gamma_U")) %>%
  mutate(bias = estimates - true_estimates,
         DAG_combined = paste(DAG_type.x, DAG_type.y, sep = "+")) %>%
  group_by(dataset.x, 
           dataset.y,
           target_measures, 
           scenario_number.x,
           scenario_number.y,
           N, 
           N_dev, 
           N_val, 
           N_imp, 
           Y_prev, 
           R_prev, 
           X_categorical,
           beta_x1.x,
           beta_x2.x,
           beta_U.x,
           beta_x1.y,
           beta_x2.y,
           beta_U.y,
           gamma_x1, 
           gamma_x2, 
           gamma_U,
           DAG_combined) %>% 
  summarise(bias_mean = mean(bias), LCI = quantile(bias, 0.025), UCI = quantile(bias, 0.975)) %>%
  mutate(scenario_combined = paste(scenario_number.x, scenario_number.y, sep = "+"))



### MCAR ***MEAN***
MCAR_MEAN_BIAS <- subset(df_imp, DAG_type == "MCAR" & dataset == "mean_imp") %>%
  rename_at(vars("estimates"), function(x) paste0("true_", x)) %>%
  left_join(df_val, MCAR_MEAN_BIAS, 
            multiple = "all", 
            by = c("Iteration",
                   "mod",
                   "target_measures",
                   "n_iter",          
                   "N",
                   "N_dev",
                   "N_imp",
                   "N_val", 
                   "Y_prev",          
                   "X_categorical",   
                   "R_prev",
                   "gamma_x1",       
                   "gamma_x2",       
                   "gamma_U")) %>%
  mutate(bias = estimates - true_estimates,
         DAG_combined = paste(DAG_type.x, DAG_type.y, sep = "+")) %>%
  group_by(dataset.x, 
           dataset.y,
           target_measures, 
           scenario_number.x,
           scenario_number.y,
           N, 
           N_dev, 
           N_val, 
           N_imp, 
           Y_prev, 
           R_prev, 
           X_categorical,
           beta_x1.x,
           beta_x2.x,
           beta_U.x,
           beta_x1.y,
           beta_x2.y,
           beta_U.y,
           gamma_x1, 
           gamma_x2, 
           gamma_U,
           DAG_combined) %>% 
  summarise(bias_mean = mean(bias), LCI = quantile(bias, 0.025), UCI = quantile(bias, 0.975)) %>%
  mutate(scenario_combined = paste(scenario_number.x, scenario_number.y, sep = "+"))





### MCAR ***MI no Y***
MCAR_MInoY_BIAS <- subset(df_imp, DAG_type == "MCAR" & dataset == "MI_imp_data_noY") %>%
  rename_at(vars("estimates"), function(x) paste0("true_", x)) %>%
  left_join(df_val, MCAR_MInoY_BIAS, 
            multiple = "all", 
            by = c("Iteration",
                   "mod",
                   "target_measures",
                   "n_iter",          
                   "N",
                   "N_dev",
                   "N_imp",
                   "N_val", 
                   "Y_prev",          
                   "X_categorical",   
                   "R_prev",
                   "gamma_x1",       
                   "gamma_x2",       
                   "gamma_U")) %>%
  mutate(bias = estimates - true_estimates,
         DAG_combined = paste(DAG_type.x, DAG_type.y, sep = "+")) %>%
  group_by(dataset.x, 
           dataset.y,
           target_measures, 
           scenario_number.x,
           scenario_number.y,
           N, 
           N_dev, 
           N_val, 
           N_imp, 
           Y_prev, 
           R_prev, 
           X_categorical,
           beta_x1.x,
           beta_x2.x,
           beta_U.x,
           beta_x1.y,
           beta_x2.y,
           beta_U.y,
           gamma_x1, 
           gamma_x2, 
           gamma_U,
           DAG_combined) %>% 
  summarise(bias_mean = mean(bias), LCI = quantile(bias, 0.025), UCI = quantile(bias, 0.975)) %>%
  mutate(scenario_combined = paste(scenario_number.x, scenario_number.y, sep = "+"))





### MCAR ***MI no Y***
MCAR_MIwithY_BIAS <- subset(df_imp, DAG_type == "MCAR" & dataset == "MI_imp_data_withY") %>%
  rename_at(vars("estimates"), function(x) paste0("true_", x)) %>%
  left_join(df_val, MCAR_MIwithY_BIAS, 
            multiple = "all", 
            by = c("Iteration",
                   "mod",
                   "target_measures",
                   "n_iter",          
                   "N",
                   "N_dev",
                   "N_imp",
                   "N_val", 
                   "Y_prev",          
                   "X_categorical",   
                   "R_prev",
                   "gamma_x1",       
                   "gamma_x2",       
                   "gamma_U")) %>%
  mutate(bias = estimates - true_estimates,
         DAG_combined = paste(DAG_type.x, DAG_type.y, sep = "+")) %>%
  group_by(dataset.x, 
           dataset.y,
           target_measures, 
           scenario_number.x,
           scenario_number.y,
           N, 
           N_dev, 
           N_val, 
           N_imp, 
           Y_prev, 
           R_prev, 
           X_categorical,
           beta_x1.x,
           beta_x2.x,
           beta_U.x,
           beta_x1.y,
           beta_x2.y,
           beta_U.y,
           gamma_x1, 
           gamma_x2, 
           gamma_U,
           DAG_combined) %>% 
  summarise(bias_mean = mean(bias), LCI = quantile(bias, 0.025), UCI = quantile(bias, 0.975)) %>%
  mutate(scenario_combined = paste(scenario_number.x, scenario_number.y, sep = "+"))



### COMBINE ALL MCAR BIASES INTO 1 DF
MCAR_ALL <- MCAR_ALLDATA_BIAS %>% mutate("imp_method" = "All data required") %>%
  bind_rows(MCAR_MEAN_BIAS %>% mutate("imp_method" = "Imputed by mean")) %>%
  bind_rows(MCAR_MInoY_BIAS %>% mutate("imp_method" = "MI without Y")) %>%
  bind_rows(MCAR_MIwithY_BIAS %>% mutate("imp_method" = "MI with Y")) 

MCAR_ALL <- MCAR_ALL %>% 
  mutate(dataset.x = recode(dataset.x, 
                          CCA_val_data = 'CCA',
                          MI_val_data_noY = 'MI no Y',
                          MI_val_data_withY = 'MI with Y',
                          mean_val = 'Imputed by mean')) 


MCAR_ALL <- MCAR_ALL %>% 
  mutate(target_measures = recode(target_measures, 
                                  AUC = 'AUC',
                                  Brier = 'Brier Score',
                                  Cal_Int = 'Calibration Intercept',
                                  Cal_Slope = 'Calibration Slope')) 

MCAR_ALL <- MCAR_ALL %>% factor(bias_all$target_measures,
         levels = c("AUC", "Calibration Intercept", "Calibration Slope", "Brier Score"))

  



plot_scenario <- function(sn) {
  print(sn)
  MCAR_ALL = MCAR_ALL
  
  plot <- ggplot(data = MCAR_ALL %>%
                   filter(scenario_combined == sn), 
                 aes(x = bias_mean, y = dataset.y, color = factor(target_measures),
                     shape = factor(target_measures))) + 
    geom_errorbar(aes(xmin = LCI, xmax = UCI), width=.1) +
    geom_point(size = 3, stroke = 0.5) +
    guides(color = guide_legend(reverse = TRUE)) + 
    scale_shape_manual(values = c(8, 17, 16, 15)) +
    scale_color_brewer(palette = "Set1") +
    geom_vline(xintercept = 0, linetype="dotted") + 
    xlab("Bias Mean") +
    ylab("Validation Data Imputation Methods") +
    theme_minimal() +
    theme(legend.position = "none",
          axis.text = element_text(size=14),
          axis.title = element_text(size=16, face="bold"),
          axis.text.x = element_text(size=14),
          axis.text.y = element_text(size=14),
          strip.text = element_text(size = 16),
          panel.background = element_rect(fill = "gray90"),  # add background color to panels
          panel.spacing.x = unit(0.5, "lines")) +  # increase space between panels
    ggh4x::facet_grid2(target_measures ~ imp_method, scales = "free_x", independent = "x") +
    scale_x_continuous(limits = function(x) c(-max(abs(x)), max(abs(x)))) +  # set limits to center zero
    theme(panel.border = element_rect(color = "black", fill = NA, size = 1.5),  # add border around panels
          #
          strip.text = element_text(size = 14, hjust = 0.5),  # modify panel label text
          strip.placement = "outside")  +  # move panel labels outside of plot area
    ggtitle("Implementation Data Imputation Methods") +
    theme(plot.title = element_text(face = "bold", size = 16, hjust = 0.5))
  plot <- plot + theme(panel.grid.major = element_line(size = 1.5))
  
  # Return the plot
  return(plot)
}

plot_scenario("5846+5846") #MCAR + MCAR
plot_scenario("5846+5927") #MCAR + MAR
plot_scenario("5846+6170") #MCAR + MNARX
plot_scenario("5846+5954") #MCAR + MNARY
plot_scenario("5846+6197") #MCAR + MNARXY

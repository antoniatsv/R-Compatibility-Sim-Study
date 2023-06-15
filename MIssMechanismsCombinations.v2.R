####################################################################################

# M     C      A      R  

####################################################################################

#MCAR BIAS ***ALL DATA*** AT IMPLEMENTATION + all validation methods + mechanisms 

# Sort the validation dataset by the "Iteration" column in ascending order
df_val <- df_val[order(df_val$Iteration), ]

#subset only rows with DAG == MCAR and dataset == all data required (this will be the true estimates)
imp_MCAR_alldata <- subset(df_imp, DAG_type == "MCAR" & dataset == "all_data_imp") %>%
  rename_at(vars("estimates"), function(x) paste0("true_", x))

# join the subset with df_val and calculate bias 
joined_imp_MCAR_alldata <- left_join(df_val, imp_MCAR_alldata, 
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
  mutate(bias = estimates - true_estimates)

#create a new column  DAG_combined NB: scenario_number.x = VAL and scenario_number.y = IMP
joined_imp_MCAR_alldata$DAG_combined = paste(joined_imp_MCAR_alldata$DAG_type.x, joined_imp_MCAR_alldata$DAG_type.y, sep = "+") 


#calculate bias mean and CI
BIAS_MCAR_ALLDATA <- joined_imp_MCAR_alldata %>%
  group_by(dataset.x, 
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
  summarise(bias_mean = mean(bias), LCI = quantile(bias, 0.025), UCI = quantile(bias, 0.975))



### SANITY CHECKS
which(BIAS_MCAR_ALLDATA$scenario_number.x == 1472 & 
        BIAS_MCAR_ALLDATA$scenario_number.y == 1472 & 
        BIAS_MCAR_ALLDATA$R_prev == 0.5 & 
        BIAS_MCAR_ALLDATA$Y_prev == 0.1)


which(bias_imp_all_data$scenario_number == 1472 & 
        bias_imp_all_data$R_prev.x == 0.5 & 
        bias_imp_all_data$Y_prev.x == 0.1)



####################################################################################
#MCAR BIAS ***MEAN*** AT IMPLEMENTATION + all validation methods + mechanisms 

#subset only rows with DAG == MCAR and dataset == all data required (this will be the true estimates)
imp_MCAR_mean <- subset(df_imp, DAG_type == "MCAR" & dataset == "mean_imp") %>%
  rename_at(vars("estimates"), function(x) paste0("true_", x))

# join the subset with df_val and calculate bias 
joined_imp_MCAR_mean <- left_join(df_val, imp_MCAR_mean, 
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
  mutate(bias = estimates - true_estimates)

#create a new column  DAG_combined NB: scenario_number.x = VAL and scenario_number.y = IMP
joined_imp_MCAR_mean$DAG_combined = paste(joined_imp_MCAR_mean$DAG_type.x, joined_imp_MCAR_mean$DAG_type.y, sep = "+") 

#create a new column dataset_combined
joined_imp_MCAR_mean$dataset_combined = paste(joined_imp_MCAR_mean$dataset.x, joined_imp_MCAR_mean$dataset.y, sep = "+")

#create a new column scenario_combined 
joined_imp_MCAR_mean$scenario_combined = paste(joined_imp_MCAR_mean$scenario_number.x, joined_imp_MCAR_mean$scenario_number.y, sep = "+")

#calculate bias mean and CI
BIAS_MCAR_MEAN <- joined_imp_MCAR_mean %>%
  group_by(dataset.x, 
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
  summarise(bias_mean = mean(bias), LCI = quantile(bias, 0.025), UCI = quantile(bias, 0.975))

BIAS_MCAR_MEAN$plot_number <- 1:nrow(BIAS_MCAR_MEAN)

which(bias_imp_mean$scenario_number == 1472 & 
        bias_imp_mean$R_prev.x == 0.5 & 
        bias_imp_mean$Y_prev.x == 0.1)


which(BIAS_MCAR_MEAN$scenario_number.x == 1472 & 
        BIAS_MCAR_MEAN$scenario_number.y == 1472 & 
        BIAS_MCAR_MEAN$R_prev == 0.5 & 
        BIAS_MCAR_MEAN$Y_prev == 0.1)


######################################################################################
#MCAR BIAS ***MI without Y*** AT IMPLEMENTATION + all validation methods + mechanisms 

#subset only rows with DAG == MCAR and dataset == all data required (this will be the true estimates)
imp_MCAR_MInoY <- subset(df_imp, DAG_type == "MCAR" & dataset == "MI_imp_data_noY") %>%
  rename_at(vars("estimates"), function(x) paste0("true_", x))

# join the subset with df_val and calculate bias 
joined_imp_MCAR_MInoY <- left_join(df_val, imp_MCAR_MInoY, 
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
  mutate(bias = estimates - true_estimates)

#create a new column  DAG_combined NB: scenario_number.x = VAL and scenario_number.y = IMP
joined_imp_MCAR_MInoY$DAG_combined = paste(joined_imp_MCAR_MInoY$DAG_type.x, joined_imp_MCAR_MInoY$DAG_type.y, sep = "+") 

#create a new column dataset_combined
joined_imp_MCAR_MInoY$dataset_combined = paste(joined_imp_MCAR_MInoY$dataset.x, joined_imp_MCAR_MInoY$dataset.y, sep = "+")

#create a new column scenario_combined 
joined_imp_MCAR_MInoY$scenario_combined = paste(joined_imp_MCAR_MInoY$scenario_number.x, joined_imp_MCAR_MInoY$scenario_number.y, sep = "+")

#calculate bias mean and CI
BIAS_MCAR_MInoY <- joined_imp_MCAR_MInoY %>%
  group_by(dataset.x, 
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
  summarise(bias_mean = mean(bias), LCI = quantile(bias, 0.025), UCI = quantile(bias, 0.975))

BIAS_MCAR_MInoY$plot_number <- 1:nrow(BIAS_MCAR_MInoY)

which(bias_imp_MI_noY$scenario_number == 1472 & 
        bias_imp_MI_noY$R_prev.x == 0.5 & 
        bias_imp_MI_noY$Y_prev.x == 0.1)


which(BIAS_MCAR_MInoY$scenario_number.x == 1472 & 
        BIAS_MCAR_MInoY$scenario_number.y == 1472 & 
        BIAS_MCAR_MInoY$R_prev == 0.5 & 
        BIAS_MCAR_MInoY$Y_prev == 0.1)




######################################################################################
#MCAR BIAS ***MI withY*** AT IMPLEMENTATION + all validation methods + mechanisms 

#subset only rows with DAG == MCAR and dataset == all data required (this will be the true estimates)
imp_MCAR_MIwithY <- subset(df_imp, DAG_type == "MCAR" & dataset == "MI_imp_data_withY") %>%
  rename_at(vars("estimates"), function(x) paste0("true_", x))

# join the subset with df_val and calculate bias 
joined_imp_MCAR_MIwithY <- left_join(df_val, imp_MCAR_MIwithY, 
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
  mutate(bias = estimates - true_estimates)

#create a new column  DAG_combined NB: scenario_number.x = VAL and scenario_number.y = IMP
joined_imp_MCAR_MIwithY$DAG_combined = paste(joined_imp_MCAR_MIwithY$DAG_type.x, joined_imp_MCAR_MIwithY$DAG_type.y, sep = "+") 

#create a new column dataset_combined
joined_imp_MCAR_MIwithY$dataset_combined = paste(joined_imp_MCAR_MIwithY$dataset.x, joined_imp_MCAR_MIwithY$dataset.y, sep = "+")

#create a new column scenario_combined 
joined_imp_MCAR_MIwithY$scenario_combined = paste(joined_imp_MCAR_MIwithY$scenario_number.x, joined_imp_MCAR_MIwithY$scenario_number.y, sep = "+")

#calculate bias mean and CI
BIAS_MCAR_MIwithY <- joined_imp_MCAR_MIwithY %>%
  group_by(dataset.x, 
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
  summarise(bias_mean = mean(bias), LCI = quantile(bias, 0.025), UCI = quantile(bias, 0.975))

BIAS_MCAR_MIwithY$plot_number <- 1:nrow(BIAS_MCAR_MIwithY)

which(bias_imp_MI_withY$scenario_number == 1472 & 
        bias_imp_MI_withY$R_prev.x == 0.5 & 
        bias_imp_MI_withY$Y_prev.x == 0.1)


which(BIAS_MCAR_MIwithY$scenario_number.x == 1472 & 
        BIAS_MCAR_MIwithY$scenario_number.y == 1472 & 
        BIAS_MCAR_MIwithY$R_prev == 0.5 & 
        BIAS_MCAR_MIwithY$Y_prev == 0.1)



#combine all BIAS dataframes into one BIAS_MCAR





####################################################################################

#                                M        A      R

####################################################################################


#subset only rows with DAG == MCAR and dataset == all data required (this will be the true estimates)
imp_MAR_alldata <- subset(df_imp, DAG_type == "MAR" & dataset == "all_data_imp") %>%
  rename_at(vars("estimates"), function(x) paste0("true_", x))

# join the subset with df_val and calculate bias 
joined_imp_MAR_alldata <- left_join(df_val, imp_MAR_alldata, 
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
  mutate(bias = estimates - true_estimates)

#create a new column  DAG_combined NB: scenario_number.x = VAL and scenario_number.y = IMP
joined_imp_MAR_alldata$DAG_combined = paste(joined_imp_MAR_alldata$DAG_type.x, joined_imp_MAR_alldata$DAG_type.y, sep = "+") 

#create a new column dataset_combined
joined_imp_MAR_alldata$dataset_combined = paste(joined_imp_MAR_alldata$dataset.x, joined_imp_MAR_alldata$dataset.y, sep = "+")

#create a new column scenario_combined 
joined_imp_MAR_alldata$scenario_combined = paste(joined_imp_MAR_alldata$scenario_number.x, joined_imp_MAR_alldata$scenario_number.y, sep = "+")

#calculate bias mean and CI
BIAS_MAR_ALLDATA <- joined_imp_MAR_alldata %>%
  group_by(dataset.x, 
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
  summarise(bias_mean = mean(bias), LCI = quantile(bias, 0.025), UCI = quantile(bias, 0.975))

BIAS_MAR_ALLDATA$plot_number <- 1:nrow(BIAS_MAR_ALLDATA)


### SANITY CHECKS
which(BIAS_MAR_ALLDATA$scenario_number.x == 1553 & 
        BIAS_MAR_ALLDATA$scenario_number.y == 1553 & 
        BIAS_MAR_ALLDATA$R_prev == 0.5 & 
        BIAS_MAR_ALLDATA$Y_prev == 0.1)


which(bias_imp_all_data$scenario_number == 1553 & 
        bias_imp_all_data$R_prev.x == 0.5 & 
        bias_imp_all_data$Y_prev.x == 0.1)



row_new_MAR <- BIAS_MAR_ALLDATA[134325, ]

row_old_MAR <- bias_imp_all_data[58700, ]

####################################################################################
#MAR BIAS ***MEAN*** AT IMPLEMENTATION + all validation methods + mechanisms 

#subset only rows with DAG == MCAR and dataset == all data required (this will be the true estimates)
imp_MAR_mean <- subset(df_imp, DAG_type == "MAR" & dataset == "mean_imp") %>%
  rename_at(vars("estimates"), function(x) paste0("true_", x))

# join the subset with df_val and calculate bias 
joined_imp_MAR_mean <- left_join(df_val, imp_MAR_mean, 
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
  mutate(bias = estimates - true_estimates)

#create a new column  DAG_combined NB: scenario_number.x = VAL and scenario_number.y = IMP
joined_imp_MAR_mean$DAG_combined = paste(joined_imp_MAR_mean$DAG_type.x, joined_imp_MAR_mean$DAG_type.y, sep = "+") 

#create a new column dataset_combined
joined_imp_MAR_mean$dataset_combined = paste(joined_imp_MAR_mean$dataset.x, joined_imp_MAR_mean$dataset.y, sep = "+")

#create a new column scenario_combined 
joined_imp_MAR_mean$scenario_combined = paste(joined_imp_MAR_mean$scenario_number.x, joined_imp_MAR_mean$scenario_number.y, sep = "+")

#calculate bias mean and CI
BIAS_MAR_MEAN <- joined_imp_MAR_mean %>%
  group_by(dataset.x, 
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
  summarise(bias_mean = mean(bias), LCI = quantile(bias, 0.025), UCI = quantile(bias, 0.975))

BIAS_MAR_MEAN$plot_number <- 1:nrow(BIAS_MAR_MEAN)

which(bias_imp_mean$scenario_number == 1553 & 
        bias_imp_mean$R_prev.x == 0.5 & 
        bias_imp_mean$Y_prev.x == 0.1)


which(BIAS_MAR_MEAN$scenario_number.x == 1553 & 
        BIAS_MAR_MEAN$scenario_number.y == 1553 & 
        BIAS_MAR_MEAN$R_prev == 0.5 & 
        BIAS_MAR_MEAN$Y_prev == 0.1)


row_old <- bias_imp_mean[58700, ]

row_new <- BIAS_MAR_MEAN[134325, ]


######################################################################################
#MAR BIAS ***MI without Y*** AT IMPLEMENTATION + all validation methods + mechanisms 

#subset only rows with DAG == MCAR and dataset == all data required (this will be the true estimates)
imp_MAR_MInoY <- subset(df_imp, DAG_type == "MAR" & dataset == "MI_imp_data_noY") %>%
  rename_at(vars("estimates"), function(x) paste0("true_", x))

# join the subset with df_val and calculate bias 
joined_imp_MAR_MInoY <- left_join(df_val, imp_MAR_MInoY, 
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
  mutate(bias = estimates - true_estimates)

#create a new column  DAG_combined NB: scenario_number.x = VAL and scenario_number.y = IMP
joined_imp_MAR_MInoY$DAG_combined = paste(joined_imp_MAR_MInoY$DAG_type.x, joined_imp_MAR_MInoY$DAG_type.y, sep = "+") 

#create a new column dataset_combined
joined_imp_MAR_MInoY$dataset_combined = paste(joined_imp_MAR_MInoY$dataset.x, joined_imp_MAR_MInoY$dataset.y, sep = "+")

#create a new column scenario_combined 
joined_imp_MAR_MInoY$scenario_combined = paste(joined_imp_MAR_MInoY$scenario_number.x, joined_imp_MAR_MInoY$scenario_number.y, sep = "+")

#calculate bias mean and CI
BIAS_MAR_MInoY <- joined_imp_MAR_MInoY %>%
  group_by(dataset.x, 
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
  summarise(bias_mean = mean(bias), LCI = quantile(bias, 0.025), UCI = quantile(bias, 0.975))

BIAS_MAR_MInoY$plot_number <- 1:nrow(BIAS_MAR_MInoY)

which(bias_imp_MI_noY$scenario_number == 1553 & 
        bias_imp_MI_noY$R_prev.x == 0.5 & 
        bias_imp_MI_noY$Y_prev.x == 0.1)


which(BIAS_MAR_MInoY$scenario_number.x == 1553 & 
        BIAS_MAR_MInoY$scenario_number.y == 1553 & 
        BIAS_MAR_MInoY$R_prev == 0.5 & 
        BIAS_MAR_MInoY$Y_prev == 0.1)



row_old <- bias_imp_MI_noY[58700, ]
row_new <- BIAS_MAR_MInoY[134325, ]

######################################################################################
#MAR BIAS ***MI withY*** AT IMPLEMENTATION + all validation methods + mechanisms 

#subset only rows with DAG == MCAR and dataset == all data required (this will be the true estimates)
imp_MAR_MIwithY <- subset(df_imp, DAG_type == "MAR" & dataset == "MI_imp_data_withY") %>%
  rename_at(vars("estimates"), function(x) paste0("true_", x))

# join the subset with df_val and calculate bias 
joined_imp_MAR_MIwithY <- left_join(df_val, imp_MAR_MIwithY, 
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
  mutate(bias = estimates - true_estimates)

#create a new column  DAG_combined NB: scenario_number.x = VAL and scenario_number.y = IMP
joined_imp_MAR_MIwithY$DAG_combined = paste(joined_imp_MAR_MIwithY$DAG_type.x, joined_imp_MAR_MIwithY$DAG_type.y, sep = "+") 

#create a new column dataset_combined
joined_imp_MAR_MIwithY$dataset_combined = paste(joined_imp_MAR_MIwithY$dataset.x, joined_imp_MAR_MIwithY$dataset.y, sep = "+")

#create a new column scenario_combined 
joined_imp_MAR_MIwithY$scenario_combined = paste(joined_imp_MAR_MIwithY$scenario_number.x, joined_imp_MAR_MIwithY$scenario_number.y, sep = "+")

#calculate bias mean and CI
BIAS_MAR_MIwithY <- joined_imp_MAR_MIwithY %>%
  group_by(dataset.x, 
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
  summarise(bias_mean = mean(bias), LCI = quantile(bias, 0.025), UCI = quantile(bias, 0.975))

BIAS_MAR_MIwithY$plot_number <- 1:nrow(BIAS_MAR_MIwithY)

which(bias_imp_MI_withY$scenario_number == 1553 & 
        bias_imp_MI_withY$R_prev.x == 0.5 & 
        bias_imp_MI_withY$Y_prev.x == 0.1)


which(BIAS_MAR_MIwithY$scenario_number.x == 1553 & 
        BIAS_MAR_MIwithY$scenario_number.y == 1553 & 
        BIAS_MAR_MIwithY$R_prev == 0.5 & 
        BIAS_MAR_MIwithY$Y_prev == 0.1)




row_old2 <- bias_imp_MI_withY[58700, ]
row_new_2 <- BIAS_MAR_MIwithY[134325, ]

#combine all BIAS dataframes into one BIAS_MCAR



####################################################################################

#                                M     N    A      R    -    X

####################################################################################


#subset only rows with DAG == MCAR and dataset == all data required (this will be the true estimates)
imp_MNAR1_alldata <- subset(df_imp, DAG_type == "MNAR1" & dataset == "all_data_imp") %>%
  rename_at(vars("estimates"), function(x) paste0("true_", x))

# join the subset with df_val and calculate bias 
joined_imp_MNAR1_alldata <- left_join(df_val, imp_MNAR1_alldata, 
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
  mutate(bias = estimates - true_estimates)

#create a new column  DAG_combined NB: scenario_number.x = VAL and scenario_number.y = IMP
joined_imp_MNAR1_alldata$DAG_combined = paste(joined_imp_MNAR1_alldata$DAG_type.x, joined_imp_MNAR1_alldata$DAG_type.y, sep = "+") 

#create a new column dataset_combined
joined_imp_MNAR1_alldata$dataset_combined = paste(joined_imp_MNAR1_alldata$dataset.x, joined_imp_MNAR1_alldata$dataset.y, sep = "+")

#create a new column scenario_combined 
joined_imp_MNAR1_alldata$scenario_combined = paste(joined_imp_MNAR1_alldata$scenario_number.x, joined_imp_MNAR1_alldata$scenario_number.y, sep = "+")

#calculate bias mean and CI
BIAS_MNAR1_ALLDATA <- joined_imp_MNAR1_alldata %>%
  group_by(dataset.x, 
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
  summarise(bias_mean = mean(bias), LCI = quantile(bias, 0.025), UCI = quantile(bias, 0.975))

BIAS_MNAR1_ALLDATA$plot_number <- 1:nrow(BIAS_MNAR1_ALLDATA)


### SANITY CHECKS
which(BIAS_MNAR1_ALLDATA$scenario_number.x == 1796 & 
        BIAS_MNAR1_ALLDATA$scenario_number.y == 1796 & 
        BIAS_MNAR1_ALLDATA$R_prev == 0.5 & 
        BIAS_MNAR1_ALLDATA$Y_prev == 0.1)


which(bias_imp_all_data$scenario_number == 1796 & 
        bias_imp_all_data$R_prev.x == 0.5 & 
        bias_imp_all_data$Y_prev.x == 0.1)



row_new_MNAR1 <- BIAS_MNAR1_ALLDATA[269621, ]

row_old_MNAR1 <- bias_imp_all_data[59672, ]

####################################################################################
#MAR BIAS ***MEAN*** AT IMPLEMENTATION + all validation methods + mechanisms 

#subset only rows with DAG == MCAR and dataset == all data required (this will be the true estimates)
imp_MNAR1_mean <- subset(df_imp, DAG_type == "MNAR1" & dataset == "mean_imp") %>%
  rename_at(vars("estimates"), function(x) paste0("true_", x))

# join the subset with df_val and calculate bias 
joined_imp_MNAR1_mean <- left_join(df_val, imp_MNAR1_mean, 
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
  mutate(bias = estimates - true_estimates)

#create a new column  DAG_combined NB: scenario_number.x = VAL and scenario_number.y = IMP
joined_imp_MNAR1_mean$DAG_combined = paste(joined_imp_MNAR1_mean$DAG_type.x, joined_imp_MNAR1_mean$DAG_type.y, sep = "+") 

#create a new column dataset_combined
joined_imp_MNAR1_mean$dataset_combined = paste(joined_imp_MNAR1_mean$dataset.x, joined_imp_MNAR1_mean$dataset.y, sep = "+")

#create a new column scenario_combined 
joined_imp_MNAR1_mean$scenario_combined = paste(joined_imp_MNAR1_mean$scenario_number.x, joined_imp_MNAR1_mean$scenario_number.y, sep = "+")

#calculate bias mean and CI
BIAS_MNAR1_MEAN <- joined_imp_MNAR1_mean %>%
  group_by(dataset.x, 
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
  summarise(bias_mean = mean(bias), LCI = quantile(bias, 0.025), UCI = quantile(bias, 0.975))

BIAS_MNAR1_MEAN$plot_number <- 1:nrow(BIAS_MNAR1_MEAN)

which(bias_imp_mean$scenario_number == 1796 & 
        bias_imp_mean$R_prev.x == 0.5 & 
        bias_imp_mean$Y_prev.x == 0.1)


which(BIAS_MNAR1_MEAN$scenario_number.x == 1796 & 
        BIAS_MNAR1_MEAN$scenario_number.y == 1796 & 
        BIAS_MNAR1_MEAN$R_prev == 0.5 & 
        BIAS_MNAR1_MEAN$Y_prev == 0.1)


row_old <- bias_imp_mean[59672, ]

row_new <- BIAS_MNAR1_MEAN[269621, ]


######################################################################################
#MAR BIAS ***MI without Y*** AT IMPLEMENTATION + all validation methods + mechanisms 

#subset only rows with DAG == MCAR and dataset == all data required (this will be the true estimates)
imp_MNAR1_MInoY <- subset(df_imp, DAG_type == "MNAR1" & dataset == "MI_imp_data_noY") %>%
  rename_at(vars("estimates"), function(x) paste0("true_", x))

# join the subset with df_val and calculate bias 
joined_imp_MNAR1_MInoY <- left_join(df_val, imp_MNAR1_MInoY, 
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
  mutate(bias = estimates - true_estimates)

#create a new column  DAG_combined NB: scenario_number.x = VAL and scenario_number.y = IMP
joined_imp_MNAR1_MInoY$DAG_combined = paste(joined_imp_MNAR1_MInoY$DAG_type.x, joined_imp_MNAR1_MInoY$DAG_type.y, sep = "+") 

#create a new column dataset_combined
joined_imp_MNAR1_MInoY$dataset_combined = paste(joined_imp_MNAR1_MInoY$dataset.x, joined_imp_MNAR1_MInoY$dataset.y, sep = "+")

#create a new column scenario_combined 
joined_imp_MNAR1_MInoY$scenario_combined = paste(joined_imp_MNAR1_MInoY$scenario_number.x, joined_imp_MNAR1_MInoY$scenario_number.y, sep = "+")

#calculate bias mean and CI
BIAS_MNAR1_MInoY <- joined_imp_MNAR1_MInoY %>%
  group_by(dataset.x, 
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
  summarise(bias_mean = mean(bias), LCI = quantile(bias, 0.025), UCI = quantile(bias, 0.975))

BIAS_MNAR1_MInoY$plot_number <- 1:nrow(BIAS_MNAR1_MInoY)

which(bias_imp_MI_noY$scenario_number == 1796 & 
        bias_imp_MI_noY$R_prev.x == 0.5 & 
        bias_imp_MI_noY$Y_prev.x == 0.1)


which(BIAS_MNAR1_MInoY$scenario_number.x == 1796 & 
        BIAS_MNAR1_MInoY$scenario_number.y == 1796 & 
        BIAS_MNAR1_MInoY$R_prev == 0.5 & 
        BIAS_MNAR1_MInoY$Y_prev == 0.1)



row_old <- bias_imp_MI_noY[59672, ]
row_new <- BIAS_MNAR1_MInoY[269621, ]

######################################################################################
#MAR BIAS ***MI withY*** AT IMPLEMENTATION + all validation methods + mechanisms 

#subset only rows with DAG == MCAR and dataset == all data required (this will be the true estimates)
imp_MNAR1_MIwithY <- subset(df_imp, DAG_type == "MNAR1" & dataset == "MI_imp_data_withY") %>%
  rename_at(vars("estimates"), function(x) paste0("true_", x))

# join the subset with df_val and calculate bias 
joined_imp_MNAR1_MIwithY <- left_join(df_val, imp_MNAR1_MIwithY, 
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
  mutate(bias = estimates - true_estimates)

#create a new column  DAG_combined NB: scenario_number.x = VAL and scenario_number.y = IMP
joined_imp_MNAR1_MIwithY$DAG_combined = paste(joined_imp_MNAR1_MIwithY$DAG_type.x, joined_imp_MNAR1_MIwithY$DAG_type.y, sep = "+") 

#create a new column dataset_combined
joined_imp_MNAR1_MIwithY$dataset_combined = paste(joined_imp_MNAR1_MIwithY$dataset.x, joined_imp_MNAR1_MIwithY$dataset.y, sep = "+")

#create a new column scenario_combined 
joined_imp_MNAR1_MIwithY$scenario_combined = paste(joined_imp_MNAR1_MIwithY$scenario_number.x, joined_imp_MNAR1_MIwithY$scenario_number.y, sep = "+")

#calculate bias mean and CI
BIAS_MNAR1_MIwithY <- joined_imp_MNAR1_MIwithY %>%
  group_by(dataset.x, 
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
  summarise(bias_mean = mean(bias), LCI = quantile(bias, 0.025), UCI = quantile(bias, 0.975))

BIAS_MNAR1_MIwithY$plot_number <- 1:nrow(BIAS_MNAR1_MIwithY)

which(bias_imp_MI_withY$scenario_number == 1796 & 
        bias_imp_MI_withY$R_prev.x == 0.5 & 
        bias_imp_MI_withY$Y_prev.x == 0.1)


which(BIAS_MNAR1_MIwithY$scenario_number.x == 1796 & 
        BIAS_MNAR1_MIwithY$scenario_number.y == 1796 & 
        BIAS_MNAR1_MIwithY$R_prev == 0.5 & 
        BIAS_MNAR1_MIwithY$Y_prev == 0.1)




#combine all BIAS dataframes into one BIAS_MCAR






####################################################################################

#                                M     N    A      R    -    Y

####################################################################################


#subset only rows with DAG == MNARY and dataset == all data required (this will be the true estimates)
imp_MNAR2_alldata <- subset(df_imp, DAG_type == "MNAR2" & dataset == "all_data_imp") %>%
  rename_at(vars("estimates"), function(x) paste0("true_", x))

# join the subset with df_val and calculate bias 
joined_imp_MNAR2_alldata <- left_join(df_val, imp_MNAR2_alldata, 
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
  mutate(bias = estimates - true_estimates)

#create a new column  DAG_combined NB: scenario_number.x = VAL and scenario_number.y = IMP
joined_imp_MNAR2_alldata$DAG_combined = paste(joined_imp_MNAR2_alldata$DAG_type.x, joined_imp_MNAR2_alldata$DAG_type.y, sep = "+") 

#create a new column dataset_combined
joined_imp_MNAR2_alldata$dataset_combined = paste(joined_imp_MNAR2_alldata$dataset.x, joined_imp_MNAR2_alldata$dataset.y, sep = "+")

#create a new column scenario_combined 
joined_imp_MNAR2_alldata$scenario_combined = paste(joined_imp_MNAR2_alldata$scenario_number.x, joined_imp_MNAR2_alldata$scenario_number.y, sep = "+")

#calculate bias mean and CI
BIAS_MNAR2_ALLDATA <- joined_imp_MNAR2_alldata %>%
  group_by(dataset.x, 
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
  summarise(bias_mean = mean(bias), LCI = quantile(bias, 0.025), UCI = quantile(bias, 0.975))

BIAS_MNAR2_ALLDATA$plot_number <- 1:nrow(BIAS_MNAR2_ALLDATA)


### SANITY CHECKS
which(BIAS_MNAR2_ALLDATA$scenario_number.x == 1580 & 
        BIAS_MNAR2_ALLDATA$scenario_number.y == 1580 & 
        BIAS_MNAR2_ALLDATA$R_prev == 0.5 & 
        BIAS_MNAR2_ALLDATA$Y_prev == 0.1)


which(bias_imp_all_data$scenario_number == 1580 & 
        bias_imp_all_data$R_prev.x == 0.5 & 
        bias_imp_all_data$Y_prev.x == 0.1)



row_new_MNAR2 <- BIAS_MNAR2_ALLDATA[268757, ]

row_old_MNAR2 <- bias_imp_all_data[58808, ]

####################################################################################
#MAR BIAS ***MEAN*** AT IMPLEMENTATION + all validation methods + mechanisms 

#subset only rows with DAG == MCAR and dataset == all data required (this will be the true estimates)
imp_MNAR2_mean <- subset(df_imp, DAG_type == "MNAR2" & dataset == "mean_imp") %>%
  rename_at(vars("estimates"), function(x) paste0("true_", x))

# join the subset with df_val and calculate bias 
joined_imp_MNAR2_mean <- left_join(df_val, imp_MNAR2_mean, 
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
  mutate(bias = estimates - true_estimates)

#create a new column  DAG_combined NB: scenario_number.x = VAL and scenario_number.y = IMP
joined_imp_MNAR2_mean$DAG_combined = paste(joined_imp_MNAR2_mean$DAG_type.x, joined_imp_MNAR2_mean$DAG_type.y, sep = "+") 

#create a new column dataset_combined
joined_imp_MNAR2_mean$dataset_combined = paste(joined_imp_MNAR2_mean$dataset.x, joined_imp_MNAR2_mean$dataset.y, sep = "+")

#create a new column scenario_combined 
joined_imp_MNAR2_mean$scenario_combined = paste(joined_imp_MNAR2_mean$scenario_number.x, joined_imp_MNAR2_mean$scenario_number.y, sep = "+")

#calculate bias mean and CI
BIAS_MNAR2_MEAN <- joined_imp_MNAR2_mean %>%
  group_by(dataset.x, 
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
  summarise(bias_mean = mean(bias), LCI = quantile(bias, 0.025), UCI = quantile(bias, 0.975))

BIAS_MNAR2_MEAN$plot_number <- 1:nrow(BIAS_MNAR2_MEAN)

which(bias_imp_mean$scenario_number == 1580 & 
        bias_imp_mean$R_prev.x == 0.5 & 
        bias_imp_mean$Y_prev.x == 0.1)


which(BIAS_MNAR2_MEAN$scenario_number.x == 1580 & 
        BIAS_MNAR2_MEAN$scenario_number.y == 1580 & 
        BIAS_MNAR2_MEAN$R_prev == 0.5 & 
        BIAS_MNAR2_MEAN$Y_prev == 0.1)


row_old <- bias_imp_mean[58808, ]

row_new <- BIAS_MNAR2_MEAN[268757, ]


######################################################################################
#MNAR2 BIAS ***MI without Y*** AT IMPLEMENTATION + all validation methods + mechanisms 

#subset only rows with DAG == MCAR and dataset == all data required (this will be the true estimates)
imp_MNAR2_MInoY <- subset(df_imp, DAG_type == "MNAR2" & dataset == "MI_imp_data_noY") %>%
  rename_at(vars("estimates"), function(x) paste0("true_", x))

# join the subset with df_val and calculate bias 
joined_imp_MNAR2_MInoY <- left_join(df_val, imp_MNAR2_MInoY, 
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
  mutate(bias = estimates - true_estimates)

#create a new column  DAG_combined NB: scenario_number.x = VAL and scenario_number.y = IMP
joined_imp_MNAR2_MInoY$DAG_combined = paste(joined_imp_MNAR2_MInoY$DAG_type.x, joined_imp_MNAR2_MInoY$DAG_type.y, sep = "+") 

#create a new column dataset_combined
joined_imp_MNAR2_MInoY$dataset_combined = paste(joined_imp_MNAR2_MInoY$dataset.x, joined_imp_MNAR2_MInoY$dataset.y, sep = "+")

#create a new column scenario_combined 
joined_imp_MNAR2_MInoY$scenario_combined = paste(joined_imp_MNAR2_MInoY$scenario_number.x, joined_imp_MNAR2_MInoY$scenario_number.y, sep = "+")

#calculate bias mean and CI
BIAS_MNAR2_MInoY <- joined_imp_MNAR2_MInoY %>%
  group_by(dataset.x, 
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
  summarise(bias_mean = mean(bias), LCI = quantile(bias, 0.025), UCI = quantile(bias, 0.975))

BIAS_MNAR2_MInoY$plot_number <- 1:nrow(BIAS_MNAR2_MInoY)

which(bias_imp_MI_noY$scenario_number == 1580 & 
        bias_imp_MI_noY$R_prev.x == 0.5 & 
        bias_imp_MI_noY$Y_prev.x == 0.1)


which(BIAS_MNAR2_MInoY$scenario_number.x == 1580 & 
        BIAS_MNAR2_MInoY$scenario_number.y == 1580 & 
        BIAS_MNAR2_MInoY$R_prev == 0.5 & 
        BIAS_MNAR2_MInoY$Y_prev == 0.1)



row_old <- bias_imp_MI_noY[59672, ]
row_new <- BIAS_MNAR2_MInoY[269621, ]

######################################################################################
#MNARY BIAS ***MI withY*** AT IMPLEMENTATION + all validation methods + mechanisms 

#subset only rows with DAG == MCAR and dataset == all data required (this will be the true estimates)
imp_MNAR2_MIwithY <- subset(df_imp, DAG_type == "MNAR2" & dataset == "MI_imp_data_withY") %>%
  rename_at(vars("estimates"), function(x) paste0("true_", x))

# join the subset with df_val and calculate bias 
joined_imp_MNAR2_MIwithY <- left_join(df_val, imp_MNAR2_MIwithY, 
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
  mutate(bias = estimates - true_estimates)

#create a new column  DAG_combined NB: scenario_number.x = VAL and scenario_number.y = IMP
joined_imp_MNAR2_MIwithY$DAG_combined = paste(joined_imp_MNAR2_MIwithY$DAG_type.x, joined_imp_MNAR2_MIwithY$DAG_type.y, sep = "+") 

#create a new column dataset_combined
joined_imp_MNAR2_MIwithY$dataset_combined = paste(joined_imp_MNAR2_MIwithY$dataset.x, joined_imp_MNAR2_MIwithY$dataset.y, sep = "+")

#create a new column scenario_combined 
joined_imp_MNAR2_MIwithY$scenario_combined = paste(joined_imp_MNAR2_MIwithY$scenario_number.x, joined_imp_MNAR2_MIwithY$scenario_number.y, sep = "+")

#calculate bias mean and CI
BIAS_MNAR2_MIwithY <- joined_imp_MNAR2_MIwithY %>%
  group_by(dataset.x, 
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
  summarise(bias_mean = mean(bias), LCI = quantile(bias, 0.025), UCI = quantile(bias, 0.975))

BIAS_MNAR2_MIwithY$plot_number <- 1:nrow(BIAS_MNAR2_MIwithY)

which(bias_imp_MI_withY$scenario_number == 1580 & 
        bias_imp_MI_withY$R_prev.x == 0.5 & 
        bias_imp_MI_withY$Y_prev.x == 0.1)


which(BIAS_MNAR2_MIwithY$scenario_number.x == 1580 & 
        BIAS_MNAR1_MIwithY$scenario_number.y == 1580 & 
        BIAS_MNAR1_MIwithY$R_prev == 0.5 & 
        BIAS_MNAR1_MIwithY$Y_prev == 0.1)




#combine all BIAS dataframes into one BIAS_MNAR2



####################################################################################

#                                M     N    A      R    -   X Y

####################################################################################


#subset only rows with DAG == MNARXY and dataset == all data required (this will be the true estimates)
imp_MNAR3_alldata <- subset(df_imp, DAG_type == "MNAR3" & dataset == "all_data_imp") %>%
  rename_at(vars("estimates"), function(x) paste0("true_", x))

# join the subset with df_val and calculate bias 
joined_imp_MNAR3_alldata <- left_join(df_val, imp_MNAR3_alldata, 
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
  mutate(bias = estimates - true_estimates)

#create a new column  DAG_combined NB: scenario_number.x = VAL and scenario_number.y = IMP
joined_imp_MNAR3_alldata$DAG_combined = paste(joined_imp_MNAR3_alldata$DAG_type.x, joined_imp_MNAR3_alldata$DAG_type.y, sep = "+") 

#create a new column dataset_combined
joined_imp_MNAR3_alldata$dataset_combined = paste(joined_imp_MNAR3_alldata$dataset.x, joined_imp_MNAR3_alldata$dataset.y, sep = "+")

#create a new column scenario_combined 
joined_imp_MNAR3_alldata$scenario_combined = paste(joined_imp_MNAR3_alldata$scenario_number.x, joined_imp_MNAR3_alldata$scenario_number.y, sep = "+")

#calculate bias mean and CI
BIAS_MNAR3_ALLDATA <- joined_imp_MNAR3_alldata %>%
  group_by(dataset.x, 
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
  summarise(bias_mean = mean(bias), LCI = quantile(bias, 0.025), UCI = quantile(bias, 0.975))

BIAS_MNAR3_ALLDATA$plot_number <- 1:nrow(BIAS_MNAR2_ALLDATA)


### SANITY CHECKS
which(BIAS_MNAR3_ALLDATA$scenario_number.x == 1580 & 
        BIAS_MNAR3_ALLDATA$scenario_number.y == 1580 & 
        BIAS_MNAR3_ALLDATA$R_prev == 0.5 & 
        BIAS_MNAR3_ALLDATA$Y_prev == 0.1)


which(bias_imp_all_data$scenario_number == 1580 & 
        bias_imp_all_data$R_prev.x == 0.5 & 
        bias_imp_all_data$Y_prev.x == 0.1)



row_new_MNAR3 <- BIAS_MNAR3_ALLDATA[268757, ]

row_old_MNAR3 <- bias_imp_all_data[58808, ]

####################################################################################
#MAR BIAS ***MEAN*** AT IMPLEMENTATION + all validation methods + mechanisms 

#subset only rows with DAG == MCAR and dataset == all data required (this will be the true estimates)
imp_MNAR3_mean <- subset(df_imp, DAG_type == "MNAR3" & dataset == "mean_imp") %>%
  rename_at(vars("estimates"), function(x) paste0("true_", x))

# join the subset with df_val and calculate bias 
joined_imp_MNAR3_mean <- left_join(df_val, imp_MNAR3_mean, 
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
  mutate(bias = estimates - true_estimates)

#create a new column  DAG_combined NB: scenario_number.x = VAL and scenario_number.y = IMP
joined_imp_MNAR3_mean$DAG_combined = paste(joined_imp_MNAR3_mean$DAG_type.x, joined_imp_MNAR3_mean$DAG_type.y, sep = "+") 

#create a new column dataset_combined
joined_imp_MNAR3_mean$dataset_combined = paste(joined_imp_MNAR3_mean$dataset.x, joined_imp_MNAR3_mean$dataset.y, sep = "+")

#create a new column scenario_combined 
joined_imp_MNAR3_mean$scenario_combined = paste(joined_imp_MNAR3_mean$scenario_number.x, joined_imp_MNAR3_mean$scenario_number.y, sep = "+")

#calculate bias mean and CI
BIAS_MNAR3_MEAN <- joined_imp_MNAR3_mean %>%
  group_by(dataset.x, 
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
  summarise(bias_mean = mean(bias), LCI = quantile(bias, 0.025), UCI = quantile(bias, 0.975))

BIAS_MNAR3_MEAN$plot_number <- 1:nrow(BIAS_MNAR3_MEAN)

which(bias_imp_mean$scenario_number == 1580 & 
        bias_imp_mean$R_prev.x == 0.5 & 
        bias_imp_mean$Y_prev.x == 0.1)


which(BIAS_MNAR2_MEAN$scenario_number.x == 1580 & 
        BIAS_MNAR2_MEAN$scenario_number.y == 1580 & 
        BIAS_MNAR2_MEAN$R_prev == 0.5 & 
        BIAS_MNAR2_MEAN$Y_prev == 0.1)


row_old <- bias_imp_mean[58808, ]

row_new <- BIAS_MNAR2_MEAN[268757, ]


######################################################################################
#MNAR2 BIAS ***MI without Y*** AT IMPLEMENTATION + all validation methods + mechanisms 

#subset only rows with DAG == MCAR and dataset == all data required (this will be the true estimates)
imp_MNAR3_MInoY <- subset(df_imp, DAG_type == "MNAR3" & dataset == "MI_imp_data_noY") %>%
  rename_at(vars("estimates"), function(x) paste0("true_", x))

# join the subset with df_val and calculate bias 
joined_imp_MNAR3_MInoY <- left_join(df_val, imp_MNAR3_MInoY, 
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
  mutate(bias = estimates - true_estimates)

#create a new column  DAG_combined NB: scenario_number.x = VAL and scenario_number.y = IMP
joined_imp_MNAR3_MInoY$DAG_combined = paste(joined_imp_MNAR3_MInoY$DAG_type.x, joined_imp_MNAR3_MInoY$DAG_type.y, sep = "+") 

#create a new column dataset_combined
joined_imp_MNAR3_MInoY$dataset_combined = paste(joined_imp_MNAR3_MInoY$dataset.x, joined_imp_MNAR3_MInoY$dataset.y, sep = "+")

#create a new column scenario_combined 
joined_imp_MNAR3_MInoY$scenario_combined = paste(joined_imp_MNAR3_MInoY$scenario_number.x, joined_imp_MNAR3_MInoY$scenario_number.y, sep = "+")

#calculate bias mean and CI
BIAS_MNAR3_MInoY <- joined_imp_MNAR3_MInoY %>%
  group_by(dataset.x, 
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
  summarise(bias_mean = mean(bias), LCI = quantile(bias, 0.025), UCI = quantile(bias, 0.975))

BIAS_MNAR3_MInoY$plot_number <- 1:nrow(BIAS_MNAR3_MInoY)

which(bias_imp_MI_noY$scenario_number == 1580 & 
        bias_imp_MI_noY$R_prev.x == 0.5 & 
        bias_imp_MI_noY$Y_prev.x == 0.1)


which(BIAS_MNAR3_MInoY$scenario_number.x == 1580 & 
        BIAS_MNAR3_MInoY$scenario_number.y == 1580 & 
        BIAS_MNAR3_MInoY$R_prev == 0.5 & 
        BIAS_MNAR3_MInoY$Y_prev == 0.1)



row_old <- bias_imp_MI_noY[59672, ]
row_new <- BIAS_MNAR3_MInoY[269621, ]

######################################################################################
#MNARY BIAS ***MI withY*** AT IMPLEMENTATION + all validation methods + mechanisms 

#subset only rows with DAG == MCAR and dataset == all data required (this will be the true estimates)
imp_MNAR3_MIwithY <- subset(df_imp, DAG_type == "MNAR3" & dataset == "MI_imp_data_withY") %>%
  rename_at(vars("estimates"), function(x) paste0("true_", x))

# join the subset with df_val and calculate bias 
joined_imp_MNAR3_MIwithY <- left_join(df_val, imp_MNAR3_MIwithY, 
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
  mutate(bias = estimates - true_estimates)

#create a new column  DAG_combined NB: scenario_number.x = VAL and scenario_number.y = IMP
joined_imp_MNAR3_MIwithY$DAG_combined = paste(joined_imp_MNAR3_MIwithY$DAG_type.x, joined_imp_MNAR3_MIwithY$DAG_type.y, sep = "+") 

#create a new column dataset_combined
joined_imp_MNAR3_MIwithY$dataset_combined = paste(joined_imp_MNAR3_MIwithY$dataset.x, joined_imp_MNAR3_MIwithY$dataset.y, sep = "+")

#create a new column scenario_combined 
joined_imp_MNAR3_MIwithY$scenario_combined = paste(joined_imp_MNAR3_MIwithY$scenario_number.x, joined_imp_MNAR3_MIwithY$scenario_number.y, sep = "+")

#calculate bias mean and CI
BIAS_MNAR3_MIwithY <- joined_imp_MNAR3_MIwithY %>%
  group_by(dataset.x, 
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
  summarise(bias_mean = mean(bias), LCI = quantile(bias, 0.025), UCI = quantile(bias, 0.975))

BIAS_MNAR3_MIwithY$plot_number <- 1:nrow(BIAS_MNAR3_MIwithY)

which(bias_imp_MI_withY$scenario_number == 1580 & 
        bias_imp_MI_withY$R_prev.x == 0.5 & 
        bias_imp_MI_withY$Y_prev.x == 0.1)


which(BIAS_MNAR3_MIwithY$scenario_number.x == 1580 & 
        BIAS_MNAR3_MIwithY$scenario_number.y == 1580 & 
        BIAS_MNAR3_MIwithY$R_prev == 0.5 & 
        BIAS_MNAR3_MIwithY$Y_prev == 0.1)




#combine all BIAS dataframes into one BIAS_MNAR3





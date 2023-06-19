library(tidyverse)
library(magrittr)
library(dplyr)
library(ggplot2)
library(data.table)
library(RColorBrewer)
library(ggh4x)

# Iterations 1-25
##############################################################################################################
setwd("/Users/user/AntoniaPhD/2. RSim/RESULTScontinuous/25x1")
##############################################################################################################

#gives the filenames in the filepath
filenames <- list.files(pattern = ".rds") 

#read all files to be concatenated
df_1 <- list.files(pattern = ".rds") %>%
  map_dfr(readRDS) 


#extract the scenario numbers from the rds files names (note 1 rds is missing?)
scenario_number <- as.numeric(str_extract(filenames, pattern = "[0-9]+"))

test_1 <- c()

for(i in 1:length(scenario_number)){ test_1 <- c(test_1, rep(scenario_number[i],900) ) }

df_1$scenario_number <- test_1


# Iterations 26 - 50 
##############################################################################################################
# C H A N G E   D I R E C T O R Y 
setwd("/Users/user/AntoniaPhD/2. RSim/RESULTScontinuous/25x2")
##############################################################################################################
filenames <- list.files(pattern = ".rds") 


df_2 <- list.files(pattern = ".rds") %>%
  map_dfr(readRDS) 

#extract the scenario numbers from the rds files names 
scenario_number <- as.numeric(str_extract(filenames, pattern = "[0-9]+"))

df_2$Iteration <- df_2$Iteration + 25 

test_2 <- c()

for(i in 1:length(scenario_number)){ test_2 <- c(test_2, rep(scenario_number[i],900) ) }

df_2$scenario_number <- test_2




# Iterations 51 - 75
##############################################################################################################
# C H A N G E   D I R E C T O R Y 
setwd("/Users/user/AntoniaPhD/2. RSim/RESULTScontinuous/25x3")
##############################################################################################################
filenames <- list.files(pattern = ".rds") 


df_3 <- list.files(pattern = ".rds") %>%
  map_dfr(readRDS) 

#extract the scenario numbers from the rds files names (note 1 rds is missing?)
scenario_number <- as.numeric(str_extract(filenames, pattern = "[0-9]+"))

df_3$Iteration <- df_3$Iteration + 50

test_3 <- c()

for(i in 1:length(scenario_number)){ test_3 <- c(test_3, rep(scenario_number[i],900) ) }

df_3$scenario_number <- test_3



# Iterations 76 - 100
##############################################################################################################
# C H A N G E   D I R E C T O R Y 

setwd("/Users/user/AntoniaPhD/2. RSim/RESULTScontinuous/25x4")
##############################################################################################################

filenames <- list.files(pattern = ".rds") 


df_4 <- list.files(pattern = ".rds") %>%
  map_dfr(readRDS) 

#extract the scenario numbers from the rds files names (note 1 rds is missing?)
scenario_number <- as.numeric(str_extract(filenames, pattern = "[0-9]+"))

df_4$Iteration <- df_4$Iteration + 75

test_4 <- c()

for(i in 1:length(scenario_number)){ test_4 <- c(test_4, rep(scenario_number[i],900) ) }

df_4$scenario_number <- test_4

##############################################################################################################
# concatenate df_1, df_2, df_3 and df_4 into one big data frame 

df_all_iter <- rbind(df_1, df_2, df_3, df_4)

df_all_iter <- df_all_iter[, -c(5,7,9,11)] #use select instead

##############################################################################################################

rm(df_1)
rm(df_2)
rm(df_3)
rm(df_4)



df_long <- pivot_longer(df_all_iter, 4:7, names_to = "target_measures", values_to = "estimates") %>%
  filter(mod == 1)

rm(df_all_iter)

sims_parameters <- readRDS(file = "sims_parameters.RDS") %>%
  mutate(scenario_number = 1:n())


df_scenario_number <- left_join(df_long, sims_parameters, by = "scenario_number") 

rm(df_long)




##### add DAG type to the df ######
df_DAG <- df_scenario_number %>% 
  mutate(
    DAG_type = case_when(
      beta_x1 == 0 & beta_x2 == 0 & beta_U == 0 ~ "MCAR",
      beta_x1 == 0 & beta_x2 != 0 & beta_U == 0 ~ "MAR", 
      beta_x1 != 0 & beta_x2 != 0 & beta_U == 0 ~ "MNAR1",
      beta_x1 == 0 & beta_x2 != 0 & beta_U != 0 ~ "MNAR2",
      beta_x1 != 0 & beta_x2 != 0 & beta_U != 0 ~ "MNAR3",
      beta_x1 != 0 & beta_x2 == 0 & beta_U != 0 ~ "MNAR*", #DAG that we don't consider: Missingness depends on itself and on some unobserved variable
      beta_x1 != 0 & beta_x2 == 0 & beta_U == 0 ~ "MNAR**", #DAG that we don't consider: Missingness depends on itself only 
      beta_x1 == 0 & beta_x2 == 0 & beta_U != 0 ~ "MNAR***", #DAG that we don't consider: Missingness depends on some unobserved variable only
      
      
      TRUE ~ as.character("error")
    )
  )


rm(df_scenario_number)

df_DAG <- df_DAG %>%
  mutate(target_measures = recode(target_measures, 
                                  AUC = 'AUC',
                                  Brier = 'Brier Score',
                                  Cal_Int = 'Calibration Intercept',
                                  Cal_Slope = 'Calibration Slope'))

df_DAG$target_measures <- factor(df_DAG$target_measures,
                                 levels = c("AUC", "Calibration Intercept", "Calibration Slope", "Brier Score"))



##############################################################################################################
#(1) split data into val and imp datasets
df_val <- df_DAG[df_DAG$dataset %like% "val", ]
df_imp <- df_DAG[df_DAG$dataset %like% "imp", ]


df_val <- df_val %>% 
  mutate(
    dataset = case_when(
      dataset == "CCA_val_data" ~ "CCA",
      dataset == "MI_val_data_noY" ~ "MI no Y",
      dataset == "MI_val_data_withY" ~ "MI with Y",
      dataset == "mean_val" ~ "Imputed by mean",
      TRUE ~ dataset))


############################################################################################################################################
# M   C   A   R
############################################################################################################################################

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





### MCAR ***MI with Y***
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


rm(MCAR_ALLDATA_BIAS)
rm(MCAR_MEAN_BIAS)
rm(MCAR_MInoY_BIAS)
rm(MCAR_MIwithY_BIAS)

############################################################################################################################################
# M  A  R
############################################################################################################################################

### MAR ***ALL DATA REQUIRED***
MAR_ALLDATA_BIAS <- subset(df_imp, DAG_type == "MAR" & dataset == "all_data_imp") %>%
  rename_at(vars("estimates"), function(x) paste0("true_", x)) %>%
  left_join(df_val, MAR_ALLDATA_BIAS, 
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



### MAR ***MEAN***
MAR_MEAN_BIAS <- subset(df_imp, DAG_type == "MAR" & dataset == "mean_imp") %>%
  rename_at(vars("estimates"), function(x) paste0("true_", x)) %>%
  left_join(df_val, MAR_MEAN_BIAS, 
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





### MAR ***MI no Y***
MAR_MInoY_BIAS <- subset(df_imp, DAG_type == "MAR" & dataset == "MI_imp_data_noY") %>%
  rename_at(vars("estimates"), function(x) paste0("true_", x)) %>%
  left_join(df_val, MAR_MInoY_BIAS, 
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





### MAR ***MI with Y***
MAR_MIwithY_BIAS <- subset(df_imp, DAG_type == "MAR" & dataset == "MI_imp_data_withY") %>%
  rename_at(vars("estimates"), function(x) paste0("true_", x)) %>%
  left_join(df_val, MAR_MIwithY_BIAS, 
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



### COMBINE ALL MAR BIASES INTO 1 DF
MAR_ALL <- MAR_ALLDATA_BIAS %>% mutate("imp_method" = "All data required") %>%
  bind_rows(MAR_MEAN_BIAS %>% mutate("imp_method" = "Imputed by mean")) %>%
  bind_rows(MAR_MInoY_BIAS %>% mutate("imp_method" = "MI without Y")) %>%
  bind_rows(MAR_MIwithY_BIAS %>% mutate("imp_method" = "MI with Y")) 

rm(MAR_ALLDATA_BIAS)
rm(MAR_MEAN_BIAS)
rm(MAR_MInoY_BIAS)
rm(MAR_MIwithY_BIAS)

############################################################################################################################################
# M  N  A  R - X
############################################################################################################################################

### MNARX ***ALL DATA REQUIRED***
MNARX_ALLDATA_BIAS <- subset(df_imp, DAG_type == "MNAR1" & dataset == "all_data_imp") %>%
  rename_at(vars("estimates"), function(x) paste0("true_", x)) %>%
  left_join(df_val, MNARX_ALLDATA_BIAS, 
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



### MNARX ***MEAN***
MNARX_MEAN_BIAS <- subset(df_imp, DAG_type == "MNAR1" & dataset == "mean_imp") %>%
  rename_at(vars("estimates"), function(x) paste0("true_", x)) %>%
  left_join(df_val, MNARX_MEAN_BIAS, 
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





### MNARX ***MI no Y***
MNARX_MInoY_BIAS <- subset(df_imp, DAG_type == "MNAR1" & dataset == "MI_imp_data_noY") %>%
  rename_at(vars("estimates"), function(x) paste0("true_", x)) %>%
  left_join(df_val, MNARX_MInoY_BIAS, 
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





### MNARX ***MI with Y***
MNARX_MIwithY_BIAS <- subset(df_imp, DAG_type == "MNAR1" & dataset == "MI_imp_data_withY") %>%
  rename_at(vars("estimates"), function(x) paste0("true_", x)) %>%
  left_join(df_val, MNARX_MIwithY_BIAS, 
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



### COMBINE ALL MNARX BIASES INTO 1 DF
MNARX_ALL <- MNARX_ALLDATA_BIAS %>% mutate("imp_method" = "All data required") %>%
  bind_rows(MNARX_MEAN_BIAS %>% mutate("imp_method" = "Imputed by mean")) %>%
  bind_rows(MNARX_MInoY_BIAS %>% mutate("imp_method" = "MI without Y")) %>%
  bind_rows(MNARX_MIwithY_BIAS %>% mutate("imp_method" = "MI with Y")) 



rm(MNARX_ALLDATA_BIAS)
rm(MNARX_MEAN_BIAS)
rm(MNARX_MInoY_BIAS)
rm(MNARX_MIwithY_BIAS)

############################################################################################################################################
# M  N  A  R - Y
############################################################################################################################################

### MNARY ***ALL DATA REQUIRED***
MNARY_ALLDATA_BIAS <- subset(df_imp, DAG_type == "MNAR2" & dataset == "all_data_imp") %>%
  rename_at(vars("estimates"), function(x) paste0("true_", x)) %>%
  left_join(df_val, MNARY_ALLDATA_BIAS, 
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



### MNARY ***MEAN***
MNARY_MEAN_BIAS <- subset(df_imp, DAG_type == "MNAR2" & dataset == "mean_imp") %>%
  rename_at(vars("estimates"), function(x) paste0("true_", x)) %>%
  left_join(df_val, MNARY_MEAN_BIAS, 
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





### MNARY ***MI no Y***
MNARY_MInoY_BIAS <- subset(df_imp, DAG_type == "MNAR2" & dataset == "MI_imp_data_noY") %>%
  rename_at(vars("estimates"), function(x) paste0("true_", x)) %>%
  left_join(df_val, MNARY_MInoY_BIAS, 
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





### MNARY ***MI with Y***
MNARY_MIwithY_BIAS <- subset(df_imp, DAG_type == "MNAR2" & dataset == "MI_imp_data_withY") %>%
  rename_at(vars("estimates"), function(x) paste0("true_", x)) %>%
  left_join(df_val, MNARY_MIwithY_BIAS, 
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



### COMBINE ALL MNARY BIASES INTO 1 DF
MNARY_ALL <- MNARY_ALLDATA_BIAS %>% mutate("imp_method" = "All data required") %>%
  bind_rows(MNARY_MEAN_BIAS %>% mutate("imp_method" = "Imputed by mean")) %>%
  bind_rows(MNARY_MInoY_BIAS %>% mutate("imp_method" = "MI without Y")) %>%
  bind_rows(MNARY_MIwithY_BIAS %>% mutate("imp_method" = "MI with Y")) 


rm(MNARY_ALLDATA_BIAS)
rm(MNARY_MEAN_BIAS)
rm(MNARY_MInoY_BIAS)
rm(MNARY_MIwithY_BIAS)


############################################################################################################################################
# M  N  A  R - X Y
############################################################################################################################################



### MNARXY ***ALL DATA REQUIRED***
MNARXY_ALLDATA_BIAS <- subset(df_imp, DAG_type == "MNAR3" & dataset == "all_data_imp") %>%
  rename_at(vars("estimates"), function(x) paste0("true_", x)) %>%
  left_join(df_val, MNARXY_ALLDATA_BIAS, 
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



### MNARXY ***MEAN***
MNARXY_MEAN_BIAS <- subset(df_imp, DAG_type == "MNAR3" & dataset == "mean_imp") %>%
  rename_at(vars("estimates"), function(x) paste0("true_", x)) %>%
  left_join(df_val, MNARXY_MEAN_BIAS, 
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





### MNARXY ***MI no Y***
MNARXY_MInoY_BIAS <- subset(df_imp, DAG_type == "MNAR3" & dataset == "MI_imp_data_noY") %>%
  rename_at(vars("estimates"), function(x) paste0("true_", x)) %>%
  left_join(df_val, MNARXY_MInoY_BIAS, 
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





### MNARXY ***MI with Y***
MNARXY_MIwithY_BIAS <- subset(df_imp, DAG_type == "MNAR3" & dataset == "MI_imp_data_withY") %>%
  rename_at(vars("estimates"), function(x) paste0("true_", x)) %>%
  left_join(df_val, MNARXY_MIwithY_BIAS, 
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



### COMBINE ALL MNARXY BIASES INTO 1 DF
#NB: data frames too big to run, so save them as csv one by one and then combine (wd:25x4)
#MNARXY_ALLDATA_BIAS <- read.csv("MNARXY_ALLDATA_BIAS.csv")
#MNARXY_MEAN_BIAS <- read.csv("MNARXY_MEAN_BIAS.csv")
#MNARXY_MInoY_BIAS <- read.csv("MNARXY_MInoY_BIAS.csv")
#MNARXY_MIwithY_BIAS <- read.csv("MNARXY_MIwithY_BIAS.csv")


#NB2: change factor levels of target measures
MNARXY_ALL <- MNARXY_ALLDATA_BIAS %>% mutate("imp_method" = "All data required") %>%
  bind_rows(MNARXY_MEAN_BIAS %>% mutate("imp_method" = "Imputed by mean")) %>%
  bind_rows(MNARXY_MInoY_BIAS %>% mutate("imp_method" = "MI without Y")) %>%
  bind_rows(MNARXY_MIwithY_BIAS %>% mutate("imp_method" = "MI with Y")) 


rm(MNARXY_ALLDATA_BIAS)
rm(MNARXY_MEAN_BIAS)
rm(MNARXY_MInoY_BIAS)
rm(MNARXY_MIwithY_BIAS)


# PLOTS
############################################################################################################################################

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

setwd("~/AntoniaPhD/2. RSim/RESULTScontinuous/plots_new")

#MCAR_ALL: Y = R1 = 0.5
plot_scenario("5846+5846") #MCAR + MCAR
plot_scenario("5846+5927") #MCAR + MAR
plot_scenario("5846+6170") #MCAR + MNARX
plot_scenario("5846+5954") #MCAR + MNARY
plot_scenario("5846+6197") #MCAR + MNARXY
#MCAR_ALL: Y = 0.1, R1 = 0.5
plot_scenario("1472+1472") #MCAR + MCAR
plot_scenario("1472+1553") #MCAR + MAR
plot_scenario("1472+1796") #MCAR + MNARX
plot_scenario("1472+1580") #MCAR + MNARY
plot_scenario("1472+1823") #MCAR + MNARXY





#MAR_ALL: Y = R1 = 0.5
plot_scenario("5927+5927") #MAR + MAR
plot_scenario("5927+5846") #MAR + MCAR
plot_scenario("5927+6170") #MAR + MNARX
plot_scenario("5927+5954") #MAR + MNARY
plot_scenario("5927+6197") #MAR + MNARXY
#MAR_ALL: Y = 0.1; R1 = 0.5
plot_scenario("1553+1553") #MAR + MAR
plot_scenario("1553+1472") #MAR + MCAR
plot_scenario("1553+1796") #MAR + MNARX
plot_scenario("1553+1580") #MAR + MNARY
plot_scenario("1553+1823") #MAR + MNARXY




#MNARX_ALL: Y = R1 = 0.5
plot_scenario("6170+6170") #MNARX + MNARX
plot_scenario("6170+5846") #MNARX + MCAR
plot_scenario("6170+5927") #MNARX + MAR
plot_scenario("6170+5954") #MNARX + MNARY
plot_scenario("6170+6197") #MNARX + MNARXY
#MNARX_ALL: Y = 01, R1 = 0.5
plot_scenario("1796+1796") #MNARX + MNARX
plot_scenario("1796+1472") #MNARX + MCAR
plot_scenario("1796+1553") #MNARX + MAR
plot_scenario("1796+1580") #MNARX + MNARY
plot_scenario("1796+1823") #MNARX + MNARXY



#MNARY_ALL: Y = R1 = 0.5
plot_scenario("5954+5954") #MNARY + MNARY
plot_scenario("5954+5846") #MNARY + MCAR
plot_scenario("5954+5927") #MNARY + MAR
plot_scenario("5954+6170") #MNARY + MNARX
plot_scenario("5954+6197") #MNARY + MNARXY
#MNARY_ALL: Y = 0.1, R1 = 0.5
plot_scenario("1580+1580") #MNARY + MNARY
plot_scenario("1580+1472") #MNARY + MCAR
plot_scenario("1580+1553") #MNARY + MAR
plot_scenario("1580+1796") #MNARY + MNARX
plot_scenario("1580+1823") #MNARY + MNARXY




#MNARXY_ALL: Y = R1 = 0.5
plot_scenario("6197+6197") #MNARXY + MNARXY
plot_scenario("6197+5846") #MNARXY + MCAR
plot_scenario("6197+5927") #MNARXY + MAR
plot_scenario("6197+6170") #MNARXY + MNARX
plot_scenario("6197+5954") #MNARXY + MNARXY
#MNARXY_ALL Y = 0.1
plot_scenario("1823+1823") #MNARXY + MNARXY
plot_scenario("1823+1472") #MNARXY + MCAR
plot_scenario("1823+1553") #MNARXY + MAR
plot_scenario("1823+1796") #MNARXY + MNARX
plot_scenario("1823+1580") #MNARXY + MNARXY

######

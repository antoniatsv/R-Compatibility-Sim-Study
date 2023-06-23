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


# (2) subset each imp dataset at a time 
imp_all_data <- subset(df_imp, dataset == "all_data_imp") %>%
  rename_at(.vars = vars("estimates"), .funs = function(x) paste0('true_', x))

imp_MI_withY <- subset(df_imp, dataset == "MI_imp_data_withY") %>%
  rename_at(.vars = vars("estimates"), .funs = function(x) paste0('true_', x))

imp_MI_noY <- subset(df_imp, dataset == "MI_imp_data_noY") %>%
  rename_at(.vars = vars("estimates"), .funs = function(x) paste0('true_', x))

imp_mean <- subset(df_imp, dataset == "mean_imp") %>%
  rename_at(.vars = vars("estimates"), .funs = function(x) paste0('true_', x))

# (3) left_join val data + each of the subset imp data  and calculate bias

joined_imp_all_data <- left_join(df_val, imp_all_data, by = c("Iteration", "mod", "scenario_number", "target_measures")) %>% 
  mutate(bias = estimates - true_estimates)

joined_imp_MI_withY <- left_join(df_val, imp_MI_withY, by = c("Iteration", "mod", "scenario_number", "target_measures")) %>% 
  mutate(bias = estimates - true_estimates)

joined_imp_MI_noY <- left_join(df_val, imp_MI_noY, by = c("Iteration", "mod", "scenario_number", "target_measures")) %>% 
  mutate(bias = estimates - true_estimates)

joined_imp_mean <- left_join(df_val, imp_mean, by = c("Iteration", "mod", "scenario_number", "target_measures")) %>% 
  mutate(bias = estimates - true_estimates)


rm(df_imp)
rm(df_val)

##############################################################################################################
# Plot biases 

# Before I make the plots, I have to group by dataset, scenario_number and then summarise all target measures estimates and calculate CIs 

# summary of biases for all data required at implementation when compared to all validation imputed datasets
bias_imp_all_data <- joined_imp_all_data %>% 
  group_by(dataset.x, 
           scenario_number, 
           target_measures, 
           N.x, 
           N_dev.x, 
           N_val.x, 
           N_imp.x, 
           Y_prev.x, 
           R_prev.x, 
           X_categorical.x,
           beta_x1.x,
           beta_x2.x,
           beta_U.x, 
           gamma_x1.x, 
           gamma_x2.x, 
           gamma_U.x,
           DAG_type.x,
           DAG_type.y) %>% 
  
  summarise(bias_mean = mean(bias), LCI = quantile(bias, 0.025), UCI = quantile(bias, 0.975))

# summary of biases for mean imputation at implementation when compared to all validation imputed datasets
bias_imp_mean <- joined_imp_mean %>% 
  group_by(dataset.x, 
           scenario_number, 
           target_measures, 
           N.x, 
           N_dev.x, 
           N_val.x, 
           N_imp.x, 
           Y_prev.x, 
           R_prev.x, 
           X_categorical.x,
           beta_x1.x,
           beta_x2.x,
           beta_U.x, 
           gamma_x1.x, 
           gamma_x2.x, 
           gamma_U.x,
           DAG_type.x) %>% 
  
  summarise(bias_mean = mean(bias), LCI = quantile(bias, 0.025), UCI = quantile(bias, 0.975))

# summary of biases for MI_noY at implementation when compared to all validation imputed datasets
bias_imp_MI_noY <- joined_imp_MI_noY %>% 
  group_by(dataset.x, 
           scenario_number, 
           target_measures, 
           N.x, 
           N_dev.x, 
           N_val.x, 
           N_imp.x, 
           Y_prev.x, 
           R_prev.x, 
           X_categorical.x,
           beta_x1.x,
           beta_x2.x,
           beta_U.x, 
           gamma_x1.x, 
           gamma_x2.x, 
           gamma_U.x,
           DAG_type.x) %>% 
  
  summarise(bias_mean = mean(bias), LCI = quantile(bias, 0.025), UCI = quantile(bias, 0.975))


# summary of biases for MI_withY at implementation when compared to all validation imputed datasets
bias_imp_MI_withY <- joined_imp_MI_withY %>% 
  group_by(dataset.x, 
           scenario_number, 
           target_measures, 
           N.x, 
           N_dev.x, 
           N_val.x, 
           N_imp.x, 
           Y_prev.x, 
           R_prev.x, 
           X_categorical.x,
           beta_x1.x,
           beta_x2.x,
           beta_U.x, 
           gamma_x1.x, 
           gamma_x2.x, 
           gamma_U.x,
           DAG_type.x) %>% 
  
  summarise(bias_mean = mean(bias), LCI = quantile(bias, 0.025), UCI = quantile(bias, 0.975))



##############################################################################################################################
################## BIAS PLOTS ################################################################################################
##############################################################################################################################

bias_all <- bias_imp_all_data %>% mutate("imp_method" = "All data required") %>%
  bind_rows(bias_imp_mean %>% mutate("imp_method" = "Imputed by mean")) %>%
  bind_rows(bias_imp_MI_withY %>% mutate("imp_method" = "MI with Y")) %>% 
  bind_rows(bias_imp_MI_noY %>% mutate("imp_method" = "MI without Y"))


bias_all <- bias_all %>%
  mutate(dataset.x = recode(dataset.x, 
                            CCA_val_data = 'CCA',
                            MI_val_data_noY = 'MI no Y',
                            MI_val_data_withY = 'MI with Y',
                            mean_val = 'Imputed by mean'))



bias_all <- bias_all %>%
  mutate(target_measures = recode(target_measures, 
                                  AUC = 'AUC',
                                  Brier = 'Brier Score',
                                  Cal_Int = 'Calibration Intercept',
                                  Cal_Slope = 'Calibration Slope'))

bias_all$target_measures <- factor(bias_all$target_measures,
                                   levels = c("AUC", "Calibration Intercept", "Calibration Slope", "Brier Score"))

df_DAG <- df_DAG %>%
  mutate(dataset = recode(dataset, 
                          CCA_val_data = 'CCA at Validation',
                          all_data_imp = 'All data required at Implementation',
                          MI_val_data_noY = 'MI no Y at Validation',
                          MI_imp_data_noY = 'MI no Y at Implementation',
                          MI_val_data_withY = 'MI with Y at Validation',
                          MI_imp_data_withY = 'MI with Y at Implementation',
                          mean_val = 'Imputed by mean at Validation',
                          mean_imp = 'Imputed by mean at Implementation',
                          all_data_imp = 'All data required at Implementation'))


plot_scenario <- function(sn) {
  print(sn)
  bias_all = bias_all
  
  plot <- ggplot(data = bias_all %>%
                   filter(scenario_number == sn), 
                 aes(x = bias_mean, y = dataset.x, color = factor(target_measures),
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




plot_scenario(5846)  # Generates the MCAR plot
plot_scenario(5927)  # Generates the MAR plot
plot_scenario(6170)  # Generates the MNAR-X plot
plot_scenario(5954)  # Generates the MNAR-Y plot
plot_scenario(6197)  # Generates the MNAR-XY plot



##############################################################################################################################
########################################## METRICS PLOTS - APPENDIX ##########################################################
##############################################################################################################################


plot_metrics <- function(sn) {
  ggplot(data = df_DAG %>%
           filter(scenario_number == sn) %>%
           group_by(dataset, target_measures) %>%
           summarise(mean_estimates = mean(estimates),
                     min_estimates = min(estimates),
                     max_estimates = max(estimates)),
         aes(x = mean_estimates, y = dataset, color = factor(target_measures))) + 
    geom_errorbar(aes(xmin = min_estimates, xmax = max_estimates), width=.1) +
    geom_point(size = 3, stroke = 0.5) +
    guides(color = guide_legend(reverse = TRUE)) + 
    scale_shape_manual(values = c(8, 17, 16, 15)) +
    scale_color_brewer(palette = "Set1") +
    xlab("Mean Estimates") +
    ylab("Data Imputation Methods") +
    theme_minimal() +
    theme(legend.position = "none",
          axis.text = element_text(size=14),
          axis.title = element_text(size=16, face="bold"),
          axis.text.x = element_text(size=14),
          axis.text.y = element_text(size=14),
          strip.text = element_text(size = 16),
          panel.background = element_rect(fill = "gray90"),  # add background color to panels
          panel.spacing.x = unit(0.5, "lines")) +  # increase space between panels
    ggh4x::facet_grid2(~  target_measures, scales = "free_x", independent = "x")
}


plot_metrics(5846)  # Generates the MCAR plot
plot_metrics(5927)  # Generates the MAR plot
plot_metrics(6170)  # Generates the MNAR-X plot
plot_metrics(5954)  # Generates the MNAR-Y plot
plot_metrics(6197)  # Generates the MNAR-XY plot





#####################################################################################################################
#####################################################################################################################
#####################################################################################################################




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

#create a new column dataset_combined
joined_imp_MCAR_alldata$dataset_combined = paste(joined_imp_MCAR_alldata$dataset.x, joined_imp_MCAR_alldata$dataset.y, sep = "+")

#create a new column scenario_combined 
joined_imp_MCAR_alldata$scenario_combined = paste(joined_imp_MCAR_alldata$scenario_number.x, joined_imp_MCAR_alldata$scenario_number.y, sep = "+")

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

BIAS_MCAR_ALLDATA$plot_number <- 1:nrow(BIAS_MCAR_ALLDATA)


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

##test

=======
#test branch
>>>>>>> 5325ce017b7f23a4f0a44c4a912f61e8e4dd4bef

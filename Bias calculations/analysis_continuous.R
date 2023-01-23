library(tidyverse)
library(magrittr)
library(dplyr)
library(ggplot2)
library(data.table)

# Iterations 1-25
##############################################################################################################
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
##############################################################################################################
filenames <- list.files(pattern = ".rds") 


df_2 <- list.files(pattern = ".rds") %>%
  map_dfr(readRDS) 

#extract the scenario numbers from the rds files names (note 1 rds is missing?)
scenario_number <- as.numeric(str_extract(filenames, pattern = "[0-9]+"))

df_2$Iteration <- df_2$Iteration + 25 

test_2 <- c()

for(i in 1:length(scenario_number)){ test_2 <- c(test_2, rep(scenario_number[i],900) ) }

df_2$scenario_number <- test_2




# Iterations 51 - 75
##############################################################################################################
# C H A N G E   D I R E C T O R Y 
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
#pivot_longer to tidy up the data 
rm(df_1)
rm(df_2)
rm(df_3)
rm(df_4)
rm(joined_imp_all_data)
rm(joined_imp_mean)
rm(joined_imp_MI_noY)
rm(joined_imp_all_data)
rm(joined_imp_MI_withY)
rm(df_all_iter)
rm(df_scenario_number)
rm(df_long)
rm(df_DAG)


df_long <- pivot_longer(df_all_iter, 4:7, names_to = "target_measures", values_to = "estimates") %>%
  filter(mod == 1)


sims_parameters <- readRDS(file = "sims_parameters.RDS") %>%
  mutate(scenario_number = 1:n())


df_scenario_number <- left_join(df_long, sims_parameters, by = "scenario_number") 


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
rm(imp_all_data)
rm(imp_mean)
rm(imp_MI_noY)
rm(imp_MI_withY)
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
           DAG_type.x) %>% 
  
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



##########################################
################## BIAS PLOTS ############
##########################################

bias_all <- bias_imp_all_data %>% mutate("imp_method" = "all required") %>%
  bind_rows(bias_imp_mean %>% mutate("imp_method" = "imputed by mean")) %>%
  bind_rows(bias_imp_MI_withY %>% mutate("imp_method" = "imputed by MI with Y")) %>% 
  bind_rows(bias_imp_MI_noY %>% mutate("imp_method" = "imputed by MI no Y"))

################### MISSING COMPLETELY AT RANDOM  #######################
MCAR <- ggplot(data = bias_all %>%
                 filter(scenario_number == 14 | scenario_number == 743 | scenario_number == 1472), 
               aes(x = bias_mean, y = dataset.x, color = factor(scenario_number))) + 
  geom_errorbar(aes(xmin = LCI, xmax = UCI), width=.1) +
  geom_point() +
  scale_color_discrete(name = "% of\n missingness", 
                       labels = c("10", "20", "50")) + 
  geom_vline(xintercept = 0, linetype="dotted") + 
  ggtitle("Bias under MCAR, Prevalence of Y = 0.1, gammas = 0.5") +
  theme(plot.title = element_text(hjust = 0.5),
        axis.title.y = element_blank()) +
  ggh4x::facet_grid2(target_measures ~ imp_method, scales = "free_x", independent = "x")

MCAR

ggsave("MCAR.tiff", width = 12, height = 9)
################### MISSING AT RANDOM #######################
MAR <- ggplot(data = bias_all %>%
                  filter(scenario_number == 95 | scenario_number == 824 | scenario_number == 1553), 
                aes(x = bias_mean, y = dataset.x, color = factor(scenario_number))) + 
  geom_errorbar(aes(xmin = LCI, xmax = UCI), width=.1) +
  geom_point() +
  scale_color_discrete(name = "% of\n missingness", 
                       labels = c("10", "20", "50")) + 
  geom_vline(xintercept = 0, linetype="dotted") + 
  ggtitle("Bias under MAR, Prevalence of Y = 0.1, gammas = 0.5") +
  theme(plot.title = element_text(hjust = 0.5),
        axis.title.y = element_blank()) +
  ggh4x::facet_grid2(target_measures ~ imp_method, scales = "free_x", independent = "x")

MAR

ggsave("MAR.tiff", width = 12, height = 9)
################### MISSING NOT AT RANDOM 1 #######################
MNAR1 <- ggplot(data = bias_all %>%
                  filter(scenario_number == 338 | scenario_number == 1067 | scenario_number == 1796), 
                aes(x = bias_mean, y = dataset.x, color = factor(scenario_number))) + 
  geom_errorbar(aes(xmin = LCI, xmax = UCI), width=.1) +
  geom_point() +
  scale_color_discrete(name = "% of\n missingness", 
                       labels = c("10", "20", "50")) + 
  geom_vline(xintercept = 0, linetype="dotted") + 
  ggtitle("Bias under MNAR1, Prevalence of Y = 0.1, gammas = 0.5") +
  theme(plot.title = element_text(hjust = 0.5),
        axis.title.y = element_blank()) +
  ggh4x::facet_grid2(target_measures ~ imp_method, scales = "free_x", independent = "x")

MNAR1

ggsave("MNAR1.tiff", width = 12, height = 9)

################### MISSING NOT AT RANDOM 2 #######################
MNAR2 <- ggplot(data = bias_all %>%
                  filter(scenario_number == 122 | scenario_number == 851 | scenario_number == 1580), 
                aes(x = bias_mean, y = dataset.x, color = factor(scenario_number))) + 
  geom_errorbar(aes(xmin = LCI, xmax = UCI), width=.1) +
  geom_point() +
  scale_color_discrete(name = "% of\n missingness", 
                       labels = c("10", "20", "50")) + 
  geom_vline(xintercept = 0, linetype="dotted") + 
  ggtitle("Bias under MNAR2, Prevalence of Y = 0.1, gammas = 0.5") +
  theme(plot.title = element_text(hjust = 0.5),
        axis.title.y = element_blank()) +
  ggh4x::facet_grid2(target_measures ~ imp_method, scales = "free_x", independent = "x")

MNAR2

ggsave("MNAR2.tiff", width = 12, height = 9)
################### MISSING NOT AT RANDOM 3 #######################

#bias_all %>%
 # ungroup() %>%
  #filter(scenario_number == 365 | scenario_number == 1094 | scenario_number == 1823) %>%
  #filter(target_measures == "Cal_Slope") %>%
  #filter(dataset.x == "MI_val_data_noY") %>%
  #select(imp_method, scenario_number, bias_mean)


##################################################################################################################
MNAR3 <- ggplot(data = bias_all %>%
                  filter(scenario_number == 365 | scenario_number == 1094 | scenario_number == 1823), 
                aes(x = bias_mean, y = dataset.x, color = factor(scenario_number))) + 
  geom_errorbar(aes(xmin = LCI, xmax = UCI), width=.1) +
  geom_point() +
  scale_color_discrete(name = "% of\n missingness", 
                     labels = c("10", "20", "50")) + 
  geom_vline(xintercept = 0, linetype="dotted") + 
  ggtitle(expression("Bias under MNAR3, Prevalence of Y = 0.1, gamma = 0.5")) +
  theme(plot.title = element_text(hjust = 0.5),
        axis.title.y = element_blank()) +
  ggh4x::facet_grid2(target_measures ~ imp_method, scales = "free_x", independent = "x")
  
MNAR3


#### Save plots for Y = 0.1, gammas = 0.5

ggsave("MNAR3.tiff", width = 12, height = 9)




##################################################################################################################
##################################################################################################################
######################################### PLOTS for Y_prev = 50% #################################################
##################################################################################################################
##################################################################################################################


MCAR_Y_prev_50 <- ggplot(data = bias_all %>%
                 filter(scenario_number == 4388 | scenario_number == 5117 | scenario_number == 5846), 
               aes(x = bias_mean, y = dataset.x, color = factor(scenario_number))) + 
  geom_errorbar(aes(xmin = LCI, xmax = UCI), width=.1) +
  geom_point() +
  scale_color_discrete(name = "% of\n missingness", 
                       labels = c("10", "20", "50")) + 
  geom_vline(xintercept = 0, linetype="dotted") + 
  ggtitle("Bias under MCAR, Prevalence of Y = 0.5, gammas = 0.5") +
  theme(plot.title = element_text(hjust = 0.5),
        axis.title.y = element_blank()) +
  ggh4x::facet_grid2(target_measures ~ imp_method, scales = "free_x", independent = "x")

MCAR_Y_prev_50

ggsave("MCAR_Y_prev_50.tiff", width = 12, height = 9)

################### MISSING AT RANDOM #######################
MAR_Y_prev_50 <- ggplot(data = bias_all %>%
                filter(scenario_number == 4469 | scenario_number == 5198 | scenario_number == 5927), 
              aes(x = bias_mean, y = dataset.x, color = factor(scenario_number))) + 
  geom_errorbar(aes(xmin = LCI, xmax = UCI), width=.1) +
  geom_point() +
  scale_color_discrete(name = "% of\n missingness", 
                       labels = c("10", "20", "50")) + 
  geom_vline(xintercept = 0, linetype="dotted") + 
  ggtitle("Bias under MAR, Prevalence of Y = 0.5, gammas = 0.5") +
  theme(plot.title = element_text(hjust = 0.5),
        axis.title.y = element_blank()) +
  ggh4x::facet_grid2(target_measures ~ imp_method, scales = "free_x", independent = "x")

MAR_Y_prev_50

ggsave("MAR_Y_prev_50.tiff", width = 12, height = 9)

################### MISSING NOT AT RANDOM 1 #######################
MNAR1_Y_prev_50 <- ggplot(data = bias_all %>%
                  filter(scenario_number == 4712 | scenario_number == 5441 | scenario_number == 6170), 
                aes(x = bias_mean, y = dataset.x, color = factor(scenario_number))) + 
  geom_errorbar(aes(xmin = LCI, xmax = UCI), width=.1) +
  geom_point() +
  scale_color_discrete(name = "% of\n missingness", 
                       labels = c("10", "20", "50")) + 
  geom_vline(xintercept = 0, linetype="dotted") + 
  ggtitle("Bias under MNAR1, Prevalence of Y = 0.5, gammas = 0.5") +
  theme(plot.title = element_text(hjust = 0.5),
        axis.title.y = element_blank()) +
  ggh4x::facet_grid2(target_measures ~ imp_method, scales = "free_x", independent = "x")

MNAR1_Y_prev_50 

ggsave("MNAR1_Y_prev_50 .tiff", width = 12, height = 9)

################### MISSING NOT AT RANDOM 2 #######################
MNAR2_Y_prev_50 <- ggplot(data = bias_all %>%
                  filter(scenario_number == 4496 | scenario_number == 5225 | scenario_number == 5954), 
                aes(x = bias_mean, y = dataset.x, color = factor(scenario_number))) + 
  geom_errorbar(aes(xmin = LCI, xmax = UCI), width=.1) +
  geom_point() +
  scale_color_discrete(name = "% of\n missingness", 
                       labels = c("10", "20", "50")) + 
  geom_vline(xintercept = 0, linetype="dotted") + 
  ggtitle("Bias under MNAR2, Prevalence of Y = 0.5, gammas = 0.5") +
  theme(plot.title = element_text(hjust = 0.5),
        axis.title.y = element_blank()) +
  ggh4x::facet_grid2(target_measures ~ imp_method, scales = "free_x", independent = "x")

MNAR2_Y_prev_50

ggsave("MNAR2_Y_prev_50.tiff", width = 12, height = 9)

##################################################################################################################
MNAR3_Y_prev_50 <- ggplot(data = bias_all %>%
                  filter(scenario_number == 4739 | scenario_number == 5468 | scenario_number == 6197), 
                aes(x = bias_mean, y = dataset.x, color = factor(scenario_number))) + 
  geom_errorbar(aes(xmin = LCI, xmax = UCI), width=.1) +
  geom_point() +
  scale_color_discrete(name = "% of\n missingness", 
                       labels = c("90", "80", "50")) + 
  geom_vline(xintercept = 0, linetype="dotted") + 
  ggtitle(expression("Bias under MNAR3, Prevalence of Y = 0.5, gamma = 0.5")) +
  theme(plot.title = element_text(hjust = 0.5),
        axis.title.y = element_blank()) +
  ggh4x::facet_grid2(target_measures ~ imp_method, scales = "free_x", independent = "x")

MNAR3_Y_prev_50


#### Save plots for Y = 0.1, gammas = 0.5

ggsave("MNAR3_Y_prev_50.tiff", width = 12, height = 9)

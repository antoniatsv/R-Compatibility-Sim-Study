
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

df_all_iter <- df_all_iter[, -c(5,7,9,11)]

##############################################################################################################
#pivot_longer to tidy up the data 

df_long <- pivot_longer(df_all_iter, 4:7, names_to = "target_measures", values_to = "estimates")



##############################################################################################################
#(1) split data into val and imp datasets
df_val <- df_long[df_long$dataset %like% "val", ]
df_imp <- df_long[df_long$dataset %like% "imp", ]

df_val <- df_val %>%
  filter(mod == 1)

df_imp <- df_imp %>%
  filter(mod == 1)

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


##############################################################################################################
# Plot biases 

# Before I make the plots, I have to group by dataset, scenario_number and then summarise all target measures estimates and calculate CIs 
summary_test <- joined_imp_all_data %>% 
  group_by(dataset, scenario_number, target_measures) %>% 
  summarise(estimates_mean = mean(estimates), LCI = quantile(estimates, 0.025), UCI = quantile(estimates, 0.975))

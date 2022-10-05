# #######################################################################################################################

# Summary and analysis of the simulation results for n_iter = 10
# Author of code: Antonia D. Tsvetanova


# #######################################################################################################################


# A figure for each of the 5 DAG scenarios where: 
# gammas are fixed at 0.5
# betas are fixed at 0.5
# missingness (R1) is fixed at 50% 
# prevalence of Y is fixed at 0.1 


# #######################
# MCAR -> scenario 1472
# MAR -> scenario 1553
# MNAR1 -> scenario 1796
# MNAR2 -> scenario 1580
# MNAR3 -> scenario 1823
# #########################



install.packages("ggplot2")
install.packages("magrittr")
install.packages("dplyr")
install.packages("tidyverse")


library(ggplot2)
library(magrittr)
library(dplyr)
library(tidyverse)

# ####################################################################################################
#Scenario corresponding to MCAR DAG with fixed Y_prev = 0.1, R_prev = 0.5, gammas = 0.5, (betas = 0)
MCAR_1472 <- readRDS("Results_1472.RDS")
# ####################################################################################################

MCAR_1472_DAG_added <- MCAR_1472 %>%
  add_column(DAG_type = "MCAR")

# #######################
# MODEL 1 AUC
# #######################
#first summarise the AUC (+CIs) for all models and all datasets 
MCAR_AUC <- MCAR_1472_DAG_added %>% 
  group_by(dataset, mod, DAG_type) %>% #group by dataset and model
  summarise(AUC_mean = mean(AUC),  #get a summary of the mean AUC + CIs 
            LCI = quantile(AUC, 0.025), 
            UCI = quantile(AUC, 0.975))

MOD1_MCAR_AUC <- MCAR_AUC %>% 
  filter(mod == 1) #get AUCs only for model 1

MOD1_MCAR_AUC <- MOD1_MCAR_AUC[-2,] #exclude the CCA_imp_data because we'll be only comparing CCA_val + all_data_imp

#Plot AUC_mean for all 8 datasets for MODEL 1
ggplot(MOD1_MCAR_AUC, aes(x = AUC_mean, y = dataset)) + geom_point() +    #plot AUC against datasets 
  geom_segment(aes(x = LCI, y = dataset, xend = UCI, yend = dataset))  


# #######################
# MODEL 2 AUC
# #######################
MOD2_MCAR_AUC <- MCAR_AUC %>% 
  filter(mod == 2) 

MOD2_MCAR_AUC <- MOD2_MCAR_AUC[-2,] 

ggplot(MOD2_MCAR_AUC, aes(x = AUC_mean, y = dataset)) + geom_point() +     
  geom_segment(aes(x = LCI, y = dataset, xend = UCI, yend = dataset))  


# #######################
# MODEL 3 AUC
# #######################
MOD3_MCAR_AUC <- MCAR_AUC %>% 
  filter(mod == 3) 

MOD3_MCAR_AUC <- MOD3_MCAR_AUC[-2,] 

ggplot(MOD3_MCAR_AUC, aes(x = AUC_mean, y = dataset)) + geom_point() +    
  geom_segment(aes(x = LCI, y = dataset, xend = UCI, yend = dataset))  



# #######################
# MODEL 4 AUC
# #######################
MOD4_MCAR_AUC <- MCAR_AUC %>% 
  filter(mod == 4) 

MOD4_MCAR_AUC <- MOD4_MCAR_AUC[-2,] 

ggplot(MOD4_MCAR_AUC, aes(x = AUC_mean, y = dataset)) + geom_point() +    
  geom_segment(aes(x = LCI, y = dataset, xend = UCI, yend = dataset))  



# #######################
# ALL MODELS
# #######################
MCAR_all_models_AUC <- rbind(MOD1_MCAR_AUC, MOD2_MCAR_AUC, MOD3_MCAR_AUC, MOD4_MCAR_AUC)
ggplot(MCAR_all_models_AUC, aes(x=AUC_mean, y = dataset)) + geom_point() +
  geom_segment(aes(x=LCI, xend = UCI, y = dataset, yend = dataset)) +
  ggtitle("MCAR: Y_prev = 0.1, R_prev = 0.5, gammas = 0.5, betas = 0") +
  facet_grid(~mod)



# ####################################################################################################
#Scenario corresponding to MAR DAG with fixed Y_prev = 0.1, R_prev = 0.5, gammas = 0.5, beta_X2 = 0.5
MAR_1553 <- readRDS("Results_1553.RDS")
# ####################################################################################################

MAR_1553_DAG_added <- MAR_1553 %>%
  add_column(DAG_type = "MAR")

# #######################
# MODEL 1 AUC
# #######################
#first summarise the AUC (+CIs) for all models and all datasets 
MAR_AUC <- MAR_1553_DAG_added %>% 
  group_by(dataset, mod, DAG_type) %>% #group by dataset and model
  summarise(AUC_mean = mean(AUC),  #get a summary of the mean AUC + CIs 
            LCI = quantile(AUC, 0.025), 
            UCI = quantile(AUC, 0.975))

MOD1_MAR_AUC <- MAR_AUC %>% 
  filter(mod == 1) #get AUCs only for model 1

MOD1_MAR_AUC <- MOD1_MAR_AUC[-2,] #exclude the CCA_imp_data because we'll be only comparing CCA_val + all_data_imp

#Plot AUC_mean for all 8 datasets for MODEL 1
ggplot(MOD1_MAR_AUC, aes(x = AUC_mean, y = dataset)) + geom_point() +    #plot AUC against datasets 
  geom_segment(aes(x = LCI, y = dataset, xend = UCI, yend = dataset))  


# #######################
# MODEL 2 AUC
# #######################
MOD2_MAR_AUC <- MAR_AUC %>% 
  filter(mod == 2) 

MOD2_MAR_AUC <- MOD2_MAR_AUC[-2,] 

ggplot(MOD2_MAR_AUC, aes(x = AUC_mean, y = dataset)) + geom_point() +     
  geom_segment(aes(x = LCI, y = dataset, xend = UCI, yend = dataset))  


# #######################
# MODEL 3 AUC
# #######################
MOD3_MAR_AUC <- MAR_AUC %>% 
  filter(mod == 3) 

MOD3_MAR_AUC <- MOD3_MAR_AUC[-2,] 

ggplot(MOD3_MAR_AUC, aes(x = AUC_mean, y = dataset)) + geom_point() +    
  geom_segment(aes(x = LCI, y = dataset, xend = UCI, yend = dataset))  



# #######################
# MODEL 4 AUC
# #######################
MOD4_MAR_AUC <- MAR_AUC %>% 
  filter(mod == 4) 

MOD4_MAR_AUC <- MOD4_MAR_AUC[-2,] 

ggplot(MOD4_MAR_AUC, aes(x = AUC_mean, y = dataset)) + geom_point() +    
  geom_segment(aes(x = LCI, y = dataset, xend = UCI, yend = dataset))  



# #######################
# ALL MODELS
# #######################
MAR_all_models_AUC <- rbind(MOD1_MAR_AUC, MOD2_MAR_AUC, MOD3_MAR_AUC, MOD4_MAR_AUC)
ggplot(MAR_all_models_AUC, aes(x=AUC_mean, y = dataset)) + geom_point() +
  geom_segment(aes(x=LCI, xend = UCI, y = dataset, yend = dataset)) +
  ggtitle("MAR: Y_prev = 0.1, R_prev = 0.5, gammas = 0.5, beta_X2 = 0.5") +
  facet_grid(~mod)



# ########################################################################################################################
#Scenario corresponding to MNAR1 DAG with fixed Y_prev = 0.1, R_prev = 0.5, gammas = 0.5, beta_X1 = 0.5, beta_X2 = 0.5
MNAR1_1796 <- readRDS("Results_1796.RDS")
# ########################################################################################################################


MNAR1_1796_DAG_added <- MNAR1_1796 %>%
  add_column(DAG_type = "MNAR1")

# #######################
# MODEL 1 AUC
# #######################
#first summarise the AUC (+CIs) for all models and all datasets 
MNAR1_AUC <- MNAR1_1796_DAG_added %>% 
  group_by(dataset, mod, DAG_type) %>% #group by dataset and model
  summarise(AUC_mean = mean(AUC),  #get a summary of the mean AUC + CIs 
            LCI = quantile(AUC, 0.025), 
            UCI = quantile(AUC, 0.975))

MOD1_MNAR1_AUC <- MNAR1_AUC %>% 
  filter(mod == 1) #get AUCs only for model 1

MOD1_MNAR1_AUC <- MOD1_MNAR1_AUC[-2,] #exclude the CCA_imp_data because we'll be only comparing CCA_val + all_data_imp

#Plot AUC_mean for all 8 datasets for MODEL 1
ggplot(MOD1_MNAR1_AUC, aes(x = AUC_mean, y = dataset)) + geom_point() +    #plot AUC against datasets 
  geom_segment(aes(x = LCI, y = dataset, xend = UCI, yend = dataset))  


# #######################
# MODEL 2 AUC
# #######################
MOD2_MNAR1_AUC <- MNAR1_AUC %>% 
  filter(mod == 2) 

MOD2_MNAR1_AUC <- MOD2_MNAR1_AUC[-2,] 

ggplot(MOD2_MNAR1_AUC, aes(x = AUC_mean, y = dataset)) + geom_point() +     
  geom_segment(aes(x = LCI, y = dataset, xend = UCI, yend = dataset))  


# #######################
# MODEL 3 AUC
# #######################
MOD3_MNAR1_AUC <- MNAR1_AUC %>% 
  filter(mod == 3) 

MOD3_MNAR1_AUC <- MOD3_MNAR1_AUC[-2,] 

ggplot(MOD3_MNAR1_AUC, aes(x = AUC_mean, y = dataset)) + geom_point() +    
  geom_segment(aes(x = LCI, y = dataset, xend = UCI, yend = dataset))  



# #######################
# MODEL 4 AUC
# #######################
MOD4_MNAR1_AUC <- MNAR1_AUC %>% 
  filter(mod == 4) 

MOD4_MNAR1_AUC <- MOD4_MNAR1_AUC[-2,] 

ggplot(MOD4_MNAR1_AUC, aes(x = AUC_mean, y = dataset)) + geom_point() +    
  geom_segment(aes(x = LCI, y = dataset, xend = UCI, yend = dataset))  



# #######################
# ALL MODELS
# #######################
MNAR1_all_models_AUC <- rbind(MOD1_MNAR1_AUC, MOD2_MNAR1_AUC, MOD3_MNAR1_AUC, MOD4_MNAR1_AUC)
ggplot(MNAR1_all_models_AUC, aes(x=AUC_mean, y = dataset)) + geom_point() +
  geom_segment(aes(x=LCI, xend = UCI, y = dataset, yend = dataset)) +
  ggtitle("MNAR1: Y_prev = 0.1, R_prev = 0.5, gammas = 0.5, beta_X1 = 0.5, beta_X2 = 0.5") +
  facet_grid(~mod)


# ########################################################################################################################
#Scenario corresponding to MNAR2 DAG with fixed Y_prev = 0.1, R_prev = 0.5, gammas = 0.5, beta_X2 = 0.5, beta_U = 0.5
MNAR2_1580 <- readRDS("Results_1580.RDS")
# ########################################################################################################################

MNAR2_1580_DAG_added <- MNAR2_1580 %>%
  add_column(DAG_type = "MNAR2")

# #######################
# MODEL 1 AUC
# #######################
#first summarise the AUC (+CIs) for all models and all datasets 
MNAR2_AUC <- MNAR2_1580_DAG_added %>% 
  group_by(dataset, mod, DAG_type) %>% #group by dataset and model
  summarise(AUC_mean = mean(AUC),  #get a summary of the mean AUC + CIs 
            LCI = quantile(AUC, 0.025), 
            UCI = quantile(AUC, 0.975))

MOD1_MNAR2_AUC <- MNAR2_AUC %>% 
  filter(mod == 1) #get AUCs only for model 1

MOD1_MNAR2_AUC <- MOD1_MNAR2_AUC[-2,] #exclude the CCA_imp_data because we'll be only comparing CCA_val + all_data_imp

#Plot AUC_mean for all 8 datasets for MODEL 1
ggplot(MOD1_MNAR2_AUC, aes(x = AUC_mean, y = dataset)) + geom_point() +    #plot AUC against datasets 
  geom_segment(aes(x = LCI, y = dataset, xend = UCI, yend = dataset))  


# #######################
# MODEL 2 AUC
# #######################
MOD2_MNAR2_AUC <- MNAR2_AUC %>% 
  filter(mod == 2) 

MOD2_MNAR2_AUC <- MOD2_MNAR2_AUC[-2,] 

ggplot(MOD2_MNAR2_AUC, aes(x = AUC_mean, y = dataset)) + geom_point() +     
  geom_segment(aes(x = LCI, y = dataset, xend = UCI, yend = dataset))  


# #######################
# MODEL 3 AUC
# #######################
MOD3_MNAR2_AUC <- MNAR2_AUC %>% 
  filter(mod == 3) 

MOD3_MNAR2_AUC <- MOD3_MNAR2_AUC[-2,] 

ggplot(MOD3_MNAR2_AUC, aes(x = AUC_mean, y = dataset)) + geom_point() +    
  geom_segment(aes(x = LCI, y = dataset, xend = UCI, yend = dataset))  



# #######################
# MODEL 4 AUC
# #######################
MOD4_MNAR2_AUC <- MNAR2_AUC %>% 
  filter(mod == 4) 

MOD4_MNAR2_AUC <- MOD4_MNAR2_AUC[-2,] 

ggplot(MOD4_MNAR2_AUC, aes(x = AUC_mean, y = dataset)) + geom_point() +    
  geom_segment(aes(x = LCI, y = dataset, xend = UCI, yend = dataset))  



# #######################
# ALL MODELS
# #######################
MNAR2_all_models_AUC <- rbind(MOD1_MNAR2_AUC, MOD2_MNAR2_AUC, MOD3_MNAR2_AUC, MOD4_MNAR2_AUC)
ggplot(MNAR2_all_models_AUC, aes(x=AUC_mean, y = dataset)) + geom_point() +
  geom_segment(aes(x=LCI, xend = UCI, y = dataset, yend = dataset)) +
  ggtitle("MNAR2: Y_prev = 0.1, R_prev = 0.5, gammas = 0.5, beta_X2 = 0.5, beta_U = 0.5") +
  facet_grid(~mod)








# ########################################################################################################################
#Scenario corresponding to MNAR3 DAG with fixed Y_prev = 0.1, R_prev = 0.5, gammas = 0.5, betas = non-zero
MNAR3_1823 <- readRDS("Results_1823.RDS")
# ########################################################################################################################

MNAR3_1823_DAG_added <- MNAR3_1823 %>%
  add_column(DAG_type = "MNAR3")

# #######################
# MODEL 1 AUC
# #######################
#first summarise the AUC (+CIs) for all models and all datasets 
MNAR3_AUC <- MNAR3_1823_DAG_added %>% 
  group_by(dataset, mod, DAG_type) %>% #group by dataset and model
  summarise(AUC_mean = mean(AUC),  #get a summary of the mean AUC + CIs 
            LCI = quantile(AUC, 0.025), 
            UCI = quantile(AUC, 0.975))

MOD1_MNAR3_AUC <- MNAR3_AUC %>% 
  filter(mod == 1) #get AUCs only for model 1

MOD1_MNAR3_AUC <- MOD1_MNAR3_AUC[-2,] #exclude the CCA_imp_data because we'll be only comparing CCA_val + all_data_imp

#Plot AUC_mean for all 8 datasets for MODEL 1
ggplot(MOD1_MNAR3_AUC, aes(x = AUC_mean, y = dataset)) + geom_point() +    #plot AUC against datasets 
  geom_segment(aes(x = LCI, y = dataset, xend = UCI, yend = dataset))  


# #######################
# MODEL 2 AUC
# #######################
MOD2_MNAR3_AUC <- MNAR3_AUC %>% 
  filter(mod == 2) 

MOD2_MNAR3_AUC <- MOD2_MNAR3_AUC[-2,] 

ggplot(MOD2_MNAR3_AUC, aes(x = AUC_mean, y = dataset)) + geom_point() +     
  geom_segment(aes(x = LCI, y = dataset, xend = UCI, yend = dataset))  


# #######################
# MODEL 3 AUC
# #######################
MOD3_MNAR3_AUC <- MNAR3_AUC %>% 
  filter(mod == 3) 

MOD3_MNAR3_AUC <- MOD3_MNAR3_AUC[-2,] 

ggplot(MOD3_MNAR3_AUC, aes(x = AUC_mean, y = dataset)) + geom_point() +    
  geom_segment(aes(x = LCI, y = dataset, xend = UCI, yend = dataset))  



# #######################
# MODEL 4 AUC
# #######################
MOD4_MNAR3_AUC <- MNAR3_AUC %>% 
  filter(mod == 4) 

MOD4_MNAR3_AUC <- MOD4_MNAR3_AUC[-2,] 

ggplot(MOD4_MNAR3_AUC, aes(x = AUC_mean, y = dataset)) + geom_point() +    
  geom_segment(aes(x = LCI, y = dataset, xend = UCI, yend = dataset))  



# #######################
# ALL MODELS
# #######################
MNAR3_all_models_AUC <- rbind(MOD1_MNAR3_AUC, MOD2_MNAR3_AUC, MOD3_MNAR3_AUC, MOD4_MNAR3_AUC)
ggplot(MNAR3_all_models_AUC, aes(x=AUC_mean, y = dataset)) + geom_point() +
  geom_segment(aes(x=LCI, xend = UCI, y = dataset, yend = dataset)) +
  ggtitle("MNAR3: Y_prev = 0.1, R_prev = 0.5, gammas = 0.5, betas = non-zero") +
  facet_grid(~mod)


# ########################################################################################################################
# Try to plot all DAGs together 

all_DAGs_models_AUC <- rbind(MCAR_all_models_AUC, 
                             MAR_all_models_AUC, 
                             MNAR1_all_models_AUC, 
                             MNAR2_all_models_AUC, 
                             MNAR3_all_models_AUC)

combined_AUC_plot <- ggplot(all_DAGs_models_AUC, aes(x=AUC_mean, y = dataset)) + geom_point() +
  geom_segment(aes(x=LCI, xend = UCI, y = dataset, yend = dataset)) +
  ggtitle("AUC estimates across Validation/Implementation datasets, imputation methods \n and missingness mechanisms, Y_prev = 0.1, R_prev = 0.5, gammas = 0.5") + 
  theme(plot.title = element_text(hjust = 0.5))
  
combined_AUC_plot + facet_grid(DAG_type ~ mod)



# ########################################################################################################################
# ########################################################################################################################
# ########################################### BRIER SCORE ################################################################
# ########################################################################################################################
# ########################################################################################################################
# ########################################################################################################################

# ####################################################################################################
#Scenario corresponding to MCAR DAG with fixed Y_prev = 0.1, R_prev = 0.5, gammas = 0.5, (betas = 0)
# ####################################################################################################

# #######################
# MODEL 1 Brier
# #######################

MCAR_Brier <- MCAR_1472_DAG_added %>% 
  group_by(dataset, mod, DAG_type) %>% #group by dataset and model
  summarise(Brier_mean = mean(Brier),  #get a summary of the mean AUC + CIs 
            LCI = quantile(Brier, 0.025), 
            UCI = quantile(Brier, 0.975))

MOD1_MCAR_Brier <- MCAR_Brier %>% 
  filter(mod == 1) #get AUCs only for model 1

MOD1_MCAR_Brier <- MOD1_MCAR_Brier[-2,] #exclude the CCA_imp_data because we'll be only comparing CCA_val + all_data_imp

#Plot AUC_mean for all 8 datasets for MODEL 1
ggplot(MOD1_MCAR_Brier, aes(x = Brier_mean, y = dataset)) + geom_point() +    #plot AUC against datasets 
  geom_segment(aes(x = LCI, y = dataset, xend = UCI, yend = dataset))  


# #######################
# MODEL 2 Brier
# #######################
MOD2_MCAR_Brier <- MCAR_Brier %>% 
  filter(mod == 2) #get AUCs only for model 1

MOD2_MCAR_Brier <- MOD2_MCAR_Brier[-2,] #exclude the CCA_imp_data because we'll be only comparing CCA_val + all_data_imp

#Plot AUC_mean for all 8 datasets for MODEL 1
ggplot(MOD2_MCAR_Brier, aes(x = Brier_mean, y = dataset)) + geom_point() +    #plot AUC against datasets 
  geom_segment(aes(x = LCI, y = dataset, xend = UCI, yend = dataset))  



# #######################
# MODEL 3 Brier
# #######################
MOD3_MCAR_Brier <- MCAR_Brier %>% 
  filter(mod == 3) #get AUCs only for model 1

MOD3_MCAR_Brier <- MOD3_MCAR_Brier[-2,] #exclude the CCA_imp_data because we'll be only comparing CCA_val + all_data_imp

#Plot AUC_mean for all 8 datasets for MODEL 1
ggplot(MOD3_MCAR_Brier, aes(x = Brier_mean, y = dataset)) + geom_point() +    #plot AUC against datasets 
  geom_segment(aes(x = LCI, y = dataset, xend = UCI, yend = dataset))  



# #######################
# MODEL 4 Brier
# #######################
MOD4_MCAR_Brier <- MCAR_Brier %>% 
  filter(mod == 4) #get AUCs only for model 1

MOD4_MCAR_Brier <- MOD4_MCAR_Brier[-2,] #exclude the CCA_imp_data because we'll be only comparing CCA_val + all_data_imp

#Plot AUC_mean for all 8 datasets for MODEL 1
ggplot(MOD4_MCAR_Brier, aes(x = Brier_mean, y = dataset)) + geom_point() +    #plot AUC against datasets 
  geom_segment(aes(x = LCI, y = dataset, xend = UCI, yend = dataset))  




# #######################
# ALL MODELS
# #######################
MCAR_all_models_Brier <- rbind(MOD1_MCAR_Brier, MOD2_MCAR_Brier, MOD3_MCAR_Brier, MOD4_MCAR_Brier)
ggplot(MCAR_all_models_Brier, aes(x=Brier_mean, y = dataset)) + geom_point() +
  geom_segment(aes(x=LCI, xend = UCI, y = dataset, yend = dataset)) +
  ggtitle("MCAR: Y_prev = 0.1, R_prev = 0.5, gammas = 0.5, betas = 0") +
  facet_grid(~mod)



# ####################################################################################################
#Scenario corresponding to MAR DAG with fixed Y_prev = 0.1, R_prev = 0.5, gammas = 0.5, beta_X2 = 0.5
# ####################################################################################################

# #######################
# MODEL 1 Brier
# #######################
#first summarise the AUC (+CIs) for all models and all datasets 
MAR_Brier <- MAR_1553_DAG_added %>% 
  group_by(dataset, mod, DAG_type) %>% #group by dataset and model
  summarise(Brier_mean = mean(Brier),  #get a summary of the mean AUC + CIs 
            LCI = quantile(Brier, 0.025), 
            UCI = quantile(Brier, 0.975))

MOD1_MAR_Brier <- MAR_Brier %>% 
  filter(mod == 1) #get AUCs only for model 1

MOD1_MAR_Brier <- MOD1_MAR_Brier[-2,] #exclude the CCA_imp_data because we'll be only comparing CCA_val + all_data_imp

#Plot AUC_mean for all 8 datasets for MODEL 1
ggplot(MOD1_MAR_Brier, aes(x = Brier_mean, y = dataset)) + geom_point() +    #plot AUC against datasets 
  geom_segment(aes(x = LCI, y = dataset, xend = UCI, yend = dataset))  


# #######################
# MODEL 2 Brier
# #######################
MOD2_MAR_Brier <- MAR_Brier %>% 
  filter(mod == 2) #get AUCs only for model 1

MOD2_MAR_Brier <- MOD2_MAR_Brier[-2,] #exclude the CCA_imp_data because we'll be only comparing CCA_val + all_data_imp

#Plot AUC_mean for all 8 datasets for MODEL 1
ggplot(MOD2_MAR_Brier, aes(x = Brier_mean, y = dataset)) + geom_point() +    #plot AUC against datasets 
  geom_segment(aes(x = LCI, y = dataset, xend = UCI, yend = dataset))  


# #######################
# MODEL 3 Brier
# #######################
MOD3_MAR_Brier <- MAR_Brier %>% 
  filter(mod == 3) #get AUCs only for model 1

MOD3_MAR_Brier <- MOD3_MAR_Brier[-2,] #exclude the CCA_imp_data because we'll be only comparing CCA_val + all_data_imp

#Plot AUC_mean for all 8 datasets for MODEL 1
ggplot(MOD3_MAR_Brier, aes(x = Brier_mean, y = dataset)) + geom_point() +    #plot AUC against datasets 
  geom_segment(aes(x = LCI, y = dataset, xend = UCI, yend = dataset))  



# #######################
# MODEL 4 Brier
# #######################
MOD4_MAR_Brier <- MAR_Brier %>% 
  filter(mod == 4) #get AUCs only for model 1

MOD4_MAR_Brier <- MOD4_MAR_Brier[-2,] #exclude the CCA_imp_data because we'll be only comparing CCA_val + all_data_imp


ggplot(MOD4_MAR_Brier, aes(x = Brier_mean, y = dataset)) + geom_point() +    #plot AUC against datasets 
  geom_segment(aes(x = LCI, y = dataset, xend = UCI, yend = dataset))   



# #######################
# ALL MODELS
# #######################
MAR_all_models_Brier <- rbind(MOD1_MAR_Brier, MOD2_MAR_Brier, MOD3_MAR_Brier, MOD4_MAR_Brier)
ggplot(MAR_all_models_Brier, aes(x = Brier_mean, y = dataset)) + geom_point() +
  geom_segment(aes(x=LCI, xend = UCI, y = dataset, yend = dataset)) +
  ggtitle("MAR: Y_prev = 0.1, R_prev = 0.5, gammas = 0.5, beta_X2 = 0.5") +
  facet_grid(~mod)



# ########################################################################################################################
#Scenario corresponding to MNAR1 DAG with fixed Y_prev = 0.1, R_prev = 0.5, gammas = 0.5, beta_X1 = 0.5, beta_X2 = 0.5
# ########################################################################################################################


# #######################
# MODEL 1 Brier
# #######################
#first summarise the AUC (+CIs) for all models and all datasets 
MNAR1_Brier <- MNAR1_1796_DAG_added %>% 
  group_by(dataset, mod, DAG_type) %>% #group by dataset and model
  summarise(Brier_mean = mean(Brier),  #get a summary of the mean AUC + CIs 
            LCI = quantile(Brier, 0.025), 
            UCI = quantile(Brier, 0.975))

MOD1_MNAR1_Brier <- MNAR1_Brier %>% 
  filter(mod == 1) #get AUCs only for model 1

MOD1_MNAR1_Brier <- MOD1_MNAR1_Brier[-2,] #exclude the CCA_imp_data because we'll be only comparing CCA_val + all_data_imp

#Plot AUC_mean for all 8 datasets for MODEL 1
ggplot(MOD1_MNAR1_Brier, aes(x = Brier_mean, y = dataset)) + geom_point() +    #plot AUC against datasets 
  geom_segment(aes(x = LCI, y = dataset, xend = UCI, yend = dataset))  


# #######################
# MODEL 2 Brier
# #######################
MOD2_MNAR1_Brier <- MNAR1_Brier %>% 
  filter(mod == 2) #get AUCs only for model 1

MOD2_MNAR1_Brier <- MOD2_MNAR1_Brier[-2,] #exclude the CCA_imp_data because we'll be only comparing CCA_val + all_data_imp

#Plot AUC_mean for all 8 datasets for MODEL 1
ggplot(MOD2_MNAR1_Brier, aes(x = Brier_mean, y = dataset)) + geom_point() +    #plot AUC against datasets 
  geom_segment(aes(x = LCI, y = dataset, xend = UCI, yend = dataset))  

# #######################
# MODEL 3 Brier
# #######################
MOD3_MNAR1_Brier <- MNAR1_Brier %>% 
  filter(mod == 3) #get AUCs only for model 1

MOD3_MNAR1_Brier <- MOD3_MNAR1_Brier[-2,] #exclude the CCA_imp_data because we'll be only comparing CCA_val + all_data_imp

#Plot AUC_mean for all 8 datasets for MODEL 1
ggplot(MOD3_MNAR1_Brier, aes(x = Brier_mean, y = dataset)) + geom_point() +    #plot AUC against datasets 
  geom_segment(aes(x = LCI, y = dataset, xend = UCI, yend = dataset))  



# #######################
# MODEL 4 Brier
# #######################
MOD4_MNAR1_Brier <- MNAR1_Brier %>% 
  filter(mod == 4) #get AUCs only for model 1

MOD4_MNAR1_Brier <- MOD4_MNAR1_Brier[-2,] #exclude the CCA_imp_data because we'll be only comparing CCA_val + all_data_imp

#Plot AUC_mean for all 8 datasets for MODEL 1
ggplot(MOD4_MNAR1_Brier, aes(x = Brier_mean, y = dataset)) + geom_point() +    #plot AUC against datasets 
  geom_segment(aes(x = LCI, y = dataset, xend = UCI, yend = dataset))  



# #######################
# ALL MODELS
# #######################
MNAR1_all_models_Brier <- rbind(MOD1_MNAR1_Brier, MOD2_MNAR1_Brier, MOD3_MNAR1_Brier, MOD4_MNAR1_Brier)
ggplot(MNAR1_all_models_Brier, aes(x = Brier_mean, y = dataset)) + geom_point() +
  geom_segment(aes(x=LCI, xend = UCI, y = dataset, yend = dataset)) +
  ggtitle("MNAR1: Y_prev = 0.1, R_prev = 0.5, gammas = 0.5, beta_X1 = 0.5, beta_X2 = 0.5") +
  facet_grid(~mod)


# ########################################################################################################################
#Scenario corresponding to MNAR2 DAG with fixed Y_prev = 0.1, R_prev = 0.5, gammas = 0.5, beta_X2 = 0.5, beta_U = 0.5
# ########################################################################################################################


# #######################
# MODEL 1 Brier
# #######################

MNAR2_Brier <- MNAR2_1580_DAG_added %>% 
  group_by(dataset, mod, DAG_type) %>% #group by dataset and model
  summarise(Brier_mean = mean(Brier),  #get a summary of the mean AUC + CIs 
            LCI = quantile(Brier, 0.025), 
            UCI = quantile(Brier, 0.975))

MOD1_MNAR2_Brier <- MNAR2_Brier %>% 
  filter(mod == 1) #get AUCs only for model 1

MOD1_MNAR2_Brier <- MOD1_MNAR2_Brier[-2,] #exclude the CCA_imp_data because we'll be only comparing CCA_val + all_data_imp

#Plot AUC_mean for all 8 datasets for MODEL 1
ggplot(MOD1_MNAR2_Brier, aes(x = Brier_mean, y = dataset)) + geom_point() +    #plot AUC against datasets 
  geom_segment(aes(x = LCI, y = dataset, xend = UCI, yend = dataset))  


# #######################
# MODEL 2 Brier
# #######################
MOD2_MNAR2_Brier <- MNAR2_Brier %>% 
  filter(mod == 2) 

MOD2_MNAR2_Brier <- MOD2_MNAR2_Brier[-2,] 

ggplot(MOD2_MNAR2_Brier, aes(x = Brier_mean, y = dataset)) + geom_point() +     
  geom_segment(aes(x = LCI, y = dataset, xend = UCI, yend = dataset))  


# #######################
# MODEL 3 Brier
# #######################
MOD3_MNAR2_Brier <- MNAR2_Brier %>% 
  filter(mod == 3) 

MOD3_MNAR2_Brier <- MOD3_MNAR2_Brier[-2,] 

ggplot(MOD3_MNAR2_Brier, aes(x = Brier_mean, y = dataset)) + geom_point() +    
  geom_segment(aes(x = LCI, y = dataset, xend = UCI, yend = dataset))  



# #######################
# MODEL 4 Brier
# #######################
MOD4_MNAR2_Brier <- MNAR2_Brier %>% 
  filter(mod == 4) 

MOD4_MNAR2_Brier <- MOD4_MNAR2_Brier[-2,] 

ggplot(MOD4_MNAR2_Brier, aes(x = Brier_mean, y = dataset)) + geom_point() +    
  geom_segment(aes(x = LCI, y = dataset, xend = UCI, yend = dataset))  



# #######################
# ALL MODELS
# #######################
MNAR2_all_models_Brier <- rbind(MOD1_MNAR2_Brier, MOD2_MNAR2_Brier, MOD3_MNAR2_Brier, MOD4_MNAR2_Brier)
ggplot(MNAR2_all_models_Brier, aes(x = Brier_mean, y = dataset)) + geom_point() +
  geom_segment(aes(x=LCI, xend = UCI, y = dataset, yend = dataset)) +
  ggtitle("MNAR2: Y_prev = 0.1, R_prev = 0.5, gammas = 0.5, beta_X2 = 0.5, beta_U = 0.5") +
  facet_grid(~mod)








# ########################################################################################################################
#Scenario corresponding to MNAR3 DAG with fixed Y_prev = 0.1, R_prev = 0.5, gammas = 0.5, betas = non-zero
# ########################################################################################################################

# #######################
# MODEL 1 Brier
# #######################
#first summarise the AUC (+CIs) for all models and all datasets 
MNAR3_Brier <- MNAR3_1823_DAG_added %>% 
  group_by(dataset, mod, DAG_type) %>% #group by dataset and model
  summarise(Brier_mean = mean(Brier),  #get a summary of the mean AUC + CIs 
            LCI = quantile(Brier, 0.025), 
            UCI = quantile(Brier, 0.975))

MOD1_MNAR3_Brier <- MNAR3_Brier %>% 
  filter(mod == 1) #get AUCs only for model 1

MOD1_MNAR3_Brier <- MOD1_MNAR3_Brier[-2,] #exclude the CCA_imp_data because we'll be only comparing CCA_val + all_data_imp

#Plot AUC_mean for all 8 datasets for MODEL 1
ggplot(MOD1_MNAR3_Brier, aes(x = Brier_mean, y = dataset)) + geom_point() +    #plot AUC against datasets 
  geom_segment(aes(x = LCI, y = dataset, xend = UCI, yend = dataset))  


# #######################
# MODEL 2 Brier
# #######################
MOD2_MNAR3_Brier <- MNAR3_Brier %>% 
  filter(mod == 2) 

MOD2_MNAR3_Brier <- MOD2_MNAR3_Brier[-2,] 

ggplot(MOD2_MNAR3_Brier, aes(x = Brier_mean, y = dataset)) + geom_point() +     
  geom_segment(aes(x = LCI, y = dataset, xend = UCI, yend = dataset))  


# #######################
# MODEL 3 Brier
# #######################
MOD3_MNAR3_Brier <- MNAR3_Brier %>% 
  filter(mod == 3) 

MOD3_MNAR3_Brier <- MOD3_MNAR3_Brier[-2,] 

ggplot(MOD3_MNAR3_Brier, aes(x = Brier_mean, y = dataset)) + geom_point() +    
  geom_segment(aes(x = LCI, y = dataset, xend = UCI, yend = dataset))  



# #######################
# MODEL 4 Brier
# #######################
MOD4_MNAR3_Brier <- MNAR3_Brier %>% 
  filter(mod == 4) 

MOD4_MNAR3_Brier <- MOD4_MNAR3_Brier[-2,] 

ggplot(MOD4_MNAR3_Brier, aes(x = Brier_mean, y = dataset)) + geom_point() +    
  geom_segment(aes(x = LCI, y = dataset, xend = UCI, yend = dataset))  



# #######################
# ALL MODELS
# #######################
MNAR3_all_models_Brier <- rbind(MOD1_MNAR3_Brier, MOD2_MNAR3_Brier, MOD3_MNAR3_Brier, MOD4_MNAR3_Brier)
ggplot(MNAR3_all_models_Brier, aes(x = Brier_mean, y = dataset)) + geom_point() +
  geom_segment(aes(x=LCI, xend = UCI, y = dataset, yend = dataset)) +
  ggtitle("MNAR3: Y_prev = 0.1, R_prev = 0.5, gammas = 0.5, betas = non-zero") +
  facet_grid(~mod)


# ########################################################################################################################
# Try to plot all DAGs together 

all_DAGs_models_Brier <- rbind(MCAR_all_models_Brier, 
                             MAR_all_models_Brier, 
                             MNAR1_all_models_Brier, 
                             MNAR2_all_models_Brier, 
                             MNAR3_all_models_Brier)

combined_AUC_plot <- ggplot(all_DAGs_models_Brier, aes(x = Brier_mean, y = dataset)) + geom_point() +
  geom_segment(aes(x=LCI, xend = UCI, y = dataset, yend = dataset)) +
  ggtitle("Brier score estimates across Validation/Implementation datasets, imputation methods \n and missingness mechanisms, Y_prev = 0.1, R_prev = 0.5, gammas = 0.5") + 
  theme(plot.title = element_text(hjust = 0.5))

combined_AUC_plot + facet_grid(DAG_type ~ mod)



# ########################################################################################################################
# ########################################################################################################################
# ########################################### OBSERVED VS EXPECTED RATIO #################################################
# ########################################################################################################################
# ########################################################################################################################
# ########################################################################################################################

# ####################################################################################################
#Scenario corresponding to MCAR DAG with fixed Y_prev = 0.1, R_prev = 0.5, gammas = 0.5, (betas = 0)
# ####################################################################################################

# #######################
# MODEL 1 O_E
# #######################

MCAR_OE <- MCAR_1472_DAG_added %>% 
  group_by(dataset, mod, DAG_type) %>% #group by dataset and model
  summarise(OE_mean = mean(O_E),  #get a summary of the mean AUC + CIs 
            LCI = quantile(O_E, 0.025), 
            UCI = quantile(O_E, 0.975))

MOD1_MCAR_OE <- MCAR_OE %>% 
  filter(mod == 1) #get AUCs only for model 1

MOD1_MCAR_OE <- MOD1_MCAR_OE[-2,] #exclude the CCA_imp_data because we'll be only comparing CCA_val + all_data_imp

#Plot AUC_mean for all 8 datasets for MODEL 1
ggplot(MOD1_MCAR_OE, aes(x = OE_mean, y = dataset)) + geom_point() +    #plot AUC against datasets 
  geom_segment(aes(x = LCI, y = dataset, xend = UCI, yend = dataset))  


# #######################
# MODEL 2 O_E
# #######################
MOD2_MCAR_OE <- MCAR_OE %>% 
  filter(mod == 2) #get AUCs only for model 1

MOD2_MCAR_OE <- MOD2_MCAR_OE[-2,] #exclude the CCA_imp_data because we'll be only comparing CCA_val + all_data_imp

#Plot AUC_mean for all 8 datasets for MODEL 1
ggplot(MOD2_MCAR_OE, aes(x = OE_mean, y = dataset)) + geom_point() +    #plot AUC against datasets 
  geom_segment(aes(x = LCI, y = dataset, xend = UCI, yend = dataset))  



# #######################
# MODEL 3 O_E
# #######################
MOD3_MCAR_OE <- MCAR_OE %>% 
  filter(mod == 3) #get AUCs only for model 1

MOD3_MCAR_OE <- MOD3_MCAR_OE[-2,] #exclude the CCA_imp_data because we'll be only comparing CCA_val + all_data_imp

#Plot AUC_mean for all 8 datasets for MODEL 1
ggplot(MOD3_MCAR_OE, aes(x = OE_mean, y = dataset)) + geom_point() +    #plot AUC against datasets 
  geom_segment(aes(x = LCI, y = dataset, xend = UCI, yend = dataset))  



# #######################
# MODEL 4 O_E
# #######################
MOD4_MCAR_OE <- MCAR_OE %>% 
  filter(mod == 4) #get AUCs only for model 1

MOD4_MCAR_OE <- MOD4_MCAR_OE[-2,] #exclude the CCA_imp_data because we'll be only comparing CCA_val + all_data_imp

#Plot AUC_mean for all 8 datasets for MODEL 1
ggplot(MOD4_MCAR_OE, aes(x = OE_mean, y = dataset)) + geom_point() +    #plot AUC against datasets 
  geom_segment(aes(x = LCI, y = dataset, xend = UCI, yend = dataset))  




# #######################
# ALL MODELS
# #######################
MCAR_all_models_OE <- rbind(MOD1_MCAR_OE, MOD2_MCAR_OE, MOD3_MCAR_OE, MOD4_MCAR_OE)
ggplot(MCAR_all_models_OE, aes(x = OE_mean, y = dataset)) + geom_point() +
  geom_segment(aes(x=LCI, xend = UCI, y = dataset, yend = dataset)) +
  ggtitle("MCAR: Y_prev = 0.1, R_prev = 0.5, gammas = 0.5, betas = 0") +
  facet_grid(~mod)



# ####################################################################################################
#Scenario corresponding to MAR DAG with fixed Y_prev = 0.1, R_prev = 0.5, gammas = 0.5, beta_X2 = 0.5
# ####################################################################################################

# #######################
# MODEL 1 O_E
# #######################
#first summarise the AUC (+CIs) for all models and all datasets 
MAR_OE <- MAR_1553_DAG_added %>% 
  group_by(dataset, mod, DAG_type) %>% #group by dataset and model
  summarise(OE_mean = mean(O_E),  #get a summary of the mean AUC + CIs 
            LCI = quantile(O_E, 0.025), 
            UCI = quantile(O_E, 0.975))

MOD1_MAR_OE <- MAR_OE %>% 
  filter(mod == 1) #get AUCs only for model 1

MOD1_MAR_OE <- MOD1_MAR_OE[-2,] #exclude the CCA_imp_data because we'll be only comparing CCA_val + all_data_imp

#Plot AUC_mean for all 8 datasets for MODEL 1
ggplot(MOD1_MAR_OE, aes(x = OE_mean, y = dataset)) + geom_point() +    #plot AUC against datasets 
  geom_segment(aes(x = LCI, y = dataset, xend = UCI, yend = dataset))  


# #######################
# MODEL 2 O_E
# #######################
MOD2_MAR_OE <- MAR_OE %>% 
  filter(mod == 2) #get AUCs only for model 1

MOD2_MAR_OE <- MOD2_MAR_OE[-2,] #exclude the CCA_imp_data because we'll be only comparing CCA_val + all_data_imp

#Plot AUC_mean for all 8 datasets for MODEL 1
ggplot(MOD2_MAR_OE, aes(x = OE_mean, y = dataset)) + geom_point() +    #plot AUC against datasets 
  geom_segment(aes(x = LCI, y = dataset, xend = UCI, yend = dataset))  


# #######################
# MODEL 3 O_E
# #######################
MOD3_MAR_OE <- MAR_OE %>% 
  filter(mod == 3) #get AUCs only for model 1

MOD3_MAR_OE <- MOD3_MAR_OE[-2,] #exclude the CCA_imp_data because we'll be only comparing CCA_val + all_data_imp

#Plot AUC_mean for all 8 datasets for MODEL 1
ggplot(MOD3_MAR_OE, aes(x = OE_mean, y = dataset)) + geom_point() +    #plot AUC against datasets 
  geom_segment(aes(x = LCI, y = dataset, xend = UCI, yend = dataset))  



# #######################
# MODEL 4 O_E
# #######################
MOD4_MAR_OE <- MAR_OE %>% 
  filter(mod == 4) #get AUCs only for model 1

MOD4_MAR_OE <- MOD4_MAR_OE[-2,] #exclude the CCA_imp_data because we'll be only comparing CCA_val + all_data_imp


ggplot(MOD4_MAR_OE, aes(x = OE_mean, y = dataset)) + geom_point() +    #plot AUC against datasets 
  geom_segment(aes(x = LCI, y = dataset, xend = UCI, yend = dataset))   



# #######################
# ALL MODELS
# #######################
MAR_all_models_OE <- rbind(MOD1_MAR_OE, MOD2_MAR_OE, MOD3_MAR_OE, MOD4_MAR_OE)
ggplot(MAR_all_models_OE, aes(x = OE_mean, y = dataset)) + geom_point() +
  geom_segment(aes(x=LCI, xend = UCI, y = dataset, yend = dataset)) +
  ggtitle("MAR: Y_prev = 0.1, R_prev = 0.5, gammas = 0.5, beta_X2 = 0.5") +
  facet_grid(~mod)



# ########################################################################################################################
#Scenario corresponding to MNAR1 DAG with fixed Y_prev = 0.1, R_prev = 0.5, gammas = 0.5, beta_X1 = 0.5, beta_X2 = 0.5
# ########################################################################################################################


# #######################
# MODEL 1 O_E
# #######################
#first summarise the AUC (+CIs) for all models and all datasets 
MNAR1_OE <- MNAR1_1796_DAG_added %>% 
  group_by(dataset, mod, DAG_type) %>% #group by dataset and model
  summarise(OE_mean = mean(O_E),  #get a summary of the mean AUC + CIs 
            LCI = quantile(O_E, 0.025), 
            UCI = quantile(O_E, 0.975))

MOD1_MNAR1_OE <- MNAR1_OE %>% 
  filter(mod == 1) #get AUCs only for model 1

MOD1_MNAR1_OE <- MOD1_MNAR1_OE[-2,] #exclude the CCA_imp_data because we'll be only comparing CCA_val + all_data_imp

#Plot AUC_mean for all 8 datasets for MODEL 1
ggplot(MOD1_MNAR1_OE, aes(x = OE_mean, y = dataset)) + geom_point() +    #plot AUC against datasets 
  geom_segment(aes(x = LCI, y = dataset, xend = UCI, yend = dataset))  


# #######################
# MODEL 2 O_E
# #######################
MOD2_MNAR1_OE <- MNAR1_OE %>% 
  filter(mod == 2) #get AUCs only for model 1

MOD2_MNAR1_OE <- MOD2_MNAR1_OE[-2,] #exclude the CCA_imp_data because we'll be only comparing CCA_val + all_data_imp

#Plot AUC_mean for all 8 datasets for MODEL 1
ggplot(MOD2_MNAR1_OE, aes(x = OE_mean, y = dataset)) + geom_point() +    #plot AUC against datasets 
  geom_segment(aes(x = LCI, y = dataset, xend = UCI, yend = dataset))  

# #######################
# MODEL 3 O_E
# #######################
MOD3_MNAR1_OE <- MNAR1_OE %>% 
  filter(mod == 3) #get AUCs only for model 1

MOD3_MNAR1_OE <- MOD3_MNAR1_OE[-2,] #exclude the CCA_imp_data because we'll be only comparing CCA_val + all_data_imp

#Plot AUC_mean for all 8 datasets for MODEL 1
ggplot(MOD3_MNAR1_OE, aes(x = OE_mean, y = dataset)) + geom_point() +    #plot AUC against datasets 
  geom_segment(aes(x = LCI, y = dataset, xend = UCI, yend = dataset))  



# #######################
# MODEL 4 O_E
# #######################
MOD4_MNAR1_OE <- MNAR1_OE %>% 
  filter(mod == 4) #get AUCs only for model 1

MOD4_MNAR1_OE <- MOD4_MNAR1_OE[-2,] #exclude the CCA_imp_data because we'll be only comparing CCA_val + all_data_imp

#Plot AUC_mean for all 8 datasets for MODEL 1
ggplot(MOD4_MNAR1_OE, aes(x = OE_mean, y = dataset)) + geom_point() +    #plot AUC against datasets 
  geom_segment(aes(x = LCI, y = dataset, xend = UCI, yend = dataset))  



# #######################
# ALL MODELS
# #######################
MNAR1_all_models_OE <- rbind(MOD1_MNAR1_OE, MOD2_MNAR1_OE, MOD3_MNAR1_OE, MOD4_MNAR1_OE)
ggplot(MNAR1_all_models_OE, aes(x = OE_mean, y = dataset)) + geom_point() +
  geom_segment(aes(x=LCI, xend = UCI, y = dataset, yend = dataset)) +
  ggtitle("MNAR1: Y_prev = 0.1, R_prev = 0.5, gammas = 0.5, beta_X1 = 0.5, beta_X2 = 0.5") +
  facet_grid(~mod)


# ########################################################################################################################
#Scenario corresponding to MNAR2 DAG with fixed Y_prev = 0.1, R_prev = 0.5, gammas = 0.5, beta_X2 = 0.5, beta_U = 0.5
# ########################################################################################################################


# #######################
# MODEL 1 O_E
# #######################

MNAR2_OE <- MNAR2_1580_DAG_added %>% 
  group_by(dataset, mod, DAG_type) %>% #group by dataset and model
  summarise(OE_mean = mean(O_E),  #get a summary of the mean AUC + CIs 
            LCI = quantile(O_E, 0.025), 
            UCI = quantile(O_E, 0.975))

MOD1_MNAR2_OE <- MNAR2_OE %>% 
  filter(mod == 1) #get AUCs only for model 1

MOD1_MNAR2_OE <- MOD1_MNAR2_OE[-2,] #exclude the CCA_imp_data because we'll be only comparing CCA_val + all_data_imp

#Plot AUC_mean for all 8 datasets for MODEL 1
ggplot(MOD1_MNAR2_OE, aes(x = OE_mean, y = dataset)) + geom_point() +    #plot AUC against datasets 
  geom_segment(aes(x = LCI, y = dataset, xend = UCI, yend = dataset))  


# #######################
# MODEL 2 O_E
# #######################
MOD2_MNAR2_OE <- MNAR2_OE %>% 
  filter(mod == 2) 

MOD2_MNAR2_OE <- MOD2_MNAR2_OE[-2,] 

ggplot(MOD2_MNAR2_OE, aes(x = OE_mean, y = dataset)) + geom_point() +     
  geom_segment(aes(x = LCI, y = dataset, xend = UCI, yend = dataset))  


# #######################
# MODEL 3 O_E
# #######################
MOD3_MNAR2_OE <- MNAR2_OE %>% 
  filter(mod == 3) 

MOD3_MNAR2_OE <- MOD3_MNAR2_OE[-2,] 

ggplot(MOD3_MNAR2_OE, aes(x = OE_mean, y = dataset)) + geom_point() +    
  geom_segment(aes(x = LCI, y = dataset, xend = UCI, yend = dataset))  



# #######################
# MODEL 4 O_E
# #######################
MOD4_MNAR2_OE <- MNAR2_OE %>% 
  filter(mod == 4) 

MOD4_MNAR2_OE <- MOD4_MNAR2_OE[-2,] 

ggplot(MOD4_MNAR2_OE, aes(x = OE_mean, y = dataset)) + geom_point() +    
  geom_segment(aes(x = LCI, y = dataset, xend = UCI, yend = dataset))  



# #######################
# ALL MODELS
# #######################
MNAR2_all_models_OE <- rbind(MOD1_MNAR2_OE, MOD2_MNAR2_OE, MOD3_MNAR2_OE, MOD4_MNAR2_OE)
ggplot(MNAR2_all_models_OE, aes(x = OE_mean, y = dataset)) + geom_point() +
  geom_segment(aes(x=LCI, xend = UCI, y = dataset, yend = dataset)) +
  ggtitle("MNAR2: Y_prev = 0.1, R_prev = 0.5, gammas = 0.5, beta_X2 = 0.5, beta_U = 0.5") +
  facet_grid(~mod)








# ########################################################################################################################
#Scenario corresponding to MNAR3 DAG with fixed Y_prev = 0.1, R_prev = 0.5, gammas = 0.5, betas = non-zero
# ########################################################################################################################

# #######################
# MODEL 1 O_E
# #######################
#first summarise the AUC (+CIs) for all models and all datasets 
MNAR3_OE <- MNAR3_1823_DAG_added %>% 
  group_by(dataset, mod, DAG_type) %>% #group by dataset and model
  summarise(OE_mean = mean(O_E),  #get a summary of the mean AUC + CIs 
            LCI = quantile(O_E, 0.025), 
            UCI = quantile(O_E, 0.975))

MOD1_MNAR3_OE <- MNAR3_OE %>% 
  filter(mod == 1) #get AUCs only for model 1

MOD1_MNAR3_OE <- MOD1_MNAR3_OE[-2,] #exclude the CCA_imp_data because we'll be only comparing CCA_val + all_data_imp

#Plot AUC_mean for all 8 datasets for MODEL 1
ggplot(MOD1_MNAR3_OE, aes(x = OE_mean, y = dataset)) + geom_point() +    #plot AUC against datasets 
  geom_segment(aes(x = LCI, y = dataset, xend = UCI, yend = dataset))  


# #######################
# MODEL 2 O_E
# #######################
MOD2_MNAR3_OE <- MNAR3_OE %>% 
  filter(mod == 2) 

MOD2_MNAR3_OE <- MOD2_MNAR3_OE[-2,] 

ggplot(MOD2_MNAR3_OE, aes(x = OE_mean, y = dataset)) + geom_point() +     
  geom_segment(aes(x = LCI, y = dataset, xend = UCI, yend = dataset))  


# #######################
# MODEL 3 O_E
# #######################
MOD3_MNAR3_OE <- MNAR3_OE %>% 
  filter(mod == 3) 

MOD3_MNAR3_OE <- MOD3_MNAR3_OE[-2,] 

ggplot(MOD3_MNAR3_OE, aes(x = OE_mean, y = dataset)) + geom_point() +    
  geom_segment(aes(x = LCI, y = dataset, xend = UCI, yend = dataset))  



# #######################
# MODEL 4 O_E
# #######################
MOD4_MNAR3_OE <- MNAR3_OE %>% 
  filter(mod == 4) 

MOD4_MNAR3_OE <- MOD4_MNAR3_OE[-2,] 

ggplot(MOD4_MNAR3_OE, aes(x = OE_mean, y = dataset)) + geom_point() +    
  geom_segment(aes(x = LCI, y = dataset, xend = UCI, yend = dataset))  



# #######################
# ALL MODELS
# #######################
MNAR3_all_models_OE <- rbind(MOD1_MNAR3_OE, MOD2_MNAR3_OE, MOD3_MNAR3_OE, MOD4_MNAR3_OE)
ggplot(MNAR3_all_models_OE, aes(x = OE_mean, y = dataset)) + geom_point() +
  geom_segment(aes(x=LCI, xend = UCI, y = dataset, yend = dataset)) +
  ggtitle("MNAR3: Y_prev = 0.1, R_prev = 0.5, gammas = 0.5, betas = non-zero") +
  facet_grid(~mod)


# ########################################################################################################################
# Try to plot all DAGs together 

all_DAGs_models_OE <- rbind(MCAR_all_models_OE, 
                               MAR_all_models_OE, 
                               MNAR1_all_models_OE, 
                               MNAR2_all_models_OE, 
                               MNAR3_all_models_OE)

combined_OE_plot <- ggplot(all_DAGs_models_OE, aes(x = OE_mean, y = dataset)) + geom_point() +
  geom_segment(aes(x=LCI, xend = UCI, y = dataset, yend = dataset)) +
  ggtitle("Observed-vs-Expected ratio estimates across Validation/Implementation datasets, imputation methods \n and missingness mechanisms, Y_prev = 0.1, R_prev = 0.5, gammas = 0.5") + 
  theme(plot.title = element_text(hjust = 0.5))

combined_OE_plot + facet_grid(DAG_type ~ mod)









# ########################################################################################################################
# ########################################################################################################################
# ########################################### CALIBRATION SLOPE ##########################################################
# ########################################################################################################################
# ########################################################################################################################
# ########################################################################################################################

# ####################################################################################################
#Scenario corresponding to MCAR DAG with fixed Y_prev = 0.1, R_prev = 0.5, gammas = 0.5, (betas = 0)
# ####################################################################################################

# #######################
# MODEL 1 CalSlope
# #######################

MCAR_CalSlope <- MCAR_1472_DAG_added %>% 
  group_by(dataset, mod, DAG_type) %>% #group by dataset and model
  summarise(CalSlope_mean = mean(Cal_Slope),  #get a summary of the mean AUC + CIs 
            LCI = quantile(Cal_Slope, 0.025), 
            UCI = quantile(Cal_Slope, 0.975))

MOD1_MCAR_CalSlope <- MCAR_CalSlope %>% 
  filter(mod == 1) #get AUCs only for model 1

MOD1_MCAR_CalSlope <- MOD1_MCAR_CalSlope[-2,] #exclude the CCA_imp_data because we'll be only comparing CCA_val + all_data_imp

#Plot AUC_mean for all 8 datasets for MODEL 1
ggplot(MOD1_MCAR_CalSlope, aes(x = CalSlope_mean, y = dataset)) + geom_point() +    #plot AUC against datasets 
  geom_segment(aes(x = LCI, y = dataset, xend = UCI, yend = dataset))  


# #######################
# MODEL 2 CalSlope
# #######################
MOD2_MCAR_CalSlope <- MCAR_CalSlope %>% 
  filter(mod == 2) #get AUCs only for model 1

MOD2_MCAR_CalSlope <- MOD2_MCAR_CalSlope[-2,] #exclude the CCA_imp_data because we'll be only comparing CCA_val + all_data_imp

#Plot AUC_mean for all 8 datasets for MODEL 1
ggplot(MOD2_MCAR_CalSlope, aes(x = CalSlope_mean, y = dataset)) + geom_point() +    #plot AUC against datasets 
  geom_segment(aes(x = LCI, y = dataset, xend = UCI, yend = dataset))  



# #######################
# MODEL 3 CalSlope
# #######################
MOD3_MCAR_CalSlope <- MCAR_CalSlope %>% 
  filter(mod == 3) #get AUCs only for model 1

MOD3_MCAR_CalSlope <- MOD3_MCAR_CalSlope[-2,] #exclude the CCA_imp_data because we'll be only comparing CCA_val + all_data_imp

#Plot AUC_mean for all 8 datasets for MODEL 1
ggplot(MOD3_MCAR_CalSlope, aes(x = CalSlope_mean, y = dataset)) + geom_point() +    #plot AUC against datasets 
  geom_segment(aes(x = LCI, y = dataset, xend = UCI, yend = dataset))  



# #######################
# MODEL 4 CalSlope
# #######################
MOD4_MCAR_CalSlope <- MCAR_CalSlope %>% 
  filter(mod == 4) #get AUCs only for model 1

MOD4_MCAR_CalSlope <- MOD4_MCAR_CalSlope[-2,] #exclude the CCA_imp_data because we'll be only comparing CCA_val + all_data_imp

#Plot AUC_mean for all 8 datasets for MODEL 1
ggplot(MOD4_MCAR_CalSlope, aes(x = CalSlope_mean, y = dataset)) + geom_point() +    #plot AUC against datasets 
  geom_segment(aes(x = LCI, y = dataset, xend = UCI, yend = dataset))  




# #######################
# ALL MODELS
# #######################
MCAR_all_models_CalSlope <- rbind(MOD1_MCAR_CalSlope, MOD2_MCAR_CalSlope, MOD3_MCAR_CalSlope, MOD4_MCAR_CalSlope)
ggplot(MCAR_all_models_CalSlope, aes(x = CalSlope_mean, y = dataset)) + geom_point() +
  geom_segment(aes(x=LCI, xend = UCI, y = dataset, yend = dataset)) +
  ggtitle("MCAR: Y_prev = 0.1, R_prev = 0.5, gammas = 0.5, betas = 0") +
  facet_grid(~mod)



# ####################################################################################################
#Scenario corresponding to MAR DAG with fixed Y_prev = 0.1, R_prev = 0.5, gammas = 0.5, beta_X2 = 0.5
# ####################################################################################################

# #######################
# MODEL 1 CalSlope
# #######################
#first summarise the AUC (+CIs) for all models and all datasets 
MAR_CalSlope <- MAR_1553_DAG_added %>% 
  group_by(dataset, mod, DAG_type) %>% #group by dataset and model
  summarise(CalSlope_mean = mean(Cal_Slope),  #get a summary of the mean AUC + CIs 
            LCI = quantile(Cal_Slope, 0.025), 
            UCI = quantile(Cal_Slope, 0.975))

MOD1_MAR_CalSlope <- MAR_CalSlope %>% 
  filter(mod == 1) #get AUCs only for model 1

MOD1_MAR_CalSlope <- MOD1_MAR_CalSlope[-2,] #exclude the CCA_imp_data because we'll be only comparing CCA_val + all_data_imp

#Plot AUC_mean for all 8 datasets for MODEL 1
ggplot(MOD1_MAR_CalSlope, aes(x = CalSlope_mean, y = dataset)) + geom_point() +    #plot AUC against datasets 
  geom_segment(aes(x = LCI, y = dataset, xend = UCI, yend = dataset))  


# #######################
# MODEL 2 CalSlope
# #######################
MOD2_MAR_CalSlope <- MAR_CalSlope %>% 
  filter(mod == 2) #get AUCs only for model 1

MOD2_MAR_CalSlope <- MOD2_MAR_CalSlope[-2,] #exclude the CCA_imp_data because we'll be only comparing CCA_val + all_data_imp

#Plot AUC_mean for all 8 datasets for MODEL 1
ggplot(MOD2_MAR_CalSlope, aes(x = CalSlope_mean, y = dataset)) + geom_point() +    #plot AUC against datasets 
  geom_segment(aes(x = LCI, y = dataset, xend = UCI, yend = dataset))  


# #######################
# MODEL 3 CalSlope
# #######################
MOD3_MAR_CalSlope <- MAR_CalSlope %>% 
  filter(mod == 3) #get AUCs only for model 1

MOD3_MAR_CalSlope <- MOD3_MAR_CalSlope[-2,] #exclude the CCA_imp_data because we'll be only comparing CCA_val + all_data_imp

#Plot AUC_mean for all 8 datasets for MODEL 1
ggplot(MOD3_MAR_CalSlope, aes(x = CalSlope_mean, y = dataset)) + geom_point() +    #plot AUC against datasets 
  geom_segment(aes(x = LCI, y = dataset, xend = UCI, yend = dataset))  



# #######################
# MODEL 4 CalSlope
# #######################
MOD4_MAR_CalSlope <- MAR_CalSlope %>% 
  filter(mod == 4) #get AUCs only for model 1

MOD4_MAR_CalSlope <- MOD4_MAR_CalSlope[-2,] #exclude the CCA_imp_data because we'll be only comparing CCA_val + all_data_imp


ggplot(MOD4_MAR_CalSlope, aes(x = CalSlope_mean, y = dataset)) + geom_point() +    #plot AUC against datasets 
  geom_segment(aes(x = LCI, y = dataset, xend = UCI, yend = dataset))   



# #######################
# ALL MODELS
# #######################
MAR_all_models_CalSlope <- rbind(MOD1_MAR_CalSlope, MOD2_MAR_CalSlope, MOD3_MAR_CalSlope, MOD4_MAR_CalSlope)
ggplot(MAR_all_models_CalSlope, aes(x = CalSlope_mean, y = dataset)) + geom_point() +
  geom_segment(aes(x=LCI, xend = UCI, y = dataset, yend = dataset)) +
  ggtitle("MAR: Y_prev = 0.1, R_prev = 0.5, gammas = 0.5, beta_X2 = 0.5") +
  facet_grid(~mod)



# ########################################################################################################################
#Scenario corresponding to MNAR1 DAG with fixed Y_prev = 0.1, R_prev = 0.5, gammas = 0.5, beta_X1 = 0.5, beta_X2 = 0.5
# ########################################################################################################################


# #######################
# MODEL 1 CalSlope
# #######################
#first summarise the AUC (+CIs) for all models and all datasets 
MNAR1_CalSlope <- MNAR1_1796_DAG_added %>% 
  group_by(dataset, mod, DAG_type) %>% #group by dataset and model
  summarise(CalSlope_mean = mean(Cal_Slope),  #get a summary of the mean AUC + CIs 
            LCI = quantile(Cal_Slope, 0.025), 
            UCI = quantile(Cal_Slope, 0.975))

MOD1_MNAR1_CalSlope <- MNAR1_CalSlope %>% 
  filter(mod == 1) #get AUCs only for model 1

MOD1_MNAR1_CalSlope <- MOD1_MNAR1_CalSlope[-2,] #exclude the CCA_imp_data because we'll be only comparing CCA_val + all_data_imp

#Plot AUC_mean for all 8 datasets for MODEL 1
ggplot(MOD1_MNAR1_CalSlope, aes(x = CalSlope_mean, y = dataset)) + geom_point() +    #plot AUC against datasets 
  geom_segment(aes(x = LCI, y = dataset, xend = UCI, yend = dataset))  


# #######################
# MODEL 2 CalSlope
# #######################
MOD2_MNAR1_CalSlope <- MNAR1_CalSlope %>% 
  filter(mod == 2) #get AUCs only for model 1

MOD2_MNAR1_CalSlope <- MOD2_MNAR1_CalSlope[-2,] #exclude the CCA_imp_data because we'll be only comparing CCA_val + all_data_imp

#Plot AUC_mean for all 8 datasets for MODEL 1
ggplot(MOD2_MNAR1_CalSlope, aes(x = CalSlope_mean, y = dataset)) + geom_point() +    #plot AUC against datasets 
  geom_segment(aes(x = LCI, y = dataset, xend = UCI, yend = dataset))  

# #######################
# MODEL 3 CalSlope
# #######################
MOD3_MNAR1_CalSlope <- MNAR1_CalSlope %>% 
  filter(mod == 3) #get AUCs only for model 1

MOD3_MNAR1_CalSlope <- MOD3_MNAR1_CalSlope[-2,] #exclude the CCA_imp_data because we'll be only comparing CCA_val + all_data_imp

#Plot AUC_mean for all 8 datasets for MODEL 1
ggplot(MOD3_MNAR1_CalSlope, aes(x = CalSlope_mean, y = dataset)) + geom_point() +    #plot AUC against datasets 
  geom_segment(aes(x = LCI, y = dataset, xend = UCI, yend = dataset))  



# #######################
# MODEL 4 CalSlope
# #######################
MOD4_MNAR1_CalSlope <- MNAR1_CalSlope %>% 
  filter(mod == 4) #get AUCs only for model 1

MOD4_MNAR1_CalSlope <- MOD4_MNAR1_CalSlope[-2,] #exclude the CCA_imp_data because we'll be only comparing CCA_val + all_data_imp

#Plot AUC_mean for all 8 datasets for MODEL 1
ggplot(MOD4_MNAR1_CalSlope, aes(x = CalSlope_mean, y = dataset)) + geom_point() +    #plot AUC against datasets 
  geom_segment(aes(x = LCI, y = dataset, xend = UCI, yend = dataset))  



# #######################
# ALL MODELS
# #######################
MNAR1_all_models_CalSlope <- rbind(MOD1_MNAR1_CalSlope, MOD2_MNAR1_CalSlope, MOD3_MNAR1_CalSlope, MOD4_MNAR1_CalSlope)
ggplot(MNAR1_all_models_CalSlope, aes(x = CalSlope_mean, y = dataset)) + geom_point() +
  geom_segment(aes(x=LCI, xend = UCI, y = dataset, yend = dataset)) +
  ggtitle("MNAR1: Y_prev = 0.1, R_prev = 0.5, gammas = 0.5, beta_X1 = 0.5, beta_X2 = 0.5") +
  facet_grid(~mod)


# ########################################################################################################################
#Scenario corresponding to MNAR2 DAG with fixed Y_prev = 0.1, R_prev = 0.5, gammas = 0.5, beta_X2 = 0.5, beta_U = 0.5
# ########################################################################################################################


# #######################
# MODEL 1 CalSlope
# #######################

MNAR2_CalSlope <- MNAR2_1580_DAG_added %>% 
  group_by(dataset, mod, DAG_type) %>% #group by dataset and model
  summarise(CalSlope_mean = mean(Cal_Slope),  #get a summary of the mean AUC + CIs 
            LCI = quantile(Cal_Slope, 0.025), 
            UCI = quantile(Cal_Slope, 0.975))

MOD1_MNAR2_CalSlope <- MNAR2_CalSlope %>% 
  filter(mod == 1) #get AUCs only for model 1

MOD1_MNAR2_CalSlope <- MOD1_MNAR2_CalSlope[-2,] #exclude the CCA_imp_data because we'll be only comparing CCA_val + all_data_imp

#Plot AUC_mean for all 8 datasets for MODEL 1
ggplot(MOD1_MNAR2_CalSlope, aes(x = CalSlope_mean, y = dataset)) + geom_point() +    #plot AUC against datasets 
  geom_segment(aes(x = LCI, y = dataset, xend = UCI, yend = dataset))  


# #######################
# MODEL 2 CalSlope
# #######################
MOD2_MNAR2_CalSlope <- MNAR2_CalSlope %>% 
  filter(mod == 2) 

MOD2_MNAR2_CalSlope <- MOD2_MNAR2_CalSlope[-2,] 

ggplot(MOD2_MNAR2_CalSlope, aes(x = CalSlope_mean, y = dataset)) + geom_point() +     
  geom_segment(aes(x = LCI, y = dataset, xend = UCI, yend = dataset))  


# #######################
# MODEL 3 CalSlope
# #######################
MOD3_MNAR2_CalSlope <- MNAR2_CalSlope %>% 
  filter(mod == 3) 

MOD3_MNAR2_CalSlope <- MOD3_MNAR2_CalSlope[-2,] 

ggplot(MOD3_MNAR2_CalSlope, aes(x = CalSlope_mean, y = dataset)) + geom_point() +    
  geom_segment(aes(x = LCI, y = dataset, xend = UCI, yend = dataset))  



# #######################
# MODEL 4 CalSlope
# #######################
MOD4_MNAR2_CalSlope <- MNAR2_CalSlope %>% 
  filter(mod == 4) 

MOD4_MNAR2_CalSlope <- MOD4_MNAR2_CalSlope[-2,] 

ggplot(MOD4_MNAR2_CalSlope, aes(x = CalSlope_mean, y = dataset)) + geom_point() +    
  geom_segment(aes(x = LCI, y = dataset, xend = UCI, yend = dataset))  



# #######################
# ALL MODELS
# #######################
MNAR2_all_models_CalSlope <- rbind(MOD1_MNAR2_CalSlope, MOD2_MNAR2_CalSlope, MOD3_MNAR2_CalSlope, MOD4_MNAR2_CalSlope)
ggplot(MNAR2_all_models_CalSlope, aes(x = CalSlope_mean, y = dataset)) + geom_point() +
  geom_segment(aes(x=LCI, xend = UCI, y = dataset, yend = dataset)) +
  ggtitle("MNAR2: Y_prev = 0.1, R_prev = 0.5, gammas = 0.5, beta_X2 = 0.5, beta_U = 0.5") +
  facet_grid(~mod)








# ########################################################################################################################
#Scenario corresponding to MNAR3 DAG with fixed Y_prev = 0.1, R_prev = 0.5, gammas = 0.5, betas = non-zero
# ########################################################################################################################

# #######################
# MODEL 1 CalSlope
# #######################
#first summarise the AUC (+CIs) for all models and all datasets 
MNAR3_CalSlope <- MNAR3_1823_DAG_added %>% 
  group_by(dataset, mod, DAG_type) %>% #group by dataset and model
  summarise(CalSlope_mean = mean(Cal_Slope),  #get a summary of the mean AUC + CIs 
            LCI = quantile(Cal_Slope, 0.025), 
            UCI = quantile(Cal_Slope, 0.975))

MOD1_MNAR3_CalSlope <- MNAR3_CalSlope %>% 
  filter(mod == 1) #get AUCs only for model 1

MOD1_MNAR3_CalSlope <- MOD1_MNAR3_CalSlope[-2,] #exclude the CCA_imp_data because we'll be only comparing CCA_val + all_data_imp

#Plot AUC_mean for all 8 datasets for MODEL 1
ggplot(MOD1_MNAR3_CalSlope, aes(x = CalSlope_mean, y = dataset)) + geom_point() +    #plot AUC against datasets 
  geom_segment(aes(x = LCI, y = dataset, xend = UCI, yend = dataset))  


# #######################
# MODEL 2 CalSlope
# #######################
MOD2_MNAR3_CalSlope <- MNAR3_CalSlope %>% 
  filter(mod == 2) 

MOD2_MNAR3_CalSlope <- MOD2_MNAR3_CalSlope[-2,] 

ggplot(MOD2_MNAR3_CalSlope, aes(x = CalSlope_mean, y = dataset)) + geom_point() +     
  geom_segment(aes(x = LCI, y = dataset, xend = UCI, yend = dataset))  


# #######################
# MODEL 3 CalSlope
# #######################
MOD3_MNAR3_CalSlope <- MNAR3_CalSlope %>% 
  filter(mod == 3) 

MOD3_MNAR3_CalSlope <- MOD3_MNAR3_CalSlope[-2,] 

ggplot(MOD3_MNAR3_CalSlope, aes(x = CalSlope_mean, y = dataset)) + geom_point() +    
  geom_segment(aes(x = LCI, y = dataset, xend = UCI, yend = dataset))  



# #######################
# MODEL 4 CalSlope
# #######################
MOD4_MNAR3_CalSlope <- MNAR3_CalSlope %>% 
  filter(mod == 4) 

MOD4_MNAR3_CalSlope <- MOD4_MNAR3_CalSlope[-2,] 

ggplot(MOD4_MNAR3_CalSlope, aes(x = CalSlope_mean, y = dataset)) + geom_point() +    
  geom_segment(aes(x = LCI, y = dataset, xend = UCI, yend = dataset))  



# #######################
# ALL MODELS
# #######################
MNAR3_all_models_CalSlope <- rbind(MOD1_MNAR3_CalSlope, MOD2_MNAR3_CalSlope, MOD3_MNAR3_CalSlope, MOD4_MNAR3_CalSlope)
ggplot(MNAR3_all_models_CalSlope, aes(x = CalSlope_mean, y = dataset)) + geom_point() +
  geom_segment(aes(x=LCI, xend = UCI, y = dataset, yend = dataset)) +
  ggtitle("MNAR3: Y_prev = 0.1, R_prev = 0.5, gammas = 0.5, betas = non-zero") +
  facet_grid(~mod)


# ########################################################################################################################
# Try to plot all DAGs together 

all_DAGs_models_CalSlope <- rbind(MCAR_all_models_CalSlope, 
                            MAR_all_models_CalSlope, 
                            MNAR1_all_models_CalSlope, 
                            MNAR2_all_models_CalSlope, 
                            MNAR3_all_models_CalSlope)

combined_CalSlope_plot <- ggplot(all_DAGs_models_CalSlope, aes(x = CalSlope_mean, y = dataset)) + geom_point() +
  geom_segment(aes(x=LCI, xend = UCI, y = dataset, yend = dataset)) +
  ggtitle("Calibration slope estimates across Validation/Implementation datasets, imputation methods \n and missingness mechanisms, Y_prev = 0.1, R_prev = 0.5, gammas = 0.5") + 
  theme(plot.title = element_text(hjust = 0.5))

combined_CalSlope_plot + facet_grid(DAG_type ~ mod)



# ########################################################################################################################
# ########################################################################################################################
# ########################################### CALIBRATION INTERCEPT ######################################################
# ########################################################################################################################
# ########################################################################################################################
# ########################################################################################################################

# ####################################################################################################
#Scenario corresponding to MCAR DAG with fixed Y_prev = 0.1, R_prev = 0.5, gammas = 0.5, (betas = 0)
# ####################################################################################################

# #######################
# MODEL 1 Cal_Int
# #######################

MCAR_CalInt <- MCAR_1472_DAG_added %>% 
  group_by(dataset, mod, DAG_type) %>% #group by dataset and model
  summarise(CalInt_mean = mean(Cal_Int),  #get a summary of the mean AUC + CIs 
            LCI = quantile(Cal_Int, 0.025), 
            UCI = quantile(Cal_Int, 0.975))

MOD1_MCAR_CalInt <- MCAR_CalInt %>% 
  filter(mod == 1) #get AUCs only for model 1

MOD1_MCAR_CalInt <- MOD1_MCAR_CalInt[-2,] #exclude the CCA_imp_data because we'll be only comparing CCA_val + all_data_imp

#Plot AUC_mean for all 8 datasets for MODEL 1
ggplot(MOD1_MCAR_CalInt, aes(x = CalInt_mean, y = dataset)) + geom_point() +    #plot AUC against datasets 
  geom_segment(aes(x = LCI, y = dataset, xend = UCI, yend = dataset))  


# #######################
# MODEL 2 CalInt
# #######################
MOD2_MCAR_CalInt <- MCAR_CalInt %>% 
  filter(mod == 2) #get AUCs only for model 1

MOD2_MCAR_CalInt <- MOD2_MCAR_CalInt[-2,] #exclude the CCA_imp_data because we'll be only comparing CCA_val + all_data_imp

#Plot AUC_mean for all 8 datasets for MODEL 1
ggplot(MOD2_MCAR_CalInt, aes(x = CalInt_mean, y = dataset)) + geom_point() +    #plot AUC against datasets 
  geom_segment(aes(x = LCI, y = dataset, xend = UCI, yend = dataset))  



# #######################
# MODEL 3 CalInt
# #######################
MOD3_MCAR_CalInt <- MCAR_CalInt %>% 
  filter(mod == 3) #get AUCs only for model 1

MOD3_MCAR_CalInt <- MOD3_MCAR_CalInt[-2,] #exclude the CCA_imp_data because we'll be only comparing CCA_val + all_data_imp

#Plot AUC_mean for all 8 datasets for MODEL 1
ggplot(MOD3_MCAR_CalInt, aes(x = CalInt_mean, y = dataset)) + geom_point() +    #plot AUC against datasets 
  geom_segment(aes(x = LCI, y = dataset, xend = UCI, yend = dataset))  



# #######################
# MODEL 4 Cal_int
# #######################
MOD4_MCAR_CalInt <- MCAR_CalInt %>% 
  filter(mod == 4) #get AUCs only for model 1

MOD4_MCAR_CalInt <- MOD4_MCAR_CalInt[-2,] #exclude the CCA_imp_data because we'll be only comparing CCA_val + all_data_imp

#Plot AUC_mean for all 8 datasets for MODEL 1
ggplot(MOD4_MCAR_CalInt, aes(x = CalInt_mean, y = dataset)) + geom_point() +    #plot AUC against datasets 
  geom_segment(aes(x = LCI, y = dataset, xend = UCI, yend = dataset))  




# #######################
# ALL MODELS
# #######################
MCAR_all_models_CalInt <- rbind(MOD1_MCAR_CalInt, MOD2_MCAR_CalInt, MOD3_MCAR_CalInt, MOD4_MCAR_CalInt)
ggplot(MCAR_all_models_CalInt, aes(x = CalInt_mean, y = dataset)) + geom_point() +
  geom_segment(aes(x=LCI, xend = UCI, y = dataset, yend = dataset)) +
  ggtitle("MCAR: Y_prev = 0.1, R_prev = 0.5, gammas = 0.5, betas = 0") +
  facet_grid(~mod)



# ####################################################################################################
#Scenario corresponding to MAR DAG with fixed Y_prev = 0.1, R_prev = 0.5, gammas = 0.5, beta_X2 = 0.5
# ####################################################################################################

# #######################
# MODEL 1 O_E
# #######################
#first summarise the AUC (+CIs) for all models and all datasets 
MAR_CalInt <- MAR_1553_DAG_added %>% 
  group_by(dataset, mod, DAG_type) %>% #group by dataset and model
  summarise(CalInt_mean = mean(Cal_Int),  #get a summary of the mean AUC + CIs 
            LCI = quantile(Cal_Int, 0.025), 
            UCI = quantile(Cal_Int, 0.975))

MOD1_MAR_CalInt <- MAR_CalInt %>% 
  filter(mod == 1) #get AUCs only for model 1

MOD1_MAR_CalInt <- MOD1_MAR_CalInt[-2,] #exclude the CCA_imp_data because we'll be only comparing CCA_val + all_data_imp

#Plot AUC_mean for all 8 datasets for MODEL 1
ggplot(MOD1_MAR_CalInt, aes(x = CalInt_mean, y = dataset)) + geom_point() +    #plot AUC against datasets 
  geom_segment(aes(x = LCI, y = dataset, xend = UCI, yend = dataset))  


# #######################
# MODEL 2 O_E
# #######################
MOD2_MAR_CalInt <- MAR_CalInt %>% 
  filter(mod == 2) #get AUCs only for model 1

MOD2_MAR_CalInt <- MOD2_MAR_CalInt[-2,] #exclude the CCA_imp_data because we'll be only comparing CCA_val + all_data_imp

#Plot AUC_mean for all 8 datasets for MODEL 1
ggplot(MOD2_MAR_CalInt, aes(x = CalInt_mean, y = dataset)) + geom_point() +    #plot AUC against datasets 
  geom_segment(aes(x = LCI, y = dataset, xend = UCI, yend = dataset))  


# #######################
# MODEL 3 O_E
# #######################
MOD3_MAR_CalInt <- MAR_CalInt %>% 
  filter(mod == 3) #get AUCs only for model 1

MOD3_MAR_CalInt <- MOD3_MAR_CalInt[-2,] #exclude the CCA_imp_data because we'll be only comparing CCA_val + all_data_imp

#Plot AUC_mean for all 8 datasets for MODEL 1
ggplot(MOD3_MAR_CalInt, aes(x = CalInt_mean, y = dataset)) + geom_point() +    #plot AUC against datasets 
  geom_segment(aes(x = LCI, y = dataset, xend = UCI, yend = dataset))  



# #######################
# MODEL 4 O_E
# #######################
MOD4_MAR_CalInt <- MAR_CalInt %>% 
  filter(mod == 4) #get AUCs only for model 1

MOD4_MAR_CalInt <- MOD4_MAR_CalInt[-2,] #exclude the CCA_imp_data because we'll be only comparing CCA_val + all_data_imp


ggplot(MOD4_MAR_CalInt, aes(x = CalInt_mean, y = dataset)) + geom_point() +    #plot AUC against datasets 
  geom_segment(aes(x = LCI, y = dataset, xend = UCI, yend = dataset))   



# #######################
# ALL MODELS
# #######################
MAR_all_models_CalInt <- rbind(MOD1_MAR_CalInt, MOD2_MAR_CalInt, MOD3_MAR_CalInt, MOD4_MAR_CalInt)
ggplot(MAR_all_models_CalInt, aes(x = CalInt_mean, y = dataset)) + geom_point() +
  geom_segment(aes(x=LCI, xend = UCI, y = dataset, yend = dataset)) +
  ggtitle("MAR: Y_prev = 0.1, R_prev = 0.5, gammas = 0.5, beta_X2 = 0.5") +
  facet_grid(~mod)



# ########################################################################################################################
#Scenario corresponding to MNAR1 DAG with fixed Y_prev = 0.1, R_prev = 0.5, gammas = 0.5, beta_X1 = 0.5, beta_X2 = 0.5
# ########################################################################################################################


# #######################
# MODEL 1 O_E
# #######################
#first summarise the AUC (+CIs) for all models and all datasets 
MNAR1_CalInt <- MNAR1_1796_DAG_added %>% 
  group_by(dataset, mod, DAG_type) %>% #group by dataset and model
  summarise(CalInt_mean = mean(Cal_Int),  #get a summary of the mean AUC + CIs 
            LCI = quantile(Cal_Int, 0.025), 
            UCI = quantile(Cal_Int, 0.975))

MOD1_MNAR1_CalInt <- MNAR1_CalInt %>% 
  filter(mod == 1) #get AUCs only for model 1

MOD1_MNAR1_CalInt <- MOD1_MNAR1_CalInt[-2,] #exclude the CCA_imp_data because we'll be only comparing CCA_val + all_data_imp

#Plot AUC_mean for all 8 datasets for MODEL 1
ggplot(MOD1_MNAR1_CalInt, aes(x = CalInt_mean, y = dataset)) + geom_point() +    #plot AUC against datasets 
  geom_segment(aes(x = LCI, y = dataset, xend = UCI, yend = dataset))  


# #######################
# MODEL 2 O_E
# #######################
MOD2_MNAR1_CalInt <- MNAR1_CalInt %>% 
  filter(mod == 2) #get AUCs only for model 1

MOD2_MNAR1_CalInt <- MOD2_MNAR1_CalInt[-2,] #exclude the CCA_imp_data because we'll be only comparing CCA_val + all_data_imp

#Plot AUC_mean for all 8 datasets for MODEL 1
ggplot(MOD2_MNAR1_CalInt, aes(x = CalInt_mean, y = dataset)) + geom_point() +    #plot AUC against datasets 
  geom_segment(aes(x = LCI, y = dataset, xend = UCI, yend = dataset))  

# #######################
# MODEL 3 O_E
# #######################
MOD3_MNAR1_CalInt <- MNAR1_CalInt %>% 
  filter(mod == 3) #get AUCs only for model 1

MOD3_MNAR1_CalInt <- MOD3_MNAR1_CalInt[-2,] #exclude the CCA_imp_data because we'll be only comparing CCA_val + all_data_imp

#Plot AUC_mean for all 8 datasets for MODEL 1
ggplot(MOD3_MNAR1_CalInt, aes(x = CalInt_mean, y = dataset)) + geom_point() +    #plot AUC against datasets 
  geom_segment(aes(x = LCI, y = dataset, xend = UCI, yend = dataset))  



# #######################
# MODEL 4 O_E
# #######################
MOD4_MNAR1_CalInt <- MNAR1_CalInt %>% 
  filter(mod == 4) #get AUCs only for model 1

MOD4_MNAR1_CalInt <- MOD4_MNAR1_CalInt[-2,] #exclude the CCA_imp_data because we'll be only comparing CCA_val + all_data_imp

#Plot AUC_mean for all 8 datasets for MODEL 1
ggplot(MOD4_MNAR1_CalInt, aes(x = CalInt_mean, y = dataset)) + geom_point() +    #plot AUC against datasets 
  geom_segment(aes(x = LCI, y = dataset, xend = UCI, yend = dataset))  



# #######################
# ALL MODELS
# #######################
MNAR1_all_models_CalInt <- rbind(MOD1_MNAR1_CalInt, MOD2_MNAR1_CalInt, MOD3_MNAR1_CalInt, MOD4_MNAR1_CalInt)
ggplot(MNAR1_all_models_CalInt, aes(x = CalInt_mean, y = dataset)) + geom_point() +
  geom_segment(aes(x=LCI, xend = UCI, y = dataset, yend = dataset)) +
  ggtitle("MNAR1: Y_prev = 0.1, R_prev = 0.5, gammas = 0.5, beta_X1 = 0.5, beta_X2 = 0.5") +
  facet_grid(~mod)


# ########################################################################################################################
#Scenario corresponding to MNAR2 DAG with fixed Y_prev = 0.1, R_prev = 0.5, gammas = 0.5, beta_X2 = 0.5, beta_U = 0.5
# ########################################################################################################################


# #######################
# MODEL 1 O_E
# #######################

MNAR2_CalInt <- MNAR2_1580_DAG_added %>% 
  group_by(dataset, mod, DAG_type) %>% #group by dataset and model
  summarise(CalInt_mean = mean(Cal_Int),  #get a summary of the mean AUC + CIs 
            LCI = quantile(Cal_Int, 0.025), 
            UCI = quantile(Cal_Int, 0.975))

MOD1_MNAR2_CalInt <- MNAR2_CalInt %>% 
  filter(mod == 1) #get AUCs only for model 1

MOD1_MNAR2_CalInt <- MOD1_MNAR2_CalInt[-2,] #exclude the CCA_imp_data because we'll be only comparing CCA_val + all_data_imp

#Plot AUC_mean for all 8 datasets for MODEL 1
ggplot(MOD1_MNAR2_CalInt, aes(x = CalInt_mean, y = dataset)) + geom_point() +    #plot AUC against datasets 
  geom_segment(aes(x = LCI, y = dataset, xend = UCI, yend = dataset))  


# #######################
# MODEL 2 O_E
# #######################
MOD2_MNAR2_CalInt <- MNAR2_CalInt %>% 
  filter(mod == 2) 

MOD2_MNAR2_CalInt <- MOD2_MNAR2_CalInt[-2,] 

ggplot(MOD2_MNAR2_CalInt, aes(x = CalInt_mean, y = dataset)) + geom_point() +     
  geom_segment(aes(x = LCI, y = dataset, xend = UCI, yend = dataset))  


# #######################
# MODEL 3 O_E
# #######################
MOD3_MNAR2_CalInt <- MNAR2_CalInt %>% 
  filter(mod == 3) 

MOD3_MNAR2_CalInt <- MOD3_MNAR2_CalInt[-2,] 

ggplot(MOD3_MNAR2_CalInt, aes(x = CalInt_mean, y = dataset)) + geom_point() +    
  geom_segment(aes(x = LCI, y = dataset, xend = UCI, yend = dataset))  



# #######################
# MODEL 4 O_E
# #######################
MOD4_MNAR2_CalInt <- MNAR2_CalInt %>% 
  filter(mod == 4) 

MOD4_MNAR2_CalInt <- MOD4_MNAR2_CalInt[-2,] 

ggplot(MOD4_MNAR2_CalInt, aes(x = CalInt_mean, y = dataset)) + geom_point() +    
  geom_segment(aes(x = LCI, y = dataset, xend = UCI, yend = dataset))  



# #######################
# ALL MODELS
# #######################
MNAR2_all_models_CalInt <- rbind(MOD1_MNAR2_CalInt, MOD2_MNAR2_CalInt, MOD3_MNAR2_CalInt, MOD4_MNAR2_CalInt)
ggplot(MNAR2_all_models_CalInt, aes(x = CalInt_mean, y = dataset)) + geom_point() +
  geom_segment(aes(x=LCI, xend = UCI, y = dataset, yend = dataset)) +
  ggtitle("MNAR2: Y_prev = 0.1, R_prev = 0.5, gammas = 0.5, beta_X2 = 0.5, beta_U = 0.5") +
  facet_grid(~mod)








# ########################################################################################################################
#Scenario corresponding to MNAR3 DAG with fixed Y_prev = 0.1, R_prev = 0.5, gammas = 0.5, betas = non-zero
# ########################################################################################################################

# #######################
# MODEL 1 O_E
# #######################
#first summarise the AUC (+CIs) for all models and all datasets 
MNAR3_CalInt <- MNAR3_1823_DAG_added %>% 
  group_by(dataset, mod, DAG_type) %>% #group by dataset and model
  summarise(CalInt_mean = mean(Cal_Int),  #get a summary of the mean AUC + CIs 
            LCI = quantile(Cal_Int, 0.025), 
            UCI = quantile(Cal_Int, 0.975))

MOD1_MNAR3_CalInt <- MNAR3_CalInt %>% 
  filter(mod == 1) #get AUCs only for model 1

MOD1_MNAR3_CalInt <- MOD1_MNAR3_CalInt[-2,] #exclude the CCA_imp_data because we'll be only comparing CCA_val + all_data_imp

#Plot AUC_mean for all 8 datasets for MODEL 1
ggplot(MOD1_MNAR3_CalInt, aes(x = CalInt_mean, y = dataset)) + geom_point() +    #plot AUC against datasets 
  geom_segment(aes(x = LCI, y = dataset, xend = UCI, yend = dataset))  


# #######################
# MODEL 2 O_E
# #######################
MOD2_MNAR3_CalInt <- MNAR3_CalInt %>% 
  filter(mod == 2) 

MOD2_MNAR3_CalInt <- MOD2_MNAR3_CalInt[-2,] 

ggplot(MOD2_MNAR3_CalInt, aes(x = CalInt_mean, y = dataset)) + geom_point() +     
  geom_segment(aes(x = LCI, y = dataset, xend = UCI, yend = dataset))  


# #######################
# MODEL 3 O_E
# #######################
MOD3_MNAR3_CalInt <- MNAR3_CalInt %>% 
  filter(mod == 3) 

MOD3_MNAR3_CalInt <- MOD3_MNAR3_CalInt[-2,] 

ggplot(MOD3_MNAR3_CalInt, aes(x = CalInt_mean, y = dataset)) + geom_point() +    
  geom_segment(aes(x = LCI, y = dataset, xend = UCI, yend = dataset))  



# #######################
# MODEL 4 O_E
# #######################
MOD4_MNAR3_CalInt <- MNAR3_CalInt %>% 
  filter(mod == 4) 

MOD4_MNAR3_CalInt <- MOD4_MNAR3_CalInt[-2,] 

ggplot(MOD4_MNAR3_CalInt, aes(x = CalInt_mean, y = dataset)) + geom_point() +    
  geom_segment(aes(x = LCI, y = dataset, xend = UCI, yend = dataset))  



# #######################
# ALL MODELS
# #######################
MNAR3_all_models_CalInt <- rbind(MOD1_MNAR3_CalInt, MOD2_MNAR3_CalInt, MOD3_MNAR3_CalInt, MOD4_MNAR3_CalInt)
ggplot(MNAR3_all_models_CalInt, aes(x = CalInt_mean, y = dataset)) + geom_point() +
  geom_segment(aes(x=LCI, xend = UCI, y = dataset, yend = dataset)) +
  ggtitle("MNAR3: Y_prev = 0.1, R_prev = 0.5, gammas = 0.5, betas = non-zero") +
  facet_grid(~mod)


# ########################################################################################################################
# Try to plot all DAGs together 

all_DAGs_models_CalInt <- rbind(MCAR_all_models_CalInt, 
                                  MAR_all_models_CalInt, 
                                  MNAR1_all_models_CalInt, 
                                  MNAR2_all_models_CalInt, 
                                  MNAR3_all_models_CalInt)

combined_CalInt_plot <- ggplot(all_DAGs_models_CalInt, aes(x = CalInt_mean, y = dataset)) + geom_point() +
  geom_segment(aes(x=LCI, xend = UCI, y = dataset, yend = dataset)) +
  ggtitle("Calibration intercept estimates across Validation/Implementation datasets, imputation methods \n and missingness mechanisms, Y_prev = 0.1, R_prev = 0.5, gammas = 0.5") + 
  theme(plot.title = element_text(hjust = 0.5))

combined_CalInt_plot + facet_grid(DAG_type ~ mod)


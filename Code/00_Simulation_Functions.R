####-----------------------------------------------------------------------------------------
## Define a function to repeat the simulation across all iterations (for a given scenario)
####-----------------------------------------------------------------------------------------

simulation_nrun_fnc <- function(n_iter,
                                N_dev,
                                N_imp, 
                                N_val, 
                                Y_prev,
                                X_categorical, 
                                R_prev,
                                beta_x1, 
                                beta_x2, 
                                beta_U,  
                                gamma_x1, 
                                gamma_x2, 
                                gamma_U)

{
  
  ## Define an empty variable, which will be used to store the results across all iterations
  results <- NULL
  
  ## Repeat the simulation across iter number of iterations
  #number of iterations n_iter
  #the single run function is what calls all the functions for a given set of parameters, so i dont need to have all the fncs 
  #repeated in a function, they are separate. Think of the single run function as the flow diagram in the protocol. 
  
  for (iter in 1:n_iter) {
    simulation_results <- simulation_singlerun_fnc(N_dev = N_dev,
                                                   N_imp = N_imp, 
                                                   N_val = N_val, 
                                                   Y_prev = Y_prev,
                                                   X_categorical = X_categorical, 
                                                   R_prev = R_prev,
                                                   beta_x1 = beta_x1, 
                                                   beta_x2 = beta_x2, 
                                                   beta_U = beta_U,  
                                                   gamma_x1 = gamma_x1, 
                                                   gamma_x2 = gamma_x2, 
                                                   gamma_U = gamma_U)
    
    results <- results %>%
      bind_rows(as_tibble(simulation_results) %>% 
                  mutate("Iteration" = iter, .before = "dataset")) 
    
    print(paste("iteration ", iter, " complete", sep = ""))
  }
  # write_rds(results, file = "test.rds")
  
  return(results)
}

#-------------------------------------------------------------------------------


####---------------------------------------------
## Function that gives a single run per scenario
####---------------------------------------------

simulation_singlerun_fnc <- function(N_dev,
                                     N_imp, 
                                     N_val, 
                                     Y_prev,
                                     X_categorical, 
                                     R_prev,
                                     beta_x1, 
                                     beta_x2, 
                                     beta_U,  
                                     gamma_x1, 
                                     gamma_x2, 
                                     gamma_U) {
  #install.packages('Rcpp')
  #install.packages('tidyverse')
  #install.packages('dplyr')
  #install.packages('magrittr')
  #install.packages('mice')
  
  library(ggplot2)
  library(dplyr)
  library(magrittr)
  library(Rcpp)
  library(mice)
  library(tidyverse)
  library(splines)
  
  
  #1.dev_data function------------
  dev_data <- dev_data_simulation_function(N_dev = N_dev,
                                           X_categorical = X_categorical,
                                           beta_x1 = beta_x1,
                                           beta_x2 = beta_x2,
                                           beta_U = beta_U,
                                           gamma_x1 = gamma_x1,
                                           gamma_x2 = gamma_x2,
                                           gamma_U = gamma_U,
                                           Y_prev = Y_prev)
  
  
  #2.val_imp_data function-------------
  df <- simulation_function(N_imp = N_imp, 
                            N_val = N_val, 
                            X_categorical = X_categorical, 
                            Y_prev = Y_prev, 
                            R_prev = R_prev, 
                            beta_x1 = beta_x1, 
                            beta_x2 = beta_x2, 
                            beta_U = beta_U,
                            gamma_x1 = gamma_x1,
                            gamma_x2 = gamma_x2,
                            gamma_U = gamma_U) 
  
  
  #3.master imputation_function-----------------------
  imputed_datasets <- imputation_function(df = df, 
                                          m = 5)
  
  #4.dev_mod_function------------------
  models <- dev_mod_function(dev_data = dev_data, 
                             delta_0 = 1, 
                             delta_1 = 0.8, 
                             delta_2 = 0.5)
  
  #6.val_imp_mode_function------------------------------------------------
  Target_measures <- val_imp_mod_function(imputed_datasets = imputed_datasets, 
                                          models = models)

  return(Target_measures)
  
} 

####---------------------------------------------
## 0. Function that takes the inverse logistic link
####---------------------------------------------

expit_function <- function(x) {   
  return(1 / (1 + exp(-x)))
}


####---------------------------------------------
## 1. Development data generation function
####---------------------------------------------

dev_data_simulation_function <- function(
  N_dev,
  X_categorical, 
  beta_x1,
  beta_x2,
  beta_U,
  gamma_x1,
  gamma_x2,
  gamma_U,
  Y_prev) 
  
  {
  
  
  if(X_categorical == FALSE) {
    dev_data_IPD <- tibble("x_1" = rnorm(n = N_dev, mean = 0, sd = 1)
                       ,"x_2" = rnorm(N_dev, mean = 0, sd = 1)
                       , "U" = rnorm(N_dev, mean = 0, sd = 1)
                       , "ID" = 1:N_dev)
  }
  
  else{
    dev_data_IPD <- tibble("x_1" = rbinom(n = N_dev, size = 1, prob = 0.1) 
                  , "x_2" = rnorm(N_dev, mean = 0, sd = 1) 
                  , "U" = rnorm(N_dev, mean = 0, sd = 1) 
                  , "ID" = 1:N_dev
    )
  }
  
  
  #determine the prevalence of the outcome based on the gamma_0
  
  gamma_0 <- as.numeric(coef(glm(rbinom(N_dev, 1, prob = Y_prev) ~ offset(gamma_x1*x_1 + gamma_x2*x_2 + gamma_U*U), 
                      family = binomial(link = "logit"),
                      data = dev_data_IPD))[1])
  
  dev_data_IPD$Y = rbinom(N_dev, size = 1, 
                          prob = expit_function(gamma_0 + gamma_x1*dev_data_IPD$x_1 + gamma_x2*dev_data_IPD$x_2 + gamma_U*dev_data_IPD$U))
  
  return(dev_data_IPD)
  
}


####------------------------------------------------------
## 2. Validation and implementation data generation function
####------------------------------------------------------

simulation_function <- function(N_imp, 
                                N_val, 
                                Y_prev,
                                X_categorical, 
                                R_prev,
                                beta_x1, 
                                beta_x2, 
                                beta_U,  
                                gamma_x1, 
                                gamma_x2, 
                                gamma_U) {
  
  N <- N_imp + N_val

  if(X_categorical == FALSE) {
    IPD <- tibble("x_1" = rnorm(n = N, mean = 0, sd = 1) #X1 is continuous and normally distributed
                ,"x_2" = rnorm(N, mean = 0, sd = 1) #X2 is continuous and normally distributed
                ,"U" = rnorm(N, mean = 0, sd = 1) #U is continuous and normally distributed but it will be unobserved
                ,"ID" = 1:N
    )
    
  } else{
    IPD <- tibble("x_1" = rbinom(n = N, size = 1, prob = 0.1) #X1 is binary (change value of prob to change prevalance of x1=1)
                  , "x_2" = rnorm(N, mean = 0, sd = 1) #X2 is continuous and normally distributed
                  , "U" = rnorm(N, mean = 0, sd = 1) #U is continuous and normally distributed but it will be unobserved
                  , "ID" = 1:N
    )
  }

  
  #determine the prevalence of R through beta_0
  beta_0 <- as.numeric(coef(glm(rbinom(N, 1, prob = R_prev) ~ offset(beta_x1*x_1 + beta_x2*x_2 + beta_U*U), #R_prev is the missingness level so we control that? We control this by the Y_prev(the number of times R1 is 1 or 0)
                                           family = binomial(link = "logit"),
                                           data = IPD))[1])
  
  IPD$Y = rbinom(N, size = 1, 
                    prob = expit_function(beta_0 + beta_x1*IPD$x_1 + beta_x2*IPD$x_2 + beta_U*IPD$U))
  
  IPD$R_1 = rbinom(N, size = 1, 
               prob = expit_function(beta_x1*IPD$x_1 + beta_x2*IPD$x_2 + beta_U*IPD$U)) 
  
  
  observed_data <- IPD %>%
    mutate(x_1 = ifelse(R_1 == 1, x_1, NA)) 
  
  implementation_data <- observed_data %>% 
    sample_n(N_imp, replace = FALSE)
  
  validation_data <- observed_data %>% 
    filter(ID %in% implementation_data$ID == FALSE) %>%
    sample_n(N_val, replace = FALSE)
  
  fully_observed_imp_data <- IPD %>%
    filter(ID %in% implementation_data$ID == TRUE) 
  
  
  
  return(list("IPD" = IPD, 
              "observed_data" = observed_data, 
              "implementation_data" = implementation_data, 
              "validation_data" = validation_data,
              "fully_observed_imp_data" = fully_observed_imp_data)) 
  
} 



####---------------------------------------------
## Multiple imputation function
####---------------------------------------------

mice_function <- function(df, m = 5, Y) {
  
  dummyrun <- mice(df, m = 1, maxit = 0) 
  predmat <- dummyrun$predictorMatrix 
  
  if (Y ==FALSE){        #if we don't want to include the Y, then we set it to be 0.
    predmat["Y",] <- 0 
    predmat[,"Y"] <- 0
  }
  predmat[,"ID"] <- 0  #we dont want MI to impute the missing values based on ID, U or R1 hence they're set to be 0
  predmat["ID",] <- 0
  predmat[,"U"] <- 0
  predmat["U",] <- 0
  predmat[,"R_1"] <- 0
  predmat["R_1",] <- 0
  mice(df, m, predictorMatrix = predmat, printFlag = FALSE) 
  
}


####---------------------------------------------
## Mean imputation function
####---------------------------------------------

mean_function <- function(df) {
  mean_imputed_df <- df
  
  if(is.factor(df$x_1) == TRUE) { #if X1 is categorical, the function will fail instead of imputing
    stop("mean_function only relevant for X_categorical == FALSE")
  } else {   
    mean_imputed_df <- mean_imputed_df %>%
      mutate(across(starts_with("x_"), 
                    ~tidyr::replace_na(.x,
                                       mean(.x, na.rm = TRUE))))
  }
  mean_imputed_df
}


####-------------
## CCA function
####-------------
CCA_function <- function(df) {
  df[complete.cases(df), ]
}

####----------------------------------------------------------
## Missing values considered normal (zero imputation) function
####----------------------------------------------------------
imputed_by_zero_function <- function(df) {
  if(is.factor(df$x_1) == TRUE) {
    df$x_1[is.na(df$x_1)] <- 0
  } else if (is.factor(df$x_1) == FALSE) {   #if X1 is continuous, the function will fail instead of imputing
     stop("imputed_by_zero_function only relevant for X_categorical == TRUE")
    }
  df 
}

  
####--------------------------
## 3. Master imputation function
####--------------------------

imputation_function <- function(df, m = 5) {
  
  MI_val_data_noY <- mice_function(df$validation_data, m = m, Y = FALSE) 
  MI_val_data_withY <- mice_function(df$validation_data, m = m, Y = TRUE)
  MI_imp_data_noY <- mice_function(df$implementation_data, m = m, Y = FALSE)
  MI_imp_data_withY <- mice_function(df$implementation_data, m = m, Y = TRUE)

  CCA_val_data <- CCA_function(df$validation_data)
  CCA_imp_data <- CCA_function(df$implementation_data)
  
  all_data_imp <- df$fully_observed_imp_data
  
  if(is.factor(df$validation_data$x_1) == TRUE) {
    # if X1 is categorical, then perform missing value is risk factor absent (no mean imputation here)
    zero_val_data <- imputed_by_zero_function(df$validation_data) 
    zero_imp_data <- imputed_by_zero_function(df$implementation_data)
    return(list(
      "MI_val_data_noY" = MI_val_data_noY,
      "MI_val_data_withY" = MI_val_data_withY,
      "MI_imp_data_noY" = MI_imp_data_noY,
      "MI_imp_data_withY" = MI_imp_data_withY,
      "CCA_val_data" = CCA_val_data,
      "CCA_imp_data" = CCA_imp_data,
      "zero_val_data" = zero_val_data,
      "zero_imp_data" = zero_imp_data,
      "all_data_imp" = all_data_imp 
    ))
    
  }else{
    # if X1 is continuous, then perform mean imputaion (no risk factor absent imputation)
    mean_val <- mean_function(df$validation_data)
    mean_imp <- mean_function(df$implementation_data)
    return(list(
      "MI_val_data_noY" = MI_val_data_noY,
      "MI_val_data_withY" = MI_val_data_withY,
      "MI_imp_data_noY" = MI_imp_data_noY,
      "MI_imp_data_withY" = MI_imp_data_withY,
      "mean_val" = mean_val,
      "mean_imp" = mean_imp,
      "CCA_val_data" = CCA_val_data,
      "CCA_imp_data" = CCA_imp_data,
      "all_data_imp" = all_data_imp 
    ))
  }
} 


####---------------------------------------
## 4. Model fitting function
####---------------------------------------

dev_mod_function <- function(dev_data, delta_0, delta_1, delta_2) {
  
  
  model_1 <- glm(Y ~ x_1 + x_2, data = dev_data, family = binomial) 
  
  #obtaining coeff for fi_0, fi_1, fi_2 
  fi_0 <- coef(model_1)[1]
  fi_x1 <- coef(model_1)[2]
  fi_x2 <- coef(model_1)[3]
  
  
  
  ## Apply model (ii) which over-predicts (mis-calibration in the large)
  ####-----------------------------------------------------------------------------------------
  delta_0 <- 1
  delta_1 <- 0.8
  delta_2 <- 0.5
  
  
  model_2 <- model_1 
  model_2$coefficients[1] <- fi_0 + delta_0 
  model_2$linear.predictors <- model_2$linear.predictors + delta_0
  model_2$fitted.values <- expit_function(model_2$linear.predictors)
  
  
  ## Apply model (iii) which under-predicts (change the overall slope, multiply all fi's by some factor)
  ####----------------------------------------------------------------------------------------------------
  model_3 <- model_1
  model_3$coefficients <- model_3$coefficients * delta_1
  model_3$linear.predictors <- model_3$linear.predictors * delta_1
  model_3$fitted.values <- expit_function(model_3$linear.predictors)
  
  ## Apply model (iv) poor discrimination (all the fi's are incorrect)
  ####----------------------------------------------------------------------------------------------------
  model_4 <- model_1
  model_4$coefficients[2] <- fi_x1 * delta_2
  model_4$linear.predictors <- fi_0 + (fi_x1*dev_data$x_1*delta_2)  + (fi_x2*dev_data$x_2)
  model_4$fitted.values <- expit_function(model_4$linear.predictors)
  
  return(list(
    model_1,
    model_2,
    model_3,
    model_4)
  )
  
}


####------------------------------------------------------
##  5. Function that returns prediction for a single dataset
####-----------------------------------------------------
predict_single_imputed <- function(imputed_datasets, models) {
  
  if(is.mids(imputed_datasets) == FALSE){ #is.mids checking whether the object is mice or not 
    
    output_predictions <- data.frame("Y" = imputed_datasets$Y) #defining the Y as the Y column from the imputed datasets
    preds <- as_tibble(sapply(models, 
                              predict, 
                              newdata = imputed_datasets, 
                              type = 'response')) #sapply and lapply apply a function over a list or vector
    names(preds) <- c(paste("Predictions_Model_", 1:length(models), sep='')) #Retrieve or set the row or column names of a matrix-like object
  }else {
    MI_long <- mice::complete(imputed_datasets, action = 'long') #extracts imputed datasets from a 'mids' object
    
    output_predictions <- data.frame("Y" = MI_long$Y)
    
    preds <- as_tibble(sapply(models, 
                              predict, 
                              newdata = MI_long, 
                              type = 'response'))
    names(preds) <- c(paste("Predictions_Model_", 1:length(models), sep=''))
  }
  
  
  output_predictions <- output_predictions %>% #binding the columns together into a table 
    bind_cols(preds)
  
  
  return(output_predictions)
 }

####-----------------------------------------------------------
## 6. Function that applies all 4 models to all imputed datasets
####-----------------------------------------------------------
val_imp_mod_function <- function(imputed_datasets, models) {
  
  preds_per_data_set <-  map(imputed_datasets, predict_single_imputed, models = models)
  
  target_measures <- c()
  
  for( i in seq_along(preds_per_data_set)){
    
    df <- eval(parse(text=(paste("preds_per_data_set$",names(preds_per_data_set[i]), sep=""))))
    
    target_measures1 <- as.data.frame(c(dataset = names(preds_per_data_set)[i], mod = 1, predictive.performance.function(Y = df$Y, 
                                                                                                                         Predicted_Risks = df$Predictions_Model_1)))
    target_measures2 <- as.data.frame(c(dataset = names(preds_per_data_set)[i], mod = 2, predictive.performance.function(Y = df$Y, 
                                                                                                                         Predicted_Risks = df$Predictions_Model_2)))
    target_measures3 <- as.data.frame(c(dataset = names(preds_per_data_set)[i], mod = 3, predictive.performance.function(Y = df$Y, 
                                                                                                                         Predicted_Risks = df$Predictions_Model_3)))
    target_measures4 <- as.data.frame(c(dataset = names(preds_per_data_set)[i], mod = 4, predictive.performance.function(Y = df$Y, 
                                                                                                                         Predicted_Risks = df$Predictions_Model_4)))
    
    target_measures <- rbind(target_measures, target_measures1, target_measures2, target_measures3, target_measures4)
    
  }
  
  return(target_measures)
  
}


####---------------------------------------------------------------
## 7. Function to calculate the predictive performance of the models
####---------------------------------------------------------------
predictive.performance.function <- function(Y, Predicted_Risks) {
  
  library(pROC)
  
  #Input: 
  # Y = a binary variable of observed outcomes
  # Predicted_Risks = a vector of predicted risks for each dataset
  
  #calculate the performance of each column (4 sets of predictive performance results per datasets) for loop
  #for each model within a given dataset, we want a table with the target measures (use dataframe from above)
  
  ## Calculate Brier Score (mean square error of predictions; the lower, the better)
  ####------------------------------------------------------------------
  Brier <- 1/length(Y) * (sum((Predicted_Risks - Y)^2))
  
  ## Calibration intercept (i.e. calibration-in-the-large)
  ####-------------------------------------------------------------------------------
  LP <- log(Predicted_Risks/ (1 - Predicted_Risks)) 
  Cal_Int <- glm(Y ~ offset(LP), family = binomial(link = "logit"))
  
  ## Calibration slope
  ####--------------------------------------------------------------------------
  Cal_Slope <- glm(Y ~ LP, family = binomial(link = "logit"))
  
  ## Observed-vs-Expected ratio
  ####------------------------------------------------------------------------
  
   Observed_outcome <- (mean(Y))
   Expected_outcome <- mean(Predicted_Risks)
  
   O_E <- Observed_outcome / Expected_outcome
    
    ## Discrimination (c-statistic?)
    ####------------------------------------------------------------------------
  AUC <- as.numeric(roc(response = Y, 
                        predictor = as.vector(Predicted_Risks),
                        direction = "<",
                        levels = c(0,1))$auc)
  

  ## Store performance results in a data.frame and return
  ####------------------------------------------------------------------------
  Target_measures <- data.frame("Cal_Int" = as.numeric(coef(Cal_Int)),
                                "Cal_Slope" = as.numeric(coef(Cal_Slope)[2]),
                                "O_E" = O_E,
                                "AUC" = AUC,
                                "Brier" = Brier)
  
  
  return(Target_measures)
  
}







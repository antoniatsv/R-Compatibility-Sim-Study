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
## Define a function to repeat the simulation across all iterations 
####----------------------------------------------------------------------------
simulation_nrun_fnc <- function(n_iter, 
                                N_dev,
                                N_val, 
                                Y_prev,
                                X_categorical, 
                                R_prev,
                                beta_x1_dev, 
                                beta_x2_dev, 
                                beta_U_dev, 
                                beta_x1_val, 
                                beta_x2_val, 
                                beta_U_val, 
                                rho_X,
                                gamma_x1, 
                                gamma_x2, 
                                gamma_U) {
  #Input: n_iter = the number of iterations to repeat the simulation over
  #       N_dev = size of the data to simulate as the development set
  #       N_val = size of the data to simulate as the validation set
  #       Y_prev = the overall event rate in the population
  #       X_categorical = TRUE/FALSE whether X1 is simulated as categorical
  #       R_prev = proportion of missing data induced
  #       beta_x1_dev = association between X1 and prob of missing data in the
  #                     development dataset
  #       beta_x2_dev = association between X2 and prob of missing data in the
  #                     development dataset
  #       beta_U_dev = association between U (unmeasured variable) and prob of 
  #                     missing data in the development dataset
  #       beta_x1_val = association between X1 and prob of missing data in the
  #                     validation dataset
  #       beta_x2_val = association between X2 and prob of missing data in the
  #                     validation dataset
  #       beta_U_val = association between U (unmeasured variable) and prob of 
  #                     missing data in the validation dataset
  #       rho_X = the correlation between X1 and X2 - if X1 is categorical, the 
  #               inputted correlation is between the latent variable Z1 and 
  #               X2 where Z1 is then used to generate X1 at a set prevalence 
  #               (see paper for details)
  #       gamma_x1 = association between X1 and prob of outcome
  #       gamma_x2 = association between X2 and prob of outcome
  #       gamma_U = association between U (unmeasured variable) and prob of 
  #                 outcome
  
  library(MASS)
  library(tidyverse)
  library(mice)
  library(pROC)
  
  ## Define an empty variable, which will be used to store the results across
  ## all iterations
  results <- NULL
  
  ## Repeat the simulation across iter number of iterations
  for (iter in 1:n_iter) {
    simulation_results <- simulation_singlerun_fnc(N_dev = N_dev,
                                                   N_val = N_val, 
                                                   Y_prev = Y_prev,
                                                   X_categorical = X_categorical, 
                                                   R_prev  =R_prev,
                                                   beta_x1_dev = beta_x1_dev, 
                                                   beta_x2_dev = beta_x2_dev, 
                                                   beta_U_dev = beta_U_dev, 
                                                   beta_x1_val = beta_x1_val, 
                                                   beta_x2_val = beta_x2_val, 
                                                   beta_U_val = beta_U_val, 
                                                   rho_X = rho_X,
                                                   gamma_x1 = gamma_x1, 
                                                   gamma_x2 = gamma_x2, 
                                                   gamma_U = gamma_U)
    
    results <- results %>%
      dplyr::bind_rows(tibble::as_tibble(simulation_results) %>% 
                         dplyr::mutate("Iteration" = iter, .before = "CPM")) 
    
    rm(simulation_results)
  }
  ## Return results from the simulation across all iterations
  return(results)
}



####----------------------------------------------------------------------------
## Function that gives a single run per scenario
####----------------------------------------------------------------------------
simulation_singlerun_fnc <- function(N_dev,
                                     N_val, 
                                     Y_prev,
                                     X_categorical, 
                                     R_prev,
                                     beta_x1_dev, 
                                     beta_x2_dev, 
                                     beta_U_dev, 
                                     beta_x1_val, 
                                     beta_x2_val, 
                                     beta_U_val, 
                                     rho_X,
                                     gamma_x1, 
                                     gamma_x2, 
                                     gamma_U) {
  
  # Generate the data according to the DGM--------------------------------------
  gen_data <- DGM_fnc(N_dev = N_dev,
                      N_val = N_val, 
                      Y_prev = Y_prev,
                      X_categorical = X_categorical, 
                      R_prev  =R_prev,
                      beta_x1_dev = beta_x1_dev, 
                      beta_x2_dev = beta_x2_dev, 
                      beta_U_dev = beta_U_dev, 
                      beta_x1_val = beta_x1_val, 
                      beta_x2_val = beta_x2_val, 
                      beta_U_val = beta_U_val, 
                      rho_X = rho_X,
                      gamma_x1 = gamma_x1, 
                      gamma_x2 = gamma_x2, 
                      gamma_U = gamma_U) 
  
  # Impute missing data in the development and validation data under
  # different methods ----------------------------------------------------------
  imputed_datasets <- imputation_fnc(df = gen_data, 
                                     m = 5,
                                     X_categorical = X_categorical)
  
  
  # Fit the prediction models to the development data, under
  # each imputation method------------------------------------------------------
  model_coefs <- dev_mod_fnc(imputed_datasets = imputed_datasets,
                             X_categorical = X_categorical)
  
 
  # Apply the models (from the development data) to the imputed validation
  # data under each different imputation methods, and estimate
  # predictive performance -----------------------------------------------------
  
  #extract each imputed version of the validation set
  val_imputed_datasets <- imputed_datasets[
    which(stringr::str_detect(names(imputed_datasets), "_val"))
    ]
  
  preds_per_data_set <- purrr::map(.x = val_imputed_datasets,
                                   .f = make_predictions_fnc,
                                   model_coefs = model_coefs,
                                   X_categorical = X_categorical)
  preds_per_data_set$RI_val_transportedimputation <- preds_per_data_set$RI_val_transportedimputation %>%
    dplyr::select(-.imp)
  preds_per_data_set$RI_val_newimputation <- preds_per_data_set$RI_val_newimputation %>%
    dplyr::select(-.imp)
  
  #calculate predictive performance of each model in each imputed dataset:
  if(X_categorical){
    CPM_names <- c("PR_fullyobserved",
                   "PR_CCA",
                   "PR_RI",
                   "PR_MInoY",
                   "PR_MIwithY",
                   "PR_patternsubmodel",
                   "PR_zero")
  }else {
    CPM_names <- c("PR_fullyobserved",
                   "PR_CCA",
                   "PR_RI",
                   "PR_MInoY",
                   "PR_MIwithY",
                   "PR_patternsubmodel",
                   "PR_mean")
  }
  
  Target_measures <- NULL
  #Apply each developed CPM to every imputed validation dataset:
  for(i in 1:length(CPM_names)) {
    Target_measures <- Target_measures %>%
      dplyr::bind_rows(purrr::map_dfr(.x = preds_per_data_set[-which(names(preds_per_data_set)=="observed_val")],
                                      .f = validation_fnc,
                                      PR_name = CPM_names[i],
                                      .id = "Validation_Dataset") %>%
                         dplyr::mutate("CPM" = CPM_names[i],
                                       .before = "Validation_Dataset"))
  }
  #Apply the sub-model to the raw (non-imputed) validation data:
  Target_measures <- Target_measures %>%
    dplyr::bind_rows(purrr::map_dfr(.x = preds_per_data_set[which(names(preds_per_data_set)=="observed_val")],
                                    .f = validation_fnc,
                                    PR_name = "PR_patternsubmodel",
                                    .id = "Validation_Dataset") %>%
                       dplyr::mutate("CPM" = "PR_patternsubmodel",
                                     .before = "Validation_Dataset"))
  
  Target_measures
} 



####----------------------------------------------------------------------------
## Data generation mechanism function
####----------------------------------------------------------------------------
DGM_fnc <- function(N_dev,
                    N_val, 
                    Y_prev,
                    X_categorical, 
                    R_prev,
                    beta_x1_dev, 
                    beta_x2_dev, 
                    beta_U_dev, 
                    beta_x1_val, 
                    beta_x2_val, 
                    beta_U_val, 
                    rho_X,
                    gamma_x1, 
                    gamma_x2, 
                    gamma_U) {
  
  N <- N_dev + N_val #total sample size to generate
  
  #generate (potentially correlated) latent predictor variables from a MVN:
  Z <- MASS::mvrnorm(n = N, mu = c(0,0), Sigma = toeplitz(c(1, rho_X)))
  
  #generate an "unmeasured variable" U:
  U <- rnorm(N, mean = 0, sd = 1)
  
  if(X_categorical == FALSE) {
    #if continuous use the latent variables directly as X
    IPD <- tibble::tibble("ID" = 1:N,
                          "x_1" = Z[,1], 
                          "x_2" = Z[,2], 
                          "U" = U)
  } else{
    #if binary convert the latent variable to achieve binary variable prevalence
    IPD <- tibble::tibble("ID" = 1:N,
                          "x_1" = ifelse(Z[,1]>=qnorm(0.3, 
                                                      lower.tail = FALSE), 
                                         1, 0), 
                          "x_2" = Z[,2], 
                          "U" = U)
  }
  
  #calculate gamma_0 to give inputted prevalence of the outcome:
  gamma_0 <- as.numeric(coef(glm(rbinom(N, 1, prob = Y_prev) ~ 
                                   offset((gamma_x1*x_1) + 
                                            (gamma_x2*x_2) + 
                                            (gamma_U*U)), 
                                 family = binomial(link = "logit"),
                                 data = IPD))[1])
  #simulate outcomes based on the data generating model:
  IPD$Y <- rbinom(N, 
                  size = 1, 
                  prob = expit_fnc(gamma_0 + 
                                     (gamma_x1*IPD$x_1) + 
                                     (gamma_x2*IPD$x_2) + 
                                     (gamma_U*IPD$U)))
  
  #separate IPD into development and validation data
  dev_IPD <- IPD %>% 
    dplyr::slice_sample(n = N_dev, replace = FALSE)
  val_IPD <- IPD %>% 
    dplyr::filter(ID %in% dev_IPD$ID == FALSE)
  if((nrow(dev_IPD) + nrow(val_IPD) != N) |
     (all(IPD$ID %in% c(dev_IPD$ID, val_IPD$ID)) == FALSE)) {
    stop("Error in DGM data sampling")
  }
  
  #calculate beta_0_dev and beta_0_val to give inputted prevalence of 
  #missing data:
  beta_0_dev <- as.numeric(coef(glm(rbinom(N_dev, 1, prob = R_prev) ~ 
                                      offset((beta_x1_dev*x_1) +
                                               (beta_x2_dev*x_2) +
                                               (beta_U_dev*U)), 
                                family = binomial(link = "logit"),
                                data = dev_IPD))[1])
  beta_0_val <- as.numeric(coef(glm(rbinom(N_val, 1, prob = R_prev) ~ 
                                      offset((beta_x1_val*x_1) +
                                               (beta_x2_val*x_2) +
                                               (beta_U_val*U)), 
                                    family = binomial(link = "logit"),
                                    data = val_IPD))[1])
  
  ##Induce missing data into development and validation data based on 
  ##missingness mechanism:
  dev_IPD$R_1 <- rbinom(N_dev, 
                        size = 1, 
                        prob = expit_fnc(beta_0_dev + 
                                           (beta_x1_dev*dev_IPD$x_1) + 
                                           (beta_x2_dev*dev_IPD$x_2) + 
                                           (beta_U_dev*dev_IPD$U))) 
  
  
  val_IPD$R_1 <- rbinom(N_val, 
                        size = 1, 
                        prob = expit_fnc(beta_0_val + 
                                           (beta_x1_val*val_IPD$x_1) + 
                                           (beta_x2_val*val_IPD$x_2) + 
                                           (beta_U_val*val_IPD$U)))  
  
  observed_dev_IPD <- dev_IPD %>%
    dplyr::mutate(x_1 = ifelse(R_1 == 1, x_1, NA))
  
  observed_val_IPD <- val_IPD %>%
    dplyr::mutate(x_1 = ifelse(R_1 == 1, x_1, NA))
  
  return(list("fully_observed_dev_data" = dev_IPD, 
              "fully_observed_val_data" = val_IPD,
              "observed_dev_data" = observed_dev_IPD, 
              "observed_val_data" = observed_val_IPD))
}

####----------------------------------------------------------------------------
## Function that takes the inverse logistic link
####----------------------------------------------------------------------------
expit_fnc <- function(x) {   
  return(1 / (1 + exp(-x)))
}


####----------------------------------------------------------------------------
## Master imputation function
####----------------------------------------------------------------------------
imputation_fnc <- function(df, m = 5, X_categorical) {
  # Inputs: df = a list of data.frames generated from the DGM_fnc() function
  #         m = number of multiple imputed datasets to generate
  #         X_categorical = TRUE/FALSE whether X1 is simulated as categorical
  
  ##
  #Complete case Analysis
  ##
  CCA_dev <- df$observed_dev_data[complete.cases(df$observed_dev_data), ]
  CCA_val <- df$observed_val_data[complete.cases(df$observed_val_data), ]
  
  
  ##
  #Single Regression Imputation
  ##
  RI_dev <- regimputation_fnc(df$observed_dev_data, 
                              X_categorical = X_categorical)
  
  #take the RI models fit in development and apply them to the validation set:
  RI_val_transportedimputation <- mice::mice.mids(RI_dev, 
                                                  newdata = df$observed_val_data,
                                                  print = F)
  
  #for comparison, also refit the RI models in the validation set:
  RI_val_newimputation <- regimputation_fnc(df$observed_val_data,
                                            X_categorical = X_categorical) 
  
  
  ##
  #Multiple Imputation
  ##
  MI_dev_noY <- mice_fnc(df$observed_dev_data,
                         m = m, 
                         include_Y = FALSE) 
  
  MI_dev_withY <- mice_fnc(df$observed_dev_data, 
                           m = m, 
                           include_Y = TRUE) 
  
  #take the MI models fit in development and apply them to the validation set:
  MI_val_noY_transportedimputation <- mice::mice.mids(MI_dev_noY, 
                                                      newdata = df$observed_val_data,
                                                      print = F)
  MI_val_withY_transportedimputation <- mice::mice.mids(MI_dev_withY, 
                                                        newdata = df$observed_val_data,
                                                        print = F)
  
  #for comparison, also refit the MI models in the validation set:
  MI_val_noY_newimputation <- mice_fnc(df$observed_val_data,
                                       m = m, 
                                       include_Y = FALSE) 
  
  MI_val_withY_newimputation <- mice_fnc(df$observed_val_data,
                                         m = m, 
                                         include_Y = TRUE) 
  
  
  ##
  #mean or risk factor absent Imputation depending on X_categorical
  ##
  if(X_categorical) {
    # if X1 is categorical, then perform risk factor absent imputation
    zero_dev <- df$observed_dev_data
    zero_dev$x_1[is.na(zero_dev$x_1)] <- 0
    
    zero_val <- df$observed_val_data
    zero_val$x_1[is.na(zero_val$x_1)] <- 0
    
    return(list(
      "CCA_dev" = CCA_dev,
      "CCA_val" = CCA_val,
      "RI_dev" = RI_dev,
      "RI_val_transportedimputation" = RI_val_transportedimputation,
      "RI_val_newimputation" = RI_val_newimputation,
      "MI_dev_noY" = MI_dev_noY,
      "MI_dev_withY" = MI_dev_withY,
      "MI_val_noY_transportedimputation" = MI_val_noY_transportedimputation,
      "MI_val_withY_transportedimputation" = MI_val_withY_transportedimputation,
      "MI_val_noY_newimputation" = MI_val_noY_newimputation,
      "MI_val_withY_newimputation" = MI_val_withY_newimputation,
      "zero_dev" = zero_dev,
      "zero_val" = zero_val,
      "fullyobserved_dev" = df$fully_observed_dev_data,
      "fullyobserved_val" = df$fully_observed_val_data,
      "observed_dev" = df$observed_dev_data,
      "observed_val" = df$observed_val_data
    ))
    
  } else {
    # if X1 is continuous, then perform mean imputation 
    mean_dev <- df$observed_dev_data 
    mean_dev$x_1[is.na(mean_dev$x_1)] <- mean(df$observed_dev_data$x_1, 
                                              na.rm = TRUE)
    
    mean_val <- df$observed_val_data
    #take the mean value of x_1 from the dev data to apply to val data:
    mean_val$x_1[is.na(mean_val$x_1)] <- mean(df$observed_dev_data$x_1, 
                                              na.rm = TRUE)
    
    return(list(
      "CCA_dev" = CCA_dev,
      "CCA_val" = CCA_val,
      "RI_dev" = RI_dev,
      "RI_val_transportedimputation" = RI_val_transportedimputation,
      "RI_val_newimputation" = RI_val_newimputation,
      "MI_dev_noY" = MI_dev_noY,
      "MI_dev_withY" = MI_dev_withY,
      "MI_val_noY_transportedimputation" = MI_val_noY_transportedimputation,
      "MI_val_withY_transportedimputation" = MI_val_withY_transportedimputation,
      "MI_val_noY_newimputation" = MI_val_noY_newimputation,
      "MI_val_withY_newimputation" = MI_val_withY_newimputation,
      "mean_dev" = mean_dev,
      "mean_val" = mean_val,
      "fullyobserved_dev" = df$fully_observed_dev_data,
      "fullyobserved_val" = df$fully_observed_val_data,
      "observed_dev" = df$observed_dev_data,
      "observed_val" = df$observed_val_data
    ))
  }
} 

####----------------------------------------------------------------------------
## Multiple imputation function
####----------------------------------------------------------------------------
mice_fnc <- function(df, m = 5, include_Y) {
  # Inputs: df = one of the data.frames generated from the DGM_fnc() function
  #         m = number of multiple imputed datasets to generate
  #         include_Y = TRUE/FALSE whether to include the outcome, Y, in 
  #                     the mice procedure
  
  dummyrun <- mice::mice(df, m = m, maxit = 0) 
  predmat <- dummyrun$predictorMatrix 
  if (include_Y ==FALSE){ 
    predmat["Y",] <- 0 
    predmat[,"Y"] <- 0
  }
  #we never want MI to impute the missing values based on ID, U or R1:
  predmat[,"ID"] <- 0 
  predmat["ID",] <- 0
  predmat[,"U"] <- 0
  predmat["U",] <- 0
  predmat[,"R_1"] <- 0
  predmat["R_1",] <- 0
  
  #only imputing one variable, so dont need many iterations for convergence:
  mice::mice(df, 
             method = "norm",
             m, 
             maxit = 10, 
             predictorMatrix = predmat, 
             printFlag = FALSE) 
}

####----------------------------------------------------------------------------
## Single Regression imputation function
####----------------------------------------------------------------------------
regimputation_fnc <- function(df, X_categorical) {
  # Inputs: df = one of the data.frames generated from the DGM_fnc() function
  #         X_categorical = TRUE/FALSE whether X1 is simulated as categorical
  
  dummyrun <- mice::mice(df, m = 1, maxit = 0) 
  predmat <- dummyrun$predictorMatrix 
  predmat["Y",] <- 0 
  predmat[,"Y"] <- 0 #dont include Y in regression imputation
  #we never want MI to impute the missing values based on ID, U or R1:
  predmat[,"ID"] <- 0 
  predmat["ID",] <- 0
  predmat[,"U"] <- 0
  predmat["U",] <- 0
  predmat[,"R_1"] <- 0
  predmat["R_1",] <- 0
  
  if (X_categorical == TRUE) {
    #regression imputation 
    #using mice like this only works for regression imputation in the case of 
    #a single missing predictor
    df$x_1 <- factor(df$x_1, levels = c(0,1)) #convert to factor for mice()
    RI_imp <- mice::mice(df, 
                         method = "logreg", 
                         m = 1, 
                         maxit = 1, 
                         predictorMatrix = predmat, 
                         printFlag = FALSE) 
    #undo factor conversion post mice():
    long_dat <- complete(RI_imp, action = "long", include = TRUE)
    long_dat$x_1 <- as.numeric(levels(long_dat$x_1))[long_dat$x_1]
    mice::as.mids(long_dat)
    
  } else {
    #regression imputation
    #using mice like this only works for regression imputation in the case of 
    #a single missing predictor
    mice::mice(df, 
               method = "norm.predict", 
               m = 1, 
               maxit = 1, 
               predictorMatrix = predmat, 
               printFlag = FALSE) 
  }
  
}


####----------------------------------------------------------------------------
##Model fitting function - fits CPMs to development data under different
##imputation methods, and extracts the model coefficients
####----------------------------------------------------------------------------
dev_mod_fnc <- function(imputed_datasets,
                        X_categorical) {
  # Inputs: imputed_datasets = datasets generated from imputation_fnc() function
  #         X_categorical = TRUE/FALSE whether X1 is simulated as categorical
  
  #fit a model to the fully observed development data
  fullyobserved_model <- glm(Y ~ x_1 + x_2, 
                             data = imputed_datasets$fullyobserved_dev, 
                             family = binomial(link="logit")) 
  fullyobserved_coefs <- coef(fullyobserved_model)
  
  
  #fit a model to the complete case development data
  CCA_model <- glm(Y ~ x_1 + x_2, 
                   data = imputed_datasets$CCA_dev, 
                   family = binomial(link="logit")) 
  CCA_coefs <- coef(CCA_model)
  
  
  #fit a model to the imputed data under regression imputation
  RI_model <- with(imputed_datasets$RI_dev, 
                   glm(Y ~ x_1 + x_2, 
                       family = binomial(link = "logit")))
  RI_coefs <- RI_model %>%
    summary %>%
    dplyr::select(estimate) %>%
    dplyr::pull()
  names(RI_coefs) <- c("(Intercept)", "x_1", "x_2")
  
  
  #fit a model to the MI(no Y) development data
  MInoY_model <- with(imputed_datasets$MI_dev_noY, 
                      glm(Y ~ x_1 + x_2, 
                          family = binomial(link = "logit")))
  MInoY_coefs <- summary(pool(MInoY_model))$estimate
  names(MInoY_coefs) <- c("(Intercept)", "x_1", "x_2")
  
  
  #fit a model to the MI(with Y) development data
  MIwithY_model <- with(imputed_datasets$MI_dev_withY,
                        glm(Y ~ x_1 + x_2, 
                            family = binomial(link = "logit")))
  MIwithY_coefs <- summary(pool(MIwithY_model))$estimate
  names(MIwithY_coefs) <- c("(Intercept)", "x_1", "x_2")
  
  
  #fit pattern sub-models to the dev data as described by Mercaldo and Blume
  #2020 (https://doi.org/10.1093/biostatistics/kxy040). 
  patternboth_model <- with(imputed_datasets$observed_dev %>%
                              filter(!is.na(x_1) & !is.na(x_2)),
                            glm(Y ~ x_1 + x_2, 
                                family = binomial(link = "logit"))) 
  patternX2_model <- with(imputed_datasets$observed_dev %>%
                            filter(is.na(x_1) & !is.na(x_2)),
                          glm(Y ~ x_2, 
                              family = binomial(link = "logit")))
  #no missingness in x_2 so dont need x_1 pattern model, nor the 
  #null pattern model
  
  patternboth_coefs <- coef(patternboth_model)
  names(patternboth_coefs) <- c("(Intercept)", "x_1", "x_2")
  
  patternX2_coefs <- c(coef(patternX2_model)[1], NA, coef(patternX2_model)[2])
  names(patternX2_coefs) <- c("(Intercept)", "x_1", "x_2")
  
  
  if(X_categorical) {
    # if X1 is categorical, then fit model on risk factor absent imputation
    zero_model <- with(imputed_datasets$zero_dev,
                       glm(Y ~ x_1 + x_2, 
                           family = binomial(link = "logit")))
    zero_coefs <- coef(zero_model)
    names(zero_coefs) <- c("(Intercept)", "x_1", "x_2")
    
    
    return(list("fullyobserved_coefs" = fullyobserved_coefs, 
                "CCA_coefs" = CCA_coefs, 
                "RI_coefs" = RI_coefs, 
                "MInoY_coefs" = MInoY_coefs, 
                "MIwithY_coefs" = MIwithY_coefs, 
                "pattern_coefs" = list("patternboth_coefs" = patternboth_coefs,
                                       "patternX2_coefs" = patternX2_coefs),
                "zero_coefs" = zero_coefs))
  } else {
    # if X1 is continuous, then fit model on mean imputated development data
    mean_model <- with(imputed_datasets$mean_dev,
                       glm(Y ~ x_1 + x_2, 
                           family = binomial(link = "logit")))
    mean_coefs <- coef(mean_model)
    names(mean_coefs) <- c("(Intercept)", "x_1", "x_2")
    
    return(list("fullyobserved_coefs" = fullyobserved_coefs, 
                "CCA_coefs" = CCA_coefs, 
                "RI_coefs" = RI_coefs, 
                "MInoY_coefs" = MInoY_coefs, 
                "MIwithY_coefs" = MIwithY_coefs, 
                "pattern_coefs" = list("patternboth_coefs" = patternboth_coefs,
                                       "patternX2_coefs" = patternX2_coefs),
                "mean_coefs" = mean_coefs))
  }
}


####----------------------------------------------------------------------------
##Function that applies the CPM to a given imputed dataset to calculate
##predicted risks for each individual in that dataset
####----------------------------------------------------------------------------
make_predictions_fnc <- function(imputed_dataset, 
                                 model_coefs,
                                 X_categorical) {
  # Inputs: imputed_dataset = one of the imputed datasets generated by 
  #                           imputation_fnc()
  #         model_coefs = a set of coefficients from each CPM fit to the 
  #                       dev data under different imputation methods, as 
  #                       produced by dev_mod_fnc()
  #         X_categorical = TRUE/FALSE whether X1 is simulated as categorical
  
  if(is.mids(imputed_dataset) == FALSE){ 
    
    output_predictions <- tibble::tibble("ID" = imputed_dataset$ID,
                                         "Y" = imputed_dataset$Y)
    
    if(any(is.na(imputed_dataset$x_1))) {
      #apply pattern submodel only - its the observed val data, so has 
      #missing x1, so can only apply the pattern sub-model from the dev set
      
      #define the design matrix:
      DM <- model.matrix(~x_1+x_2, 
                         model.frame(~x_1 + x_2, imputed_dataset,
                                     na.action=NULL))
      
      output_predictions$LP_patternsubmodel <- ifelse(is.na(DM[,"x_1"]),
                                                      as.numeric((model_coefs$pattern_coefs$patternX2_coefs["(Intercept)"])+
                                                                   (DM[,3]*model_coefs$pattern_coefs$patternX2_coefs["x_2"])),
                                                      as.numeric(DM %*% model_coefs$pattern_coefs$patternboth_coefs))
      output_predictions$PR_patternsubmodel <- expit_fnc(output_predictions$LP_patternsubmodel)
      
    } else{
      
      #define the design matrix:
      DM <- model.matrix(~x_1 + x_2, imputed_dataset)
      
      output_predictions <- output_predictions %>%
        dplyr::mutate(
          #apply the fully observed developed CPM to the validation data:
          LP_fullyobserved = as.numeric(DM %*% model_coefs$fullyobserved_coefs),
          PR_fullyobserved = expit_fnc(LP_fullyobserved),
          
          #apple the CCA developed CPM to the validation data:
          LP_CCA = as.numeric(DM %*% model_coefs$CCA_coefs),
          PR_CCA = expit_fnc(LP_CCA),
          
          #apply the RI developed CPM to the validation data:
          LP_RI = as.numeric(DM %*% model_coefs$RI_coefs),
          PR_RI = expit_fnc(LP_RI),
          
          #apply the MI(noY) developed CPM to the validation data:
          LP_MInoY = as.numeric(DM %*% model_coefs$MInoY_coefs),
          PR_MInoY = expit_fnc(LP_MInoY),
          
          #apply the MI(with Y) developed CPM to the validation data:
          LP_MIwithY = as.numeric(DM %*% model_coefs$MIwithY_coefs),
          PR_MIwithY = expit_fnc(LP_MIwithY),
          
          #apply the pattern-submodel developed CPM to the validation data:
          LP_patternsubmodel = as.numeric(DM %*% model_coefs$pattern_coefs$patternboth_coefs),
          PR_patternsubmodel = expit_fnc(LP_patternsubmodel)
        )
      
      if(X_categorical) {
        output_predictions <- output_predictions %>%
          dplyr::mutate(LP_zero = as.numeric(DM %*% model_coefs$zero_coefs),
                        PR_zero = expit_fnc(LP_zero))
      } else {
        output_predictions <- output_predictions %>%
          dplyr::mutate(LP_mean = as.numeric(DM %*% model_coefs$mean_coefs),
                        PR_mean = expit_fnc(LP_mean))
      }
    }
  } else {
    MI_long <- mice::complete(imputed_dataset, action = 'long')
    
    output_predictions <- tibble::tibble(".imp" = MI_long$.imp,
                                         "ID" = MI_long$ID,
                                         "Y" = MI_long$Y)
    
    #define the design matrix:
    DM <- model.matrix(~x_1 + x_2, MI_long)
    
    output_predictions <- output_predictions %>%
      dplyr::mutate(
        #apply the fully observed developed CPM to the validation data:
        LP_fullyobserved = as.numeric(DM %*% model_coefs$fullyobserved_coefs),
        PR_fullyobserved = expit_fnc(LP_fullyobserved),
        
        #apple the CCA developed CPM to the validation data:
        LP_CCA = as.numeric(DM %*% model_coefs$CCA_coefs),
        PR_CCA = expit_fnc(LP_CCA),
        
        #apply the RI developed CPM to the validation data:
        LP_RI = as.numeric(DM %*% model_coefs$RI_coefs),
        PR_RI = expit_fnc(LP_RI),
        
        #apply the MI(noY) developed CPM to the validation data:
        LP_MInoY = as.numeric(DM %*% model_coefs$MInoY_coefs),
        PR_MInoY = expit_fnc(LP_MInoY),
        
        #apply the MI(with Y) developed CPM to the validation data:
        LP_MIwithY = as.numeric(DM %*% model_coefs$MIwithY_coefs),
        PR_MIwithY = expit_fnc(LP_MIwithY),
        
        #apply the pattern-submodel developed CPM to the validation data:
        LP_patternsubmodel = as.numeric(DM %*% model_coefs$pattern_coefs$patternboth_coefs),
        PR_patternsubmodel = expit_fnc(LP_patternsubmodel)
      )
    
    if(X_categorical) {
      output_predictions <- output_predictions %>%
        dplyr::mutate(LP_zero = as.numeric(DM %*% model_coefs$zero_coefs),
                      PR_zero = expit_fnc(LP_zero))
    } else {
      output_predictions <- output_predictions %>%
        dplyr::mutate(LP_mean = as.numeric(DM %*% model_coefs$mean_coefs),
                      PR_mean = expit_fnc(LP_mean))
    }
  }
  output_predictions
}



####----------------------------------------------------------------------------
##Function to assess the predictive performance of each model in a given imputed
##dataset
####----------------------------------------------------------------------------
validation_fnc <- function(df_i, PR_name) {
  # Inputs: df_i = a dataframe with the predicted risks from each 
  #                 model and observed outcomes produced by 
  #                 the functions make_predictions_fnc() 
  #         PR_names = column names in df_i that contain the 
  #                     predicted risks from the model under evaluation
  
  if(".imp" %in% names(df_i)) {
    #test performance in each imputed dataset (and for each model separately)
    #and then apply Rubins Rules.
    df_i_subset <- df_i %>%
      dplyr::select(.imp, ID, Y, {{PR_name}}) %>%
      dplyr::rename(PR = {{PR_name}})
    target_measures <- df_i_subset %>%
      dplyr::group_by(.imp) %>%
      tidyr::nest() %>% 
      dplyr::mutate(results = purrr::map(data,
                                         function(df) {
                                           predictive_performance_fnc(
                                             Y = df$Y, 
                                             Predicted_Risks = df$PR)
                                           })) %>%
      dplyr::select(-data) %>%
      tidyr::unnest(cols = c(".imp", "results")) %>%
      tidyr::pivot_longer(cols = c(-".imp")) %>%
      tidyr::separate_wider_delim(cols = "name",
                                  delim = "_",
                                  names = c("Metric", "Value_type")) %>%
      tidyr::pivot_wider(id_cols = c(".imp", "Metric"),
                         names_from = "Value_type",
                         values_from = "value") %>%
      dplyr::group_by(Metric) %>%
      tidyr::nest() %>%
      dplyr::mutate(results = map(data,
                                  function(df) {
                                    RR <- mice::pool.scalar(Q = df$est,
                                                            U = df$var,
                                                            n = nrow(df_i)/nrow(df))
                                    data.frame("est" = RR$qbar,
                                               "var" = RR$t)})) %>%
      dplyr::select(-data) %>%
      tidyr::unnest(cols = c("Metric", "results"))  %>% 
      tidyr::pivot_wider(names_from = "Metric", 
                         values_from = c("est", "var"), 
                         names_glue = "{Metric}_{.value}")
    
  } else {
    target_measures <- tibble(
      predictive_performance_fnc(Y = df_i$Y, 
                                 Predicted_Risks = df_i[[PR_name]]))
    
  }
  return(target_measures)
}

####----------------------------------------------------------------------------
##Function to estimate the predictive performance of a given set of predicted
##risks against a given set of observed outcomes
####----------------------------------------------------------------------------
predictive_performance_fnc <- function(Y, Predicted_Risks) {
  #Input: 
  # Y = a binary variable of observed outcomes
  # Predicted_Risks = a vector of predicted risks 
  
  ## Calculate Brier Score (mean square error of predictions)
  ####------------------------------------------------------------------
  Brier_individuals <- (Predicted_Risks - Y)^2
  Brier <- mean(Brier_individuals)
  Brier_var <- ((1/(length(Predicted_Risks)^2)) *
                  sum((Predicted_Risks)*(1-Predicted_Risks)*((1-(2*Predicted_Risks))^2)))
  #Spiegelhalter, D. J. 1986 https://doi.org/10.1002/sim.4780050506
  
  
  ## Calibration intercept (i.e. calibration-in-the-large)
  ####-------------------------------------------------------------------------------
  LP <- log(Predicted_Risks/ (1 - Predicted_Risks)) 
  Cal_Int <- glm(Y ~ offset(LP), family = binomial(link = "logit"))
  Cal_Int_var <- vcov(Cal_Int)[1,1]
  
  ## Calibration slope
  ####--------------------------------------------------------------------------
  Cal_Slope <- glm(Y ~ LP, family = binomial(link = "logit"))
  Cal_Slope_var <- vcov(Cal_Slope)[2,2]
  
  
  ## Discrimination
  ####------------------------------------------------------------------------
  AUC <- pROC::roc(response = Y, 
                   predictor = as.vector(Predicted_Risks),
                   direction = "<",
                   levels = c(0,1))
  
  ## Return performance results in a data.frame
  ####------------------------------------------------------------------------
  return(data.frame("CalInt_est" = as.numeric(coef(Cal_Int)),
                    "CalInt_var" = Cal_Int_var,
                    "CalSlope_est" = as.numeric(coef(Cal_Slope)[2]),
                    "CalSlope_var" = as.numeric(Cal_Slope_var),
                    "AUC_est" = as.numeric(AUC$auc),
                    "AUC_var" = as.numeric(var(AUC, 
                                               method = "delong")),
                    "Brier_est" = as.numeric(Brier),
                    "Brier_var" = as.numeric(Brier_var)))
}

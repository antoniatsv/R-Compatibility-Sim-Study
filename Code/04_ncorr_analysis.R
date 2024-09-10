# ##############################################################################

# Author of code: Antonia D. Tsvetanova & Glen P. Martin

# This is code runs the empirical study analysis in a manuscript entitled: 
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

library(tidyverse)
library(mice)
library(pROC)

####----------------------------------------------------------------------------
## Load the dataset
####----------------------------------------------------------------------------

thoracic_raw <- haven::read_sav("//nasr.man.ac.uk/bmhrss$/snapped/replicated/PSTRC_Dyn_Pred/Antonia/Data/6600 anonymous.sav")

####----------------------------------------------------------------------------
## Select Relevant Variables for the Analysis - those in the RESECT model
####----------------------------------------------------------------------------

Analysis_cohort <- thoracic_raw %>%
  dplyr::select("Age", 
                "Sex", 
                "ECOG", 
                "DLCOPredicted", 
                "BMI", 
                "CreatinineumolL", 
                "Anaemia", 
                "Arrhythmia", 
                "Right", 
                "ResectedSegments", 
                "Thoracotomy", 
                "Malignant",
                "Deadat90days") %>%
  dplyr::mutate("ID" = 1:n(), .before = "Age") 

####----------------------------------------------------------------------------
## Data Cleaning
####----------------------------------------------------------------------------
Analysis_cohort <- Analysis_cohort %>%
  dplyr::mutate(DLCOPredicted = ifelse(DLCOPredicted < 18 |
                                         DLCOPredicted > 187,
                                       NA,
                                       DLCOPredicted),
                
                BMI = ifelse(BMI < 14 |
                               BMI > 64, 
                             NA, 
                             BMI),
                
                CreatinineumolL = ifelse(CreatinineumolL < 11.3 |
                                           CreatinineumolL > 654, 
                                         NA, 
                                         CreatinineumolL),
                
                #factor variables
                Sex = factor(Sex, levels = c("Female",
                                             "Male")),
                
                ECOG = as.numeric(ifelse(ECOG == "", 
                                         NA, 
                                         as.character(ECOG))),
                ECOG = factor(ECOG,
                              levels = c("0",
                                         "1",
                                         "2",
                                         "3")),
                
                Anaemia = factor(ifelse(Anaemia == 1,
                                        "Yes",
                                        "No"),
                                 levels = c("No", "Yes")),
                
                Arrhythmia = factor(ifelse(Arrhythmia == 1,
                                           "Yes",
                                           "No"),
                                    levels = c("No", "Yes")),
                
                Right = factor(ifelse(Right == 1,
                                      "Yes",
                                      "No"),
                               levels = c("No", "Yes")),
                
                Thoracotomy = factor(ifelse(Thoracotomy == 1,
                                            "Yes",
                                            "No"),
                                     levels = c("No", "Yes")),
                
                Malignant = factor(ifelse(Malignant == 1,
                                          "Yes",
                                          "No"),
                                   levels = c("No", "Yes")),
                
                Deadat90days = factor(ifelse(Deadat90days == 1,
                                             "Yes",
                                             "No"),
                                      levels = c("No", "Yes")))


####----------------------------------------------------------------------------
## Baseline Summary of the cohort
####----------------------------------------------------------------------------

Analysis_cohort %>%
  dplyr::select(-ID) %>%
  gtsummary::tbl_summary(by = Deadat90days,
                         label = list(Age = "Age (years)",
                                      ECOG = "Performance Status",
                                      DLCOPredicted = "Diffusion capacity of the lung for carbon monoxide ",
                                      BMI = "Body Mass Index",
                                      CreatinineumolL = "Creatinine (umolL)",
                                      Right = "Right-side resection",
                                      ResectedSegments = "Number of Resected Segments",
                                      Thoracotomy = "Surgery via thoracotomy",
                                      Malignant = "Diagnosis of malignancy")) %>%
  gtsummary::add_overall()



####----------------------------------------------------------------------------
##Create a function that imputes the data under each different imputation method
####----------------------------------------------------------------------------
imputation_fnc <- function(df, m) {
  # Inputs: df = a list of data.frames of dev and val data sets
  #         m = number of multiple imputed datasets to generate
  
  ##
  #Complete case Analysis
  ##
  CCA_dev <- df$Dev_data[complete.cases(df$Dev_data), ]
  CCA_val <- df$Val_data[complete.cases(df$Val_data), ]
  
  ##
  #Mean/Risk Factor Absent Imputation
  ##
  miss_vars_dev <- colnames(df$Dev_data)[colSums(is.na(df$Dev_data)) > 0]
  miss_vars_val <- colnames(df$Val_data)[colSums(is.na(df$Val_data)) > 0]
  
  mean_imputed_dev <- df$Dev_data
  for(i in miss_vars_dev) {
    if(is.factor(mean_imputed_dev[[i]])) {
      #risk factor absent:
      RFA <- levels(df$Dev_data[[i]])[which.min(table(df$Dev_data[[i]]))]
      mean_imputed_dev[[i]][which(is.na(mean_imputed_dev[[i]]))] <- RFA
    } else{
      #calc mean:
      mean_val <- mean(df$Dev_data[[i]], na.rm = T)
      mean_imputed_dev[[i]][which(is.na(mean_imputed_dev[[i]]))] <- mean_val
    }
  }
  mean_imputed_val <- df$Val_data
  for(i in miss_vars_val) {
    if(is.factor(mean_imputed_val[[i]])) {
      #risk factor absent (from the dev data, to apply to val data):
      RFA <- levels(df$Dev_data[[i]])[which.min(table(df$Dev_data[[i]]))]
      mean_imputed_val[[i]][which(is.na(mean_imputed_val[[i]]))] <- RFA
    } else{
      #calc mean (from the dev data, to apply to val data):
      mean_val <- mean(df$Dev_data[[i]], na.rm = T)
      mean_imputed_val[[i]][which(is.na(mean_imputed_val[[i]]))] <- mean_val
    }
  }
  
  ##
  #Single Regression Imputation
  ##
  dummyrun <- mice::mice(df$Dev_data, m = 1, maxit = 0) 
  predmat <- dummyrun$predictorMatrix 
  predmat["Deadat90days",] <- 0 
  predmat[,"Deadat90days"] <- 0 #dont include outcome in regression imputation
  predmat[,"ID"] <- 0 
  predmat["ID",] <- 0
  imp_method <- dummyrun$method
  imp_method[which(imp_method == "pmm")] <- "norm.predict"
  RI_dev <- mice::mice(df$Dev_data, 
                       method = imp_method, 
                       m = 1, 
                       maxit = 10, 
                       predictorMatrix = predmat, 
                       printFlag = FALSE) 
  
  RI_val <- mice::mice.mids(RI_dev, 
                            newdata = df$Val_data,
                            print = F)
  
  ##
  #Multiple Imputation without Y
  ##
  dummyrun <- mice::mice(df$Dev_data, m = m, maxit = 0) 
  predmat <- dummyrun$predictorMatrix 
  predmat["Deadat90days",] <- 0 
  predmat[,"Deadat90days"] <- 0 
  predmat[,"ID"] <- 0 
  predmat["ID",] <- 0
  imp_method <- dummyrun$method
  imp_method[which(imp_method == "pmm")] <- "norm"
  MInoY_dev <- mice::mice(df$Dev_data, 
                          method = imp_method,
                          m = m, 
                          maxit = 50, 
                          predictorMatrix = predmat, 
                          printFlag = FALSE) 
  
  MInoY_val <- mice::mice.mids(MInoY_dev, 
                               newdata = df$Val_data,
                               print = F)
  
  ##
  #Multiple Imputation with Y
  ##
  predmat <- dummyrun$predictorMatrix 
  predmat[,"ID"] <- 0 
  predmat["ID",] <- 0
  MIwithY_dev <- mice::mice(df$Dev_data,
                            method = imp_method,
                            m = m,
                            maxit = 50, 
                            predictorMatrix = predmat, 
                            printFlag = FALSE) 
  
  MIwithY_val <- mice::mice.mids(MIwithY_dev, 
                                 newdata = df$Val_data,
                                 print = F)
  
  ##
  #Return the Results
  ##
  list("CCA_dev" = CCA_dev,
       "CCA_val" = CCA_val,
       "mean_imputed_dev" = mean_imputed_dev,
       "mean_imputed_val" = mean_imputed_val,
       "RI_dev" = RI_dev,
       "RI_val" = RI_val,
       "MInoY_dev" = MInoY_dev,
       "MInoY_val" = MInoY_val,
       "MIwithY_dev" = MIwithY_dev,
       "MIwithY_val" = MIwithY_val)
}


####----------------------------------------------------------------------------
##Function to fit the pattern sub-model
#adapted from https://academic.oup.com/biostatistics/article/21/2/236/5092384
# and https://github.com/sarahmercaldo/MissingDataAndPrediction/blob/master/PMKSfunctions.R
# to work specifically for ncorr data
####----------------------------------------------------------------------------
pmks <- function(DATA, 
                 model, 
                 raw_data_patterns){
  # Inputs: DATA = data.frame of observations to fit the pattern submodels on
  #         model = string giving the model formula
  #         raw_data_patterns = factor vector giving all observed missing 
  #                 covariate patterns in the raw ncorr dataset
  
  mod.DATA <- get_all_vars(as.formula(model), data=DATA)
  SDATA <- mod.DATA[,-1] #remove the outcome 
  tmp.dat <- as.data.frame(is.na(SDATA)*1)
  tmp.pattern <- factor(apply(tmp.dat,1,function(z) paste(z,collapse="")))
  obs.patterns <- unique(tmp.pattern)
  tmp.info <- split(seq(nrow(SDATA)), tmp.pattern)
  mp.levels <- levels(tmp.pattern)
  mp.pattern <- do.call(rbind, lapply(as.list(mp.levels),function(ZZ) strsplit(ZZ,'')[[1]])) 		
  mp.info <- data.frame(cbind(names(tmp.info), 
                              unlist(lapply(tmp.info, length)),
                              unlist(lapply(split(mod.DATA[,1], tmp.pattern), 
                                            function(X) sum(X == "Yes")))),
                        stringsAsFactors= FALSE)
  rownames(mp.info) <- seq(nrow(mp.info))
  colnames(mp.info) <- c('mp','n', 'E')
  if(length(setdiff(raw_data_patterns, obs.patterns)) == 0){
    empty.patterns = NULL
  } else {
    empty.patterns <- data.frame(mp = factor(setdiff(raw_data_patterns,obs.patterns)), n=0, E=0)
  }
  mp.info <- rbind(mp.info,empty.patterns)
  
  mod.rhs <- strsplit(model, '~')[[1]][1]
  mod.lhs <- strsplit(model, '~')[[1]][2]
  mod.lhs <- strsplit(mod.lhs,' ')[[1]]
  mod.lhs <- mod.lhs[!(mod.lhs%in%c('','+','~'))]
  
  cc <- ncol(model.matrix(as.formula(model),mod.DATA))
  
  # modify this slightly over the original paper - outcome quite rare in ncorr
  # dataset, so propose that the number of events per pattern needs to be
  # greater than P*2, rather than N per patter as in original publication:
  threshold <- cc*2
  # mp.info$use.ptmx <- (as.numeric(mp.info$n)>=threshold)*1
  mp.info$use.ptmx <- (as.numeric(mp.info$E)>=threshold)*1
  
  reg.out <- vector('list', nrow(mp.info))
  names(reg.out) <- mp.info$mp
  
  for(ixx in seq(nrow(mp.info))) {
    col.keep  <- which(strsplit(mp.info$mp[ixx],'')[[1]]=='0')
    if(length(col.keep)==0) {
      new.mod <- as.formula(paste(mod.rhs,1,sep='~'))
    } else {
      new.mod   <- as.formula(paste(mod.rhs,paste(mod.lhs[col.keep],collapse='+'),
                                    sep='~'))
    }
    
    if(mp.info$use.ptmx[ixx]==1) {
      reg.out[[ixx]] <- list(pattern = mod.lhs[col.keep],
                             mod = glm(new.mod,data=mod.DATA[tmp.info[[ixx]],],
                                       family = 'binomial'))
    } else {
      reg.out[[ixx]] <- list(pattern = mod.lhs[col.keep],
                             mod	 = glm(new.mod,data=mod.DATA, 
                                        family='binomial'))
    }
  }
  reg.out
}

####----------------------------------------------------------------------------
##Function to make predictions from pattern sub-model
#adapted from https://academic.oup.com/biostatistics/article/21/2/236/5092384
# and https://github.com/sarahmercaldo/MissingDataAndPrediction/blob/master/PMKSfunctions.R
# to work specifically for ncorr data
####----------------------------------------------------------------------------
predict.sm <- function(prediction.data, model, pmks.object){
  mod.DATA <- get_all_vars(as.formula(model), data=prediction.data)
  PDATA <- mod.DATA[,-1] #remove the outcome 
  tmp.dat <- as.data.frame(is.na(PDATA)*1)
  tmp.pattern <- factor(apply(tmp.dat,1,function(z) paste(z,collapse="")))
  tmp.info <- split(seq(nrow(PDATA)), tmp.pattern)
  mp.levels <- levels(tmp.pattern)
  mp.pattern <- do.call(rbind, lapply(as.list(mp.levels),function(ZZ) strsplit(ZZ,'')[[1]])) 		
  mp.info <- data.frame(cbind(names(tmp.info), unlist(lapply(tmp.info, length))),
                        stringsAsFactors= FALSE)
  rownames(mp.info) <- seq(nrow(mp.info))
  colnames(mp.info) <- c('mp','n')
  mod.rhs <- strsplit(model, '~')[[1]][1]		
  mod.rhs <- strsplit(mod.rhs,' ')[[1]]
  mod.lhs <- strsplit(model, '~')[[1]][2]
  mod.lhs <- strsplit(mod.lhs,' ')[[1]]
  mod.lhs <- mod.lhs[!(mod.lhs%in%c('','+','~'))]
  pred.out <- vector('list', nrow(mp.info))
  #For the different patterns 
  for(ixx in seq(length(tmp.info))){
    col.keep <- which(strsplit(mp.info$mp[ixx],'')[[1]]=='0')
    pattern <- mod.lhs[col.keep]
    which.mod <- which(lapply(pmks.object, function(z) identical(z$pattern, pattern))==TRUE)
    
    pred.out[[ixx]] <- data.frame("ID" = prediction.data[tmp.info[[ixx]],"ID"],
                                  "LP" = predict(pmks.object[[which.mod]]$mod, 
                                                 prediction.data[tmp.info[[ixx]],]),
                                  "PR" = predict(pmks.object[[which.mod]]$mod, 
                                                 prediction.data[tmp.info[[ixx]],],
                                                 type = "response"))		
  }
  pred.out <- dplyr::bind_rows(pred.out)
  rownames(pred.out) <- NULL
  pred.out
}



####----------------------------------------------------------------------------
##Function that applies the CPM to a given imputed dataset to calculate
##predicted risks for each individual in that dataset
####----------------------------------------------------------------------------
make_predictions_fnc <- function(imputed_dataset, 
                                 model_coefs,
                                 pmks_object,
                                 model_formula) {
  # Inputs: imputed_dataset = one of the imputed validation datasets 
  #         model_coefs = a set of coefficients from each CPM fit to the 
  #                       dev data under different imputation methods
  #         pmks_object = a pmks object, produced by pmks() function
  #         model_formula = string giving the model formula
  
  if(is.mids(imputed_dataset) == FALSE) { 
    
    output_predictions <- tibble::tibble("ID" = imputed_dataset$ID,
                                         "Deadat90days" = imputed_dataset$Deadat90days)
    
    #define the design matrix:
      DM <- model.matrix(formula(model_formula), imputed_dataset)
      
      output_predictions <- output_predictions %>%
        dplyr::mutate(
          #apply the CCA developed CPM to the validation data:
          LP_CCA = as.numeric(DM %*% model_coefs$CCA_coefs),
          PR_CCA = expit_fnc(LP_CCA),
          
          #apply the mean imputed developed CPM to the validation data:
          LP_meanimputed = as.numeric(DM %*% model_coefs$mean_imputed_coefs),
          PR_meanimputed = expit_fnc(LP_meanimputed),
          
          #apply the RI developed CPM to the validation data:
          LP_RI = as.numeric(DM %*% model_coefs$RI_coefs),
          PR_RI = expit_fnc(LP_RI),
          
          #apply the MI(noY) developed CPM to the validation data:
          LP_MInoY = as.numeric(DM %*% model_coefs$MInoY_coefs),
          PR_MInoY = expit_fnc(LP_MInoY),
          
          #apply the MI(with Y) developed CPM to the validation data:
          LP_MIwithY = as.numeric(DM %*% model_coefs$MIwithY_coefs),
          PR_MIwithY = expit_fnc(LP_MIwithY)
        )
      
      #make predictions from pattern sub-model:
      psm_predictions <- predict.sm(prediction.data = imputed_dataset, 
                                    model = model_formula,
                                    pmks.object = pmks_object)
      
      output_predictions <- output_predictions %>%
        dplyr::left_join(psm_predictions %>%
                           dplyr::rename("LP_patternsubmodel" = LP,
                                         "PR_patternsubmodel" = PR),
                         by = "ID")
      
    } else {
    MI_long <- mice::complete(imputed_dataset, action = 'long')
    
    output_predictions <- tibble::tibble(".imp" = MI_long$.imp,
                                         "ID" = MI_long$ID,
                                         "Deadat90days" = MI_long$Deadat90days)
    
    #define the design matrix:
    DM <- model.matrix(formula(model_formula), MI_long)
    
    output_predictions <- output_predictions %>%
      dplyr::mutate(
        #apply the CCA developed CPM to the validation data:
        LP_CCA = as.numeric(DM %*% model_coefs$CCA_coefs),
        PR_CCA = expit_fnc(LP_CCA),
        
        #apply the mean imputed developed CPM to the validation data:
        LP_meanimputed = as.numeric(DM %*% model_coefs$mean_imputed_coefs),
        PR_meanimputed = expit_fnc(LP_meanimputed),
        
        #apply the RI developed CPM to the validation data:
        LP_RI = as.numeric(DM %*% model_coefs$RI_coefs),
        PR_RI = expit_fnc(LP_RI),
        
        #apply the MI(noY) developed CPM to the validation data:
        LP_MInoY = as.numeric(DM %*% model_coefs$MInoY_coefs),
        PR_MInoY = expit_fnc(LP_MInoY),
        
        #apply the MI(with Y) developed CPM to the validation data:
        LP_MIwithY = as.numeric(DM %*% model_coefs$MIwithY_coefs),
        PR_MIwithY = expit_fnc(LP_MIwithY)
      )
    
    #make predictions from pattern sub-model:
    psm_predictions <- predict.sm(prediction.data = MI_long, 
                                  model = model_formula,
                                  pmks.object = pmks_object)
    psm_predictions$.imp <- MI_long$.imp
    output_predictions <- output_predictions %>%
      dplyr::left_join(psm_predictions %>%
                         dplyr::rename("LP_patternsubmodel" = LP,
                                       "PR_patternsubmodel" = PR),
                       by = c(".imp", "ID"))
  }
  output_predictions
}


####----------------------------------------------------------------------------
## Function that takes the inverse logistic link
####----------------------------------------------------------------------------
expit_fnc <- function(x) {   
  return(1 / (1 + exp(-x)))
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
      dplyr::select(.imp, ID, Deadat90days, {{PR_name}}) %>%
      dplyr::rename(PR = {{PR_name}})
    target_measures <- df_i_subset %>%
      dplyr::group_by(.imp) %>%
      tidyr::nest() %>% 
      dplyr::mutate(results = purrr::map(data,
                                         function(df) {
                                           predictive_performance_fnc(
                                             Y = df$Deadat90days, 
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
      predictive_performance_fnc(Y = df_i$Deadat90days, 
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
  
  Y <- ifelse(Y == "Yes", 1, 0)
  
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

  
####----------------------------------------------------------------------------
## Function to run the modelling/analyses over each bootstrap IV iteration
####----------------------------------------------------------------------------
modelling_fnc <- function(Dev_data,
                          Val_data,
                          model_formula) {
  # Inputs: Dev_data = a tibble of the data available to develop the CPM
  #         Val_data = a tibble of the data available to validate the CPM (can 
  #                     be the same as Dev_data to give apparent performance)
  #         model_formula = string giving the model formula
  
  if (is.list(Dev_data)) {
    Dev_data <- dplyr::bind_rows(Dev_data) 
  } 
  if (is.list(Val_data)) {
    Val_data <- dplyr::bind_rows(Val_data)
  } 
  
  # Impute missing data in the development and validation data under
  # different methods ----------------------------------------------------------
  imputed_datasets <- imputation_fnc(df = list("Dev_data" = Dev_data,
                                               "Val_data" = Val_data), 
                                     m = 5)
  
  # Fit the prediction model to the development data, under
  # each imputation method------------------------------------------------------
  CCA_model <- with(imputed_datasets$CCA_dev,
                    glm(formula(model_formula),
                        family = binomial(link = "logit")))
  CCA_coefs <- coef(CCA_model)
  
  mean_imputed_model <- with(imputed_datasets$mean_imputed_dev,
                             glm(formula(model_formula),
                                 family = binomial(link = "logit")))
  mean_imputed_coefs <- coef(mean_imputed_model)
  
  RI_model <- with(imputed_datasets$RI_dev, 
                   glm(formula(model_formula), 
                       family = binomial(link = "logit")))
  RI_coefs <- RI_model %>%
    summary %>%
    dplyr::select(estimate) %>%
    dplyr::pull()
  names(RI_coefs) <- summary(RI_model)$term
  
  MInoY_model <- with(imputed_datasets$MInoY_dev, 
                      glm(formula(model_formula), 
                          family = binomial(link = "logit")))
  MInoY_coefs <- summary(pool(MInoY_model))$estimate
  names(MInoY_coefs) <- summary(pool(MInoY_model))$term
  
  MIwithY_model <- with(imputed_datasets$MIwithY_dev,
                        glm(formula(model_formula), 
                            family = binomial(link = "logit")))
  MIwithY_coefs <- summary(pool(MIwithY_model))$estimate
  names(MIwithY_coefs) <- summary(pool(MIwithY_model))$term
  
  #fit pattern sub-models to the dev data as described by Mercaldo and Blume
  #2020 (https://doi.org/10.1093/biostatistics/kxy040). 
  #- data has missingness in Sex, Ecog, DLCO, BMI and Creatinine
  mod.DATA <- get_all_vars(formula(model_formula), 
                           data = Val_data)
  SDATA <- mod.DATA[,-1] #remove the outcome 
  tmp.dat <- as.data.frame(is.na(SDATA)*1)
  tmp.pattern <- factor(apply(tmp.dat,1,function(z) paste(z,collapse="")))
  raw_data_patterns <- unique(tmp.pattern)
  pattern_submodel <- pmks(Dev_data, 
                           model = model_formula,
                           raw_data_patterns = raw_data_patterns)
  
  
  # Make predictions from each model in the validation data---------------------
  
  #extract each imputed version of the validation set
  val_imputed_datasets <- imputed_datasets[
    which(stringr::str_detect(names(imputed_datasets), "_val"))
  ]
  
  model_coefs <- list("CCA_coefs" = CCA_coefs,
                      "mean_imputed_coefs" = mean_imputed_coefs,
                      "RI_coefs" = RI_coefs,
                      "MInoY_coefs" = MInoY_coefs,
                      "MIwithY_coefs" = MIwithY_coefs)
  
  preds_per_data_set <- purrr::map(.x = val_imputed_datasets,
                                   .f = make_predictions_fnc,
                                   model_coefs = model_coefs,
                                   pmks_object = pattern_submodel,
                                   model_formula = model_formula)
  
  preds_per_data_set$RI_val <- preds_per_data_set$RI_val %>%
    dplyr::select(-.imp)
  #make predictions from pattern sub-model in the raw validation set:
  psm_predictions <- predict.sm(prediction.data = Val_data, 
                                model = model_formula,
                                pmks.object = pattern_submodel)
  
  preds_per_data_set$raw_val <- Val_data %>%
    dplyr::select(ID, Deadat90days) %>%
    dplyr::left_join(psm_predictions %>%
                       dplyr::rename("LP_patternsubmodel" = LP,
                                     "PR_patternsubmodel" = PR),
                       by = "ID")
  
  
  
  CPM_names <- c("PR_CCA",
                 "PR_meanimputed",
                 "PR_RI",
                 "PR_MInoY",
                 "PR_MIwithY",
                 "PR_patternsubmodel")
  Target_measures <- NULL
  #Apply each developed CPM to every imputed validation dataset:
  for(i in 1:length(CPM_names)) {
    Target_measures <- Target_measures %>%
      dplyr::bind_rows(purrr::map_dfr(.x = preds_per_data_set[-which(names(preds_per_data_set)=="raw_val")],
                                      .f = validation_fnc,
                                      PR_name = CPM_names[i],
                                      .id = "Validation_Dataset") %>%
                         dplyr::mutate("CPM" = CPM_names[i],
                                       .before = "Validation_Dataset"))
  }
  #Apply the sub-model to the raw (non-imputed) validation data:
  Target_measures <- Target_measures %>%
    dplyr::bind_rows(purrr::map_dfr(.x = preds_per_data_set[which(names(preds_per_data_set)=="raw_val")],
                                    .f = validation_fnc,
                                    PR_name = "PR_patternsubmodel",
                                    .id = "Validation_Dataset") %>%
                       dplyr::mutate("CPM" = "PR_patternsubmodel",
                                     .before = "Validation_Dataset"))
  
  Target_measures
}
  

####----------------------------------------------------------------------------------------
## Fit the models to the raw data, bootstrap data, and obtain internal validation results
####----------------------------------------------------------------------------------------

#Create a nested dataframe with each bootstrap data per (nested) row, 
# then pass this list of data to the above function
set.seed(75982)
nested_analysis_data <- tibble::tibble("Bootstrap_Index" = 0,
                                       "Original_Data" = list(Analysis_cohort),
                                       #set first row to raw data (to obtain apparent performance):
                                       "Bootstrap_Data" = list(Analysis_cohort)) %>%
  dplyr::bind_rows(tibble::tibble("Bootstrap_Index" = 1:100, #set how many bootstrap sample we wish to take
                                  "Original_Data" = list(Analysis_cohort)) %>%
                     #Apply the boostrap resampling:
                     dplyr::mutate("Bootstrap_Data" =  map(Original_Data,
                                                           function(df) df %>% sample_n(nrow(df), replace = TRUE))))

#identical(nested_analysis_data$Bootstrap_Data[[20]], nested_analysis_data$Bootstrap_Data[[12]]) #=FALSE
#identical(nested_analysis_data$Bootstrap_Data[[10]], nested_analysis_data$Bootstrap_Data[[95]]) #=FALSE
#identical(nested_analysis_data$Original_Data[[10]], nested_analysis_data$Bootstrap_Data[[10]]) #=FALSE
#identical(nested_analysis_data$Original_Data[[1]], nested_analysis_data$Bootstrap_Data[[1]]) #=TRUE, as expected


##Apply the modelling functions to each bootstrap dataset (runs in parallel):
library(furrr)
plan(multisession, workers = (availableCores() - 1))

nested_analysis_data <- nested_analysis_data %>%
  #Calculate the performance results of a model developed on each bootstrap data and tested in the original data:
  dplyr::mutate("Results" = furrr::future_pmap(list(Dev_data = Bootstrap_Data,# fit models on bootstrap data
                                                    Val_data = Original_Data),#test them on the raw data
                                               modelling_fnc,
                                               model_formula = "Deadat90days ~ Age + Sex + ECOG + DLCOPredicted + BMI + CreatinineumolL + Anaemia + Arrhythmia + Right + ResectedSegments + Thoracotomy + Malignant",
                                               .progress = TRUE,
                                               .options = furrr_options(seed = TRUE)
                                               ) 
                )

# write_rds(nested_analysis_data, file = here::here("Outputs", "ncorr_analysis_results.RDS"))


####----------------------------------------------------------------------------------------
## Summarise results for entry into the manuscript
####----------------------------------------------------------------------------------------

# nested_analysis_data <- read_rds(here::here("Outputs", "ncorr_analysis_results.RDS"))

# Unnest each result to create a tibble of performance results across the bootstraps:
summary_ncorr_results <- nested_analysis_data %>%
  select(Bootstrap_Index, Results) %>%
  unnest(cols = c(Results)) 

# write_rds(summary_ncorr_results, file = here::here("Outputs", "summary_ncorr_results.RDS"))

##First manipulate the data in a format we can work with:
Bootstrap_InternalValidation <- summary_ncorr_results %>%
  filter(Bootstrap_Index != 0) %>%
  select(Bootstrap_Index,
         CPM,
         Validation_Dataset,
         CalInt_est,
         CalInt_var,
         CalSlope_est,
         CalSlope_var,
         AUC_est,
         AUC_var,
         Brier_est,
         Brier_var) %>%
  pivot_longer(col = c(-Bootstrap_Index, -CPM, -Validation_Dataset),
               names_to = "Metric_Type") %>%
  separate(Metric_Type, into = c("Metric", "Type"), sep = "_") %>%
  pivot_wider(id_cols = c("Bootstrap_Index", "CPM", "Validation_Dataset", "Metric"),
              names_from = "Type",
              values_from = "value") 


##plot results from the empirical study
Bootstrap_InternalValidation %>%
  left_join(Bootstrap_InternalValidation %>%
              filter(Validation_Dataset == "mean_imputed_val") %>%
              select(-Validation_Dataset) %>%
              rename("MeanMode_est" = "est",
                     "MeanMode_var" = "var"),
            by = c("Bootstrap_Index", "CPM", "Metric")) %>%
  left_join(Bootstrap_InternalValidation %>%
              filter(Validation_Dataset == "RI_val") %>%
              select(-Validation_Dataset) %>%
              rename("RI_est" = "est",
                     "RI_var" = "var"),
            by = c("Bootstrap_Index", "CPM", "Metric")) %>%
  left_join(Bootstrap_InternalValidation %>%
              filter(Validation_Dataset == "MInoY_val") %>%
              select(-Validation_Dataset) %>%
              rename("MInoY_est" = "est",
                     "MInoY_var" = "var"),
            by = c("Bootstrap_Index", "CPM", "Metric")) %>%
  left_join(Bootstrap_InternalValidation %>%
              filter(Validation_Dataset == "raw_val")  %>%
              select(-Validation_Dataset)%>%
              rename("PSM_est" = "est",
                     "PSM_var" = "var"),
            by = c("Bootstrap_Index", "CPM", "Metric")) %>%
  select(Bootstrap_Index, CPM, Validation_Dataset, Metric, contains("est")) %>%
  pivot_longer(cols = ends_with("_est")) %>%
  mutate(bias = est - value) %>%
  group_by(CPM, Validation_Dataset, Metric, name) %>%
  summarise(bias_mean = mean(bias, na.rm = TRUE),
            bias_lower = quantile(bias, 0.025, na.rm = TRUE),
            bias_upper = quantile(bias, 0.975, na.rm = TRUE),
            .groups = "drop") %>%
  mutate(name = str_remove(name, "_est"),
         name = forcats::fct_relevel(name,
                                     c("MeanMode", "RI", "MInoY", "PSM")),
         name = forcats::fct_recode(name,
                                    "Targetting\n MI-no Y Performance" = "MInoY",
                                    "Targetting\n Mean/Mode Performance" = "MeanMode",
                                    "Targetting\n PSM Performance" = "PSM",
                                    "Targetting\n RI Performance" = "RI"),
         
         CPM = str_remove(CPM, "PR_"),
         CPM = forcats::fct_recode(CPM,
                                   "CCA\n developed CPM" = "CCA",
                                   "RI\n developed CPM" = "RI",
                                   "PSM\n developed CPM" = "patternsubmodel",
                                   "MI with Y\n developed CPM" = "MIwithY",
                                   "MI no Y\n developed CPM" = "MInoY",
                                   "Mean/Mode\n developed CPM" = "meanimputed"),
         CPM = forcats::fct_relevel(CPM,
                                    c("CCA\n developed CPM",
                                      "Mean/Mode\n developed CPM",
                                      "RI\n developed CPM",
                                      "MI with Y\n developed CPM",
                                      "MI no Y\n developed CPM",
                                      "PSM\n developed CPM")),
         Validation_Dataset = str_remove(Validation_Dataset, "_val"),
         Validation_Dataset = forcats::fct_recode(Validation_Dataset,
                                                  "PSM" = "raw",
                                                  "MI with Y" = "MIwithY",
                                                  "MI no Y" = "MInoY",
                                                  "Mean/Mode" = "mean_imputed"),
         Validation_Dataset = forcats::fct_relevel(Validation_Dataset,
                                                   c("CCA",
                                                     "Mean/Mode",
                                                     "RI",
                                                     "MI with Y",
                                                     "MI no Y",
                                                     "PSM"))) %>%
  rename("Target" = name) %>%
  ggplot(aes(x = bias_mean,
             y = Validation_Dataset,
             color = Metric)) +
  geom_point(aes(shape = Metric)) +
  geom_errorbar(aes(xmin = bias_lower, 
                    xmax = bias_upper), width=.1) +
  scale_color_brewer(palette = "Set1") +
  xlab("Bias") +
  ylab("Validation Data Imputation Method") +
  geom_vline(data = data.frame("Metric" = c("AUC",
                                            "Brier Score",
                                            "Calibration Intercept",
                                            "Calibration Slope"),
                               "bias_mean" = c(0,0,0,0)),
             aes(xintercept = bias_mean),
             linetype = "dashed") +
  ggh4x::facet_grid2(CPM ~ Target, scales = "fixed") +
  ggtitle("Targetted Performance ") +
  theme_minimal(base_size = 12) +
  theme(legend.position = "bottom",
        panel.background = element_rect(fill = "gray90"),  
        panel.spacing.x = unit(0.5, "lines"),
        axis.text.x = element_text(angle = 45, vjust = 1, hjust=1),
        plot.title = element_text(size = 14, hjust = 0.5),
        panel.border = element_rect(color = "black", fill = NA, linewidth = 0.5)) 

ggsave(file = here::here("Manuscript", "Fig6.tiff"), 
       height = 10, width = 10, 
       dpi = 300)


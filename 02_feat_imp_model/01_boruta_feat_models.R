### Script to run boruta feature importance seletion #####
# S.Vickery April 2024 #

## INPUTS ##
# 1. data frame of cleaned data
# 2. Target column name
# 3. Seed for reproducibility
# 4. cleaned dataset modality files (.rds)

## OUTPUTS ##
# 1. Selected Boruta features through 10 fold selection (.rds)
# 2. Iteration data frame with accuracy measures from additive features models (.rds)
# 3. Comparison model accuaracy of itation selected features and all features (.rds)
# 4. Comparison model accuracy of all boruta features and all features (.rds)

# Set working directory #
wd <- ".../Berlin_Back/"
setwd(wd)

library("Boruta")
library("caret")
library("dplyr")
library("fs")
library("ggplot2")
library("randomForest") # 
library("ranger")
library("tibble") #
library("tidyr")
library("purrr")
library("pROC")
library("tictoc")


##################### INPUTS ###################################################
################################################################################

input_files <- list.files(path = path_wd("01_Data", "Cleaned"),
                          pattern = "ML_cleaned.*\\.rds$")

## Source feature reduction functions ##
source(path_wd("02_code", "func", "feat_engineering", ext = "R"))

# The name of the target column used for classification #
target <- "Demo_clinic_LBP"

# Seed for reproducibility of sub-sampling and RF classification #
seed <- 1027

# numer of folds we will be using 10 #
n_fold <- 10

######## Boruta feature selection & RF classification comparison ################
tic()
for (modality in 1:length(input_files)) {
  # This refers to the modality or sample description that will be used 
  # for output names E.g. Clinic#
  modality_name <- sub("_ML_cleaned_.*", "", input_files[modality])
  
  print(paste("Start modality:", modality_name))
  tic(paste0(modality_name, " finished"))
  
  print(paste("read in", modality_name, "data frame"))
  input_data <- readRDS(file = path_wd("01_Data",
                                       "Cleaned",
                                       input_files[modality])) %>%
    # remove subject id column #
    select(-Study_ID)
  print("##DONE##")
  
  # 10-fold training and testing using both boruta selected features & all 
  # features
  set.seed(seed)
  train_test_folds <- createFolds(input_data[[target]], k = n_fold, list = TRUE,
                                  returnTrain = TRUE)
  
  for (fold in 1:n_fold) {
    print(paste("starting Fold", fold, "of modality", modality_name))
    
    tic(paste("Fold", fold, modality_name))
    
    print("create tain-test splits")
    train_data <- input_data[train_test_folds[[fold]], ]
    test_data <- input_data[-train_test_folds[[fold]], ]
    print("##DONE##")
    
    print("Run Boruta feature importance on training fold")
    boruta_imp_dat <- boruta_feat_imp(data = train_data,
                                      target = target,
                                      seed = seed,
                                      max_runs = 2000,
                                      ntree = 1000)
    print("##DONE##")
    
    print("Save Boruta feature data")
    write.csv(boruta_imp_dat, file = path_wd("04_outputs",
                                             paste(modality_name,
                                                   nrow(input_data), "X", 
                                                   ncol(input_data),
                                                   "boruta_feat_imp_seed",
                                                   seed, "fold", fold, sep = "_"),
                                             ext = "csv"),
              row.names = FALSE)
    
    # save output as rds #
    saveRDS(boruta_imp_dat, file = path_wd("04_outputs",
                                           paste(modality_name,
                                                 nrow(input_data), "X", 
                                                 ncol(input_data),
                                                 "boruta_feat_imp_seed",
                                                 seed, "fold", fold, sep = "_"),
                                           ext = "rds"))
    print("##DONE##")
    
    print("Compare Boruta features accuracy to all features in test data")
    boruta_compare <- feature_select_comparison(train_data = train_data,
                                                test_data = test_data,
                                                target = target,
                                                selected_features = boruta_imp_dat$features,
                                                ntrees = 1000,
                                                seed = seed)
    print("##DONE##")
    
    
    print("Save comparison outputs")
    # save results #
    saveRDS(boruta_compare, file = path_wd("04_outputs", 
                                           paste(modality_name,
                                                 nrow(input_data),
                                                 "X",
                                                 ncol(input_data),
                                                 "boruta_feat_Whole",
                                                 length(boruta_imp_dat$features),
                                                 "comparison_seed",
                                                 seed, "fold", fold, sep = "_"), 
                                           ext = "rds"))
    print("##DONE##")
    toc()
  }
  toc()
}
toc()
################################################################################

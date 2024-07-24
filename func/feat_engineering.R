# Functions used for boruta feature engineering and classification #


############## Boruta feature importance selection ######################
################################################################################

# create a data frame with confirmed important features using boruta package #

boruta_feat_imp <- function(data,
                            target,
                            seed = 123,
                            max_runs = 500,
                            ntree = 500) {
  
  require("caret")
  require("Boruta")
  require("ranger")
  require("purrr")
  require("dplyr")
  require("tidyr")
  
  # Create folds
  set.seed(seed)

  train_features <- data[, !(colnames(data) %in% target)]
  train_target <- data[[target]]
  
  
  print("starting Boruta feature selection")
  set.seed(seed)
  boruta_result <- Boruta(x = train_features, 
                          y = train_target,
                          doTrace = 1,
                          getImp = getImpRfZ,
                          maxRuns = max_runs,
                          ntree = ntree)
  print("#### DONE ####")
  
  confirmed_features_data <- attStats(boruta_result) %>% 
    rownames_to_column(var = "features") %>% 
    data.frame() %>% 
    filter(decision == "Confirmed") %>% 
    select(features, meanImp) %>%
    mutate(imp_rank = rank(desc(meanImp))) 
  
  return(confirmed_features_data)
  
}
################################################################################

############## Boruta K- Fold feature importance selction ######################
################################################################################

# Using K-fold of training data Boruta is conducted to determine the most important
# features for prediction
boruta_kfold <- function(data,
                         target,
                         k_folds,
                         seed = 123,
                         max_runs = 500,
                         ntree = 500) {
  
  require("caret")
  require("Boruta")
  require("ranger")
  require("purrr")
  require("dplyr")
  require("tidyr")
  
  # Create folds
  set.seed(seed)
  folds <- createFolds(data[[target]], k = k_folds, returnTrain = TRUE)
  
  # Define a function to perform Boruta on a single fold
  process_fold <- function(fold_indices) {
    train_indices <- unlist(fold_indices)
    
    train_data <- data[train_indices, ]
    
    train_features <- train_data[, !(colnames(train_data) %in% target)]
    train_target <- train_data[[target]]
    
   
    print("starting Boruta feature selection")
    set.seed(seed)
    boruta_result <- Boruta(x = train_features, 
                            y = train_target,
                            doTrace = 1,
                            getImp = getImpRfZ,
                            maxRuns = max_runs,
                            ntree = ntree)
    print("#### DONE ####")
    
    confirmed_features_data <- attStats(boruta_result) %>% 
      rownames_to_column(var = "features") %>% 
      data.frame() %>% 
      filter(decision == "Confirmed") %>% 
      select(features, meanImp) %>%
      mutate(imp_rank = rank(desc(meanImp))) 
    
    return(confirmed_features_data)
  }
  
  # # Use map to process each fold and combine results using bind_rows()
  result_df <- map(folds, process_fold) %>%
    bind_rows(.id = "folds") %>%
    pivot_wider(names_from = folds,
                values_from = c(meanImp, imp_rank),
                names_glue = "{.value}_{folds}") %>%

  return(result_df)
}
################################################################################

###### Comparison of RF models with selected features and All features #########
################################################################################

## conduct RF classification model using all features and selected features.
## A test set should be used that has not be applied in any feature selection 
## approach
feature_select_comparison <- function(train_data,
                                      test_data,
                                      target,
                                      selected_features,
                                      ntrees = 500,
                                      seed = 123) {
  set.seed(seed)

  # Extract all feature names 
  all_features <- names(train_data)[!names(train_data) %in% target]

  # Create a grid of values for tuning mtry
  # grid for selected features
  mtry_grid_select <- expand.grid(mtry = 1:sqrt(length(selected_features)),
                                  splitrule = "gini",
                                  min.node.size = c(5, 10))
  # grid for all features #
  mtry_grid_all <- expand.grid(mtry = 1:sqrt(length(all_features)),
                                  splitrule = "gini",
                                  min.node.size = c(5, 10))
  set.seed(seed)  
  cv_control <- trainControl(method = "cv", 
                             number = 10,
                             search = "grid",
                             classProbs = TRUE
  ) 

  # Train a model using the selected features and tuning mtry
  print("Training RF model using selected features")
  set.seed(seed)
  model_selected_features <- train(x = train_data[, selected_features],
                                   y = train_data[[target]],
                                   method = "ranger", 
                                   trControl = cv_control,
                                   tuneGrid = mtry_grid_select,
                                   num.trees = ntrees)
  print("## Done ##")
    
  # Test the model on the test set
  print("Testing boruta feature selection model on hold out test set")
  prediction_selected <- predict(model_selected_features, 
                                 test_data[, c(target, selected_features), drop = FALSE])
  print("## Done ##")
  test_data_target <- test_data[[target]]
    
    
  # Calculate accuracy, AUC, sensitivity, and specificity
  print("creating boruta confusion matrix")
  ## need to add positive = "yes to get the correct sens and spec
  cm_selected <- confusionMatrix(prediction_selected, test_data_target, positive = "yes")
  print("## Done ##")
  accuracy_selected <- round(cm_selected$overall[["Accuracy"]], digits = 2)
  print("calculating AUC")
  auc_selected <- round(auc(roc(as.numeric(test_data_target), 
                                as.numeric(prediction_selected))), digits = 2)
  print("## Done ##")
    
  sensitivity_selected <- round(cm_selected$byClass[["Sensitivity"]], digits = 2)
  specificity_selected <- round(cm_selected$byClass[["Specificity"]], digits = 2)
  precision_selected <- round(cm_selected$byClass[["Precision"]], digits = 2)
    
  ### Using all features ###
    
  # Train a model using all features
  print("Training RF model using all features")
  set.seed(seed)
  model_all_features <- train(x = train_data[, all_features],
                              y = train_data[[target]],
                              method = "ranger", 
                              trControl = cv_control,
                              tuneGrid = mtry_grid_all,
                              num.trees = ntrees)
  print("## Done ##")
    
  # Test the model on the test set
  print("Testing RF model with all features on hold out test set")
  prediction_all <- predict(model_all_features, newdata = test_data[, all_features])
  print("## Done ##")
    
  # Calculate accuracy, AUC, sensitivity, and specificity
  print("create all feature confusion matrix")
  ## need to add positive = "yes to get the correct sens and spec
  cm_all <- confusionMatrix(prediction_all, test_data_target, positive = "yes")
  print("## Done ##")
  accuracy_all <- cm_all$overall[["Accuracy"]]
  print("calculate AUC for all features")
  auc_all <- round(auc(roc(as.numeric(test_data_target), 
                           as.numeric(prediction_all))), digits = 2)
  print("## Done ##")
    
  sensitivity_all <- round(cm_all$byClass[["Sensitivity"]], digits = 2)
  specificity_all <- round(cm_all$byClass[["Specificity"]], digits = 2)
  precision_all <- round(cm_all$byClass[["Precision"]], digits = 2)
        
  # Create a tibble row with the results
  print("Creating output list")
  return(list(
    test_data_target = test_data_target,
    boruta_feats = selected_features,
    boruta_accuracy = accuracy_selected,
    boruta_prediction = prediction_selected,
    boruta_AUC = auc_selected,
    boruta_sens = sensitivity_selected,
    boruta_spec = specificity_selected,
    boruta_prec = precision_selected,
    boruta_cm_tbl = cm_selected$table,
    all_feats = all_features,
    all_feats_accuracy = accuracy_all,
    all_feats_prediction = prediction_all,
    all_feats_AUC = auc_all,
    all_feats_sens = sensitivity_all,
    all_feats_spec = specificity_all,
    all_feats_prec = precision_all,
    all_feats_cm_tbl = cm_all$table
  ))
  print("## Done ##")

}


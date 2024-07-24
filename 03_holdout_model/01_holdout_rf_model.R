## using hold-out sample to test the most important & 
## robust features using RF model #
## INPUT: Holdout .csv data created from 02_cleaning.R
## INPUT: Boruta selected varaibles summary cerated by 04_feature_imp.R
## OUTPUT: classifciation model performance with Boruta 100% features and all features (.csv)
## OUTPUT: model performance column plot (Figure 3B)

# Set working directory #
wd <- "..../Berlin_Back/"
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

#### Input ####

# input df #
input_df <- readRDS(file = path_wd("01_Data", "Cleaned", "Ml_cleaned", 
                                   "Holdout_ALL_ML_Clinic_BP_dim_116X145.rds"))

# Most important and robust features across all modalities #
feat_df <- read.csv(file = path_wd("04_outputs", 
                                   "boruta_robust_all_modality_vars.csv"))

robust_feats <- feat_df |>
  filter(selected_percent == 100)

# seed for reproducibility #
seed <- 18
  
#### RF model ####

## Source feature reduction functions ##
source(path_wd("02_code", "func", "feat_engineering", ext = "R"))

# The name of the target column used for classification #
target <- "Demo_clinic_LBP"

# test when removing the spine mouse data #
#input_df <- input_df |> select(-c(starts_with("SM_")))

set.seed(seed)
train_idx <- createDataPartition(input_df$Demo_clinic_LBP, p = 0.8, 
                                 list = TRUE)

# train-test split #
train_input <- input_df[train_idx$Resample1, ]
test_input <- input_df[-train_idx$Resample1, ]

# Run RF model on boruta features and all features to compare results in test set #
robust_feats_RF_compare <- feature_select_comparison(train_data = train_input,
                                            test_data = test_input,
                                            target = target,
                                            selected_features = robust_feats$boruta_feats,
                                            ntrees = 1000,
                                            seed = seed)

# 5-fold comparsion using boruta robust features #

n_fold = 5

set.seed(seed)
train_test_folds <- createFolds(input_df$Demo_clinic_LBP, k = n_fold, 
                                list = TRUE, returnTrain = TRUE)

# create empty list to fill with 5 -fold comparison results #
comp_res <- vector(mode = "list", length = n_fold)

for (fold in 1:n_fold) {

    print("create tain-test splits")
  train_data <- input_df[train_test_folds[[fold]], ]
  test_data <- input_df[-train_test_folds[[fold]], ]
  print("##DONE##")
  
  print("Compare Boruta features accuracy to all features in test data")
  boruta_compare <- feature_select_comparison(train_data = train_data,
                                              test_data = test_data,
                                              target = target,
                                              selected_features = robust_feats$boruta_feats,
                                              ntrees = 1000,
                                              seed = seed)
  print("##DONE##")
  
  # print("Save comparison outputs")
  # # save results #
  # saveRDS(boruta_compare, file = path_wd("04_outputs", 
  #                                        paste("holdout_sample",
  #                                              nrow(input_df),
  #                                              "X",
  #                                              ncol(input_df),
  #                                              "boruta_robust_feats",
  #                                              length(robust_feats$boruta_feats),
  #                                              "comparison_seed",
  #                                              seed, "fold", fold, sep = "_"), 
  #                                        ext = "rds"))
  # print("##DONE##")
  
  print("save outputs into list")
  comp_res[[fold]] <- boruta_compare
  print("##DONE##")
  
}

# Extract main results and plot comparison #

metrics <- c("boruta_accuracy", "boruta_AUC", "boruta_sens", "boruta_spec",
             "all_feats_accuracy", "all_feats_AUC","all_feats_sens", 
             "all_feats_spec")

holdout_rf_res <- comp_res |>
  map(~ pluck(.x, function(x) x[metrics])) |> 
  bind_rows() |>
  summarise(
    across(
      .cols = c(boruta_accuracy,
                boruta_AUC, 
                boruta_sens, 
                boruta_spec,
                all_feats_accuracy,
                all_feats_AUC,
                all_feats_sens,
                all_feats_spec),
      .fns = list(mean = mean,
                  sd = sd),
      .names = "{col}_{fn}"
    ),
    across(
      .cols = ends_with("_sd"),
      .fn = list(CI = function(x) {qt(1 - (0.05 / 2), n()-1) * x / sqrt(n())}),
      .names = "{col}_{fn}"
    )
  ) |>
  select(-c(ends_with("_sd"))) |>
  pivot_longer(cols = everything(), names_to = "name", values_to = "value") |> 
  separate_wider_regex(name, c(method = "[^_]*?", "_", unit = "\\w+")) |>
  mutate(unit = case_when(
    unit == "accuracy_sd_CI" ~ "accuracy_CI",
    unit == "AUC_sd_CI" ~ "AUC_CI",
    unit == "sens_sd_CI" ~ "sens_CI",
    unit == "spec_sd_CI" ~ "spec_CI",
    unit == "feats_accuracy_mean" ~ "accuracy_mean",
    unit == "feats_AUC_mean" ~ "AUC_mean",
    unit == "feats_sens_mean" ~ "sens_mean",
    unit == "feats_spec_mean" ~ "spec_mean",
    unit == "feats_accuracy_sd_CI" ~ "accuracy_CI",
    unit == "feats_AUC_sd_CI" ~ "AUC_CI",
    unit == "feats_sens_sd_CI" ~ "sens_CI",
    unit == "feats_spec_sd_CI" ~ "spec_CI",
    .default = unit
  )) |>
  separate_wider_delim(unit, delim = "_", names = c("metric", "unit")) |>
  pivot_wider(id_cols = c(method, metric), 
              names_from = unit, 
              values_from = value) 

# save performance data frame #
write.csv(holdout_rf_res, file = path_wd("04_outputs",
                                         paste0("hold_out_5fold_performance_seed", 
                                                seed, ".csv")),
          row.names = FALSE)

holdout_rf_res <- read.csv(path_wd("04_outputs", "hold_out_5fold_performance_seed18.csv"))

# create bar plot for all & boruta test performance #
holdout_rf_res |>
  ggplot(aes(x = metric, y = mean, fill = method)) +
  geom_bar(stat="identity",position="dodge") +
  #scale_fill_brewer(palette = "Paired") +
  scale_fill_grey() +
  geom_errorbar(aes(ymin = mean - CI, 
                    ymax = mean + CI), 
                position=position_dodge(0.8), width = 0.5) +
  scale_y_continuous(limits = c(-0.05, 1.03), breaks = seq(0, 1, 0.1)) +
  scale_x_discrete(labels = c("Acc", "AUC", "Sens", "Spec")) +
  xlab("Performance Metric") +
  ylab("Value") +
  theme_classic(base_size = 15) +
  theme(legend.position = c(0.6, 0.9), legend.title = element_blank())
  
# save plot #
ggsave(filename = path_wd("04_outputs", 
                          paste0("hold_out_5fold_perf_bar_plot_seed_grey", 
                                 seed), ext = "png"), dpi = 800)



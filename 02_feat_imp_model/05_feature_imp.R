## Summarising Boruta feature selection across all dataset modalities ##
## 100% selected features then used for classification in holdout sample
## INPUTS: Boruta feature selection from 02_model_perf_tables
## OUTPUTS: all selected variables by Boruta (Supplementary Table 20) 
## & variables that were never selected

# Set working directory #
wd <- "..../Berlin_Back/"
setwd(wd)


library("dplyr")
library("fs")
library("ggplot2")
library("tibble") 
library("tidyr")
library("purrr")
library("stringr")
library("readr")


# read in output csv files from boruta feature reduction #

fils <- list.files(path_wd("04_outputs"), 
                   pattern = "_boruta_whole_features.csv", full.names = TRUE)

# read in whole dataset cleaned df #

all_data <- readRDS(file = path_wd("01_Data", "Cleaned", 
                                   "MRI_Quest_Clinic_SM_SUM_ML_cleaned_Clinic_BP_dim_492X145", 
                                   ext = "rds"))

# gather all features selected by boruta feat selection #
boruta_features <- fils |> 
  set_names(basename) |>
  map(read_csv) |>
  map(~ pluck(.x, function(x) x["boruta_feats"])) |>
  bind_rows() |>
  distinct()

# gather all features in cleaned dataset
all_features <- all_data |>
  select(-Study_ID, -Demo_clinic_LBP) |>
  colnames()

# features that were never selected by boruta across all modalities 
never_select_feat <- setdiff(all_features, boruta_features$boruta_feats)
# create df for saving output
poor_feats_df <- data.frame(not_selected = never_select_feat)

# save df as csv #
write.csv(poor_feats_df, 
          file = path_wd("04_outputs", "boruta_never_selected_feats", ext = "csv"), 
          row.names = FALSE)


# read in boruta outputs and sum() the No. selected and importance score #
# to present the ribustness of each features across modalities #
boruta_features_robust <- fils |> 
  set_names(basename) |>
  map(read_csv) |>
  bind_rows() |>
  summarise(
    selected_No = sum(feature_n),
    feat_imp = sum(feature_meanImp),
    .by = boruta_feats
  )

boruta_robust_demo <- boruta_features_robust |>
  filter(boruta_feats %in% c("Demo_age", "Demo_sex", "Demo_BMI")) |>
  summarise(
    selected_percent = (selected_No / 150) * 100,
    feat_imp_avg = feat_imp / 15,
    .by = boruta_feats
  ) |> # arrange appropriately 
  arrange(desc(selected_percent)) |>
  arrange(desc(feat_imp_avg))

# save df as csv #
write.csv(boruta_robust_demo, 
          file = path_wd("04_outputs", "boruta_robust_all_Demo", ext = "csv"), 
          row.names = FALSE)

boruta_robust_modalities <- boruta_features_robust |>
  filter(!boruta_feats %in% c("Demo_age", "Demo_sex", "Demo_BMI")) |>
  summarise(
    selected_percent = (selected_No / 80) * 100,
    feat_imp_avg = round(feat_imp / 8, 2),
    .by = boruta_feats
  ) |> # arrange appropriately
  arrange(desc(selected_percent)) |>
  arrange(desc(feat_imp_avg))

# save df as csv #
write.csv(boruta_robust_modalities, 
          file = path_wd("04_outputs", "boruta_robust_all_modality_vars", 
                         ext = "csv"), 
          row.names = FALSE)

# keep only very robust (>90%) features #
boruta_robust_modalities_90 <- boruta_robust_modalities |> 
  filter(selected_percent >= 90)

# save df as csv #
write.csv(boruta_robust_modalities_90, 
          file = path_wd("04_outputs", "boruta_robust_all_modality_vars_90", 
                         ext = "csv"), 
          row.names = FALSE)




## create results tables for classification and features selection ##
## Using output from Boruta feature selection and classification models from 
## 01_boruta_feat_models and create result tables for both test set RF classification
## as well as the 10-fold feature selection within each dataset modality

## INPUT: model outputs (.rds) from 01_boruta_feat_models
## OUTPUTS: 15 performance table (.csv) and boruta important feature tables (.csv)

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


# read in output rds files from LBP classification with and without feature 
# reduction

fils <- list.files(path_wd("04_outputs"), 
                   pattern = "_comparison_seed_", full.names = TRUE)

# acccuracy measurement column names #
accuracy_names <- c("boruta_accuracy", "boruta_AUC", "boruta_sens", "boruta_spec",
                    "boruta_prec", "all_feats_accuracy", "all_feats_AUC", 
                    "all_feats_sens", "all_feats_spec", "all_feats_prec")

###### Create Function for 95% CI ###########

# boruta feature selection data #
# NEED To ADD MORE COMMENTS EXPLAINING PROCESS #
boruta_data <- fils |> 
  set_names(basename) |>
  map(readRDS) |>
  map(~ pluck(.x, function(x) x[accuracy_names])) |>
  bind_rows(.id = "fil_name") |>
  mutate(
    sample_name = as.factor(tolower(str_extract(fil_name, ".*?(?=_[0-9])"))),
    boruta_method  = as.factor(case_when(
    grepl("feat_Whole", fil_name, ignore.case = TRUE) ~ "boruta_whole",
    .default = NA
  )),
  boruta_feat_num = as.numeric(case_when(
    boruta_method == "boruta_whole" ~ str_extract(fil_name, "(?<=feat_Whole_)[0-9]+"),
    .default = NA
  )),
  sample_size = as.numeric(str_extract(fil_name, "\\d+")),
  all_feature_num = as.numeric(str_extract(fil_name, "(?<=_X_)\\d+"))
  ) |>
  select(-fil_name) |>
  #group_by(sample_name, boruta_method) |>
   summarise(
     across(
       .cols = c(boruta_accuracy,
                 boruta_AUC, 
                 boruta_sens, 
                 boruta_spec, 
                 boruta_prec,
                 all_feats_accuracy,
                 all_feats_AUC,
                 all_feats_sens,
                 all_feats_spec,
                 all_feats_prec,
                 boruta_feat_num),
       .fns = list(mean = mean,
            sd = sd),
       .names = "{col}_{fn}"
       ),
     across(
       .cols = ends_with("_sd"),
       .fn = list(CI = function(x) {qt(1 - (0.05 / 2), n()-1) * x / sqrt(n())}),
       .names = "{col}_{fn}"
     ),
     all_feat_num = mean(all_feature_num),
     sample_size = mean(sample_size),
     .by = c(sample_name, boruta_method)
   ) |>
  relocate(contains("boruta_"), .after = sample_name) |>
  relocate(sample_size, .after = sample_name)
  

# save extracted and summarised data 
write.csv(boruta_data, file = path_wd("04_outputs", "boruta_summary_feature_reduction",
                                      ext = "csv"), row.names = FALSE)
saveRDS(boruta_data, file = path_wd("04_outputs", "boruta_summary_feature_reduction",
                                    ext = "rds"))


# selected features data from 10fold data #

feature_data <- fils |> 
  set_names(basename) |>
  map(readRDS) |>
  map(~ pluck(.x, function(x) x["boruta_feats"])) |>
  bind_rows(.id = "fil_name") |>
  mutate(
    fil_name = sub("(_feat_Whole)_\\d+.*", "\\1", fil_name),
  ) |>
  summarise(
    feature_n = n(),
    feature_perc = round((feature_n / 10) * 100, 1), ### NOTE HARD CODED With number of sub-samples
    .by = c(fil_name, boruta_feats)
  ) |>
  mutate(
    boruta_method  = as.factor(case_when(
      grepl("feat_Whole", fil_name, ignore.case = TRUE) ~ "boruta_whole",
      .default = NA
    )),
    fil_name = tolower(sub("(\\w)_\\d+.*", "\\1", fil_name))
  ) |>
  relocate(boruta_method, .after = fil_name)

# ranks of selected features #
feat_imp_fils <- list.files(path_wd("04_outputs"), 
                            pattern = "feat_imp_.*\\.rds", full.names = TRUE) 

feature_imp_data <- feat_imp_fils |> 
  set_names(basename) |>
  map(readRDS) |>
  list_rbind(names_to = "fil_name") |>
  select(!starts_with("imp_rank")) |>
  #select(!starts_with("meanImp_")) |>
  mutate(
    fil_name = sub("(_feat_imp_seed)_\\d+.*", "\\1", fil_name)
  ) |>
  summarise(
    feature_meanImp = round(mean(meanImp), digits = 2),
    .by = c(fil_name, features)
  ) |>
  mutate(
    feature_rank = rank(-feature_meanImp),
    .by = fil_name
  ) |>
  mutate(fil_name = tolower(sub("(\\w)_\\d+.*", "\\1", fil_name)))

# Create new strict and All df that joins feature data #

feat_all_data <- feature_data |>
  #filter(boruta_method == "boruta_all") |>
  full_join(feature_imp_data, by = join_by(fil_name, boruta_feats == features)) |>
  group_by(fil_name, boruta_method) |>
  arrange(desc(-feature_rank)) |>
  nest() |>
  mutate(tbl_path = path_wd("04_outputs", str_glue("{fil_name}_{boruta_method}_features"), ext = "csv"))

# write csv files for each boruta method and data modality
walk2(feat_all_data$data, feat_all_data$tbl_path, readr::write_csv)

# create a table for overall reduction of features using Boruta
# Here we need to use all features selected across all sub-samples to determine
# the percentage of features never selected in each modality
# The total number of features is povided in "boruta_data" and all the features is
# shown in "feature_data"

boruta_feat_reduct <- feature_data |>
  summarise(
    num_bor_feats = n(),
    .by = fil_name
  ) |>
  rename(sample_name = fil_name) |>
  full_join(boruta_data, by = join_by(sample_name)) |>
  select(sample_name, num_bor_feats, all_feat_num) |>
  mutate(
    boruta_perc = (num_bor_feats / all_feat_num) * 100,
    boruta_reduct_perc = 100 - boruta_perc
  )
  
# save output table for plotting #
write.csv(boruta_feat_reduct, file = path_wd("04_outputs", 
                                             "boruta_feature_reduction_percentage",
                                      ext = "csv"), row.names = FALSE)
saveRDS(boruta_feat_reduct, file = path_wd("04_outputs", 
                                           "boruta_feature_reduction_percentage",
                                    ext = "rds"))

  
  

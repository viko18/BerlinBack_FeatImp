## Summarise results across all models ##
## create RF model classification performance table from 02_model_perf_tables ##
## INPUT: individual output tables from datasets created by 02_model_perf_tables ##
## OUTPUT: The preformance table of all models (Supplementary Table 7) ##
## OUTPUT: AUC vs feature number line plots across all datasets and separated into
## single, dual, & multi-modalities (Fig. 2A-C)
## OUTPUT: amount of feature reduction per dataset modality column plot ##

# Set working directory #
wd <- ".../Berlin_Back/4"
setwd(wd)

library("dplyr")
library("fs")
library("ggplot2")
library("tibble") 
library("tidyr")
library("Polychrome")


boruta_sum_data <- readRDS(file = path_wd("04_outputs", 
                                          "boruta_summary_feature_reduction",
                                          ext = "rds")) |>
  # change the levels of sample name for plot organisation #
  mutate(
    sample_name = factor(sample_name, level = c("quest", "clinic", "sm_sum", "mri", 
                                                "quest_clinic", "quest_sm_sum", "mri_quest", 
                                                "clinic_sm_sum", "mri_clinic", "mri_sm_sum",
                                                "quest_clinic_sm_sum", "mri_quest_clinic", 
                                                "mri_quest_sm_sum", "mri_clinic_sm_sum",
                                                "mri_quest_clinic_sm_sum"))
  )

#####------------ complete model accuracy summary table ----------------########

# boruta whole (liberal model) #
model_res <- boruta_sum_data |>
  filter(boruta_method == "boruta_whole") |>
  select(-c(contains("prec_"),
            ends_with("sd"),
            boruta_method)) |>
  pivot_longer(cols = -c(sample_name, sample_size)) |>
  separate_wider_regex(name, c(method = "[^_]*?", "_", unit = "\\w+")) |>
  mutate(unit = case_when(
    unit == "accuracy_sd_CI" ~ "accuracy_CI",
    unit == "AUC_sd_CI" ~ "AUC_CI",
    unit == "sens_sd_CI" ~ "sens_CI",
    unit == "spec_sd_CI" ~ "spec_CI",
    unit == "feat_num_sd_CI" ~ "feat_num_CI",
    unit == "feats_accuracy_mean" ~ "accuracy_mean",
    unit == "feats_AUC_mean" ~ "AUC_mean",
    unit == "feats_sens_mean" ~ "sens_mean",
    unit == "feats_spec_mean" ~ "spec_mean",
    unit == "feats_accuracy_sd_CI" ~ "accuracy_CI",
    unit == "feats_AUC_sd_CI" ~ "AUC_CI",
    unit == "feats_sens_sd_CI" ~ "sens_CI",
    unit == "feats_spec_sd_CI" ~ "spec_CI",
    unit == "feat_num" ~ "feat_num_mean",
    .default = unit
  )) |>
  pivot_wider(id_cols = c(sample_name, sample_size, method),
              names_from = unit,
              values_from = value) |> 
  replace_na(list(feat_num_CI = 0)) |>
  relocate(feat_num_mean, .after = sample_size) |>
  relocate(method, .after = sample_name) |>
  mutate(
    across(where(is.numeric), function(x) round(x, digits = 3)),
    feature_CI_95 = paste(c(feat_num_mean - feat_num_CI), 
                          "-", 
                          c(feat_num_mean + feat_num_CI), 
                          sep = " "),
    accuracy_CI_95 = paste(c(accuracy_mean - accuracy_CI), 
                          "-", 
                          c(accuracy_mean + accuracy_CI), 
                          sep = " "),
    AUC_CI_95 = paste(c(AUC_mean - AUC_CI), 
                      "-", 
                      c(AUC_mean + AUC_CI), 
                      sep = " "),
    sens_CI_95 = paste(c(sens_mean - sens_CI), 
                       "-", 
                       c(sens_mean + sens_CI), 
                       sep = " "),
    spec_CI_95 = paste(c(spec_mean - spec_CI), 
                       "-", 
                       c(spec_mean + spec_CI), 
                       sep = " ")
  ) |>
  select(-c(ends_with("_CI"))) |>
  relocate(starts_with("AUC"), .after = sample_size) |>
  relocate(starts_with("feat"), .after = AUC_CI_95) |>
  relocate(starts_with("accuracy"), .after = feature_CI_95) |>
  relocate(starts_with("sens"), .after = accuracy_CI_95) |>
  relocate(starts_with("spec"), .after = sens_CI_95)

write.csv(model_res, file = path_wd("04_outputs",
                                    "all_boruta_model_comparison_perf",
                                    ext = "csv"),
          row.names = FALSE)


#####------------- plots - AUC vs feat num ----------------------######

modality_labels <- c("Questionnaire (Q)", "Clinic (C)", "Back shape and function (S)", 
                     "MRI (M)", "Q+C", "Q+S", "Q+M", "C+S", "C+M", "S+M", 
                     "Q+C+S", "Q+C+M", "Q+S+M","C+S+M", "Q+C+S+M")

# all liberal model #
model_parisomy <- boruta_sum_data |>
  filter(boruta_method == "boruta_whole") |>
  select(sample_name, boruta_AUC_mean, boruta_AUC_sd_CI,
         all_feats_AUC_mean, all_feats_AUC_sd_CI,
         all_feat_num, boruta_feat_num_mean, boruta_feat_num_sd_CI) |>
  mutate(all_feat_num_CI = 0) |>
  pivot_longer(cols = -sample_name) |>
  separate_wider_regex(name, c(method = "[^_]*?", "_[^_]+_", unit = "\\w+")) |>
  mutate(unit = case_when(
    unit == "mean" ~ "AUC_mean",
    unit == "sd_CI" ~ "AUC_CI",
    unit == "AUC_sd_CI" ~ "AUC_CI",
    unit == "num" ~ "feat_num",
    unit == "num_sd_CI" ~ "feat_num_CI",
    unit == "num_CI" ~ "feat_num_CI",
    unit == "feat_num_sd_CI" ~ "feat_num_CI",
    unit == "num_mean" ~ "feat_num",
    .default = unit
  )) |>
  pivot_wider(id_cols = c(sample_name, method), 
              names_from = unit, 
              values_from = value) 

# create a 15 set color pallette using Polychrome package #
set.seed(189)
color_set_15 <- createPalette(15, c("#010101","#ff0000"), 
                              M=10000)
swatch(color_set_15)
# remove names so palette can be used by ggplot #
names(color_set_15) <- NULL
  
ggplot(model_parisomy, aes(x = feat_num, y = AUC_mean, color = sample_name,
                           shape = method)) +
  geom_point(size = 2.5) +
  geom_line(aes(group = sample_name)) +
  scale_color_manual(values = c(color_set_15), labels = modality_labels) +
  scale_shape_manual(values = c(17, 19)) +
  scale_x_continuous(limits = c(0, 160),breaks = seq(0, 160, 20)) +
  scale_y_continuous(limits = c(0.52, 0.74), breaks = seq(0.52, 0.74, 0.04)) +
  geom_errorbar(aes(ymin = AUC_mean - AUC_CI, 
                    ymax = AUC_mean + AUC_CI,
                    width = 3)) +
  # geom_errorbar(aes(xmin = feat_num - feat_num_CI, 
  #                   xmax = feat_num + feat_num_CI,
  #                   width = 3)) +
  labs(x = "Number of Fetaures", y = "AUC", color = "Modalitiy", 
       shape = "Feature Selection") +
  theme_classic() +
  theme(legend.position = "bottom")

ggsave(filename = path_wd("04_outputs", "model_parisomy", ext = "png"),
       dpi = 600, width = 12, height = 6, units = "in")

# modalitity names fro splitting data frame for plotting #
single_mod <- c("clinic", "mri", "quest", "sm_sum")

dual_mod <- c("mri_clinic", "quest_clinic", "clinic_sm_sum", "mri_quest", 
              "mri_sm_sum", "quest_sm_sum")

multi_mod <- c("mri_quest_clinic", "mri_clinic_sm_sum", "quest_clinic_sm_sum",
               "mri_quest_sm_sum", "mri_quest_clinic_sm_sum")

########################## Single modality plot ################################

model_parisomy |>
  filter(sample_name %in% single_mod) |>
  ggplot(aes(x = feat_num, y = AUC_mean, color = sample_name,
             shape = method)) +
  geom_point(size = 5) +
  scale_shape_manual(values = c(19, 17)) +
  geom_line(aes(group = sample_name), linewidth = 2) +
  scale_color_manual(values = c(color_set_15), 
                     labels = modality_labels) +
  scale_x_continuous(limits = c(5, 55), 
                     breaks = seq(5, 55, 15)) +
  scale_y_continuous(limits = c(0.50, 0.74), 
                     breaks = seq(0.50, 0.74, 0.04)) +
  geom_errorbar(aes(ymin = AUC_mean - AUC_CI, 
                    ymax = AUC_mean + AUC_CI,
                    width = 1.3)) +
  labs(x = "Number of Fetaures", y = "AUC", color = "Modalitiy", 
       shape = "Features") +
  theme_classic(base_size = 20) +
  theme(legend.position = "bottom") +
  guides(colour = guide_legend(nrow = 2), 
         shape = guide_legend(nrow = 2))

ggsave(filename = path_wd("04_outputs", "plots", "Single_model_parisomy", ext = "png"),
       dpi = 800, width = 30, height = 20, units = "cm")

#### dual modality plot ####

model_parisomy |>
  filter(sample_name %in% dual_mod) |>
  ggplot(aes(x = feat_num, y = AUC_mean, color = sample_name,
             shape = method)) +
  geom_point(size = 5) +
  scale_shape_manual(values = c(19, 17)) +
  geom_line(aes(group = sample_name), linewidth = 2) +
  scale_color_manual(values = c(color_set_15[5:10]), 
                     labels = modality_labels[5:10]) +
  scale_x_continuous(limits = c(10, 100), 
                     breaks = seq(10, 100, 20)) +
  scale_y_continuous(limits = c(0.50, 0.74), 
                     breaks = seq(0.50, 0.74, 0.04)) +
  geom_errorbar(aes(ymin = AUC_mean - AUC_CI, 
                    ymax = AUC_mean + AUC_CI,
                    width = 1.2)) +
  labs(x = "Number of Fetaures", y = "AUC", color = "Modalitiy", 
       shape = "Features") +
  theme_classic(base_size = 20) +
  theme(legend.position = "bottom") +
  guides(colour = guide_legend(nrow = 2), 
         shape = guide_legend(nrow = 2))

ggsave(filename = path_wd("04_outputs", "dual_model_parisomy", ext = "png"),
       dpi = 800, width = 30, height = 20, units = "cm")

#### Multi modality plot ####

model_parisomy |>
  filter(sample_name %in% multi_mod) |>
  ggplot(aes(x = feat_num, y = AUC_mean, color = sample_name,
             shape = method)) +
  geom_point(size = 5) +
  scale_shape_manual(values = c(19, 17)) +
  geom_line(aes(group = sample_name), linewidth = 2) +
  scale_color_manual(values = c(color_set_15[11:15]), 
                     labels = modality_labels[11:15]) +
  scale_x_continuous(limits = c(15, 150), 
                     breaks = seq(15, 150, 15)) +
  scale_y_continuous(limits = c(0.50, 0.74), 
                     breaks = seq(0.50, 0.74, 0.04)) +
  geom_errorbar(aes(ymin = AUC_mean - AUC_CI, 
                    ymax = AUC_mean + AUC_CI,
                    width = 1.2)) +
  geom_errorbar(aes(xmin = feat_num - feat_num_CI, 
                    xmax = feat_num + feat_num_CI,
                    width = 1.2)) +
  labs(x = "Number of Fetaures", y = "AUC", color = "Modalitiy", 
       shape = "Features") +
  theme_classic(base_size = 20) +
  theme(legend.position = "bottom") +
  guides(colour = guide_legend(nrow = 2), 
         shape = guide_legend(nrow = 2))

ggsave(filename = path_wd("04_outputs", "multi_model_parisomy", ext = "png"),
       dpi = 800, width = 30, height = 20, units = "cm")

######## Boruta Feature reduction improvement and percentage decrease ###########

bor_percent <- readRDS(file = path_wd("04_outputs", 
                                      "boruta_feature_reduction_percentage",
                                      ext = "rds")) |>
  # change the levels of sample name for plot organisation #
  mutate(
    sample_name = factor(sample_name, level = c("quest", "clinic", "sm_sum", "mri", 
                                                "quest_clinic", "quest_sm_sum", "mri_quest", 
                                                "clinic_sm_sum", "mri_clinic", "mri_sm_sum",
                                                "quest_clinic_sm_sum", "mri_quest_clinic", 
                                                "mri_quest_sm_sum", "mri_clinic_sm_sum",
                                                "mri_quest_clinic_sm_sum"))
  )

# modalitiy names with single mdalities having short names
mod_labels_short <- c("Q", "C", "S", "M",
                      "Q+C", "Q+S", "Q+M", "C+S", "C+M", "S+M", 
                      "Q+C+S", "Q+C+M", "Q+S+M","C+S+M",
                      "Q+C+S+M")

# coumn plot of boruta feature selection reduction across all data sets
bor_percent |>
  ggplot(aes(x = sample_name, y = boruta_reduct_perc, color = sample_name, fill = sample_name)) +
  geom_bar(position = "dodge", stat = "identity") +
  scale_color_manual(values = c(color_set_15)) +
  scale_fill_manual(values = c(color_set_15)) +
  scale_x_discrete(name = "Data Modality", labels = mod_labels_short,
                   guide = guide_axis(angle = 45)) +
  scale_y_continuous(name = "Boruta Feature Reduction (%)", 
                     limits = c(0, 90), breaks = seq(0, 90, 10)) +
  theme_classic(base_size = 20) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.3, hjust=1), 
        legend.position = "none")

ggsave(filename = path_wd("04_outputs", "Boruta_reduction_bar_plot", ext = "png"),
       dpi = 800, width = 30, height = 20, units = "cm")

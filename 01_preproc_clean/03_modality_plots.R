## Column plots showing sample distributions across all modalities ##
## Figure 1A top and bottom from manuscript ##
## INPUT: cleaned data from 02_cleaning.R saved at ../Berlin_Back/01_Data/cleaned/
## OUTPUT: column plots (Fig. 1A) as png files ##
## OUTPUT Dir: .../Berlin_Back/03_Outputs/ is created if needed to save all subsequent outputs

# Set working directory #
wd <- ".../Berlin_Back/"
setwd(wd)

library("dplyr")
library("fs")
library("ggplot2")
library("tibble") 
library("tidyr")
library("purrr")
library("Polychrome")

############ INPUTS ############################################################

# cleaned data file names #

clean_files <- list.files(path = path_wd("01_Data", "Cleaned"), 
                          pattern = "\\.rds$", full.names = TRUE)

# remove the holdout sample
clean_modalities <- clean_files[!grepl("Demo", clean_files)]

################################################################################

######################## OUTPUT Dir ############################################

# Create output directory

output_dir <- path_wd("04_Outputs")
if(!dir_exists(output_dir)) {dir_create(output_dir)}

################################################################################

# modality names
mod_names <- c("Q", "C", "S", "M",
               "Q+C", "Q+S", "Q+M", "C+S", "C+M", "S+M", 
               "Q+C+S", "Q+C+M", "Q+S+M","C+S+M",
               "Q+C+S+M")

clean_modalities |> 
  set_names(basename) |>
  map(readRDS) |>
  map(~ pluck(.x, function(x) summary(x$Demo_clinic_LBP))) |>
  bind_rows(.id = "fil_name") |>
  pivot_longer(cols = c("yes", "no"),
               names_to = "cLBP") |>
  mutate(
    #sample_name = tolower(substr(fil_name, 1, regexpr("_ML_cleaned_", fil_name) - 1))
    sample_name = tolower(substr(fil_name, 1, regexpr("_cleaned_", fil_name) - 1))
  ) |>
  ggplot(aes(x = factor(sample_name, levels = c(
    "questionnaire", "clinic", "sm_sum", "mri", 
    "quest_clinic", "quest_sm_sum", "mri_quest", "clinic_sm_sum", "mri_clinic", "mri_sm_sum",
    "quest_clinic_sm_sum", "mri_quest_clinical", "mri_quest_sm_sum", "mri_clinic_sm_sum",
    "mri_quest_clinic_sm_sum")), y = value, label = value)) +
  geom_col(aes(fill = cLBP)) +
  scale_fill_brewer(palette = "Dark2") +
  geom_text(size = 3, position = position_stack(vjust = 0.5)) +
  scale_y_continuous(name = "Sample Size", limits = c(0, 1200),
                     breaks = seq(0, 1200, 200)) +
  scale_x_discrete(name = "Data Modality", labels = mod_names, 
                   guide = guide_axis(angle = 45)) +
  theme_classic(base_size = 15) +
  theme(legend.position = c(0.9, 0.8))

# save #
ggsave(filename = path_wd("04_Outputs", "plots", "modality_sample_size.png"), 
       width = 25, height = 10, units = "cm")


# create a 15 set color pallette using Polychrome package #
set.seed(189)
color_set_15 <- createPalette(15, c("#010101","#ff0000"), 
                              M=10000)
swatch(color_set_15)
# remove names so palette can be used by ggplot #
names(color_set_15) <- NULL

clean_modalities |> 
  set_names(basename) |>
  map(readRDS) |>
  map(~ pluck(.x, function(x) ncol(x) - 2)) |>
  bind_rows() |>
  pivot_longer(cols = everything(), names_to = "fil_name", values_to = "number_vars") |>
  mutate(
    #sample_name = tolower(substr(fil_name, 1, regexpr("_ML_cleaned_", fil_name) - 1))
    sample_name = tolower(substr(fil_name, 1, regexpr("_cleaned_", fil_name) - 1)),
    sample_name = factor(sample_name, levels = c(
      "questionnaire", "clinic", "sm_sum", "mri", 
      "quest_clinic", "quest_sm_sum", "mri_quest", "clinic_sm_sum", "mri_clinic", "mri_sm_sum",
      "quest_clinic_sm_sum", "mri_quest_clinical", "mri_quest_sm_sum", "mri_clinic_sm_sum",
      "mri_quest_clinic_sm_sum"))
  ) |>
  ggplot(aes(x = sample_name, y = number_vars, fill = sample_name)) +
  geom_col() +
  scale_fill_manual(values = c(color_set_15), labels = mod_names) +
  scale_y_continuous(name = "Variable Number", limits = c(0, 150),
                     breaks = seq(0, 150, 25)) +
  scale_x_discrete(name = "Data Modality", labels = mod_names, 
                   guide = guide_axis(angle = 45)) +
  theme_classic(base_size = 15) +
  theme(legend.position = "")

# save #
ggsave(filename = path_wd("04_Outputs", "modality_feat_num.png"), 
       width = 25, height = 10, units = "cm")

## Column plots showing sample distributions across all modalities ##
## Figure 1A top and bottom from manuscript ##
## INPUT: cleaned data from 02_cleaning.R saved at ../Berlin_Back/01_Data/cleaned/
## OUTPUT: 
## column plots (Fig. 1A) as png files ##
## violin demo plots (Fig .2) ##
## demographic tables for all 15 datasets and whole sample (Table 1) ##
## OUTPUT Dir: .../Berlin_Back/04_Outputs/ is created if needed to save all subsequent outputs

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

############## Demographic data Violin plots ###################################

demo_plot <- function(demo_data, sample_name) {
  if(!is.data.frame(demo_data)) {
    print("data needs to be a data frame")
  }
  
  sum_df <- demo_data |> 
    select(Demo_age, Demo_sex, Demo_clinic_LBP) |>
    summarise(
      number = n(),
      .by = c(Demo_sex, Demo_clinic_LBP)
    )
  
  CBP_sum_df <- sum_df |>
    summarise(
      total = paste("total =", sum(number)),
      .by = Demo_clinic_LBP)
  
  demo_data |> select(Demo_age, Demo_sex, Demo_clinic_LBP) |>
    ggplot(aes(x = Demo_clinic_LBP, y = Demo_age, fill = Demo_sex)) +
    geom_violin(trim = TRUE, color = NA) +
    geom_boxplot(width = 0.3, position = position_dodge(width = 0.9)) +
    scale_y_continuous(limits = c(10, 80)) +
    scale_fill_manual(values = c("#e78ac3", "#a6d854")) +
    scale_color_manual(values = c("#e78ac3", "#a6d854")) +
    #scale_fill_brewer(palette = "Dark2") +
    #scale_color_brewer(palette = "Dark2") +
    labs(x = "Chronic Low Back Pain", y = "Age (years)",
         title = paste0(sample_name, "\n(N = ", sum(sum_df$number), ")")) +
    geom_text(data = sum_df,
              aes(y = 75, label = number, color = Demo_sex), 
              show.legend = FALSE, 
              position = position_dodge(0.9), size = 8) +
    geom_text(data = CBP_sum_df,
              aes(y = 80, label = total, x = Demo_clinic_LBP),
              size = 10, position = position_dodge(0.8), inherit.aes = FALSE, col = "darkblue") +
    theme_classic(base_size = 17) +
    theme(legend.title= element_blank(), legend.position = c(0.5, 0.1), 
          legend.text = element_text(size=20)) +
    guides(fill = guide_legend(ncol = 2))
  
}

# loop through the cleaned data and create violin plots #
for (i in 1:length(clean_files)) {
  
  fil_name <- substr(clean_files[i], 1, nchar(clean_files[i])-4)
  
  demo_data <- readRDS(path_wd("01_Data", "Cleaned",
                               fil_name,
                               ext = "rds"))
  
  # Sample name for Title of plot #
  sample_name <- mod_names[i]
  
  # create plot #
  demo_plot(demo_data, sample_name)
  
  # save plot #
  # png #
  ggsave(filename = paste0(fil_name, "_violin_plot.png"), 
         path = path_wd("04_outputs"), device = "png", dpi = 2000)
  # SVG #
  ggsave(filename = paste0(fil_name, "_violin_plot.svg"), 
         path = path_wd("04_outputs"), device = "svg", dpi = 2000)
  
}

########## demographic table from whole sample #################################
## This will include Age, Sex, BMI, Pain intensity, Pain Duration ##

# load all data to gather pain intensity & duration data for table #
all_preproc_df <- readRDS(path_wd("01_Data", "preproc", 
                                  "preprocessed_ALL_2024-04-17", ext = "rds"))

# Select study id, pain intensity, pain duration to join with demographic 
# cleaned data #

pain_dat_all <- all_preproc_df |>
  select(Study_ID, Clinic_current_pain, Demo_LBP_pain_duration, 
         Demo_LBP_past_pain_duration)

demo_df <- readRDS(path_wd("01_Data", "cleaned", 
                           "Demo_cleaned_Clinic_BP_dim_1161X5", ext = "rds"))

pain_demo_df <- inner_join(demo_df, pain_dat_all, by = "Study_ID") |>
  mutate(
    # one subject has clinic LBP but no current pain duration and need to take from past duration #
    Demo_LBP_pain_duration = case_when(
      is.na(Demo_LBP_pain_duration) ~  Demo_LBP_past_pain_duration,
      .default = Demo_LBP_pain_duration
    ),
    LBP_duration = case_when(
      Demo_clinic_LBP == "no" ~ 0,
      .default = as.double(Demo_LBP_pain_duration)
    ),
    LBP_intensity = case_when(
      Demo_clinic_LBP == "no" ~ 0,
      .default = as.double(Clinic_current_pain)
    )
  ) |>
  # remove old suration and intensity cols
  select(-c(Demo_LBP_pain_duration,
            Demo_LBP_past_pain_duration,
            Clinic_current_pain))

# save for creating modalities supp tables #
write.csv(pain_demo_df, file = path_wd("01_Data", "cleaned",
                                       "Demographic_Pain_data_cleaned_1161x7",
                                       ext = "csv"),
          row.names = FALSE)

# now create same demo tables for all datasets #
for (j in 1:length(clean_files)) {
  
  fil_name <- substr(clean_files[j], 1, nchar(clean_files[j])-4)
  
  demo_pain <- readRDS(path_wd("01_Data", "Cleaned",
                               fil_name,
                               ext = "rds")) |>
    select(Study_ID,
           starts_with("Demo_")) |>
    inner_join(pain_demo_df, by = c("Study_ID", "Demo_age", "Demo_clinic_LBP",
                                    "Demo_sex", "Demo_BMI"))
  
  # Sample name for Title of plot #
  sample_name <- plot_title[j]
  
  pain_demo_summary <- demo_pain |>
    select(-Study_ID) |>
    summarise(
      mean_age = mean(Demo_age),
      sd_age = sd(Demo_age),
      n = n(),
      mean_BMI = mean(Demo_BMI),
      sd_BMI = sd(Demo_BMI),
      mean_duration = mean(LBP_duration, na.rm = TRUE),
      sd_duration = sd(LBP_duration, na.rm = TRUE),
      mean_intensity = mean(LBP_intensity, na.rm = TRUE),
      sd_intensity = sd(LBP_intensity, na.rm = TRUE),
      .by = Demo_clinic_LBP
    )
  
  # hacky way to get the n females stratified by LBP #
  demo_sex <- demo_pain |>
    select(-Study_ID) |>
    summarise(
      n = n(),
      .by = c(Demo_clinic_LBP, Demo_sex)
    ) |>
    filter(Demo_sex == "female") |>
    rename(n_female = n) |>
    select(-Demo_sex)
  
  # join n_females to summary table #
  
  pain_demo_summary_full <- full_join(pain_demo_summary, demo_sex, 
                                      by = "Demo_clinic_LBP") |>
    mutate(
      across(where(is.double), function(x) round(x, digits = 2))
    )
  
  # flip to be more like table for manuscript #
  pain_demo_tbl <- pain_demo_summary_full |>
    pivot_longer(cols = -Demo_clinic_LBP) |>
    pivot_wider(id_cols = name, names_from = Demo_clinic_LBP, 
                values_from = value) |>
    rename(
      asymptomatic = no,
      cLBP = yes
    )
  
  write.csv(pain_demo_tbl, 
            file = path_wd("04_outputs", 
                           paste0(fil_name, "_demo_tbl"),
                           ext = "csv"),
            row.names = FALSE)
  
  
}

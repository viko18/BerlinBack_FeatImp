## data cleaning for all 15 modalities following 01_preprocessing.R ##
## INPUT: Preprocessed_ALL*.rds from ../Berlin_Back/01_Data/preproc/ ##
## OUTPUT: Cleaned 15 dataset modalities & ML model dataset modalities with removed 
##        holdout sample ##
## Output files save to ../01_Data/cleaned/

# Set working directory #
wd <- ".../Berlin_Back/"
setwd(wd)

# load dependencies #
library("dplyr")
library("fs")
library("tibble") 
library("tidyr")
library("caret")
library("corrplot")
library("RColorBrewer")
library("janitor")


######################## Input Preprocessed data ###############################

preproc_data <- readRDS(file = path_wd("01_Data", "preproc", 
                                       "preprocessed_ALL_2024-04-17", 
                                       ext = "rds")) %>%
  distinct(Study_ID, .keep_all = TRUE)

######################## Create output directory ###############################

output_dir <- path_wd("01_Data", "Cleaned")
if(!dir_exists(output_dir)) {dir_create(output_dir)}

############# Function to determine mising data ################################

# create a df with col names, col type and number of missing values #
df_types <- function(df) {
  require("purrr")
  tibble(
    col_name = names(df), 
    col_type = map_chr(df, vctrs::vec_ptype_full),
    n_miss = map_int(df, \(x) sum(is.na(x)))
  )
}

####################### Missing df of whole dataset ############################

# clean preproc data to remove some cols captured by summary cols #
preproc_missing <- preproc_data %>%
  ## Demo data ##
  select(-c(
    Demo_combi_LBP, # created col showing difference between LBP quest and clinic
    Study_ID,
    Study_included,
    Study_mri_included)
    ) %>%
  ## Questionnaires ##
  select(-c(
    starts_with("Quest_sf36_q"), # individual questions #
    starts_with("Quest_ipaq_q"), # individual questions #
    starts_with("Quest_srbai_q"), # individual questions Behavioural Automaticity Questionnaire (SRBAI) #
    starts_with("Quest_brsq_q"), # individual questions  Behavioral Regulation in Sport Questionnaire (BRSQ) #
    starts_with("Quest_tskgv_"), # individual questions  Kinesiophobia#
    starts_with("Quest_cor_"), # individual qs Pain and disability by Korff #
    starts_with("Quest_rms_"), # individual qs Roland Morris Disability #
    starts_with("Quest_fabq_"), # individual qs #
    num_range("Quest_stress_", 1:8), # individual qs #
  )) %>% 
  ## clincal assessment ##
  select(-c(
    Clinic_3_SM_measure # did 3 spine mouse measurements occur -> here not relavant
  )) %>%
  ## Spine Mouse - we will keep everything ##
  df_types()

# save df as csv #

write.csv(preproc_missing, file = path(output_dir,
                                       "All_preproc_data_missing_value", 
                                       ext = "csv"), 
          row.names = FALSE)


################################################################################
###################### Demographic DATA ########################################

# original demographic data before cleaning #
demo_orig <- preproc_data %>%
  select(Study_ID, starts_with("Demo_"))

# remove not relevant cols and subjects with "in past" LBP

# create vectors of col names to be removed for later remove table creation text #

# Only used the clinically defined LBP for classification target not other definitions #
only_clinic_LBP <- c("Demo_question_LBP", "Demo_combi_LBP")
# Used BMI only in modelling so removed height and weight and similar measures #
BMI_used <- c("Demo_height", "Demo_weight", "Demo_waist", "Demo_hip", "Demo_WHR")
# these are not relevant to classifying LBP and were taken to be used in movement analyses
not_relevant <- c("Demo_leg_l", "Demo_leg_r", "Demo_r_l_ratio")
# these are only questions asked of LBP patients #
LBP_question_D <- c("Demo_LBP_past_pain_duration", "Demo_LBP_pain_duration") 

demo_data <- demo_orig %>%
  select(
    # remove cols not needed in back pain classification
    -c(
      all_of(only_clinic_LBP),
      all_of(BMI_used), 
      all_of(not_relevant), 
      all_of(LBP_question_D)
    )
  ) %>%
  # rm subjects with previous back pain to make control-patient groups more distinguishable 
  filter(Demo_clinic_LBP != "in_past") %>%
  droplevels() # remove "in_past level from back pain col
  
# df showing number of missing values #
demo_missing <- df_types(demo_data)

write.csv(demo_missing, file = path(output_dir,
                                    paste0("Demo_data_missing_",
                                           nrow(demo_data),
                                           "X",
                                           ncol(demo_data)), 
                                    ext = "csv"), 
          row.names = FALSE)

# after check for number of NA's drop them for further cleaning #
demo_data <- demo_data %>% drop_na()

# print summaries #
glimpse(demo_data)
summary(demo_data)

# check for near zero variance of features #
demo_nzv <- nearZeroVar(demo_data, names = TRUE, saveMetrics = TRUE)
demo_nzv # no near zeo variance features #

demo_cor_sp <- demo_data |>
  select(-Study_ID) |>
  mutate(across(where(is.factor), as.numeric)) |>
  cor(method = "spearman")

demo_sp_path= path_wd("04_outputs", "Demographic_spearman_corr_mat",
                     ext = "png")

# create png for writing plot
png(height=1800, width=1800, file=demo_sp_path, type = "cairo")

corrplot(demo_cor_sp, 
         method = "circle",
         tl.cex = 1, # text size
         cl.cex = 2, # color bar text size
         addCoef.col = 1, # add corr values
         number.cex = 1, # size of numbers
         tl.col = "black", # color of text
         title = "Demographic Spearman",
         col = brewer.pal(n=10, name="PiYG")) 
dev.off()

findCorrelation(demo_cor_sp, cutoff = 0.9, names = TRUE)
# NO highly correlated features #

# save cleaned data as .rds and .csv #

demo_data_fn <- paste0("Demo_cleaned_Clinic_BP_dim_",
                       nrow(demo_data),
                       "X",
                       ncol(demo_data))

saveRDS(demo_data, file = path(output_dir, 
                               demo_data_fn,
                               ext = "rds"))

write.csv(demo_data, file = path(output_dir,
                                 demo_data_fn,
                                 ext = "csv"),
          row.names = FALSE)

# create df containing the columns removed during cleaning #

demo_rm <- compare_df_cols(demo_data, demo_orig, return = "match") %>%
  filter(is.na(demo_data)) %>%
  mutate(
    Reason = case_when(
      column_name %in% BMI_used ~ "BMI was used instead of this related variable",
      column_name %in% only_clinic_LBP ~ "Clinical diagnosis was used as target",
      column_name %in% not_relevant ~ "Not relevant to LBP classification as used for movement analyses",
      column_name %in% LBP_question_D ~ "Question only asked to LBP patients",
      .default = "missing"
    )
  ) %>%
  select(-demo_data) %>%
  rename(col_type = demo_orig)

# save #
write.csv(demo_rm, file = path(output_dir, "Demo_removed_cols_preproc",
                               ext = "csv"), row.names = FALSE)

################################################################################
############################## MRI DATA ########################################

mri_orig <- preproc_data %>%
  select(Study_ID, starts_with("MRI_"))

# create vectors of col names to be removed for later remove table creation text #

# MRI few subjects that have L6 values < 10 values # 
mri_L6S1 <- c("MRI_ivd_deg_l6_s1", "MRI_disc_hern_l6_s1", "MRI_facet_l6_s1_l",
              "MRI_facet_l6_s1_r", "MRI_osteo_l6_s1", "MRI_spinal_canal_l6",
              "MRI_schizas_l6", "MRI_lordosis_l6_s1")

# rm oberserver coding as it's not important for our model #
mri_obs <- mri_orig %>% select(ends_with("oberservers")) %>% colnames()

# rm schizas cols as there are < 10 values #
mri_schiz <- c("MRI_schizas_l1", "MRI_schizas_l2", "MRI_schizas_l3", "MRI_schizas_l4",
               "MRI_schizas_l5")

# rm lordosis rating as they have > 100 mire missing values compared to other ratings #
mri_lord <- c("MRI_lordosis_l1_s1", "MRI_lordosis_l1_l2", "MRI_lordosis_l2_l3",
              "MRI_lordosis_l3_l4", "MRI_lordosis_l4_l5", "MRI_lordosis_l5_s1")

# create cleaned data frame by removing unwanted columns and subj with NAs #
mri_data <- mri_orig %>%
  select(
    # these cols removed #
    -c(
      all_of(mri_L6S1),
      all_of(mri_obs),
      all_of(mri_schiz), 
      all_of(mri_lord)
      )
  ) %>%
  droplevels() 

# df showing number of missing values #
mri_missing <- df_types(mri_data)

write.csv(mri_missing, file = path(output_dir,
                                    paste0("MRI_data_missing_",
                                           nrow(mri_data),
                                           "X",
                                           ncol(mri_data)), 
                                    ext = "csv"), 
          row.names = FALSE)

# after check for number of NA's drop them for further cleaning #
mri_data <- mri_data %>% drop_na()


# check for near zero variance of features #
mri_nzv <- nearZeroVar(mri_data, names = TRUE, saveMetrics = TRUE)
mri_nzv 
# nzv = TRUE
# MRI_osteo_l1_l2 var = 0.05 -> remove
# MRI_osteo_l2_l3 var = 0.17 -> remove
# MRI_osteo_l3_l4 var = 0.16 -> remove
# MRI_disc_hern_l1_l2 var = 0.08 -> remove

mri_nzv_cols <- c("MRI_osteo_l1_l2", "MRI_osteo_l2_l3", "MRI_osteo_l3_l4", 
                  "MRI_disc_hern_l1_l2")

# remove low variance columns #
mri_data <- select(mri_data,
                   -c(all_of(mri_nzv_cols))
)

mri_cor_sp <- mri_data |>
  select(-Study_ID) |>
  mutate(across(where(is.factor), as.numeric)) |>
  cor(method = "spearman")

mri_sp_path= path_wd("04_outputs", "mri_spearman_corr_mat",
                     ext = "png")

# create png for writing plot
png(height=1800, width=1800, file=mri_sp_path, type = "cairo")

corrplot(mri_cor_sp, 
         method = "circle",
         tl.cex = 1, # text size
         cl.cex = 2, # color bar text size
         addCoef.col = 1, # add corr values
         number.cex = 1, # size of numbers
         tl.col = "black", # color of text
         title = "MRI Spearman",
         col = brewer.pal(n=10, name="PiYG")) 
dev.off()

findCorrelation(mri_cor_sp, cutoff = 0.9, names = TRUE)
# NO highly correlated features #

# join demographic data with mri data for saving #

mri_data_clean <- full_join(x = demo_data, y = mri_data, by = "Study_ID") %>%
  drop_na()

# print summaries #
glimpse(mri_data_clean)

# save cleaned data as .rds and .csv #

mri_data_clean_fn <- paste0("MRI_cleaned_Clinic_BP_dim_",
                            nrow(mri_data_clean),
                            "X",
                            ncol(mri_data_clean))

saveRDS(mri_data_clean, file = path(output_dir, 
                                    mri_data_clean_fn,
                                    ext = "rds"))

write.csv(mri_data_clean, file = path(output_dir,
                                      mri_data_clean_fn,
                                      ext = "csv"),
          row.names = FALSE)

# create df containing the columns removed during cleaning #

mri_rm <- compare_df_cols(mri_data, mri_orig, return = "match") %>%
  filter(is.na(mri_data)) %>%
  mutate(
    Reason = case_when(
      column_name %in% mri_L6S1 ~ "Very few (< 10) subjects had values for L6-S1",
      column_name %in% mri_nzv_cols ~ "Near zero variance (< 0.2)",
      column_name %in% mri_obs ~ "radiologist rater info not relevant",
      column_name %in% mri_schiz ~ "Schizas rating present in < 10 subjects",
      column_name %in% mri_lord ~ "Lordosis rating has > 100 missing values compared to other ratings",
      .default = "Missing Error"
    )
  ) %>%
  select(-mri_data) %>%
  rename(col_type = mri_orig)

# save #
write.csv(mri_rm, file = path(output_dir, "MRI_removed_cols_preproc",
                               ext = "csv"), row.names = FALSE)

################################################################################
############################# Questionnaire Data ###############################

quest_orig <- preproc_data %>%
  select(Study_ID,
         starts_with("Clinic_question_"),
         starts_with("Clinic_prior_"),
         starts_with("Clinic_medication_"),
         starts_with("Quest_"),
         starts_with("Clinic_pain_"),
         # remove these cols 
         -c(
           starts_with("Quest_sf36_q"), # sf36 individual Q's
           starts_with("Quest_ipaq_q"), # IPAQ individual Q'S
           starts_with("Quest_srbai_q"), # SRBAI individual Q's
           starts_with("Quest_brsq_q"), # BRSQ individual Q's
           starts_with("Quest_cor"), # Korff individual Q's
           starts_with("Quest_rms_"), # Rolland morris individual Q's
           num_range("Quest_fabq_", 1:16), # fabq individual Q'S
           starts_with("Quest_tskgv_"), # kinesiophobia individual Q's
           num_range("Quest_stress_", 1:8), # stress individual Q's
         ))

# create vectors of colnames removed during preproc for easily creating reasons table

# medication question heavily biased to LBP patients #
med_cols <- quest_orig %>% 
  select(starts_with("Clinic_medication_")) %>% 
  colnames()

# prior LBP diagnoses havily biased to LBP patients #
prior_diagnosis <- quest_orig %>% 
  select(
    starts_with("Clinic_prior_"),
    -c(Clinic_prior_surgeries, Clinic_prior_surgeries_which)) %>%
  colnames()

# pain tests and questions highly biased towards LBP pateints #
clinic_pain <- quest_orig %>%
  select(starts_with("Clinic_pain"),
         -c(Clinic_pain_hip)) %>%
  colnames()

# questionnaires only asked by LBP patients #
lbp_questions_Q <- quest_orig %>%
  select(starts_with("Quest_employment"),
         starts_with("Quest_chron"),
         starts_with("Quest_stress"),
         starts_with("Quest_fabq"),
         Quest_kinesiophobia,
         starts_with("Quest_korff"),
         Quest_RM_disability) %>%
  colnames()

# questions and questionnaires that are too biased to LBP patient status #
lbp_bias_questions <- quest_orig %>%
  select(starts_with("Quest_therapy"),
         Clinic_question_body_self_assessment,
         Quest_sf36_physical_role_function,
         Quest_sf36_physical_pain,
         Quest_sf36_health_perception,
         Quest_sf36_vitality, 
         Quest_sf36_physical_function,
         ) %>%
  colnames()

# some cols with large amount of missing values #
missing_values_Qs <- c("Clinic_question_job_posture_duration", 
                       "Clinic_question_job_load_2_duration",
                       "Clinic_question_job_load_2")

# job duration is highly correlated with Age which is kept in the model #
high_cor_age <- c("Clinic_question_job_duration_years")

# only used IPAQ sum score
ipaq_non_sum <- c("Quest_ipaq_met_moderate", "Quest_ipaq_met_sitting", 
                  "Quest_ipaq_met_vigorous", "Quest_ipaq_met_walking")


quest_data <- quest_orig %>%
  select(
    Study_ID,
    starts_with("Clinic_question_"),
    Clinic_prior_surgeries,
    Clinic_prior_surgeries_which,
    Clinic_pain_hip, # may relate to back pain but not directly a measure
    Quest_brsq_intrinsic_motivation,
    Quest_brsq_integrated_regulation,
    Quest_brsq_external_regulation,
    Quest_srbai_sum,
    Quest_ipaq_met_sum,
    Quest_sf36_social_function,
    Quest_sf36_emotional_role_function,
    Quest_sf36_psychological_well_being,
    # these cols remove #
     -c(
       Clinic_question_body_self_assessment, # relates too heavily with self perception of being a pain patient
       Clinic_question_job_posture_duration, # ~ 290 NAs
       Clinic_question_job_duration_years, # high correlation with age and has more missing values so removed
       Clinic_question_job_load_2_duration, # > 850 Na's
       Clinic_question_job_load_2, # > 850 Na's
     )
  ) %>%
  mutate(
    Clinic_prior_surgeries_which = as.factor(case_when(
      Clinic_prior_surgeries == "yes" & is.na(Clinic_prior_surgeries_which) ~ "non_MSK_OP", # only MSK type OP's are named
      Clinic_prior_surgeries == "no" & is.na(Clinic_prior_surgeries_which) ~ "none",
      .default = Clinic_prior_surgeries_which
    )),
    Clinic_prior_surgeries = as.factor(case_when(
      Clinic_prior_surgeries_which != "none" & !is.na(Clinic_prior_surgeries_which) ~ "yes", # these are tipping errors at OP -> yes no
      .default = Clinic_prior_surgeries
    )),
    Clinic_question_smoking_pack_years = case_when( 
      Clinic_question_smoking == "no" ~ 0,
      .default = Clinic_question_smoking_pack_years
    ) 
  ) %>%
  droplevels() 

# df showing number of missing values #
quest_missing <- df_types(quest_data)

write.csv(quest_missing, file = path(output_dir,
                                   paste0("Quest_data_missing_",
                                          nrow(quest_data),
                                          "X",
                                          ncol(quest_data)), 
                                   ext = "csv"), 
          row.names = FALSE)

# after check for number of NA's drop them for further cleaning #
quest_data <- quest_data %>% drop_na()

# look at summaries #
glimpse(quest_data)
summary(quest_data)

# check for near zero variance of features #
quest_nzv <- nearZeroVar(quest_data, names = TRUE, saveMetrics = TRUE)
quest_nzv 
# nzv = TRUE
# Clinic_question_alcohol_restrict var = 0.48 -> remove 
# Clinic_question_smoking_pack_years var = 34.4 -> keep 

# remove low variance cols #
low_var_Qs <- c("Clinic_question_alcohol_restrict")

quest_data <- select(quest_data,
                     -c(all_of(low_var_Qs))
)

# questionnaire spearman corrlation matrix #

quest_cor_sp <- full_join(x = demo_data, y = quest_data, by = "Study_ID") |>
  select(-Study_ID) |>
  drop_na() |>
  mutate(across(where(is.factor), as.numeric)) |>
  cor(method = "spearman")

quest_sp_path= path_wd("04_outputs", "quest_spearman_corr_mat",
                       ext = "png")

# create png for writing plot
png(height=1800, width=1800, file=quest_sp_path, type = "cairo")

corrplot(quest_cor_sp, 
         method = "circle",
         tl.cex = 1, # text size
         cl.cex = 2, # color bar text size
         addCoef.col = 1, # add corr values
         number.cex = 1, # size of numbers
         tl.col = "black", # color of text
         title = "Questionnaire Spearman",
         col = brewer.pal(n=10, name="PiYG")) 
dev.off()

findCorrelation(quest_cor_sp, cutoff = 0.9, names = TRUE)
# Clinic_question_smoking_pack_years & Clinic_question_smoking = 0.99
# keep Clinic_question_smoking_pack_years as it has a higher correlation to target (LBP)
# Clinic_prior_surgeries_which and Clinic_prior_surgeries = -0.98
# Keep Clinic_prior_surgeries as it has slightly higher correlation to target

# remove highly correlated features #
high_cor_Qs <- c("Clinic_question_smoking", "Clinic_prior_surgeries_which")

quest_data <- select(quest_data,
                     -c(all_of(high_cor_Qs))
)

# clean quest data with addition of removing extra cols -> may increase N
quest_data <- preproc_data %>%
  select(
    Study_ID, # need subject ID for joining with both modalities and demo data
    #starts_with("Quest_"),
    starts_with("Clinic_question_"),
    Clinic_prior_surgeries,
    Clinic_prior_surgeries_which,
    Clinic_pain_hip, # may relate to back pain but not directly a measure
    Quest_brsq_intrinsic_motivation,
    Quest_brsq_integrated_regulation,
    Quest_brsq_external_regulation,
    Quest_srbai_sum,
    Quest_ipaq_met_sum,
    Quest_sf36_social_function,
    Quest_sf36_emotional_role_function,
    Quest_sf36_psychological_well_being,
    # these cols remove #
    -c(
      Clinic_question_body_self_assessment, # relates too heavily with self perception of being a pain patient
      Clinic_question_job_posture_duration, # ~ 290 NAs
      Clinic_question_job_duration_years, # high correlation with age and has more missing values so removed
      Clinic_question_job_load_2_duration, # > 850 Na's
      Clinic_question_job_load_2, # > 850 Na's
    )
  ) %>%
  mutate(
    Clinic_prior_surgeries_which = as.factor(case_when(
      Clinic_prior_surgeries == "yes" & is.na(Clinic_prior_surgeries_which) ~ "non_MSK_OP", # only MSK type OP's are named
      Clinic_prior_surgeries == "no" & is.na(Clinic_prior_surgeries_which) ~ "none",
      .default = Clinic_prior_surgeries_which
    )),
    Clinic_prior_surgeries = as.factor(case_when(
      Clinic_prior_surgeries_which != "none" & !is.na(Clinic_prior_surgeries_which) ~ "yes", # these are tipping errors at OP -> yes no
      .default = Clinic_prior_surgeries
    )),
    Clinic_question_smoking_pack_years = case_when( 
      Clinic_question_smoking == "no" ~ 0,
      .default = Clinic_question_smoking_pack_years
    ) 
  ) %>%
  select(
    -c(Clinic_question_smoking,
       Clinic_prior_surgeries_which,
       Clinic_question_alcohol_restrict)
    ) %>%
  droplevels() %>%
  drop_na()

quest_cor_sp <- quest_data |>
  select(-Study_ID) |>
  mutate(across(where(is.factor), as.numeric)) |>
  cor(method = "spearman")

quest_sp_path= path_wd("04_outputs", "quest_spearman_corr_mat_cleaned",
                       ext = "png")

# create png for writing plot
png(height=1800, width=1800, file=quest_sp_path, type = "cairo")

corrplot(quest_cor_sp, 
         method = "circle",
         tl.cex = 1, # text size
         cl.cex = 2, # color bar text size
         addCoef.col = 1, # add corr values
         number.cex = 1, # size of numbers
         tl.col = "black", # color of text
         title = "Questionnaire Spearman",
         col = brewer.pal(n=10, name="PiYG")) 
dev.off()

# join demographic data with questionnaire data for saving #
quest_data_clean <- full_join(x = demo_data, y = quest_data, by = "Study_ID") %>%
  drop_na()

# print summaries #
glimpse(quest_data_clean)

# save cleaned data as .rds and .csv #

quest_data_clean_fn <- paste0("Questionnaire_cleaned_Clinic_BP_dim_",
                              nrow(quest_data_clean),
                              "X",
                              ncol(quest_data_clean))

saveRDS(quest_data_clean, file = path(output_dir, 
                                      quest_data_clean_fn,
                                      ext = "rds"))

write.csv(quest_data_clean, file = path(output_dir,
                                        quest_data_clean_fn,
                                        ext = "csv"),
          row.names = FALSE)

# questionaire removed cols during preprocessing and cleaning reasons table #

quest_rm <- compare_df_cols(quest_data, quest_orig, return = "match") %>%
  filter(is.na(quest_data)) %>%
  mutate(
    Reason = case_when(
      column_name %in% med_cols ~ "Medication clinical questions heavily biased towards LBP patients",
      column_name %in% prior_diagnosis ~ "Prior diagnoses and surgery clincal questions bias towards LBP pateints",
      column_name %in% clinic_pain ~ "Pain questions and tests on back heavily biased towards LBP pateints",
      column_name %in% lbp_questions_Q ~ "Questions only asked of LBP patients",
      column_name %in% ipaq_non_sum ~ "Only used IPAQ sum score and not individual sub-scores",
      column_name %in% lbp_bias_questions ~ "Questions biased towards LBP patients",
      column_name %in% missing_values_Qs ~ "Questions with large amount of missing values",
      column_name %in% high_cor_age ~ "Job duration has high correlation with age",
      column_name %in% low_var_Qs ~ "Very low variance (0.48)",
      column_name %in% high_cor_Qs ~ "Very high correlation (> 0.9)",
      .default = "Missing Error"
    )
  ) %>%
  select(-quest_data) %>%
  rename(col_type = quest_orig)

# save #
write.csv(quest_rm, file = path(output_dir, "Quest_removed_cols_preproc",
                              ext = "csv"), row.names = FALSE)

################################################################################
############################ Clinical Assessment ###############################

clinic_orig <- preproc_data %>%
  select(
    Study_ID, # need subject ID for joining with both modalities and demo data
    starts_with("Clinic_body"),
    starts_with("Clinic_motion"),
    starts_with("Clinic_heart_"),
    starts_with("Clinic_breath_"),
    Clinic_3_SM_measure
  )

clinic_data <- clinic_orig %>%
  select(
    Study_ID, # need subject ID for joining with both modalities and demo data
    starts_with("Clinic_body"),
    starts_with("Clinic_motion"),
    # remove cols #
    -c(
      Clinic_body_hip_flexion, # has over 250 NA's could be added later in study
      Clinic_motion_pseudo_lasegue_r, # ~280 could have been added later in study
      Clinic_motion_pseudo_lasegue_l, # ~280 could have been added later in study
    )
  ) %>%
  droplevels() 

# removed features col names #

# large amount of missing values
high_missing_C <- c("Clinic_body_hip_flexion", 
                    "Clinic_motion_pseudo_lasegue_r",
                    "Clinic_motion_pseudo_lasegue_l")

# heart and breath rate not relevant for LBP classification #
heart_breath <- clinic_orig %>%
  select(starts_with("Clinic_heart"), starts_with("Clinic_breath_")) %>%
  colnames()

# conducting spine mouse 3 time doesn't relate to LBP
SM_3 <- "Clinic_3_SM_measure"

# df showing number of missing values #
clinic_missing <- df_types(clinic_data)

write.csv(clinic_missing, file = path(output_dir,
                                     paste0("Clinic_data_missing_",
                                            nrow(clinic_data),
                                            "X",
                                            ncol(clinic_data)), 
                                     ext = "csv"), 
          row.names = FALSE)

# after check for number of NA's drop them for further cleaning #
clinic_data <- clinic_data %>% drop_na()


# view summaries #
glimpse(clinic_data)
summary(clinic_data)

# near zero variance check #
clinic_nzv <- nearZeroVar(clinic_data, names = TRUE, saveMetrics = TRUE)
clinic_nzv |> filter(nzv == TRUE)
# nzv = TRUE
# Clinic_body_back_head_wall_distance var = 0.06 -> remove
# Clinic_body_toe_stand var = 0.02 -> remove
# Clinic_body_heel_stand var = 0.02 -> remove
# Clinic_motion_coronal_balance var = 0.02 -> remove
# Clinic_motion_trendelenburg_sign var = 0.007 -> remove
# Clinic_motion_straight_leg_raise var = 0.03 -> remove
# Clinic_motion_gait_pattern var = 0.007 -> remove
# Clinic_motion_reflex_quad_l var = 0.08 -> remove
# Clinic_motion_reflex_quad_r var = 0.09 -> remove
# Clinic_motion_reflex_achilles_l var = 0.08 -> remove
# Clinic_motion_reflex_achilles_r var = 0.06 -> remove


# very low variance cols to be removed #
low_var_clinic <- c("Clinic_body_back_head_wall_distance",
                    "Clinic_motion_reflex_quad_l",
                    "Clinic_motion_reflex_quad_r",
                    "Clinic_motion_reflex_achilles_l",
                    "Clinic_motion_reflex_achilles_r",
                    "Clinic_motion_gait_pattern",
                    "Clinic_motion_trendelenburg_sign",
                    "Clinic_motion_straight_leg_raise",
                    "Clinic_motion_coronal_balance",
                    "Clinic_body_heel_stand",
                    "Clinic_body_toe_stand")

# remove low variance cols #
clinic_data <- select(clinic_data,
                      -c(all_of(low_var_clinic))
)

# clinic spearman correlation #

clinic_cor_sp <- clinic_data |>
  select(-Study_ID) |>
  mutate(across(where(is.factor), as.numeric)) |>
  cor(method = "spearman")

clinic_sp_path= path_wd("04_outputs", "clinic_spearman_corr_mat",
                        ext = "png")

# create png for writing plot
png(height=1800, width=1800, file=clinic_sp_path, type = "cairo")

corrplot(clinic_cor_sp, 
         method = "circle",
         tl.cex = 1, # text size
         cl.cex = 2, # color bar text size
         addCoef.col = 1, # add corr values
         number.cex = 1, # size of numbers
         tl.col = "black", # color of text
         col = brewer.pal(n=10, name="PiYG")) 
dev.off()

findCorrelation(clinic_cor_sp, cutoff = 0.9, names = TRUE)
# no highly correlated features #

# conduct cleaning with additionally removed cols #
clinic_data <- preproc_data %>%
  select(
    Study_ID, # need subject ID for joining with both modalities and demo data
    starts_with("Clinic_body"),
    starts_with("Clinic_motion"),
    # remove cols #
    -c(
      Clinic_body_hip_flexion, # has over 250 NA's could be added later in study
      Clinic_motion_pseudo_lasegue_r, # ~280 could have been added later in study
      Clinic_motion_pseudo_lasegue_l, # ~280 could have been added later in study
      Clinic_body_back_head_wall_distance, # near zero variance
      Clinic_motion_reflex_achilles_l, # near zero variance
      Clinic_motion_reflex_achilles_r, # near zero variance
      Clinic_motion_reflex_quad_l, # near zero variance
      Clinic_motion_reflex_quad_r, # near zero variance
      Clinic_motion_gait_pattern, # near zero variance
      Clinic_motion_trendelenburg_sign, # near zero variance
      Clinic_motion_straight_leg_raise, # near zero variance
      Clinic_motion_coronal_balance, # near zero variance
      Clinic_body_heel_stand, # near zero variance
      Clinic_body_toe_stand # near zero variance
    )
  ) %>%
  droplevels() %>%
  drop_na()

# join clinic & demo data #
clinic_data_clean <- full_join(demo_data, clinic_data, by="Study_ID") %>%
  drop_na()

# view summary #
glimpse(clinic_data_clean)

# save cleaned data as .rds and .csv #

clinic_data_clean_fn <- paste0("Clinic_cleaned_Clinic_BP_dim_",
                               nrow(clinic_data_clean),
                               "X",
                               ncol(clinic_data_clean))

saveRDS(clinic_data_clean, file = path(output_dir, 
                                       clinic_data_clean_fn,
                                       ext = "rds"))

write.csv(clinic_data_clean, file = path(output_dir,
                                         clinic_data_clean_fn,
                                         ext = "csv"),
          row.names = FALSE)

# clinic physical removed cols during preprocessing and cleaning reasons table #

clinic_rm <- compare_df_cols(clinic_data, clinic_orig, return = "match") %>%
  filter(is.na(clinic_data)) %>%
  mutate(
    Reason = case_when(
      column_name %in% high_missing_C ~ "Large amount of missing values (> 250)",
      column_name %in% heart_breath ~ "Heart rate, breath rate, and blood pressure not related to LBP",
      column_name %in% low_var_clinic ~ "Near zero variance (< 0.1)",
      column_name %in% SM_3 ~ "Conducting spine mouse 3x not related to LBP",
      .default = "Missing Error"
    )
  ) %>%
  select(-clinic_data) %>%
  rename(col_type = clinic_orig)

# save #
write.csv(clinic_rm, file = path(output_dir, "Clinic_removed_cols_preproc",
                                ext = "csv"), row.names = FALSE)

################################################################################
############################ Spine Mouse #######################################


# Use just summary data from lumbar and thoracic section and length, surface area #
sm_sum_orig <- preproc_data %>%
  select(
    Study_ID,
    contains("thoracic_value"),
    contains("lumbar_value"),
    contains("inclination_value"),
    contains("spine_length"),
    contains("surface_length"),
    contains("_sacral_") # this will be included in both df as there are no summaries for sacral spine #
  )

sm_summary_data <- sm_sum_orig %>%
  droplevels() 

# df showing number of missing values #
sm_summary_missing <- df_types(sm_summary_data)

write.csv(sm_summary_missing, file = path(output_dir,
                                      paste0("SM_Sum_data_missing_",
                                             nrow(sm_summary_data),
                                             "X",
                                             ncol(sm_summary_data)), 
                                      ext = "csv"), 
          row.names = FALSE)

# after check for number of NA's drop them for further cleaning #
sm_summary_data <- sm_summary_data %>% drop_na()


# summaries #
glimpse(sm_summary_data)
summary(sm_summary_data)

# near zero variance check #
sm_sum_nzv <- nearZeroVar(sm_summary_data, names = TRUE, saveMetrics = TRUE)
sm_sum_nzv |> filter(nzv == TRUE)

sm_sum_cor_sp <- full_join(x = demo_data, y = sm_summary_data, by = "Study_ID") |>
  select(-Study_ID) |>
  drop_na() |>
  mutate(across(where(is.factor), as.numeric)) |>
  cor(method = "spearman")

sum_sum_sp_path= path_wd("04_outputs", "spine_mouse_spearman_corr_mat",
                         ext = "png")

# create png for writing plot
png(height=1800, width=1800, file=sum_sum_sp_path, type = "cairo")

corrplot(sm_sum_cor_sp, 
         method = "circle",
         tl.cex = 1, # text size
         cl.cex = 2, # color bar text size
         addCoef.col = 1, # add corr values
         number.cex = 1, # size of numbers
         tl.col = "black", # color of text
         col = brewer.pal(n=10, name="PiYG")) 
dev.off()

findCorrelation(sm_sum_cor_sp, cutoff = 0.9, names = TRUE)

# look at the correlation with LBP status to decide which to keep #
# highest correlation will be kept #

x_cor <- findCorrelation(sm_sum_cor_sp, cutoff = 0.9, names = TRUE)

lbp <- sm_sum_cor_sp[3,]

lbp[x_cor]

# SM_lumbar_value_sagittal_sit_flexion higher than SM_lumbar_value_sagittal_stand_flexion
# so SM_lumbar_value_sagittal_stand_flexion -> removed
# SM_spine_length_sagittal_stand_extension highest and will be kept all others regarding
# spine length will be removed #

# high correlated spine length and value cols to be removed 

high_cor_sm <- sm_sum_orig %>% 
  select(SM_lumbar_value_sagittal_stand_flexion,
         starts_with("SM_surface_length"),
         starts_with("SM_spine_length_frontal"),
         starts_with("SM_spine_length_sagittal_sit"),
         SM_spine_length_sagittal_stand_straight,
         SM_spine_length_sagittal_stand_flexion
  ) %>%
  colnames()

sm_summary_data <- select(sm_summary_data,
                          -c(all_of(high_cor_sm)))

sm_sum_cor_sp <- sm_summary_data |>
  select(-Study_ID) |>
  mutate(across(where(is.factor), as.numeric)) |>
  cor(method = "spearman")

sum_sum_sp_path= path_wd("04_outputs", "spine_mouse_spearman_corr_mat_cleaned",
                         ext = "png")

# create png for writing plot
png(height=1800, width=1800, file=sum_sum_sp_path, type = "cairo")

corrplot(sm_sum_cor_sp, 
         method = "circle",
         tl.cex = 1, # text size
         cl.cex = 2, # color bar text size
         addCoef.col = 1, # add corr values
         number.cex = 1, # size of numbers
         tl.col = "black", # color of text
         col = brewer.pal(n=10, name="PiYG")) 
dev.off()

# join sm data with demo #

sm_summary_data_clean <- full_join(demo_data, sm_summary_data, by = "Study_ID") %>%
  drop_na()

# save cleaned data as .rds and .csv #

sm_summary_data_clean_fn <- paste0("SM_SUM_cleaned_Clinic_BP_dim_",
                                   nrow(sm_summary_data_clean),
                                   "X",
                                   ncol(sm_summary_data_clean))

saveRDS(sm_summary_data_clean, file = path(output_dir, 
                                           sm_summary_data_clean_fn,
                                           ext = "rds"))

write.csv(sm_summary_data_clean, file = path(output_dir,
                                             sm_summary_data_clean_fn,
                                             ext = "csv"),
          row.names = FALSE)

# Spine mouse summary removed cols during preprocessing and cleaning reasons table #

sm_sum_rm <- compare_df_cols(sm_summary_data, sm_sum_orig, return = "match") %>%
  filter(is.na(sm_summary_data)) %>%
  mutate(
    Reason = case_when(
      column_name %in% high_cor_sm ~ "High correlation (> 0.9)",
      .default = "Missing Error"
    )
  ) %>%
  select(-sm_summary_data) %>%
  rename(col_type = sm_sum_orig)

# save #
write.csv(sm_sum_rm, file = path(output_dir, "SM_sum_removed_cols_preproc",
                                 ext = "csv"), row.names = FALSE)

################################################################################
####################### MRI & Questionnaire ####################################

# join Demo, MRI, & Questionnaire data #

mri_quest_data_clean <- full_join(demo_data, mri_data, by = "Study_ID") %>%
  full_join(., quest_data, by = "Study_ID") %>%
  drop_na()

# save cleaned data as .rds and .csv #

mri_quest_data_clean_fn <- paste0("MRI_Quest_cleaned_Clinic_BP_dim_",
                                  nrow(mri_quest_data_clean),
                                  "X",
                                  ncol(mri_quest_data_clean))

saveRDS(mri_quest_data_clean, file = path(output_dir, 
                                          mri_quest_data_clean_fn,
                                          ext = "rds"))

write.csv(mri_quest_data_clean, file = path(output_dir, 
                                            mri_quest_data_clean_fn,
                                            ext = "csv"),
          row.names = FALSE)


################################################################################
####################### MRI & Clinical Assessment ##############################

# join demo, mri, & clinical data #

mri_clinic_data_clean <- full_join(demo_data, mri_data, by = "Study_ID") %>%
  full_join(., clinic_data, by = "Study_ID") %>%
  drop_na()


# save cleaned data as .rds and .csv #

mri_clinic_data_clean_fn <- paste0("MRI_Clinic_cleaned_Clinic_BP_dim_",
                                   nrow(mri_clinic_data_clean),
                                   "X",
                                   ncol(mri_clinic_data_clean))

saveRDS(mri_clinic_data_clean, file = path(output_dir, 
                                           mri_clinic_data_clean_fn,
                                           ext = "rds"))

write.csv(mri_clinic_data_clean, file = path(output_dir,
                                             mri_clinic_data_clean_fn,
                                             ext = "csv"),
          row.names = FALSE)

################################################################################
####################### MRI & Spine Mouse ######################################

# join demo, mri, & spine mouse data #

# Spine Mouse summarised data set #
mri_sm_sum_data_clean <- full_join(demo_data, mri_data, by = "Study_ID") %>%
  full_join(., sm_summary_data, by = "Study_ID") %>%
  drop_na()


# save data as .rds and .csv #

mri_sm_sum_data_clean_fn <- paste0("MRI_SM_SUM_cleaned_Clinic_BP_dim_",
                                    nrow(mri_sm_sum_data_clean),
                                    "X",
                                    ncol(mri_sm_sum_data_clean))

saveRDS(mri_sm_sum_data_clean, file = path(output_dir, 
                                           mri_sm_sum_data_clean_fn,
                                           ext = "rds"))

write.csv(mri_sm_sum_data_clean, file = path(output_dir,
                                             mri_sm_sum_data_clean_fn,
                                             ext = "csv"),
          row.names = FALSE)


################################################################################
################# Questionnaire & Clinical Assessment ##########################

# join demo, questionnaire, & Clinical data #

quest_clinic_clean <- full_join(demo_data, quest_data, by = "Study_ID") %>%
  full_join(., clinic_data, by = "Study_ID") %>%
  drop_na()

# save cleaned data as .rds and .csv #

quest_clinic_clean_fn <- paste0("Quest_Clinic_cleaned_Clinic_BP_dim_",
                                nrow(quest_clinic_clean),
                                "X",
                                ncol(quest_clinic_clean))

saveRDS(quest_clinic_clean, file = path(output_dir, 
                                        quest_clinic_clean_fn,
                                        ext = "rds"))

write.csv(quest_clinic_clean, file = path(output_dir,
                                             quest_clinic_clean_fn,
                                             ext = "csv"),
          row.names = FALSE)


################################################################################
################# Questionnaire & Spine Mouse ##################################

# join demo, questionnaire, & summarise Spine Mouse data #

quest_sm_sum_clean <- full_join(demo_data, quest_data, by = "Study_ID") %>%
  full_join(., sm_summary_data, by = "Study_ID") %>%
  drop_na()

# save cleaned data as .rds and .csv #

quest_sm_sum_clean_fn <- paste0("Quest_SM_SUM_cleaned_Clinic_BP_dim_",
                                 nrow(quest_sm_sum_clean),
                                 "X",
                                 ncol(quest_sm_sum_clean))

saveRDS(quest_sm_sum_clean, file = path(output_dir, 
                                         quest_sm_sum_clean_fn,
                                         ext = "rds"))

write.csv(quest_sm_sum_clean, file = path(output_dir,
                                           quest_sm_sum_clean_fn,
                                           ext = "csv"),
          row.names = FALSE)


################################################################################
################# Clinical Assessment & Spine Mouse ############################

# join demo, Clinic, & summmary Spine Mouse data #

clinic_sm_sum_clean <- full_join(demo_data, clinic_data, by = "Study_ID") %>%
  full_join(., sm_summary_data, by = "Study_ID") %>%
  drop_na()

# save cleaned data as .rds and .csv #

clinic_sm_sum_clean_fn <- paste0("Clinic_SM_SUM_cleaned_Clinic_BP_dim_",
                                nrow(clinic_sm_sum_clean),
                                "X",
                                ncol(clinic_sm_sum_clean))

saveRDS(clinic_sm_sum_clean, file = path(output_dir, 
                                        clinic_sm_sum_clean_fn,
                                        ext = "rds"))

write.csv(clinic_sm_sum_clean, file = path(output_dir,
                                          clinic_sm_sum_clean_fn,
                                          ext = "csv"),
          row.names = FALSE)



################################################################################
#################### MRI + Quest + Clinical ######################################

# join data frames and remove na's #

mri_quest_clinic_data_clean <- full_join(demo_data, mri_data, by = "Study_ID") %>%
  full_join(., quest_data, by = "Study_ID") %>%
  full_join(., clinic_data, by = "Study_ID") %>%
  drop_na()

# save cleaned data as .rds and .csv #

mri_quest_clinic_clean_fn <- paste0("MRI_Quest_Clinical_cleaned_Clinic_BP_dim_",
                                    nrow(mri_quest_clinic_data_clean),
                                    "X",
                                    ncol(mri_quest_clinic_data_clean))

saveRDS(mri_quest_clinic_data_clean, file = path(output_dir, 
                                                 mri_quest_clinic_clean_fn,
                                                 ext = "rds"))

write.csv(mri_quest_clinic_data_clean, file = path(output_dir,
                                                   mri_quest_clinic_clean_fn,
                                                   ext = "csv"),
          row.names = FALSE)


################################################################################
#################### MRI + Quest + Spine Mouse #################################

# join demo, mri, questionnaire, & spine mouse summary data  #

mri_quest_sm_sum_data_clean <- full_join(demo_data, mri_data, by = "Study_ID") %>%
  full_join(., quest_data, by = "Study_ID") %>%
  full_join(., sm_summary_data, by = "Study_ID") %>%
  drop_na()

# save cleaned data as .rds and .csv #

mri_quest_sm_sum_clean_fn <- paste0("MRI_Quest_SM_SUM_cleaned_Clinic_BP_dim_",
                                     nrow(mri_quest_sm_sum_data_clean),
                                     "X",
                                     ncol(mri_quest_sm_sum_data_clean))

saveRDS(mri_quest_sm_sum_data_clean, file = path(output_dir, 
                                                  mri_quest_sm_sum_clean_fn,
                                                  ext = "rds"))

write.csv(mri_quest_sm_sum_data_clean, file = path(output_dir,
                                                    mri_quest_sm_sum_clean_fn,
                                                    ext = "csv"),
          row.names = FALSE)


################################################################################
#################### MRI + Clinical + Spine Mouse ##############################

# join demo, mri, clinic, & spine mouse summary data  #

mri_clinic_sm_sum_data_clean <- full_join(demo_data, mri_data, by = "Study_ID") %>%
  full_join(., clinic_data, by = "Study_ID") %>%
  full_join(., sm_summary_data, by = "Study_ID") %>%
  drop_na()

# save cleaned data as .rds and .csv #

mri_clinic_sm_sum_clean_fn <- paste0("MRI_Clinic_SM_SUM_cleaned_Clinic_BP_dim_",
                                     nrow(mri_clinic_sm_sum_data_clean),
                                     "X",
                                     ncol(mri_clinic_sm_sum_data_clean))

saveRDS(mri_clinic_sm_sum_data_clean, file = path(output_dir, 
                                                 mri_clinic_sm_sum_clean_fn,
                                                 ext = "rds"))

write.csv(mri_clinic_sm_sum_data_clean, file = path(output_dir,
                                                   mri_clinic_sm_sum_clean_fn,
                                                   ext = "csv"),
          row.names = FALSE)


################################################################################
#################### Quest + Clinical + Spine Mouse ############################

# join demo, quest, clinic, & spine mouse summary data  #

quest_clinic_sm_sum_data_clean <- full_join(demo_data, quest_data, by = "Study_ID") %>%
  full_join(., clinic_data, by = "Study_ID") %>%
  full_join(., sm_summary_data, by = "Study_ID") %>%
  drop_na()

# save cleaned data as .rds and .csv #

quest_clinic_sm_sum_clean_fn <- paste0("Quest_Clinic_SM_SUM_cleaned_Clinic_BP_dim_",
                                     nrow(quest_clinic_sm_sum_data_clean),
                                     "X",
                                     ncol(quest_clinic_sm_sum_data_clean))

saveRDS(quest_clinic_sm_sum_data_clean, file = path(output_dir, 
                                                  quest_clinic_sm_sum_clean_fn,
                                                  ext = "rds"))

write.csv(quest_clinic_sm_sum_data_clean, file = path(output_dir,
                                                    quest_clinic_sm_sum_clean_fn,
                                                    ext = "csv"),
          row.names = FALSE)


################################################################################
#################### MRI + Quest + Clinical + Spine Mouse ########################

# join demo, MRI quest, clinic, & spine mouse summary data  #

mri_quest_clinic_sm_sum_data_clean <- full_join(demo_data, mri_data, by = "Study_ID") %>%
  full_join(., quest_data, by = "Study_ID") %>%
  full_join(., clinic_data, by = "Study_ID") %>%
  full_join(., sm_summary_data, by = "Study_ID") %>%
  drop_na()

#### All data correlation matrix ######

all_cor_sp <- mri_quest_clinic_sm_sum_data_clean |>
  select(-Study_ID) |>
  mutate(across(where(is.factor), as.numeric)) |>
  cor(method = "spearman")

# create overall correlation matrix

all_sp_path= path_wd("04_outputs", "all_feats_spearman_corr_mat",
                     ext = "png")

# create png for writing plot
png(height=1800, width=1800, file=all_sp_path, type = "cairo")

corrplot(all_cor_sp, 
         method = "circle",
         tl.cex = 1, # text size
         cl.cex = 2, # color bar text size
         #addCoef.col = 1, # add corr values
         number.cex = 1, # size of numbers
         tl.col = "black", # color of text
         col = brewer.pal(n=10, name="PiYG")) 
dev.off()

# save cleaned data as .rds and .csv #

mri_quest_clinic_sm_sum_clean_fn <- paste0("MRI_Quest_Clinic_SM_SUM_cleaned_Clinic_BP_dim_",
                                            nrow(mri_quest_clinic_sm_sum_data_clean),
                                            "X",
                                            ncol(mri_quest_clinic_sm_sum_data_clean))

saveRDS(mri_quest_clinic_sm_sum_data_clean, file = path(output_dir, 
                                                         mri_quest_clinic_sm_sum_clean_fn,
                                                         ext = "rds"))

write.csv(mri_quest_clinic_sm_sum_data_clean, file = path(output_dir,
                                                           mri_quest_clinic_sm_sum_clean_fn,
                                                           ext = "csv"),
          row.names = FALSE)


##### Hold-out sample for robust best feature testing ##########################

# Using the all modality sample, I will take 10% of the whole sample - size of demo_data = 1161
# 116 subjects will be taken from mri_quest_clinic_sm_sum_data_clean as these will have 
# all data from all modalities and can be used to test the best features following Boruta selection
# After taking them out of mri_quest_clinic_sm_sum_data_clean I will then use the Study_ID's to 
# remove them from all the samples, and these are the samples I will then use for the Boruta and RF modelling #


set.seed(1027)
holdout_ids <- mri_quest_clinic_sm_sum_data_clean |>
  slice_sample(n=116) |> # 10% of total sample
  pull(Study_ID)
  
# For the classification and feature selection (Boruta), I will remove the hold-out
# sample from each dataset to be then used for testing of the overall best features

################# Single Dataset modalities ####################################

## Clinic ###
clinic_ML_clean <- clinic_data_clean |> 
  filter(! Study_ID %in% holdout_ids) 

# save #
clinic_ML_clean_fn <- paste0("Clinic_ML_cleaned_Clinic_BP_dim_",
                            nrow(clinic_ML_clean),
                            "X",
                            ncol(clinic_ML_clean))

saveRDS(clinic_ML_clean, file = path(output_dir, 
                                  clinic_ML_clean_fn,
                                  ext = "rds"))

write.csv(clinic_ML_clean, file = path(output_dir,
                                    clinic_ML_clean_fn,
                                    ext = "csv"),
          row.names = FALSE)

## questionnaire ###
quest_ML_clean <- quest_data_clean |> 
  filter(! Study_ID %in% holdout_ids) 

# save #
quest_ML_clean_fn <- paste0("Quest_ML_cleaned_Clinic_BP_dim_",
                             nrow(quest_ML_clean),
                             "X",
                             ncol(quest_ML_clean))

saveRDS(quest_ML_clean, file = path(output_dir, 
                                    quest_ML_clean_fn,
                                     ext = "rds"))

write.csv(quest_ML_clean, file = path(output_dir,
                                      quest_ML_clean_fn,
                                       ext = "csv"),
          row.names = FALSE)

## MRI ###
mri_ML_clean <- mri_data_clean |> 
  filter(! Study_ID %in% holdout_ids) 

# save #
mri_ML_clean_fn <- paste0("MRI_ML_cleaned_Clinic_BP_dim_",
                            nrow(mri_ML_clean),
                            "X",
                            ncol(mri_ML_clean))

saveRDS(mri_ML_clean, file = path(output_dir, 
                                  mri_ML_clean_fn,
                                  ext = "rds"))

write.csv(mri_ML_clean, file = path(output_dir,
                                    mri_ML_clean_fn,
                                    ext = "csv"),
          row.names = FALSE)

## spine mouse ###
sm_ML_clean <- sm_summary_data_clean |> 
  filter(! Study_ID %in% holdout_ids) 

# save #
sm_ML_clean_fn <- paste0("SM_SUM_ML_cleaned_Clinic_BP_dim_",
                          nrow(sm_ML_clean),
                          "X",
                          ncol(sm_ML_clean))

saveRDS(sm_ML_clean, file = path(output_dir, 
                                 sm_ML_clean_fn,
                                  ext = "rds"))

write.csv(sm_ML_clean, file = path(output_dir,
                                   sm_ML_clean_fn,
                                    ext = "csv"),
          row.names = FALSE)

################# Dual Dataset modalities ######################################

## MRI + quest ###
mri_quest_ML_clean <- mri_quest_data_clean|> 
  filter(! Study_ID %in% holdout_ids) 

# save #
mri_quest_ML_clean_fn <- paste0("MRI_Quest_ML_cleaned_Clinic_BP_dim_",
                                nrow(mri_quest_ML_clean),
                                "X",
                                ncol(mri_quest_ML_clean))

saveRDS(mri_quest_ML_clean, file = path(output_dir, 
                                        mri_quest_ML_clean_fn,
                                        ext = "rds"))

write.csv(mri_quest_ML_clean, file = path(output_dir,
                                          mri_quest_ML_clean_fn,
                                          ext = "csv"),
          row.names = FALSE)

## mri + clinic ###
mri_clinic_ML_clean <- mri_clinic_data_clean  |> 
  filter(! Study_ID %in% holdout_ids) 

# save #
mri_clinic_ML_clean_fn <- paste0("MRI_Clinic_ML_cleaned_Clinic_BP_dim_",
                                 nrow(mri_clinic_ML_clean),
                                 "X",
                                 ncol(mri_clinic_ML_clean))

saveRDS(mri_clinic_ML_clean, file = path(output_dir, 
                                         mri_clinic_ML_clean_fn,
                                         ext = "rds"))

write.csv(mri_clinic_ML_clean, file = path(output_dir,
                                           mri_clinic_ML_clean_fn,
                                           ext = "csv"),
          row.names = FALSE)

## mri + sm ###
mri_sm_ML_clean <- mri_sm_sum_data_clean |> 
  filter(! Study_ID %in% holdout_ids) 

# save #
mri_sm_ML_clean_fn <- paste0("MRI_SM_SUM_ML_cleaned_Clinic_BP_dim_",
                             nrow(mri_sm_ML_clean),
                             "X",
                             ncol(mri_sm_ML_clean))

saveRDS(mri_sm_ML_clean, file = path(output_dir, 
                                     mri_sm_ML_clean_fn,
                                     ext = "rds"))

write.csv(mri_sm_ML_clean, file = path(output_dir,
                                       mri_sm_ML_clean_fn,
                                       ext = "csv"),
          row.names = FALSE)

## quest + clinic ###
quest_clinic_ML_clean <- quest_clinic_clean |> 
  filter(! Study_ID %in% holdout_ids) 

# save #
quest_clinic_ML_clean_fn <- paste0("Quest_Clinic_ML_cleaned_Clinic_BP_dim_",
                                   nrow(quest_clinic_ML_clean),
                                   "X",
                                   ncol(quest_clinic_ML_clean))

saveRDS(quest_clinic_ML_clean, file = path(output_dir, 
                                           quest_clinic_ML_clean_fn,
                                           ext = "rds"))

write.csv(quest_clinic_ML_clean, file = path(output_dir,
                                             quest_clinic_ML_clean_fn,
                                             ext = "csv"),
          row.names = FALSE)

## quest + sm ###
quest_sm_ML_clean <- quest_sm_sum_clean |> 
  filter(! Study_ID %in% holdout_ids) 

# save #
quest_sm_ML_clean_fn <- paste0("Quest_SM_SUM_ML_cleaned_Clinic_BP_dim_",
                               nrow(quest_sm_ML_clean),
                               "X",
                               ncol(quest_sm_ML_clean))

saveRDS(quest_sm_ML_clean, file = path(output_dir, 
                                       quest_sm_ML_clean_fn,
                                       ext = "rds"))

write.csv(quest_sm_ML_clean, file = path(output_dir,
                                         quest_sm_ML_clean_fn,
                                         ext = "csv"),
          row.names = FALSE)

## clinic + sm summary ###
clinic_sm_ML_clean <- clinic_sm_sum_clean |> 
  filter(! Study_ID %in% holdout_ids) 

# save #
Clinic_sm_ML_clean_fn <- paste0("Clinic_SM_SUM_ML_cleaned_Clinic_BP_dim_",
                                nrow(clinic_sm_ML_clean),
                                "X",
                                ncol(clinic_sm_ML_clean))

saveRDS(clinic_sm_ML_clean, file = path(output_dir, 
                                        Clinic_sm_ML_clean_fn,
                                        ext = "rds"))

write.csv(clinic_sm_ML_clean, file = path(output_dir,
                                          Clinic_sm_ML_clean_fn,
                                          ext = "csv"),
          row.names = FALSE)

################# multi Dataset modalities #####################################

## mri + quest + clinic ###
mri_quest_clinic_ML_clean <- mri_quest_clinic_data_clean |> 
  filter(! Study_ID %in% holdout_ids) 

# save #
mri_quest_clinic_ML_clean_fn <- paste0("MRI_Quest_Clinic_ML_cleaned_Clinic_BP_dim_",
                                       nrow(mri_quest_clinic_ML_clean),
                                       "X",
                                       ncol(mri_quest_clinic_ML_clean))

saveRDS(mri_quest_clinic_ML_clean, file = path(output_dir, 
                                               mri_quest_clinic_ML_clean_fn,
                                               ext = "rds"))

write.csv(mri_quest_clinic_ML_clean, file = path(output_dir,
                                                 mri_quest_clinic_ML_clean_fn,
                                                 ext = "csv"),
          row.names = FALSE)

## mri + quest + sm ###
mri_quest_sm_ML_clean <- mri_quest_sm_sum_data_clean |> 
  filter(! Study_ID %in% holdout_ids) 

# save #
mri_quest_sm_ML_clean_fn <- paste0("MRI_Quest_SM_SUM_ML_cleaned_Clinic_BP_dim_",
                                   nrow(mri_quest_sm_ML_clean),
                                   "X",
                                   ncol(mri_quest_sm_ML_clean))

saveRDS(mri_quest_sm_ML_clean, file = path(output_dir, 
                                           mri_quest_sm_ML_clean_fn,
                                           ext = "rds"))

write.csv(mri_quest_sm_ML_clean, file = path(output_dir,
                                             mri_quest_sm_ML_clean_fn,
                                             ext = "csv"),
          row.names = FALSE)

## mri + clinic + sm summary ###
mri_clinic_sm_ML_clean <- mri_clinic_sm_sum_data_clean |> 
  filter(! Study_ID %in% holdout_ids) 

# save #
mri_clinic_sm_ML_clean_fn <- paste0("MRI_Clinic_SM_SUM_ML_cleaned_Clinic_BP_dim_",
                                    nrow(mri_clinic_sm_ML_clean),
                                    "X",
                                    ncol(mri_clinic_sm_ML_clean))

saveRDS(mri_clinic_sm_ML_clean, file = path(output_dir, 
                                            mri_clinic_sm_ML_clean_fn,
                                            ext = "rds"))

write.csv(mri_clinic_sm_ML_clean, file = path(output_dir,
                                              mri_clinic_sm_ML_clean_fn,
                                              ext = "csv"),
          row.names = FALSE)

## quest + clinic + sm summary ###
quest_clinic_sm_ML_clean <- quest_clinic_sm_sum_data_clean |> 
  filter(! Study_ID %in% holdout_ids) 

# save #
quest_clinic_sm_ML_clean_fn <- paste0("Quest_Clinic_SM_SUM_ML_cleaned_Clinic_BP_dim_",
                                      nrow(quest_clinic_sm_ML_clean),
                                      "X",
                                      ncol(quest_clinic_sm_ML_clean))

saveRDS(quest_clinic_sm_ML_clean, file = path(output_dir, 
                                              quest_clinic_sm_ML_clean_fn,
                                              ext = "rds"))

write.csv(quest_clinic_sm_ML_clean, file = path(output_dir,
                                                quest_clinic_sm_ML_clean_fn,
                                                ext = "csv"),
          row.names = FALSE)

##################### All data modalities ######################################

## mri + quest + clinic + sm summary ###
mri_quest_clinic_sm_ML_clean <- mri_quest_clinic_sm_sum_data_clean |> 
  filter(! Study_ID %in% holdout_ids) 

# save #
mri_quest_clinic_sm_ML_clean_fn <- paste0("MRI_Quest_Clinic_SM_SUM_ML_cleaned_Clinic_BP_dim_",
                                          nrow(mri_quest_clinic_sm_ML_clean),
                                          "X",
                                          ncol(mri_quest_clinic_sm_ML_clean))

saveRDS(mri_quest_clinic_sm_ML_clean, file = path(output_dir, 
                                                  mri_quest_clinic_sm_ML_clean_fn,
                                                  ext = "rds"))

write.csv(mri_quest_clinic_sm_ML_clean, file = path(output_dir,
                                                    mri_quest_clinic_sm_ML_clean_fn,
                                                    ext = "csv"),
          row.names = FALSE)

## sample used for testing most robust and important features ##
hold_out_sample <- mri_quest_clinic_sm_sum_data_clean |> 
  filter(Study_ID %in% holdout_ids) 

# save #
holdout_ML_clean_fn <- paste0("Holdout_ALL_ML_Clinic_BP_dim_",
                              nrow(hold_out_sample),
                              "X",
                              ncol(hold_out_sample))

saveRDS(hold_out_sample, file = path(output_dir, 
                                     holdout_ML_clean_fn,
                                     ext = "rds"))

write.csv(hold_out_sample, file = path(output_dir,
                                       holdout_ML_clean_fn,
                                       ext = "csv"),
          row.names = FALSE)

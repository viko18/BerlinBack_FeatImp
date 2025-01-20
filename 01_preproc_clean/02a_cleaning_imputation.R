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

output_dir <- path_wd("01_Data", "Cleaned_imp")
if(!dir_exists(output_dir)) {dir_create(output_dir)}


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

# after check for number of NA's drop them for further cleaning #
demo_data <- demo_data %>% drop_na()

# print summaries #
glimpse(demo_data)
summary(demo_data)


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


# create cleaned data frame by removing unwanted columns and subj with NAs #
mri_data <- mri_orig %>%
  select(
    # these cols removed #
    -c(
      all_of(mri_L6S1),
      all_of(mri_obs),
      all_of(mri_schiz), 
      #all_of(mri_lord) missing lordosis values will then be imputed
    )
  ) %>%
  droplevels() 

# These cols are taken from original cleaning script #
mri_nzv_cols <- c("MRI_osteo_l1_l2", "MRI_osteo_l2_l3", "MRI_osteo_l3_l4", 
                  "MRI_disc_hern_l1_l2")

# remove low variance columns #
mri_data <- select(mri_data,
                   -c(all_of(mri_nzv_cols))
)

# join with demographic data #
mri_data_imp <- inner_join(x = demo_data, y = mri_data, by = "Study_ID")

# view data
glimpse(mri_data_imp)
summary(mri_data_imp)

# save cleaned data as .rds and .csv #

mri_data_clean_fn <- paste0("MRI_cleaned_IMP_Clinic_BP_dim_",
                            nrow(mri_data_imp),
                            "X",
                            ncol(mri_data_imp))

saveRDS(mri_data_imp, file = path(output_dir, 
                                  mri_data_clean_fn,
                                  ext = "rds"))

write.csv(mri_data_imp, file = path(output_dir,
                                    mri_data_clean_fn,
                                    ext = "csv"),
          row.names = FALSE)

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
      #Clinic_question_job_posture_duration, # ~ 290 NAs can be imputed
      Clinic_question_job_duration_years, # high correlation with age and has more missing values so removed
      Clinic_question_job_load_2_duration, # > 850 Na's too many for imputation
      Clinic_question_job_load_2, # > 850 Na's too many for imputation
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
    -c(Clinic_question_smoking, # high cor
       Clinic_prior_surgeries_which, # high cor
       Clinic_question_alcohol_restrict) # low var
  ) %>%
  droplevels()

# join demographic data with questionnaire data for saving #
quest_data_imp <- inner_join(x = demo_data, y = quest_data, by = "Study_ID") 

# print summaries #
glimpse(quest_data_imp)
summary(quest_data_imp)

# save cleaned data as .rds and .csv #

quest_data_clean_fn <- paste0("Questionnaire_cleaned_IMP_Clinic_BP_dim_",
                              nrow(quest_data_imp),
                              "X",
                              ncol(quest_data_imp))

saveRDS(quest_data_imp, file = path(output_dir, 
                                    quest_data_clean_fn,
                                    ext = "rds"))

write.csv(quest_data_imp, file = path(output_dir,
                                      quest_data_clean_fn,
                                      ext = "csv"),
          row.names = FALSE)


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

# conduct cleaning with additionally removed cols #
clinic_data <- preproc_data %>%
  select(
    Study_ID, # need subject ID for joining with both modalities and demo data
    starts_with("Clinic_body"),
    starts_with("Clinic_motion"),
    # remove cols #
    -c(
      # Clinic_body_hip_flexion, # has over 250 NA's could be added later in study
      #Clinic_motion_pseudo_lasegue_r, # ~280 could have been added later in study
      #Clinic_motion_pseudo_lasegue_l, # ~280 could have been added later in study
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
  droplevels()

# join clinic & demo data #
clinic_data_imp <- inner_join(demo_data, clinic_data, by="Study_ID")

summary(clinic_data_imp)

# save cleaned data as .rds and .csv #

clinic_data_clean_fn <- paste0("Clinic_cleaned_IMP_Clinic_BP_dim_",
                               nrow(clinic_data_imp),
                               "X",
                               ncol(clinic_data_imp))

saveRDS(clinic_data_imp, file = path(output_dir, 
                                     clinic_data_clean_fn,
                                     ext = "rds"))

write.csv(clinic_data_imp, file = path(output_dir,
                                       clinic_data_clean_fn,
                                       ext = "csv"),
          row.names = FALSE)

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

sm_summary_data_imp <- inner_join(demo_data, sm_summary_data, by = "Study_ID")

summary(sm_summary_data_imp)

# save cleaned data as .rds and .csv #

sm_summary_data_clean_fn <- paste0("SM_SUM_cleaned_Clinic_BP_dim_",
                                   nrow(sm_summary_data_imp),
                                   "X",
                                   ncol(sm_summary_data_imp))

saveRDS(sm_summary_data_imp, file = path(output_dir, 
                                         sm_summary_data_clean_fn,
                                         ext = "rds"))

write.csv(sm_summary_data_imp, file = path(output_dir,
                                           sm_summary_data_clean_fn,
                                           ext = "csv"),
          row.names = FALSE)


################################################################################
####################### MRI & Questionnaire ####################################

# join Demo, MRI, & Questionnaire data #

mri_quest_data_imp <- inner_join(demo_data, mri_data, by = "Study_ID") %>%
  inner_join(., quest_data, by = "Study_ID") 

# save cleaned data as .rds and .csv #

mri_quest_data_clean_fn <- paste0("MRI_Quest_cleaned_IMP_Clinic_BP_dim_",
                                  nrow(mri_quest_data_imp),
                                  "X",
                                  ncol(mri_quest_data_imp))

saveRDS(mri_quest_data_imp, file = path(output_dir, 
                                        mri_quest_data_clean_fn,
                                        ext = "rds"))

write.csv(mri_quest_data_imp, file = path(output_dir, 
                                          mri_quest_data_clean_fn,
                                          ext = "csv"),
          row.names = FALSE)


################################################################################
####################### MRI & Clinical Assessment ##############################

# join demo, mri, & clinical data #

mri_clinic_data_imp <- inner_join(demo_data, mri_data, by = "Study_ID") %>%
  inner_join(., clinic_data, by = "Study_ID") 

# save cleaned data as .rds and .csv #

mri_clinic_data_clean_fn <- paste0("MRI_Clinic_cleaned_Clinic_BP_dim_",
                                   nrow(mri_clinic_data_imp),
                                   "X",
                                   ncol(mri_clinic_data_imp))

saveRDS(mri_clinic_data_imp, file = path(output_dir, 
                                         mri_clinic_data_clean_fn,
                                         ext = "rds"))

write.csv(mri_clinic_data_imp, file = path(output_dir,
                                           mri_clinic_data_clean_fn,
                                           ext = "csv"),
          row.names = FALSE)

################################################################################
####################### MRI & Spine Mouse ######################################

# join demo, mri, & spine mouse data #

# Spine Mouse summarised data set #
mri_sm_sum_data_imp <- inner_join(demo_data, mri_data, by = "Study_ID") %>%
  inner_join(., sm_summary_data, by = "Study_ID")

# save data as .rds and .csv #

mri_sm_sum_data_clean_fn <- paste0("MRI_SM_SUM_cleaned_Clinic_BP_dim_",
                                   nrow(mri_sm_sum_data_imp),
                                   "X",
                                   ncol(mri_sm_sum_data_imp))

saveRDS(mri_sm_sum_data_imp, file = path(output_dir, 
                                         mri_sm_sum_data_clean_fn,
                                         ext = "rds"))

write.csv(mri_sm_sum_data_imp, file = path(output_dir,
                                           mri_sm_sum_data_clean_fn,
                                           ext = "csv"),
          row.names = FALSE)


################################################################################
################# Questionnaire & Clinical Assessment ##########################

# join demo, questionnaire, & Clinical data #

quest_clinic_imp <- inner_join(demo_data, quest_data, by = "Study_ID") %>%
  inner_join(., clinic_data, by = "Study_ID") 

# save cleaned data as .rds and .csv #

quest_clinic_clean_fn <- paste0("Quest_Clinic_cleaned_Clinic_BP_dim_",
                                nrow(quest_clinic_imp),
                                "X",
                                ncol(quest_clinic_imp))

saveRDS(quest_clinic_imp, file = path(output_dir, 
                                      quest_clinic_clean_fn,
                                      ext = "rds"))

write.csv(quest_clinic_imp, file = path(output_dir,
                                        quest_clinic_clean_fn,
                                        ext = "csv"),
          row.names = FALSE)


################################################################################
################# Questionnaire & Spine Mouse ##################################

# join demo, questionnaire, & summarise Spine Mouse data #

quest_sm_sum_imp <- inner_join(demo_data, quest_data, by = "Study_ID") %>%
  inner_join(., sm_summary_data, by = "Study_ID")

# save cleaned data as .rds and .csv #

quest_sm_sum_clean_fn <- paste0("Quest_SM_SUM_cleaned_Clinic_BP_dim_",
                                nrow(quest_sm_sum_imp),
                                "X",
                                ncol(quest_sm_sum_imp))

saveRDS(quest_sm_sum_imp, file = path(output_dir, 
                                      quest_sm_sum_clean_fn,
                                      ext = "rds"))

write.csv(quest_sm_sum_imp, file = path(output_dir,
                                        quest_sm_sum_clean_fn,
                                        ext = "csv"),
          row.names = FALSE)


################################################################################
################# Clinical Assessment & Spine Mouse ############################

# join demo, Clinic, & summmary Spine Mouse data #

clinic_sm_sum_imp <- inner_join(demo_data, clinic_data, by = "Study_ID") %>%
  inner_join(., sm_summary_data, by = "Study_ID") 

# save cleaned data as .rds and .csv #

clinic_sm_sum_clean_fn <- paste0("Clinic_SM_SUM_cleaned_Clinic_BP_dim_",
                                 nrow(clinic_sm_sum_imp),
                                 "X",
                                 ncol(clinic_sm_sum_imp))

saveRDS(clinic_sm_sum_imp, file = path(output_dir, 
                                       clinic_sm_sum_clean_fn,
                                       ext = "rds"))

write.csv(clinic_sm_sum_imp, file = path(output_dir,
                                         clinic_sm_sum_clean_fn,
                                         ext = "csv"),
          row.names = FALSE)



################################################################################
#################### MRI + Quest + Clinical ######################################

# join data frames

mri_quest_clinic_data_imp <- inner_join(demo_data, mri_data, by = "Study_ID") %>%
  inner_join(., quest_data, by = "Study_ID") %>%
  inner_join(., clinic_data, by = "Study_ID") 

# save cleaned data as .rds and .csv #

mri_quest_clinic_clean_fn <- paste0("MRI_Quest_Clinical_cleaned_Clinic_BP_dim_",
                                    nrow(mri_quest_clinic_data_imp),
                                    "X",
                                    ncol(mri_quest_clinic_data_imp))

saveRDS(mri_quest_clinic_data_imp, file = path(output_dir, 
                                               mri_quest_clinic_clean_fn,
                                               ext = "rds"))

write.csv(mri_quest_clinic_data_imp, file = path(output_dir,
                                                 mri_quest_clinic_clean_fn,
                                                 ext = "csv"),
          row.names = FALSE)


################################################################################
#################### MRI + Quest + Spine Mouse #################################

# join demo, mri, questionnaire, & spine mouse summary data  #

mri_quest_sm_sum_data_imp <- inner_join(demo_data, mri_data, by = "Study_ID") %>%
  inner_join(., quest_data, by = "Study_ID") %>%
  inner_join(., sm_summary_data, by = "Study_ID") 

# save cleaned data as .rds and .csv #

mri_quest_sm_sum_clean_fn <- paste0("MRI_Quest_SM_SUM_cleaned_Clinic_BP_dim_",
                                    nrow(mri_quest_sm_sum_data_imp),
                                    "X",
                                    ncol(mri_quest_sm_sum_data_imp))

saveRDS(mri_quest_sm_sum_data_imp, file = path(output_dir, 
                                               mri_quest_sm_sum_clean_fn,
                                               ext = "rds"))

write.csv(mri_quest_sm_sum_data_imp, file = path(output_dir,
                                                 mri_quest_sm_sum_clean_fn,
                                                 ext = "csv"),
          row.names = FALSE)


################################################################################
#################### MRI + Clinical + Spine Mouse ##############################

# join demo, mri, clinic, & spine mouse summary data  #

mri_clinic_sm_sum_data_imp <- inner_join(demo_data, mri_data, by = "Study_ID") %>%
  inner_join(., clinic_data, by = "Study_ID") %>%
  inner_join(., sm_summary_data, by = "Study_ID") 

# save cleaned data as .rds and .csv #

mri_clinic_sm_sum_clean_fn <- paste0("MRI_Clinic_SM_SUM_cleaned_Clinic_BP_dim_",
                                     nrow(mri_clinic_sm_sum_data_imp),
                                     "X",
                                     ncol(mri_clinic_sm_sum_data_imp))

saveRDS(mri_clinic_sm_sum_data_imp, file = path(output_dir, 
                                                mri_clinic_sm_sum_clean_fn,
                                                ext = "rds"))

write.csv(mri_clinic_sm_sum_data_imp, file = path(output_dir,
                                                  mri_clinic_sm_sum_clean_fn,
                                                  ext = "csv"),
          row.names = FALSE)


################################################################################
#################### Quest + Clinical + Spine Mouse ############################

# join demo, quest, clinic, & spine mouse summary data  #

quest_clinic_sm_sum_data_imp <- inner_join(demo_data, quest_data, by = "Study_ID") %>%
  inner_join(., clinic_data, by = "Study_ID") %>%
  inner_join(., sm_summary_data, by = "Study_ID") 

# save cleaned data as .rds and .csv #

quest_clinic_sm_sum_clean_fn <- paste0("Quest_Clinic_SM_SUM_cleaned_Clinic_BP_dim_",
                                       nrow(quest_clinic_sm_sum_data_imp),
                                       "X",
                                       ncol(quest_clinic_sm_sum_data_imp))

saveRDS(quest_clinic_sm_sum_data_imp, file = path(output_dir, 
                                                  quest_clinic_sm_sum_clean_fn,
                                                  ext = "rds"))

write.csv(quest_clinic_sm_sum_data_imp, file = path(output_dir,
                                                    quest_clinic_sm_sum_clean_fn,
                                                    ext = "csv"),
          row.names = FALSE)


################################################################################
#################### MRI + Quest + Clinical + Spine Mouse ########################

# join demo, MRI quest, clinic, & spine mouse summary data  #

mri_quest_clinic_sm_sum_data_imp <- inner_join(demo_data, mri_data, by = "Study_ID") %>%
  inner_join(., quest_data, by = "Study_ID") %>%
  inner_join(., clinic_data, by = "Study_ID") %>%
  inner_join(., sm_summary_data, by = "Study_ID") 

# save cleaned data as .rds and .csv #

mri_quest_clinic_sm_sum_clean_fn <- paste0("MRI_Quest_Clinic_SM_SUM_cleaned_Clinic_BP_dim_",
                                           nrow(mri_quest_clinic_sm_sum_data_imp),
                                           "X",
                                           ncol(mri_quest_clinic_sm_sum_data_imp))

saveRDS(mri_quest_clinic_sm_sum_data_imp, file = path(output_dir, 
                                                      mri_quest_clinic_sm_sum_clean_fn,
                                                      ext = "rds"))

write.csv(mri_quest_clinic_sm_sum_data_imp, file = path(output_dir,
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
holdout_ids <- mri_quest_clinic_sm_sum_data_imp |>
  slice_sample(n=116) |> # 10% of total sample
  pull(Study_ID)

# For the classification and feature selection (Boruta), I will remove the hold-out
# sample from each dataset to be then used for testing of the overall best features

################# Single Dataset modalities ####################################

## Clinic ###
clinic_ML_imp <- clinic_data_imp |> 
  filter(! Study_ID %in% holdout_ids) 

# save #
clinic_ML_clean_fn <- paste0("Clinic_ML_cleaned_IMP_Clinic_BP_dim_",
                             nrow(clinic_ML_imp),
                             "X",
                             ncol(clinic_ML_imp))

saveRDS(clinic_ML_imp, file = path(output_dir, 
                                   clinic_ML_clean_fn,
                                   ext = "rds"))

write.csv(clinic_ML_imp, file = path(output_dir,
                                     clinic_ML_clean_fn,
                                     ext = "csv"),
          row.names = FALSE)

## questionnaire ###
quest_ML_imp <- quest_data_imp |> 
  filter(! Study_ID %in% holdout_ids) 

# save #
quest_ML_clean_fn <- paste0("Quest_ML_cleaned_IMP_Clinic_BP_dim_",
                            nrow(quest_ML_imp),
                            "X",
                            ncol(quest_ML_imp))

saveRDS(quest_ML_imp, file = path(output_dir, 
                                  quest_ML_clean_fn,
                                  ext = "rds"))

write.csv(quest_ML_imp, file = path(output_dir,
                                    quest_ML_clean_fn,
                                    ext = "csv"),
          row.names = FALSE)

## MRI ###
mri_ML_imp <- mri_data_imp |> 
  filter(! Study_ID %in% holdout_ids) 

# save #
mri_ML_clean_fn <- paste0("MRI_ML_cleaned_IMP_Clinic_BP_dim_",
                          nrow(mri_ML_imp),
                          "X",
                          ncol(mri_ML_imp))

saveRDS(mri_ML_imp, file = path(output_dir, 
                                mri_ML_clean_fn,
                                ext = "rds"))

write.csv(mri_ML_imp, file = path(output_dir,
                                  mri_ML_clean_fn,
                                  ext = "csv"),
          row.names = FALSE)

## spine mouse ###
sm_ML_imp <- sm_summary_data_imp |> 
  filter(! Study_ID %in% holdout_ids) 

# save #
sm_ML_clean_fn <- paste0("SM_SUM_ML_cleaned_IMP_Clinic_BP_dim_",
                         nrow(sm_ML_imp),
                         "X",
                         ncol(sm_ML_imp))

saveRDS(sm_ML_imp, file = path(output_dir, 
                               sm_ML_clean_fn,
                               ext = "rds"))

write.csv(sm_ML_imp, file = path(output_dir,
                                 sm_ML_clean_fn,
                                 ext = "csv"),
          row.names = FALSE)

################# Dual Dataset modalities ######################################

## MRI + quest ###
mri_quest_ML_imp <- mri_quest_data_imp |> 
  filter(! Study_ID %in% holdout_ids) 

# save #
mri_quest_ML_clean_fn <- paste0("MRI_Quest_ML_cleaned_IMP_Clinic_BP_dim_",
                                nrow(mri_quest_ML_imp),
                                "X",
                                ncol(mri_quest_ML_imp))

saveRDS(mri_quest_ML_imp, file = path(output_dir, 
                                      mri_quest_ML_clean_fn,
                                      ext = "rds"))

write.csv(mri_quest_ML_imp, file = path(output_dir,
                                        mri_quest_ML_clean_fn,
                                        ext = "csv"),
          row.names = FALSE)

## mri + clinic ###
mri_clinic_ML_imp <- mri_clinic_data_imp  |> 
  filter(! Study_ID %in% holdout_ids) 

# save #
mri_clinic_ML_clean_fn <- paste0("MRI_Clinic_ML_cleaned_IMP_Clinic_BP_dim_",
                                 nrow(mri_clinic_ML_imp),
                                 "X",
                                 ncol(mri_clinic_ML_imp))

saveRDS(mri_clinic_ML_imp, file = path(output_dir, 
                                       mri_clinic_ML_clean_fn,
                                       ext = "rds"))

write.csv(mri_clinic_ML_imp, file = path(output_dir,
                                         mri_clinic_ML_clean_fn,
                                         ext = "csv"),
          row.names = FALSE)

## mri + sm ###
mri_sm_ML_imp <- mri_sm_sum_data_imp |> 
  filter(! Study_ID %in% holdout_ids) 

# save #
mri_sm_ML_clean_fn <- paste0("MRI_SM_SUM_ML_cleaned_IMP_Clinic_BP_dim_",
                             nrow(mri_sm_ML_imp),
                             "X",
                             ncol(mri_sm_ML_imp))

saveRDS(mri_sm_ML_imp, file = path(output_dir, 
                                   mri_sm_ML_clean_fn,
                                   ext = "rds"))

write.csv(mri_sm_ML_imp, file = path(output_dir,
                                     mri_sm_ML_clean_fn,
                                     ext = "csv"),
          row.names = FALSE)

## quest + clinic ###
quest_clinic_ML_imp <- quest_clinic_imp |> 
  filter(! Study_ID %in% holdout_ids) 

# save #
quest_clinic_ML_clean_fn <- paste0("Quest_Clinic_ML_cleaned_IMP_Clinic_BP_dim_",
                                   nrow(quest_clinic_ML_imp),
                                   "X",
                                   ncol(quest_clinic_ML_imp))

saveRDS(quest_clinic_ML_imp, file = path(output_dir, 
                                         quest_clinic_ML_clean_fn,
                                         ext = "rds"))

write.csv(quest_clinic_ML_imp, file = path(output_dir,
                                           quest_clinic_ML_clean_fn,
                                           ext = "csv"),
          row.names = FALSE)

## quest + sm ###
quest_sm_ML_imp <- quest_sm_sum_imp |> 
  filter(! Study_ID %in% holdout_ids) 

# save #
quest_sm_ML_clean_fn <- paste0("Quest_SM_SUM_ML_cleaned_IMP_Clinic_BP_dim_",
                               nrow(quest_sm_ML_imp),
                               "X",
                               ncol(quest_sm_ML_imp))

saveRDS(quest_sm_ML_imp, file = path(output_dir, 
                                     quest_sm_ML_clean_fn,
                                     ext = "rds"))

write.csv(quest_sm_ML_imp, file = path(output_dir,
                                       quest_sm_ML_clean_fn,
                                       ext = "csv"),
          row.names = FALSE)

## clinic + sm summary ###
clinic_sm_ML_imp <- clinic_sm_sum_imp |> 
  filter(! Study_ID %in% holdout_ids) 

# save #
Clinic_sm_ML_clean_fn <- paste0("Clinic_SM_SUM_ML_cleaned_IMP_Clinic_BP_dim_",
                                nrow(clinic_sm_ML_imp),
                                "X",
                                ncol(clinic_sm_ML_imp))

saveRDS(clinic_sm_ML_imp, file = path(output_dir, 
                                      Clinic_sm_ML_clean_fn,
                                      ext = "rds"))

write.csv(clinic_sm_ML_imp, file = path(output_dir,
                                        Clinic_sm_ML_clean_fn,
                                        ext = "csv"),
          row.names = FALSE)

################# multi Dataset modalities #####################################

## mri + quest + clinic ###
mri_quest_clinic_ML_imp <- mri_quest_clinic_data_imp |> 
  filter(! Study_ID %in% holdout_ids) 

# save #
mri_quest_clinic_ML_clean_fn <- paste0("MRI_Quest_Clinic_ML_cleaned_IMP_Clinic_BP_dim_",
                                       nrow(mri_quest_clinic_ML_imp),
                                       "X",
                                       ncol(mri_quest_clinic_ML_imp))

saveRDS(mri_quest_clinic_ML_imp, file = path(output_dir, 
                                             mri_quest_clinic_ML_clean_fn,
                                             ext = "rds"))

write.csv(mri_quest_clinic_ML_imp, file = path(output_dir,
                                               mri_quest_clinic_ML_clean_fn,
                                               ext = "csv"),
          row.names = FALSE)

## mri + quest + sm ###
mri_quest_sm_ML_imp <- mri_quest_sm_sum_data_imp |> 
  filter(! Study_ID %in% holdout_ids) 

# save #
mri_quest_sm_ML_clean_fn <- paste0("MRI_Quest_SM_SUM_ML_cleaned_IMP_Clinic_BP_dim_",
                                   nrow(mri_quest_sm_ML_imp),
                                   "X",
                                   ncol(mri_quest_sm_ML_imp))

saveRDS(mri_quest_sm_ML_imp, file = path(output_dir, 
                                         mri_quest_sm_ML_clean_fn,
                                         ext = "rds"))

write.csv(mri_quest_sm_ML_imp, file = path(output_dir,
                                           mri_quest_sm_ML_clean_fn,
                                           ext = "csv"),
          row.names = FALSE)

## mri + clinic + sm summary ###
mri_clinic_sm_ML_imp <- mri_clinic_sm_sum_data_imp |> 
  filter(! Study_ID %in% holdout_ids) 

# save #
mri_clinic_sm_ML_clean_fn <- paste0("MRI_Clinic_SM_SUM_ML_cleaned_IMP_Clinic_BP_dim_",
                                    nrow(mri_clinic_sm_ML_imp),
                                    "X",
                                    ncol(mri_clinic_sm_ML_imp))

saveRDS(mri_clinic_sm_ML_imp, file = path(output_dir, 
                                          mri_clinic_sm_ML_clean_fn,
                                          ext = "rds"))

write.csv(mri_clinic_sm_ML_imp, file = path(output_dir,
                                            mri_clinic_sm_ML_clean_fn,
                                            ext = "csv"),
          row.names = FALSE)

## quest + clinic + sm summary ###
quest_clinic_sm_ML_imp <- quest_clinic_sm_sum_data_imp |> 
  filter(! Study_ID %in% holdout_ids) 

# save #
quest_clinic_sm_ML_clean_fn <- paste0("Quest_Clinic_SM_SUM_ML_cleaned_IMP_Clinic_BP_dim_",
                                      nrow(quest_clinic_sm_ML_imp),
                                      "X",
                                      ncol(quest_clinic_sm_ML_imp))

saveRDS(quest_clinic_sm_ML_imp, file = path(output_dir, 
                                            quest_clinic_sm_ML_clean_fn,
                                            ext = "rds"))

write.csv(quest_clinic_sm_ML_imp, file = path(output_dir,
                                              quest_clinic_sm_ML_clean_fn,
                                              ext = "csv"),
          row.names = FALSE)

##################### All data modalities ######################################

## mri + quest + clinic + sm summary ###
mri_quest_clinic_sm_ML_imp <- mri_quest_clinic_sm_sum_data_imp |> 
  filter(! Study_ID %in% holdout_ids) 

# save #
mri_quest_clinic_sm_ML_clean_fn <- paste0("MRI_Quest_Clinic_SM_SUM_ML_cleaned_IMP_Clinic_BP_dim_",
                                          nrow(mri_quest_clinic_sm_ML_imp),
                                          "X",
                                          ncol(mri_quest_clinic_sm_ML_imp))

saveRDS(mri_quest_clinic_sm_ML_imp, file = path(output_dir, 
                                                mri_quest_clinic_sm_ML_clean_fn,
                                                ext = "rds"))

write.csv(mri_quest_clinic_sm_ML_imp, file = path(output_dir,
                                                  mri_quest_clinic_sm_ML_clean_fn,
                                                  ext = "csv"),
          row.names = FALSE)

## sample used for testing most robust and important features ##
hold_out_sample <- mri_quest_clinic_sm_sum_data_imp |> 
  filter(Study_ID %in% holdout_ids) 

# save #
holdout_ML_clean_fn <- paste0("Holdout_ALL_ML_IMP_Clinic_BP_dim_",
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
## Pre-Processing berlin_back_data from Berlin Back Study
#Including: Extraction, Renaming, Calculation of Scores for e.g. Questionnaires, Cleaning
#Requires: Path to Excel file with all berlin_back_data (File_berlin_back_data_RAW)
#Requires: Path to output file to store results

library("readxl") # read in excel xlsx file with German Umlaut characters e.g. ä
library("dplyr") # berlin_back_data frame manipulation
library("fs") # file structure manipulation
library("tidyr")
library("tibble") # for glimpse() to provide nice overview of berlin_back_data frames for checking

# Set working directory where RAW berlin_back_data is located and processed berlin_back_data will be written #
wd <- ".../Berlin_Back/"
setwd(wd)

#####---------------- RAW data location ----------------------------------######
# RAW berlin_back_data location #
berlin_back_path <- path_wd("01_Data", "2024-03_05_XPORT_ALL", ext = "xlsx")

#####-------------------- Output file names -------------------------------##### 

# create preprocessed output dir #
preproc_dir <- path_wd("01_Data", "preproc")
if (!dir_exists(preproc_dir)) {
  dir_create(preproc_dir) } else {
    print(paste(preproc_dir, "already exists"))
}

# Preprocessed csv file out put file name #
preproc_output_csv <- path(preproc_dir,
                           paste("preprocessed_ALL", Sys.Date(), sep = "_"), 
                           ext = "csv") 

preproc_output_rds <- path(preproc_dir,
                           paste("preprocessed_ALL", Sys.Date(), sep = "_"), 
                           ext = "rds") # file format for loading in berlin_back_data to R 

# Raw data translated from German to English #
translate_output_csv <- path(preproc_dir,
                             paste("translated_ENG_ALL", Sys.Date(), sep = "_"),
                             ext = "csv")

translate_output_rds <- path(preproc_dir,
                             paste("translated_ENG_ALL", Sys.Date(), sep = "_"),
                             ext = "rds")

##### Read in and translate RAW data ######

# read in excel RAW berlin_back_data
# The next functions are only to translate some german to english in variables and 
# col names
berlin_back_raw <- readxl::read_xlsx(path = berlin_back_path,
                                      sheet = "GESAMT", # specify exact sheet
                                      na = "EMPTY") 

berlin_back_translate <- berlin_back_raw %>% # convert missing values to "EMPTY" chr
  select(-c(contains("date"), # rm date columns
            contains("datum"), # rm date columns
            starts_with("sp2_"), # not needed for our study
            contains("kommentar"), # clinician comments will not be analysed
            contains("notizen") # Gait measurement comments will not be analysed
            )
         ) %>%
  distinct(prob_id, .keep_all = TRUE) %>% # remove duplicate subjects using subject ID col
  mutate(
    across(everything(), ~replace(., . == "Empty", "NA")),
    across(everything(), ~replace(., . == "NO CHRON. LBP", "no_LBP")),
    across(everything(), ~replace(., . == "NO REC", NA)),
    across(everything(), ~replace(., . == "ERROR", NA)),
    #translate German words into English
    across(everything(), ~replace(., . == "Ja", "yes")),
    across(everything(), ~replace(., . == "ja", "yes")),
    across(everything(), ~replace(., . == "Nein", "no")),
    across(everything(), ~replace(., . == "nein", "no")),
    across(everything(), ~replace(., . == "stimmt", "true")),
    across(everything(), ~replace(., . == "stimmt nicht", "false")),
    across(everything(), ~replace(., . == "Stimmt", "true")),
    across(everything(), ~replace(., . == "Stimmt nicht", "false")),
    across(everything(), ~replace(., . == "In der Vergangenheit vor", "in_past")),
    across(everything(), ~replace(., . == "links", "left")),
    across(everything(), ~replace(., . == "f", "female")),
    across(everything(), ~replace(., . == "m", "male")),
    across(everything(), ~replace(., . == "F", "female")),
    across(everything(), ~replace(., . == "M", "male")),
    across(everything(), ~replace(., . == "Links", "left")),
    across(everything(), ~replace(., . == "rechts", "right")),
    across(everything(), ~replace(., . == "Rechts", "right")),
    across(everything(), ~replace(., . == "beidhändig", "both_hands")),
    across(everything(), ~replace(., . == "Beidhäandig", "both_hands")),
    across(everything(), ~replace(., . == "beidseitig", "both_sides")),
    across(everything(), ~replace(., . == "Beidseitig", "both_sides")),
    across(everything(), ~replace(., . == "Beidseits Positiv", "both_side_positive")),
    across(everything(), ~replace(., . == "Links Positiv", "left_positive")),
    across(everything(), ~replace(., . == "Rechts Positiv", "right_positive")),
    across(everything(), ~replace(., . == "Geradstand", "straight_stand")),
    across(everything(), ~replace(., . == "Geradestand", "straight_stand")),
    across(everything(), ~replace(., . == "Links > Rechts", "left_>_right")),
    across(everything(), ~replace(., . == "Rechts > Links", "right_>_left")),
    across(everything(), ~replace(., . == "Normoreflexie", "normoreflexia")),
    across(everything(), ~replace(., . == "Areflexie", "areflexia")),
    across(everything(), ~replace(., . == "Hyperreflexie", "hyper_reflexia")),
    across(everything(), ~replace(., . == "Hyporeflexie", "hypo_reflexia")),
    across(everything(), ~replace(., . == "flüssiges Gangbild", "fluid_gait")),
    across(everything(), ~replace(., . == "hinkendes Gangbild", "limping_gait")),
    across(everything(), ~replace(., . == "Steppergang", "step_gait")),
    across(everything(), ~replace(., . == "Hypermobil", "hyper_mobile")),
    across(everything(), ~replace(., . == "Hypomobil", "hypo_mobile")),
    across(everything(), ~replace(., . == "normale Mobilität", "normal_mobility")),
    across(everything(), ~replace(., . == "Flachrücken", "flat_back")),
    across(everything(), ~replace(., . == "Hyperkyphose", "hyperkyphosis")),
    across(everything(), ~replace(., . == "Hyperlordose", "hyperlordosis")),
    across(everything(), ~replace(., . == "Normalbefund", "normal_finding")),
    across(everything(), ~replace(., . == "Skoliose", "scoliosis")),
    across(everything(), ~replace(., . == "Fußaußenkante", "foot_outer_edge")),
    across(everything(), ~replace(., . == "Fußsole", "foot_sole")),
    across(everything(), ~replace(., . == "Genitalbereich", "genitals")),
    across(everything(), ~replace(., . == "Oberschenkel", "thigh")),
    across(everything(), ~replace(., . == "Unterschenkel", "lower_leg")),
    across(everything(), ~replace(., . == "Becken", "pelvis")),
    across(everything(), ~replace(., . == "Zehen", "toes")),
    across(everything(), ~replace(., . == "Lumbal", "lumbar")),
    across(everything(), ~replace(., . == "Thorakal", "thoracic")),
    across(everything(), ~replace(., . == "Zervikal", "cervical")),
    across(everything(), ~replace(., . == "Mehrere Rückenpartien", "several_back_sections")),
    across(everything(), ~replace(., . == "Positiv", "positive")),
    across(everything(), ~replace(., . == "positiv", "positive")),
    across(everything(), ~replace(., . == "Negativ", "negative")),
    across(everything(), ~replace(., . == "negativ", "negative")),
    across(everything(), ~replace(., . == "Männlich", "male")),
    across(everything(), ~replace(., . == "männlich", "male")),
    across(everything(), ~replace(., . == "Weiblich", "female")),
    across(everything(), ~replace(., . == "weiblich", "female")),
    across(everything(), ~replace(., . == "Divers", "non_binary")),
    across(everything(), ~replace(., . == "divers", "non_binary")),
    across(everything(), ~replace(., . == "gesamte BWS", "whole_thoracic_spine")),
    across(everything(), ~replace(., . == "obere BWS", "upper_thoracic_spine")),
    across(everything(), ~replace(., . == "mittlere BWS", "middle_thoracic_spine")),
    across(everything(), ~replace(., . == "untere BWS", "lower_thoracic_spine")),
    across(everything(), ~replace(., . == "gesamte LWS", "whole_lumbar_spine")),
    across(everything(), ~replace(., . == "mittlere LWS", "middle_lumbar_spine")),
    across(everything(), ~replace(., . == "untere LWS", "lower_lumbar_spine")),
    across(everything(), ~replace(., . == "Hüfte", "hip")),
    across(everything(), ~replace(., . == "HWS", "cervical_spine")),
    across(everything(), ~replace(., . == "Iliosakralgelenk", "sacroiliac_joint")),
    across(everything(), ~replace(., . == "Sakrum", "sacrum")),
    across(everything(), ~replace(., . == "fortschreitend nach vorhergehenden Kreuzschmerzen", "pregressive_after_past_LBP")),
    across(everything(), ~replace(., . == "langsam zunehmend", "slowly_increasing")),
    across(everything(), ~replace(., . == "nach einem Unfall", "after_accident")),
    across(everything(), ~replace(., . == "nach einer Fehlbewegung", "after_wrong_movement")),
    across(everything(), ~replace(., . == "plötzlich nach einer Anstrengung einsetzend", "sudden_after_hard_exertion")),
    across(everything(), ~replace(., . == "Immer gleich stark", "always_equally_intense")),
    across(everything(), ~replace(., . == "Nachts", "nights")),
    across(everything(), ~replace(., . == "nur Nachts", "only_nights")),
    across(everything(), ~replace(., . == "Tags", "days")),
    across(everything(), ~replace(., . == "nur Tags", "only_days")),
    across(everything(), ~replace(., . == "Nachts > Tags", "nights_>_days")),
    across(everything(), ~replace(., . == "Tags > Nachts", "days_>_nights")),
    across(everything(), ~replace(., . == "Bandscheiben - Prolaps", "IVD_prolapse")),
    across(everything(), ~replace(., . == "Bandscheiben - Protrusion", "IVD_protrusion")),
    across(everything(), ~replace(., . == "Bandscheiben - Sequester", "IVD_sequestrum")),
    across(everything(), ~replace(., . == "Hüfte-Links", "hip_left")),
    across(everything(), ~replace(., . == "Hüfte-Rechts", "hip_right")),
    across(everything(), ~replace(., . == "Hüfte beidseitig", "hip_both_sides")),
    across(everything(), ~replace(., . == "Hüfte-beidseitig", "hip_both_sides")),
    across(everything(), ~replace(., . == "Multiple Diagnose Wirbelsäule", "multiple_spine_diagnoses")),
    across(everything(), ~replace(., . == "Osteochrondrose", "osteochrondrosis")),
    across(everything(), ~replace(., . == "Skoliose", "Scoliosis")),
    across(everything(), ~replace(., . == "Spinalkanalstenose", "spinal_stenosis")),
    across(everything(), ~replace(., . == "Spondylarthrose", "spondyloarthritis")),
    across(everything(), ~replace(., . == "Spondylolisthese", "spondylolisthesis")),
    across(everything(), ~replace(., . == "Wirbelkörperfraktur", "vertebral_body_fracture")),
    across(everything(), ~replace(., . == "Bandscheiben-OP", "IVD_OP")),
    across(everything(), ~replace(., . == "Fuß-beidseitig", "both_feet")),
    across(everything(), ~replace(., . == "Fuß-Links", "foot_left")),
    across(everything(), ~replace(., . == "Fuß-Rechts", "foot_right")),
    across(everything(), ~replace(., . == "Knie-beidseitig", "knee_both_sides")),
    across(everything(), ~replace(., . == "Knie-Links", "knee_left")),
    across(everything(), ~replace(., . == "Knie-Rechts", "knee_right")),
    across(everything(), ~replace(., . == "Multiple Vor-Ops", "multiple_OPs")),
    across(everything(), ~replace(., . == "Skoliose", "Scoliosis")),
    across(everything(), ~replace(., . == "Spondylodesen", "spondylodesis")),
    across(everything(), ~replace(., . == "Sprunggelenk-beidseitig", "ankle_both_sides")),
    across(everything(), ~replace(., . == "Sprunggelenk-Links", "ankle_left")),
    across(everything(), ~replace(., . == "Sprunggelenk-Rechts", "ankle_right")),
    across(everything(), ~replace(., . == "10 oder mehr", ">=10")),
    across(everything(), ~replace(., . == "nie", "never")),
    across(everything(), ~replace(., . == "seltener als einmal pro Monat", "less_than_once_month")),
    across(everything(), ~replace(., . == "einmal pro Woche", "once_per_week")),
    across(everything(), ~replace(., . == "einmal pro Monat", "once_per_month")),
    across(everything(), ~replace(., . == "viermal oder öfters pro Woche", ">=4_per_week")),
    across(everything(), ~replace(., . == "zwei- bis dreimal pro Woche", "2-3_per_week")),
    across(everything(), ~replace(., . == "zwei- bis viermal im Monat", "2-4_per_month")),
    across(everything(), ~replace(., . == "uneingeschränkt", "unrestricted")),
    across(everything(), ~replace(., . == "nicht arbeitend", "unemployed")),
    across(everything(), ~replace(., . == "Vibrationen", "vibrations")),
    across(everything(), ~replace(., . == "geringe Belastung", "low_load")),
    across(everything(), ~replace(., . == "moderate Belastung", "moderate_load")),
    across(everything(), ~replace(., . == "schwere Belastung", "heavy_load")),
    across(everything(), ~replace(., . == "Vibrationen + schwere Belastung", "vibrations_heavy_load")),
    across(everything(), ~replace(., . == "Sitzen", "sitting")),
    across(everything(), ~replace(., . == "Sitzen + Stehen", "sitting_standing")),
    across(everything(), ~replace(., . == "Stehen", "standing")),
    across(everything(), ~replace(., . == "Stehen + Gehen", "standing_walking")),
    across(everything(), ~replace(., . == "Arbeitslos", "unemployed")),
    across(everything(), ~replace(., . == "Bau, Architektur, Vermessung", "construction_architecture_surveying")),
    across(everything(), ~replace(., . == "Berufsunfähig/Berentung", "occupational_disability/retirement")),
    across(everything(), ~replace(., . == "Dienstleistung", "service")),
    across(everything(), ~replace(., . == "Elektro", "electric")),
    across(everything(), ~replace(., . == "Gesundheit", "health")),
    across(everything(), ~replace(., . == "IT, Computer", "IT")),
    across(everything(), ~replace(., . == "Kunst, Kultur, Gestaltung", "art_culture_design")),
    across(everything(), ~replace(., . == "Landwirtschaft, Natur, Umwelt", "agriculture_nature_environment")),
    across(everything(), ~replace(., . == "Medien", "media")),
    across(everything(), ~replace(., . == "Metall, Maschinenbau", "metal_mechanical_engineering")),
    across(everything(), ~replace(., . == "Naturwissenschaften", "natural_sciences")),
    across(everything(), ~replace(., . == "Produktion, Fertigung", "manufacturing")),
    across(everything(), ~replace(., . == "Schüler", "secondary_student")),
    across(everything(), ~replace(., . == "Soziales, Pädagogik", "social_pedagogy")),
    across(everything(), ~replace(., . == "Student", "tertiary_student")),
    across(everything(), ~replace(., . == "Technik, Technologiefelder", "technology")),
    across(everything(), ~replace(., . == "Verkehr, Logistik", "transport_logistics")),
    across(everything(), ~replace(., . == "Wirtschaft, Verwaltung", "economy_administration")),
    across(everything(), ~replace(., . == "kaum", "hardly")),
    across(everything(), ~replace(., . == "maßgeblich", "significantly")),
    across(everything(), ~replace(., . == "moderat", "moderate")),
    across(everything(), ~replace(., . == "nicht", "not")),
    across(everything(), ~replace(., . == "Nein, und ich habe es auch nicht vor.", "no_not_intend_to")),
    across(everything(), ~replace(., . == "Nein, aber ich denke darüber nach, aktiver zu werden.", "no_but_thinking_to")),
    across(everything(), ~replace(., . == "Nein, aber ich habe es mir fest vorgenommen.", "no_but_I_want_to")),
    across(everything(), ~replace(., . == "Ja, aber es fällt mir schwer.", "yes_but_difficult")),
    across(everything(), ~replace(., . == "Ja, und es fällt mir sehr leicht.", "yes_and_easy")),
    across(everything(), ~replace(., . == "allein", "alone")),
    across(everything(), ~replace(., . == "Familie", "family")),
    across(everything(), ~replace(., . == "Wohngemeinschaft", "shared_apartment")),
    across(everything(), ~replace(., . == "Beckenrückkippung", "pelvic_back_tilt")),
    across(everything(), ~replace(., . == "Beckenvorkippung", "pelvic_forward_tilt")),
    across(everything(), ~replace(., . == "ausgezeichnet", "excellent")),
    across(everything(), ~replace(., . == "gut", "good")),
    across(everything(), ~replace(., . == "sehr gut", "very_good")),
    across(everything(), ~replace(., . == "weniger gut", "less_good")),
    across(everything(), ~replace(., . == "schlecht", "bad")),
    across(everything(), ~replace(., . == "gar nicht", "not_at_all")),
    across(everything(), ~replace(., . == "< 100 m", "<_100m")),
    across(everything(), ~replace(., . == "100 - 500 m", "100-500m")),
    across(everything(), ~replace(., . == "500 - 1000 m", "500-1000m")),
    across(everything(), ~replace(., . == "1000 - 2000 m", "1000-2000m")),
    across(everything(), ~replace(., . == "Jahre", "years")),
    across(everything(), ~replace(., . == "Monate", "months")),
    across(everything(), ~replace(., . == "Wochen", "weeks")),
    across(everything(), ~replace(., . == "Tage", "days")),
    across(everything(), ~replace(., . == "Mehr oder weniger einverstanden", "more_or_less_agree")),
    across(everything(), ~replace(., . == "Mehr oder weniger nicht einverstanden", "more_or_less_disagree")),
    across(everything(), ~replace(., . == "Überhaupt nicht einverstanden", "not_agree_at_all")),
    across(everything(), ~replace(., . == "Völlig einverstanden", "totally_agree")),
    across(everything(), ~replace(., . == "1-Überhaupt nicht", "1-not_at_all")),
    across(everything(), ~replace(., . == "2-An einzelnen Tagen", "2-on_single_days")),
    across(everything(), ~replace(., . == "3-An mehr als der Hälfte der Tage", "3-on_more_than_half_of_days")),
    across(everything(), ~replace(., . == "4-Beinahe jeden Tag", "4-almost_every_day")),
    across(everything(), ~replace(., . == "1-Nie", "1-nerver")),
    across(everything(), ~replace(., . == "2-Selten", "2-rare")),
    across(everything(), ~replace(., . == "3-Manchmal", "3-sometimes")),
    across(everything(), ~replace(., . == "4-Häufig", "4-frequently")),
    across(everything(), ~replace(., . == "5-Sehr oft", "5-very_often")),
    across(everything(), ~replace(., . == "Überhaupt nicht", "not_at_all")),
    across(everything(), ~replace(., . == "1-trifft überhaupt nicht zu", "1-does_not_apply_at_all")),
    across(everything(), ~replace(., . == "2-trifft nicht zu", "2-does_not_apply")),
    across(everything(), ~replace(., . == "3-trifft eher nicht zu", "3-rather_not_true")),
    across(everything(), ~replace(., . == "4-trifft eher zu", "4-rather_true")),
    across(everything(), ~replace(., . == "5-trifft zu", "5-applies")),
    across(everything(), ~replace(., . == "6-trifft voll und ganz zu", "6-totally_applies")),
    across(everything(), ~replace(., . == "Berufsunfähigkeit", "occupational_diability")),
    across(everything(), ~replace(., . == "Erreichen der Altersgrenze", "reach_age_limit")),
    across(everything(), ~replace(., . == "Erwerbsunfähigkeit", "earning_capacity_disability")),
    across(everything(), ~replace(., . == "Teilweise Erwerbsminderung", "partial_earning_reduction_capacity")),
    across(everything(), ~replace(., . == "Volle Erwerbsminderung", "full_reduction_earning_capacity")),
    across(everything(), ~replace(., . == "Vorgezogenes Altersruhegeld", "early_retirement")),
    across(everything(), ~replace(., . == "Witwen- oder Waisenrente", "widow_orphan_pension")),
    across(everything(), ~replace(., . == "Dauerschmerzen mit leichten Schwankungen", "continuous_pain_slight_fluctuations")),
    across(everything(), ~replace(., . == "Dauerschmerzen mit starken Schwankungen", "continuous_pain_strong_fluctuations")),
    across(everything(), ~replace(., . == "Hatte in den letzten 12 Wochen keine Schmerzen", "no_pain_last_12_weeks")),
    across(everything(), ~replace(., . == "Schmerzattacken auch dazwischen Schmerzen", "pain_attackes_in_between_pain")),
    across(everything(), ~replace(., . == "Schmerzattacken dazwischen schmerzfrei", "pain_attacks_in_between_pain_free")),
    across(everything(), ~replace(., . == "1-mehrfach täglich", "1-multiple_daily")),
    across(everything(), ~replace(., . == "2-einmal täglich", "2-once_daily")),
    across(everything(), ~replace(., . == "3-mehrfach wöchentlich", "3-muliple_weekly")),
    across(everything(), ~replace(., . == "4-einmal wöchentlich", "4-once_weekly")),
    across(everything(), ~replace(., . == "5-mehrfach monatlich", "5-multiple_monthly")),
    across(everything(), ~replace(., . == "6-einmal monatlich", "6-once_monthly")),
    across(everything(), ~replace(., . == "bis zu drei Tagen ", "up_to_3_days")),
    across(everything(), ~replace(., . == "länger als drei Tage", "longer_than_3_days")),
    across(everything(), ~replace(., . == "Minuten", "minutes")),
    across(everything(), ~replace(., . == "Sekunden", "seconds")),
    across(everything(), ~replace(., . == "Stunden", "hours")),
    ) %>%
  # translate column names to English #
  rename(Study_ID = prob_id) %>%
  rename_with(., ~sub(pattern = "sp1_klin", replacement = "Clinic", .x)) %>%
  rename_with(., ~sub(pattern = "sp_1frag_", replacement = "Quest", .x)) %>%
  rename_with(., ~sub(pattern = "sp1_frag", replacement = "Quest", .x)) %>%
  rename_with(., ~sub(pattern = "sp1_mrt", replacement = "MRI", .x)) %>%
  rename_with(., ~sub(pattern = "sp1_gang", replacement = "Gait", .x)) %>%
  rename_with(., ~sub(pattern = "sp1_sm", replacement = "SM", .x)) %>% # spine mouse data
  rename_with(., ~gsub(pattern = " ",replacement = "_", .x)) %>% # is spine mouse change spaces to underscores
  rename_with(., ~sub("^(.*?):_*(SM_*.+)", "\\2_\\L\\1", .x, perl = TRUE)) %>% # move "SM" and what spine section after to the front of the col name
  rename_with(., ~sub(pattern = "lws", replacement = "lumbar", .x)) %>%
  rename_with(., ~sub(pattern = "bws", replacement = "thoracic", .x)) %>%
  rename_with(., ~sub(pattern = "sak", replacement = "sacral", .x)) %>%
  rename_with(., ~sub(pattern = "beruf_", replacement = "employment", .x)) %>%
  rename_with(., ~sub(pattern = "thera_", replacement = "therapy_", .x)) %>%
  rename_with(., ~sub(pattern = "sf36_koefu", replacement = "sf36_physical_function", .x)) %>%
  rename_with(., ~sub(pattern = "sf36_koero", replacement = "sf36_physical_role_function", .x)) %>%
  rename_with(., ~sub(pattern = "sf36_schm", replacement = "sf36_physical_pain", .x)) %>%
  rename_with(., ~sub(pattern = "sf36_ages", replacement = "sf36_health_perception", .x)) %>%
  rename_with(., ~sub(pattern = "sf36_vita", replacement = "sf36_vitality", .x)) %>%
  rename_with(., ~sub(pattern = "sf36_sofu", replacement = "sf36_social_function", .x)) %>%
  rename_with(., ~sub(pattern = "sf36_emro", replacement = "sf36_emotional_role_function", .x)) %>%
  rename_with(., ~sub(pattern = "sf36_psyc", replacement = "sf36_psychological_well_being", .x)) %>%
  rename_with(., ~sub(pattern = "frontal_sitzend_aufrecht", replacement = "frontal_sit_straight", .x)) %>%
  rename_with(., ~sub(pattern = "frontal_sitzend_links", replacement = "frontal_sit_left", .x)) %>%
  rename_with(., ~sub(pattern = "frontal_sitzend_rechts", replacement = "frontal_sit_right", .x)) %>%
  rename_with(., ~sub(pattern = "frontal_stehend_aufrecht", replacement = "frontal_stand_straight", .x)) %>%
  rename_with(., ~sub(pattern = "frontal_stehend_links", replacement = "frontal_stand_left", .x)) %>%
  rename_with(., ~sub(pattern = "frontal_stehend_rechts", replacement = "frontal_stand_right", .x)) %>%
  rename_with(., ~sub(pattern = "sagittal_sitzend_aufrecht", replacement = "sagittal_sit_straight", .x)) %>%
  rename_with(., ~sub(pattern = "sagittal_sitzend_extension", replacement = "sagittal_sit_extension", .x)) %>%
  rename_with(., ~sub(pattern = "sagittal_sitzend_flexion", replacement = "sagittal_sit_flexion", .x)) %>%
  rename_with(., ~sub(pattern = "sagittal_stehend_aufrecht", replacement = "sagittal_stand_straight", .x)) %>%
  rename_with(., ~sub(pattern = "sagittal_stehend_extension", replacement = "sagittal_stand_extension", .x)) %>%
  rename_with(., ~sub(pattern = "sagittal_stehend_flexion", replacement = "sagittal_stand_flexion", .x)) %>%
  # translate the clinical assessment col name to English #
  rename("Demo_age" = "Clinic_alter",
         "Demo_sex" = "Clinic_geschlecht",
         "Study_included" = "Clinic_studieneinschluss",
         "Study_mri_included" = "Clinic_mrt",
         "Demo_clinic_LBP" = "Clinic_chron_rs",
         "Demo_LBP_past_pain_duration" = "Clinic_chron_rs_verg_t1",
         "Demo_LBP_past_pain_duration_scale" = "Clinic_chron_rs_verg_t2",
         "Demo_LBP_pain_duration" = "Clinic_wie_lange_rs_t1",
         "Demo_LBP_pain_duration_scale" = "Clinic_wie_lange_rs_t2",
         "Clinic_pain_primary_location" = "Clinic_lok_primaer",
         "Clinic_pain_secondary_location" = "Clinic_lok_sekundaer",
         "Clinic_current_pain" = "Clinic_moment_schmerz",
         "Clinic_pain_begin" = "Clinic_schmerz_beginn",
         "Clinic_pain_coughing_sneezing" = "Clinic_husten_niesen",
         "Clinic_pain_standing" = "Clinic_stehen",
         "Clinic_pain_walking" = "Clinic_gehen",
         "Clinic_pain_sitting" = "Clinic_sitzen",
         "Clinic_pain_resting" = "Clinic_ruheschmerz",
         "Clinic_pain_day_diff" = "Clinic_tag_unterschied",
         "Clinic_pain_when_strongest" = "Clinic_wann_stark",
         "Clinic_pain_arm" = "Clinic_armsch",
         "Clinic_pain_leg" = "Clinic_beinschmerz",
         "Clinic_prior_general_disease" = "Clinic_allg_krankheitsgef",
         "Clinic_pain_walking_distance" = "Clinic_gehstrecke",
         "Clinic_pain_movment_limit" = "Clinic_bew_einschr",
         "Clinic_pain_sensitivity" = "Clinic_sensibilitaet",
         "Clinic_pain_paralysis" = "Clinic_laehmung",
         "Clinic_pain_paralysis_location" = "Clinic_laehm_lok",
         "Clinic_sensory_symptom" = "Clinic_sens_symp",
         "Clinic_sensory_deficit" = "Clinic_sens_defizi",
         "Clinic_sensory_location" = "Clinic_sens_lok",
         "Clinic_prior_diagnosis" = "Clinic_vor_diag",
         "Clinic_prior_diagnosis_which" = "Clinic_vor_diag_welche",
         "Clinic_prior_diagnosis_back_level" = "Clinic_vor_diag_etage",
         "Clinic_prior_surgeries" = "Clinic_vor_ops",
         "Clinic_prior_surgeries_which" = "Clinic_vor_ops_welche",
         "Clinic_prior_surgeries_spine_level" = "Clinic_vor_ops_etage",
         "Clinic_question_physical_activity" = "Clinic_koerp_aktiv",
         "Clinic_question_covid19_activity" = "Clinic_covid19",
         "Clinic_question_alcohol_freq" = "Clinic_alkohol",
         "Clinic_question_alcohol_glasses" = "Clinic_glaeser",
         "Clinic_question_alcohol_restrict" = "Clinic_alk_einschr",
         "Clinic_question_smoking" = "Clinic_rauchen",
         "Clinic_question_smoking_pack_years" = "Clinic_packjahre",
         "Clinic_medication_1_registration" = "Clinic_med_reg",
         "Clinic_medication_1_dosis_mg" = "Clinic_dosierung_in_mg",
         "Clinic_medication_1_morning" = "Clinic_frueh",
         "Clinic_medication_1_midday" = "Clinic_mittags",
         "Clinic_medication_1_noon" = "Clinic_abends",
         "Clinic_medication_2_registration" = "Clinic_med_2tes",
         "Clinic_medication_2_dosis_mg" = "Clinic_2tes_dosierung_in_mg",
         "Clinic_medication_2_morning" = "Clinic_2tes_frueh",
         "Clinic_medication_2_midday" = "Clinic_2tes_mittags",
         "Clinic_medication_2_noon" = "Clinic_2tes_abends",
         "Clinic_medication_1_demand" = "Clinic_med_bedarf",
         "Clinic_medication_1_demand_dosis_mg" = "Clinic_bedarf_dosierung_in_mg",
         "Clinic_medication_1_demand_month" = "Clinic_bedarf_pro_monat",
         "Clinic_medication_2_demand" = "Clinic_med_bedarf_2tes",
         "Clinic_medication_2_demand_dosis_mg" = "Clinic_bedarf_2tes_dosierung",
         "Clinic_medication_2_demand_month" = "Clinic_bedarf_2tes_pro_monat",
         "Clinic_question_family_BP" = "Clinic_familie_rs",
         "Clinic_question_job_field" = "Clinic_berufsfeld",
         "Clinic_question_job_posture" = "Clinic_charakt",
         "Clinic_question_job_posture_duration" = "Clinic_charakt_h_t",
         "Clinic_question_job_load_1" = "Clinic_belastung1",
         "Clinic_question_job_load_1_duration" = "Clinic_belastung1_h_t",
         "Clinic_question_job_load_2" = "Clinic_belastung2",
         "Clinic_question_job_load_2_duration" = "Clinic_belastung2_h_t",
         "Clinic_question_job_duration_years" = "Clinic_taetig_seit",
         "Clinic_question_pyschological_stress" = "Clinic_psycho_soz_bel",
         "Clinic_question_living_situation" = "Clinic_wohnsituation",
         "Clinic_question_family_conflicts" = "Clinic_fam_konfl",
         "Clinic_question_body_self_assessment" = "Clinic_selbsteinsch",
         "Clinic_body_cervical_incl" = "Clinic_hws_inkl",
         "Clinic_body_cervical_recl" = "Clinic_hws_rekl",
         "Clinic_body_chin_sternum_dist" = "Clinic_kinn_sternum",
         "Clinic_body_cervical_lat_bend_left" = "Clinic_hws_sn_links",
         "Clinic_body_cervical_lat_bend_right" = "Clinic_hws_sn_rechts",
         "Clinic_body_cervical_axial_rotate_left" = "Clinic_hws_ar_links",
         "Clinic_body_cervical_axial_rotate_right" = "Clinic_hws_ar_rechts",
         "Clinic_body_thoracic_lumbar_incl" = "Clinic_thoracic_lumbar_inkl",
         "Clinic_body_thoracic_lumbar_recl" = "Clinic_thoracic_lumbar_rekl",
         "Clinic_motion_finger_floor_dist" = "Clinic_finger_boden_abstand",
         "Clinic_body_hip_flexion" = "Clinic_hueft_flexion",
         "Clinic_body_thoracic_lumbar_lat_bend_left" = "Clinic_thoracic_lumbar_sn_links",
         "Clinic_body_thoracic_lumbar_lat_bend_right" = "Clinic_thoracic_lumbar_sn_rechts",
         "Clinic_body_thoracic_lumbar_ar_left" = "Clinic_thoracic_lumbar_ar_links",
         "Clinic_body_thoracic_lumbar_ar_right" = "Clinic_thoracic_lumbar_ar_rechts",
         "Clinic_motion_ott" = "Clinic_ott",
         "Clinic_motion_shober" = "Clinic_schober",
         "Clinic_body_square_shoulders" = "Clinic_schultergeradestand",
         "Clinic_body_plump_line" = "Clinic_proc_spinosi_lotgerecht",
         "Clinic_body_rib_hump" = "Clinic_rippenbuckel",
         "Clinic_body_lumbar_bulge" = "Clinic_lendenwulst",
         "Clinic_body_asym_waist_triangle" = "Clinic_asym_taillendreieck",
         "Clinic_body_back_form" = "Clinic_rueckenform",
         "Clinic_body_roussoly_type" = "Clinic_roussoly_typ",
         "Clinic_motion_sagittal_balance" = "Clinic_sagittale_balance",
         "Clinic_motion_coronal_balance" = "Clinic_koronare_balance",
         "Clinic_body_back_head_wall_distance" = "Clinic_hinterkopf_wand_abstand",
         "Clinic_motion_rigid_muscle" = "Clinic_muskul_hartspann",
         "Clinic_pain_pressure" = "Clinic_druckschmerz",
         "Clinic_pain_knocking" = "Clinic_klopfschmerz",
         "Clinic_pain_spinous_process_shake" = "Clinic_dornfortsatzruettelschmerz",
         "Clinic_pain_mennell_sign" = "Clinic_menell_zeichen",
         "Clinic_pain_jumping" = "Clinic_springing_test_schmerz",
         "Clinic_motion_mobility" = "Clinic_beweglichkeit",
         "Clinic_body_toe_stand" = "Clinic_zehenspitzenabstand",
         "Clinic_body_heel_stand" = "Clinic_hackenstand",
         "Clinic_pain_heel_drop" = "Clinic_fersenfallschmerz",
         "Clinic_motion_trendelenburg_sign" = "Clinic_trendelenburg",
         "Clinic_motion_straight_leg_raise" = "Clinic_lasegue",
         "Clinic_motion_pseudo_lasegue_l" = "Clinic_pseudo_lasegue_l",
         "Clinic_motion_pseudo_lasegue_r" = "Clinic_pseudo_lasegue_r",
         "Clinic_motion_gait_pattern" = "Clinic_gangbild",
         "Clinic_motion_reflex_quad_l" = "Clinic_quad_sehnenreflex_l",
         "Clinic_motion_reflex_quad_r" = "Clinic_quad_sehnenreflex_r",
         "Clinic_motion_reflex_achilles_l" = "Clinic_achillessehnenreflex_l",
         "Clinic_motion_reflex_achilles_r" = "Clinic_achillessehnenreflex_r",
         "Clinic_pain_hip" = "Clinic_hueftschmerz",
         "Clinic_motion_hip_flexion_l" = "Clinic_h_flex_l",
         "Clinic_motion_hip_flexion_r" = "Clinic_h_flex_r",
         "Clinic_motion_hip_extension_l" = "Clinic_h_ex_l",
         "Clinic_motion_hip_extension_r" = "Clinic_h_ex_r",
         "Clinic_motion_hip_external_rotate_l" = "Clinic_h_ar_l",
         "Clinic_motion_hip_external_rotate_r" = "Clinic_h_ar_r",
         "Clinic_motion_hip_internal_rotate_l" = "Clinic_h_ir_l",
         "Clinic_motion_hip_internal_rotate_r" = "Clinic_h_ir_r",
         "Clinic_motion_hip_abduction_l" = "Clinic_h_ab_l",
         "Clinic_motion_hip_abduction_r" = "Clinic_h_ab_r",
         "Clinic_motion_hip_adduction_l" = "Clinic_h_ad_l",
         "Clinic_motion_hip_adduction_r" = "Clinic_h_ad_r",
         "Clinic_motion_thomas_handle_l" = "Clinic_thomas_l",
         "Clinic_motion_thomas_handle_r" = "Clinic_thomas_r",
         "Clinic_body_pelvic_tilt_frontal" = "Clinic_beckenstellung_frontal",
         "Clinic_body_pelvic_tilt_sagittal" = "Clinic_beckenstellung_sagittal",
         "Clinic_motion_sit_to_stand_30sec" = "Clinic_sit_to_stand",
         "Clinic_heart_rate" = "Clinic_herzfrequ",
         "Clinic_breath_rate" = "Clinic_atemfrequ",
         "Clinic_heart_systolic_pres" = "Clinic_blut_sys",
         "Clinic_heart_diastolic" = "Clinic_blut_dias",
         "Demo_height" = "Clinic_groesse",
         "Demo_weight" = "Clinic_gewicht",
         "Demo_BMI" = "Clinic_bmi",
         "Demo_waist" = "Clinic_taille",
         "Demo_hip" = "Clinic_huefte",
         "Demo_WHR" = "Clinic_whr",
         "Demo_leg_l" = "Clinic_bein_l",
         "Demo_leg_r" = "Clinic_bein_r",
         "Demo_r_l_ratio" = "Clinic_r_l",
         "Clinic_3_SM_measure" = "Clinic_3wdh", 
         "Demo_question_LBP" = "Quest_rs_1",
         "Quest_employment_currently" = "Quest_employment1",
         "Quest_employment_unable_work" = "Quest_employment2",
         "Quest_employment_unable_work_could_return" = "Quest_employment3",
         "Quest_employment_unable_work_last_3_months" = "Quest_employment4",
         "Quest_employment_pension_application" = "Quest_employment5",
         "Quest_employment_pension_application_not_decided" = "Quest_employment6",
         "Quest_employment_pension_application_rejected" = "Quest_employment7",
         "Quest_employment_pension_application_opposition" = "Quest_employment8",
         "Quest_employment_pension" = "Quest_employment9",
         "Quest_employment_pension_for_time" = "Quest_employment10", 
         "Quest_employment_pension_final" = "Quest_employment11",
         "Quest_employment_pension_type" = "Quest_employment12",
         "Quest_employment_disability" = "Quest_employment13", 
         "Quest_employment_disability_grade" = "Quest_employment14",
         ) %>%
  relocate(starts_with("Demo_"), .after = "Study_included")

# save translated data #
write.csv(x = berlin_back_translate, file = translate_output_csv, row.names=FALSE)
saveRDS(object = berlin_back_translate, file = translate_output_rds)

##------------------------------------------------------------------------------
## DEMOGRAPHICS ----------------------------------------------------------------
##------------------------------------------------------------------------------

# First I will create a new data frame "berlin_back_data_D" to conduct all the 
# changes to the demographic data and then over convert in back to "berlin_back_data"
# and remove the "*_data_D". This is to break up the different changes and for checking
# while coding the changes -> This could be removed but may make the code easier to 
# understand

berlin_back_D <- berlin_back_translate %>%
# If the subject conducted the MRI & Gait analysis then they are included in the 
# study and this missing data is a tipping error
  mutate(
     Study_included = as.factor(case_when(
       !is.na(MRI_ivd_deg_l1_l2) & is.na(Study_included) ~ "yes",
       !is.na(Gait_head_height) & is.na(Study_included) ~ "yes",
       !is.na(Clinic_question_body_self_assessment) & is.na(Study_included) ~ "yes",
       !is.na(SM_th1_frontal_sit_straight) & is.na(Study_included) ~ "yes",
       .default = Study_included
     )),
    # Using Gait sex, age, and LBP cols fill in missing values for demographics
    Demo_age = as.integer(case_when(
      !is.na(Gait_age) & is.na(Demo_age) ~ Gait_age,
      .default = Demo_age
    )),
    Demo_sex = as.factor(case_when(
      !is.na(Gait_sex) & is.na(Demo_sex) ~ Gait_sex,
      Demo_sex == "non_binary" ~ Gait_sex, # we are only interested in biological sex
      .default = Demo_sex
    )),
    # again use gait test to fill in missing height and weight
    # For height as the Gait needs to be multiplied by 100 to change to cm we will
    # first covert Demo_height to an integer so both sides match when using 
    # case_when()
    Demo_height = as.integer(Demo_height),
    Demo_height = case_when(
      !is.na(Gait_height) & is.na(Demo_height) ~ as.double(Gait_height)*100,
      .default = Demo_height
    ),
    Demo_weight = as.integer(case_when(
      !is.na(Gait_weight) & is.na(Demo_weight) ~ Gait_weight,
      .default = Demo_weight
    )),
    # now replace the Demo_BMI values using the new Gait values
    # BMI needs to be changed to int for manipulation 
    Demo_BMI = round(as.double(Demo_BMI), 2),
    Demo_BMI = case_when(
      is.na(Demo_BMI) & !is.na(Demo_height) ~ round(Demo_weight/((Demo_height/100)^2), 2),
      .default = Demo_BMI
    ), 
    # one subject is missing weight and has a BMI=0. this is changed is a NA
    Demo_BMI = na_if(Demo_BMI, 0),
    Demo_WHR = round(as.double(Demo_WHR), 2),
    Demo_WHR = na_if(Demo_WHR, 0),
    Demo_r_l_ratio = round(as.double(Demo_r_l_ratio), 2),
    Demo_waist = as.double(Demo_waist),
    Demo_hip = as.integer(Demo_hip),
    Demo_leg_l = as.double(Demo_leg_l),
    Demo_leg_r = as.double(Demo_leg_r),
    # convert the pain duration cols into single weeks column
    Demo_LBP_pain_duration = if_else(Demo_LBP_pain_duration == "no_LBP", NA, Demo_LBP_pain_duration),
    Demo_LBP_pain_duration = as.integer(Demo_LBP_pain_duration),
    Demo_LBP_pain_duration = case_when(
      Demo_LBP_pain_duration_scale == "years" ~ Demo_LBP_pain_duration*52,
      Demo_LBP_pain_duration_scale == "months" ~ Demo_LBP_pain_duration*4,
      Demo_LBP_pain_duration_scale == "weeks" ~ Demo_LBP_pain_duration*1,
      is.na(Demo_LBP_pain_duration_scale) & !is.na(Demo_LBP_pain_duration) ~ 0,
      .default = Demo_LBP_pain_duration
    ),
    Demo_LBP_pain_duration = na_if(Demo_LBP_pain_duration, 0), # should be a better way to do this
    # convert the past pain duration cols into single days column
    # there is possibly something wrong with the past pain duration
    Demo_LBP_past_pain_duration = if_else(Demo_LBP_past_pain_duration == "no_LBP", NA, Demo_LBP_past_pain_duration),
    Demo_LBP_past_pain_duration = as.integer(Demo_LBP_past_pain_duration),
    Demo_LBP_past_pain_duration = case_when(
      Demo_LBP_past_pain_duration_scale == "years" ~ Demo_LBP_past_pain_duration * 365,
      Demo_LBP_past_pain_duration_scale == "months" ~ Demo_LBP_past_pain_duration * 30,
      Demo_LBP_past_pain_duration_scale == "weeks" ~ Demo_LBP_past_pain_duration * 7,
      Demo_LBP_past_pain_duration_scale == "days" ~ Demo_LBP_past_pain_duration * 1,
      .default = Demo_LBP_past_pain_duration
    ),
    # Some subjects have contradicting values for the LBP questionnaire compared 
    # to when the clinician examined them. For these we will create a new column
    # with a intermediate_C for changing from control in questionnaire to patient
    # in clinical assessment and intermediate_Q for questionnaire patient and 
    # clinical assessment negative
    Demo_combi_LBP = case_when(
      Demo_clinic_LBP == "yes" & Demo_question_LBP == "false" ~ "intermediate_C",
      Demo_clinic_LBP == "no" & Demo_question_LBP == "true" ~ "intermediate_Q",
      .default = Demo_clinic_LBP
    ), 
    Demo_clinic_LBP = factor(Demo_clinic_LBP, levels = c("yes", "no", "in_past")),
    Demo_question_LBP = factor(Demo_question_LBP, levels = c("true", "false")),
    Demo_combi_LBP = as.factor(Demo_combi_LBP), 
  ) %>%
  # remove subjects that shouldn't be included in study #
  filter(Study_included == "yes") %>%
  # remove pain duration scale columns as they have been combined into one
  select(!c(Demo_LBP_pain_duration_scale,
            Demo_LBP_past_pain_duration_scale)) %>%
  # finally we will relocate a few columns to make the data better to read
  relocate(Demo_question_LBP, .after = Demo_clinic_LBP) %>%
  relocate(Demo_combi_LBP, .after = Demo_question_LBP) %>%
  relocate(Study_mri_included, .after = Study_included)

#summary(as.factor(berlin_back_D$Study_included))


##------------------------------------------------------------------------------
## QUESTIONNAIRES --------------------------------------------------------------
##------------------------------------------------------------------------------

# Using the already updated demographic columns we adapt the questionnaire data

berlin_back_DQ <- berlin_back_D %>%
  ## SF36 ##
  # convert the sf36 summary columns to numeric
  mutate(
    Quest_sf36_emotional_role_function = round(as.numeric(Quest_sf36_emotional_role_function), 1),
    Quest_sf36_health_perception = round(as.numeric(Quest_sf36_health_perception), 1),
    Quest_sf36_physical_function = round(as.numeric(Quest_sf36_physical_function), 1),
    Quest_sf36_physical_pain = round(as.numeric(Quest_sf36_physical_pain), 1),
    Quest_sf36_vitality = round(as.numeric(Quest_sf36_vitality), 1),
    Quest_sf36_physical_role_function = round(as.numeric(Quest_sf36_physical_role_function), 1),
    Quest_sf36_psychological_well_being = round(as.numeric(Quest_sf36_psychological_well_being), 1),
    Quest_sf36_social_function = round(as.numeric(Quest_sf36_social_function), 1),
    ## IPAQ ##
    # first change "UNCERTAIN" to NA in all ipaq #
    across(starts_with("Quest_ipaq"), ~na_if(.x, "UNCERTAIN")),
    # convert the sum columns to MET minutes by multiplying by the MET estimate
    # fo the type of activity #
    Quest_ipaq_met_vigorous = as.numeric(Quest_ipaq_q2_sum) * 8,
    Quest_ipaq_met_moderate = as.numeric(Quest_ipaq_q4_sum) * 4,
    Quest_ipaq_met_walking = as.numeric(Quest_ipaq_q6_sum) * 3.3,
    Quest_ipaq_met_sitting = as.numeric(Quest_ipaq_q7_sum) * 1.5,
    # Sum all the activities into a MET total column #
    Quest_ipaq_met_sum = rowSums(across(starts_with("Quest_ipaq_met")), na.rm = TRUE),
    # if values are 0 then all ipaq where NA and can therefore also be NA
    Quest_ipaq_met_sum = na_if(Quest_ipaq_met_sum, 0),
    ## FABQ ##
    # Sum the FABQ scores for physical activity and workload #
    # first change "no_LBP" to NA for simple addition #
    across(starts_with("Quest_fabq"), ~na_if(.x, "no_LBP")),
    across(starts_with("Quest_fabq"), ~as.numeric(.x)),
    Quest_fabq_physical_activity = Quest_fabq_2 + Quest_fabq_3 + Quest_fabq_4 +
      Quest_fabq_5,
    Quest_fabq_workload = Quest_fabq_6 + Quest_fabq_7 + Quest_fabq_9 + Quest_fabq_10 +
     Quest_fabq_11 + Quest_fabq_12 + Quest_fabq_15,
    ## Kinesiophobia ##
    # create a sum column by first coverting the 4 ordinal values to numbers and then
    # adding them together #
    across(starts_with("Quest_tskgv"), ~replace(., . == "not_agree_at_all", 1)),
    across(starts_with("Quest_tskgv"), ~replace(., . == "more_or_less_disagree", 2)),
    across(starts_with("Quest_tskgv"), ~replace(., . == "more_or_less_agree", 3)),
    across(starts_with("Quest_tskgv"), ~replace(., . == "totally_agree", 4)),
    across(starts_with("Quest_tskgv"), ~na_if(.x, "no_LBP")),
    across(starts_with("Quest_tskgv"), ~as.numeric(.x)),
    Quest_kinesiophobia = rowSums(across(starts_with("Quest_tskgv")), na.rm = TRUE),
    Quest_kinesiophobia = na_if(Quest_kinesiophobia, 0),
    ## Behavioural Automaticity Questionnaire (SRBAI) ##
    # create a sum column by first coverting the 6 ordinal values to numbers and then
    # adding them together #
    across(starts_with("Quest_"), ~replace(., . == "1-does_not_apply_at_all", 1)),
    across(starts_with("Quest_"), ~replace(., . == "2-does_not_apply", 2)),
    across(starts_with("Quest_"), ~replace(., . == "3-rather_not_true", 3)),
    across(starts_with("Quest_"), ~replace(., . == "4-rather_true", 4)),
    across(starts_with("Quest_"), ~replace(., . == "5-applies", 5)),
    across(starts_with("Quest_"), ~replace(., . == "6-totally_applies", 6)),
    across(starts_with("Quest_srbai"), ~as.numeric(.x)),
    Quest_srbai_sum = rowSums(across(starts_with("Quest_srbai")), na.rm = TRUE),
    Quest_srbai_sum = na_if(Quest_srbai_sum, 1),
    ## Behavioral Regulation in Sport Questionnaire (BRSQ) ##
    across(starts_with("Quest_brsq"), ~as.numeric(.x)),
    Quest_brsq_intrinsic_motivation = Quest_brsq_q1 + Quest_brsq_q3,
    Quest_brsq_integrated_regulation = Quest_brsq_q2 + Quest_brsq_q4,
    Quest_brsq_external_regulation = Quest_brsq_q5 + Quest_brsq_q6,
    ## Stress score and days ##
    # Questions 5-8 relate to stress score and questions 1-4 relate to days #
    across(starts_with("Quest_stress"), ~replace(., . == "1-not_at_all", 1)),
    across(starts_with("Quest_stress"), ~replace(., . == "2-on_single_days", 2)),
    across(starts_with("Quest_stress"), ~replace(., . == "3-on_more_than_half_of_days", 3)),
    across(starts_with("Quest_stress"), ~replace(., . == "4-almost_every_day", 4)),
    across(starts_with("Quest_stress"), ~replace(., . == "not_at_all", 0)),
    across(starts_with("Quest_stress"), ~replace(., . == "1-nerver", 1)),
    across(starts_with("Quest_stress"), ~replace(., . == "2-rare", 2)),
    across(starts_with("Quest_stress"), ~replace(., . == "3-sometimes", 3)),
    across(starts_with("Quest_stress"), ~replace(., . == "4-frequently", 4)),
    across(starts_with("Quest_stress"), ~replace(., . == "5-very_often", 5)),
    across(starts_with("Quest_stress"), ~na_if(.x, "no_LBP")),
    across(starts_with("Quest_stress"), ~as.numeric(.x)),
    Quest_stress_days = Quest_stress_1 + Quest_stress_2 + Quest_stress_3 + 
      Quest_stress_4,
    Quest_stress_score = Quest_stress_5 + Quest_stress_6 + Quest_stress_7 + 
      Quest_stress_8,
    ## Pain and disability by Korff ##
    across(starts_with("Quest_cor"), ~na_if(.x, "no_LBP")),
    across(starts_with("Quest_cor"), ~as.numeric(.x)),
    Quest_korff_pain_intensity = (Quest_cor_2 + Quest_cor_3 + Quest_cor_4) /3 *10,
    Quest_korff_disability = (Quest_cor_5 + Quest_cor_6 + Quest_cor_7 + 
                                Quest_cor_8) /4 *10,
    Quest_korff_disability_grading = case_when(
      Quest_korff_disability < 30 ~ 0,
      Quest_korff_disability >= 30 & Quest_korff_disability < 50 ~ 1,
      Quest_korff_disability >= 50 & Quest_korff_disability < 70 ~ 2,
      Quest_korff_disability >= 70 ~ 3,
      .default = Quest_korff_disability
    ),
    Quest_korff_days_grading = case_when(
      Quest_cor_1 < 4 ~ 0,
      Quest_cor_1 >= 4 & Quest_cor_1 < 8 ~ 1,
      Quest_cor_1 >= 8 & Quest_cor_1 < 16 ~ 2,
      Quest_cor_1 >= 16 ~ 3,
      .default = Quest_cor_1
    ),
    Quest_korff_disability_score = Quest_korff_disability_grading + Quest_korff_days_grading,
    Quest_korff_grading = case_when(
      Quest_korff_pain_intensity < 50 & Quest_korff_disability_score < 3 ~ 1,
      Quest_korff_pain_intensity >= 50 & Quest_korff_disability_score < 3 ~ 2,
      Quest_korff_pain_intensity >= 50 & Quest_korff_disability_score >=3 & Quest_korff_disability_score < 5 ~ 3,
      Quest_korff_pain_intensity >= 50 & Quest_korff_disability_score >= 5 ~ 4,
      .default = Quest_korff_disability_score
    ),
    ## Roland Morris Disability ##
    across(starts_with("Quest_rms"), ~na_if(.x, "no_LBP")),
    across(starts_with("Quest_rms"), ~replace(., . == "true", 1)),
    across(starts_with("Quest_rms"), ~replace(., . == "false", 0)),
    across(starts_with("Quest_rms"), ~as.numeric(.x)),
    Quest_RM_disability = rowSums(across(starts_with("Quest_rms")))
  ) %>%
  relocate(starts_with("Quest_"), .after = "Clinic_3_SM_measure")

##------------------------------------------------------------------------------
## Spine MRI -------------------------------------------------------------------
##------------------------------------------------------------------------------

berlin_back_DQM <- berlin_back_DQ %>%
  mutate(
    across(starts_with("MRI_ivd_"), as.factor),
    across(starts_with("MRI_disc_hern"), as.factor),
    across(starts_with("MRI_facet_"), as.factor),
    across(starts_with("MRI_osteo_"), as.factor),
    across(starts_with("MRI_spinal_canal"), as.double),
    across(starts_with("MRI_schizas"), as.double),
    across(starts_with("MRI_lordosis"), as.double)
  )

##------------------------------------------------------------------------------
## Spine Mouse -----------------------------------------------------------------
##------------------------------------------------------------------------------

### When reading in the excel sheet matthiass_test columns turn to a logical when they
### are numeric in the excel sheet. I don't know why this happens but only a single
### subject has these values so I will just remove the columns and try to figure out 
### the bug (maybe) at another time.

# Once matthiass columns are removed all other cols are converted to double as they
# are continuous variables #

berlin_back_DQMS <- berlin_back_DQM %>%
  select(!ends_with("matthiass_test")) %>%
  mutate(
    across(starts_with("SM"), as.double)
    )


##------------------------------------------------------------------------------
## Clinical Data ---------------------------------------------------------------
##------------------------------------------------------------------------------

berlin_back_DQMSC <- berlin_back_DQMS %>%
  mutate(
    across(starts_with("Clinic_motion_h"), as.numeric),
    across(starts_with("Clinic_pain_"), as.factor),
    across(starts_with("Clinic_sensory"), as.factor),
    across(starts_with("Clinic_prior"), as.factor),
    across(starts_with("Clinic_question_alcohol"), as.factor),
    Clinic_question_smoking = as.factor(Clinic_question_smoking),
    Clinic_question_smoking_pack_years = as.numeric(Clinic_question_smoking_pack_years),
    across(starts_with("Clinic_medication"), as.factor),
    Clinic_question_family_BP = as.factor(Clinic_question_family_BP),
    Clinic_question_job_field = as.factor(Clinic_question_job_field),
    Clinic_question_job_posture = as.factor(Clinic_question_job_posture),
    Clinic_question_job_posture_duration = as.numeric(Clinic_question_job_posture_duration),
    Clinic_question_job_load_1 = as.factor(Clinic_question_job_load_1),
    Clinic_question_job_load_1_duration = as.numeric(Clinic_question_job_load_1_duration),
    Clinic_question_job_load_2 = as.factor(Clinic_question_job_load_2),
    Clinic_question_job_load_2_duration = as.numeric(Clinic_question_job_load_2_duration),
    Clinic_question_job_duration_years = as.numeric(Clinic_question_job_duration_years),
    Clinic_question_pyschological_stress = as.factor(Clinic_question_pyschological_stress),
    Clinic_question_living_situation = as.factor(Clinic_question_living_situation),
    Clinic_question_family_conflicts = as.factor(Clinic_question_family_conflicts),
    Clinic_question_body_self_assessment = as.numeric(Clinic_question_body_self_assessment),
    across(starts_with("Clinic_body_cervical"), as.numeric),
    Clinic_body_chin_sternum_dist = as.factor(Clinic_body_chin_sternum_dist),
    across(starts_with("Clinic_body_thoracic"), as.numeric),
    Clinic_motion_finger_floor_dist = as.numeric(Clinic_motion_finger_floor_dist),
    Clinic_body_hip_flexion = as.numeric(Clinic_body_hip_flexion),
    Clinic_motion_trendelenburg_sign = as.factor(Clinic_motion_trendelenburg_sign),
    Clinic_motion_ott = as.double(Clinic_motion_ott),
    Clinic_motion_shober = as.double(Clinic_motion_shober),
    Clinic_body_square_shoulders = as.factor(Clinic_body_square_shoulders),
    Clinic_body_plump_line = as.factor(Clinic_body_plump_line),
    Clinic_body_rib_hump = as.factor(Clinic_body_rib_hump),
    Clinic_body_lumbar_bulge = as.factor(Clinic_body_lumbar_bulge),
    Clinic_body_asym_waist_triangle = as.factor(Clinic_body_asym_waist_triangle),
    Clinic_body_back_form = as.factor(Clinic_body_back_form),
    Clinic_body_roussoly_type = as.factor(Clinic_body_roussoly_type),
    Clinic_motion_sagittal_balance = as.factor(Clinic_motion_sagittal_balance),
    Clinic_motion_coronal_balance = as.factor(Clinic_motion_coronal_balance),
    Clinic_body_back_head_wall_distance = as.numeric(Clinic_body_back_head_wall_distance),
    Clinic_motion_rigid_muscle = as.factor(Clinic_motion_rigid_muscle),
    Clinic_motion_mobility  = as.factor(Clinic_motion_mobility),
    Clinic_body_toe_stand = as.factor(Clinic_body_toe_stand),
    Clinic_body_heel_stand = as.factor(Clinic_body_toe_stand),
    Clinic_motion_straight_leg_raise = as.factor(Clinic_motion_straight_leg_raise),
    across(starts_with("Clinic_motion_pseudo_lasegue"), as.numeric),
    Clinic_motion_gait_pattern = as.factor(Clinic_motion_gait_pattern),
    across(starts_with("Clinic_motion_reflex_quad"), as.factor),
    across(starts_with("Clinic_motion_reflex_achilles"), as.factor),
    across(starts_with("Clinic_motion_hip"), as.numeric),
    across(starts_with("Clinic_motion_thomas"), as.factor),
    across(starts_with("Clinic_body_pelvic"), as.factor),
    Clinic_motion_sit_to_stand_30sec = as.numeric(Clinic_motion_sit_to_stand_30sec),
    Clinic_question_physical_activity = as.factor(Clinic_question_physical_activity),
    across(starts_with("Clinic_heart"), as.numeric),
    Clinic_breath_rate = as.numeric(Clinic_breath_rate),
    Clinic_3_SM_measure = as.factor(Clinic_3_SM_measure),
    Clinic_question_covid19_activity = as.factor(Clinic_question_covid19_activity),
    # change alcohol consumption to ordinal and add "none" to glasses when never drinks #
    Clinic_question_alcohol_glasses = as.factor(case_when(
      Clinic_question_alcohol_freq == "never" ~ "none",
      .default = Clinic_question_alcohol_glasses
    )),
    Clinic_question_alcohol_restrict = as.factor(case_when(
      Clinic_question_alcohol_freq == "never" ~ "never",
      .default = Clinic_question_alcohol_restrict
    )),
    # change create ordinal features #
    Clinic_question_alcohol_freq = factor(Clinic_question_alcohol_freq,
                                          levels = c("never",
                                                     "less_than_once_month",
                                                     "2-4_per_month",
                                                     "2-3_per_week",
                                                     ">=4_per_week"),
                                          ordered = TRUE),
    Clinic_question_alcohol_glasses = factor(Clinic_question_alcohol_glasses,
                                          levels = c("none",
                                                     "1-2",
                                                     "3-4",
                                                     "5-6",
                                                     "7-9",
                                                     ">=10"),
                                          ordered = TRUE),
    Clinic_question_alcohol_restrict = factor(Clinic_question_alcohol_restrict,
                                          levels = c("never",
                                                     "less_than_once_month",
                                                     "once_per_month",
                                                     "once_per_week"),
                                          ordered = TRUE),
    Clinic_question_covid19_activity = factor(Clinic_question_covid19_activity,
                                               levels = c("not",
                                                          "hardly",
                                                          "moderate",
                                                          "significantly"),
                                               ordered = TRUE),
    Clinic_pain_walking_distance = factor(Clinic_pain_walking_distance,
                                             levels = c("unrestricted",
                                                        "<_100m",
                                                        "100-500m",
                                                        "500-1000m",
                                                        "1000-2000m"),
                                             ordered = TRUE),
    Clinic_question_physical_activity = factor(Clinic_question_physical_activity,
                                              levels = c("no_not_intend_to",
                                                         "no_but_thinking_to",
                                                         "no_but_I_want_to",
                                                         "yes_but_difficult",
                                                         "yes_and_easy"),
                                              ordered = TRUE),
  ) 


##------------------------------------------------------------------------------
## save preprocessed data ------------------------------------------------------
##------------------------------------------------------------------------------

berlin_back_preproc <- berlin_back_DQMSC %>% 
  # subject not applicable for our study, only conducted gait analysis #
  filter(Study_ID != 1)

## Save berlin_back_data
# csv #
write.csv(x = berlin_back_preproc, file = preproc_output_csv, row.names=FALSE)
saveRDS(object = berlin_back_preproc, file = preproc_output_rds)







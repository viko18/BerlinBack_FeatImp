## correlation plots matrices following boruta feature reduction ##
## INPUT: cleaned csv files & boruta features from 02_model_perf_tables.R ##
## OUTput: correlation matrix plots of boruta selected fratures for all datasets ##

# Set working directory #
wd <- "..../Berlin_Back/"
setwd(wd)

# load dependencies #
library("dplyr")
library("fs")
library("tibble") 
library("tidyr")
library("stringr")
library("caret")
library("corrplot")
library("RColorBrewer")
library("janitor")

# list all 15 cleaned datasets #
clean_dat_fils <- list.files(path = path_wd("01_Data", "Cleaned"),
                             pattern = "_cleaned_Clinic_BP.*\\.rds$")

# remove only demographic dataset #
clean_dat_fils <- clean_dat_fils[-3]

# boruta feat selection tables files #

boruta_tbl_fils <- list.files(path = path_wd("04_outputs"),
                             pattern = "_boruta_whole_features.*\\.csv$")

# order of file names is not the same as cleaned files so change it #
boruta_tbl_fils <- c("clinic_boruta_whole_features.csv",
                     "clinic_sm_sum_boruta_whole_features.csv",
                     "mri_boruta_whole_features.csv",
                     "mri_clinic_boruta_whole_features.csv",
                     "mri_clinic_sm_sum_boruta_whole_features.csv",
                     "mri_quest_boruta_whole_features.csv",
                     "mri_quest_clinic_sm_sum_boruta_whole_features.csv",
                     "mri_quest_clinic_boruta_whole_features.csv",
                     "mri_quest_sm_sum_boruta_whole_features.csv",
                     "mri_sm_sum_boruta_whole_features.csv",
                     "quest_clinic_boruta_whole_features.csv",
                     "quest_clinic_sm_sum_boruta_whole_features.csv",
                     "quest_sm_sum_boruta_whole_features.csv",
                     "quest_boruta_whole_features.csv",
                     "sm_sum_boruta_whole_features.csv")

plot_title <- c("C", "C + S", "M", "C + M",
                "C + M + S", "M + Q", "C + M + Q + S", "C + M + Q", "M + Q + S",
                "M + S", "C + Q", "C + Q + S", "Q + S", "Q", 
                "S")

for (i in 1:length(clean_dat_fils)) {
  
  # read clean df #
  clean_dat_df <- readRDS(file = path_wd("01_Data", "Cleaned", clean_dat_fils[i]))
  
  # read boruta feats #
  boruta_feats_df <- read.csv(file = path_wd("04_outputs", boruta_tbl_fils[i])) 
  
  # vector with boruta variable names #
  boruta_feats <- boruta_feats_df$boruta_feats
  
  # Select all Boruta variables and target for correlation matrix #
  boruta_dat_df <- clean_dat_df |> 
    select(all_of(boruta_feats), Demo_clinic_LBP)
  
  # create correlation matrix
  demo_cor_sp <- boruta_dat_df |>
    mutate(across(where(is.factor), as.numeric)) |>
    cor(method = "spearman")
  
  # file path for output plots #
  demo_sp_path= path_wd("04_outputs", "plots",
                        paste0(str_remove(boruta_tbl_fils[i], ".csv"), 
                               "_spear_corr_plot"),
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
           title = paste0(plot_title[i], "_spearman"),
           col = brewer.pal(n=10, name="PiYG")) 
  dev.off()
  
}




# Univariate statistics for each modality of the Berlin Back Study Data 
# Steps: 
#   (1) Test for normality of the data
#   (2) Group comparison for ordinal and continuous data
#   (3) Group comparison for nominal data
#   (4) Data description (max, min, mean, median, etc.)
# Author: Frederick Junker [frederick.junker@hs-gesundheit.de]



### Please define the following directory, where all data are stored
# (Data: "Demo_cleaned_Clinic_BP_dim_1161X5", "MRI_cleaned_Clinic_BP_dim_799X31", 
# "Clinic_cleaned_Clinic_BP_dim_1080X48", "Questionnaire_cleaned_Clinic_BP_dim_986X28", 
# "SM_SUM_cleaned_Clinic_BP_dim_1113X53")
wd <- "C:/Data/Sciebo/2023_Charite_HSG_Rueckenstudie/"
setwd(wd)


# load required libraries
if (!require("coin")) install.packages("coin")
library("coin")
if (!require("rcompanion")) install.packages("rcompanion")
library("rcompanion")
if (!require("dplyr")) install.packages("dplyr")
library("dplyr")
if (!require("fs")) install.packages("fs")
library("fs")
if (!require("tibble")) install.packages("tibble")
library("tibble") 
if (!require("tidyr")) install.packages("tidyr")
library("tidyr")
if (!require("nortest")) install.packages("nortest")
library("nortest")


#### load cleaned data frames ####
df_path <- path_wd("01_Data", "2024", "cleaned")
demo_df <- readRDS(path(df_path, "Demo_cleaned_Clinic_BP_dim_1161X5",ext = "rds"))
mri_df <- readRDS(path(df_path, "MRI_cleaned_Clinic_BP_dim_799X31",ext = "rds"))
clinic_df <- readRDS(path(df_path, "Clinic_cleaned_Clinic_BP_dim_1080X48",ext = "rds"))
quest_df <- readRDS(path(df_path, "Questionnaire_cleaned_Clinic_BP_dim_986X28",ext = "rds"))
sm_sum_df <- readRDS(path(df_path, "SM_SUM_cleaned_Clinic_BP_dim_1113X53",ext = "rds"))



################################################################################
########## back pain vs control significance difference tests ################## 

### 1: Demographic data
## 1.1: Continuous variables (Age, BMI)
# 1.1.1: Anderson-Darling test for normality
# Anderson-Darling test was used as Kolmogorov Smirnov test is not suitable for ties
Demo_Distribution <- data.frame()
for (i in c(2,5)){
  tmp <- as.numeric(unlist(demo_df[,i]))
  AD_pain <- ad.test(tmp[demo_df$Demo_clinic_LBP == "yes"])
  AD_control <- ad.test(tmp[demo_df$Demo_clinic_LBP == "no"])
  Demo_Distribution <- rbind(Demo_Distribution, cbind(colnames(demo_df)[i], AD_pain[1], AD_pain[2], AD_control[1], AD_control[2]))
}
Demo_Distribution <- cbind(Demo_Distribution, 
                           p.adjust(Demo_Distribution[,3], method = "holm", n = nrow(Demo_Distribution)*2),
                           p.adjust(Demo_Distribution[,5], method = "holm", n = nrow(Demo_Distribution)*2))
Demo_Distribution <- sapply(Demo_Distribution, unlist)
colnames(Demo_Distribution) <- c("Feature", "BackPain_AD_A-Value", "BackPain_AD_p-Value", 
                                 "Control_AD_A-Value", "Control_AD_p-Value",
                                 "BackPain_AD_FWE_p-Value", "Control_AD_FWE_p-Value")
write.csv(Demo_Distribution, file = paste(wd,"Results_Univariate/Demo_Distribution.csv", sep=""),row.names = FALSE)
rm(i, tmp, AD_pain, AD_control)


# 1.1.2: Wilcox test (aka Mann-Whitney U Test)
# Non-parametric Wilcoxon-Mann-Whitney test to determine significant difference between 
# cLBP patients and asymptomatic controls. Hence, u-values, z-values, r-value (effect sizes), 
# as well as the p-value are reported from the Wilcoxon-Mann-Whitney test
Demo_Results <- data.frame()
for (i in c(2,5)){
  data_tmp <- data.frame(demo_df$Demo_clinic_LBP, demo_df[,i])
  colnames(data_tmp) <- c("Demo_clinic_LBP","Variable")
  tmp <- wilcox_test(data = data_tmp, Variable~Demo_clinic_LBP)
  Demo_Results <- rbind(Demo_Results, cbind(colnames(demo_df)[i], statistic(tmp, type = "linear"),
                                          statistic(tmp, "standardized"),
                                          pvalue(tmp)))
}
Demo_Results <- cbind(Demo_Results, p.adjust(Demo_Results[,4], method = "holm", n = 3))
Demo_Results <- cbind(Demo_Results, as.numeric(Demo_Results[,3])/sqrt(nrow(demo_df)))
colnames(Demo_Results) <- c("Feature", "u-Value", "z-value", "p-value", "FWE_p-value", "r-value")
Demo_Results <- sapply(Demo_Results, unlist)
rm(tmp, i , data_tmp)
write.csv(Demo_Results, file = paste(wd,"Results_Univariate/Demo_Results.csv", sep=""),row.names = FALSE)


## 1.2: Discrete variables (sex)
# 1.2.1: Chi square test
# Nominal data were compared using the Chi-Square test and reported using Chi2 values, 
# Cohen’s ω-values (effect size), and p-values.
tmp <- chisq.test(demo_df$Demo_sex, demo_df$Demo_clinic_LBP, simulate.p.value = TRUE)
Demo_Results_Nominal <- cbind("Demo_sex", tmp[1], tmp[2], nrow(demo_df), cohenW(demo_df$Demo_sex, demo_df$Demo_clinic_LBP), tmp[3], p.adjust(tmp[3], method = "holm", n = 3))
colnames(Demo_Results_Nominal) <- c("Feature", "x-value", "DF", "n", "Cohen's_ω", "p-value", "FWE_p-value")
write.csv(Demo_Results_Nominal, file = paste(wd,"Results_Univariate/Demo_Results_Nominal.csv", sep=""),row.names = FALSE)
rm(tmp)


## 1.3: Summary stats -> mean, sd, min, max, sample size ##
for (i in c("yes","no")){
  tmp <- demo_df[demo_df$Demo_clinic_LBP == i,] %>%
  select(where(is.numeric)) %>%
  pivot_longer(everything())%>%
  group_by(name) %>%
  summarise(mean = mean(value),
            sd = sd(value),
            minimum = min(value),
            maximum = max(value),
            median = median(value),
            IQR = IQR(value),
            sample_size = n()) 
  if (i == "yes"){Demo_Summary <-tmp}
  else{Demo_Summary <- cbind(Demo_Summary, tmp[, 2:8])}
}
rm(tmp, i)
colnames(Demo_Summary) <- c("Feature", "Patient_Mean", "Patient_SD", "Patient_Min", 
                            "Patient_Max", "Patient_Median", "Patient_IQR", "Patient_SampleSize",
                            "Control_Mean", "Control_SD", "Control_Min", 
                            "Control_Max", "Control_Median", "Control_IQR", "Control_SampleSize")

write.csv(Demo_Summary, file = paste(wd,"Results_Univariate/Demo_Summary.csv", sep=""),row.names = FALSE)




################################################################################
### 2: Spino-pelvic MRI
## 2.1: Continuous/Ordinal variables
# 2.1.1: Anderson-Darling test for normality
# Anderson-Darling test was used as Kolmogorov Smirnov test is not suitable for ties
MRI_Distribution <- data.frame()
for (i in c(6:31)){
  tmp <- as.numeric(unlist(mri_df[,i]))
  AD_pain <- ad.test(tmp[mri_df$Demo_clinic_LBP == "yes"])
  AD_control <- ad.test(tmp[mri_df$Demo_clinic_LBP == "no"])
  MRI_Distribution <- rbind(MRI_Distribution, cbind(colnames(mri_df)[i], AD_pain[1],AD_pain[2], AD_control[1], AD_control[2]))
}
MRI_Distribution <- cbind(MRI_Distribution, 
                          p.adjust(MRI_Distribution[,3], method = "holm", n = nrow(MRI_Distribution)*2),
                          p.adjust(MRI_Distribution[,5], method = "holm", n = nrow(MRI_Distribution)*2))
colnames(MRI_Distribution) <- c("Feature", "BackPain_AD_A-Value", "BackPain_AD_p-Value", "Control_AD_A-Value",
                                "Control_AD_p-Value", "BackPain_AD_FWE_p-Value", "Control_AD_FWE_p-Value")
MRI_Distribution <- sapply(MRI_Distribution, unlist)
write.csv(MRI_Distribution, file = paste(wd,"Results_Univariate/MRI_Distribution.csv", sep=""),row.names = FALSE)
rm(i, tmp, AD_pain, AD_control)


# 2.1.2: Wilcox test (aka Mann-Whitney U Test)
# Non-parametric Wilcoxon-Mann-Whitney test to determine significant difference between 
# cLBP patients and asymptomatic controls. Hence, u-values, z-values, r-value (effect sizes), 
# as well as the p-value are reported from the Wilcoxon-Mann-Whitney test
MRI_Results <- data.frame()
for (i in c(6:31)){
  data_tmp <- data.frame(mri_df$Demo_clinic_LBP, as.numeric(unlist(mri_df[,i])))
  colnames(data_tmp) <- c("Demo_clinic_LBP","Variable")
  tmp <- wilcox_test(data = data_tmp, Variable~Demo_clinic_LBP)
  MRI_Results <- rbind(MRI_Results, cbind(colnames(mri_df)[i], statistic(tmp, type = "linear"),
                                                statistic(tmp, "standardized"),
                                                pvalue(tmp)))
}
MRI_Results <- cbind(MRI_Results, p.adjust(MRI_Results[,4], method = "holm", n = nrow(MRI_Results)))
MRI_Results <- cbind(MRI_Results, as.numeric(MRI_Results[,3])/sqrt(nrow(mri_df)))
colnames(MRI_Results) <- c("Feature", "u-Value", "z-value", "p-value", "FWE_p-value", "r-value")
MRI_Results <- sapply(MRI_Results, unlist)
rm(tmp, i , data_tmp)
write.csv(MRI_Results, file = paste(wd,"Results_Univariate/MRI_Results.csv", sep=""),row.names = FALSE)


## 2.2: Summary stats -> mean, sd, min, max, sample size ##
# Set "deg", "hern", "facet", "osteo" to be numeric ordinal
for (i in 6:31){
  mri_df[,i] <- as.numeric(unlist(mri_df[,i]))
}

for (i in c("yes","no")){
  tmp <- mri_df[mri_df$Demo_clinic_LBP == i,] %>%
    select(where(is.numeric)) %>%
#    select(where(is.ordered())) %>%
    pivot_longer(everything())%>%
    group_by(name) %>%
    summarise(mean = mean(value),
              sd = sd(value),
              minimum = min(value),
              maximum = max(value),
              median = median(value),
              IQR = IQR(value),
              sample_size = n()) 
  if (i == "yes"){MRI_Summary <-tmp}
  else{MRI_Summary <- cbind(MRI_Summary, tmp[, 2:8])}
}
rm(tmp, i)
colnames(MRI_Summary) <- c("Feature", "Patient_Mean", "Patient_SD", "Patient_Min", 
                            "Patient_Max", "Patient_Median", "Patient_IQR", "Patient_SampleSize",
                            "Control_Mean", "Control_SD", "Control_Min", 
                            "Control_Max", "Control_Median", "Control_IQR", "Control_SampleSize")
write.csv(MRI_Summary, file = paste(wd,"Results_Univariate/MRI_Summary.csv", sep=""),row.names = FALSE)




################################################################################
### 3: Clinic examination 
## 3.1: Continuous/Ordinal variables
# 3.1.1: Anderson-Darling test for normality
# Anderson-Darling test was used as Kolmogorov Smirnov test is not suitable for ties
Clinic_Distribution <- data.frame()
for (i in c(6:18,25,28:30,34:45,48)){
  tmp <- as.numeric(unlist(clinic_df[,i]))
  AD_pain <- ad.test(tmp[clinic_df$Demo_clinic_LBP == "yes"])
  AD_control <- ad.test(tmp[clinic_df$Demo_clinic_LBP == "no"])
  Clinic_Distribution <- rbind(Clinic_Distribution, cbind(colnames(clinic_df)[i], AD_pain[1],AD_pain[2], AD_control[1], AD_control[2]))
}
Clinic_Distribution <- cbind(Clinic_Distribution, 
                          p.adjust(Clinic_Distribution[,3], method = "holm", n = nrow(Clinic_Distribution)*2),
                          p.adjust(Clinic_Distribution[,5], method = "holm", n = nrow(Clinic_Distribution)*2))
colnames(Clinic_Distribution) <- c("Feature", "BackPain_AD_A-Value", "BackPain_AD_p-Value", "Control_AD_A-Value",
                                "Control_AD_p-Value", "BackPain_AD_FWE_p-Value", "Control_AD_FWE_p-Value")
Clinic_Distribution <- sapply(Clinic_Distribution, unlist)
write.csv(Clinic_Distribution, file = paste(wd,"Results_Univariate/Clinic_Distribution.csv", sep=""),row.names = FALSE)
rm(i, tmp, AD_pain, AD_control)


# 3.1.2: Wilcox test (aka Mann-Whitney U Test)
# Non-parametric Wilcoxon-Mann-Whitney test to determine significant difference between 
# cLBP patients and asymptomatic controls. Hence, u-values, z-values, r-value (effect sizes), 
# as well as the p-value are reported from the Wilcoxon-Mann-Whitney test
Clinic_Results <- data.frame()
for (i in c(6:18,25,28:30,34:45,48)){
  data_tmp <- data.frame(clinic_df$Demo_clinic_LBP, as.numeric(unlist(clinic_df[,i])))
  colnames(data_tmp) <- c("Demo_clinic_LBP","Variable")
  tmp <- wilcox_test(data = data_tmp, Variable~Demo_clinic_LBP)
  Clinic_Results <- rbind(Clinic_Results, cbind(colnames(clinic_df)[i], statistic(tmp, type = "linear"),
                                                statistic(tmp, "standardized"),
                                                pvalue(tmp)))
}
Clinic_Results <- cbind(Clinic_Results, p.adjust(Clinic_Results[,4], method = "holm", n = ncol(clinic_df)-5))
Clinic_Results <- cbind(Clinic_Results, as.numeric(Clinic_Results[,3])/sqrt(nrow(clinic_df)))
colnames(Clinic_Results) <- c("Feature", "u-Value", "z-value", "p-value", "FWE_p-value", "r-value")
Clinic_Results <- sapply(Clinic_Results, unlist)
rm(tmp, i , data_tmp)
write.csv(Clinic_Results, file = paste(wd,"Results_Univariate/Clinic_Results.csv", sep=""),row.names = FALSE)


## 3.2: Discrete variables
# 3.2.1: Chi square test
# Nominal data were compared using the Chi-Square test and reported using Chi2 values, 
# Cohen’s ω-values (effect size), and p-values.
Clinic_Results_Nominal <- data.frame()
for (i in c(19:24,26,27,31:33,46,47)){
  data_tmp <- data.frame(cbind(clinic_df$Demo_clinic_LBP, as.numeric(unlist(clinic_df[,i]))))
  colnames(data_tmp) <- c("Demo_clinic_LBP","Variable")
  tmp <- chisq.test(data_tmp$Variable, data_tmp$Demo_clinic_LBP, simulate.p.value = TRUE)
  Clinic_Results_Nominal <- rbind(Clinic_Results_Nominal, cbind(colnames(clinic_df)[i], tmp[1], tmp[2],nrow(clinic_df),cohenW(data_tmp$Variable, data_tmp$Demo_clinic_LBP), tmp[3]))
}
Clinic_Results_Nominal <- cbind(Clinic_Results_Nominal, p.adjust(as.numeric(unlist(Clinic_Results_Nominal[6]), method = "holm", n = ncol(clinic_df)-5)))
colnames(Clinic_Results_Nominal) <- c("Feature", "x-value", "DF", "n", "Cohen's_ω", "p-value", "FWE_p-value")
Clinic_Results_Nominal <- sapply(Clinic_Results_Nominal, unlist)
write.csv(Clinic_Results_Nominal, file = paste(wd,"Results_Univariate/Clinic_Results_Nominal.csv", sep=""),row.names = FALSE)
rm(tmp, i, data_tmp)


## 3.3: Summary stats -> mean, sd, min, max, sample size ##
clinic_df$Clinic_body_chin_sternum_dist <- as.numeric(clinic_df$Clinic_body_chin_sternum_dist)
clinic_df$Clinic_body_roussoly_type <- as.numeric(clinic_df$Clinic_body_roussoly_type)

for (i in c("yes","no")){
  tmp <- clinic_df[clinic_df$Demo_clinic_LBP == i,] %>%
    select(where(is.numeric)) %>%
    pivot_longer(everything())%>%
    group_by(name) %>%
    summarise(mean = mean(value),
              sd = sd(value),
              minimum = min(value),
              maximum = max(value),
              median = median(value),
              IQR = IQR(value),
              sample_size = n()) 
  if (i == "yes"){Clinic_Summary <-tmp}
  else{Clinic_Summary <- cbind(Clinic_Summary, tmp[, 2:8])}
}
rm(tmp, i)
colnames(Clinic_Summary) <- c("Feature", "Patient_Mean", "Patient_SD", "Patient_Min", 
                            "Patient_Max", "Patient_Median", "Patient_IQR", "Patient_SampleSize",
                            "Control_Mean", "Control_SD", "Control_Min", 
                            "Control_Max", "Control_Median", "Control_IQR", "Control_SampleSize")
write.csv(Clinic_Summary, file = paste(wd,"Results_Univariate/Clinic_Summary.csv", sep=""),row.names = FALSE)




################################################################################
### 4: Questionnaire data
# Set Load_1 to ordinal
quest_df$Clinic_question_job_load_1 <- factor(quest_df$Clinic_question_job_load_1,
                                              levels= c("low_load","moderate_load","heavy_load"), 
                                              ordered = TRUE)

## 4.1: Continuous/Ordinal variables
# 4.1.1: Anderson-Darling test for normality
# Anderson-Darling test was used as Kolmogorov Smirnov test is not suitable for ties
Quest_Distribution <- data.frame()
for (i in c(6:10,14,15,21:28)){
  tmp <- as.numeric(unlist(quest_df[,i]))
  AD_pain <- ad.test(tmp[quest_df$Demo_clinic_LBP == "yes"])
  AD_control <- ad.test(tmp[quest_df$Demo_clinic_LBP == "no"])
  Quest_Distribution <- rbind(Quest_Distribution, cbind(colnames(quest_df)[i], AD_pain[1],AD_pain[2], AD_control[1], AD_control[2]))
}
Quest_Distribution <- cbind(Quest_Distribution, 
                             p.adjust(Quest_Distribution[,3], method = "holm", n = nrow(Quest_Distribution)*2),
                             p.adjust(Quest_Distribution[,5], method = "holm", n = nrow(Quest_Distribution)*2))
colnames(Quest_Distribution) <- c("Feature", "BackPain_AD_A-Value", "BackPain_AD_p-Value", "Control_AD_A-Value",
                                   "Control_AD_p-Value", "BackPain_AD_FWE_p-Value", "Control_AD_FWE_p-Value")
Quest_Distribution <- sapply(Quest_Distribution, unlist)
write.csv(Quest_Distribution, file = paste(wd,"Results_Univariate/Quest_Distribution.csv", sep=""),row.names = FALSE)
rm(i, tmp, AD_pain, AD_control)


# 4.1.2: Wilcox test (aka Mann-Whitney U Test)
# Non-parametric Wilcoxon-Mann-Whitney test to determine significant difference between 
# cLBP patients and asymptomatic controls. Hence, u-values, z-values, r-value (effect sizes), 
# as well as the p-value are reported from the Wilcoxon-Mann-Whitney test
Quest_Results <- data.frame()
for (i in c(6:10,14,15,21:28)){
  data_tmp <- data.frame(quest_df$Demo_clinic_LBP, as.numeric(unlist(quest_df[,i])))
  colnames(data_tmp) <- c("Demo_clinic_LBP","Variable")
  tmp <- wilcox_test(data = data_tmp, Variable~Demo_clinic_LBP)
  Quest_Results <- rbind(Quest_Results, cbind(colnames(quest_df)[i], statistic(tmp, type = "linear"),
                                                statistic(tmp, "standardized"),
                                                pvalue(tmp)))
}
Quest_Results <- cbind(Quest_Results, p.adjust(Quest_Results[,4], method = "holm", n = ncol(quest_df)-5))
Quest_Results <- cbind(Quest_Results, as.numeric(Quest_Results[,3])/sqrt(nrow(quest_df)))
colnames(Quest_Results) <- c("Feature", "u-Value", "z-value", "p-value", "FWE_p-value", "r-value")
Quest_Results <- sapply(Quest_Results, unlist)
rm(tmp, i , data_tmp)
write.csv(Quest_Results, file = paste(wd,"Results_Univariate/Quest_Results.csv", sep=""),row.names = FALSE)


## 4.2: Discrete variables
# 4.2.1: Chi square test 
# Nominal data were compared using the Chi-Square test and reported using Chi2 values, 
# Cohen’s ω-values (effect size), and p-values.
Quest_Results_Nominal <- data.frame()
for (i in c(11:13,16:20)){
  data_tmp <- data.frame(cbind(quest_df$Demo_clinic_LBP, as.numeric(unlist(quest_df[,i]))))
  colnames(data_tmp) <- c("Demo_clinic_LBP","Variable")
  tmp <- chisq.test(data_tmp$Variable, data_tmp$Demo_clinic_LBP, simulate.p.value = TRUE)
  Quest_Results_Nominal <- rbind(Quest_Results_Nominal, cbind(colnames(quest_df)[i], tmp[1], tmp[2],nrow(quest_df),cohenW(data_tmp$Variable, data_tmp$Demo_clinic_LBP), tmp[3]))
  }
Quest_Results_Nominal <- cbind(Quest_Results_Nominal, p.adjust(as.numeric(unlist(Quest_Results_Nominal[6]), method = "holm", n = ncol(quest_df)-5)))
colnames(Quest_Results_Nominal) <- c("Feature", "x-value", "DF", "n", "Cohen's_ω", "p-value", "FWE_p-value")
Quest_Results_Nominal <- sapply(Quest_Results_Nominal, unlist)
write.csv(Quest_Results_Nominal, file = paste(wd,"Results_Univariate/Quest_Results_Nominal.csv", sep=""),row.names = FALSE)
rm(tmp, i, data_tmp)


## 4.3: Summary stats -> mean, sd, min, max, sample size ##
for (i in c("yes","no")){
  tmp <- quest_df[quest_df$Demo_clinic_LBP == i,] %>%
    select(where(is.numeric)) %>%
    pivot_longer(everything())%>%
    group_by(name) %>%
    summarise(mean = mean(value),
              sd = sd(value),
              minimum = min(value),
              maximum = max(value),
              median = median(value),
              IQR = IQR(value),
              sample_size = n()) 
  if (i == "yes"){Quest_Summary <-tmp}
  else{Quest_Summary <- cbind(Quest_Summary, tmp[, 2:8])}
}
rm(tmp, i)
colnames(Quest_Summary) <- c("Feature", "Patient_Mean", "Patient_SD", "Patient_Min", 
                              "Patient_Max", "Patient_Median", "Patient_IQR", "Patient_SampleSize",
                              "Control_Mean", "Control_SD", "Control_Min", 
                              "Control_Max", "Control_Median", "Control_IQR", "Control_SampleSize")
write.csv(Quest_Summary, file = paste(wd,"Results_Univariate/Quest_Summary.csv", sep=""),row.names = FALSE)




################################################################################
### 5: Back shape and function (spine mouse)
## 5.1: Continuous/Ordinal variables
# 5.1.1: Anderson-Darling test for normality
# Anderson-Darling test was used as Kolmogorov Smirnov test is not suitable for ties
SM_Distribution <- data.frame()
for (i in c(6:53)){
  tmp <- as.numeric(unlist(sm_sum_df[,i]))
  AD_pain <- ad.test(tmp[sm_sum_df$Demo_clinic_LBP == "yes"])
  AD_control <- ad.test(tmp[sm_sum_df$Demo_clinic_LBP == "no"])
  SM_Distribution <- rbind(SM_Distribution, cbind(colnames(sm_sum_df)[i], AD_pain[1],AD_pain[2], AD_control[1], AD_control[2]))
}
SM_Distribution <- cbind(SM_Distribution, 
                            p.adjust(SM_Distribution[,3], method = "holm", n = nrow(SM_Distribution)*2),
                            p.adjust(SM_Distribution[,5], method = "holm", n = nrow(SM_Distribution)*2))
colnames(SM_Distribution) <- c("Feature", "BackPain_AD_A-Value", "BackPain_AD_p-Value", "Control_AD_A-Value",
                                  "Control_AD_p-Value", "BackPain_AD_FWE_p-Value", "Control_AD_FWE_p-Value")
SM_Distribution <- sapply(SM_Distribution, unlist)
write.csv(SM_Distribution, file = paste(wd,"Results_Univariate/SM_Distribution.csv", sep=""),row.names = FALSE)
rm(i, tmp, AD_pain, AD_control)


# 5.1.2: Wilcox test (aka Mann-Whitney U Test)
# Non-parametric Wilcoxon-Mann-Whitney test to determine significant difference between 
# cLBP patients and asymptomatic controls. Hence, u-values, z-values, r-value (effect sizes), 
# as well as the p-value are reported from the Wilcoxon-Mann-Whitney test
SM_Results <- data.frame()
for (i in c(6:53)){
  data_tmp <- data.frame(sm_sum_df$Demo_clinic_LBP, as.numeric(unlist(sm_sum_df[,i])))
  colnames(data_tmp) <- c("Demo_clinic_LBP","Variable")
  tmp <- wilcox_test(data = data_tmp, Variable~Demo_clinic_LBP)
  SM_Results <- rbind(SM_Results, cbind(colnames(sm_sum_df)[i], statistic(tmp, type = "linear"),
                                              statistic(tmp, "standardized"),
                                              pvalue(tmp)))
}
SM_Results <- cbind(SM_Results, p.adjust(SM_Results[,4], method = "holm", n = nrow(SM_Results)))
SM_Results <- cbind(SM_Results, as.numeric(SM_Results[,3])/sqrt(nrow(sm_sum_df)))
colnames(SM_Results) <- c("Feature", "u-Value", "z-value", "p-value", "FWE_p-value", "r-value")
SM_Results <- sapply(SM_Results, unlist)
rm(tmp, i , data_tmp)
write.csv(SM_Results, file = paste(wd,"Results_Univariate/SM_Results.csv", sep=""),row.names = FALSE)


## 5.2: Summary stats -> mean, sd, min, max, sample size ##
for (i in c("yes","no")){
  tmp <- sm_sum_df[sm_sum_df$Demo_clinic_LBP == i,] %>%
    select(where(is.numeric)) %>%
    pivot_longer(everything())%>%
    group_by(name) %>%
    summarise(mean = mean(value),
              sd = sd(value),
              minimum = min(value),
              maximum = max(value),
              median = median(value),
              IQR = IQR(value),
              sample_size = n()) 
  if (i == "yes"){SM_Summary <-tmp}
  else{SM_Summary <- cbind(SM_Summary, tmp[, 2:8])}
}
rm(tmp, i)
colnames(SM_Summary) <- c("Feature", "Patient_Mean", "Patient_SD", "Patient_Min", 
                             "Patient_Max", "Patient_Median", "Patient_IQR", "Patient_SampleSize",
                             "Control_Mean", "Control_SD", "Control_Min", 
                             "Control_Max", "Control_Median", "Control_IQR", "Control_SampleSize")
write.csv(SM_Summary, file = paste(wd,"Results_Univariate/SM_Summary.csv", sep=""),row.names = FALSE)
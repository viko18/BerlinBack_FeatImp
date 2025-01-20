# Berlin Back Feature Importance in cLBP Diagnosis

This repository contains the code used for the manuscript titled:

**Integrating Multidimensional Data Analytics for Precision Diagnosis and Personalized Treatment of Chronic Low Back Pain** [medRxiv] (https://www.medrxiv.org/content/10.1101/2024.10.29.24316352v1)

**Sam Vickery1**, Frederick Junker1, Rebekka Döding1, Daniel L Belavy1, Maia Angelova2,3, Chandan Karmakar2, Luis Becker4,5, Nima Taheri4,5, Matthias Pumberger4, Sandra Reitmaier5, Hendrik Schmidt5 

1 Fachbereich Pflege-, Hebammen- und Therapiewissenschaften (PHT) | Hochschule Bochum (University of Applied Sciences) | Bochum | Germany 

2 Aston Digital Futures Institute | Aston University| Birmingham | United Kingdom 

3 School of Information Technology | Deakin University | Geelong | Australia 

4 Center for Musculoskeletal Surgery, Charité – Universitätsmedizin Berlin, Berlin, Germany

5 Julius Wolff Institut, Berlin Institute of Health - Charité at Universitätsmedizin Berlin | Berlin | Germany

Furhter information regarding the Berlin Back study and the dataset we used can be found [HERE](https://jwi.charite.de/en/research/research_organ_level_biomechanics/spine_biomechanics/spine_study/)

## Directory Information
- `01_preproc_clean` contains scripts preprocessing, cleaning (removing missing values), dividing Berlin Back dataset into modality datastes, and creating sample distribution plots
- `02_feat_imp_model` contains scripts for running the feature selection and classificaion model functions located in `func` and summarise the model outputs
- `03_holdout_model` Uses the most robust and important features for classification in holdout (10%) sample
- `04_univariate_stats` contains the script used for conducting univariate group (cLBP pateint vs control) statistics that was written by Dr Frederick Junker (Frederick.Junker@hs-gesundheit.de)
- `func` Functions used for feature importance selection and machine learning modelling


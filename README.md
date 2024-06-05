#Project Title: Subgroup Analysis Using Latent Profile Analysis (LPA) and Logistic Regression

#Overview:
This project aims to explore the heterogeneity among patients to improve personalized treatment strategies. 
The research is part of a collaborative project between SSBS and HCJMRI in Pune.
The analysis involves three main steps: Latent Profile Analysis (LPA), Multiple Logistic Regression (MLR), and prediction validation.

About the script: The study utilized data from the participants of the HCJMRI cohort, who were divided into Group A and Group B. To identify subgroups 
Group A participants were subjected to LPA using non-collinear input factors. The obtained subgroups were studied for their association with complications.
A predictive model was built based on these findings to identify at-risk cases 

#Dependencies:
All statistical analyses were performed using R (Version 4.3.1) 

Packages used: Refer the scripts

Data Availaility: Available on request

Steps of Analysis:
1. Latent Profile Analysis (LPA):
LPA clusters participants into distinct subgroups and can also impute missing values. It relies on various assumtions of variances and covariances to employ different models for identifying subgroups,
aiding in the identification of different risk profiles.

2. Multivariate Logistic Regression (MLR):
MLR develops a predictive model based on significant input variables identified from LPA, determining class membership with high accuracy.

3. Prediction Validation:
The logistic regression model is validated on an independent dataset to ensure robustness and reliability in predicting class assignments

Contact: Ruchi: ruchipunjabi27@gmail.com

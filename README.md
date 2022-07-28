# Master Thesis
This repository contains the code and data for the Master Thesis Project: Peer influences on online incivility. The raw data (not available due to sensitivity reasons) was collected from the subreddit R/politiek on May 12th, 2022. The pseudonymised data can be found in the excel file "df" . 

# Content of the scripts

File name: thesis_cleaning.r
In this file, the R script for the first part of the research can be found. It includes the use of the Reddit API, the first cleaning steps, anonymising the data and creating random coding samples. The raw data is not included as it contained the user names and URLs to the posts. 

File name: thesis_preprocessing.r
This file contains the R script of calculating the Krippendorf’s alpha, preprocessing steps for text analysis
Accompanying files needed: “icr_sample_c_r.xlsx”, “icr_sample_c_m.xlsx”, "coding_sample_c.xlsx" . 

File name: fasttext_analysis.py
The python script for the fastText classifier (including word-embeddings) can be found in this file. In order to run the code, either “coded_min.xlsx” is needed for the data with minimal pre-processing steps, or “coded_final.xlsx” for the final dataset with additional pre-processing steps. 
For the preliminary results of the classifier as reported in the thesis, change the file name in line 11 to “coded_min.xlsx”, and the hyperparameters to “lr=0.1” and “epoch=5” in lines 31 and 32 respectively. 

File name: thesis_regression.r
This code contains the construction of the variables from the manually coded data, the tests for the logistic regression assumptions, and the analysis itself. The dataset “df.xlsx” is required.  

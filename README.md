# Stroke Dataset Preprocessing and Analysis


## 📌 Project Overview

The objective of this project is to demonstrate the complete data preprocessing pipeline required before building machine learning models. Various data cleaning, transformation, validation, and exploratory analysis techniques are applied to improve data quality and prepare the dataset for predictive modeling.

## 📂 Dataset

The project uses the **Stroke Classification Dataset**, which contains **5,110 patient records** with **11 variables**. The target variable (`stroke`) indicates whether a patient has experienced a stroke (1 = Yes, 0 = No).

## ✨ Project Tasks

* Dataset loading and initial exploration
* Missing value creation and handling

  * Mode Imputation
  * Record Discard (NA Removal)
* Noise generation and noise handling
* Invalid data detection and validation
* Data normalization using Min-Max Scaling
* Outlier detection using the IQR method
* Descriptive statistical analysis and visualization
* Dataset splitting (80% Training, 20% Testing)
* Feature selection using Logistic Regression

## 🛠 Technologies Used

* **R Programming**
* tidyverse
* caret
* ggplot2
* dplyr

## 📊 Data Processing Techniques

* Missing Value Imputation
* Data Cleaning
* Noise Detection and Correction
* Data Validation
* Min-Max Normalization
* Outlier Detection (IQR)
* Descriptive Statistics
* Correlation Analysis
* Feature Importance Ranking

## 📁 Repository Structure

```text
├── Dataset/
│   ├── stroke.csv
│   ├── stroke_gender_missing.csv
│   ├── stroke_gender_mode_imputed.csv
│   ├── stroke_gender_discard.csv
│   ├── stroke_noise_added.csv
│   ├── stroke_noise_handled.csv
│   ├── stroke_normalized.csv
│   ├── stroke_train.csv
│   ├── stroke_test.csv
│   └── feature_selection.csv
├── IDS_Mid_Project.Rproj
├── stroke_project.R
├── download
├── Output/
├── README.md
```
### Outlier Detection
<img src="Output/Output (3).png">

## 📈 Key Learning Outcomes

* Understanding real-world data quality issues
* Applying preprocessing techniques for machine learning
* Detecting and handling missing, noisy, and invalid data
* Performing normalization and outlier analysis
* Generating descriptive statistics and visualizations
* Preparing datasets for predictive modeling
* Identifying the most influential features for stroke prediction

## 📚 Dataset Source

Stroke Classification Dataset (MLDataR Repository)

https://vincentarelbundock.github.io/Rdatasets/articles/data.html


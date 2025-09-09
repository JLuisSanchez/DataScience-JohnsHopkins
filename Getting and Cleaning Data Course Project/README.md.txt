# Getting and Cleaning Data - Final Project  
**Author:** Jose Luis  
**Date:** August 12, 2025  

This repository contains the final project for the **Getting and Cleaning Data** course.  
It processes and cleans the *Human Activity Recognition Using Smartphones Dataset* to produce a tidy dataset suitable for later analysis.

---

## **Project Files**

### 1. `run_analysis.R`  
Main R script that:
1. **Downloads** the original dataset if it is not present in the working directory.
2. **Reads** and **merges** the training and test datasets into a single dataset.
3. **Extracts** only the measurements containing mean (`mean()`) and standard deviation (`std()`).
4. **Replaces** numeric activity codes with descriptive names (WALKING, SITTING, etc.).
5. **Cleans and renames** variables with clear, descriptive names.
6. **Groups** data by subject and activity to calculate the **average** of each measurement.
7. **Saves** the final tidy dataset (`tidy_data.txt`) in the required format using `write.table(..., row.name = FALSE)`.

---

### 2. `tidy_data.txt`  
A space-delimited text file containing the final tidy dataset.  
**Features:**
- 180 rows (30 subjects × 6 activities).
- 68 columns (2 identifiers + 66 averaged measurements).
- Each row represents the mean of each variable for a unique subject–activity pair.

---

### 3. `CodeBook.md`  
Document describing:
- Variables and their units.
- Data source and original file descriptions.
- The transformations applied to produce the tidy dataset.
- Specific details about preprocessing.

---

## **How to Reproduce the Analysis**

1. Clone or download this repository.
2. Open **R** or **RStudio** and set your working directory to the project folder.
3. Run:


4. The script will download (if needed) and process the data, generating the file `tidy_data.txt`.

---

## **About the Original Dataset**
- **Name:** Human Activity Recognition Using Smartphones Dataset.
- **Source:** UCI Machine Learning Repository.
- **Description:** Data obtained from the accelerometer and gyroscope of a Samsung Galaxy S II smartphone, capturing motion metrics of 30 volunteers performing 6 different activities.

---

## **License**
The original dataset is licensed as per UCI repository guidelines.  
The code and data processing scripts in this repository may be reused for educational and research purposes.

---

# Introduction

This is my submission for Peer Graded Assignment: Getting and Cleaning Data Course Project, applied on the "Human Activity Recognition Using Smartphones Dataset" Version 1.0.


# Raw data
Raw Data was downloaded from this link https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip.
For this analysis, only the following data sets were used
* activity_labels.txt
Links the class labels with their activity name
* features.txt
List of all features
* X_train.txt
Training set
* y_train.txt
Training labels
* subject_train.txt
Subjects who performed the activity for each window sample for the training set
* X_test.txt
Test set
* y_test.txt
Test labels
* subject_test.txt
Subjects who performed the activity for each window sample for the test set


# Analysis
## Objective
The objective of this analysis was to produce a tidy data set from raw data showing the average of each variable for each activity and each subject.

## Tools
The analysis was performed using an R script:
- R version 3.4.1
- OS : Windows 7 Enterprise, SP 1
- dplyr package shall be installed

## Data transformation steps (run_analysis.R script description)
1. Download the dataset and unzip the file if it's not available in the working directory
2. Read the activity labels from activity_labels.txt
3. Reorder the activity factor levels to match the indexes
4. Read the feature names from features.txt.
5. Prepare the training set
* Read the training data from X_train.txt
* Read the training activities from y_train.txt
* Read the training subjects from subject_train.txt
* Set the variable names in the training set from the loaded features
* Extract only variables containing "mean()" or "std()" strings in their names, and overload the training set with the resultion data frame
* Convert the training activity ids to activity names
* Name the activity column and add it at the begining of the training data frame
* Name the subject column and add it at the begining of the training data frame
6. Prepare the test set
* exact same operations performed in step (5) using equivalent "test" data
7. Merge the training and test data into one data frame called "merged_data"
8. Set explicit variable names in the resulting data frame
9. Build the tidy data set by grouping the merged data by subject id and by activity; resulting variables are the mean value of the grouped variables
10. Write the resulting data frame into "tidy.txt" file.
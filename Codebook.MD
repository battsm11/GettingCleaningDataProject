---
title: "Codebook"
author: "Mike Battreall"
date: "May 29, 2016"
output: html_document
---

# Getting and Cleaning Data Project Code Book

##Description
This codebook describes the process undergone to complete the Coursera Data Science Getting and Cleaning Data course Project by Johns Hopkins

##Source Data

Description found here: http://archive.ics.uci.edu/ml/datasets/Human+Activity+Recognition+Using+Smartphones

Source Data found here: 
https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip

## Information about the Data Set

The experiments have been carried out with a group of 30 volunteers within an age bracket of 19-48 years. Each person performed six activities (WALKING, WALKING_UPSTAIRS, WALKING_DOWNSTAIRS, SITTING, STANDING, LAYING) wearing a smartphone (Samsung Galaxy S II) on the waist. Using its embedded accelerometer and gyroscope, we captured 3-axial linear acceleration and 3-axial angular velocity at a constant rate of 50Hz. The experiments have been video-recorded to label the data manually. The obtained dataset has been randomly partitioned into two sets, where 70% of the volunteers was selected for generating the training data and 30% the test data. 

The sensor signals (accelerometer and gyroscope) were pre-processed by applying noise filters and then sampled in fixed-width sliding windows of 2.56 sec and 50% overlap (128 readings/window). The sensor acceleration signal, which has gravitational and body motion components, was separated using a Butterworth low-pass filter into body acceleration and gravity. The gravitational force is assumed to have only low frequency components, therefore a filter with 0.3 Hz cutoff frequency was used. From each window, a vector of features was obtained by calculating variables from the time and frequency domain.

For each record in the dataset it is provided: 
- Triaxial acceleration from the accelerometer (total acceleration) and the estimated body acceleration. 
- Triaxial Angular velocity from the gyroscope. 
- A 561-feature vector with time and frequency domain variables. 
- Its activity label. 
- An identifier of the subject who carried out the experiment.

### Part 1
Read in the following tables, assign column names and merge into one data set:

*features.txt
*activity_labels.txt
*subject_train.txt
*x_train.txt
*y_train.txt
*subject_test.txt
*x_test.txt
*y_test.txt

###Part 2
Take only measurements on the mean and standard deviations

This was done by creating a logical vector that will return TRUE on the column names that contain "mean" or "stdev". The other columns return false and will not be included.

### Part 3
Merge this new data set with the "activitylabels" table so that the activity will be included in the data set for each observation

### Part 4
Clean up the column names so that they fall under tidy data standards. This includes getting rid of excess characters and changing names to make them more descriptive

### Part 5

Create a second table that aggregates the data into averages based on each activity and subject



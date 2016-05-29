##Goal: Create a function that follows the 5 guidelines below
##1) Merges the training and the test sets to create one data set.
##2) Extracts only the measurements on the mean and standard deviation for each measurement.
##3) Uses descriptive activity names to name the activities in the data set
##4) Appropriately labels the data set with descriptive variable names.
##5) From the data set in step 4, creates a second, 
##   independent tidy data set with the average of each variable for each activity and each subject.
library(plyr)


## download and unzip file

filename <- "getdata_dataset.zip"

if (!file.exists(filename)){
  fileURL <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
  download.file(fileURL, filename, method= "curl")
}
if (!file.exists("UCI HAR Dataset")){
  unzip(filename)
}
##set wd to UCI HAR Dataset 
setwd("UCI HAR Dataset")
## 1) read and merge the datasets

##read all tables into R
features = read.table('./features.txt',header=FALSE); #imports features.txt
activitylabels <- read.table("activity_labels.txt")
  #training data
xtrain <- read.table("train/X_train.txt")
ytrain <- read.table("train/y_train.txt")
subjecttrain <- read.table("train/subject_train.txt")

    #label cols
colnames(xtrain)        = features[,2]; 
colnames(ytrain)        = "activityId";
colnames(subjecttrain)  = "subjectId";

#merge training data (Y data, subject, x data)
trainingdata <- cbind(ytrain, subjecttrain, xtrain)

  #test data
xtest <- read.table("test/X_test.txt")
ytest <- read.table("test/y_test.txt")
subjecttest <- read.table("test/subject_test.txt")

  #colnames
colnames(xtest)       = features[,2] 
colnames(ytest)       = "activityId"
colnames(subjecttest) = "subjectId"

#merge test data (Y data, subject, x data)
testdata <- cbind(ytest, subjecttest, xtest)

#merge test and subject data

finalData <- rbind(trainingdata, testdata)

## 2) extract the mean and SD for each measurement

colNames = colnames(finalData) #vector of column names to find mean and SD

#create a logical vector where subjectID mean and SD are TRUE so I can extract the true cols
logicalvector = (grepl("activity..",colNames) | grepl("subject..",colNames) | grepl("-mean..",colNames) & !grepl("-meanFreq..",colNames) & !grepl("mean..-",colNames) | grepl("-std..",colNames) & !grepl("-std()..-",colNames))

finalData= finalData[logicalvector == TRUE]

## 3) add descriptive labels

#load activity table

activitylabels <- read.table("activity_labels.txt")
#label columns
colnames(activitylabels)  = c('activityId','activityType')

#merge this table with finalData

finalData <- merge(finalData, activitylabels, by="activityId", all.x=TRUE)
colNames= colnames(finalData)

## 4) clean up label names
## Need to eliminate "-", (), and double body; change t-> time, f-> freq, mag -> magnitude, 
## use a loop to go through the colNames vector and change the parts necessary

for (i in 1:length(colNames)){
  colNames[i] = gsub("\\()","",colNames[i])
  colNames[i] = gsub("-std$","StdDev",colNames[i])
  colNames[i] = gsub("-mean","Mean",colNames[i])
  colNames[i] = gsub("^(f)","freq",colNames[i])
  colNames[i] = gsub("^(t)", "time", colNames[i])
  colNames[i] = gsub("([Bb]ody[Bb]ody|[Bb]ody)","Body",colNames[i])
  colNames[i] = gsub("[Mm]ag", "Magnitude", colNames[i])
  
}

#apply these to the data set

colnames(finalData) = colNames


## 5) create a second tidy data set with the average of each var for each activity and subject

##eliminate activity type

finalDataNoAct = finalData[,names(finalData) !="activityType"]

##means for each var by activity and subject ID
tidyData = aggregate(finalDataNoAct[,names(finalDataNoAct) != c("activityId", "subjectId")], by = list(activityId= finalDataNoAct$activityId, subjectId = finalDataNoAct$subjectId),mean)

## add in the descriptives made previously

tidyData = merge(tidyData, activitylabels, by= "activityId", all.x = TRUE)

#create a file with this data set

write.table(tidyData, "./tidyData.txt", row.names=TRUE, sep='\t')
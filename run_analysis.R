

############################################################
#### Created by: Andres Suquillo
#### Coursera Getting and Cleaning Data: Final Assignment
#### Date: 25/03/2021
############################################################

#Load the necessary packages for the assignment
if (!require("data.table")) {
  install.packages("data.table")
}

if (!require("dplyr")) {
  install.packages("dplyr")
}

require("data.table")
require("dplyr")




########## Read feature (name of the features) and activity_labels (name of the activities).
featureNames <- read.table("UCI HAR Dataset/features.txt")
activityLabels <- read.table("UCI HAR Dataset/activity_labels.txt", header = FALSE)

########## Read training and test data sets are split up into subject, activity and features.
### Read training data
subjectTrain <- read.table("UCI HAR Dataset/train/subject_train.txt", header = FALSE)
activityTrain <- read.table("UCI HAR Dataset/train/y_train.txt", header = FALSE)
featuresTrain <- read.table("UCI HAR Dataset/train/X_train.txt", header = FALSE)

### Read test data
subjectTest <- read.table("UCI HAR Dataset/test/subject_test.txt", header = FALSE)
activityTest <- read.table("UCI HAR Dataset/test/y_test.txt", header = FALSE)
featuresTest <- read.table("UCI HAR Dataset/test/X_test.txt", header = FALSE)

####################################################################################################
########## 1.- Merges the training and the test sets to create one data set.
####################################################################################################
### rbind subject, activity and features of training and test data sets
subject <- rbind(subjectTrain, subjectTest)
activity <- rbind(activityTrain, activityTest)
features <- rbind(featuresTrain, featuresTest)

### columns of features with featureNames 
colnames(features) <- t(featureNames[2])

### merge data features,activity and subject
colnames(activity) <- "Activity"
colnames(subject) <- "Subject"
mergeData <- cbind(features,activity,subject)

####################################################################################################
########## 2.- Extracts only the measurements on the mean and standard deviation for each measurement.
####################################################################################################
### Extract the column indices that have either mean or std in them.
columnsMeanSTD <- grep(".*Mean.*|.*Std.*", names(completeData), ignore.case=TRUE)

### Add activity and subject columns
IndexColumns <- c(columnsMeanSTD, 562, 563)

### extract data with the selected columns in IndexColumns
dataNew <- completeData[,IndexColumns]


####################################################################################################
########## 3.- Uses descriptive activity names to name the activities in the data set.
####################################################################################################
str(dataNew)
### Change Activity to character and rename with names of activityLabels.
dataNew$Activity <- as.factor(activityLabels[dataNew$Activity,2])


####################################################################################################
########## 4.- Appropriately labels the data set with descriptive variable names.
####################################################################################################
### Replaced Acc with Accelerometer, Gyro with Gyroscope, BodyBody with Body, Mag with Magnitude, 
### f with Frequency and t with Time.
names(dataNew)

names(dataNew)<-gsub("Acc", "Accelerometer", names(dataNew))
names(dataNew)<-gsub("Gyro", "Gyroscope", names(dataNew))
names(dataNew)<-gsub("BodyBody", "Body", names(dataNew))
names(dataNew)<-gsub("Mag", "Magnitude", names(dataNew))
names(dataNew)<-gsub("^t", "Time", names(dataNew))
names(dataNew)<-gsub("^f", "Frequency", names(dataNew))
names(dataNew)<-gsub("tBody", "TimeBody", names(dataNew))
names(dataNew)<-gsub("-mean()", "Mean", names(dataNew), ignore.case = TRUE)
names(dataNew)<-gsub("-std()", "STD", names(dataNew), ignore.case = TRUE)
names(dataNew)<-gsub("-freq()", "Frequency", names(dataNew), ignore.case = TRUE)
names(dataNew)<-gsub("angle", "Angle", names(dataNew))
names(dataNew)<-gsub("gravity", "Gravity", names(dataNew))

names(dataNew)


####################################################################################################
########## 5.- From the data set in step 4, creates a second, independent tidy data set with the 
##########    average of each variable for each activity and each subject.
####################################################################################################
### Apply mean function to dataset using dcast function
tidy_data <- aggregate(. ~Subject + Activity, dataNew, mean)

### Order the enties in tidy_data and write it into data file Tidy.txt that contains the processed data.
tidy_data <- tidy_data[order(tidy_data$Subject,tidy_data$Activity),]
write.table(tidy_data, "TidyData.txt", row.names = FALSE)

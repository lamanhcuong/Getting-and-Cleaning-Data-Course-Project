library(plyr)

## Assume "./UCI HAR Dataset" is the folder where UCI HAR data sets are stored (i.e. unzip the downloaded zip file into this folder)
## temporarily set working directory to UCI HAR data folder for this script for convenient access to data file
setwd("./UCI HAR Dataset")

## Step 1 
## Merges the training and the test sets to create one data set
##****************************************************************##

## Get train data  
obs_train <- read.table("train/X_train.txt")
act_train <- read.table("train/y_train.txt")
sub_train <- read.table("train/subject_train.txt")

obs_test <- read.table("test/X_test.txt")
act_test <- read.table("test/y_test.txt")
sub_test <- read.table("test/subject_test.txt")

## Merge observed (x) data set (from train and test data set)
obs_mdata <- rbind(obs_train, obs_test)

## Merge activity (y) data set (from train and test data set)
act_mdata <- rbind(act_train, act_test)

## Merge subject data set (from train and test data set)
sub_mdata <- rbind(sub_train, sub_test)

## Step 2
## Extracts only the measurements on the mean and standard deviation for each measurement
##****************************************************************##

## Read all features (corresponding columns) in observed (x) data set
features <- read.table("features.txt")

## Get only columns with mean() or std() in their names (wanted features).  
## Note the ordering numbers of wanted columns (featured) to be got from obs_mdata set are in column #1 of features table, 
## they are not the (physical) row numbers of features table
## fealist0: (physical) rows numbers in features table for which columns (features) to be got
## fealist1: actual numbers of wanted columns to be got from obs_mdata set

fealist0 <- grep("(mean|std)\\(\\)", features[, 2]) 
fealist1 <- features[fealist0, 1]
feacount <- length(fealist0)

## subset the desired columns
obs_mdata <- obs_mdata[, fealist1]

## Step 3
## Uses descriptive activity names to name the activities in the data set
##****************************************************************##

## Read activity label 
activities <- read.table("activity_labels.txt")

## Update activity code (number) in activity data set with activity names
## The match function is to asssure correct activity name is updated even if activity codes (numbers) are not sorted in activity_labels.txt file
act_mdata[, 1] <- activities[match(act_mdata[, 1], activities[, 1]), 2]

## Step 4
## Appropriately labels the data set with descriptive variable names
##****************************************************************##

## Update correct column (variable) names for observed (x) data set
names(obs_mdata) <- features[fealist0, 2]

## Update correct column (variable) name for subject
names(sub_mdata) <- "subject"

## Update correct column (variable) name for activity
names(act_mdata) <- "activity"

## Step 5
## creates a second, independent tidy data set with the average of each variable for each activity and each subject
##****************************************************************##

## bind all the data in a single data set
all_data <- cbind(obs_mdata, act_mdata, sub_mdata)

# 1 <- 66 columns but first two (activity & subject)
tidy_data <- ddply(all_data, .(subject, activity), function(x) colMeans(x[, 1:feacount]))
write.table(tidy_data, "tidy_data.txt", row.name=FALSE)

## Set working directory back to original one before script
setwd("../")

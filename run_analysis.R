#This program is for the Getting and Cleaning Data Assignement Project of Coursera
#July 5th, 2016

#Project scope:
#You should create one R script called run_analysis.R that does the following.

# 1 Merges the training and the test sets to create one data set.
# 2 Extracts only the measurements on the mean and standard deviation for each measurement.
# 3 Uses descriptive activity names to name the activities in the data set
# 4 Appropriately labels the data set with descriptive variable names.
# 5 From the data set in step 4, creates a second, independent tidy data set with
# the average of each variable for each activity and each subject.

#_____________________________________________________________________________________#
#Loading packages
rm(list=ls())
library(data.table)
library(dplyr)

#First of all, let us download the file with the source data

setwd("D:/brunofbessa/estudos/coursera/cleaning_data/assignment")
data_url <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
download.file( data_url, destfile = "source_data.zip" )

#Unziping the files
if(!file.exists("/source_data")){dir.create("./source_data")}
unzip("source_data.zip", overwrite = TRUE)

#seting data into R:
X_test <- read.table("UCI HAR Dataset/test/X_test.txt")
Y_test <- read.table("UCI HAR Dataset/test/Y_test.txt")
subject_test <- read.table("UCI HAR Dataset/test/subject_test.txt")
X_train <- read.table("UCI HAR Dataset/train/X_train.txt")
Y_train <- read.table("UCI HAR Dataset/train/Y_train.txt")
subject_train <- read.table("UCI HAR Dataset/train/subject_train.txt")
activity_labels <- read.table("UCI HAR Dataset/activity_labels.txt")



#The READ_ME.txt file says that the test and train datasets (X) have the same layout. This layout
#is specified in features.txt:
features <- read.table("UCI HAR Dataset/features.txt")
#The first column of feaures has the feature's index and the second coluumn has it's description
#So we have to transpose it in order to use the labels:
colnames(X_test) <- t(features[2])
colnames(X_train) <- t(features[2])

#Assigning column names for the other datasets
colnames(Y_test) <- "activity_id"
colnames(Y_train) <- "activity_id"
colnames(subject_test) <- "subject_id"
colnames(subject_train) <- "subject_id"
colnames(activity_labels) <- c("activity_id", "activity_type")

#Let us follow the project script

# 1 Merges the training and the test sets to create one data set.

#First we have make the train and test datasets have all the information needed in two objects


test_merged <- cbind ( X_test, subject_test, Y_test )
train_merged <- cbind ( X_train, subject_train, Y_train )

merged_data <- rbind ( test_merged, train_merged )

#We may use str() to se that the total number of rows is conserved

# 2 Extracts only the measurements on the mean and standard deviation for each measurement.
mean_std_logical <- grepl( "mean|std", features$V2 )
mean_std_data <- merged_data[ , mean_std_logical ]

# 3 Uses descriptive activity names to name the activities in the data set

# We will "join" the mean_std_data and activity_labels tables by activity_id to bring the 
# activity names
mean_std_data_2 = merge ( mean_std_data, activity_labels, by = "activity_id", all.x = TRUE )
mean_std_data_2 <- select ( mean_std_data_2, -activity_id  )

# 4 Appropriately labels the data set with descriptive variable names.

# Now we will have to use regular expressions to rename the variables on the dataset with gsub command

names_dataset <- names( mean_std_data_2 )

for ( i in 1:length(names_dataset) ){
        names_dataset[i] <- gsub("\\()", "", names_dataset[i] )
        names_dataset[i] <- gsub("^t", "Time", names_dataset[i] )
        names_dataset[i] <- gsub("^f", "Frequency", names_dataset[i] )
        names_dataset[i] <- gsub("Gyro", "Gyroscope", names_dataset[i] )
        names_dataset[i] <- gsub("Acc", "Acceleration", names_dataset[i] )
        names_dataset[i] <- gsub("Mag", "Magnitude", names_dataset[i] )
        names_dataset[i] <- gsub("std", "StandardDeviation", names_dataset[i] )
        names_dataset[i] <- gsub("X", "XAxis", names_dataset[i] )
        names_dataset[i] <- gsub("Y", "YAxis", names_dataset[i] )
        names_dataset[i] <- gsub("Z", "ZAxis", names_dataset[i] )
}

names( mean_std_data_2 ) <- names_dataset

# 5 From the data set in step 4, creates a second, independent tidy data set with
# the average of each variable for each activity and each subject.

# For this step we will use the group and summarise verbs of dplyr

tidy_data <- 
mean_std_data_2 %>% 
        group_by( subject_id, activity_type ) %>%
        summarise_each( funs( mean ) ) 
# Now writing this dataset on disk
write.table(tidy_data, 'tidy_data.txt',row.names=TRUE)
     
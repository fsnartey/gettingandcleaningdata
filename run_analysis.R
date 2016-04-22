###Download data from source

path <- file.path("~", "UCI HAR Dataset")
setwd(path)
if(!file.exists("./data")){dir.create("./data")}
fileUrl <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
download.file(fileUrl,destfile="./data/Dataset.zip",method="curl")


###Unzip DataSet to /data directory

unzip(zipfile="./data/Dataset.zip",exdir="./data")

###Load packages required for cleaning data
library(dplyr)
library(data.table)
library(tidyr)

###Read files and create datable

# Read subject files
SubjectTrain <- tbl_df(read.table(file.path(path, "train", "subject_train.txt")))
SubjectTest  <- tbl_df(read.table(file.path(path, "test" , "subject_test.txt" )))

# Read activity files
ActivityTrain <- tbl_df(read.table(file.path(path, "train", "Y_train.txt")))
ActivityTest  <- tbl_df(read.table(file.path(path, "test" , "Y_test.txt" )))

#Read data files.
Trainingdata <- tbl_df(read.table(file.path(path, "train", "X_train.txt" )))
Testdata  <- tbl_df(read.table(file.path(path, "test" , "X_test.txt" )))

# Merge traing and test sets by row binding and and remane variables appropriately 

combinedSubjectdata <- rbind(SubjectTrain, SubjectTest)
setnames(combinedSubjectdata, "V1", "subject")
combinedActivitydata<- rbind(ActivityTrain, ActivityTest)
setnames(combinedActivitydata, "V1", "activityNum")

#combine the DATA training and test files
dataTable <- rbind(Trainingdata, Testdata)

# name variables according to feature 
Features <- tbl_df(read.table(file.path(path, "features.txt")))
setnames(Features, names(Features), c("featureNum", "featureName"))
colnames(dataTable) <- Features$featureName

#column names for activity labels
activityLabels<- tbl_df(read.table(file.path(path, "activity_labels.txt")))
setnames(activityLabels, names(activityLabels), c("activityNum","activityName"))

# Merge columns
combinedSubcols<- cbind(combinedSubjectdata, combinedActivitydata)
dataTable <- cbind(combinedSubcols, dataTable)

# load "features.txt" and extract only the mean and standard deviation as required
FeaturesMeanStd <- grep("mean\\(\\)|std\\(\\)",Features$featureName,value=TRUE) 

# Extrating measurements for the mean and standard deviation and add "subject","activityNum"

FeaturesMeanStd <- union(c("subject","activityNum"), FeaturesMeanStd)
dataTable<- subset(dataTable,select=FeaturesMeanStd) 

##Use descriptive activity names to name the activities in the data set

dataTable <- merge(activityLabels, dataTable , by="activityNum", all.x=TRUE)
dataTable$activityName <- as.character(dataTable$activityName)

## create dataTable with variable means sorted by subject and Activity
dataTable$activityName <- as.character(dataTable$activityName)
dataAggr<- aggregate(. ~ subject - activityName, data = dataTable, mean) 
dataTable<- tbl_df(arrange(dataAggr,subject,activityName))

###Appropriately labels the data set with descriptive variable names
names(dataTable)<-gsub("std()", "SD", names(dataTable))
names(dataTable)<-gsub("mean()", "MEAN", names(dataTable))
names(dataTable)<-gsub("^t", "time", names(dataTable))
names(dataTable)<-gsub("^f", "frequency", names(dataTable))
names(dataTable)<-gsub("Acc", "Accelerometer", names(dataTable))
names(dataTable)<-gsub("Gyro", "Gyroscope", names(dataTable))
names(dataTable)<-gsub("Mag", "Magnitude", names(dataTable))
names(dataTable)<-gsub("BodyBody", "Body", names(dataTable))

###From the data set in step 4, creates a second, independent tidy data set with the average of each variable 
###for each activity and each subject.

write.table(dataTable, "TidyData.txt", row.name=FALSE)

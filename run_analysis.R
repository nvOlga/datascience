# Download required libraries
library(dplyr)

# Download the dataset

filename <- "data.zip"
fileURL <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
filePath <- "C:/Users/olga.nevinchana/Documents/Coursera/Getting_and_Cleaning_Data/Course_project/UCI HAR Dataset"

  ## Checking if archieve already exists.
  if (!file.exists(filename)) {download.file(fileURL, filename, method = "curl")}

  ## Cheking if folder already exists.
  if (!file.exists("UCI HAR Dataset")) {unzip(filename)}

  ## remove archive from directory.
  unlink("C:/Users/olga.nevinchana/Documents/Coursera/Getting_and_Cleaning_Data/Course_project/data.zip")
  
# Assigning all data frames
  
  features <- read.table("./UCI HAR Dataset/features.txt", col.names = c("n", "functions"))
  activities <- read.table("./UCI HAR Dataset/activity_labels.txt", col.names = c("n", "activities"))

  subject_test <- read.table("./UCI HAR Dataset/test/subject_test.txt", col.names = "subject")
  X_test <- read.table("./UCI HAR Dataset/test/X_test.txt", col.names = features$functions)
  y_test <- read.table("./UCI HAR Dataset/test/y_test.txt", col.names = "code")
                   
  subject_train <- read.table("./UCI HAR Dataset/train/subject_train.txt", col.names = "subject")
  X_train <- read.table("./UCI HAR Dataset/train/X_train.txt", col.names = features$functions)
  y_train <- read.table("./UCI HAR Dataset/train/y_train.txt", col.names = "code")
  
  
# Merging the training and the test sets to create one data set.
  
  X <- rbind(X_train, X_test)
  y <- rbind(y_train, y_test)
  Subject <- rbind(subject_train, subject_test)
  Merged_Data <- cbind(Subject, y, X)
  
  
# Extracting only the measurements on the mean and standard deviation for each measurement.
  
  TidyData <- Merged_Data %>% select(subject, code, contains("mean"), contains("std"))
  
  
# Using descriptive activity names to name the activities in the data set.
  
  TidyData$code <- activities[TidyData$code, 2] # assign values from column 2 of the activities df based on the code variable
  
  
# Labeling the data set with descriptive variable names.
  
  names(TidyData)[2] = "activity"
  names(TidyData) <- gsub("Acc", "Accelerometer", names(TidyData))
  names(TidyData) <- gsub("Gyro", "Gyroscope", names(TidyData))
  names(TidyData) <- gsub("BodyBody", "Body", names(TidyData))
  names(TidyData) <- gsub("Mag", "Magnitude", names(TidyData))
  names(TidyData) <- gsub("^t", "Time", names(TidyData))
  names(TidyData) <- gsub("^f", "Frequency", names(TidyData))
  names(TidyData) <- gsub("tBody", "TimeBody", names(TidyData))
  names(TidyData) <- gsub("-mean()", "Mean", names(TidyData), ignore.case = TRUE)
  names(TidyData) <- gsub("-std()", "STD", names(TidyData), ignore.case = TRUE)
  names(TidyData) <- gsub("-freq()", "Frequency", names(TidyData), ignore.case = TRUE)
  names(TidyData) <- gsub("angle", "Angle", names(TidyData))
  names(TidyData) <- gsub("gravity", "Gravity", names(TidyData))

  
# From the data set in step 4, create a second, independent tidy data set 
#  with the average of each variable for each activity and each subject.
  
  Res <- TidyData %>%
    group_by(subject, activity) %>%
    summarise_all(funs(mean))
  
  write.table(Res, "Result", row.name = FALSE)
  str(Res)
  
  
run_analysis <- function() {
  
  library(dplyr)
  
  if (!file.exists("UCI HAR Dataset")) {
    
    download.file("https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip", destfile = "Dataset.zip")
    unzip("Dataset.zip")
    
  }
  
  # Read activity labels
  activitylabels <- read.table("UCI HAR Dataset/activity_labels.txt")
  
  # Reorder the factor levels to match the indexes
  levels(activitylabels[[2]]) <- activitylabels[[2]]

  # Read the feature names
  featurelabels <- read.table("UCI HAR Dataset/features.txt", stringsAsFactors = F)

  # Training data #########
  training <- read.table("UCI HAR Dataset/train/X_train.txt")
  training_activities <- read.table("UCI HAR Dataset/train/y_train.txt")
  
  training_subjects <- read.table("UCI HAR Dataset/train/subject_train.txt")
  
  # Set the column names
  names(training) <- tolower(featurelabels[[2]])
  
  # Extract only mean and std measurments
  training <- training[,grep("mean\\(\\)|std\\(\\)", names(training))]
  
  # Convert activities ids to activities names
  training_activities[,1] <- levels(activitylabels[[2]])[training_activities[[1]]]
  training_activities[,1] <- as.factor(training_activities[,1])
  names(training_activities) <- "activity"
  
  # Add an activity column to training data
  training <- cbind(training_activities, training)
  
  # Add subject column to training data
  names(training_subjects) <- "subject_id"
  training <- cbind(training_subjects, training)
  
  # Test data #########
  test <- read.table("UCI HAR Dataset/test/X_test.txt")
  test_activities <- read.table("UCI HAR Dataset/test/y_test.txt")
  
  test_subjects <- read.table("UCI HAR Dataset/test/subject_test.txt")
  
  # Set the column names
  names(test) <- tolower(featurelabels[[2]])
  
  # Extract only mean and std measurments
  test <- test[,grep("mean\\(\\)|std\\(\\)", names(test))]
  
  # Convert activities ids to activities names
  test_activities[,1] <- levels(activitylabels[[2]])[test_activities[[1]]]
  test_activities[,1] <- as.factor(test_activities[,1])
  names(test_activities) <- "activity"
  
  # Add an activity column to test data
  test <- cbind(test_activities, test)
  
  # Add subject column to test data
  names(test_subjects) <- "subject_id"
  test <- cbind(test_subjects, test)
  
  # Merge training and test data set
  merged_data <- rbind(training, test)
  
  # Set explicit variable names
  names(merged_data) <- c("subject_id",
                          "activity",
                          "body acceleration-mean-x",
                          "body acceleration-mean-y",
                          "body acceleration-mean-z",
                          "body acceleration-standard deviation-x",
                          "body acceleration-standard deviation-y",
                          "body acceleration-standard deviation-z",
                          "gravity acceleration-mean-x",
                          "gravity acceleration-mean-y",
                          "gravity acceleration-mean-z",
                          "gravity acceleration-standard deviation-x",
                          "gravity acceleration-standard deviation-y",
                          "gravity acceleration-standard deviation-z",
                          "body jerk  acceleration-mean-x",
                          "body jerk  acceleration-mean-y",
                          "body jerk  acceleration-mean-z",
                          "body jerk  acceleration-standard deviation-x",
                          "body jerk  acceleration-standard deviation-y",
                          "body jerk  acceleration-standard deviation-z",
                          "body angular velocity-mean-x",
                          "body angular velocity-mean-y",
                          "body angular velocity-mean-z",
                          "body angular velocity-standard deviation-x",
                          "body angular velocity-standard deviation-y",
                          "body angular velocity-standard deviation-z" ,
                          "body jerk angular velocity-mean-x",
                          "body jerk angular velocity-mean-y",
                          "body jerk angular velocity-mean-z",
                          "body jerk angular velocity-standard deviation-x",
                          "body jerk angular velocity-standard deviation-y",
                          "body jerk angular velocity-standard deviation-z",
                          "body acceleration magnitude-mean",
                          "body acceleration magnitude-standard deviation",
                          "gravity acceleration magnitude-mean",
                          "gravity acceleration magnitude-standard deviation",
                          "body jerk acceleration magnitude-mean",
                          "body jerk acceleration magnitude-standard deviation",
                          "body angular velocity magnitude-mean",
                          "body angular velocity magnitude-standard deviation",
                          "body jerk angular velocity magnitude-mean",
                          "body jerk angular velocity magnitude-standard deviation",
                          "body acceleration fourier transform-mean-x",
                          "body acceleration fourier transform-mean-y",
                          "body acceleration fourier transform-mean-z",
                          "body acceleration fourier transform-standard deviation-x",
                          "body acceleration fourier transform-standard deviation-y",
                          "body acceleration fourier transform-standard deviation-z",
                          "body jerk acceleration fourier transform-mean-x",
                          "body jerk acceleration fourier transform-mean-y",
                          "body jerk acceleration fourier transform-mean-z",
                          "body jerk acceleration fourier transform-standard deviation-x",
                          "body jerk acceleration fourier transform-standard deviation-y",
                          "body jerk acceleration fourier transform-standard deviation-z",
                          "body angular velocity fourier transform-mean-x",
                          "body angular velocity fourier transform-mean-y",
                          "body angular velocity fourier transform-mean-z",
                          "body angular velocity fourier transform-standard deviation-x",
                          "body angular velocity fourier transform-standard deviation-y",
                          "body angular velocity fourier transform-standard deviation-z",
                          "body acceleration magnitude fourier transform-mean",
                          "body acceleration magnitude fourier transform-standard deviation",
                          "body jerk acceleration magnitude  fourier transform-mean",
                          "body jerk acceleration magnitude  fourier transform-standard deviation",
                          "body angular velocity magnitude fourier transform-mean",
                          "body angular velocity magnitude fourier transform-standard deviation",
                          "body jerk angular velocity magnitude fourier transform-mean",
                          "body jerk angular velocity magnitude fourier transform-standard deviation")
  
  # Build the tidy data by merging all variables by subject id and activity
  # resulting variables are the mean value of the grouped variables
  tidy_data <- merged_data %>% group_by(subject_id, activity) %>% summarise_all(funs(mean))
  
  write.table(tidy_data, "tidy.txt", row.names = FALSE)
  
}
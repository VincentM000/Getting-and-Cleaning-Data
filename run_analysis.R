#1 Merges the training and the test sets to create one data set.
subject_train <- read.table("C:/Users/Vincent/Desktop/UCI HAR Dataset/train/subject_train.txt", col.names = "subject")
subject_test <- read.table("C:/Users/Vincent/Desktop/UCI HAR Dataset/test/subject_test.txt", col.names = "subject")
subject <- rbind(subject_train, subject_test)

x_train <- read.table("C:/Users/Vincent/Desktop/UCI HAR Dataset/train/X_train.txt")
x_test <- read.table("C:/Users/Vincent/Desktop/UCI HAR Dataset/test/X_test.txt")
x_full <- rbind(x_train, x_test)

y_train <- read.table("C:/Users/Vincent/Desktop/UCI HAR Dataset/train/y_train.txt", col.names = "label")
y_test <- read.table("C:/Users/Vincent/Desktop/UCI HAR Dataset/test/y_test.txt", col.names = "label")
y_full <- rbind(y_train, y_test)

data <- cbind(subject, y_full, x_full)

#2 Extracts only the measurements on the mean and standard deviation for each measurement. 
features <- read.table("C:/Users/Vincent/Desktop/UCI HAR Dataset/features.txt")
features_names <- subset(features, grepl("mean\\()|std\\()", features$V2))
data_mean_std <- data[, 2+features_names$V1]
data_mean_std <- cbind(subject, y_full, data_mean_std)


#3 Uses descriptive activity names to name the activities in the data set
activity_labels <- read.table("C:/Users/Vincent/Desktop/UCI HAR Dataset/activity_labels.txt")
activity_labels$V2 <- tolower(activity_labels$V2)

data_mean_std$label <- as.character(data_mean_std$label)
for (i in 1:6) {
  data_mean_std$label[data_mean_std$label == i] <- as.character(activity_labels[i,2]) 
}

#4 Appropriately labels the data set with descriptive variable names. 
tfeatures_names = t(features_names)
vars = c(tfeatures_names[2,])
names(data_mean_std) <- c("subject", "activity", vars)

#5 From the data set in step 4, creates a second, independent tidy data set with the average of each variable
# for each activity and each subject.
avg_var <- aggregate(. ~ subject + activity, data_mean_std, mean)
avg_var <- avg_var[order(avg_var$subject, avg_var$activity),]
write.table(avg_var, file = "tidy.txt", row.names = FALSE)
#Rob Wiederstein
#Coursera-Leek-"Getting & Cleaning Data"
#Program Assignment--UCI HAR Data Set

#1. Merges the training and the test sets to create one data set.

#open subject_train.txt in train dir
file <- paste(getwd(), "train", "subject_train.txt", sep = "/")
subject_train <- read.table(file = file, sep = "")

#open subject_test.txt in test dir
file <- paste(getwd(), "test", "subject_test.txt", sep = "/")
subject_test <- read.table(file = file, sep = "", header = F)

df_subject <- rbind(subject_train, subject_test)

#open X_train.txt in train dir
file <- paste(getwd(), "train", "X_train.txt", sep = "/")
x_train <- read.table(file = file)

#open X_test.txt in text dir
file <- paste(getwd(), "test", "X_test.txt", sep = "/")
x_test <- read.table(file = file)

df_x <- rbind(x_train, x_test)

#open y_train.txt in train dir
file <- paste(getwd(), "train", "y_train.txt", sep = "/")
y_train <- read.table(file = file)

#open y_test.txt in test dir
file <- paste(getwd(), "test", "y_test.txt", sep = "/")
y_test <- read.table(file = file)

df_y <- rbind(y_train, y_test)

rm(file, subject_test, subject_train, x_test, x_train, y_test, y_train)

#2. Extracts only the measurements on the mean and standard deviation 
#   for each measurement.
df_x_mean <- apply(df_x, MARGIN = 2, mean)
df_x_stdv <- apply(df_x, MARGIN = 2, sd)

#3. Uses descriptive activity names to name the activities in the data set
file <- paste(getwd(), "activity_labels.txt", sep = "/")
activity_labels <- readLines(con = file)
library(stringr)
activity_labels <- str_split(activity_labels, pattern = " ")
df <- data.frame(matrix(unlist(activity_labels), nrow = 6, byrow = T))
names(df) <- c("activity_id", "activity")
names(df_y) <- c("activity_id")
df_y$activity_id <- as.character(df_y$activity_id)
df_y[which(df_y$activity_id == "1"), 1] <- "walking"
df_y[which(df_y$activity_id == "2"), 1] <- "walking_upstairs"
df_y[which(df_y$activity_id == "3"), 1] <- "walking_downstairs"
df_y[which(df_y$activity_id == "4"), 1] <- "sitting"
df_y[which(df_y$activity_id == "5"), 1] <- "standing"
df_y[which(df_y$activity_id == "6"), 1] <- "laying"

#4. Appropriately labels the data set with descriptive variable names.
file <- paste(getwd(), "features.txt", sep = "/")
feature_labels <- readLines(con = file)
names(df_x) <- feature_labels

#5. From the data set in step 4, creates a second, independent tidy data set 
#   with the average of each variable for each activity and each subject.

df <- cbind(df_subject, df_y, df_x)
#rename variables
names(df)[1] <- "subject_id"
pattern <- "^[0-9](.*) "
names(df) <- gsub(pattern, "", names(df))
nums <- c(paste("00", 1:9, sep = ""), 
          paste("0", 10:99, sep = ""),
          100:563)
names(df) <- paste(names(df), nums, sep = "_")

#wide to long--reshape package
library(reshape)
library(reshape2)
df.1 <- melt.data.frame(df, id.vars = c("subject_id_001", "activity_id_002"))

#mean for each variable-plyr package
library(plyr)
library(dplyr)
df.2 <- ddply(df.1, .(subject_id_001, activity_id_002, variable), summarize,
              mean = mean(value))

#write table out
file <- "leek_tidy_HAR.txt"
write.table(df.2, file = file, row.names = F)

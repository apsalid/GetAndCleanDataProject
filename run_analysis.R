library(dplyr)
## Downloaded the dataset locally, unzip and setwd
## 1. Merges the training and the test sets to create one data set.

## Load train and test datasets
X_test_df <- read.table("UCI HAR Dataset/test/X_test.txt")
y_test_df <- read.table("UCI HAR Dataset/test/y_test.txt")
subject_test_df <- read.table("UCI HAR Dataset/test/subject_test.txt")

X_train_df <- read.table("./UCI HAR Dataset/train/X_train.txt")
y_train_df <- read.table("./UCI HAR Dataset/train/y_train.txt")
subject_train_df <- read.table("./UCI HAR Dataset/train/subject_train.txt")

## First merge each relative dataset
X_all <- rbind(X_test_df,X_train_df)
y_all <- rbind(y_test_df,y_train_df)
subject_all <- rbind(subject_test_df,subject_train_df)

## Merge them all to a single dataset
single.dataset <- cbind(X_all,y_all,subject_all)

## 2. Extracts only the measurements on the mean and standard deviation for each measurement. 
## We will use dplyr
df <- tbl_df(single.dataset)

## The features table has the info on which columns to keep
feat <- read.table("UCI HAR Dataset/features.txt")
featdf <- tbl_df(feat)

## So these are the columns we want to keep: -mean and -std
columntokeep <- filter(featdf, grepl('-mean|-std', V2) )
## of course plus the y and subject columns
columntokeep <- c(columntokeep[[1]], 562, 563 )

## Subsetting now 
## I could use select(df, columntokeep ) but I will keep the flow 
## of the questions since the names of the columns will be changed later
df_sub <- df[,columntokeep]

## 3. Uses descriptive activity names to name the activities in the data set
activity <- read.table("UCI HAR Dataset/activity_labels.txt")
## This is the y column that is the one before the last, so 80
df_sub[,80] <- activity[ df_sub[[80]] , 2 ]

## 4. Appropriately labels the data set with descriptive variable names. 
## Names of the columns that we kept 
## Observe the na.omit due to the fact that features does not contain the 
## activity and subject labels of the two merged files
featurenames <- na.omit(featdf[ columntokeep ,2 ])
namesofcolumns <- c(as.character(featurenames[[1]]), "activity", "subject")
names(df_sub) <- namesofcolumns

## 5.From the data set in step 4, creates a second, independent tidy data set 
## with the average of each variable for each activity and each subject.
grouped_df_sub <-group_by(df_sub, subject , activity)

final_df <- summarise_each(grouped_df_sub, funs(mean))

write.table(final_df, "tidy_dataset.txt", row.name=FALSE)



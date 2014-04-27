## Getting and Cleaning Data Course

To run this R script perform the following steps:

1. Unzip https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip
2. Place this script in the same directory as the datasets
3. In R-Studio load the script via

	source("run_analysis.r");

Run Everything:
main();

To run parts-

You should create one R script called run_analysis.R that does the following.
Merges the training and the test sets to create one data set.
ds = get.datasets();
  
ts = combine.testsets(ds);
  
#Extracts only the measurements on the mean and standard deviation for each measurement. 
ms = extract.measurements(ts);
  
#Uses descriptive activity names to name the activities in the data set
names(ms) <- ds$activity_labels
  
#Appropriately labels the data set with descriptive activity names. 
 
#Creates a second, independent tidy data set with the average of each variable for each activity and each subject.   
t = merge(ds$trainset$signals$bodyacc$X,ds$trainset$signals$bodyacc$Y)
write.table(t, file="tidy.txt")
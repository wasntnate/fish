###
#
# Run Analysis
#   Script to clean and conslidate accelormeter data
#     Original: http://archive.ics.uci.edu/ml/datasets/Human+Activity+Recognition+Using+Smartphones 
#     Assignment: https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip
#
# (c) Nate Bachmeier 2014
# http://wasntnate.com
#
###

library("data.table");

#
# Global variable to hold all the data
#   Access through get.datasets API
all_datasets<-NULL;

# 
# This private function will read all values in the supplied dataset
#   Access through get.datasets API
compile_dataset <- function(){
  
  #
  # This function will load a dataset as a data structure
  #   Note- The datasets provided with the assignment are called:
  #     test
  #     train
  read_dataset <- function(name){    
    
    #
    # Load the associated 'Inertial Signals'
    readsignals <- function()
    {
      #
      # Load a Vector3 of a given series name
      #   Note- The datasets provided with the assignment are called:
      #     body_acc
      #     body_gryo
      #     total_acc
      read_vect <- function(series){
        series_x <- read.table(paste(name,"\\Inertial Signals\\",series,"_x_",name,".txt",sep=""));
        series_y <- read.table(paste(name,"\\Inertial Signals\\",series,"_y_",name,".txt",sep=""));
        series_z <- read.table(paste(name,"\\Inertial Signals\\",series,"_z_",name,".txt",sep=""));
        
        #
        # Return the vector3
        return(list(
          series=series,
          X=series_x,
          Y=series_y,
          Z=series_z
        ));
      }
      
      #
      # Read each vector data set
      bodyacc <- read_vect("body_acc");
      bodygyro <- read_vect("body_gyro");
      totalacc <- read_vect("total_acc");
      
      #
      # Return back to the caller
      return(list(
        bodyacc=bodyacc,
        bodygyro=bodygyro,
        totalacc=totalacc));
    }
    
    #
    # Read information from this set
    subjects = read.table(paste(name,"\\subject_",name,".txt",sep=""));
    X <- read.table(paste(name,"\\X_",name,".txt",sep=""));
    Y <- read.table(paste(name,"\\X_",name,".txt",sep=""));    
    signals <- readsignals();
    
    #
    # Return the datastructure
    return(list(
      name=name,
      subjects=subjects,
      X=X,
      Y=Y,
      signals=signals));  
  }
  
  #
  # Finally... 
  #   Now request the entire folder open at once
  features = read.table("features.txt");
  activity_labels = read.table("activity_labels.txt");  
  testset = read_dataset("test");
  trainset = read_dataset("train");
  
  #
  # And return everything as a 'purdy' datastruct
  return(list(
    features=features,
    activity_labels=activity_labels,  
    testset=testset,
    trainset=trainset));
}

##############################################################
# get.datasets - 
#   Load all files into a graph with same layout as folders
##############################################################
get.datasets <- function(reload=FALSE){
  #
  # If first call, load from disk
  if(is.null(all_datasets) || reload == TRUE){
    all_datasets <- compile_dataset();
  }
  
  # 
  return(all_datasets);
}

##############################################################
# combine.testsets  -
#   Merge the train and test sets together
#
# Parameters:
#   Optional: ds a result from get.datasets()
##############################################################
combine.testsets <- function(ds=NULL){
  if(is.null(ds)){
    ds <- get.datasets();
  }
  
  #
  # Revision Note:
  #   Logic combines as (testset,trainset)
  #   If more properties added later, keep order consistent  
  
  #
  # Merge the Signals together
  combinesignals <- function(){
    
    #
    # Combine two Vector3 in a given series
    #   Ex: bodyacc
    combineseries <- function(series){
      return(list(
        series=series,
        X=c(ds$testset$signals[[series]]$X, ds$trainset$signals[[series]]$X),
        Y=c(ds$testset$signals[[series]]$Y, ds$trainset$signals[[series]]$Y),
        Z=c(ds$testset$signals[[series]]$Z, ds$trainset$signals[[series]]$Z)
      ));
    }  
    
    #
    # Return merged series
    return(list(
      bodyacc=combineseries("bodyacc"),
      bodygyro=combineseries("bodygyro"),
      totalacc=combineseries("totalacc")));            
  }
  
  #
  # Quash everything together, and return
  return(list(
    name="Combined Set",
    subjects=c(ds$testset$subjects, ds$trainset$subjects),
    X=c(ds$testset$X, ds$trainset$X),
    Y=c(ds$testset$Y, ds$trainset$Y),
    signals=combinesignals())); 
}

##############################################################
# extract.measurements  -
#   Merge the train and test sets together
#
# Parameters:
#   Optional: ts - the test set to use
##############################################################
extract.measurements <- function(ts=NULL){
  if(is.null(ts)){
    ts <- combine.testsets();
  }
  
  #
  # Function to handle one series of the data
  getseries <- function(series){
    
    signal <- ts$signals[[series]];
    
    vals <- list(
      xmean=lapply(signal$X,mean),
      ymean=lapply(signal$Y,mean),
      zmean=lapply(signal$Z,mean),
      
      xstd=lapply(signal$X,sd),
      ystd=lapply(signal$Y,sd),
      zstd=lapply(signal$Z,sd));
    
    return(vals);
  }
  
  #
  # Build a list that maps each dataset to the aggregated values
  return(list(
      bodyacc=getseries("bodyacc"),
      bodygyro=getseries("bodygyro"),
      totalacc=getseries("totalacc")
    ));  
}

#
# Main function
main <- function(){
  
  #You should create one R script called run_analysis.R that does the following.
  #Merges the training and the test sets to create one data set.
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
}
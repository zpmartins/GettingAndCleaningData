##
## From coursera assignment: Getting and Cleaning Data Course Project
##
## The purpose of this project is to demonstrate your ability to collect, work with, and clean a data set.
## The goal is to prepare tidy data that can be used for later analysis. You will be graded by your peers
## on a series of yes/no questions related to the project. You will be required to submit:
##   1) a tidy data set as described below,
##   2) a link to a Github repository with your script for performing the analysis, and
##   3) a code book that describes the variables, the data, and any transformations or work that you
##      performed to clean up the data called CodeBook.md. You should also include a README.md in the
##      repo with your scripts.
##      This repo explains how all of the scripts work and how they are connected.
##
## (...)
##
## You should create one R script called run_analysis.R that does the following.
## Merges the training and the test sets to create one data set.
## Extracts only the measurements on the mean and standard deviation for each measurement.
## Uses descriptive activity names to name the activities in the data set
## Appropriately labels the data set with descriptive variable names.
## From the data set in step 4, creates a second, independent tidy data set with the average
## of each variable for each activity and each subject.
##

## Package dependencies
library(plyr)

## Assigment Setup Data

# Generate date/ time timestamp
tstamp  <- format(Sys.time(), "%Y%m%d%H%M%S")

# Data file source
fileURL <-
  "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"

# Downloaded file name (with timestamp)
fName <- paste(tstamp,"_data.zip", sep = "")

# Data directory
dataPath <- "Data"

##
## Clean up function
##
cleanUp <- function(dataPath) {
  cat("Clear last execution data.\n")
  if (dir.exists(dataPath)) {
    if (unlink(dataPath, recursive = TRUE, force = TRUE) != 0) {
      warning("Unable to remove ",dataPath)
    }
  }
}

##
## Data downloading & extracting fuction
##
gatherData <- function(url, fName, dataPath) {
  # Download file
  cat("Downloading file",fileURL, "as", fName,"\n")
  if (download.file(fileURL, destfile = fName, method = "curl", quiet = TRUE) != 0) {
    stop("Unable to download file.")
  }
  
  # Create data dir
  if (!dir.exists(dataPath)) {
    dir.create(dataPath)
  } else {
    warning("Using existing data directory ", dataPath)
  }
  
  ## Unzip downloaded file
  cat("Unzipping file",fName, "into", dataPath,"\n")
  unzip(fName, exdir = dataPath)
  
}

##
## Data merging function
## Receives the data directory name, loads the datasets and returns a merged dataset with the content:
##   subject
##   activity
##   V1.. V561 features
##
mergeDatasets <- function(dataPath) {
  dsdir_test  <- "test"
  dsdir_train <- "train"
  
  #Local Function to assemble a dataset
  assembleDataset <- function(datasetName) {
    assembleFileName <- function(fName) {
      paste(fName, "_", datasetName, ".txt", sep = "")
    }
    
    # Store currect working directory
    cdir <- getwd()
    
    if (!dir.exists(datasetName)) {
      stop("Unable find directory ", datasetName)
    } else {
      setwd(datasetName)
    }
    
    cat("Loading dataset from", getwd(),"\n")
    
    s <- read.table(assembleFileName("subject"))
    y <- read.table(assembleFileName("y"))
    X <- read.table(assembleFileName("X"))
    
    dset <- merge(s, y, by = "row.names", sort = FALSE)
    dset$Row.names <- NULL
    colnames(dset) <- c("subject","activity")
    dset <- merge(dset, X, by = "row.names", sort = FALSE)
    dset$Row.names <- NULL
    
    # Restore working directory
    setwd(cdir)
    
    dset
  }
  
  # Store currect working directory
  cdir <- getwd()
  setwd(dataPath)
  
  dirDataset <- "UCI HAR Dataset"
  
  if (!dir.exists(dirDataset)) {
    stop("Unable find directory ", dirDataset)
  } else {
    setwd(dirDataset)
  }
  
  cat("Loading datasets from", getwd(),"\n")
  
  dstest  <- assembleDataset(dsdir_test)
  dstrain <- assembleDataset(dsdir_train)
  
  dset <- rbind(dstest,dstrain)
  
  # Restore working directory
  setwd(cdir)
  
  dset
}

##
## Mean and Standard Deviation function
## From the dataset with the format:
##   subject
##   activity
##   V1.. V561 features
## Extracts the Produces a Matrix with the Mean and Standard Deviation of the features.
##
extractMeanAndStandardDeviation <- function(dataset) {
  cat("Calculating Dataset Statistics\n")
  # Calc the Mean for the features
  dsMean  <- sapply(dataset,mean)
  
  # Calc the Standard Derivation  for the features
  dsStdev <- sapply(dataset,sd)
  
  # Bind the measures in a vector (transform it in a data frame)
  dsMeanAndStandardDeviation <- data.frame(rbind(dsMean, dsStdev))
  
  # Remove the subject and the activity as they are not features.
  dsMeanAndStandardDeviation$subject <- NULL
  dsMeanAndStandardDeviation$activity <- NULL
  
  dsMeanAndStandardDeviation
}

loadFeatureNames <- function(dataPath, dataset) {
  cdir <- getwd()
  setwd(dataPath)
  
  dirDataset <- "UCI HAR Dataset"
  
  if (!dir.exists(dirDataset)) {
    stop("Unable find directory ", dirDataset)
  } else {
    setwd(dirDataset)
  }
  
  cat("Loading features names from", getwd(),"\n")
  
  # Loading Features
  dsFeatures <- read.table("features.txt")
  
  # Creating Dataframe to hold subject and activity feature
  dsDum      <- data.frame(-2:-1,names(dataset)[1:2])
  
  # Making dsDum's names equal to dsFeatures's names
  names(dsDum) <- names(dsFeatures)
  
  # Merging both vectores
  dsFeatures <- rbind(dsDum,dsFeatures)

  # Setting dataset column's name
  colnames(dataset) <- dsFeatures$V2
  
  # Restore working directory
  setwd(cdir)

  dataset
}

######
## Assignment Function
##
## Function that executes all the steps specified by the assignment.
##
######
assignment <- function() {
  cleanUp(dataPath)
  
  cat("## Assignment Stage 1\n")
  gatherData(fileURL, fName, dataPath)
  dataset <- mergeDatasets(dataPath)
  
  cat("## Assignment Stage 2\n")
  dsStats <- extractMeanAndStandardDeviation(dataset)
  cat("Printing Stat Data")
  print(dsStats)
  
  cat("## Assignment Stage 3\n")
  dataset <- loadFeatureNames("Data", dataset)
  
  cat("## Assignment Stage 4\n")
  
  cat("## Assignment Stage 5\n")
  
  dataset
}
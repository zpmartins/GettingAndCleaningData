# GettingAndCleaningData
Getting and Cleaning Data's Coursera Assignment Readme File

The deliverables that comprehend this assignment are mainly the file run_analysis.R. Inside the specified file there are several functions that implement the specified behaviour (all the methods and functions are commented to ease its understanding).

* **Launching the script**
    * The script is configured for local execution. 
    * The files on the specified dataset should be downloaded and uncompressed in the local disk. 
    * The script should be copied for the directory **UCI HAR Dataset**. 
    * The following files / directories should exist as siblings to the *run_analysis.R* script:
        * activity_labels.txt (activity names)
        * features.txt (feature names)
        * test (test data directory)
        * train (train data directory)
     * Using R / RStudio, the current working directory should be set to the script's directory (with the command setwd(&lt;dirname&gt;))
     * The function **assignment** should be evaluated with the command **assignment()**
     
---
     
* **Script Interface**
   For the general user there are the following items to consider:

    * Flag **remote** (boolean, line 42)
        * If this flag is enabled (TRUE), the script will download the data from the website. Name the downloaded file with a timestamp stored in the variable *tstamp* and the *_data.zip* suffix, expand the downloaded file to the directory specified by the *dataPath* variable and analise the downloaded data. Otherwise,if the flag is disabled (FALSE), the script will run against the data that is stored in the script's directory.
    
    * Variable **outputFileName** (string, line 55)
        * This variable holds the name of the *new tidy dataset* that is created in the Step 5 of the assignment. Currently its value is a concatenation of the script execution timestamp (stored in the variable *tstamp*) and the "_summarised.txt" suffix.
    
  
    * Function **assignment** (function, line 290)
        * The function call in sequence the following functions, aiming to implement the steps specified in the assignment: 
          * cleanUp(dataPath)
            * Cleanup old execution data (if remote flag is enable).
          * gatherData(fileURL, fName, dataPath)
            * Downloads the data file from the specified URL (in the variable *fileURL*), and extract its content to the directory specified by the variable *dataPath*.
          * mergeDatasets(dataPath)
            * Merges the train and test datasets (*Step 1*).
          * extractMeanAndStandardDeviation(dataset)
            * Extracts the mean and the standard deviation for each measurement (*Step 2*).
          * loadFeatureNames(dataPath, dataset)
            * Loads the feature names from the file *features.txt* (*Step 4*)
          * loadActivityNames(dataPath, dataset)
            * Loads the feature names from the file *activity_labels.txt* (*Step 3*)
          * summarizeDataset(dataset, outputFileName)
            * Creates a new tidy dataset and stores it in the file specified by the *outputFileName* variable (*Step 5*)

---

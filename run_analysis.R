##########################################################################################################


## Course      : Getting and Cleaning Data
## University  : Jonhs Hopkins
## Due Date    : 2015-Feb-22
## Project     : Course Project
## Created  by : Hernando Grisales     Created Date: 2015-Feb-17
## Modified by : Hernando Grisales     Created Date: 2015-Feb-17

# Script summary:

# Transformations or work that you performed to clean up the data 

# Data Source: https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip 

# Transformations steps:

# 1. Merge the training and the test sets to create one data set.
# 2. Extract only the measurements on the mean and standard deviation for each measurement. 
# 3. Use descriptive activity names to name the activities in the data set
# 4. Appropriately label the data set with descriptive activity names. 
# 5. Creates a second, independent tidy data set with the average of each variable for each activity and each subject. 

##########################################################################################################

##  Prepare Execution Environment

        rm(list=ls()); # Clean up workspace
        library(plyr); # Load plyr package
        setwd('C:/Users/hgrisales/Documents/Coursera/Course_Project/UCI HAR Dataset/'); # set working directory where the UCI HAR Dataset is locates
        
        ##  Prepare Training Data
        
        #  Read Traing Data Files
        
        features                 <- read.table('./features.txt'           ,header=FALSE); 
        activityType             <- read.table('./activity_labels.txt'    ,header=FALSE); 
        subjectTrain             <- read.table('./train/subject_Train.txt',header=FALSE); 
        xTrain                   <- read.table('./train/x_train.txt'      ,header=FALSE); 
        yTrain                   <- read.table('./train/y_train.txt'      ,header=FALSE); 
        
        # Set columns names
        
        colnames(activityType)   <- c('activityID','activityType');
        colnames(subjectTrain)   <- "subjectID";
        colnames(xTrain)         <- features[,2]; 
        colnames(yTrain)         <- "activityID";
        
        # Combine Training Data in single dataframe
        trainingData              <- cbind(yTrain,subjectTrain,xTrain);
        
        
        ##  Prepare Test Data
        
        #  Read Test Data Files
        subjectTest              <-  read.table('./test/subject_test.txt',header=FALSE);
        xTest                    <-  read.table('./test/x_test.txt'      ,header=FALSE); 
        yTest                    <-  read.table('./test/y_test.txt'      ,header=FALSE);
        
        # Set columns names
        colnames(subjectTest)    <- "subjectID";
        colnames(xTest)          <- features[,2]; 
        colnames(yTest)          <- "activityID";
        
        
        # Combine Training Data in single dataframe
        testData                   <- cbind(yTest,subjectTest,xTest);
        
        
        
        # Merges the training and the test sets to create one data set
        finalData                 <- rbind(trainingData,testData);

# Step-002 
# Extracts only the measurements on the mean and standard deviation for each measurement.
############################################################################################

        # Create Vector with the Columns Name in the final data set
        col_names                 <- colnames(finalData); 
        
        # Calculate logical vector with columns with (mean, std, subject, activity) in their names 
        valid_columns              <- (grepl("activity..",col_names) | grepl("subject..",col_names) | grepl("-mean..",col_names) & !grepl("-meanFreq..",col_names) & !grepl("mean..-",col_names) | grepl("-std..",col_names) & !grepl("-std()..-",col_names));
        
        # Extract only valid measurements
        
        finalData                  <- finalData[valid_columns==TRUE];


# Step-003
# Uses descriptive activity names to name the activities in the data set
############################################################################################

        # Merge the final_DT set with the activityType table to include descriptive activity names
        finalData                 <- merge(finalData ,activityType,by='activityID',all.x=TRUE);
        
        # Updating the colNames vector to include the new column names after merge
        col_names                  <- colnames(finalData); 


# Step-003
# Appropriately label the data set with descriptive activity names. 
############################################################################################

        # Cleaning up the variable names
        
        for (i in 1:length(col_names)) 
        {
                col_names[i] <- gsub("\\()",""                                   ,col_names[i])
                col_names[i] <- gsub("-std$","StdDev"                            ,col_names[i])
                col_names[i] <- gsub("-mean","Mean"                              ,col_names[i])
                col_names[i] <- gsub("^(t)","time"                               ,col_names[i])
                col_names[i] <- gsub("^(f)","freq"                               ,col_names[i])
                col_names[i] <- gsub("([Gg]ravity)","Gravity"                    ,col_names[i])
                col_names[i] <- gsub("([Bb]ody[Bb]ody|[Bb]ody)","Body"           ,col_names[i])
                col_names[i] <- gsub("[Gg]yro","Gyro"                            ,col_names[i])
                col_names[i] <- gsub("AccMag","AccMagnitude"                     ,col_names[i])
                col_names[i] <- gsub("([Bb]odyaccjerkmag)","BodyAccJerkMagnitude",col_names[i])
                col_names[i] <- gsub("JerkMag","JerkMagnitude"                   ,col_names[i])
                col_names[i] <- gsub("GyroMag","GyroMagnitude"                   ,col_names[i])
        };
        
        # Update the new descriptive column names to the finalData set
        colnames(finalData)     <- col_names;

# Step-004
# creates a second, independent tidy data set with the average of 
# each variable for each activity and each subject.
############################################################################################

        independent_DT           <- finalData[,names(finalData) != 'activityID'];
        
        # Summarizing the finalDataNoActivityType table to include just the mean of each variable for each activity and each subject
        tidy_DT    = aggregate(independent_DT[,names(independent_DT) != c('activityID','subjectID')],by=list(activityID=independent_DT$activityID,subjectID = independent_DT$subjectID),mean);
        
        # Merging the tidyData with activityType
        tidy_DT    = merge(tidy_DT,activityType,by='activityID',all.x=TRUE);
        
        # Export the tidy_DT set 
        write.table(tidy_DT, './tidyData.txt',row.names=TRUE,sep='\t');

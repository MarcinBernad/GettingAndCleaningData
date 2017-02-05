if (!getwd() == "./data") {
        dir.create("./data")
}


rm(list = ls(all = TRUE))
library(plyr) # load plyr first, then dplyr 
library(data.table) # a prockage that handles dataframe better
library(dplyr) # for fancy data table manipulations and organization

temp <- tempfile()
download.file("http://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip",temp)
unzip(temp, list = TRUE) #This provides the list of variables and I choose the ones that are applicable for this data set
y.Test <- read.table(unzip(temp, "UCI HAR Dataset/test/y_test.txt"))
x.Test <- read.table(unzip(temp, "UCI HAR Dataset/test/X_test.txt"))
Subject.Test <- read.table(unzip(temp, "UCI HAR Dataset/test/subject_test.txt"))
y.Train <- read.table(unzip(temp, "UCI HAR Dataset/train/y_train.txt"))
x.Train <- read.table(unzip(temp, "UCI HAR Dataset/train/X_train.txt"))
Subject.Train <- read.table(unzip(temp, "UCI HAR Dataset/train/subject_train.txt"))
Features <- read.table(unzip(temp, "UCI HAR Dataset/features.txt"))
unlink(temp) # very important to remove this

colnames(x.Train) <- t(Features[2])
colnames(x.Test) <- t(Features[2])

x.Train$activities <- y.Train[, 1]
x.Train$participants <- Subject.Train[, 1]
x.Test$activities <- y.Test[, 1]
x.Test$participants <- Subject.Test[, 1]

mergesData <- rbind(x.Train, x.Test)
duplicated(colnames(mergesData))
mergesData <- mergesData[, !duplicated(colnames(mergesData))]

Mean <- grep("mean()", names(mergesData), value = FALSE, fixed = TRUE)
#In addition, we need to include 555:559 as they have means and are associated with the gravity terms
Mean <- append(Mean, 471:477)
InstrumentMeanMatrix <- mergesData[Mean]
# For STD
STD <- grep("std()", names(mergesData), value = FALSE)
InstrumentSTDMatrix <- mergesData[STD]

mergesData$activities <- as.character(mergesData$activities)
mergesData$activities[mergesData$activities == 1] <- "Walking"
mergesData$activities[mergesData$activities == 2] <- "Walking Upstairs"
mergesData$activities[mergesData$activities == 3] <- "Walking Downstairs"
mergesData$activities[mergesData$activities == 4] <- "Sitting"
mergesData$activities[mergesData$activities == 5] <- "Standing"
mergesData$activities[mergesData$activities == 6] <- "Laying"
mergesData$activities <- as.factor(mergesData$activities)


names(mergesData) 
names(mergesData) <- gsub("Acc", "Accelerator", names(mergesData))
names(mergesData) <- gsub("Mag", "Magnitude", names(mergesData))
names(mergesData) <- gsub("Gyro", "Gyroscope", names(mergesData))
names(mergesData) <- gsub("^t", "time", names(mergesData))
names(mergesData) <- gsub("^f", "frequency", names(mergesData))

mergesData$participants <- as.character(mergesData$participants)
mergesData$participants[mergesData$participants == 1] <- "Participant 1"
mergesData$participants[mergesData$participants == 2] <- "Participant 2"
mergesData$participants[mergesData$participants == 3] <- "Participant 3"
mergesData$participants[mergesData$participants == 4] <- "Participant 4"
mergesData$participants[mergesData$participants == 5] <- "Participant 5"
mergesData$participants[mergesData$participants == 6] <- "Participant 6"
mergesData$participants[mergesData$participants == 7] <- "Participant 7"
mergesData$participants[mergesData$participants == 8] <- "Participant 8"
mergesData$participants[mergesData$participants == 9] <- "Participant 9"
mergesData$participants[mergesData$participants == 10] <- "Participant 10"
mergesData$participants[mergesData$participants == 11] <- "Participant 11"
mergesData$participants[mergesData$participants == 12] <- "Participant 12"
mergesData$participants[mergesData$participants == 13] <- "Participant 13"
mergesData$participants[mergesData$participants == 14] <- "Participant 14"
mergesData$participants[mergesData$participants == 15] <- "Participant 15"
mergesData$participants[mergesData$participants == 16] <- "Participant 16"
mergesData$participants[mergesData$participants == 17] <- "Participant 17"
mergesData$participants[mergesData$participants == 18] <- "Participant 18"
mergesData$participants[mergesData$participants == 19] <- "Participant 19"
mergesData$participants[mergesData$participants == 20] <- "Participant 20"
mergesData$participants[mergesData$participants == 21] <- "Participant 21"
mergesData$participants[mergesData$participants == 22] <- "Participant 22"
mergesData$participants[mergesData$participants == 23] <- "Participant 23"
mergesData$participants[mergesData$participants == 24] <- "Participant 24"
mergesData$participants[mergesData$participants == 25] <- "Participant 25"
mergesData$participants[mergesData$participants == 26] <- "Participant 26"
mergesData$participants[mergesData$participants == 27] <- "Participant 27"
mergesData$participants[mergesData$participants == 28] <- "Participant 28"
mergesData$participants[mergesData$participants == 29] <- "Participant 29"
mergesData$participants[mergesData$participants == 30] <- "Participant 30"
mergesData$participants <- as.factor(mergesData$participants)


mergesData.dt <- data.table(mergesData)
#This takes the mean of every column broken down by participants and activities
TidyData <- mergesData.dt[, lapply(.SD, mean), by = 'participants,activities']
write.table(TidyData, file = "Tidy.txt", row.names = FALSE)
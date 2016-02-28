setwd("~/Desktop/Data Science/Getting and cleaning Data/Project")

#This script is will:
# 1. Merge the training and the test sets to create one data set.
# 2. Extract only the measurements on the mean and standard deviation for each measurement. 
# 3. Use descriptive activity names to name the activities in the data set
# 4. Appropriately label the data set with descriptive activity names. 
# 5. Creates a second, independent tidy data set with the average of each variable for each activity and each subject

library(data.table)

### 1. Merge the training and the test sets to create one data set:

# Load the training data from the UCI HAR Dataset into the global enviroment
features<-read.table("./UCI HAR Dataset/features.txt",header=F)
labels<-read.table("./UCI HAR Dataset/activity_labels.txt",header=F)
subjecttrain<-read.table("./UCI HAR Dataset/train/subject_train.txt",header=F)
xtrain<-read.table("./UCI HAR Dataset/train/x_train.txt",header=F)
ytrain<-read.table("./UCI HAR Dataset/train/y_train.txt",header=F)

# Assigning column names to the imported data
colnames(labels)<-c("excersiceid","excersisetype")
colnames(subjecttrain)<-"subjectid"
colnames(xtrain)<-features[,2]
colnames(ytrain)<-"excersiceid"

# Merging traning data by cbind
traningdata<-cbind(ytrain,subjecttrain,xtrain)

# Load the test data from the UCI HAR Dataset into the global enviroment
subjecttest<-read.table("./UCI HAR Dataset/test/subject_test.txt",header=F)
xtest<-read.table("./UCI HAR Dataset/test/x_test.txt",header=F)
ytest<-read.table("./UCI HAR Dataset/test/y_test.txt",header=F)


# Assigning column names to the imported data
colnames(subjecttest)<-"subjectid"
colnames(xtest)<-features[,2]
colnames(ytest)<-"excersiceid"

#creating a joint test data frame using cbind
testdata<-cbind(ytest,subjecttest,xtest)

#Combining the two datasets into one
datacombined<-rbind(traningdata , testdata)

# Create a vector of coloumn names
coloumnnames <- colnames(datacombined)

### 2. Extract only the measurements on the mean and standard deviation for each measurement. 

#Create a logical vector that contains the TRUE value for the mesurements
logicalvector <- (grepl("excersice",coloumnnames) | grepl("subject..",coloumnnames) | grepl("-mean..",coloumnnames) & !grepl("-meanFreq..",coloumnnames) & !grepl("mean..-",coloumnnames) | grepl("-std..",coloumnnames) & !grepl("-std()..-",coloumnnames))

#Subset the combined data so that it only keeps desired columns
finaldata <- datacombined[logicalvector==TRUE]

### 3. Use descriptive activity names to name the activities in the data set

# Merge the finaldata set with the excersisetype table to include descriptive activity names
finaldata <- merge(finaldata,labels,by="excersiceid",all.x=TRUE)

#updating the coloumnnames vector
columnnames <- colnames(finaldata)

### 4. Appropriately label the data set with descriptive activity names.
# Cleaning up the variable names

for (i in 1:length(columnnames)) 
{
        columnnames[i] = gsub("\\()","",columnnames[i])
        columnnames[i] = gsub("-std$","StdDev",columnnames[i])
        columnnames[i] = gsub("-mean","mean",columnnames[i])
        columnnames[i] = gsub("^(t)","time",columnnames[i])
        columnnames[i] = gsub("^(f)","freq",columnnames[i])
        columnnames[i] = gsub("([Gg]ravity)","gravity",columnnames[i])
        columnnames[i] = gsub("([Bb]ody[Bb]ody|[Bb]ody)","body",columnnames[i])
        columnnames[i] = gsub("[Gg]yro","gyro",columnnames[i])
        columnnames[i] = gsub("AccMag","accmagnitude",columnnames[i])
        columnnames[i] = gsub("([Bb]odyaccjerkmag)","bodyaccjerkmagnitude",columnnames[i])
        columnnames[i] = gsub("JerkMag","jerkmagnitude",columnnames[i])
        columnnames[i] = gsub("GyroMag","gyromagnitude",columnnames[i])
}

# updating the coloumnnames vector
colnames(finaldata)<-columnnames

###5. Create a second, independent tidy data set with the average of each variable for each activity and each subject.

# Create a new table, finaldata2 without the excersisetype column
finaldata2  <- finaldata[,names(finaldata) != "excersisetype"]

# Summarizing the finaldata2 table to include just the mean of each variable for each activity and each subject
tidydata    <- aggregate(finaldata2[,names(finaldata2) != c("excersiceid","subjectid")],by=list(excersiceid=finaldata2$excersiceid,subjectid = finaldata2$subjectid),mean)

# Merging the tidydata with excersisetype to include descriptive acitvity names
tidydata    <- merge(tidydata,labels,by='excersiceid',all.x=TRUE);

# Export the tidydata set 
write.table(tidydata, './tidydata.txt',row.names=TRUE,sep='\t');

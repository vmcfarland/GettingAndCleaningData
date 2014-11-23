# run_analysis.R should do the following:
#   
#   1)  Merges the training and the test sets to create one data set.
# 
#   2)  Extracts only the measurements on the mean and standard deviation for each measurement. 
# 
#   3)  Uses descriptive activity names to name the activities in the data set
# 
#   4)  Appropriately labels the data set with descriptive variable names. 
#   
#   5)  From the data set in step 4, creates a second, independent tidy data set with the average
#       of each variable for each activity and each subject.

#     Please upload the tidy data set created in step 5 of the instructions. 

#     Please upload your data set as a txt file created with write.table() using row.name=FALSE 
#     (do not cut and paste a dataset directly into the text box, as this may cause errors 
#     saving your submission).


library(reshape2)
setwd("./UCI HAR Dataset")
features <- read.table("features.txt",header=F)
xfeatures <- grepl("[Mm]ean|[Ss]td", features[,2])

activity_labels <- read.table("./activity_labels.txt", header=F, col.names = c("activityId","activityType"))

subject_train <- read.table("./train/subject_train.txt", header=F, col.names = "subjectId")
x_train <- read.table("./train/x_train.txt",header=F,col.names=features[,2])
y_train <- read.table("./train/y_train.txt",header=F,col.names="activityId")

train <- cbind(y_train,subject_train,x_train[,xfeatures])

subject_test <- read.table("./test/subject_test.txt",header=F,col.names="subjectId")
x_test <- read.table("./test/x_test.txt",header=F,col.names=features[,2])
y_test <- read.table("./test/y_test.txt",header=F,col.names="activityId")

test <- cbind(y_test,subject_test,x_test[,xfeatures])

data <- rbind(train,test)

fdata <- merge(activity_labels,data,by="activityId",all.x=T)
fdata <- fdata[,names(fdata) != "activityId"]

# need to clean up names here
dnames<-names(fdata)
dnames<-gsub("^t", "time",dnames)
dnames <-gsub("^f", "freq",dnames)
dnames <-gsub("([Gg]ravity)","Gravity",dnames)
dnames <-gsub("([Bb]ody[Bb]ody|[Bb]ody)","Body",dnames)
dnames <-gsub("[Gg]yro","Gyro",dnames)
dnames <-gsub("AccMag","AccMagnitude",dnames)
dnames <-gsub("([Bb]odyaccjerkmag)","BodyAccJerkMagnitude",dnames)
dnames <-gsub("JerkMag","JerkMagnitude",dnames)
dnames <-gsub("GyroMag","GyroMagnitude",dnames)
dnames <-gsub("\\()","",dnames)
dnames <-gsub("std","StdDev",dnames)
dnames <-gsub("mean","Mean",dnames)


mdata <- melt(fdata, id=c("subjectId", "activityType"))
tidyMeans<-dcast(mdata, subjectId + activityType~variable, mean) 
write.table(tidyMeans, "../tidyMeans.txt", row.names = F, sep="\t")

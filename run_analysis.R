#1.Merges the training and the test sets to create one data set.
  
  #Identifying train-data
  subject_train<-read.table(file="./train/subject_train.txt")
  data_train<-read.table(file="./train/X_train.txt")
  act_label_train<-read.table(file="./train/y_train.txt")
  
  #Combine the three into one data frame
  train<- data.frame(data_train, act_label_train, subject_train)
  
  #Identifying test-data
  subject_test<-read.table(file="./test/subject_test.txt")
  data_test<-read.table(file="./test/X_test.txt")
  act_label_test<-read.table(file="./test/y_test.txt")
  
  #Adding 2 more variables: activity and subject
  test<- data.frame(data_test, act_label_test, subject_test)
 
  #Building the whole dataset
  #alldata<-merge(train, test, all=TRUE)
  alldata<-rbind(train, test)

  
#2.Extracts only the measurements on the mean and standard deviation for each measurement.
  
  #Identifying features-data
  features<-read.table(file="./features.txt")
  
  #choosing mean(), std() features
  toMatch <- c(".*mean\\(\\).*-X$", ".*std\\(\\).*-X$",
               ".*mean\\(\\).*-Y$", ".*std\\(\\).*-Y$",
               ".*mean\\(\\).*-Z$", ".*std\\(\\).*-Z$")#Thanks to Dung Minh Tran
  matches <- unique (grep(paste(toMatch,collapse="|"),features$V2, value=FALSE))
  
  #extracting mean() and std() columns, and furthermore activities and subject columns
  stdmeandata<-alldata[,c(matches,562,563)]

  
#3.Uses descriptive activity names to name the activities in the data set
  
  #Identifying activity labels
  activity<-read.table(file="./activity_labels.txt")
  
  #Data with the activity descriptions
  stdmeandatact<-merge(stdmeandata,activity,by.x="V1.2",by.y="V1")[-1]

  
#4.Appropriately labels the data set with descriptive activity names.
  matchesnames <- as.vector(features[matches,2])
  colnames(stdmeandatact)<-c(matchesnames, "subject","activity")

  
#5.Creates a second, independent tidy data set with the average of each variable 
#for each activity and each subject.
  
  library(reshape2)
  molten = melt(stdmeandatact, id.vars=c("activity","subject"))
  newdata <- dcast(molten,formula= activity + subject ~ variable,mean)
  
  #Finally, let's generate the txt file with the data
  write.table(newdata, "./finaldataset.txt", sep="\t")

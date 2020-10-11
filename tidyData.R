tidyData<-function(){
  
  dataFeatureTrain<-read.table(file.path("UCI HAR Dataset", "train",  "X_train.txt") )
  dataFeatureTest<-read.table(file.path("UCI HAR Dataset", "test",  "X_test.txt") )
  dataActivityTrain<-read.table(file.path("UCI HAR Dataset", "train",  "Y_train.txt") )
  dataActivityTest<-read.table(file.path("UCI HAR Dataset", "test",  "Y_test.txt") )
  dataSubjectTrain<-read.table(file.path("UCI HAR Dataset", "train",  "subject_train.txt") )
  dataSubjectTest<-read.table(file.path("UCI HAR Dataset", "test",  "subject_test.txt") )
  
  dataSubject<-rbind(dataSubjectTrain,dataSubjectTest)
  dataActivity<-rbind(dataActivityTrain,dataActivityTest)
  dataFeature<-rbind(dataFeatureTrain,dataFeatureTest)
  names(dataSubject)<-c("subject")
  names(dataActivity)<-c("Activity")
  
  varnames<-read.table(file.path("UCI HAR Dataset","features.txt"))
  names(dataFeature)<-varnames[,2]
  data<-cbind(dataActivity,dataSubject)
  data<-cbind(data,dataFeature)
  
  selectedNames<-select(data,grep("mean\\(\\)|std\\(\\)",colnames(data)))
  selectName<-names(selectedNames)
  selectname1<-append(selectName, c("subject","Activity"))
  dataMeanStd<-select(data, selectname1)
  
  names(dataMeanStd)<-gsub("^t", "time", names(dataMeanStd))
  names(dataMeanStd)<-gsub("^f", "frequency", names(dataMeanStd))
  names(dataMeanStd)<-gsub("Acc", "Accelemetor", names(dataMeanStd))
  names(dataMeanStd)<-gsub("Gyro", "Gyroscope", names(dataMeanStd))
  names(dataMeanStd)<-gsub("Mag", "Magnitude", names(dataMeanStd))
  names(dataMeanStd)<-gsub("BodyBody", "Body", names(dataMeanStd))
  
  tidydata<-aggregate(. ~subject + Activity, dataMeanStd, mean)
  write.table(tidydata, file = "tidydata.txt",row.name=FALSE)
  
}
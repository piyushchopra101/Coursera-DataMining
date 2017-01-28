# ------------------------------------------------------------------------------ 
# Process the "Human Activity Recognition Using Smartphones" dataset 
# 
# Did as part of coursera project work 
# Author: Piyush Chopra
# ------------------------------------------------------------------------------



## load the data into R

X_train <- read.table("T:/class docs/Data Mining/data mining/New Folder/UCI HAR Dataset/train/X_train.txt", quote="\"", comment.char="")
y_train<-read.table("T:/class docs/Data Mining/data mining/New Folder/UCI HAR Dataset/train/y_train.txt", quote="\"", comment.char="")
subject_train<-read.table("T:/class docs/Data Mining/data mining/New Folder/UCI HAR Dataset/train/subject_train.txt", quote="\"", comment.char="")

features<-read.table("T:/class docs/Data Mining/data mining/New Folder/UCI HAR Dataset/features.txt", quote="\"", comment.char="")

X_test <- read.table("T:/class docs/Data Mining/data mining/New Folder/UCI HAR Dataset/test/X_test.txt", quote="\"", comment.char="")
y_test<-read.table("T:/class docs/Data Mining/data mining/New Folder/UCI HAR Dataset/train/y_test.txt", quote="\"", comment.char="")
subject_test<-read.table("T:/class docs/Data Mining/data mining/New Folder/UCI HAR Dataset/test/subject_test.txt", quote="\"", comment.char="")


mytrain<-X_train
myclasslabels<-y_train

library(plyr) 
#change values to class labels 
myclasslabels<-mapvalues(myclasslabels[,1], c("1", "2","3","4","5","6"), c( "WALKING","WALKING_UPSTAIRS","WALKING_DOWNSTAIRS","SITTING","STANDING","LAYING"))

## Assigning colnames to mytraining dataset.
name<-features$V2
colnames(mytrain)<-name


## insert column pertaining to the class label in mytrain dataset
mytrain<-cbind(Activity=myclasslabels,mytrain)
mytrain<-cbind(Volunteer_Number=subject_train[,1],mytrain)

##performing the same operations on the test dataset
mytest<-X_test
testclasslabels<-y_test
testclasslabels<-mapvalues(testclasslabels[,1], c("1", "2","3","4","5","6"), c( "WALKING","WALKING_UPSTAIRS","WALKING_DOWNSTAIRS","SITTING","STANDING","LAYING"))
colnames(mytest)<-name
mytest<-cbind(Activity=testclasslabels,mytest)
mytest<-cbind(Volunteer_Number=subject_test[,1],mytest)


## Merging test and training datasets
total_data<-rbind(mytrain,mytest)

#subset only those columns which have mean Observations
#subset only those columns which have standard deviations observations
mean_var<-total_data[,grep(".mean",names(total_data),value = TRUE)]
std_var<-total_data[,grep(".std",names(total_data),value = TRUE)]
total_meanvalues<- cbind(total_data[,1:2],mean_var)
total_std<- cbind(total_data[,1:2],std_var)

#### Dataset with only the mean values of each Measurement
total_meanvalues
### Dataset with only the Standard Deviation of each Measuremnt
total_std
### Dataset with both mean and Standard Deviation Values
total_Mean_Std<-cbind(total_data[,1:2],mean_var,std_var)

              
  
## Creating an independant tidy data set containing only the values of average 
## of of each variable for each activity and each subject.
t<-NULL
d<-NULL
rq<-NULL
x<-NULL
c<-NULL
r<-NULL
q<-NULL
we<-NULL
for(e in c(1:30)){
  re<-subset(total_data,total_data$Volunteer_Number==e)
  print(e)
  for (j in c( "WALKING","WALKING_UPSTAIRS","WALKING_DOWNSTAIRS","SITTING","STANDING","LAYING")){
    #print(j)
    #create a subset of the dataset
    x<-subset(re,re$Activity==j  )
    #print(x[1:3,2:3])
    #calculate the colmeans for all columns
    c<-colMeans(x[3:563],na.rm = TRUE)
    #print(c[1:2])
    #bind all the column means together for each activity
    d<-rbind(d,t(c))
    # bind all the initializations of j i.e label of the activity
    t<-rbind(t,print(j))
    #print(t)
    #bind the labels column with the col.means for that label
    q<-cbind(t,d)
    volunteer=e
    we<-rbind(we,as.numeric(volunteer))
    #r<-cbind(volunteer=e,q)
    #print(q[,1:3])
    #print(r[,1:4])
  }
  r<-cbind(we,q)
  #rq<-rbind(rq,r)
} 
dim(r)
try2<-r
try3<-transform(try2,as.numeric(try2[,3:563]))
colnames(try3)[1] <- "Volunteer_No"
colnames(try3)[2] <- "Activity"



## Independant tidy dataset with the average of each variable for each activity and each subject.
tidyset<-try3
write.csv(tidyset, "tidyset.csv", row.names=FALSE)
write.table(tidyset, "tidyset.txt", row.names=FALSE,sep = "\t ")


                 

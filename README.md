###### This file contains the description of the code and the working of the script.

## Logic Walkthrough:
---

Initial operations are performed on the mytrain dataset and once succesful the same set of operations are perfomred on the test data set.

### This code gets the individual datasets into new objects called mytrain and            myclasslabels
---

    mytrain<-X_train
    myclasslabels<-y_train

### Call library plyr to perform operations like mapvalues and grep
---    
    library(plyr) 

### Change values to class labels 
---
   
    myclasslabels<-mapvalues(myclasslabels[,1], c("1", "2","3","4","5","6"), c( "WALKING","WALKING_UPSTAIRS","WALKING_DOWNSTAIRS","SITTING","STANDING","LAYING"))

After loading the features file from training dataset folder the following lines are executed

### Assigning colnames to mytraining dataset.
---
    name<-features$V2
    colnames(mytrain)<-name

### Insert column pertaining to the class label in mytrain dataset
---

    mytrain<-cbind(Activity=myclasslabels,mytrain)
    mytrain<-cbind(Volunteer_Number=subject_train[,1],mytrain)

The code on training set is performed and the training dataset is ready


### Performing the same operations on the test dataset
---
    mytest<-X_test
    testclasslabels<-y_test
    testclasslabels<-mapvalues(testclasslabels[,1], c("1", "2","3","4","5","6"), c( "WALKING","WALKING_UPSTAIRS","WALKING_DOWNSTAIRS","SITTING","STANDING","LAYING"))
    colnames(mytest)<-name
    mytest<-cbind(Activity=testclasslabels,mytest)
    mytest<-cbind(Volunteer_Number=subject_test[,1],mytest)


### Merging test and training datasets
---

    total_data<-rbind(mytrain,mytest)

The secondary dataset after merging Test and Training datasets are kept in the object total_data. 
Further analysis are perfomred on the total_data as base dataset

## The columns relating only to the mean and std deviation are selected by the following lines

### Subset only those columns which have mean Observations
---
    mean_var<-total_data[,grep(".mean",names(total_data),value = TRUE)]
- ###### grep function is used to grab all those colums which have the word "mean " in them

### Subset only those columns which have standard deviations observations
---
    std_var<-total_data[,grep(".std",names(total_data),value = TRUE)]
- Grep function is used to grab all those colums which have the word "std " in them

### The following two lines are used to merge first two columns of total_data with the above obtained datasets.
---
    total_meanvalues<- cbind(total_data[,1:2],mean_var)
    total_std<- cbind(total_data[,1:2],std_var)

#### Dataset with only the mean values of each Measurement
---

    total_meanvalues
### Dataset with only the Standard Deviation of each Measurment
---
    total_std
### Dataset with both mean and Standard Deviation Values
---
    total_Mean_Std<-cbind(total_data[,1:2],mean_var,std_var)


The following code is used to give the independant tidy dataset
### Creating an independant tidy data set containing only the values of average of each variable for each activity and each subject.
---

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
        
        #create a subset of the dataset
        x<-subset(re,re$Activity==j  )
        
        #calculate the colmeans for all columns
        c<-colMeans(x[3:563],na.rm = TRUE)
        
        #bind all the column means together for each activity
        d<-rbind(d,t(c))
        # bind all the initializations of j i.e label of the activity
        t<-rbind(t,print(j))
        
        #bind the labels column with the col.means for that label
        q<-cbind(t,d)
        volunteer=e
        we<-rbind(we,as.numeric(volunteer))
       
      }
      r<-cbind(we,q)
      #rq<-rbind(rq,r)
    } 

    try3<-transform(r,as.numeric(r[,3:563]))
    colnames(try3)[1] <- "Volunteer_No"
    colnames(try3)[2] <- "Activity"



### Independant tidy dataset with the average of each variable for each activity and each subject.
---
    tidyset<-try3

### This would return the file in the text delimited format or csv format
---
    write.csv(tidyset, "tidyset.csv", row.names=FALSE)
    write.table(tidyset, "tidyset1.txt", row.names=FALSE,sep = "\t ")





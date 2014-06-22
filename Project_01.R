#Proyecto Getting and cleaning data

### PARTE 1. Merges the training and the test sets to create one data set
# Merging Test data 
X_test <- read.table("./Dataset/test/X_test.txt")       #Data
y_test <-read.table("./Dataset/test/y_test.txt")        #Activity
s_test <-read.table("./Dataset/test/subject_test.txt")  #Subject
Test_data <- cbind(y_test,s_test,X_test)

# Merging Train data
X_train <- read.table("./Dataset/train/X_train.txt")      #Data
y_train <-read.table("./Dataset/train/y_train.txt")       #Activity
s_train <-read.table("./Dataset/train/subject_train.txt") #Subject
Train_data <- cbind(y_train,s_train,X_train)

#Mergin Test and Training data
Data_set <- rbind(Test_data,Train_data)

### PARTE 2. Extracts only the measurements on the mean and standard deviation for each measurement
features <- read.table("./Dataset/features.txt")
FT <-as.character(features[,2])
ind_mean <- grep("mean()", FT)+2   #Index of the measurements with mean values
ind_std  <- grep("std()",FT)+2     #Index of the measurements with standard values
ind_val <- sort(c(ind_mean,ind_std))   # Join mean and standard values
ind_def <- c(1,2,ind_val)    # Complete index with 1st and 2nd dedicate "Activity" and "Subject" variables 

# Tidy data set that contain only the extracted "mean" and "standard deviation" variables.
Data_set2 <- Data_set[,ind_def]  
#Ordering Data_set2 respect "Activity" and "Subject" variable
library(plyr)
Data_set3 <- arrange(Data_set2,Data_set2$V1,Data_set2$V1.1) 

### PARTE 3 Uses descriptive activity names to name the activities in the data set

Name_act <- read.table("./Dataset/activity_labels.txt") #Read activity labels
NameR <- as.character(Name_act$V2)
#Changing 1,2,3... by activity names WALKING, Walking UPstairs,...
Data_set3$V1[Data_set3$V1 %in% "1"] <- NameR[1]
Data_set3$V1[Data_set3$V1 %in% "2"] <- NameR[2]
Data_set3$V1[Data_set3$V1 %in% "3"] <- NameR[3]
Data_set3$V1[Data_set3$V1 %in% "4"] <- NameR[4]
Data_set3$V1[Data_set3$V1 %in% "5"] <- NameR[5]
Data_set3$V1[Data_set3$V1 %in% "6"] <- NameR[6]


### Part. 4. Appropriately labels the data set with descriptive variable names
FT_val <- FT[ind_val-2]
#FT_names <- character(length(FT_val))
for (i in seq_along(FT_val)) {
    FT_val[i] <- gsub("tBodyAcc","TimeBodyAcc",FT_val[[i]])
    FT_val[i] <- gsub("tGravityAcc","TimeGravityAcc",FT_val[[i]])
    FT_val[i] <- gsub("tBodyGyro","TimeBodyGyr",FT_val[[i]])
    FT_val[i] <- gsub("fBodyAcc","FreqBodyAcc",FT_val[[i]])
    FT_val[i] <- gsub("fGravityAcc","FreqGravityAcc",FT_val[[i]])
    FT_val[i] <- gsub("fBodyGyro","FreqBodyGyr",FT_val[[i]])
  
}

for (i in seq_along(FT_val)) {
  FT_val[i] <- gsub("mean\\(\\)","Mean",FT_val[[i]])
  FT_val[i] <- gsub("std\\(\\)","Std",FT_val[[i]])
  FT_val[i] <- gsub("Freq\\(\\)","Freq",FT_val[[i]])
  FT_val[i] <- gsub("meanFreq","MeanFreq",FT_val[[i]])
}
colnames(Data_set3)<-c("Activity","Subject",FT_val)

write.table(Data_set3, file="Tidy_DataS_1.txt")


### Part 5. Creates a second, independent tidy data set with the 
#   average of each variable for each activity and each subject. 


#Create a sequence number of  the index number that represent same values
sq1 <- seq(3,31,3);sq1b <- sq1+2
sq2 <- seq(33,42,1)
sq3 <- seq(43,67,3);sq3b <- sq3+2
sq4 <- seq(70,81)

sq <- c(1,2,sq1,sq2,sq3,sq4)
nombres <- c("Activity","Subject",FT_val)

nombres_n <- nombres[sq]

for (i in seq_along(nombres_n)) {
  nombres_n[i] <- gsub("X","XYZ",nombres_n[[i]])
}

A <- matrix(0, nrow=10299, ncol=length(sq))
#A[,1] <- Data_set3[,1]
#A[,2] <- Data_set3[,2]

nc <-2
for (i in seq_along(sq1)) {
   A[,nc+i]<- rowMeans(Data_set3[,sq1[i]:sq1b[i]])
}

nc<-nc+length(sq1)
for (i in seq_along(sq2)) {
  A[,nc+i]<- Data_set3[,sq2[i]]
}

nc <- nc+length(sq2)
for (i in seq_along(sq3)) {
  A[,10+2+10+i]<- rowMeans(Data_set3[,sq3[i]:sq3b[i]])
}

nc <- nc+length(sq3)
for (i in seq_along(sq4)) {
  A[,10+2+10+9+i]<- Data_set3[,sq4[i]]
}

data_set4 <- as.data.frame(A)
colnames(data_set4) <- nombres_n
data_set4$Activity <- Data_set3[,1] 
data_set4$Subject <- Data_set3[,2] 


data_set5 <- aggregate(. ~ Subject+Activity,data = data_set4,FUN=mean  )
write.table(data_set5, file="Tidy_DataS_2.txt")

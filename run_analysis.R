# read test data
f<-file("UCI HAR Dataset/test/X_test.txt","rt")
v<-scan(f)
close(f)
m<-matrix(v,ncol=561)

# create dataframe from test data
ds<-data.frame(m)

# read activity label values for test data
alv<-read.table("UCI HAR Dataset/test/y_test.txt")
# read activity labels
al<-read.table("UCI HAR Dataset/activity_labels.txt",sep=" ")
# read subjects for test data
sub<-read.table("UCI HAR Dataset/test/subject_test.txt")

#extend ds with test data activity label values and labels and subject
ds <- cbind(ds,alv,al[alv[,1],2],sub)

# read train data
f<-file("UCI HAR Dataset/train/X_train.txt","rt")
v<-scan(f)
close(f)
m<-matrix(v,ncol=561)
dst <- data.frame(m)

# read activity label values for train data
alv<-read.table("UCI HAR Dataset/train/y_train.txt")
# read subjects for train data
sub<-read.table("UCI HAR Dataset/train/subject_train.txt")
#extend ds with train data activity label values and labels and subject
dst <- cbind(dst,alv,al[alv[,1],2],sub)

# merge together the test and train dataset
ds <- rbind(ds,dst)

# read features
features<-read.table("UCI HAR Dataset/features.txt",sep=" ",colClasses="character")
# set names for dataset
names(ds)<-c(features[,2],"ALV","AL","Subject")

# select only the mean, std attributes 
neededColsMean<-sqldf("select V1 from features where V2 like '%mean%'")
neededColsStd<-sqldf("select V1 from features where V2 like '%std%'")
neededCols<-rbind(neededColsMean,neededColsStd)
neededCols<-as.integer(neededCols[,1])

# and the added cols: ALV,AL,Subject
dsfinal <- ds[,c(neededCols,562,563,564)]
dsfinal$Subject<-factor(dsfinal$Subject)
dsfinal$AL<-factor(dsfinal$AL)

# calculate mean for each var for each Activity Label and each Subject
# and put it into dsmean
dsmean<-data.frame()
vn<-c()
rn<-c()
for (i in seq_along(neededCols)) {
  t<-data.frame(tapply(dsfinal[,i],list(dsfinal$AL,dsfinal$Subject),mean))
  dsmean <- rbind(dsmean,t)
  vn<-c(vn,rep(names(ds)[i],6))
  rn<-c(rn,rownames(t))
}
# put variable name and activity label into each row
dsmean<-cbind(data.frame(vn),dsmean)
dsmean<-cbind(data.frame(rn),dsmean)
# delete rownames
rownames(dsmean)<-c()

#write out dsmean
write.table(dsmean,"dsmean.txt",row.names=FALSE)
#this script will parse the pre logfile and extra the pre ratings data and ouptut to a subject specific csv

#####Read in Data

#read in run1 ratings log file, run1 faces/tones log file, scanid, and path
args<- commandArgs(TRUE)
data1<- read.table(args[1],fill=T)
scanid<- as.character(args[2])
path<- as.character(args[3])

#data1<- read.table("/import/monstrum/conte_815814/subjects/17648_9111/associated_files/ratings/pre/9111-Pre_task_faces_BL_wheel1.log",fill=TRUE)
#scanid<- "9111"
#path<- "/import/monstrum/conte_815814/subjects/17648_9111/associated_files/ratings/pre"
####Convert Logfiles to easily readable csv format for later manipulation

#rename columns
colnames(data1)<- c("Subject","Trial","Event_Type","Code","Time","TTime")
#subset columns
data1<- data1[,c(1,2,4)]
#create a column called Trial which gets the rows in order (so can re-order data later)
data1$Trial<- as.numeric(1:nrow(data1))

####Subset and Reshape Question and Answer File 

#find question and answer rows
q<- grep("quest",data1$Code)
a<- grep("ans",data1$Code)
#create two files, one with only question data and one with only answer data
q2<- as.data.frame(data1[q,])
a2<- as.data.frame(data1[a,])
#reorder data files by Trial
q2<- q2[order(q2$Trial),]
a2<- a2[order(a2$Trial),]
#delete unnecessary rows (the pre scan asks about 17 faces, there are only 4 in the task) 
q2<-q2[c(29:32,37:40),]
a2<-a2[c(29:32,37:40),]
#combine two data files so that question and answer are next to eachother
qa<- cbind(q2,a2$Trial,a2$Code)
#create a timepoint column that gets "pre"
qa$Timepoint<- "pre"
#subset data to only Subject, Question, Answer and Timepoint columns
qa<- qa[,c(1,3,5,6)]
#Rename Code and a2$Code columns to Question and Answer
colnames(qa)[2]<- "Question"
colnames(qa)[3]<- "Answer"
#add column (called Face) for which face goes with which questions 
qa$Face<- c(rep("face_1077",4),rep("face_1086",4))
#the answer column needs to be converted to something other than factors
qa$Answer<- as.character(qa$Answer)
qa$Answer<- substring(qa$Answer,4)
qa$Answer<- as.numeric(qa$Answer)
#because some of the files are incomplete/have missing questions, these will have NA rows. In order to reshape data the Subject column
#needs to not be NA, so create a variable called subject which gets the Subject number, then fill all NA subject column rows with this
#subject number
subject<- as.character(qa$Subject[1])
qa$Subject[is.na(qa$Subject)]<- subject
#reshape data so that each question is a column and has the face and answer below it (each subject is in a single row)
library(reshape2)
qa<- reshape(qa, timevar="Question",idvar=c("Subject","Timepoint"),direction="wide")

#####Write out data

write.csv(qa,paste(path,"/",scanid,"_pre_ratings_data.csv",sep=""))

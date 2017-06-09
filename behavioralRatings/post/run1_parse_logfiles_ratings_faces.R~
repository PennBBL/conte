#####Read in Data

#read in run1 ratings log file, run1 faces/tones log file, and scanid
args<- commandArgs(TRUE)
data1<- read.table(args[1],fill=T)
data2<- read.table(args[2],fill=T)
scanid<- as.character(args[3])
path<- as.character(args[4])

####Convert Logfiles to easily readable csv format for later manipulation

#rename columns
colnames(data1)<- c("Subject","Trial","Event_Type","Code","Time","TTime")
#subset columns
data1<- data1[,c(1,2,4)]
#create a column called Trial which gets the rows in order (so can re-order data later)
data1$Trial<- as.numeric(1:nrow(data1))
#rename columns
colnames(data2)<- c("Subject","Trial","Event_Type","Code","Time","TTime")
#subset columns
data2<- data2[,c(1,2,4)]
#create a column called Trial which gets the rows in order (so can re-order data later)
data2$Trial<- as.numeric(1:nrow(data2))

####Subset and Reshape Data1 (Question and Answer) File 

#find question and answer rows
q<- grep("quest",data1$Code)
a<- grep("ans",data1$Code)
#create two files, one with only question data and one with only answer data
q2<- as.data.frame(data1[q,])
a2<- as.data.frame(data1[a,])
#reorder data files by Trial
q2<- q2[order(q2$Trial),]
a2<- a2[order(a2$Trial),]
#combine two data files so that question and answer are next to eachother
qa<- cbind(q2,a2$Trial,a2$Code)
#create a timepoint column that gets "run1"
qa$Timepoint<- "run1"
#subset data to only Subject, Question, Answer and Timepoint columns
qa<- qa[,c(1,3,5,6)]
#Rename Code and a2$Code columns to Question and Answer
colnames(qa)[2]<- "Question"
colnames(qa)[3]<- "Answer"
#add column (called Face) for which face goes with which questions ##########this needs fixing############
qa$Face<- c(rep("face_1077",4),rep("face_1086",4),"both",NA)
#the answer column needs to be converted to something other than factors
qa$Answer<- as.character(qa$Answer)
qa$Answer<- substring(qa$Answer,4)
qa$Answer<- as.numeric(qa$Answer)
#put in if statement that looks at question 9 answer and fills in face cell for question 10 depending on question 9 answer
if(qa$Answer[ qa$Question=="quest9"]==-140){
qa$Face[qa$Question=="quest10"]<- "face_1077"
}
if(qa$Answer[ qa$Question=="quest9"]==140){
qa$Face[qa$Question=="quest10"]<- "face_1086"
}
#reshape data so that each question is a column and has the face and answer below it (each subject is in a single row)
library(reshape2)
qa<- reshape(qa, timevar="Question",idvar=c("Subject","Timepoint"),direction="wide")

####Create Face Condition File 

#find face and tone rows
face_1077_condition<- data2[ grep("what_face_1077",data2$Code),]
face_1086_condition<- data2[ grep("what_face_1086",data2$Code),]

#create a file which gets face conditions
face<- data.frame(Subject=scanid,Timepoint="run1",face_1077_condition=NA,face_1086_condition=NA)

#populate cells depending on which face is aversive
tone_1077 <- grep("what_face_1077_tone",face_1077_condition$Code)[1]
tone_1086 <- grep("what_face_1086_tone",face_1086_condition$Code)[1]

if(! is.na(tone_1077)==TRUE){
  face$face_1077_condition<-"aversive"
  face$face_1086_condition<-"neutral"
} 
if(! is.na(tone_1086)==TRUE){
  face$face_1086_condition<-"aversive"
  face$face_1077_condition<-"neutral"
}

#####Write out data

write.csv(qa,paste(path,"/",scanid,"_run1_ratings_data.csv",sep=""))
write.csv(face,paste(path,"/",scanid,"_run1_faces_data.csv",sep=""))

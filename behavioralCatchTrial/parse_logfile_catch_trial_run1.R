#this script is run by the wrapper /data/joy/BBL/projects/conteReproc2017/behavioralCatchTrial/get_catch_trial_data.sh and will extract catch trial data for each subject passed to this script

#####Read in Data

#read in run1 faces/tones log file, scanid and path to the subject's catch trial directory
args<- commandArgs(TRUE)
data1<- read.table(args[1],fill=T)
scanid<- as.character(args[2])
path<- as.character(args[3])

#for one subject
#scanid<- as.character(9962)
#data1<-read.table("/import/monstrum/conte_815814/subjects/82051_9962/associated_files/ratings/post/09962-fearConditioning_run1_wheel_Pairing0.log",fill=T)
#path<-as.character("/import/monstrum/conte_815814/subjects/82051_9962/associated_files/behavioral_qa/catch_trials")
  
####Convert Logfiles to easily readable csv format for later manipulation

#rename columns
colnames(data1)<- c("Subject","Trial","Event_Type","Code","Time","TTime")
#create a column called Trial which gets the rows in order (so can re-order data later)
data1$Trial<- as.numeric(1:nrow(data1))
#subset to only necessary columns
data1<- data1[,1:6]

####create variables for the pairing 
what_face<-  grep("what_face",data1$Code)
pairing<- as.data.frame(data1[what_face,])
pairing$Code<- as.character(pairing$Code)

#set appropriate variables for onset times of different stimuli depending on which order the subject had
if(unlist(strsplit(pairing[1,4],split="_"))[4]=="tone"){
  catch<- read.table("/data/joy/BBL/studies/conte/fmriDesignFiles/order_0_2/run1/catch.txt",fill=T)
  tone<- read.table("/data/joy/BBL/studies/conte/fmriDesignFiles/order_0_2/run1/aversive_tone.txt",fill=T)
  face_av<- read.table("/data/joy/BBL/studies/conte/fmriDesignFiles/order_0_2/run1/face1_aversive.txt",fill=T)
  face_notone<- read.table("/data/joy/BBL/studies/conte/fmriDesignFiles/order_0_2/run1/face1_notone.txt",fill=T)
  face_neu<- read.table("/data/joy/BBL/studies/conte/fmriDesignFiles/order_0_2/run1/face2_notone.txt",fill=T)
} else if(unlist(strsplit(pairing[1,4],split="_"))[4]=="notone"){
  catch<- read.table("/data/joy/BBL/studies/conte/fmriDesignFiles/order_1_3/run1/catch.txt",fill=T) 
  tone<- read.table("/data/joy/BBL/studies/conte/fmriDesignFiles/order_1_3/run1/aversive_tone.txt",fill=T)
  face_neu<- read.table("/data/joy/BBL/studies/conte/fmriDesignFiles/order_1_3/run1/face1_notone.txt",fill=T)
  face_av<- read.table("/data/joy/BBL/studies/conte/fmriDesignFiles/order_1_3/run1/face2_aversive.txt",fill=T)
  face_notone<- read.table("/data/joy/BBL/studies/conte/fmriDesignFiles/order_1_3/run1/face2_notone.txt",fill=T)
} else {
  Print("ERROR NEITHER ORDER 0 2 NOR ORDER 1 3")
}

####Subset and Reshape Data

#find Response rows
r<- grep("Response",data1$Event_Type)
#create file with only response data
response<- as.data.frame(data1[r,])
#reorder data files by Trial
response<- response[order(response$Trial),]

#create a timepoint column that gets "run1"
response$Timepoint<- "run1"

#find start time from data 1 (we delete first six volumes so start time should be at pulse count 7)
start_time<- grep("pulseCount_7",data1$Code,fixed=TRUE)
start_time<- data1[start_time[1],] 

#create a column called delta time which gets the time minus the start time so can get onset time and translate delta 10th miliseconds
#to delta seconds
response$Time<- as.numeric(as.character(response$Time))
start_time$Time<- as.numeric(as.character(start_time$Time))
response$delta_time<- response$Time-start_time$Time
response$onset<- response$delta_time/10000

#create trial data frame so can match responses to the trials
trials<- rbind(catch,tone,face_av,face_notone,face_neu)
trials$type<- c(rep("catch",8),rep("tone",12),rep("face_aversive",12),rep("face_notone",12),rep("face_neu",24))
trials<- trials[,c(1,4)]
colnames(trials)[1]<- "onset"

cross<- data1[ grep("cross",data1$Code,fixed=TRUE),]
cross<- cross[grep("green_cross",cross$Code,invert=TRUE),c(5,4)]
colnames(cross)[1:2]<- c("onset","type")
cross$onset<- as.numeric(as.character(cross$onset))
cross$onset<- cross$onset-start_time$Time
cross$onset<- cross$onset/10000

trials<- rbind(trials,cross)
trials<- trials[order(trials$onset),]


#put in if statement that looks at the response trial time and if there is a trial at that time (or within 2 seconds) 
#then put the catch trial time in the response file in a column called match
response$match<- NA
response$match_type<- NA

for (i in 1:nrow(response)){
  for (j in 1:nrow(trials)){
  x<- trials[j,1]
  y<- response[i,"onset"]
  type<- trials[j,"type"]
  if (y>x && y<x+2){
    response$match[i]<- x
    response$match_type[i]<- type
  } 
  }
}

#table response types
response_type<- data.frame(scanid=scanid)
response_type$face_aversive_tone<- sum(response$match_type=="face_aversive",na.rm=T)
response_type$face_neutral<- sum(response$match_type=="face_neu",na.rm=T)
response_type$face_aversive_notone<- sum(response$match_type=="face_notone",na.rm=T)
response_type$aversive_tone<- sum(response$match_type=="tone",na.rm=T)
response_type$cross<- sum(response$match_type=="cross",na.rm=T)
response_type$catch<- sum(response$match_type=="catch",na.rm=T)

#check to make sure that there are the correct number of responses based on catch trials 
num_catch_missing<- 8-sum(! duplicated(response$match[response$match_type=="catch" & ! is.na(response$match_type)]))
num_catch_extra_responses<- sum(duplicated(response$match[response$match_type=="catch" & ! is.na(response$match_type)]))
non_catch<- nrow(response[! response$match_type=="catch" & ! is.na(response$onset),])

sum_response<- as.data.frame(t(c(scanid,num_catch_missing,num_catch_extra_responses,non_catch)))
colnames(sum_response)<- c("scanid","number_catch_responses_missing","number_catch_extra_responses","number_responses_to_non_catch_trials")

#####Write out data
response<- response[,c("Subject","Timepoint","Event_Type","onset","match","match_type")]
write.csv(response,paste(path,"/",scanid,"_run1_response_trial_data.csv",sep=""))
write.csv(sum_response,paste(path,"/",scanid,"_run1_summary_response_trial_data.csv",sep=""))
write.csv(response_type,paste(path,"/",scanid,"_run1_response_type_table_data.csv",sep=""))

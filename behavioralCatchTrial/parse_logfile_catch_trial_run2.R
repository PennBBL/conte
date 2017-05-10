#####Read in Data

#read in run1 faces/tones log file, scanid
args<- commandArgs(TRUE)
data1<- read.table(args[1],fill=T)
scanid<- as.character(args[2])
path<- as.character(args[3])

#for one subject
#scanid<- as.character(9241)
#data1<-read.table("/import/monstrum/conte_815814/subjects/18356_9241/ratings/post/09241-fearConditioning_rev_wheel.log",fill=T)

####Convert Logfiles to easily readable csv format for later manipulation

#rename columns
colnames(data1)<- c("Subject","Trial","Event_Type","Code","Time","TTime")
#create a column called Trial which gets the rows in order (so can re-order data later)
data1$Trial<- as.numeric(1:nrow(data1))
#subset to only necessary columns
data1<- data1[,1:6]

####load in stimuli data depending on if the subject is order 0_2 or order 1_3

what_face<-  grep("what_face",data1$Code)
pairing<- as.data.frame(data1[what_face,])
pairing$Code<- as.character(pairing$Code)

if(unlist(strsplit(pairing[1,4],split="_"))[4]=="tone"){
  catch<- read.table("/data/joy/BBL/studies/conte/fmriDesignFiles/order_0_2/run2_rev/catch.txt",fill=T)
  tone<- read.table("/data/joy/BBL/studies/conte/fmriDesignFiles/order_0_2/run2_rev/aversive_tone.txt",fill=T)
  face_av<- read.table("/data/joy/BBL/studies/conte/fmriDesignFiles/order_0_2/run2_rev/face2_aversive.txt",fill=T)
  face_notone<- read.table("/data/joy/BBL/studies/conte/fmriDesignFiles/order_0_2/run2_rev/face2_notone.txt",fill=T)
  face_neu<- read.table("/data/joy/BBL/studies/conte/fmriDesignFiles/order_0_2/run2_rev/face1_notone.txt",fill=T)
} else if(unlist(strsplit(pairing[1,4],split="_"))[4]=="notone"){
  catch<- read.table("/data/joy/BBL/studies/conte/fmriDesignFiles/order_1_3/run2_rev/catch.txt",fill=T) 
  tone<- read.table("/data/joy/BBL/studies/conte/fmriDesignFiles/order_1_3/run2_rev/aversive_tone.txt",fill=T)
  face_neu<- read.table("/data/joy/BBL/studies/conte/fmriDesignFiles/order_1_3/run2_rev/face2_notone.txt",fill=T)
  face_av<- read.table("/data/joy/BBL/studies/conte/fmriDesignFiles/order_1_3/run2_rev/face1_aversive.txt",fill=T)
  face_notone<- read.table("/data/joy/BBL/studies/conte/fmriDesignFiles/order_1_3/run2_rev/face1_notone.txt",fill=T)
} else {
  Print("ERROR NEITHER ORDER 0 2 NOR ORDER 1 3")
}

trials<- rbind(catch,tone,face_av,face_notone,face_neu)
trials$type<- c(rep("catch",8),rep("tone",12),rep("face_aversive",12),rep("face_notone",12),rep("face_neu",24))
trials<- trials[order(trials$V1),]

####Subset and Reshape Data

#find Response rows
r<- grep("Response",data1$Event_Type)
#create file with only response data
response<- as.data.frame(data1[r,])
#reorder data files by Trial
response<- response[order(response$Trial),]

#create a timepoint column that gets "run2"
response$Timepoint<- "run2"

#find start time from data 1 (we delete first six volumes so start time should be at pulse count 7)
start_time<- grep("pulseCount_7",data1$Code,fixed=TRUE)
start_time<- data1[start_time[1],] 

#create a column called delta time which gets the time minus the start time so can get onset time and translate delta 10th miliseconds
#to delta seconds
response$Time<- as.numeric(as.character(response$Time))
start_time$Time<- as.numeric(as.character(start_time$Time))
response$delta_time<- response$Time-start_time$Time
response$onset<- response$delta_time/10000

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
write.csv(response,paste(path,"/",scanid,"_run2_response_trial_data.csv",sep=""))
write.csv(sum_response,paste(path,"/",scanid,"_run2_summary_response_trial_data.csv",sep=""))
write.csv(response_type,paste(path,"/",scanid,"_run2_response_type_table_data.csv",sep=""))

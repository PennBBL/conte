#this script will loop through subject's pre and post ratings data and calculate a subtraction of pre - post rating answer then output this information along with which face was aversive or neutral to an aggregate csv file

#read in data, will need to change the csv's to the most recent run (denoted by the date at the end)
condition<- read.csv("/data/joy/BBL/studies/conte/subjectData/behavioralRatings/post/run1_faces_2017-03-01.csv")
pre<- read.csv("/data/joy/BBL/studies/conte/subjectData/behavioralRatings/pre/pre_ratings_2017-03-01.csv")
post<- read.csv("/data/joy/BBL/studies/conte/subjectData/behavioralRatings/post/run1_ratings_2017-03-01.csv")
dx<- read.csv("/data/joy/BBL/studies/conte/subjectData/design_data_n96.csv",header=FALSE,sep="\t")

#load in libararies and functions
source("/home/mquarmley/summary_se_function.R")
library(reshape2)
library(ggplot2)

#create a variable called date which gets the date
date<- Sys.Date()

#subset the condition data to only desired columns and merge diagnosis, condition, pre and post data all into a data frame called data
condition<- condition[,c(1,3,4)]
data<- merge(dx,condition,by.x="scanid",by.y="Subject",all=T,sort=F)
data<- merge(data,pre,by.x="scanid",by.y="Subject",all=T,sort=F)
data<- merge(data,post,by.x="scanid",by.y="Subject",all=T,sort=F)

####CONVERT THE ORIGINAL ANSWERS TO A MORE INTUITIVE NUMERICAL SYSTEM IN GRAPHS (0-100 or -100-100) as per DW 01/2016#####

#convert questions of arousal and confidence to 0-100 (add 420 to answer and then divide by 8.4)
#convert questions of valence (friendly, positive emotion, positive feeling) to -100 - 100 (divide answer by 4.2)
#convert accuracy question to 0-100 (0 is incorrect; 100 is correct) (convert q9 column 140 to 1086 and -140 to 1077 then check for match with aversive face column then change answer to correct (100) and incorrect (0))
#Arousal and confidence (pre Q's: Q4, Q8; post Q's: Q4, Q8, Q10)
#Valence (pre Q's: Q1, Q2, Q3, Q5, Q6, Q7; post Q's: Q1, Q2, Q3, Q5, Q6, Q7)
#Accuracy question (post Q9)

#Arousal and confidence
pre$Answer.quest32<- (pre$Answer.quest32+420)/8.4
pre$Answer.quest40<- (pre$Answer.quest40+420)/8.4
post$Answer.quest4<- (post$Answer.quest4+420)/8.4
post$Answer.quest8<- (post$Answer.quest8+420)/8.4
post$Answer.quest10<- (post$Answer.quest10+420)/8.4

#Valence
pre$Answer.quest29<- (pre$Answer.quest29)/4.2
pre$Answer.quest37<- (pre$Answer.quest37)/4.2
pre$Answer.quest30<- (pre$Answer.quest30)/4.2
pre$Answer.quest38<- (pre$Answer.quest38)/4.2
pre$Answer.quest31<- (pre$Answer.quest31)/4.2
pre$Answer.quest39<- (pre$Answer.quest39)/4.2
post$Answer.quest1<- (post$Answer.quest1)/4.2
post$Answer.quest5<- (post$Answer.quest5)/4.2
post$Answer.quest2<- (post$Answer.quest2)/4.2
post$Answer.quest6<- (post$Answer.quest6)/4.2
post$Answer.quest3<- (post$Answer.quest3)/4.2
post$Answer.quest7<- (post$Answer.quest7)/4.2


#for each row (subject) 
for (i in 1:nrow(data)){
  
#if there is data for face_1077 and face 1077 is aversive calculate the pre - post absolute value subtraction and difference (if pre or post is more positive or negative) for each question pairing pre and post
  if(data$face_1077[i]=="aversive" & ! is.na(data$face_1077[i])){
   
    data$pre_post_q1_av[i]<- abs(data$Answer.quest29[i]-data$Answer.quest1[i])
    if (data$Answer.quest29[i]>data$Answer.quest1[i] && ! is.na(data$Answer.quest29[i]) && ! is.na(data$Answer.quest1[i])){
      data$pre_post_q1_av[i]<- (data$pre_post_q1_av[i])*(-1)
    } 
    data$pre_post_q2_av[i]<- abs(data$Answer.quest30[i]-data$Answer.quest2[i])
    if (data$Answer.quest30[i]>data$Answer.quest2[i]  && ! is.na(data$Answer.quest30[i]) && ! is.na(data$Answer.quest2[i])){
      data$pre_post_q2_av[i]<- (data$pre_post_q2_av[i])*(-1)
    } 
    data$pre_post_q3_av[i]<- abs(data$Answer.quest31[i]-data$Answer.quest3[i])
    if (data$Answer.quest31[i]>data$Answer.quest3[i]  && ! is.na(data$Answer.quest31[i]) && ! is.na(data$Answer.quest3[i])){
      data$pre_post_q3_av[i]<- (data$pre_post_q3_av[i])*(-1)
    } 
    data$pre_post_q4_av[i]<- abs(data$Answer.quest4[i]-data$Answer.quest32[i])
    if (data$Answer.quest32[i]<data$Answer.quest4[i]  && ! is.na(data$Answer.quest32[i]) && ! is.na(data$Answer.quest4[i])){
      data$pre_post_q4_av[i]<- (data$pre_post_q4_av[i])*(-1)
    } 
    data$pre_post_q1_neu[i]<- abs(data$Answer.quest37[i]-data$Answer.quest5[i])
    if (data$Answer.quest37[i]>data$Answer.quest5[i]  && ! is.na(data$Answer.quest37[i]) && ! is.na(data$Answer.quest5[i])){
      data$pre_post_q1_neu[i]<- (data$pre_post_q1_neu[i])*(-1)
    } 
    data$pre_post_q2_neu[i]<- abs(data$Answer.quest38[i]-data$Answer.quest6[i])
    if (data$Answer.quest38[i]>data$Answer.quest6[i]  && ! is.na(data$Answer.quest38[i]) && ! is.na(data$Answer.quest6[i])){
      data$pre_post_q2_neu[i]<- (data$pre_post_q2_neu[i])*(-1)
    } 
    data$pre_post_q3_neu[i]<- abs(data$Answer.quest39[i]-data$Answer.quest7[i])
    if (data$Answer.quest39[i]>data$Answer.quest7[i]  && ! is.na(data$Answer.quest39[i]) && ! is.na(data$Answer.quest7[i])){
      data$pre_post_q3_neu[i]<- (data$pre_post_q3_neu[i])*(-1)
    } 
    data$pre_post_q4_neu[i]<- abs(data$Answer.quest8[i]-data$Answer.quest40[i]) 
    if (data$Answer.quest40[i]<data$Answer.quest8[i]  && ! is.na(data$Answer.quest40[i]) && ! is.na(data$Answer.quest8[i])){
      data$pre_post_q4_neu[i]<- (data$pre_post_q4_neu[i])*(-1)
    } 
   
  }

#if there is data for face_1086 and face 1086 is aversive calculate the pre - post absolute value subtraction and difference (if pre or post is more positive or negative) for each question pairing pre and post
  if(data$face_1086[i]=="aversive" & ! is.na(data$face_1086[i])){
    
    data$pre_post_q1_neu[i]<- abs(data$Answer.quest29[i]-data$Answer.quest1[i])
    if (data$Answer.quest29[i]>data$Answer.quest1[i]  && ! is.na(data$Answer.quest29[i]) && ! is.na(data$Answer.quest1[i])){
      data$pre_post_q1_neu[i]<- (data$pre_post_q1_neu[i])*(-1)
    } 
    data$pre_post_q2_neu[i]<- abs(data$Answer.quest30[i]-data$Answer.quest2[i])
    if (data$Answer.quest30[i]>data$Answer.quest2[i]   && ! is.na(data$Answer.quest30[i]) && ! is.na(data$Answer.quest2[i])){
      data$pre_post_q2_neu[i]<- (data$pre_post_q2_neu[i])*(-1)
    } 
    data$pre_post_q3_neu[i]<- abs(data$Answer.quest31[i]-data$Answer.quest3[i])
    if (data$Answer.quest31[i]>data$Answer.quest3[i]  && ! is.na(data$Answer.quest31[i]) && ! is.na(data$Answer.quest3[i])){
      data$pre_post_q3_neu[i]<- (data$pre_post_q3_neu[i])*(-1)
    } 
    data$pre_post_q4_neu[i]<- abs(data$Answer.quest4[i]-data$Answer.quest32[i])
    if (data$Answer.quest32[i]<data$Answer.quest4[i]  && ! is.na(data$Answer.quest32[i]) && ! is.na(data$Answer.quest4[i])){
      data$pre_post_q4_neu[i]<- (data$pre_post_q4_neu[i])*(-1)
    } 
    data$pre_post_q1_av[i]<- abs(data$Answer.quest37[i]-data$Answer.quest5[i])
    if (data$Answer.quest37[i]>data$Answer.quest5[i]  && ! is.na(data$Answer.quest37[i]) && ! is.na(data$Answer.quest5[i])){
      data$pre_post_q1_av[i]<- (data$pre_post_q1_av[i])*(-1)
    } 
    data$pre_post_q2_av[i]<- abs(data$Answer.quest38[i]-data$Answer.quest6[i])
    if (data$Answer.quest38[i]>data$Answer.quest6[i]  && ! is.na(data$Answer.quest38[i]) && ! is.na(data$Answer.quest6[i])){
      data$pre_post_q2_av[i]<- (data$pre_post_q2_av[i])*(-1)
    } 
    data$pre_post_q3_av[i]<- abs(data$Answer.quest39[i]-data$Answer.quest7[i])
    if (data$Answer.quest39[i]>data$Answer.quest7[i]  && ! is.na(data$Answer.quest39[i]) && ! is.na(data$Answer.quest7[i])){
      data$pre_post_q3_av[i]<- (data$pre_post_q3_av[i])*(-1)
    } 
    data$pre_post_q4_av[i]<- abs(data$Answer.quest8[i]-data$Answer.quest40[i]) 
    if (data$Answer.quest40[i]<data$Answer.quest8[i]  && ! is.na(data$Answer.quest40[i]) && ! is.na(data$Answer.quest8[i])){
      data$pre_post_q4_av[i]<- (data$pre_post_q4_av[i])*(-1)
    } 
  }

#if either actor condition (aversive or neutral) is missing then output an error and NA data to the file
  if(is.na(data$face_1077[i]) | is.na(data$face_1086[i])){
  print(paste(i, "is missing condition",sep=" "))
  data$pre_post_q1_av[i]<- NA
  data$pre_post_q2_av[i]<- NA
  data$pre_post_q3_av[i]<- NA
  data$pre_post_q4_av[i]<- NA
  data$pre_post_q1_neu[i]<- NA
  data$pre_post_q2_neu[i]<- NA
  data$pre_post_q3_neu[i]<- NA
  data$pre_post_q4_neu[i]<- NA
  }
}

#subset the data to the subtracted questions and write out the data  
data2<- data[,c(1,2,3,4,5,44:51)]
write.csv(data,file=paste("/data/joy/BBL/studies/conte/subjectData/behavioralRatings/pre-post_delta_ratings_by_condition",date,".csv",sep="")

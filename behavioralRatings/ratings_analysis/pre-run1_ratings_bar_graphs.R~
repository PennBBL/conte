#this script creates bar graphs for pre minus run1 ratings while taking into account diagnosis

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

#rename first few columns in dx dataset and exclude any that were fmri excludes
colnames(dx)[1:4]<- c("bblid","scanid","exclude","dx")

#reshape condition data
condition<- melt(condition,id.vars=c("Subject","Timepoint"),measure.vars=c("face_1077","face_1086"))

#edit the scanid in post from 9552 to 9952, the logfiles have the wrong scanid in there
post$Subject<- as.character(post$Subject)
post$Subject[ post$Subject=="9552"]<- "9952"

####CONVERT THE ORIGINAL ANSWERS TO A MORE INTUITIVE NUMERICAL SYSTEM IN GRAPHS (0-100 or -100-100) as per DW 01/2016#####

#convert questions of arousal and confidence to 0-100 (add 420 to answer and then divide by 8.4)
#convert questions of valence (friendly, positive emotion, positive feeling) to -100 - 100 (divide answer by 4.2)
#convert accuracy question to 0-100 (0 is incorrect; 100 is correct) (convert q9 column 140 to 1086 and -140 to 1077 then check for match with aversive face column then change answer to correct (100) and incorrect (0))
#Arousal and confidence (pre Q's: Q4, Q8; post Q's: Q4, Q8, Q10)
#Valence (pre Q's: Q1, Q2, Q3, Q5, Q6, Q7; post Q's: Q1, Q2, Q3, Q5, Q6, Q7)
#Accuracy question (post Q9)

#Arousal and confidence
#Optional to include or exclude subjects that were fMRI excludes (those that weren't in the dx dataset because of exclusions)
pre$exclude<- dx$exclude[ match(pre$Subject,dx$scanid)]
pre<- pre[pre$exclude=="0",]
post$exclude<- dx$exclude[ match(post$Subject,dx$scanid)]
post<- post[post$exclude=="0",]

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

#write this data out to csv's so have the converted raw data saved
write.csv(pre,file=paste("/data/joy/BBL/studies/conte/subjectData/ratingsPre/pre_ratings_data_",date,".csv",sep="")
write.csv(post,file=paste("/data/joy/BBL/studies/conte/subjectData/ratingsPost/run1_ratings_data_",date,".csv",sep="")


#################PRE######################

#####Question 1

#for pre question 1 transform the data and rename the questions to which actor/face they refer to
pre_q1<- melt(pre,id.vars=c("Subject","Timepoint"),measure.vars=c("Answer.quest29","Answer.quest37"))
pre_q1$variable<- as.character(pre_q1$variable)
pre_q1$variable[ pre_q1$variable=="Answer.quest29"]<- "face_1077"
pre_q1$variable[ pre_q1$variable=="Answer.quest37"]<- "face_1086"

#merge in diagnosis
pre_q1$dx<- dx$dx[ match(pre_q1$Subject,dx$scanid)]
#Optional to include or exclude subjects without a diagnosis (those that weren't in the dx dataset because of exclusions)
pre_q1<- pre_q1[! is.na(pre_q1$dx),]

#create a table which gets mean, se, sd of rating for the question split by diagnosis, and rename the numerical diagnostic codings into letters
pre_q1_se<- summarySE(pre_q1, measurevar="value", groupvars="dx",na.rm=T)
pre_q1_se$dx<- as.character(pre_q1_se$dx)
pre_q1_se$dx[ pre_q1_se$dx=="0"]<- "1_NC"
pre_q1_se$dx[ pre_q1_se$dx=="1"]<- "2_CR"
pre_q1_se$dx[ pre_q1_se$dx=="2"]<- "3_P"

#print out a bar graph with diagnosis on the x axis, mean rating on the y axis
ggplot(data=pre_q1_se, aes(x=dx, y=value, fill=dx)) +
  geom_bar(stat="identity", position=position_dodge()) +
  geom_bar(colour="black", position=position_dodge(), show_guide=FALSE, stat="identity") +
  geom_errorbar(aes(ymin=value-se, ymax=value+se), width=.2, position=position_dodge(.9)) +
  xlab("Diagnosis") +
  ylab("mean rating") +
  scale_x_discrete(breaks=c("1_NC", "2_CR", "3_P"), labels=c("Normal Control","Clinical Risk","Patient")) +
  ggtitle("Pre: How friendly or unfriendly does\n this person seem?") +
  scale_fill_manual(name="Diagnosis",breaks=c("1_NC", "2_CR", "3_P"),labels=c("Normal Control","Clinical Risk","Patient"),values=c("green","blue","red"))

#####Question 2

#for pre question 2 transform the data and rename the questions to which actor/face they refer to
pre_q2<- melt(pre,id.vars=c("Subject","Timepoint"),measure.vars=c("Answer.quest30","Answer.quest38"))
pre_q2$variable<- as.character(pre_q2$variable)
pre_q2$variable[ pre_q2$variable=="Answer.quest30"]<- "face_1077"
pre_q2$variable[ pre_q2$variable=="Answer.quest38"]<- "face_1086"

#merge in diagnosis
pre_q2$dx<- dx$dx[ match(pre_q2$Subject,dx$scanid)]
#Optional to include or exclude subjects without a diagnosis (those that weren't in the dx dataset because of exclusions)
pre_q2<- pre_q2[! is.na(pre_q2$dx),]

#create a table which gets mean, se, sd of rating for the question split by diagnosis, and rename the numerical diagnostic codings into letters
pre_q2_se<- summarySE(pre_q2, measurevar="value", groupvars="dx",na.rm=T)
pre_q2_se$dx<- as.character(pre_q2_se$dx)
pre_q2_se$dx[ pre_q2_se$dx=="0"]<- "1_NC"
pre_q2_se$dx[ pre_q2_se$dx=="1"]<- "2_CR"
pre_q2_se$dx[ pre_q2_se$dx=="2"]<- "3_P"

#print out a bar graph with diagnosis on the x axis, mean rating on the y axis
ggplot(data=pre_q2_se, aes(x=dx, y=value, fill=dx)) +
  geom_bar(stat="identity", position=position_dodge()) +
  geom_bar(colour="black", position=position_dodge(), show_guide=FALSE, stat="identity") +
  geom_errorbar(aes(ymin=value-se, ymax=value+se), width=.2, position=position_dodge(.9)) +
  xlab("Diagnosis") +
  scale_x_discrete(breaks=c("1_NC", "2_CR", "3_P"), labels=c("Normal Control","Clinical Risk","Patient")) +
  ylab("mean rating") +
  ggtitle("Pre: How positive or negative is the\n emotional expression on this persons face?") +
  scale_fill_manual(name="Diagnosis",breaks=c("1_NC", "2_CR", "3_P"),labels=c("Normal Control","Clinical Risk","Patient"),values=c("green","blue","red"))

#####Question 3

#for pre question 3 transform the data and rename the questions to which actor/face they refer to
pre_q3<- melt(pre,id.vars=c("Subject","Timepoint"),measure.vars=c("Answer.quest31","Answer.quest39"))
pre_q3$variable<- as.character(pre_q3$variable)
pre_q3$variable[ pre_q3$variable=="Answer.quest31"]<- "face_1077"
pre_q3$variable[ pre_q3$variable=="Answer.quest39"]<- "face_1086"

#merge in diagnosis
pre_q3$dx<- dx$dx[ match(pre_q3$Subject,dx$scanid)]
#Optional to include or exclude subjects without a diagnosis (those that weren't in the dx dataset because of exclusions)
pre_q3<- pre_q3[! is.na(pre_q3$dx),]

#create a table which gets mean, se, sd of rating for the question split by diagnosis, and rename the numerical diagnostic codings into letters
pre_q3_se<- summarySE(pre_q3, measurevar="value", groupvars="dx",na.rm=T)
pre_q3_se$dx<- as.character(pre_q3_se$dx)
pre_q3_se$dx[ pre_q3_se$dx=="0"]<- "1_NC"
pre_q3_se$dx[ pre_q3_se$dx=="1"]<- "2_CR"
pre_q3_se$dx[ pre_q3_se$dx=="2"]<- "3_P"

#print out a bar graph with diagnosis on the x axis, mean rating on the y axis
ggplot(data=pre_q3_se, aes(x=dx, y=value, fill=dx)) +
  geom_bar(stat="identity", position=position_dodge()) +
  geom_bar(colour="black", position=position_dodge(), show_guide=FALSE, stat="identity") +
  geom_errorbar(aes(ymin=value-se, ymax=value+se), width=.2, position=position_dodge(.9)) +
  xlab("Diagnosis") +
  scale_x_discrete(breaks=c("1_NC", "2_CR", "3_P"), labels=c("Normal Control","Clinical Risk","Patient")) +
  ylab("mean rating") +
  ylim(-30,15) +
  ggtitle("Pre: How positive or negative do you\n feel when you look at this persons face?") +
  scale_fill_manual(name="Diagnosis",breaks=c("1_NC", "2_CR", "3_P"),labels=c("Normal Control","Clinical Risk","Patient"),values=c("green","blue","red"))

#####Question 4

#for pre question 4 transform the data and rename the questions to which actor/face they refer to
pre_q4<- melt(pre,id.vars=c("Subject","Timepoint"),measure.vars=c("Answer.quest32","Answer.quest40"))
pre_q4$variable<- as.character(pre_q4$variable)
pre_q4$variable[ pre_q4$variable=="Answer.quest32"]<- "face_1077"
pre_q4$variable[ pre_q4$variable=="Answer.quest40"]<- "face_1086"

#merge in diagnosis
pre_q4$dx<- dx$dx[ match(pre_q4$Subject,dx$scanid)]
#Optional to include or exclude subjects without a diagnosis (those that weren't in the dx dataset because of exclusions)
pre_q4<- pre_q4[! is.na(pre_q4$dx),]

#create a table which gets mean, se, sd of rating for the question split by diagnosis, and rename the numerical diagnostic codings into letters
pre_q4_se<- summarySE(pre_q4, measurevar="value", groupvars="dx",na.rm=T)
pre_q4_se$dx<- as.character(pre_q4_se$dx)
pre_q4_se$dx[ pre_q4_se$dx=="0"]<- "1_NC"
pre_q4_se$dx[ pre_q4_se$dx=="1"]<- "2_CR"
pre_q4_se$dx[ pre_q4_se$dx=="2"]<- "3_P"

#print out a bar graph with diagnosis on the x axis, mean rating on the y axis
ggplot(data=pre_q4_se, aes(x=dx, y=value, fill=dx)) +
  geom_bar(stat="identity", position=position_dodge()) +
  geom_bar(colour="black", position=position_dodge(), show_guide=FALSE, stat="identity") +
  geom_errorbar(aes(ymin=value-se, ymax=value+se), width=.2, position=position_dodge(.9)) +
  xlab("Diagnosis") +
  scale_x_discrete(breaks=c("1_NC", "2_CR", "3_P"), labels=c("Normal Control","Clinical Risk","Patient")) +
  ylab("mean rating") +
  ggtitle("Pre: How strong are the emotions you\n feel when you see this person?") +
  scale_fill_manual(name="Diagnosis",breaks=c("1_NC", "2_CR", "3_P"),labels=c("Normal Control","Clinical Risk","Patient"),values=c("green","blue","red"))


##################RUN 1###################

#####Question 1

#for post question 1 transform the data and rename the questions to which actor/face they refer to
run1_q1<- melt(post,id.vars=c("Subject","Timepoint"),measure.vars=c("Answer.quest1","Answer.quest5"))
run1_q1$variable<- as.character(run1_q1$variable)
run1_q1$variable[ run1_q1$variable=="Answer.quest1"]<- "face_1077"
run1_q1$variable[ run1_q1$variable=="Answer.quest5"]<- "face_1086"

#merge in condition (aversive or neutral) and diagnosis 
run1_q1 <- merge(run1_q1, condition[c("Subject","variable","value")], by=c("Subject","variable"),sort=FALSE)
colnames(run1_q1)[4:5]<- c("value","condition")
run1_q1$dx<- dx$dx[ match(run1_q1$Subject,dx$scanid)]

#Optional to include or exclude subjects without a diagnosis (those that weren't in the dx dataset because of exclusions)
run1_q1<- run1_q1[! is.na(run1_q1$dx),]

#create a table which gets mean, se, sd of rating for the question split by diagnosis, and rename the numerical diagnostic codings into letters
run1_q1_se<- summarySE(run1_q1, measurevar="value", groupvars=c("condition","dx"),na.rm=T)
run1_q1_se$dx<- as.character(run1_q1_se$dx)
run1_q1_se$dx[ run1_q1_se$dx=="0"]<- "1_NC"
run1_q1_se$dx[ run1_q1_se$dx=="1"]<- "2_CR"
run1_q1_se$dx[ run1_q1_se$dx=="2"]<- "3_P"

#remove any subjects who don't have aversive or neutral coding
run1_q1_se<- run1_q1_se[! is.na(run1_q1_se$condition),]

#print out a bar graph with aversive/neutral condition on the x axis, mean rating on the y axis and also by diagnosis
ggplot(data=run1_q1_se, aes(x=condition, y=value, fill=dx)) +
  geom_bar(stat="identity", position=position_dodge()) +
  geom_bar(colour="black", position=position_dodge(), show_guide=FALSE, stat="identity") +
  geom_errorbar(aes(ymin=value-se, ymax=value+se), width=.2, position=position_dodge(.9)) +
  xlab("Condition") +
  ylab("mean rating") +
  ggtitle("Run1: How friendly or unfriendly does this person seem?") +
  scale_fill_manual(name="Diagnosis",breaks=c("1_NC", "2_CR", "3_P"),labels=c("Normal Control","Clinical Risk","Patient"),values=c("green","blue","red"))

#####Question 2

#for post question 2 transform the data and rename the questions to which actor/face they refer to
run1_q2<- melt(post,id.vars=c("Subject","Timepoint"),measure.vars=c("Answer.quest2","Answer.quest6"))
run1_q2$variable<- as.character(run1_q2$variable)
run1_q2$variable[ run1_q2$variable=="Answer.quest2"]<- "face_1077"
run1_q2$variable[ run1_q2$variable=="Answer.quest6"]<- "face_1086"

#merge in condition (aversive or neutral) and diagnosis 
run1_q2 <- merge(run1_q2, condition[c("Subject","variable","value")], by=c("Subject","variable"),sort=FALSE)
colnames(run1_q2)[4:5]<- c("value","condition")
run1_q2$dx<- dx$dx[ match(run1_q2$Subject,dx$scanid)]
#Optional to include or exclude subjects without a diagnosis (those that weren't in the dx dataset because of exclusions)
run1_q2<- run1_q2[! is.na(run1_q2$dx),]

#create a table which gets mean, se, sd of rating for the question split by diagnosis, and rename the numerical diagnostic codings into letters
run1_q2_se<- summarySE(run1_q2, measurevar="value", groupvars=c("condition","dx"),na.rm=T)
run1_q2_se$dx<- as.character(run1_q2_se$dx)
run1_q2_se$dx[ run1_q2_se$dx=="0"]<- "1_NC"
run1_q2_se$dx[ run1_q2_se$dx=="1"]<- "2_CR"
run1_q2_se$dx[ run1_q2_se$dx=="2"]<- "3_P"

#remove any subjects who don't have aversive or neutral coding
run1_q2_se<- run1_q2_se[! is.na(run1_q2_se$condition),]

#print out a bar graph with aversive/neutral condition on the x axis, mean rating on the y axis and also by diagnosis
ggplot(data=run1_q2_se, aes(x=condition, y=value, fill=dx)) +
  geom_bar(stat="identity", position=position_dodge()) +
  geom_bar(colour="black", position=position_dodge(), show_guide=FALSE, stat="identity") +
  geom_errorbar(aes(ymin=value-se, ymax=value+se), width=.2, position=position_dodge(.9)) +
  xlab("Condition") +
  ylab("mean rating") +
  ggtitle("Run1: How positive or negative is the\n emotional expression on this persons face?") +
  scale_fill_manual(name="Diagnosis",breaks=c("1_NC", "2_CR", "3_P"),labels=c("Normal Control","Clinical Risk","Patient"),values=c("green","blue","red"))

#####Question 3

#for post question 3 transform the data and rename the questions to which actor/face they refer to
run1_q3<- melt(post,id.vars=c("Subject","Timepoint"),measure.vars=c("Answer.quest3","Answer.quest7"))
run1_q3$variable<- as.character(run1_q3$variable)
run1_q3$variable[ run1_q3$variable=="Answer.quest3"]<- "face_1077"
run1_q3$variable[ run1_q3$variable=="Answer.quest7"]<- "face_1086"

#merge in condition (aversive or neutral) and diagnosis 
run1_q3 <- merge(run1_q3, condition[c("Subject","variable","value")], by=c("Subject","variable"),sort=FALSE)
colnames(run1_q3)[4:5]<- c("value","condition")
run1_q3$dx<- dx$dx[ match(run1_q3$Subject,dx$scanid)]
#Optional to include or exclude subjects without a diagnosis (those that weren't in the dx dataset because of exclusions)
run1_q3<- run1_q3[! is.na(run1_q3$dx),]

#create a table which gets mean, se, sd of rating for the question split by diagnosis, and rename the numerical diagnostic codings into letters
run1_q3_se<- summarySE(run1_q3, measurevar="value", groupvars=c("condition","dx"),na.rm=T)
run1_q3_se$dx<- as.character(run1_q3_se$dx)
run1_q3_se$dx[ run1_q3_se$dx=="0"]<- "1_NC"
run1_q3_se$dx[ run1_q3_se$dx=="1"]<- "2_CR"
run1_q3_se$dx[ run1_q3_se$dx=="2"]<- "3_P"

#remove any subjects who don't have aversive or neutral coding
run1_q3_se<- run1_q3_se[! is.na(run1_q3_se$condition),]

#print out a bar graph with aversive/neutral condition on the x axis, mean rating on the y axis and also by diagnosis
ggplot(data=run1_q3_se, aes(x=condition, y=value, fill=dx)) +
  geom_bar(stat="identity", position=position_dodge()) +
  geom_bar(colour="black", position=position_dodge(), show_guide=FALSE, stat="identity") +
  geom_errorbar(aes(ymin=value-se, ymax=value+se), width=.2, position=position_dodge(.9)) +
  xlab("Condition") +
  ylab("mean rating") +
  ylim(-35,25) +
  ggtitle("Run1: How positive or negative do you\n feel when you look at this persons face?") +
  scale_fill_manual(name="Diagnosis",breaks=c("1_NC", "2_CR", "3_P"),labels=c("Normal Control","Clinical Risk","Patient"),values=c("green","blue","red"))

#get the same stuff but aversive - neutral for icosr_2017 poster graphs
run1_q3_av<- run1_q3[run1_q3$condition=="aversive",]
run1_q3_av<- run1_q3_av[! is.na(run1_q3_av$Subject),]
run1_q3_neu<- run1_q3[run1_q3$condition=="neutral",]
run1_q3_neu<- run1_q3_neu[! is.na(run1_q3_neu$Subject),]

#merge these aversive and neutral datsets together by subject so can subtract the pre - post diff
run1_q3_av_neu<- merge(run1_q3_av,run1_q3_neu,by=c("Subject","dx"))
run1_q3_av_neu$av_neu<- run1_q3_av_neu$value.x-run1_q3_av_neu$value.y


#create a table with mean, sd, and se for the answer to aversive face - neutral face for this question split by diagonsis
run1_q3_av_neu_se<- summarySE(run1_q3_av_neu,measurevar="av_neu",groupvar=c("dx"),na.rm=T)
run1_q3_av_neu_se$dx<- as.character(run1_q3_av_neu_se$dx)

#print out bar graph with diagnosis on the x axis, the aversive - neutral answer on the y axis
ggplot(data=run1_q3_av_neu_se, aes(x=dx, y=av_neu, fill=dx)) +
  geom_bar(stat="identity", position=position_dodge()) +
  geom_bar(colour="black", position=position_dodge(), show_guide=FALSE, stat="identity") +
  geom_errorbar(aes(ymin=av_neu-se, ymax=av_neu+se), width=.2, position=position_dodge(.9)) +
  xlab("Group") +
  ylab("mean delta rating") +
  #ylim(-35,25) +
  ggtitle("Run1 Aversive - Neutral: How positive or negative do you\n feel when you look at this persons face?") +
  scale_fill_manual(name="Diagnosis",breaks=c("1_NC", "2_CR", "3_P"),labels=c("Normal Control","Clinical Risk","Patient"),values=c("green","blue","red"))

#run an anova testing differences in group in cspu (aversive) answers for this question 
run1_q3_cs_u_aov<- aov(value.x~ dx,data=run1_q3_av_neu)
summary(run1_q3_cs_u_aov)

#do pairwise t-tests testing differences in group in cspu (aversive) answers for this question
hc_sz_r1_q3_cspu<- t.test(run1_q3_av_neu$value.x[run1_q3_av_neu$dx=="0"],run1_q3_av_neu$value.x[run1_q3_av_neu$dx=="2"],paired=FALSE,na.rm=T)
cr_sz_r1_q3_cspu<- t.test(run1_q3_av_neu$value.x[run1_q3_av_neu$dx=="1"],run1_q3_av_neu$value.x[run1_q3_av_neu$dx=="2"],paired=FALSE,na.rm=T)
hc_cr_r1_q3_cspu<- t.test(run1_q3_av_neu$value.x[run1_q3_av_neu$dx=="0"],run1_q3_av_neu$value.x[run1_q3_av_neu$dx=="1"],paired=FALSE,na.rm=T)

#run an anova testing differences in group in csm (neutral) answers for this question 
run1_q3_cs_m_aov<- aov(value.y~ dx,data=run1_q3_av_neu)
summary(run1_q3_cs_m_aov)

#do pairwise t-tests testing differences in group in csm (neutral) answers for this question
hc_sz_r1_q3_csm<- t.test(run1_q3_av_neu$value.y[run1_q3_av_neu$dx=="0"],run1_q3_av_neu$value.y[run1_q3_av_neu$dx=="2"],paired=FALSE,na.rm=T)
cr_sz_r1_q3_csm<- t.test(run1_q3_av_neu$value.y[run1_q3_av_neu$dx=="1"],run1_q3_av_neu$value.y[run1_q3_av_neu$dx=="2"],paired=FALSE,na.rm=T)
hc_cr_r1_q3_csm<- t.test(run1_q3_av_neu$value.y[run1_q3_av_neu$dx=="0"],run1_q3_av_neu$value.y[run1_q3_av_neu$dx=="1"],paired=FALSE,na.rm=T)




#####Question 4

#for post question 4 transform the data and rename the questions to which actor/face they refer to
run1_q4<- melt(post,id.vars=c("Subject","Timepoint"),measure.vars=c("Answer.quest4","Answer.quest8"))
run1_q4$variable<- as.character(run1_q4$variable)
run1_q4$variable[ run1_q4$variable=="Answer.quest4"]<- "face_1077"
run1_q4$variable[ run1_q4$variable=="Answer.quest8"]<- "face_1086"

#merge in condition (aversive or neutral) and diagnosis 
run1_q4 <- merge(run1_q4, condition[c("Subject","variable","value")], by=c("Subject","variable"),sort=FALSE)
colnames(run1_q4)[4:5]<- c("value","condition")
run1_q4$dx<- dx$dx[ match(run1_q4$Subject,dx$scanid)]
#Optional to include or exclude subjects without a diagnosis (those that weren't in the dx dataset because of exclusions)
run1_q4<- run1_q4[! is.na(run1_q4$dx),]

#create a table which gets mean, se, sd of rating for the question split by diagnosis, and rename the numerical diagnostic codings into letters
run1_q4_se<- summarySE(run1_q4, measurevar="value", groupvars=c("condition","dx"),na.rm=T)
run1_q4_se$dx<- as.character(run1_q4_se$dx)
run1_q4_se$dx[ run1_q4_se$dx=="0"]<- "1_NC"
run1_q4_se$dx[ run1_q4_se$dx=="1"]<- "2_CR"
run1_q4_se$dx[ run1_q4_se$dx=="2"]<- "3_P"

#remove any subjects who don't have aversive or neutral coding
run1_q4_se<- run1_q4_se[! is.na(run1_q4_se$condition),]

#print out a bar graph with aversive/neutral condition on the x axis, mean rating on the y axis and also by diagnosis
ggplot(data=run1_q4_se, aes(x=condition, y=value, fill=dx)) +
  geom_bar(stat="identity", position=position_dodge()) +
  geom_bar(colour="black", position=position_dodge(), show_guide=FALSE, stat="identity") +
  geom_errorbar(aes(ymin=value-se, ymax=value+se), width=.2, position=position_dodge(.9)) +
  xlab("Condition") +
  ylab("mean rating") +
  ggtitle("Run1: How strong are the emotions\n you feel when you see this person?") +
  scale_fill_manual(name="Diagnosis",breaks=c("1_NC", "2_CR", "3_P"),labels=c("Normal Control","Clinical Risk","Patient"),values=c("green","blue","red"))

######Question 5 accuracy

#for post question 5 transform the data and rename the answers to which actor/face they refer to (-140 is face 1077; 140 is face 1086)
run1_q5<- melt(post,id.vars=c("Subject","Timepoint"),measure.vars="Answer.quest9")
run1_q5$variable<- as.character(run1_q5$variable)
run1_q5$value[ run1_q5$value=="-140"]<- "face_1077"
run1_q5$value[ run1_q5$value=="140"]<- "face_1086"

#create a variable which gets which face was aversive for that subject
aversive_condition<- condition[ condition$value=="aversive",]
aversive_condition$variable<- as.character(aversive_condition$variable)

#merge in condition (aversive or neutral)
run1_q5$aversive_face<- aversive_condition$variable[ match(run1_q5$Subject,aversive_condition$Subject)]
run1_q5$correct<- NA
run1_q5<- run1_q5[! is.na(run1_q5$aversive_face),]
for (i in 1:(nrow(run1_q5))){
if(run1_q5$value[i]==run1_q5$aversive_face[i]){
  run1_q5$correct[i]<- 100
}
if(! run1_q5$value[i]==run1_q5$aversive_face[i]){
  run1_q5$correct[i]<- 0
}
}

#merge in diagnosis and subset the data to desired columns
run1_q5$dx<- dx$dx[ match(run1_q5$Subject,dx$scanid)]

run1_q5<- run1_q5[,c(1,4,6,7)]

#run an anova to test if there is a difference in accuracy between diagnoses
run1_q5_aov<- aov(correct~ dx,data=run1_q5)
summary(run1_q5_aov)

#run t-tests to test pairwise differences in accuracy between diagnoses
hc_sz_r1_q5<- t.test(run1_q5$correct[run1_q5$dx=="0"],run1_q5$correct[run1_q5$dx=="2"],paired=FALSE,na.rm=T)
cr_sz_r1_q5<- t.test(run1_q5$correct[run1_q5$dx=="1"],run1_q5$correct[run1_q5$dx=="2"],paired=FALSE,na.rm=T)
hc_cr_r1_q5<- t.test(run1_q5$correct[run1_q5$dx=="0"],run1_q5$correct[run1_q5$dx=="1"],paired=FALSE,na.rm=T)


#transform diagnosis into a character type rather than factor
run1_q5$dx<- as.character(run1_q5$dx)

#Optional to include or exclude subjects without a diagnosis (those that weren't in the dx dataset because of exclusions)
run1_q5<- run1_q5[! is.na(run1_q5$dx),]

#create a table which gets mean, se, sd of accuracy and for the question split by diagnosis, do the same for accuracy split by diagnosis and actor/face, and rename the numerical diagnostic codings into letters
run1_q5_se<- summarySE(run1_q5,measurevar="correct",groupvars="dx",na.rm=T)
run1_q5_face_se<- summarySE(run1_q5,measurevar="correct",groupvars=c("dx","value"),na.rm=T)

run1_q5_se$dx[ run1_q5_se$dx=="0"]<- "1_NC"
run1_q5_se$dx[ run1_q5_se$dx=="1"]<- "2_CR"
run1_q5_se$dx[ run1_q5_se$dx=="2"]<- "3_P"

run1_q5_face_se$dx[ run1_q5_face_se$dx=="0"]<- "1_NC"
run1_q5_face_se$dx[ run1_q5_face_se$dx=="1"]<- "2_CR"
run1_q5_face_se$dx[ run1_q5_face_se$dx=="2"]<- "3_P"

#print out bar plots of accuracy, with diagnosis on the x axis and accuracy on the y axis
ggplot(data=run1_q5_se, aes(x=dx, y=correct, fill=dx)) +
  geom_bar(stat="identity", position=position_dodge()) +
  geom_bar(colour="black", position=position_dodge(), show_guide=FALSE, stat="identity") +
  geom_errorbar(aes(ymin=correct-se, ymax=correct+se), width=.2, position=position_dodge(.9)) +
  xlab("Diagnosis") +
  scale_x_discrete(breaks=c("1_NC", "2_CR", "3_P"), labels=c("Normal Control","Clinical Risk","Patient")) +
  ylab("mean percent correct") +
  ggtitle("Run1: Which face is paired with the\n scream more? Accuracy by Diagnosis") +
  scale_fill_manual(name="Diagnosis",breaks=c("1_NC", "2_CR", "3_P"),labels=c("Normal Control","Clinical Risk","Patient"),values=c("green","blue","red"))

#print out bar graph of accuracy with diagnosis on the x axis, accuracy on the y axis and also by actor/face
ggplot(data=run1_q5_face_se, aes(x=dx, y=correct, fill=value)) +
  geom_bar(stat="identity", position=position_dodge()) +
  geom_bar(colour="black", position=position_dodge(), show_guide=FALSE, stat="identity") +
  geom_errorbar(aes(ymin=correct-se, ymax=correct+se), width=.2, position=position_dodge(.9)) +
  xlab("Diagnosis") +
  scale_x_discrete(breaks=c("1_NC", "2_CR", "3_P"), labels=c("Normal Control","Clinical Risk","Patient")) +
  ylab("mean percent correct") +
  ggtitle("Run1: Which face is paired with the\n scream more? Accuracy by Diagnosis and Face") 


######Question 6 correctness vs confidence

#for post question 6 transform the data
run1_q6<- melt(post,id.vars=c("Subject","Timepoint"),measure.vars="Answer.quest10")

#merge in accuracy from question 5 and diagnosis
run1_q6$correct<- run1_q5$correct[ match(run1_q6$Subject,run1_q5$Subject)]
run1_q6$dx<- dx$dx[ match(run1_q6$Subject,dx$scanid)]

#Optional to include or exclude subjects without a diagnosis (those that weren't in the dx dataset because of exclusions)
run1_q6<- run1_q6[! is.na(run1_q6$dx),]

#create a table which gets mean, se, sd of confidence and for the question split by diagnosis, do the same for confidence split by diagnosis and accuracy
run1_q6_se<- summarySE(run1_q6,measurevar="value",groupvars="dx",na.rm=T)
run1_q6_correct_se<- summarySE(run1_q6,measurevar="value",groupvars=c("dx","correct"),na.rm=T)

#remove any rows that don't have accuracy data
run1_q6_correct_se<- run1_q6_correct_se[! is.na(run1_q6_correct_se$correct),]

#rename the numerical diagnostic codings into letters
run1_q6_se$dx<- as.character(run1_q6_se$dx)
run1_q6_se$dx[ run1_q6_se$dx=="0"]<- "1_NC"
run1_q6_se$dx[ run1_q6_se$dx=="1"]<- "2_CR"
run1_q6_se$dx[ run1_q6_se$dx=="2"]<- "3_P"

run1_q6_correct_se$dx<- as.character(run1_q6_correct_se$dx)
run1_q6_correct_se$dx[ run1_q6_correct_se$dx=="NC"]<- "1_NC"
run1_q6_correct_se$dx[ run1_q6_correct_se$dx=="CR"]<- "2_CR"
run1_q6_correct_se$dx[ run1_q6_correct_se$dx=="P"]<- "3_P"

#convert the correct variable (accuracy) to a character
run1_q6_correct_se$correct<- as.character(run1_q6_correct_se$correct)

#print out bar plots of confidence, with diagnosis on the x axis and confidence on the y axis
ggplot(data=run1_q6_se, aes(x=dx, y=value, fill=dx)) +
  geom_bar(stat="identity", position=position_dodge()) +
  geom_bar(colour="black", position=position_dodge(), show_guide=FALSE, stat="identity") +
  geom_errorbar(aes(ymin=value-se, ymax=value+se), width=.2, position=position_dodge(.9)) +
  xlab("Diagnosis") +
  scale_x_discrete(breaks=c("1_NC", "2_CR", "3_P"), labels=c("Normal Control","Clinical Risk","Patient")) +
  ylab("mean confidence rating") +
  ggtitle("Run1: How confident are you that this face\n was presented with a scream more often?") +
  scale_fill_manual(name="Diagnosis",breaks=c("1_NC", "2_CR", "3_P"),labels=c("Normal Control","Clinical Risk","Patient"),values=c("green","blue","red"))

#print out bar graph of confidence with accuracy on the x axis, confidence on the y axis and also by diagnosis
ggplot(data=run1_q6_correct_se, aes(x=correct, y=value, fill=dx)) +
  geom_bar(stat="identity", position=position_dodge()) +
  geom_bar(colour="black", position=position_dodge(), show_guide=FALSE, stat="identity") +
  geom_errorbar(aes(ymin=value-se, ymax=value+se), width=.2, position=position_dodge(.9)) +
  xlab("Correctness") +
  scale_x_discrete(breaks=c(0,100), labels=c("Incorrect","Correct")) +
  ylab("mean confidence rating") +
  ggtitle("Run1: Confidence and Correctness by Diagnosis") +
  scale_fill_manual(name="Diagnosis",breaks=c("1_NC", "2_CR", "3_P"),labels=c("Normal Control","Clinical Risk","Patient"),values=c("green","blue","red"))


####################PRE-RUN1########################


###########Question 1

#for pre and post question 1 merge the data together from pre and post
pre_run1_q1<- merge(pre_q1,run1_q1,by=c("Subject","variable","dx"))

#calculate the absolute difference between the pre and post question answer (pre - post)
pre_run1_q1$diff<- abs(pre_run1_q1$value.x-pre_run1_q1$value.y)

#figure out the direction, as in if pre to post the rating became more negative, or positive and create a column called direction which gets either -1 (more negative from pre to post), 1 (more positive pre to post), or 0 if there was no change
pre_run1_q1$direction<- NA
pre_run1_q1<- pre_run1_q1[! is.na(pre_run1_q1$value.x),]
for (i in 1:nrow(pre_run1_q1)){
if(pre_run1_q1$value.x[i]>pre_run1_q1$value.y[i]){
  pre_run1_q1$direction[i]<- -1
}                                              
if(pre_run1_q1$value.x[i]<pre_run1_q1$value.y[i]){
  pre_run1_q1$direction[i]<- 1
}
if(pre_run1_q1$diff[i]==0){
  pre_run1_q1$direction[i]<- 0
}
}

#calculate the subtraction by multiplying the absolute answer difference by the direction and then delete those columns
pre_run1_q1$pre_post<- pre_run1_q1$diff*pre_run1_q1$direction
pre_run1_q1<- pre_run1_q1[,c(1,2,3,8,11)]

#create a summary table (mean, se, sd) which gets the difference for each diagnosis and condition (aversive or neutral)
pre_run1_q1_se<- summarySE(pre_run1_q1,measurevar="pre_post",groupvar=c("dx","condition"),na.rm=T)

#change the numerical diagnosis values to words for diagnoses and remove any subjects without aversive/neutral information
pre_run1_q1_se$dx<- as.character(pre_run1_q1_se$dx)
pre_run1_q1_se$dx[ pre_run1_q1_se$dx=="0"]<- "1_NC"
pre_run1_q1_se$dx[ pre_run1_q1_se$dx=="1"]<- "2_CR"
pre_run1_q1_se$dx[ pre_run1_q1_se$dx=="2"]<- "3_P"
pre_run1_q1_se<- pre_run1_q1_se[! is.na(pre_run1_q1_se$condition),]

#print out a graph with condition (aversive/neutral) on the x axis, pre - post rating on the y axis and split by diagnosis
ggplot(data=pre_run1_q1_se, aes(x=condition, y=pre_post, fill=dx)) +
  geom_bar(stat="identity", position=position_dodge()) +
  geom_bar(colour="black", position=position_dodge(), show_guide=FALSE, stat="identity") +
  geom_errorbar(aes(ymin=pre_post-se, ymax=pre_post+se), width=.2, position=position_dodge(.9)) +
  xlab("Condition") +
  ylab("mean delta rating") +
  ggtitle("Run1-Pre: How friendly or unfriendly\n does this person seem?") +
  scale_fill_manual(name="Diagnosis",breaks=c("1_NC", "2_CR", "3_P"),labels=c("Normal Control","Clinical Risk","Patient"),values=c("green","blue","red"))


###########Question 2

#for pre and post question 2 merge the data together from pre and post
pre_run1_q2<- merge(pre_q2,run1_q2,by=c("Subject","variable","dx"))

#calculate the absolute difference between the pre and post question answer (pre - post)
pre_run1_q2$diff<- abs(pre_run1_q2$value.x-pre_run1_q2$value.y)

#figure out the direction, as in if pre to post the rating became more negative, or positive and create a column called direction which gets either -1 (more negative from pre to post), 1 (more positive pre to post), or 0 if there was no change
pre_run1_q2$direction<- NA
pre_run1_q2<- pre_run1_q2[! is.na(pre_run1_q2$value.x),]
for (i in 1:nrow(pre_run1_q2)){
  if(pre_run1_q2$value.x[i]>pre_run1_q2$value.y[i]){
    pre_run1_q2$direction[i]<- -1
  }
  if(pre_run1_q2$value.x[i]<pre_run1_q2$value.y[i]){
    pre_run1_q2$direction[i]<- 1
  }
  if(pre_run1_q2$diff[i]==0){
    pre_run1_q2$direction[i]<- 0
  }
}

#calculate the subtraction by multiplying the absolute answer difference by the direction and then delete those columns
pre_run1_q2$pre_post<- pre_run1_q2$diff*pre_run1_q2$direction
pre_run1_q2<- pre_run1_q2[,c(1,2,3,8,11)]

#create a table which gets the pre - post rating mean, se, and sd by condition (aversive/neutral) and also diagnosis
pre_run1_q2_se<- summarySE(pre_run1_q2,measurevar="pre_post",groupvar=c("dx","condition"),na.rm=T)
pre_run1_q2_se$dx<- as.character(pre_run1_q2_se$dx)

#change the numerical diagnosis values to words for diagnoses and remove any subjects without aversive/neutral information
pre_run1_q2_se$dx[ pre_run1_q2_se$dx=="0"]<- "1_NC"
pre_run1_q2_se$dx[ pre_run1_q2_se$dx=="1"]<- "2_CR"
pre_run1_q2_se$dx[ pre_run1_q2_se$dx=="2"]<- "3_P"
pre_run1_q2_se<- pre_run1_q2_se[! is.na(pre_run1_q2_se$condition),]

#print out a graph with condition (aversive/neutral) on the x axis, pre - post rating on the y axis and split by diagnosis
ggplot(data=pre_run1_q2_se, aes(x=condition, y=pre_post, fill=dx)) +
  geom_bar(stat="identity", position=position_dodge()) +
  geom_bar(colour="black", position=position_dodge(), show_guide=FALSE, stat="identity") +
  geom_errorbar(aes(ymin=pre_post-se, ymax=pre_post+se), width=.2, position=position_dodge(.9)) +
  xlab("Condition") +
  ylab("mean delta rating") +
  ggtitle("Run1-Pre: How positive or negative is the\n emotional expression on this persons face?") +
  scale_fill_manual(name="Diagnosis",breaks=c("1_NC", "2_CR", "3_P"),labels=c("Normal Control","Clinical Risk","Patient"),values=c("green","blue","red"))


###########Question 3

#for pre and post question 3 merge the data together from pre and post
pre_run1_q3<- merge(pre_q3,run1_q3,by=c("Subject","variable","dx"))

#calculate the absolute difference between the pre and post question answer (pre - post)
pre_run1_q3$diff<- abs(pre_run1_q3$value.x-pre_run1_q3$value.y)

#figure out the direction, as in if pre to post the rating became more negative, or positive and create a column called direction which gets either -1 (more negative from pre to post), 1 (more positive pre to post), or 0 if there was no change
pre_run1_q3$direction<- NA
pre_run1_q3<- pre_run1_q3[! is.na(pre_run1_q3$value.x),]
for (i in 1:nrow(pre_run1_q3)){
  if(pre_run1_q3$value.x[i]>pre_run1_q3$value.y[i]){
    pre_run1_q3$direction[i]<- -1
  }
  if(pre_run1_q3$value.x[i]<pre_run1_q3$value.y[i]){
    pre_run1_q3$direction[i]<- 1
  }
  if(pre_run1_q3$diff[i]==0){
    pre_run1_q3$direction[i]<- 0
  }
}

#calculate the subtraction by multiplying the absolute answer difference by the direction and then delete those columns
pre_run1_q3$pre_post<- pre_run1_q3$diff*pre_run1_q3$direction
pre_run1_q3<- pre_run1_q3[,c(1,2,3,8,11)]

#create a summary table (mean, se, sd) which gets the difference for each diagnosis and condition (aversive or neutral)
pre_run1_q3_se<- summarySE(pre_run1_q3,measurevar="pre_post",groupvar=c("dx","condition"),na.rm=T)

#change the numerical diagnosis values to words for diagnoses and remove any subjects without aversive/neutral information
pre_run1_q3_se$dx<- as.character(pre_run1_q3_se$dx)
pre_run1_q3_se$dx[ pre_run1_q3_se$dx=="0"]<- "1_NC"
pre_run1_q3_se$dx[ pre_run1_q3_se$dx=="1"]<- "2_CR"
pre_run1_q3_se$dx[ pre_run1_q3_se$dx=="2"]<- "3_P"
pre_run1_q3_se<- pre_run1_q3_se[! is.na(pre_run1_q3_se$condition),]


#pull in aversive or neutral condition for each subject and get rid of any row without a subject id 
pre_run1_q3_av<- pre_run1_q3[pre_run1_q3$condition=="aversive",]
pre_run1_q3_av<- pre_run1_q3_av[! is.na(pre_run1_q3_av$Subject),]
pre_run1_q3_neu<- pre_run1_q3[pre_run1_q3$condition=="neutral",]
pre_run1_q3_neu<- pre_run1_q3_neu[! is.na(pre_run1_q3_neu$Subject),]

#merge these aversive and neutral datsets together by subject so can subtract the pre - post diff
pre_run1_q3_av_neu<- merge(pre_run1_q3_av,pre_run1_q3_neu,by=c("Subject","dx"))
pre_run1_q3_av_neu$av_neu<- pre_run1_q3_av_neu$pre_post.x-pre_run1_q3_av_neu$pre_post.y

#print out a graph with condition (aversive/neutral) on the x axis, pre - post rating on the y axis and split by diagnosis
ggplot(data=pre_run1_q3_se, aes(x=condition, y=pre_post, fill=dx)) +
  geom_bar(stat="identity", position=position_dodge()) +
  geom_bar(colour="black", position=position_dodge(), show_guide=FALSE, stat="identity") +
  geom_errorbar(aes(ymin=pre_post-se, ymax=pre_post+se), width=.2, position=position_dodge(.9)) +
  xlab("Condition") +
  ylab("mean delta rating") +
  ylim(-35,25) +
  ggtitle("Run1-Pre: How positive or negative do you\n feel when you look at this persons face?") +
  scale_fill_manual(name="Diagnosis",breaks=c("1_NC", "2_CR", "3_P"),labels=c("Normal Control","Clinical Risk","Patient"),values=c("green","blue","red"))

#create a summary table (mean, se, sd) which gets the pre run1 difference for each diagnosis and condition (aversive or neutral)
pre_run1_q3_av_neu_se<- summarySE(pre_run1_q3_av_neu,measurevar="av_neu",groupvar=c("dx"),na.rm=T)
pre_run1_q3_av_neu_se$dx<- as.character(pre_run1_q3_av_neu_se$dx)

#print out a graph with diagnosis on the x axis, pre - post aversive - neutral rating on the y axis and split by diagnosis
ggplot(data=pre_run1_q3_av_neu_se, aes(x=dx, y=av_neu, fill=dx)) +
  geom_bar(stat="identity", position=position_dodge()) +
  geom_bar(colour="black", position=position_dodge(), show_guide=FALSE, stat="identity") +
  geom_errorbar(aes(ymin=av_neu-se, ymax=av_neu+se), width=.2, position=position_dodge(.9)) +
  xlab("Group") +
  ylab("mean delta rating") +
  ylim(-35,25) +
  ggtitle("Run1-Pre, Aversive - Neutral: How positive or negative do you\n feel when you look at this persons face?") +
  scale_fill_manual(name="Diagnosis",breaks=c("1_NC", "2_CR", "3_P"),labels=c("Normal Control","Clinical Risk","Patient"),values=c("green","blue","red"))

#run an anova which tests differences in pre post differences for the cspu (aversive) image between diagnoses
run1_m_pre_q3_cs_u_aov<- aov(pre_post.x~ dx,data=pre_run1_q3_av_neu)
summary(run1_m_pre_q3_cs_u_aov)

#do pairwise t-tests to test differences in pre post differences for the cspu (aversive) image between diagnoses
hc_sz_r1_pre_q3_cspu<- t.test(pre_run1_q3_av_neu$pre_post.x[pre_run1_q3_av_neu$dx=="0"],pre_run1_q3_av_neu$pre_post.x[pre_run1_q3_av_neu$dx=="2"],paired=FALSE,na.rm=T)
cr_sz_r1_pre_q3_cspu<- t.test(pre_run1_q3_av_neu$pre_post.x[pre_run1_q3_av_neu$dx=="1"],pre_run1_q3_av_neu$pre_post.x[pre_run1_q3_av_neu$dx=="2"],paired=FALSE,na.rm=T)
hc_cr_r1_pre_q3_cspu<- t.test(pre_run1_q3_av_neu$pre_post.x[pre_run1_q3_av_neu$dx=="0"],pre_run1_q3_av_neu$pre_post.x[pre_run1_q3_av_neu$dx=="1"],paired=FALSE,na.rm=T)

#run an anova which tests differences in pre post differences for the csm (neutral) image between diagnoses
run1_m_pre_q3_cs_m_aov<- aov(pre_post.y~ dx,data=pre_run1_q3_av_neu)
summary(run1_m_pre_q3_cs_m_aov)

#do pairwise t-tests to test differences in pre post differences for the csm (neutral) image between diagnoses
hc_sz_r1_pre_q3_csm<- t.test(pre_run1_q3_av_neu$pre_post.y[pre_run1_q3_av_neu$dx=="0"],pre_run1_q3_av_neu$pre_post.y[pre_run1_q3_av_neu$dx=="2"],paired=FALSE,na.rm=T)
cr_sz_r1_pre_q3_csm<- t.test(pre_run1_q3_av_neu$pre_post.y[pre_run1_q3_av_neu$dx=="1"],pre_run1_q3_av_neu$pre_post.y[pre_run1_q3_av_neu$dx=="2"],paired=FALSE,na.rm=T)
hc_cr_r1_pre_q3_csm<- t.test(pre_run1_q3_av_neu$pre_post.y[pre_run1_q3_av_neu$dx=="0"],pre_run1_q3_av_neu$pre_post.y[pre_run1_q3_av_neu$dx=="1"],paired=FALSE,na.rm=T)

#run an anova which tests differences in pre post differences for aversive - neutral between diagnoses
run1_m_pre_q3_cspu_csm_aov<- aov(av_neu~ dx,data=pre_run1_q3_av_neu)
summary(run1_m_pre_q3_cspu_csm_aov)

#do pairwise t-tests to test differences in pre post differences for aversive - neutral between diagnoses
hc_sz_r1_pre_q3_cspu_csm<- t.test(pre_run1_q3_av_neu$av_neu[pre_run1_q3_av_neu$dx=="0"],pre_run1_q3_av_neu$av_neu[pre_run1_q3_av_neu$dx=="2"],paired=FALSE,na.rm=T)
cr_sz_r1_pre_q3_cspu_csm<- t.test(pre_run1_q3_av_neu$av_neu[pre_run1_q3_av_neu$dx=="1"],pre_run1_q3_av_neu$av_neu[pre_run1_q3_av_neu$dx=="2"],paired=FALSE,na.rm=T)
hc_cr_r1_pre_q3_cspu_csm<- t.test(pre_run1_q3_av_neu$av_neu[pre_run1_q3_av_neu$dx=="0"],pre_run1_q3_av_neu$av_neu[pre_run1_q3_av_neu$dx=="1"],paired=FALSE,na.rm=T)





###########Question 4

#for pre and post question 4 merge the data together from pre and post
pre_run1_q4<- merge(pre_q4,run1_q4,by=c("Subject","variable","dx"))

#calculate the absolute difference between the pre and post question answer (pre - post)
pre_run1_q4$diff<- abs(pre_run1_q4$value.y-pre_run1_q4$value.x)

#figure out the direction, as in if pre to post the rating became more negative, or positive and create a column called direction which gets either -1 (more negative from pre to post), 1 (more positive pre to post), or 0 if there was no change
pre_run1_q4$direction<- NA
pre_run1_q4<- pre_run1_q4[! is.na(pre_run1_q4$value.x),]
for (i in 1:nrow(pre_run1_q4)){
  if(pre_run1_q4$value.x[i]<pre_run1_q4$value.y[i]){
    pre_run1_q4$direction[i]<- -1
  }
  if(pre_run1_q4$value.x[i]>pre_run1_q4$value.y[i]){
    pre_run1_q4$direction[i]<- 1
  }
  if(pre_run1_q4$diff[i]==0){
    pre_run1_q4$direction[i]<- 0
  }
}

#calculate the subtraction by multiplying the absolute answer difference by the direction and then delete those columns
pre_run1_q4$pre_post<- pre_run1_q4$diff*pre_run1_q4$direction
pre_run1_q4<- pre_run1_q4[,c(1,2,3,8,11)]

#create a summary table (mean, se, sd) which gets the difference for each diagnosis and condition (aversive or neutral)
pre_run1_q4_se<- summarySE(pre_run1_q4,measurevar="pre_post",groupvar=c("dx","condition"),na.rm=T)

#change the numerical diagnosis values to words for diagnoses and remove any subjects without aversive/neutral information
pre_run1_q4_se$dx<- as.character(pre_run1_q4_se$dx)
pre_run1_q4_se$dx[ pre_run1_q4_se$dx=="0"]<- "1_NC"
pre_run1_q4_se$dx[ pre_run1_q4_se$dx=="1"]<- "2_CR"
pre_run1_q4_se$dx[ pre_run1_q4_se$dx=="2"]<- "3_P"
pre_run1_q4_se<- pre_run1_q4_se[! is.na(pre_run1_q4_se$condition),]

#print out a graph with condition (aversive/neutral) on the x axis, pre - post rating on the y axis and split by diagnosis
ggplot(data=pre_run1_q4_se, aes(x=condition, y=pre_post, fill=dx)) +
  geom_bar(stat="identity", position=position_dodge()) +
  geom_bar(colour="black", position=position_dodge(), show_guide=FALSE, stat="identity") +
  geom_errorbar(aes(ymin=pre_post-se, ymax=pre_post+se), width=.2, position=position_dodge(.9)) +
  xlab("Condition") +
  ylab("mean delta rating") +
  ggtitle("Run1-Pre: How strong are the emotions\n you feel when you see this person?") +
  scale_fill_manual(name="Diagnosis",breaks=c("1_NC", "2_CR", "3_P"),labels=c("Normal Control","Clinical Risk","Patient"),values=c("green","blue","red"))


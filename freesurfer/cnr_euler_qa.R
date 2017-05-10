
#This script takes the CNR and Euler number output from 
#/data/joy/BBL/projects/conteReproc2017/freesurfer/cnr_euler_number_calculation.sh
#for Freesurfer version 5.3 processing and calculates binary flags for the euler number and cnr subcategories

#load libraries
library(ggplot2)

############################################
###########DATA PREP########################

#read in data
output.dir<-commandArgs(TRUE)[1]
cnr_data<- read.csv(paste(output.dir,"/cnr/cnr_buckner.csv",sep=""))
euler_data<- read.csv(paste(output.dir,"/cnr/euler_number.csv",sep=""))

#merge the files together by datexscanid
data<- cnr_data
data$left_euler<- euler_data$left_euler[match(data$scanid,euler_data$scanid)]
data$right_euler<- euler_data$right_euler[match(data$scanid,euler_data$scanid)]

#################################################
###########BINARY EXCLUSION FLAGS################

#create a dataframe which will get the flags based on euler and cnr calculations
flags<- data

#get mean values for gray/csf cnr, gray/white cnr and euler numbers (average across hemispheres)
flags$mean_euler<-(flags$left_euler+flags$right_euler)/2
flags$mean_graycsf_cnr<- (flags$graycsflh+flags$graycsfrh)/2
flags$mean_graywhite_cnr<- (flags$graywhitelh+flags$graywhiterh)/2

#subset data frame to only IDs and averages
flags<- flags[,c(1,2,10:12)]

#create variables that get the standard deviation for cnr and euler number averages
graycsf_cutoff<- mean(flags$mean_graycsf_cnr-(2*sd(flags$mean_graycsf_cnr)))
graywhite_cutoff<- mean(flags$mean_graywhite_cnr-(2*sd(flags$mean_graywhite_cnr)))
euler_cutoff<- mean(flags$mean_euler-(2*sd(flags$mean_euler)))

#create a binary flag column (1=yes, 0=no) for average cnr and euler numbers (<2 SD =1, >2 SD=0)
flags$graycsf_flag<- NA
flags$graywhite_flag<- NA
flags$euler_flag<- NA

for (i in 1:nrow(flags)){
  if (flags$mean_graycsf_cnr[i]<=graycsf_cutoff){
flags$graycsf_flag[i]<- 1
} else if (flags$mean_graycsf_cnr[i]>graycsf_cutoff){
flags$graycsf_flag[i]<- 0 
}
if (flags$mean_graywhite_cnr[i]<=graywhite_cutoff){
  flags$graywhite_flag[i]<- 1
} else if (flags$mean_graywhite_cnr[i]>graywhite_cutoff){
  flags$graywhite_flag[i]<- 0 
}
if (flags$mean_euler[i]<=euler_cutoff){
  flags$euler_flag[i]<- 1
} else if (flags$mean_euler[i]>euler_cutoff){
  flags$euler_flag[i]<- 0 
}

} # for (i in 1:nrow(flags)){

#merge in number of outliers roi thickness from the auto qa
flags$noutliers.thickness.rois_outlier<- auto_qa$noutliers.thickness.rois_outlier[match(flags$scanid,auto_qa$scanid)]

#subset data frame to only IDs and flags
flags<- flags[,c(1,2,6:9)]

#create a total outliers column which gets the number of total outliers and a column which gets a binary flag 1=yes, 0=no
flags$total_outliers<- NA
x<- cbind(flags$graycsf_flag,flags$graywhite_flag,flags$euler_flag,flags$noutliers.thickness.rois_outlier)
flags$total_outliers<- apply(x,1,sum)
flags$flagged<- flags$total_outliers
flags$flagged[flags$flagged>0]<- 1

nsubj<- nrow(flags)

#write out flagged data
write.csv(flags,paste("/data/joy/BBL/studies/conte/subjectData/freesurfer/stats/cnr_euler_flags_n",nsubj,".csv",sep=""))

#################################################
###########EXCLUSIONS BY DIAGNOSIS###############

data2$flagged<- flags$flagged[match(data2$bblid,flags$bblid)]


table(data2$flagged,data2$dxpmr4)
table(data2$flagged,data2$age)

mean(data2$age[data2$flagged==1])


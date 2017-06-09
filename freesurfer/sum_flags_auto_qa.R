#MQ March 10, 2017

#This script takes the Flag Outliers csv and Euler number flags output from 
#/data/joy/BBL/projects/conteReproc2017/freesurfer/QA.sh
#for CONTE Freesurfer version 5.3 reprocessing and does the following:
### 1) merges the files into an aggregate qa file
### 2) calculates an overall "flagged" column which is a binary 1 (flagged) or 0 (not flagged) based on QA metrics
### 3) outputs a csv of images that are flagged

#load libraries
library(ggplot2)

##################
###### ARGS ######
##################
output.dir<-commandArgs(TRUE)[1]
auto_flag<-read.csv(commandArgs(TRUE)[2])
euler_flag<-read.csv(commandArgs(TRUE)[3])


##################
### DATA PREP ####
##################

#create flags datasheet which gets all the columns which should be included in the automatic QA flagging sum
#Note: this is excluding the columns of: meanthickness, totalarea, cnr,snr SubCortGayVol, CortexVol, CorticalWhiteMatterVol, noutliers.thickness.rois, noutliers.lat.thickness.rois, 
#and cnr_outlier (since we use the CNR values from the euler script)
flags<- auto_flag[,c(1:2,12:16,18:20)]

#merge in the euler number columns (gray/white cnr, gray/csf cnr, euler number)
flags$graycsf_flag<- euler_flag$graycsf_flag[match(flags$scanid,euler_flag$scanid)]
flags$graywhite_flag<- euler_flag$graywhite_flag[match(flags$scanid,euler_flag$scanid)]
flags$euler_flag<- euler_flag$euler_flag[match(flags$scanid,euler_flag$scanid)]


##################
### SUM FLAGS ####
##################


#create summary column that gets a sum of the flags in this csv
flags$total_outliers<- rowSums(flags[,3:13])

#create column that gets a binary 1 (flagged) or 0 (not flagged) if the subject is flagged or not
flags$fsFlag<- "NA"
flags$fsFlag[which(flags$total_outliers==0)]<- "0"
flags$fsFlag[which(flags$total_outliers>0)]<- "1"


##################
### OUTPUT DATA ##
##################

write.csv(flags, file.path(output.dir, "auto.qa.summary.flags.csv"), quote=FALSE, row.names=FALSE)
cat('wrote file to', file.path(output.dir, "auto.qa.summary.flags.csv"), '\n')


#this script extracts post ratings data for each subject in design 3 and outputs data to both subject specific and aggregate files 

#create a variable which gets today's date
date=`date +%Y-%m-%d`

#loop through each design 3 subject and
for i in `cat /data/joy/BBL/studies/conte/subjectData/design3FullSubjectList.txt`
do

#create a variable which gets the bblid and scanid and prints the scanid to the screen
bblid=`echo $i | cut -d "_" -f 1`
scanid=`echo $i | cut -d "_" -f 2`

echo "Processing subject......." $scanid

#find the file for the subject's ratings and faces
path=`ls -d /data/joy/BBL/studies/conte/processedData/$bblid/*$scanid/associated_files/ratings/post`
rating=`ls -d /data/joy/BBL/studies/conte/rawData/$bblid/*$scanid/associated_files/ratings/post/*$scanid-Faces_ratings_wheel*.log` 
faces=`ls -ltr -d /data/joy/BBL/studies/conte/rawData/$bblid/*$scanid/associated_files/ratings/post/*$scanid-fearConditioning_run1_wheel*.log | tail -1 | rev | cut -d " " -f1 | rev` 

#run the R script that parses the log file and gets the ratings responses for each subject
/import/monstrum2/Applications/R3.2.3/bin/R --file=/data/joy/BBL/projects/conteReproc2017/behaviroalRatings/post/run1_parse_logfiles_ratings_faces.R --slave --args "$rating" "$faces" "$scanid" "$path"

#create a file called run1_ratings_date.csv which gets the following column headers
echo "Subject,Timepoint,Answer.quest1,Face.quest1,Answer.quest2,Face.quest2,Answer.quest3,Face.quest3,Answer.quest4,Face.quest4,Answer.quest5,Face.quest5,Answer.quest6,Face.quest6,Answer.quest7,Face.quest7,Answer.quest8,Face.quest8,Answer.quest9,Face.quest9,Answer.quest10,Face.quest10" > /data/joy/BBL/studies/conte/subjectData/ratingsPost/run1_ratings_"$date".csv

#create a file called run1_faces_date.csv which gets the following column headers
echo "Subject,Timepoint,face_1077,face_1086" > /data/joy/BBL/studies/conte/subjectData/ratingsPost/run1_faces_"$date".csv

done

#for every individual's ratings and faces files, append to the aggregate csv file
for k in $( ls -d /data/joy/BBL/studies/conte/processedData/$bblid/*$scanid/associated_files/ratings/post/*_run1_ratings_data.csv ) ;do
tail -1 "$k" | cut -d "," -f 2-100 >> /data/joy/BBL/studies/conte/subjectData/ratingsPost/run1_ratings_"$date".csv
done

for k in $( ls -d /data/joy/BBL/studies/conte/processedData/$bblid/*$scanid/associated_files/ratings/post/*_run1_faces_data.csv ) ;do
tail -1 "$k" | cut -d "," -f 2-100 >> /data/joy/BBL/studies/conte/subjectData/ratingsPost/run1_faces_"$date".csv
done


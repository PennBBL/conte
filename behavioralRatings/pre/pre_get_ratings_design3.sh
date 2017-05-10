#this script extracts pre ratings data for each subject in design 3 and outputs data to both subject specific and aggregate files 

#create a variable which gets today's date
date=`date +%Y-%m-%d`

#loop through the design 3 subjects and
for i in `cat /data/joy/BBL/studies/conte/subjectData/design3FullSubjectList.txt`
do

#create a variable which gets the bblid and scanid and prints the scanid to the screen
bblid=`echo $i | cut -d "_" -f 1`
scanid=`echo $i | cut -d "_" -f 2`

echo "Processing subject........" $scanid

#find the file for the subject's ratings and faces
path=`ls -d /data/joy/BBL/studies/conte/processedData/$bblid/*$scanid/associated_files/behavioralRatings/pre`
rating=`ls -d /data/joy/BBL/studies/conte/rawData/$bblid/*$scanid/associated_files/behavioralRatings/pre/*"$scanid"*Pre_task_faces*.log` 

#run the R script that parses the log file and gets the ratings responses for each subject
/import/monstrum2/Applications/R3.2.3/bin/R --file=/data/joy/BBL/projects/conteReproc2017/behaviroalRatings/pre/pre_parse_logfiles_ratings_design3.R --slave --args "$rating" "$scanid" "$path"

#create a file called pre_ratings_date.csv which gets the following column headers
echo "Subject,Timepoint,Answer.quest29,Face.quest29,Answer.quest30,Face.quest30,Answer.quest31,Face.quest31,Answer.quest32,Face.quest32,Answer.quest37,Face.quest37,Answer.quest38,Face.quest38,Answer.quest39,Face.quest39,Answer.quest40,Face.quest40" > //data/joy/BBL/studies/conte/subjectData/behavioralRatings/pre_ratings_"$date".csv
done

#for every individual's ratings file, append to the aggregate csv file
for k in $( ls -d /data/joy/BBL/studies/conte/processedData/$bblid/*$scanid/associated_files/behavioralRatings/pre/*_pre_ratings_data.csv ) ;do
tail -1 "$k" | cut -d "," -f 2-100 >> data/joy/BBL/studies/conte/subjectData/behavioralRatings/pre_ratings_"$date".csv
done




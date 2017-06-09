#this script will go through the conte design3 subjects and extract their responses to catch trials during run1 and run2 fear conditioning and output the data to aggregate files

#get today's date into a variable
date=`date +%Y-%m-%d`

#loop through each conte subject and
for i in `cat /data/joy/BBL/studies/conte/subjectData/design3FullSubjectList.txt`
do

#create a variable for bblid and scanid and output scanid to the screen
bblid=`echo $i | cut -d "/" -f 1`
datexscanid=`echo $i | cut -d "/" -f 2`
scanid=`echo $datexscanid | cut -d "x" -f 2`

echo "Processing subject......." $scanid

#create variables for each subject's run1 and run2 fear conditioning logfile
logfile_run1=`ls -ltr -d /data/joy/BBL/studies/conte/rawData/$bblid/*$scanid/associated_files/ratings/post/*$scanid-fearConditioning_run1_wheel*.log | tail -1 | rev | cut -d " " -f1 | rev` 
logfile_run2=`ls -ltr -d /data/joy/BBL/studies/conte/rawData/$bblid/*$scanid/associated_files/ratings/post/*$scanid-fearConditioning_rev_wheel*.log | tail -1 | rev | cut -d " " -f1 | rev` 

#if the catch_trial directory doesn't exist then make it
if [ ! `ls -d /data/joy/BBL/studies/conte/rawData/$bblid/*$scanid/associated_files/behavioralQa/catchTrials` ]; then
mkdir /data/joy/BBL/studies/conte/rawData/$bblid/$datexscanid/associated_files/behavioralQa
mkdir /data/joy/BBL/studies/conte/rawData/$bblid/$datexscanid/associated_files/behavioralQa/catchTrials
fi

#create a variable for the catch trial directory for each subject
path=`ls -d /data/joy/BBL/studies/conte/rawData/$bblid/*$scanid/associated_files/behavioralQa/catchTrials`

#run the R scripts to parse the fear conditioning run1 and run2 logfiles and extract catch trial data, passing them the file path to the log file, the scanid, and the subject's catch trial directory
/share/apps/R/R-3.1.1/bin/R --file=/data/joy/BBL/projects/conteReproc2017/behavioralCatchTrial/parse_logfile_catch_trial_run1.R --slave --args "$logfile_run1" "$scanid" "$path"

/share/apps/R/R-3.1.1/bin/R --file=/data/joy/BBL/projects/conteReproc2017/behavioralCatchTrial/parse_logfile_catch_trial_run2.R --slave --args "$logfile_run2" "$scanid" "$path"

#create a file called run1_summary_response_trial_data_"$date".csv and run2_summary_response_trial_data_"$date".csv which get the following column headers
echo "Scanid,Number Catch Responses Missing,Number Catch Extra Responses,Number Response to Non-Catch Trials" > /data/joy/BBL/studies/conte/subjectData/behavioralCatchTrial/run1_summary_response_trial_data_"$date".csv

echo "Scanid,Number Catch Responses Missing,Number Catch Extra Responses,Number Response to Non-Catch Trials" > /data/joy/BBL/studies/conte/subjectData/behavioralCatchTrial/run2_summary_response_trial_data_"$date".csv

echo "Scanid,Aversive Face with Tone,Neutral Face,Aversive Face no Tone,Aversive Tone,Crosshair,Catch Trial" > /data/joy/BBL/studies/conte/subjectData/behavioralCatchTrial/run1_response_type_table_data_"$date".csv

echo "Scanid,Aversive Face with Tone,Neutral Face,Aversive Face no Tone,Aversive Tone,Crosshair,Catch Trial" > /data/joy/BBL/studies/conte/subjectData/behavioralCatchTrial/run2_response_type_table_data_"$date".csv

done

#for every individual's summary response files, append to the appropriate csv file
for k in $( ls -d /data/joy/BBL/studies/conte/rawData/*/*x*/associated_files/behavioralQa/catchTrials/*_run1_summary_response_trial_data.csv ) ;do
tail -1 "$k" | cut -d "," -f 2-100 >> /data/joy/BBL/studies/conte/subjectData/behavioralCatchTrial/run1_summary_response_trial_data_"$date".csv
done

for j in $( ls -d /data/joy/BBL/studies/conte/rawData/*/*x*/associated_files/behavioralQa/catchTrials/*_run2_summary_response_trial_data.csv ) ;do
tail -1 "$j" | cut -d "," -f 2-100 >> /data/joy/BBL/studies/conte/subjectData/behavioralCatchTrial/run2_summary_response_trial_data_"$date".csv
done

for l in $( ls -d /data/joy/BBL/studies/conte/rawData/*/*x*/associated_files/behavioralQa/catchTrials/*_run1_response_type_table_data.csv ) ;do
tail -1 "$l" | cut -d "," -f 2-8 >> /data/joy/BBL/studies/conte/subjectData/behavioralCatchTrial/run1_response_type_table_data_"$date".csv
done

for k in $( ls -d /data/joy/BBL/studies/conte/rawData/*/*x*/associated_files/behavioralQa/catchTrials/*_run2_response_type_table_data.csv ) ;do
tail -1 "$k" | cut -d "," -f 2-8 >> /data/joy/BBL/studies/conte/subjectData/behavioralCatchTrial/run2_response_type_table_data_"$date".csv
done

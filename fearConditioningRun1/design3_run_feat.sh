#this script runs first-level (subject-level) feats on the fear conditioning run1 data. It will run different designs depending on which line you comment in. Note: you can only run one feat design at a time, but can run it for all subjects in your subject list. 

#for each subject in the subject list
for i in `cat /data/joy/BBL/studies/conte/subjectData/design3InclusionsSubjectListFslFeat.txt`;
do

#create variables for their fear conditioning order and the subject's scanid and bblid  
bblid=`echo $i | cut -d "," -f 1`
scanid=`echo $i | cut -d "," -f 2`
order=`echo $i | cut -d "," -f 3`

#if the subject is order 1_3 and you chose to run run1 then run the following feat design (the one commented in) template
if [ $order == "1_3" ]; then
fsf1=/import/monstrum/conte_815814/scripts/functional/fsf_templates/order1_3_run1_linear_mini_block_design.fsf
#fsf1=/import/monstrum/conte_815814/scripts/functional/fsf_templates/order1_3_run1_linear.fsf
#fsf1=/import/monstrum/conte_815814/scripts/functional/fsf_templates/order1_3_run1_split_half_sec1_sec5.fsf
task=fearcondV3R1
fi

#if the subject is order 0_2 and you chose to run run1 then run the following feat design (the one commented in) template
if [ $order == "0_2" ]; then
fsf1=/data/joy/BBL/studies/conte/fmriDesignFiles/order0_2/run1/fsf_templates/order0_2_run1_linear_mini_block_design.fsf
#fsf1=/data/joy/BBL/studies/conte/fmriDesignFiles/order0_2/run1/fsf_templates/order0_2_run1_linear.fsf
#fsf1=/data/joy/BBL/studies/conte/fmriDesignFiles/order0_2/run1/fsf_templates/order0_2_run1_split_half_sec1_sec5.fsf
task=fearcondV3R1
fi

#create a temp design.fsf which will hold subject specific data to replace the standard template for feat
temp1=/data/joy/BBL/studies/conte/fmriDesignFiles/order0_2/run1/fsf_templates/${task}_temp1.fsf

#comment in the suffix you wish to use, this will be how the feat design output is identified in the subject's directory
suffix1=bbr_linear_mini_block.feat
#suffix1=bbr_linear.feat
#suffix1=bbr_split_half_sec1_sec5.feat


### CREATES FSF FILES

	#print to the screen the scanid, order and run to be processed
	echo $scanid
	echo $order

	#create a variable for the subject's functional nifti image, print it to the screen and if one doesn't exist print an error and move to the next subject
	func=$(ls /data/joy/BBL/studies/conte/rawData/$bblid/*$scanid/*${task}*/nifti/*fearcond*.nii.gz 2>/dev/null)	
        echo $func
	if [ -z $func ]; then
		echo  "no functional image"
		continue
	fi

	#remove the nii.gz appendage from the file name and append the suffix you chose above for the output name then print that to the screen
	func=$(echo $func | sed "s+.nii.gz++g")
	feat1=${func}_${suffix1}

	echo $feat1

	#create a variable for the subject's biascorrected structural image, print it to the screen and if one doesn't exist print an error and move to the next subject	
	mprage=$(ls /data/joy/BBL/studies/conte/processedData/structural/conte_design3_n118_structural_201706051547/$bblid/*$scanid/antsCT/BrainSegmentation0N4.nii.gz 2>/dev/null)
	if [ -z $mprage ]; then
		echo  "no bias corrected mprage"
		continue
	fi

	#remove the nii.gz appendage from the file name and print that file name to the screen
	mprage=$(echo $mprage | sed "s+.nii.gz++g")
	echo $mprage

	#print to the screen which feat design will be run
	echo $fsf1

	
#	if [ ! `ls -d $feat1` ];then
		# modify the design template
		
		#copy the design template to the temp file created above
		cp $fsf1 $temp1
	
		#replace the standard template "fillers" with the subject specific paths to the output directory, functional image, and mprage image
		sed -i "s|###OUTPUT###|$feat1|g" "$temp1" 
                echo "$temp1"
		sed  -i "s|###NIFTI###|$func|g" "$temp1"
		sed  -i "s|###STRUCT###|$mprage|g" "$temp1"

		#run the feat design for that subject
		feat $temp1
#	fi


done


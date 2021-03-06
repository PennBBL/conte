#this script will merge cope & varcope images and make mask for group analysis with flameo

#######
#create a variable for the subject list you pass it and the path to where the output images will go
scanids=$(cat /data/joy/BBL/studies/conte/subjectDatadesign3InclusionsSubjectListFslFeat.txt)
outdir=/data/joy/BBL/studies/conte/subjectData/fearConditioningRun1/merged_copes/

#the suffix of the feat which you want to merge images from
#nickname="bbr_split_half_sec1_sec5"
#nickname="bbr_linear"
nickname="bbr_linear_mini_block"

feat=$nickname.feat


#variables for the level1 feat copes to merge-- can be multiple copes e.g., "cope1 cope2 cope3 cope7"
#Note you'll see cope1 is separate in many so that cope1 can be run and merged with the maskflag set to 1, then can be commented out and the rest of the copes can be run without the mask flag to speed things up
#copes="cope1"
#copes="cope2 cope3 cope4 cope5 cope6 cope7 cope8 cope9 cope10 cope11 cope12 cope13 cope14 cope15 cope16 cope17 cope18 cope19 cope20 cope21 cope22 cope23 cope24" 
#copes="cope2 cope3 cope4 cope5 cope6 cope7 cope8 cope9 cope10 cope11 cope12 cope13 cope14 cope15 cope16 cope17 cope18 cope19 cope20 cope21 cope22 cope23 cope24 cope25 cope26 cope27 cope28 cope29 cope30 cope31 cope32"
copes="cope2 cope3 cope4 cope5 cope6 cope7 cope8 cope9 cope10 cope11 cope12"
mask="mask"
varcopeflag=1   #if=1, also will merge varcopes 
maskflag=0 # if =1, also will merge masks  
run=1 #the fear conditioning run you are merging (run1 or run2 reversal)
 
#create a variable for the output log directory
logdir=/data/joy/BBL/projects/conteReproc2017/fearConditioningRun1/logs

#######

#remove any existing log file for missing copes
rm -f $logdir/conte_missing_copes_for_merge.txt

#print out which feat will be merged and the number of subjects to merge
echo "feat is $feat"
nsubj=$(echo $scanids | wc |  awk '{print $2}')
echo "$nsubj subjects"

echo "*********"

#for each cope in the list, print out the cope and remove any existing cope and varcope lists 
for c in $copes; do
	echo ""
	echo ""
	echo "********************"
	echo "now working on $c"
	echo "********************"
	rm -f copelist_tmp.txt
	rm -f varcopelist_tmp.txt

	#for each subject print out the scanid
	for b in $scanids; do
		echo ""
		echo $b
	
		#create a variable which gets the fear conditioning standardized cope image for each subject
		image=$(ls -d /data/joy/BBL/studies/conte/processedData/$bblid/*$scanid/*V3R${run}*/nifti/*SEQ*${feat}/reg_standard/stats/${c}.nii.gz)

		#if that image doesn't exist then output an error and append it to the error text file
		if [ -z "$image" ]; then
			echo "COPE MISSING"
			echo -e"$b \t $c" >> $logdir/conte_missing_copes_for_merge.txt
			exit 1
		fi

		#if it does exist print the path and output to a cope list
		echo $image
		echo $image >> copelist_tmp.txt
		
		#if you should also merge varcopes then 
		if [ "$varcopeflag" == 1 ]; then
			echo "merging varcopes also"	

			#create a variable which gets the fear conditioning standardized varcope image for each subject
			varimage=$(ls -d /data/joy/BBL/studies/conte/processedData/$bblid/*$scanid/*V3R${run}*/nifti/*SEQ*${feat}/reg_standard/stats/var${c}.nii.gz)	

			#if that image doesn't exist then output an error and append it to the error text file
			if [ -z "$varimage" ]; then
				echo "VARCOPE MISSING"
				echo -e"$b \t var${c}" >> $logdir/conte_missing_copes_for_merge.txt
				exit 1
			fi

		#if it does exist print the path and output to a varcope list
		echo $varimage
		echo $varimage >> varcopelist_tmp.txt
		fi

		#if the mask flag is set to 1 then 
		if [ "$maskflag" == 1 ]; then
			echo "merging masks also"	

			#create a variable which gets the fear conditioning standardized mask image for each subject
			maskimage=$(ls -d /data/joy/BBL/studies/conte/processedData/$bblid/*$scanid/*V3R${run}*/nifti/*SEQ*${feat}/reg_standard/mask.nii.gz)

			#if that image doesn't exist then output an error and append it to the error text file
			if [ -z "$maskimage" ]; then
				echo "MASK MISSING"
				echo -e"$b \t mask" >> $logdir/conte_missing_copes_for_merge.txt
				exit 1
			fi

		#if it does exist print the path and output to a mask list
		echo $maskimage
		echo $maskimage >> masklist_tmp.txt
		fi


	done	

	#print the number of subjects to be merged for copes
	echo ""
	echo "********************"
	echo "now merging $nsubj images for $c"

	#merge the copes in the cope list for each subject
	fslmerge -t ${outdir}/n${nsubj}_${nickname}_run${run}_${c} $(cat copelist_tmp.txt) 
	
	#if varcope is set to 1 then
	if [ "$varcopeflag" == 1 ]; then
	
		#print the number of subjects to be merged for varcopes
		echo ""
		echo "now merging $nsubj images for var${c}"

		#merge the varcopes in the varcope list for each subject
		fslmerge -t ${outdir}/n${nsubj}_${nickname}_run${run}_var${c} $(cat varcopelist_tmp.txt) 
		echo "********************"
	fi

	#if mask is set to 1 then
	if [ "$maskflag" == 1 ]; then
	
		#print the number of subjects to be merged for the mask
		echo ""
		echo "now merging $nsubj images for mask"

		#merge the masks in the mask list for each subject and binarize the mask and get overlap
		fslmerge -t ${outdir}/n${nsubj}_mask $(cat masklist_tmp.txt) 
		fslmaths ${outdir}/n${nsubj}_mask -Tmin -bin -mas /import/monstrum/Applications/fsl5/data/standard/MNI152_T1_2mm_brain_mask ${outdir}/${nickname}"_n"${nsubj}"_run"${run}"_final_maskoverlap"
		echo "********************"
	fi

done

rm -f copelist_tmp.txt
rm -f varcopelist_tmp.txt
rm -f masklist_tmp.txt

echo "output at ${outdir}/n${nsubj}_${nickname}_run${run}_${c}"

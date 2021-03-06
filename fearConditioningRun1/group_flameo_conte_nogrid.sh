#assumes have already run created working directory wd with design files, as well as mask and merged images in merged_images directory.
#designs are expected to be named "design".mat, .con, .grp, etc etc
#specify wd and nickname below
#this version runs without grid

#set number of subjects
nsubj=n110  #eg n61

#create a variable for the path to where the output images will go and where the design files exist
wd=/data/joy/BBL/studies/conte/processedData/fearconditioningRun1/single_group_level_feats/single_group_linear_mini_block_${nsubj}

#variables for the level1 feat copes ----can be multiple copes e.g., "cope1 cope2 cope3 cope7"
#copes="cope1 cope2 cope3 cope4 cope5 cope6 cope7 cope8 cope9 cope10 cope11 cope12 cope13 cope14 cope15 cope16 cope17 cope18 cope19 cope20 cope21 cope22 cope23 cope24 cope25 cope26 cope27 cope28 cope29 cope30 cope31 cope32"
#copes="cope1 cope2 cope3 cope4 cope5 cope6 cope7 cope8 cope9 cope10 cope11 cope12 cope13 cope14 cope15 cope16 cope17 cope18 cope19 cope20 cope21 cope22 cope23 cope24"
copes="cope1 cope2 cope3 cope4 cope5 cope6 cope7 cope8 cope9 cope10 cope11 cope12"

#the suffix of the feat which you want to run flame1 on
#nickname="bbr_linear"
nickname="bbr_linear_mini_block"
#nickname="bbr_split_half_sec1_sec5"

#print out the working directory and feat name and if the working directory doesn't exist then exit the script
echo "working directory is $wd"
echo ""
echo "nickname is $nickname"
echo ""
if [ ! -d "$wd" ]; then
	echo "working directory not found!! exiting!"
	exit 1
fi

echo "*********"
echo "for all analyses, lev2 cope is $lev2_cope"
echo "*******"

#for each cope in the list defined above...
for c in $copes; do
	echo ""
	echo "working on $c"

#create variables for the merged cope, varcope, and mask files created by the conte_merge_images_mask.sh script
cfile=$(ls -d /data/joy/BBL/studies/conte/subjectData/fearConditioningRun1/merged_copes/${nsubj}_${nickname}_run${run}_${c}.nii.gz)
vfile=$(ls -d /data/joy/BBL/studies/conte/subjectData/fearConditioningRun1/merged_copes/${nsubj}_${nickname}_run${run}_var${c}.nii.gz)

mask=/data/joy/BBL/studies/conte/subjectData/fearConditioningRun1/merged_copes/${nickname}_${nsubj}_run${run}_final_maskoverlap.nii.gz

#makes output dir within working directory defined above and the cope number appended
outdir=${nickname}_${nsubj}_${c}  

#create a variable for the outpath directory
outpath=$(ls -d $wd/$outdir 2> /dev/null)

#run flameo on the cope, varcope, mask specified with the design files in the working directory
flameo --cope=$cfile --varcope=$vfile --mask=$mask --dm=${wd}/design.mat --tc=${wd}/design.con  --cs=${wd}/design.grp --runmode=flame1 --logdir=${wd}/${outdir} --npo

#if have fstat designs, add the above after the .grp: --fc=${wd}/design.fts

done

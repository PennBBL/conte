%make a 2nd level EV design matrix (for design.mat) for conte flameo analysis;
data=load('design_data_n96.csv');%this is the matrix with all the subject variables;
%data=load('ppi_design_data.csv');

%conte subjects the data frame is 1- bblid, 2-scanid, 3-fmri_exclude, 4-group, 5-fmri version order, 6- scream unpleasantness
%rating, 7-cains total (negative symptoms), 8-positive symptoms (sips p),
%9-anxiety rating (stai trait), 10-post scan ratings confidence, 11-motion,
%12-eeg, 13-clean, 14=social cains, 15=non social cains, 16=CAINS map
%score, 17=CAINS expression score, 18=CAINS sum 1 + 2 (motivation only), 19=age, 20=sex (1=male, 2=female), 21=race (1=caucasian, 2=non-caucasian, 22=run1 ratings aversive - neutral, 23=run1 neutral ratings, 24=run1 - pre ratings aversive - neutral, 25=run1 - pre neutral ratings. 

%SET FLAGS
numgp=3; %1 or 2 or 3?
patient=1; %exclude NC's and count patients and cr's as one single group
covariate=0; %0 if only running group, 1 if including covariate(s)
clean=0; %to delete subjects with this conditioner rating (0=bad,1=ok,2=good), 
%if want to delete all but good (2) subjects, then make this flag a 2, if want to delete all but ok (1) subjects then make this flag a 1;
%if want to keep all subjects make this a 0
demean_within=0; %do you want to demean your covariate within groups or across all groups? If you want to demean within groups then set this flage to 1;

%exclude data based on missing values (set to group missing right now)
excludeind=find(isnan(data(:,4)));
data(excludeind,:)=''; 

%exclude based on high motion (>.3)
motionind=find(data(:,11)>.3);
data(motionind,:)=''; 

%exclude based on fmri_exclude column
exclude=find(data(:,3)==1);
data=data(setdiff(1:size(data,1),exclude),:);

%exclude data based on conditioner status (0=bad, 1=ok, 2=good)
condition=find(data(:,13)<clean);
data=data(setdiff(1:size(data,1),condition),:);

%dx EVs
group=data(:,4); %NC=0, CR=1, P=2
ncind=find(group==0);
crind=find(group==1);
szind=find(group==2);
ncinclude=zeros(length(group),1); 
ncinclude(ncind)=1;
crinclude=zeros(length(group),1); 
crinclude(crind)=1;
szinclude=zeros(length(group),1); 
szinclude(szind)=1;

if patient==1;
data(ncind,:)='';
end;

%other covariates
%covariate1=data(:,5); %0=0_2, 1=1_3; 
%covariate1=data(:,6); %scream unpleasantness
%covariate1=data(:,7); %cains total
%covariate1=data(:,8); %positive symptoms
%covariate1=data(:,9); %stai trait anxiety
%covariate1=data(:,10); %ratings confidence
%covariate1=data(:,12); %eeg
%covariate1=data(:,14); %social cains
%covariate1=data(:,15); %non social cains
%covariate1=data(:,16); %CAINS MAP score
%covariate1=data(:,17); %CAINS expression score
%covariate1=data(:,18); %CAINS motivation only
%covariate1=data(:,19); %age
%covariate1=data(:,20); %sex
%covariate1=data(:,21); %race
%covariate1=data(:,22); %ratings run1 aversive - neutral
%covariate1=data(:,23); %ratings run1 neutral
%covariate1=data(:,24); %ratings run1 - pre aversive - neutral
covariate1=data(:,25); %ratings run1 - pre neutral

if numgp==3 & covariate==0 & demean_within==0;
design=[ncinclude crinclude szinclude];% run most basic group only no covariates model
end;

if numgp==1 & covariate==0 & demean_within==0;
design=ones(length(data),1);
end;
    
if covariate==1 & numgp==3 & demean_within==0;
design=[ncinclude crinclude szinclude covariate1 ];%add covariates in
end;

if covariate==1 & numgp==1 & demean_within==0;
design=[ones(length(data),1) covariate1 ];
end;

%if you want to have your design demean within group
if covariate==1 & numgp==3 & demean_within==1;
design=[ncinclude crinclude szinclude covariate1 covariate1 covariate1];
design(ncinclude==0,4)=NaN;
design(crinclude==0,5)=NaN;
design(szinclude==0,6)=NaN;
end;

designincl=design(:,:);
 
%demean all but the group EVs;
szdes=size(designincl);
designincl2=designincl;

if covariate==1;
for i=1+numgp:szdes(2);
designincl2(:,i)=designincl(:,i)-nanmean(designincl(:,i));
end;
end;

numnan=length(find(isnan(designincl2)));
disp(['There were this many NaNs in the design matrix: ' num2str(numnan)]);
designincl2(isnan(designincl2))=0;
disp('Your final design matrix is called designincl2');

%create design.grp
designgrp=ones(length(designincl2),1);

%print out design.mat template
designincl2

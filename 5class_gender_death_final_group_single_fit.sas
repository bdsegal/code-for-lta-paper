filename acllta "lta_data.dat.dat";

PROC IMPORT OUT= WORK.acllta 
            DATAFILE= "ACL HBs with covariates for LTA analyses incl death.csv" 
            DBMS=CSV REPLACE;
     GETNAMES=YES;
     DATAROW=2; 
RUN;

*BCB RECOMMENDATION: Add a "dead" response option to the 3 indicators of interest;
data recode;
	set acllta;

	*create indicator for death at W1;
	deadw1=2;

	*create indicator responses for death at all waves;
	array recode1(5)smokeW1 smokeW2 smokeW3 smokeW4 smokeW5;
	array recode2(5)bmiW1 bmiW2 bmiW3 bmiW4 bmiW5;
	array recode3(5)drinkW1 drinkW2 drinkW3 drinkW4 drinkW5;
	array recode4(5)deadW1 deadW2 deadW3 deadW4 deadW5;

	do i=1 to 5;
	if recode4[i]=1 and recode1[i]=. then recode1[i]=3;
	if recode4[i]=1 and recode2[i]=. then recode2[i]=4;
	if recode4[i]=1 and recode3[i]=. then recode3[i]=4;
	end;

	*drop new variable i;
	drop i;

  age1Centered = age1 - 54;
  
run;

data maleData;
  set recode;
  if male1 = 1;
run;

data femaleData;
  set recode;
  if male1 = 0;
run;

*BCB RECOMMENDATION: Add one subject who is dead at wave 1;
data dead;
input V1 age1Centered male1 white1 black1 educ1
      smokew1 smokew2 smokew3 smokew4 smokew5
      bmiw1 bmiw2 bmiw3 bmiw4 bmiw5
      drinkw1 drinkw2 drinkw3 drinkw4 drinkw5
      weight
      deadw2 deadw3 deadw4 deadw5 deadw1;
cards;
3618 0 0 1 0 0 3 3 3 3 3 4 4 4 4 4 4 4 4 4 4 . 1 1 1 1 1
3619 0 0 0 1 0 3 3 3 3 3 4 4 4 4 4 4 4 4 4 4 . 1 1 1 1 1
3620 0 0 1 0 1 3 3 3 3 3 4 4 4 4 4 4 4 4 4 4 . 1 1 1 1 1
3621 0 0 0 1 1 3 3 3 3 3 4 4 4 4 4 4 4 4 4 4 . 1 1 1 1 1
3622 0 1 1 0 0 3 3 3 3 3 4 4 4 4 4 4 4 4 4 4 . 1 1 1 1 1
3623 0 1 0 1 0 3 3 3 3 3 4 4 4 4 4 4 4 4 4 4 . 1 1 1 1 1
3624 0 1 1 0 1 3 3 3 3 3 4 4 4 4 4 4 4 4 4 4 . 1 1 1 1 1
3625 0 1 0 1 1 3 3 3 3 3 4 4 4 4 4 4 4 4 4 4 . 1 1 1 1 1
;
run;

proc sort data=dead; by V1; run;
proc sort data=maleData; by V1; run;
proc sort data=femaleData; by V1; run;

data analysis;
  merge maleData femaleData dead; by V1;
  maleGroup = male1;
  if maleGroup = 0 then maleGroup = 2;
run;

proc lta data=analysis OUTPARAM=param outest=fit outpost=post ;
title "Grouped by sex, covariate at wave 1 (age, educ, black)";
Seed 1;
nstatus 6;
ntimes 5;
items smokeW1 bmiW1 drinkW1 deadW1
      smokeW2 bmiW2 drinkW2 deadW2
      smokeW3 bmiW3 drinkW3 deadW3
	    smokeW4 bmiW4 drinkW4 deadW4
      smokeW5 bmiW5 drinkW5 deadW5;
categories 3 4 4 2;
measurement times;
covariates1 age1Centered educ1 black1;
groups maleGroup;
cores 4;
run;

PROC EXPORT DATA= WORK.Post 
            OUTFILE= ".\singleFitOut\post_probs_group_final.csv" 
            DBMS=CSV REPLACE;
     PUTNAMES=YES;
RUN;

PROC EXPORT DATA= WORK.Param 
            OUTFILE= ".\singleFitOut\param_group_final.csv" 
            DBMS=CSV REPLACE;
     PUTNAMES=YES;
RUN;

PROC EXPORT DATA= WORK.Fit 
            OUTFILE= ".\singleFitOut\fit_group_final.csv" 
            DBMS=CSV REPLACE;
     PUTNAMES=YES;
RUN;


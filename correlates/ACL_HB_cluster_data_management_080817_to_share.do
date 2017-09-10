*********************************************
*											*
*    ACL Clusters of HBs across Life Course * 
*
*    code started Nov 1, 2016	            
*
*    Author: Katherine Y. Lin               *
*********************************************
clear all
clear matrix
set more off
set maxvar 32767

** First get general ACL data
cap: use "acl12345d6b.dta"
cap: renpfix v V

** Merge in LTA statuses
sort V1
cap: merge 1:1 V1 using  "latent statuses 080617.dta"

drop _merge 

** Merge in health behaviors 
cap: merge 1:1 V1 using "healthbehaviorsnew.dta"
drop smokew1-smokew5 healthCategory3w1-healthCategory5w5
drop _merge

*******************************

* MAKING DEMOGRAPHIC CONTROLS * 

*******************************

** AGE
gen age1=V2000
gen age2=age1+3
gen age3=age2+5
gen age4=age3+7
gen age5=age4+10


** GENDER 
gen male1=.
recode male1 .=1 if V103==1
recode male1 .=0 if V103==2

gen female1=. 
recode female1 .=1 if male1==0
recode female1 .=0 if male1==1

* Years of education at baseline
gen educ1=V2007

** 4 category EDUCATION 
gen ed4cat1=.
replace ed4cat1=0 if educ1<12
replace ed4cat1=1 if educ1==12
replace ed4cat1=2 if educ1>12&educ1<16
replace ed4cat1=3 if educ1>=16&educ1<.
lab def ed4cat1 0 "< 12 yrs." 1 "12 yrs." 2 "13-15 yrs." 3 "16+ yrs."
lab val ed4cat1 ed4cat1
gen ed4cat2=ed4cat1
gen ed4cat3=ed4cat1
gen ed4cat4=ed4cat1
gen ed4cat5=ed4cat1

** 3 category education
gen ed3cat1=.
recode ed3cat1 .=1 if educ1<=12
recode ed3cat1 .=2 if educ1>12&educ1<16
recode ed3cat1 .=3 if educ1>=16&educ1<.
label def ed3cat 1"HS or less" 2"Some college" 3"BA+"
label val ed3cat1 ed3cat
gen ed3cat2=ed3cat1
gen ed3cat3=ed3cat1
gen ed3cat4=ed3cat1
gen ed3cat5=ed3cat1

** Race
gen white1=.
recode white1 .=1 if V2004==1
recode white1 .=0

gen black1=.
recode black1 .=1 if V2004==2
recode black1 .=0

gen race1=.
recode race1 .=1 if V2004==1
recode race1 .=2 if V2004==2
recode race1 .=3
label def race1 1"NHWhite" 2"NHBlack" 3"Other"
label val race1 race1

** HH income 
gen cont_hhincome1=V2035

* Marital status 
gen married1=.
recode married1 .=1 if V601!=3
recode married1 .=0  

gen married2=.
recode married2 .=1 if V4501!=3
recode married2 .=0 
replace married2=. if V4001==.a

gen married3=.
recode married3 .=1 if V10127==1
recode married3 .=1 if V10127==2
recode married3 .=0 
replace married3=. if V10001==.a

//Vars asked different in wave 4//
*gen years lived with someone if not married
gen months4=V12150*12 if V12151==2
replace months4=V12150 if V12151==1
gen gt6mths4=.
recode gt6mths4 .=1 if months4>=6&months4<=300
recode gt6mths4 .=0 if V12149==1
replace gt6mths4=. if V12001==.a
replace gt6mths4=. if V12150==.n
tab gt6mths4

gen married4=.
recode married4 .=1 if V12147==1
recode married4 .=1 if V12149==1&gt6mths4==1
recode married4 .=0 
replace married4=. if V12001==.a
tab married4

gen married5=.
recode married5 .=1 if V15408==1
recode married5 .=0
replace married5=. if V15001==3
tab married5

* Employment Status
gen work1=.
recode work1 .=1 if V1110!=5
recode work1 .=0
 
gen work2=.
recode work2 .=1 if V5110!=5
recode work2 .=0
replace work2=. if V4001==.a

gen work3=.
recode work3 .=1 if V10310!=5
recode work3 .=0
replace work3=. if V10001==.a

gen work4=.
recode work4 .=1 if V13302!=5
recode work4 .=0
replace work4=. if V12001==.a

gen work5=.
recode work5 .=1 if V16109!=5
recode work5 .=0
replace work5=. if V15001==3

* Ever parent?
gen everparent1=.
recode everparent1 .=1 if V430!=1
recode everparent1 .=0

gen everparent2=.
recode everparent2 .=1 if V4413!=1
recode everparent2 .=0 
replace everparent2 =. if V4001==.a

gen everparent3=.
recode everparent3 .=1 if V10133!=1
recode everparent3 .=0
replace everparent3 =. if V10001==.a

gen everparent4=.
recode everparent4 .=1 if V12157!=2
recode everparent4 .=0
replace everparent4 =. if V12001==.a

gen everparent5=.
recode everparent5 .=1 if V15501!=0
recode everparent5 .=0
replace everparent5=. if V15001==3


****************
* HEALTH SHOCK * 
****************

* serious illness in last three years 
gen seriousill1=.
recode seriousill1 .=1 if V1509==1
recode seriousill1 .=0

gen seriousill2=.
recode seriousill2 .=1 if V5414==1
recode seriousill2 .=0
replace seriousill2=. if V4001==.a

gen seriousill3=.
recode seriousill3 .=1 if V10413==1
recode seriousill3 .=0
replace seriousill3=0 if V10414<1989
replace seriousill3=. if V10001==.a

gen seriousill4=.
recode seriousill4 .=1 if V12484==1
recode seriousill4 .=0
replace seriousill4=0 if V12485<1994
replace seriousill4=. if V12001==.a

gen seriousill5=.
recode seriousill5 .=1 if V16318==1
recode seriousill5 .=0
replace seriousill5=0 if V16319<2001
replace seriousill5=. if V15001==3

* life threatening illess EVER
gen lifethreaten1=.
recode lifethreaten1 .=1 if V1505==1
recode lifethreaten1 .=0
tab lifethreaten1


***********
* SMOKING *
***********

* 3 CATEGORY INDICATORS OF SMOKING STATUS

la def smoke 1 "current" 2 "never" 3  "former"

//WAVE 1//
gen smoke1=.
    replace smoke1=1 if V943==1
    replace smoke1=2 if V945==5
    replace smoke1=3 if V945==1

la val smoke1 smoke

//WAVE 2//
gen smoke2=.
    replace smoke2=1 if V4942==1
    replace smoke2=2 if V4942==5
    replace smoke2=3 if V4942==5 & V943==1
    replace smoke2=3 if V4942==5 & V945==1
    replace smoke2=. if V4001==.a
la val smoke2 smoke

//WAVE 3//
gen smoke3=.
    replace smoke3=1 if V10269==1
    replace smoke3=2 if V10269==5
    replace smoke3=3 if V10269==5 & V943==1
    replace smoke3=3 if V10269==5 & V4942==1
    replace smoke3=3 if V10269==5 & V945==1
    replace smoke3=. if V10001==.a
la val smoke3 smoke

//WAVE 4//
gen smoke4=.
    replace smoke4=1 if V12239==1
    replace smoke4=2 if V12239==5
    replace smoke4=3 if V12239==5 & V943==1
    replace smoke4=3 if V12239==5 & V4942==1
    replace smoke4=3 if V12239==5 & V10269==1
    replace smoke4=3 if V12239==5 & V945==1
    replace smoke4=. if V12001==.a
la val smoke4 smoke

//WAVE 5//
gen smoke5=.
	replace smoke5=2 if V15834==5
	replace smoke5=1 if V15835==1
	replace smoke5=3 if V15835==5
	replace smoke5=. if V15001==3
la val smoke5 smoke
tab smoke5

** # of chronic conditions
gen baselinechronic1=V2612

** Functional limitations
*1 -- no functional impairment
*2 -- least severely functionally impaired
*3 -- moderate/severely functionally impaired
*4 -- most serverely functionally impaired

gen func_impair1=.
recode func_impair1 .=1 if V2604==4
recode func_impair1 .=2 if V2604==3
recode func_impair1 .=3 if V2604==2
recode func_impair1 .=4 if V2604==1

** Self-rated health
gen srh1=.
recode srh1 .=1 if V915==5
recode srh1 .=2 if V915==4
recode srh1 .=3 if V915==3
recode srh1 .=4 if V915==2
recode srh1 .=5 if V915==1

***********
* Weights * 
***********
gen weight1=V1860
gen weight2=V5860
gen weight3=V10822
gen weight4=V12962
gen weight5=V16901

***********
* DEATH   * 
***********

* Updated dead-by-wave numbers in 2012

gen death1=0
gen death2=V9003
gen death3=V11037
gen death4=V11648
gen death5=V11708
recode death5 1=0 if V15001==1|V15001==2
** Recoded N=15 cases back from the dead 
** These respondents died by end of data collection in W5, but provided valid survey data @ W5

*****************	
* Resp status   * 
*****************	

label def resp 1"respondent" 2"dead" 3"non-respondent" 

gen resp1=1
label val resp1 resp

gen resp2=1
recode resp2 1=2 if death2==1
recode resp2 1=3 if V4001==.a 
label val resp2 resp

gen resp3=1
recode resp3 1=2 if death3==1
recode resp3 1=3 if V10001==.a
label val resp3 resp

gen resp4=1
recode resp4 1=2 if death4==1
recode resp4 1=3 if V12001==.a
label val resp4 resp

gen resp5=1
recode resp5 1=2 if death5==1
recode resp5 1=3 if V15001==3
label val resp5 resp

***********
* STATUS  * 
***********

** Uses max posterior probability, plus known dead status, and known NR status, to construct. 

gen status1=t1maxstatus

gen status2=t2maxstatus
replace status2=. if resp2==3
tab status2

gen status3=t3maxstatus
replace status3=. if resp3==3
replace status3=6 if resp3==2
tab status3

gen status4=t4maxstatus
replace status4=. if resp4==3
replace status4=6 if resp4==2
tab status4

gen status5=t5maxstatus
replace status5=. if resp5==3
replace status5=6 if resp5==2
tab status5



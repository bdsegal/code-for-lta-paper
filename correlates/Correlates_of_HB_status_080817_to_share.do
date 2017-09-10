****************************************
* CORRELATES OF BASELINE HEALTH STATUS *
*
* Author: Katherine Y. Lin             *
****************************************

//First set up for survey analyses

* create new variable that incorporates strata and psu
gen newpsu = 2*(V1891-1)+V1892

* Set up for survey analyses
svyset [pweight=V1860], strata(newpsu)  

// Table 1 Weighted Baseline Descriptive Information 

* ALL SAMPLE

# delimit ; 
svy: mean age1 cont_hhincome1 work1 married1 everparent1
 baselinechronic1 func_impair1 seriousill1 lifethreaten1 srh1 
;
# delimit cr 

svy: tab ed4cat1 
svy: tab race1 
svy: tab smoke1 
svy: tab drinkCategoryw1 
svy: tab bmi_catW1 
svy: tab status1


* FEMALE sample

preserve
keep if female==1

# delimit ; 
svy: mean age1 cont_hhincome1 work1 married1 everparent1
 baselinechronic1 func_impair1 seriousill1 lifethreaten1 srh1 
;
# delimit cr 

svy: tab ed4cat1 
svy: tab race1 
svy: tab smoke1 
svy: tab drinkCategoryw1 
svy: tab bmi_catW1 
svy: tab status1

restore

* MALE sample

preserve
keep if female==0

# delimit ; 
svy: mean age1 cont_hhincome1 work1 married1 everparent1
 baselinechronic1 func_impair1 seriousill1 lifethreaten1 srh1 
;
# delimit cr 

svy: tab ed4cat1 
svy: tab race1 
svy: tab smoke1 
svy: tab drinkCategoryw1 
svy: tab bmi_catW1 
svy: tab status1

restore 

* Testing for gender difference

# delimit ; 
quietly svy: mean age1 cont_hhincome1 work1 married1 everparent1
 baselinechronic1 func_impair1 seriousill1 lifethreaten1 srh1 
 , over(female) 
;
# delimit cr 

test [age1]0=[age1]1
test [cont_hhincome1]0=[cont_hhincome1]1
test [work1]0=[work1]1
test [married1]0=[married1]1
test [everparent1]0=[everparent1]1
test [baselinechronic1]0=[baselinechronic1]1
test [func_impair1]0=[func_impair1]1
test [seriousill1]0=[seriousill1]1
test [lifethreaten1]0=[lifethreaten1]1
test [srh1]0=[srh1]1

svy: tab ed4cat1 female, col
svy: tab race1 female, col
svy: tab smoke1 female, col
svy: tab drinkCategoryw1 female, col
svy: tab bmi_catW1 female, col
svy: tab status1 female, col

// Table 2. Predictors of Baseline Latent Status membership for Women 

# delimit ;
svy: mlogit status1 age1  i.ed4cat1 i.race1 cont_hhincome1 
work1 married1 everparent1
baselinechronic1 func_impair1 seriousill1 lifethreaten1
 srh1
 if female1==1
, baseoutcome(1) rrr
;
# delimit cr 

est store female_mlogit

// Table 3. Predictors of Baseline Latent Status membership for Men 

# delimit ;
svy: mlogit status1 age1  i.ed4cat1 i.race1 cont_hhincome1 
work1 married1 everparent1
baselinechronic1 func_impair1 seriousill1 lifethreaten1
 srh1
 if female1==0
, baseoutcome(1) rrr
;
# delimit cr 

est store male_mlogit 

est drop _all

// Table 4

svy: tab status1 female, col
tab status1 female

svy: tab status2 female, col
tab status2 female

svy: tab status3 female, col
tab status3 female

svy: tab status4 female, col
tab status4 female

svy: tab status5 female, col
tab status5 female 



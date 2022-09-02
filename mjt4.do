cd E:\lsu\宏观4\replication4
use "ASEC_EC7071.dta", clear

ren asecwt wgt	 

recode race (100=1)(200=2)(300/830=3)
lab def RACE 1 "White" 2 "Black" 3 "Other", modify

//labellist educ

recode educ (0/2=0)(10/19=2.5)(20/29=5.5)(30/39=7.5)(40=9)(50=10)(60=11)(70/73=12) ///
            (80 81=13)(90/92=14)(100/109=15)(110/121=16)(122/125=18), gen(sch)
recode educ (1/71=1)(72/73=2)(80/100=3)(110/125=4), gen(educat)    
lab def educatlb 1 "LTHS" 2 "HS" 3 "SCLG" 4 "CLG+", modify  
lab val educat educatlb
lab var educat "Educ grps"

* Defining potential experience
gen exp=floor(min(max(age-sch-7,0),age-17))
lab var sch "yrs of school"
lab var exp "yrs of exp."

recode exp (0/4=1)(5/9=2)(10/14=3)(15/19=4)(20/24=5)(25/29=6)(30/39=7)(40/50=8), gen(expcat)
lab var expcat "exp groups"

* class of worker last year
recode classwly (0 99=.)(10/14 29=2)(20/28=1) 
lab def CLASSWLY 1 "wage wkr" 2 "selfemp", modify

* dropping imputed earnings
replace incwage=.  if (qincwage>=1 & qincwage<4) | classwly==2 | classwly==.

* calculating weeks & hours for missing values
gen wgtw=wgt if classwly==1 & wkswork1>=1 & wkswork1<=52 & year>=1976 & year<1988 //用来估算1976以前的缺失值
bys sex wkswork2: egen twks=sum(wkswork1*wgtw) 
bys sex wkswork2: egen twgt=sum(wgtw) 
replace wkswork1=floor(twks/twgt) if classwly<=2 & wkswork1==. 

replace uhrsworkly=. if uhrsworkly==999
replace uhrsworkly=98 if uhrsworkly== 99
gen wgth=wgt if classwly==1 & uhrsworkly>=1 & uhrsworkly<=98 & year>=1976 & year<1988 //把1980改成了1976-1987
bys sex fullpart: egen tuhrs=sum(uhrsworkly*wgth)
bys sex fullpart: egen twgth=sum(wgth)
replace uhrsworkly=floor(tuhrs/twgth) if classwly<=2 & uhrsworkly==. 

ren uhrsworkly uhrsly
ren wkswork1 wksly


* identifying FTFY non-agr workers with 0-39 yrs exp
//gen ftfy=1 if exp<49 & wksly>=39 & fullpart==1 
gen ftfy=1 if exp<49 & wksly>=1 & fullpart==1 
lab var ftfy "full-time & full year"
		
replace year=year-1
merge m:1 year using "pce.dta", keepusing(pce)
drop if _merge==2
gen wage=100*incwage/(pce*wksly)  //要乘100，pce是百分数
replace wage=. if wage<141 | ftfy!=1 
lab var wage "real wkly earning"
drop _merge inc* wkswork2 wgth wgtw tw* tuhrs q* pce

format %8.0g year wgt age sex race educ* exp* classw wksly uhrs
save "asec_new.dta", replace



cd E:\lsu\宏观4\replication4
use "asec_new.dta", clear

//keep if year<=1987
//gen lnwage=ln(wage)
//bys year sex educat expcat: egen averagelogweeklywage=mean(lnwage)
//gen wgtw=wgt if wage!=.
gen wgtw=wgt if wage!=. & ftfy==1
bys year sex educat expcat: egen wage1=sum(wage*wgtw) 
bys year sex educat expcat: egen twgt=sum(wgtw) 
gen wage2=wage1/twgt  //wage2 is average log weekly wage in each group each year


//keep if ftfy==1
keep if uhrsly>=35
drop if fullpart==2
gen totalhour1=wksly*uhrsly
bys year sex educat expcat: egen totalhour2=sum(totalhour1*wgt) 
bys year sex educat expcat: egen totalhour4=sum(totalhour1) 
format %14.0g totalhour2 totalhour4
save asec1.dta, replace

keep wage2 year sex educat expcat totalhour2
duplicates drop

bys year: egen totalhour3=sum(totalhour2) 
format %14.0g totalhour3
gen hourshare=totalhour2/totalhour3
bys sex educat expcat: egen meanhourshare=mean(hourshare) 
//bys year sex educat expcat: egen num=_N
//collapse (count) hourshaare [fw=number], by(year sex educat expcat)
save asec2.dta, replace

keep if educat==4
bys year: egen totalwage=sum(wage2*meanhourshare)
bys year: egen totalweight=sum(meanhourshare)
gen skillwage=totalwage/totalweight
keep year skillwage
duplicates drop
save asec3.dta, replace

use asec2.dta, clear
keep if educat==1 | educat==2
bys year: egen totalwage=sum(wage2*meanhourshare)
bys year: egen totalweight=sum(meanhourshare)
gen unskillwage=totalwage/totalweight
keep year unskillwage
duplicates drop

merge 1:1 year using asec3
drop _merge
gen skilltounskill=skillwage/unskillwage
gen logratio=ln(skillwage)-ln(unskillwage)
keep year logratio
save asec4.dta, replace
/////////////////////////////

//use asec1.dta, clear
//keep year sex educat expcat wage2 totalhour4
use "asec_new.dta", clear

//keep if year<=1987
//gen lnwage=ln(wage)
//bys year sex educat expcat: egen averagelogweeklywage=mean(lnwage)
//gen wgtw=wgt if wage!=.
gen wgtw=wgt if wage!=. & ftfy==1
bys year sex educat expcat: egen wage1=sum(wage*wgtw) 
bys year sex educat expcat: egen twgt=sum(wgtw) 
gen wage2=wage1/twgt  //wage2 is average log weekly wage in each group each year


//keep if ftfy==1
//keep if uhrsly>=35
keep if wksly>=1
gen totalhour1=wksly*uhrsly
bys year sex educat expcat: egen totalhour2=sum(totalhour1*wgt) 
bys year sex educat expcat: egen totalhour4=sum(totalhour1) 
format %14.0g totalhour2 totalhour2

keep year sex educat expcat wage2 totalhour2
duplicates drop
//sort year sex educat expcat
//forvalues a=1963/1999{
gen a=wage2 if sex==1 & educat==2 & expcat==3
bys year: egen base=sum(a)
drop a
gen relativewage=wage2/base
bys sex educat expcat: egen meanrelativewage=mean(relativewage) 
gen efficiencyunit=totalhour2*meanrelativewage
save asec5.dta, replace

keep if educat==1
bys year: egen lesshighschool=sum(efficiencyunit) 
keep year lesshighschool
duplicates drop
save asec6.dta, replace

use asec5.dta, clear
keep if educat==2
bys year: egen highschool=sum(efficiencyunit) 
keep year highschool
duplicates drop
save asec7.dta, replace

use asec5.dta, clear
keep if educat==3
bys year: egen somecollege=sum(efficiencyunit) 
keep year somecollege
duplicates drop
save asec8.dta, replace

use asec5.dta, clear
keep if educat==4
bys year: egen college=sum(efficiencyunit) 
keep year college
duplicates drop

merge 1:1 year using asec8.dta
drop _merge
merge 1:1 year using asec7.dta
drop _merge
merge 1:1 year using asec6.dta
drop _merge
gen skillsupply=college+0.5*somecollege
gen unskillsupply=highschool+lesshighschool+0.5*somecollege
gen logsupplyratio=ln(skillsupply/unskillsupply)

gen t=_n
reg logsupplyratio t
predict pred_logsupplyratio
predict resid_logsupplyratio, residual

merge 1:1 year using asec4.dta
drop _merge
reg logratio t
predict resid_logratio, residual
predict pred_logratio

//keep year resid_logsupplyratio resid_logratio

label var year "year"
label var resid_logratio "Detrend relative wage"
label var resid_logsupplyratio "Detrend relative supply"
label var logratio "log wage ratio"
label var logsupplyratio "log supply ratio"


twoway (connected resid_logratio year, yaxis(1)) (connected resid_logsupplyratio year, yaxis(2) msymbol(triangle)), ///
xlabel(1963(6)1999)  aspectratio(0.4) graphregion(color(white)) graphregion(margin(zero)) ///
plotregion(margin(tiny)) xsize(8) ///
title("Detrended skill/unskill wage differential and relative supply")


twoway (connected logratio year, yaxis(1)) (connected logsupplyratio year, yaxis(2) msymbol(triangle)), ///
xlabel(1963(6)1999)  aspectratio(0.5) graphregion(color(white)) ///
plotregion(margin(tiny)) xsize(7) ///
title("Log wage ratio and log supply ratio")


reg logratio logsupplyratio t if year<=1987 
estimates store model1
reg logratio logsupplyratio t
estimates store model2
gen d=0
replace d=1 if year>=1990
gen dt=d*t
reg logratio logsupplyratio t dt
estimates store model3

# delimit ;	
estout model1 model2 model3  using E:\lsu\宏观4\replication4/results.xls,
          replace
		  style(tab) 
	      label  
	      varwidth(20)  
	      
		  mlabels( ) 
          
		  
postfoot(Note: (1) $* p<0.10,\ ** p<0.05,\ *** p<.01$.)  
               
 cells(b(star fmt(3)) 	      	 
		    se(par(`"="("' `")""')))	    
	      stats(N r2, fmt(%9.0g %9.2g)
		    layout("@" "@") 
		    labels("N" "R_squared")) 
	      starlevels(* 0.10 ** 0.05 *** .01)   
         ;   //drop(_cons)  adjusted R_2 is r2_a
		 # delimit cr

////////////////////////////
gen t2=t*t/100
gen t3=t*t*t/1000
gen d92=0
replace d92=1 if year>=1992
gen d92t=d92*t
reg logratio logsupplyratio t t2
estimates store model1
reg logratio logsupplyratio t t2 t3
estimates store model2
reg logratio logsupplyratio t d92t
estimates store model3



//////////////////////////
gen skillsupply2=college+0.29*somecollege
gen unskillsupply2=highschool+0.93*lesshighschool+0.69*somecollege
gen logsupplyratio2=ln(skillsupply2/unskillsupply2)

reg logratio logsupplyratio2 t if year<=1987

reg logratio logsupplyratio2 t



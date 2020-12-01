* Generate dataset with binary outcome

set seed 17022014

scalar k=10					// number of studies
scalar totalsize=0
forvalues i=1/`=k' {
	* Generate study size for trial i from N(n, n/4)
	* mean=200
	scalar studysize`i'=round(rnormal(200, 100))
	scalar totalsize=totalsize+studysize`i'
	scalar cumulsize`i'=totalsize
}

* Create dataset structure
drop _all
set obs `=totalsize'
gen trialid=.
scalar cumulsize0=0
forvalues i=1/`=k' {
	replace trialid=`i' if inrange(_n,`=cumulsize`=`i'-1'+1', `=cumulsize`i'')
}
/*
* Treatment, disease stage, age, sex
gen treat=int(2*uniform())
gen age=50+10 * invnorm(uniform()) // mean 50, sd 10
gen sex=int(2*uniform())

* Model for log odds that Y=1
gen y = uniform() < invlogit(-3 + .1*sex + .045*(age-50) + (-2*treat))
*/

* "Disease stage" allocation
gen stage=.
gen stagetmp=runiform()
replace stage=-.08 if stagetmp<1/3
replace stage=0 if stagetmp>=1/3 & stagetmp<2/3
replace stage=.08 if stagetmp>=2/3
drop stagetmp

* Generate data
gen trt=.
gen y=.
forvalues i=1/`=k' {
	* Treatment allocation
	gen trt_temp=round(runiform()) if trialid==`i'				// equal allocation within trials
	replace trt=trt_temp if trial==`i'
	
	* Generate effect size for trial i from N(mu, tausq)
	* PORT is roughly mu=0.16, tausq=0.05
	* Try mu=0.4, tausq=0.15
	scalar effectsize=rnormal(0.4, `=sqrt(0.15)')
	replace y = uniform() < invlogit(0.01 + (scalar(effectsize)+stage) * trt) if trialid==`i'
	drop trt_temp
}

* Tidying up
label var y "Outcome"

label define trt_ 0 "Control" 1 "Treatment"
label values trt trt_
label var trt "Treatment arm"

recode stage (-.08=1) (0=2) (.08=3)
replace stage=. if inlist(trialid, 3, 5, 9)			// assume stage data not available for all trials
label define stage_ 1 "I" 2 "II" 3 "III"
label values stage stage_
label var stage "Disease stage of patient"

recode trialid (3=1) (4=5) (5=3) (7=2) (10=4) (1=9) (2=7) (6=8) (8=6) (9=10)
label define trialid_ 1 "London" 2 "Paris" 3 "Amsterdam" 4 "Stockholm" 5 "Madrid" ///
	6 "New York" 7 "Chicago" 8 "Los Angeles" 9 "Toronto" 10 "College Station, TX"
label values trialid trialid_
label variable trialid "Trial name"

gen region = 1 + (trialid>5)
label define region_ 1 "Europe" 2 "North America"
label values region region_
label var region "Region"

gen sex = runiform()
replace sex = 1 + (sex<0.5)
label define sex_ 1 "Male" 2 "Female"
label values sex sex_
label var sex "Sex of patient"

gen age=rnormal(50, 7.5)
label var age "Age of patient"

bysort trialid: gen patid = _n
label var patid "Patient ID"

order trialid region patid trt y sex age stage

save "H:\Meta-analysis\ipdmetan_example_logit.dta"

***************************************

cd "H:\Meta-analysis"
use ipdmetan_example_logit, clear

ipdmetan, study(trialid) nograph : logit y trt
ipdmetan, study(trialid) nograph interaction : logit y trt##c.stage

* Use Mantel-Haenszel weighting with metan
collapse (count) total=y (sum) events=y, by(trialid trt)
reshape wide total events, i(trialid) j(trt)
gen nonevents0 = total0 - events0
gen nonevents1 = total1 - events1
metan events1 nonevents1 events0 nonevents0, nograph label(namevar=trialid)
clonevar _NN=_SS
replace _ES = log(_ES)
rename _selogES _seES

* forestplot
admetan _ES _seES, eform(RR) study(trialid)

* what happens to existing vars if metan is run again
metan _ES _seES, eform nograph label(namevar=trialid)
* _ES and _seES have disappeared
* _LCI, _UCI and _WT have been deleted and recalculated (but that's OK)
* how to deal with this possibility in ipdmetan??




* Create simulated example dataset for ipdmetan
* (following Viechtbauer but modified for survival analysis)

* Updated Feb 2014

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

* "Disease stage" allocation
gen stage=.
gen stagetmp=runiform() /*if trialid==`i'*/
replace stage=-.08 if stagetmp<1/3 /*& trialid==`i'*/
replace stage=0 if stagetmp>=1/3 & stagetmp<2/3 /*& trialid==`i'*/
replace stage=.08 if stagetmp>=2/3 /*& trialid==`i'*/
drop stagetmp

* Generate survival data
gen drug=.
gen tcens=.
gen fail=.
forvalues i=1/`=k' {
	* Treatment allocation
	gen drugtmp=round(runiform()) if trialid==`i'				// equal allocation within trials
	replace drug=drugtmp if trial==`i'
	
	* Generate effect size for trial i from N(mu, tausq)
	* PORT is roughly mu=0.16, tausq=0.05
	* Try mu=0.4, tausq=0.15
	scalar effectsize=rnormal(0.4, `=sqrt(0.15)')
	gen lambda = (scalar(effectsize)+stage) * drug if trialid==`i'	// incorporate stage to create interaction
	gen t=(-ln(runiform())/exp(lambda)) if trialid==`i'				// exponential

	* Censorship: assume constant rate of ~0.5
	gen cens=-ln(uniform())/0.5 if trialid==`i'
	egen tcenstmp=rowmin(t cens) if trialid==`i'
	replace tcens=tcenstmp if trialid==`i'
	replace fail=(t<=cens) if trialid==`i'
	drop drugtmp lambda t cens tcenstmp
}

recode stage (-.08=0) (0=1) (.08=2)
* save example02aug

* gen subgroup=inlist(trialid, 1, 2, 7, 8, 9)
* save example19feb

save example17feb

***

cd "H:\Meta-analysis\Stata Journal submission"

use example17feb, clear

recode trialid (3=1) (4=2) (5=3) (7=4) (10=5) (1=6) (2=7) (6=8) (8=9) (9=10)
gen region = 1 + (trialid>5)
label define region_ 1 "Europe" 2 "North America"
label values region region_

label define trialid_ 1 "London" 2 "Paris" 3 "Amsterdam" 4 "Stockholm" 5 "Madrid" ///
	6 "New York" 7 "Chicago" 8 "Los Angeles" 9 "Toronto" 10 "College Station, TX"
label values trialid trialid_
label variable trialid "Trial name"

replace stage=. if inlist(trialid, 3, 5, 9)

gen sex=runiform()
replace sex=(sex<0.5)

gen age=rnormal(50, 7.5)

rename drug trt

stset tcens, fail(fail)


*************

* SJ article examples
* Basic use
ipdmetan, study(trialid) hr by(region) forest(favours(Favours treatment # Favours control) texts(80)) : stcox trt, strata(sex)

* Treatment-covariate interactions
ipdmetan, study(trialid) interaction hr keepall forest(favours("Favours greater treatment effect" "with higher disease stage" ///
	# "Favours greater treatment effect" "with lower disease stage") boxsca(200)) : stcox trt##c.stage

* Random effects
ipdmetan, study(trialid) nograph re : stcox trt
ipdmetan, study(trialid) nograph re(q) : stcox trt
ipdmetan, study(trialid) nograph re(gamma) : stcox trt

* Aggregate data
qui ipdmetan, study(trialid) hr nograph nooverall nosubgroup saving(region2.dta) by(region) : stcox trt if region==2, strata(sex)
clonevar _STUDY = trialid
ipdmetan, study(_STUDY) hr ad(region2.dta, vars(_ES _seES) npts(_NN) byad) nooverall : stcox trt if region==1, strata(sex)

* Figure 3
ipdmetan (u[1,1]/V[1,1]) (1/sqrt(V[1,1])), study(trialid) rcols((u[1,1]) %5.2f "o-E(o)" (V[1,1]) %5.1f "V(o)") ///
	by(region) plotid(region) hr ///
	forest(nooverall nostats nowt box1opts(mcolor(red)) ci1opts(lcolor(red)) box2opts(mcolor(blue)) ci2opts(lcolor(blue)) xlabel(.5 1 2) ///
	favours(Favours treatment # Favours control) fp(1)) : sts test trt, mat(u V)



 
 
 **********************
 
 * Simulation -- check setup is correct
 
 program ipdmetasim

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
	
	* "Disease stage" allocation
	gen stage=.
	gen stagetmp=runiform() /*if trialid==`i'*/
	replace stage=-.08 if stagetmp<1/3 /*& trialid==`i'*/
	replace stage=0 if stagetmp>=1/3 & stagetmp<2/3 /*& trialid==`i'*/
	replace stage=.08 if stagetmp>=2/3 /*& trialid==`i'*/
	drop stagetmp

	* Generate survival data
	gen drug=.
	gen tcens=.
	gen fail=.
	forvalues i=1/`=k' {
		* Treatment allocation
		gen drugtmp=round(runiform()) if trialid==`i'				// equal allocation within trials
		replace drug=drugtmp if trial==`i'
		
		* Generate effect size for trial i from N(mu, tausq)
		* mu=0.16
		* tausq=0.05 (similar to PORT)
		scalar effectsize=rnormal(0.16, `=sqrt(0.05)')
		gen lambda = (scalar(effectsize)+stage) * drug if trialid==`i'
		gen t=(-ln(runiform())/exp(lambda)) if trialid==`i'			// exponential

		* Censorship rate is ~0.5 (assume constant for now)
		gen cens=-ln(uniform())/0.5 if trialid==`i'
		egen tcenstmp=rowmin(t cens) if trialid==`i'
		replace tcens=tcenstmp if trialid==`i'
		replace fail=(t<=cens) if trialid==`i'
		drop drugtmp lambda t cens tcenstmp
	}
	
	recode stage (-.08=0) (0=1) (.08=2)

	stset tcens, fail(fail)
	ipdmetan, study(trialid) nograph : stcox drug
	* ipdmetan, study(trialid) nograph interaction : stcox drug##c.stage
end

simulate mu_hat=r(mu_hat) se_mut_hat=r(se_mu_hat) tausq=r(tausq), reps(100) : ipdmetasim
* simulate mu_hat=r(mu_hat) se_mut_hat=r(se_mu_hat), reps(100) saving(check2) : ipdmetasim




***************************
***  Validation script  ***
***************************

// Compiled by David Fisher
// for validating  -metan- v4.05  29nov2021
// [and previously -metan- v4.04  16aug2021
// [and previously -metan- v4.03  28apr2021
// [and previously -metan- v4.02  23feb2021
// [and previously -metan- v4.01  10feb2021
// [and previously -metan- v4.00  25nov2020
// [and previously -metan- v3.9 (beta)  05nov2020
// [and previously -metan- v3.8 (beta)  23sep2020
// [and previously -metan- v3.7 (beta)  04jul2020]

** Validation file 2: Functionality (including advanced methods)
// - comparison with -metaan-
// - test of other functionality not covered by (1), espescially if not possible to compare with metan9
// - compare with publications if possible, particularly for iterative/complex methods
// - also include zero-cell / Sweeting methods here
// - also missing-data subgroup example provided by Richard Riley


global Date = subinstr("$S_DATE", " ", "", .)

// global BaseDir2  `"S:/MRCCTU_Methodology/Software/Meta-analysis/metan/Validation/2. Functionality"'
global Datasets `"$BaseDir2/Example datasets"'
global Graphs   `"$BaseDir2/Graphs_${Date}"'

// cap log close
log using `"$BaseDir2/metan_test_${Date}_log"', name(functionality)
loc starttime "$S_TIME"

nois di _n(3)
nois di `"Current directory:"' c(pwd)
nois di `"File run: `logname'.do"'
nois di `"Date file produced: $S_DATE $S_TIME"'
nois di `"--------------------------------------"'
nois di _n(3)


** Check that we're using the correct version
// should be -metan9- v3.04 (i.e. the version currently available via SSC)
which metan
which metan9



*******************************
* Part 1: Basic functionality *
*******************************


*** Simple comparisons of -metan- with -metan9- and -metaan-
use "$Datasets/metan_example_data", clear
gen double lnOR = ln(OR)
gen double lnORlci = ln(ORlci)
gen double lnORuci = ln(ORuci)
gen double selnOR = (lnORuci - lnORlci)/(2*invnorm(.975))
gen ntotal = tsample + csample

// Fixed-effects inverse-variance
metan  lnOR selnOR, nograph
metaan lnOR selnOR, fe
metan9 lnOR selnOR, nograph

// DerSimonian-Laird random-effects
metan  lnOR selnOR, nograph re
metan  lnOR selnOR, nograph re(dl)
metaan lnOR selnOR, dl
metan9 lnOR selnOR, nograph random

// Maximum likelihood
metan  lnOR selnOR, nograph re(ml)
metaan lnOR selnOR, ml
	
// Profiled ML
metan  lnOR selnOR, nograph re(pl)
metaan lnOR selnOR, pl
	
// REML
metan lnOR selnOR, nograph re(reml)
metaan lnOR selnOR, reml
// N.B. Hsq/Isq CIs different; metaan uses test-based CIs; admetan uses likelihood profiling

// All random-effects options
// (test that they run successfully, not necessarily that results are correct)
// Some models require patient numbers, hence "cap nois"
local modellist fe dl dlb he mp ml reml hm b0 bp sj2s dk2s hksj pl kr bt hc ivhet mult
foreach model of local modellist {
	cap nois metan lnOR selnOR, nogr model(`model')
}

// All-in-one, incuding models requiring additional options
local modellist2 = subinstr("`modellist'", " ", " \ ", .)
metan lnOR selnOR, nogr model(`modellist2' \ pl, bart \ pl, skov) npts(ntotal) summaryonly



*** Using the study() and by() options
// including string vs numeric, and handling of missing/non-integer values

// "by" option, missing values
metan lnOR selnOR, nograph study(id) by(type_study)
gen type2 = type_study
replace type2 = "" if type_study=="2: Case control"
metan lnOR selnOR, nograph study(id) by(type2)
metan lnOR selnOR, nograph study(id) by(type2, m)

label define type_ 1 "1: Cohort Studies" 2 "2: Case control"
encode type_study, gen(type3) label(type_)
metan lnOR selnOR, nograph study(id) by(type3)

replace type3=. if type_study=="2: Case control"
metan lnOR selnOR, nograph study(id) by(type3)
metan lnOR selnOR, nograph study(id) by(type3, m)

replace type3=.a if type_study=="2: Case control"
metan lnOR selnOR, nograph study(id) by(type3)
metan lnOR selnOR, nograph study(id) by(type3, m)

replace type3=_pi if type_study=="2: Case control"
metan lnOR selnOR, nograph study(id) by(type3)
metan lnOR selnOR, nograph study(id) by(type3, m)

replace type3=10000 if type_study=="2: Case control"
metan lnOR selnOR, nograph study(id) by(type3)
metan lnOR selnOR, nograph study(id) by(type3, m)

replace type3=2 if type_study=="2: Case control"
replace type3=. in 16/18
metan lnOR selnOR, nograph study(id) by(type3)
metan lnOR selnOR, nograph study(id) by(type3, m)

gen id2 = id
replace id2="" in 16/18
metan lnOR selnOR, nograph study(id2) by(type3)
metan lnOR selnOR, nograph study(id2, m) by(type3)
metan lnOR selnOR, nograph study(id2) by(type3, m)
metan lnOR selnOR, nograph study(id2, m) by(type3, m)

drop id2
encode id, gen(id2)
replace id2=.b in 16/18
metan lnOR selnOR, nograph study(id2) by(type3)
metan lnOR selnOR, nograph study(id2, m) by(type3)
metan lnOR selnOR, nograph study(id2) by(type3, m)
metan lnOR selnOR, nograph study(id2, m) by(type3, m)

replace id2=_pi in 16/18
metan lnOR selnOR, nograph study(id2) by(type3)
metan lnOR selnOR, nograph study(id2, m) by(type3)
metan lnOR selnOR, nograph study(id2) by(type3, m)
metan lnOR selnOR, nograph study(id2, m) by(type3, m)

replace id2=100000 in 16/18
metan lnOR selnOR, nograph study(id2) by(type3)
metan lnOR selnOR, nograph study(id2, m) by(type3)
metan lnOR selnOR, nograph study(id2) by(type3, m)
metan lnOR selnOR, nograph study(id2, m) by(type3, m)



*** Glitch previously identified by Richard Riley
// to do with missing data in single-study subgroups
// also tests cumulative & influence for 2/3-var syntax
egen subgroup = group(type_study)
replace subgroup = 3 in 1
replace lnOR = . in 1

metan lnOR selnOR, study(id) re(reml, hksj) by(subgroup) nograph
metan lnOR selnOR, study(id) re(reml, hksj) by(subgroup) nograph keepall keeporder
metan lnOR selnOR, study(id) re(reml, hksj) by(subgroup) nograph keepall keeporder influence
metan lnOR selnOR, study(id) re(reml, hksj) by(subgroup) nograph keepall keeporder cumulative



*** Proportion functionality
metan cdeath csample, proportion study(id)
metan cdeath csample, proportion study(id) nograph ftt					// -metaprop- syntax, for backwards compatibility
metan cdeath csample, proportion study(id) nograph transform(ftukey)	// new -metan- syntax
metan cdeath csample, proportion study(id) nograph transform(arcsine)
metan cdeath csample, proportion study(id) nograph transform(logit)

// with cumulative/influence
metan cdeath csample, proportion study(id) cumulative
metan cdeath csample, proportion study(id) influence
metan cdeath csample, proportion study(id) transform(ftukey) cumulative
metan cdeath csample, proportion study(id) transform(ftukey) influence

// denominator
metan cdeath csample, proportion study(id) denominator(1000)

// continuity correction
// (for proportions; more general cc testing below)
gen int cdeath2 = cdeath
replace cdeath2 = 0 in 1/5
metan cdeath2 csample, proportion study(id) nograph
metan cdeath2 csample, proportion study(id) nograph transform(ftukey)
metan cdeath2 csample, proportion study(id) nograph transform(arcsine)
metan cdeath2 csample, proportion study(id) nograph transform(logit)

metan cdeath2 csample, proportion study(id) nograph nocc
metan cdeath2 csample, proportion study(id) nograph nocc transform(ftukey)
metan cdeath2 csample, proportion study(id) nograph nocc transform(arcsine)
metan cdeath2 csample, proportion study(id) nograph nocc transform(logit)

cap nois metan cdeath2 csample, proportion study(id) nograph cc(.1, opposite)		// only with 2x2, not proportion
cap nois metan cdeath2 csample, proportion study(id) nograph cc(.1, empirical)		// only with 2x2, not proportion

// nointeger with proportions
replace cdeath2 = cdeath + 0.5
cap nois metan cdeath2 csample, proportion study(id) nograph
metan cdeath2 csample, proportion study(id) nograph nointeger


*** cumulative, influence
// with proportion and 4/6-var syntax
metan cdeath csample, proportion study(id) nograph cumulative by(type_study)
metan cdeath csample, proportion study(id) nograph influence by(type_study)

metan tdeath tnodeath cdeath cnodeath, nograph cumulative by(type_study)
metan tdeath tnodeath cdeath cnodeath, nograph influence by(type_study)

metan tsample tmean tsd csample cmean csd, nograph cumulative by(type_study)
metan tsample tmean tsd csample cmean csd, nograph influence by(type_study)




*****************************
* Part 2: Iterative methods *
*****************************


*** Skovgaard and Bartlett corrections to the profiled likelihood

// Source: Appendix of Guolo SMMR 2017
use "$Datasets/cooper", clear
gen v = se^2
list
metan smd se, nograph model(dl \ ml \ reml)
mat list r(ovstats)
// N.B. In the Appendix, the REML and ML *rows* are erroneously switched;
//  as are the Std. Err. and Heterogeneity *colums*

// p-values
// (also given in Table 4)
metan smd se, nograph model(dl \ hksj \ ml \ pl, z maxtausq(1000) \ pl, skov maxtausq(1000) \ pl, bart maxtausq(1000) \ reml)
mat list r(ovstats)


// Source: Section 5 "Illustrations" in Guolo & Varin J Stat Software 2012 (metaLik)
use "$Datasets/diuretics", clear
gen s = sqrt(v)
list
metan y s, nograph model(ml \ pl, z \ pl, skov)
mat list r(ovstats)


// Source: Table I of Noma StatMed 2011
use "$Datasets/teo", clear
metan tdeath tnodeath cdeath cnodeath, nograph or model(fe \ dl \ pl \ pl, bart)
mat list r(ovstats)
// Note: Noma's derivation of the Bartlett correction is slightly different to that of Huizinga et al
// Hence my result here is not quite the same as in Noma.



*** Henmi-Copas model

// Source: Section 5 "Example" in Henmi & Copas Stat Med 2010
use "$Datasets/lee", clear
metan tevent tnoevent cevent cnoevent, nograph or model(dl \ bt \ pl \ reml \ hksj \ hc)
mat list r(ovstats)



*** Doi IVHet and Quality Effects

// Source: Thyroid MA data used by Doi et al in their Epidemiology 2008 paper
use "$Datasets/doi", clear
metan e_treat f_treat e_ctrl f_ctrl, label(namevar=Study, yearvar=Year) by(Subgroup) sgwt rr nograph cc(.25) ///
	model(iv \ re \ qe, qwt(Qi))
mat list r(ovstats)


// Source: Data from MA by Verhagen et al used by Doi et al in their Letter to Epidemiology 2009
//  (Quality scores and ORs taken from Verhagen Int J Tech Ass Health Care 2002;
//    count data taken and O-E, V from Yusuf European Heart Journal 1985)
use "$Datasets/verhagen", clear
gen lnOR = ln(OR)
gen lnORlci = ln(lci)
gen lnORuci = ln(uci)

global ver_opts `"label(namevar=Study, yearvar=Year) by(Subgroup) sgwt or nograph"'
metan e_treat f_treat e_ctrl f_ctrl, $ver_opts peto					// Yusuf count data; Peto method
metan lnOR lnORlci lnORuci, $ver_opts fe							// Verhagen OR+CI data; FE
metan lnOR lnORlci lnORuci if Subgroup==1, $ver_opts qe(Delphi)		// Verhagen OR+CI data; QE (IV subgroup only)


// Source: Data from a MA by Mozurkewich et al (Obstet Gynecol 2000)
//   used by Doi in their "Empirical example", CCT 2011
// (Quality scores given in Table 1 of Doi CCT 2011)
use "$Datasets/mozurkewich", clear
metan e_treat f_treat e_ctrl f_ctrl, study(Study) nograph or cc(.25) ///
	model(iv \ re \ qe, qwt(Qi))
mat list r(ovstats)


// Source: Magnesium data used by Doi in the IVHet and QE papers (both CCT 2015)
//  (Quality scores taken from the supplementary material to Al Khalaf JCE 2011)
// Figure 3a, b, c
use "$Datasets/magnesium", clear
metan e_treat f_treat e_ctrl f_ctrl, study(Study) nograph or ///
	model(ivhet \ qe, qwt(Qi) \ re)
mat list r(ovstats)




*** Biggerstaff-Tweedie model

// Source: Biggerstaff & Tweedie, Stat Med 1997; Collins example (tables II and III)
use "$Datasets/collins", clear
list

// Table II: Estimates of CI for tausq
// Note: "MM" = model(bt); "ALR" = model(pl)
metan tevent tnoevent cevent cnoevent, study(study) or nograph model(bt \ pl)
mat list r(ovstats)

// Table III: Estimates of beta
metan tevent tnoevent cevent cnoevent, study(study) or nograph model(iv \ dl \ bt)
mat list r(ovstats)



// Source: Biggerstaff & Tweedie, Stat Med 1997; Mengersen example (tables V and VI)
use "$Datasets/mengersen", clear
gen logrr = ln(rr)
gen loglci = ln(lci)
gen loguci = ln(uci)

// Table V: Estimates of CI for tausq
// Note: "MM" = model(bt); "ALR" = model(pl)
metan logrr loglci loguci, study(study) rr nograph model(bt \ pl)
mat list r(ovstats)

// Table VI: Estimates of beta
metan logrr loglci loguci, study(study) rr nograph model(iv \ dl \ bt)
mat list r(ovstats)




**********************************************************
* Part 3: Continuity correction, zero cells, proportions *
**********************************************************

*** Continuity correction work by Sweeting
// All data taken from Sweeting, Stat Med 2004

// Table V
clear
set obs 5
gen byte Study = _n
gen byte tevent = 0
gen int tnoevent = 100
gen byte cevent = 1
gen int cnoevent = .

replace cnoevent = 99 in 1
replace cnoevent = 199 in 2
replace cnoevent = 399 in 3
replace cnoevent = 799 in 4
replace cnoevent = 1599 in 5

metan tevent tnoevent cevent cnoevent, or nograph nooverall
metan tevent tnoevent cevent cnoevent, or nograph nooverall cc(, opposite)

// For empirical, add a dummy study with an odds ratio of 0.5
expand 2 if _n==_N
replace tevent=10 in L
replace cnoevent=5 in L
metan tevent tnoevent cevent cnoevent, or nograph nooverall cc(, empirical)

// Table VII
use "$Datasets/sweeting_table7", clear
metan tevent tnoevent cevent cnoevent, or nograph peto						// Peto
metan tevent tnoevent cevent cnoevent, or nograph mh cc(.5)					// MH with cc=0.5
metan tevent tnoevent cevent cnoevent, or nograph mh cc(.5, opposite)		// MH with opposite trt-arm cc
metan tevent tnoevent cevent cnoevent, or nograph mh cc(.5, empirical)		// MH with empirical cc
metan tevent tnoevent cevent cnoevent, or nograph mh cc(.005)				// MH with cc=0.005
metan tevent tnoevent cevent cnoevent, or nograph mh cc(.005, opposite)		// MH with opposite trt-arm cc
metan tevent tnoevent cevent cnoevent, or nograph mh cc(.005, empirical)	// MH with empirical cc
metan tevent tnoevent cevent cnoevent, or nograph iv cc(.5)					// IV with cc=0.5
metan tevent tnoevent cevent cnoevent, or nograph iv cc(.5, opposite)		// IV with opposite trt-arm cc
metan tevent tnoevent cevent cnoevent, or nograph iv cc(.5, empirical)		// IV with empirical cc
metan tevent tnoevent cevent cnoevent, or nograph iv cc(.005)				// IV with cc=0.005
metan tevent tnoevent cevent cnoevent, or nograph iv cc(.005, opposite)		// IV with opposite trt-arm cc
metan tevent tnoevent cevent cnoevent, or nograph iv cc(.005, empirical)	// IV with empirical cc

// original analysis
keep if _rsample==1
collapse (sum) tevent tnoevent cevent cnoevent
metan tevent tnoevent cevent cnoevent, or nogr			// ??  can't recreate original analysis... obtain paper??

// original analysis (2)
use "$Datasets/sweeting_table7", clear
collapse (sum) tevent tnoevent cevent cnoevent
metan cevent cnoevent tevent tnoevent, or nogr			// need to keep *all* observations AND switch treatment and control!!  Sweeting et al must have missed this


// Table VIII
use "$Datasets/sweeting_table8", clear
metan tevent tnoevent cevent cnoevent, or nograph peto						// Peto
metan tevent tnoevent cevent cnoevent, or nograph mh cc(.5)					// MH with cc=0.5
metan tevent tnoevent cevent cnoevent, or nograph mh cc(.5, opposite)		// MH with opposite trt-arm cc
metan tevent tnoevent cevent cnoevent, or nograph mh cc(.5, empirical)		// MH with empirical cc
metan tevent tnoevent cevent cnoevent, or nograph mh cc(.005)				// MH with cc=0.005
metan tevent tnoevent cevent cnoevent, or nograph mh cc(.005, opposite)		// MH with opposite trt-arm cc
metan tevent tnoevent cevent cnoevent, or nograph mh cc(.005, empirical)	// MH with empirical cc
metan tevent tnoevent cevent cnoevent, or nograph iv cc(.5)					// IV with cc=0.5
metan tevent tnoevent cevent cnoevent, or nograph iv cc(.5, opposite)		// IV with opposite trt-arm cc
metan tevent tnoevent cevent cnoevent, or nograph iv cc(.5, empirical)		// IV with empirical cc
metan tevent tnoevent cevent cnoevent, or nograph iv cc(.005)				// IV with cc=0.005
metan tevent tnoevent cevent cnoevent, or nograph iv cc(.005, opposite)		// IV with opposite trt-arm cc
metan tevent tnoevent cevent cnoevent, or nograph iv cc(.005, empirical)	// IV with empirical cc



*** More zero-cell testing

use "$Datasets/sweeting_table7", clear
gen byte subgroup=inlist(_n,2,12,14,19,21,23)
metan tevent tnoevent cevent cnoevent, or nograph model(iv, cc(0) \ iv \ mh) by(subgroup)

metan tevent tnoevent cevent cnoevent, by(subgroup) nogr iv cc(0) study(Study) cumulative study(Study)
metan tevent tnoevent cevent cnoevent if subgroup==1, nogr iv cc(0) study(Study) cumulative study(Study)
metan tevent tnoevent cevent cnoevent, by(subgroup) nogr keeporder iv cc(0) study(Study) cumulative study(Study)
metan tevent tnoevent cevent cnoevent if subgroup==1, nogr keeporder iv cc(0) study(Study) cumulative study(Study)

clear
input str19 author byte(e1 f1 n1 e0 f0 n0)
"EXTRA1 (nonevent00)" 64  0 64 69  0 69
"EXTRA2 (crossed00)"  64  0 64  0 69 69
"EXTRA3 (crossed00)"   0 64 64 69  0 69
"EXTRA4 (single0)"    66  0 66  7 57 64
"EXTRA5 (single0)"     3 63 66  0 64 64
"EXTRA6 (single0)"     3 63 66 64  0 64
end
tempfile extra
save `extra'


use "$Datasets/nielweise2007", clear
gen int f1 = n1-e1
gen int f0 = n0-e0
append using `extra'

metan e1 f1 e0 f0, study(author) nograph or
metan e1 f1 e0 f0, study(author) nograph rr
metan e1 f1 e0 f0, study(author) nograph rd

metan e1 f1 e0 f0, study(author) nograph or nocc
metan e1 f1 e0 f0, study(author) nograph rr nocc
metan e1 f1 e0 f0, study(author) nograph rd nocc

gen byte subgroup = inrange(_n, 13, 24)
metan e1 f1 e0 f0, study(author) nograph or by(subgroup)
metan e1 f1 e0 f0, study(author) nograph rr by(subgroup)
metan e1 f1 e0 f0, study(author) nograph rd by(subgroup)



*** Proportions
// using data from Schwarzer et al, RSM 2019

clear
input str8 id int events long total
"Study 1"  422 217154
"Study 10"  32  16557
"Study 13"   1    676
"Study 18"   1     44
"Study 26"   1     29
end

// Table 1
metan events total, proportion study(id) nograph transform(arcsine) nopr
metan events total, proportion study(id) nograph transform(arcsine) denom(1000)

metan events total, proportion study(id) nograph transform(ftukey) nopr
metan events total, proportion study(id) nograph transform(ftukey) denom(1000)

metan events total, proportion study(id) nograph transform(logit) nopr
metan events total, proportion study(id) nograph transform(logit) denom(1000)

metan events total, proportion study(id) nograph transform(arcsine) model(dl) nopr
metan events total, proportion study(id) nograph transform(arcsine) model(dl) denom(1000)

metan events total, proportion study(id) nograph transform(ftukey) model(dl) nopr
metan events total, proportion study(id) nograph transform(ftukey) model(dl) denom(1000)

metan events total, proportion study(id) nograph transform(logit) model(dl) nopr
metan events total, proportion study(id) nograph transform(logit) model(dl) denom(1000)

metan events total, proportion study(id) nograph transform(arcsine) model(reml) nopr
metan events total, proportion study(id) nograph transform(ftukey) model(reml) nopr
metan events total, proportion study(id) nograph transform(logit) model(reml) nopr


// Figure 1
metan events total, proportion study(id) counts transform(ftukey) nopr model(fe \ dl) xlabel(.025 .05 .075, force)

// Figure 2
metan events total, proportion study(id) counts transform(ftukey) denom(1000) model(fe \ dl) xlabel(0(5)15, force)

// Figure 4
metan events total, proportion study(id) counts transform(logit) denom(1000) model(fe \ dl) xlabel(0(5)15, force)


*** NEW FEB 2021 ***

// Barendregt-Doi back-transform
metan events total, proportion study(id) nograph transform(ftukey, arithmetic) denom(1000)
metan events total, proportion study(id) nograph transform(ftukey, arithmetic) denom(1000) model(dl)
metan events total, proportion study(id) nograph transform(ftukey, arithmetic) denom(1000) model(reml)
metan events total, proportion study(id) nograph transform(ftukey, arithmetic) denom(1000) model(ivhet)

metan events total, proportion study(id) nograph transform(ftukey, ivariance) denom(1000)
metan events total, proportion study(id) nograph transform(ftukey, ivariance) denom(1000) model(dl)
metan events total, proportion study(id) nograph transform(ftukey, ivariance) denom(1000) model(reml)
metan events total, proportion study(id) nograph transform(ftukey, ivariance) denom(1000) model(ivhet)

metan events total, proportion study(id) counts transform(ftukey, ivariance) denom(1000) model(fe \ dl \ ivhet) xlabel(0(5)15, force)




************************************************

log close functionality


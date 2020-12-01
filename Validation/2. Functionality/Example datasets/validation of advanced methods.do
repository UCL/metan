****************************
* Validation of MA methods *
****************************


cd "S:\MRCCTU_Methodology\Software\ipdmetan\Validation\Advanced methods"



*** Skovgaard and Bartlett corrections to the profiled likelihood

// Source: Appendix of Guolo SMMR 2017
use cooper, clear
gen v = se^2
list
foreach model in dl ml reml {
	qui metan smd se, nograph model(`model')
	mat list r(ovstats)
}
// N.B. In the Appendix, the REML and ML *rows* are erroneously switched;
//  as are the Std. Err. and Heterogeneity *colums*

// p-values
// (also given in Table 4)
foreach model in dl hk ml plz plskov plbart reml {
	local model2 `model'
	if "`model'"=="plz" local model2 "pl, z maxtausq(1000)"
	else if "`model'"=="plskov" local model2 "pl, skov maxtausq(1000)"
	else if "`model'"=="plbart" local model2 "pl, bart maxtausq(1000)"
	qui metan smd se, nograph model(`model2')
	mat ovstats = r(ovstats)
	disp "`model': {col 15}" _c
	disp %07.5f ovstats[rownumb(ovstats, "pvalue"), 1]
}

// Source: Section 5 "Illustrations" in Guolo & Varin J Stat Software 2012 (metaLik)
use diuretics, clear
gen s = sqrt(v)
list
metan y s, nograph model(ml)
foreach model in ml plz plskov {
	local model2 `model'
	if "`model'"=="plz" local model2 "pl, z"
	else if "`model'"=="plskov" local model2 "pl, skov"
	qui metan y s, nograph model(`model2')
	mat list r(ovstats)
}

// Source: Table I of Noma StatMed 2011
use teo, clear
metan tdeath tnodeath cdeath cnodeath, nograph or model(fe) summaryonly
metan tdeath tnodeath cdeath cnodeath, nograph or model(dl) summaryonly
metan tdeath tnodeath cdeath cnodeath, nograph or model(pl) summaryonly
metan tdeath tnodeath cdeath cnodeath, nograph or model(pl, bart) summaryonly
// Note: Noma's derivation of the Bartlett correction is slightly different to that of Huizinga et al
// Hence my result here is not quite the same as in Noma.



*** Henmi-Copas model

// Source: Section 5 "Example" in Henmi & Copas Stat Med 2010
use lee, clear
foreach model in dl bt pl reml hk hc {
	metan tevent tnoevent cevent cnoevent, nograph log or model(`model') summaryonly
}
*km http://www.metafor-project.org/doku.php/analyses:henmi2010 10/11
// km metan tevent tnoevent cevent cnoevent, nograph log or model(dl) summaryonly
* km checks ok fr dl option 


*** Doi IVHet and Quality Effects

// Source: Thyroid MA data used by Doi et al in their Epidemiology 2008 paper
use doi, clear
global doi_opts `"label(namevar=Study, yearvar=Year) by(Subgroup) sgwt rr nograph cc(.25)"'
metan e_treat f_treat e_ctrl f_ctrl, $doi_opts iv fe			// Fixed-effects
metan e_treat f_treat e_ctrl f_ctrl, $doi_opts iv re			// Random-effects
metan e_treat f_treat e_ctrl f_ctrl, $doi_opts iv qe(Qi)		// Quality effects


*km10/11
metan e_treat f_treat e_ctrl f_ctrl, $doi_opts fe 	

// Source: Data from MA by Verhagen et al used by Doi et al in their Letter to Epidemiology 2009
//  (Quality scores and ORs taken from Verhagen Int J Tech Ass Health Care 2002;
//    count data taken and O-E, V from Yusuf European Heart Journal 1985)
use verhagen, clear
gen lnOR = ln(OR)
gen lnORlci = ln(lci)
gen lnORuci = ln(uci)

global ver_opts `"label(namevar=Study, yearvar=Year) by(Subgroup) sgwt or nogr"'
metan e_treat f_treat e_ctrl f_ctrl, $ver_opts peto				// Yusuf count data; Peto method
metan lnOR lnORlci lnORuci, $ver_opts fe							// Verhagen OR+CI data; FE
metan lnOR lnORlci lnORuci if Subgroup==1, $ver_opts qe(Delphi)	// Verhagen OR+CI data; QE (IV subgroup only)


// Source: Data from a MA by Mozurkewich et al (Obstet Gynecol 2000)
//   used by Doi in their "Empirical example", CCT 2011
// (Quality scores given in Table 1 of Doi CCT 2011)
use mozurkewich, clear
gen lnOR = ln(OR)
gen lnORlci = ln(lci)
gen lnORuci = ln(uci)

metan e_treat f_treat e_ctrl f_ctrl, study(Study) nograph or cc(.25) iv fe		// Fixed-effects
metan e_treat f_treat e_ctrl f_ctrl, study(Study) nograph or cc(.25) iv re		// Random-effects
metan e_treat f_treat e_ctrl f_ctrl, study(Study) nograph or cc(.25) iv qe(Qi)	// Quality effects


// Source: Magnesium data used by Doi in the IVHet and QE papers (both CCT 2015)
//  (Quality scores taken from the supplementary material to Al Khalaf JCE 2011)
use magnesium, clear
metan e_treat f_treat e_ctrl f_ctrl, study(Study) nograph or ivh			// Fig 3a: IVhet
metan e_treat f_treat e_ctrl f_ctrl, study(Study) nograph or qe(Qi) 		// Fig 3b: QE
metan e_treat f_treat e_ctrl f_ctrl, study(Study) nograph or re			// Fig 3c: D+L RE




*** Biggerstaff-Tweedie model

// Source: Biggerstaff & Tweedie, Stat Med 1997; Collins example (tables II and III)
use collins, clear
list

// Table II: Estimates of CI for tausq
metan tevent tnoevent cevent cnoevent, study(study) or summaryonly nograph model(gamma)		// "MM"
metan tevent tnoevent cevent cnoevent, study(study) or summaryonly nograph model(pl) 		// "ALR"

// Table III: Estimates of beta
metan tevent tnoevent cevent cnoevent, study(study) or summaryonly nograph iv				// Fixed-effects
metan tevent tnoevent cevent cnoevent, study(study) or summaryonly nograph model(dl) 		// Standard random-effects
metan tevent tnoevent cevent cnoevent, study(study) or summaryonly nograph model(gamma)		// Biggerstaff-Tweedie


// Source: Biggerstaff & Tweedie, Stat Med 1997; Mengersen example (tables V and VI)
use mengersen, clear
gen logrr = ln(rr)
gen loglci = ln(lci)
gen loguci = ln(uci)

// Table V: Estimates of CI for tausq
metan logrr loglci loguci, study(study) rr summaryonly nograph model(gamma)		// "MM"
metan logrr loglci loguci, study(study) rr summaryonly nograph model(pl) 		// "ALR"

// Table VI: Estimates of beta
metan logrr loglci loguci, study(study) rr summaryonly nograph iv				// Fixed-effects
metan logrr loglci loguci, study(study) rr summaryonly nograph model(dl) 		// Standard random-effects
metan logrr loglci loguci, study(study) rr summaryonly nograph model(gamma)		// Biggerstaff-Tweedie



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
use sweeting_table7, clear
metan tevent tnoevent cevent cnoevent, or nograph peto						// Peto
metan tevent tnoevent cevent cnoevent, or nograph mh							// MH with cc=0.5
metan tevent tnoevent cevent cnoevent, or nograph mh cc(, opposite)			// MH with opposite trt-arm cc
metan tevent tnoevent cevent cnoevent, or nograph mh cc(, empirical)			// MH with empirical cc
metan tevent tnoevent cevent cnoevent, or nograph mh cc(.005)				// MH with cc=0.005
metan tevent tnoevent cevent cnoevent, or nograph mh cc(.005, opposite)		// MH with opposite trt-arm cc
metan tevent tnoevent cevent cnoevent, or nograph mh cc(.005, empirical)		// MH with empirical cc
metan tevent tnoevent cevent cnoevent, or nograph iv							// IV with cc=0.5
metan tevent tnoevent cevent cnoevent, or nograph iv cc(, opposite)			// IV with opposite trt-arm cc
metan tevent tnoevent cevent cnoevent, or nograph iv cc(, empirical)			// IV with empirical cc
metan tevent tnoevent cevent cnoevent, or nograph iv cc(.005)				// IV with cc=0.005
metan tevent tnoevent cevent cnoevent, or nograph iv cc(.005, opposite)		// IV with opposite trt-arm cc
metan tevent tnoevent cevent cnoevent, or nograph iv cc(.005, empirical)		// IV with empirical cc

keep if _rsample
collapse (sum) tevent tnoevent cevent cnoevent
metan tevent tnoevent cevent cnoevent, or nogr			// ??  can't recreate original analysis... obtain paper??


// Table VIII
use sweeting_table8, clear
metan tevent tnoevent cevent cnoevent, or nograph peto						// Peto
metan tevent tnoevent cevent cnoevent, or nograph mh							// MH with cc=0.5
metan tevent tnoevent cevent cnoevent, or nograph mh cc(, opposite)			// MH with opposite trt-arm cc
metan tevent tnoevent cevent cnoevent, or nograph mh cc(, empirical)			// MH with empirical cc
metan tevent tnoevent cevent cnoevent, or nograph mh cc(.005)				// MH with cc=0.005
metan tevent tnoevent cevent cnoevent, or nograph mh cc(.005, opposite)		// MH with opposite trt-arm cc
metan tevent tnoevent cevent cnoevent, or nograph mh cc(.005, empirical)		// MH with empirical cc
metan tevent tnoevent cevent cnoevent, or nograph iv							// IV with cc=0.5
metan tevent tnoevent cevent cnoevent, or nograph iv cc(, opposite)			// IV with opposite trt-arm cc
metan tevent tnoevent cevent cnoevent, or nograph iv cc(, empirical)			// IV with empirical cc
metan tevent tnoevent cevent cnoevent, or nograph iv cc(.005)				// IV with cc=0.005
metan tevent tnoevent cevent cnoevent, or nograph iv cc(.005, opposite)		// IV with opposite trt-arm cc
metan tevent tnoevent cevent cnoevent, or nograph iv cc(.005, empirical)		// IV with empirical cc



*** More zero-cell testing

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


use nielweise2007.dta, clear
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



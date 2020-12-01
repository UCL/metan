* Test script for metan v4.00  21jan2020

cap log close
log using metan_test_21jan2020
loc starttime "$S_TIME"

nois di _n(3)
nois di `"Current directory:"' c(pwd)
nois di `"File run: `logname'.do"'
nois di `"Date file produced: $S_DATE $S_TIME"'
nois di `"--------------------------------------"'
nois di _n(3)


** Check that we're using the correct version
// should be -metan-  v4.00  21jan2020
//       and -metan9- v3.04 (i.e. the version currently available via SSC)
which metan
which metan9

// We proceed by taking the exact command lines from the help file and SJ article,
// only changing the command name "metan" to "metan9" (i.e. the current v3.04)

// We then change the command line minimally, to run using "metan" (i.e. the new v4.00)



***************************************
* 1. Recreate examples from metan.hlp *
***************************************

use "http://fmwww.bc.edu/repec/bocode/m/metan_example_data", clear


* Example 1
// Risk difference from raw cell counts, random effects model, "label" specification with counts displayed

metan9 tdeath tnodeath cdeath cnodeath, rd random label(namevar=id, yearid=year) counts
// Note: the current metan help file gives the label() syntax as (namevar=... , yearvar=... )
// but the *example* has "yearid=..." and this indeed is how the command is actually coded.
// The new v4.00 uses namevar, yearvar.

metan  tdeath tnodeath cdeath cnodeath, rd random label(namevar=id, yearvar=year) counts

// Note: new version does not automatically present a p-value for heterogeneity on the forest plot
// to avoid confusion with significance test of overall effect
// Hence, to *exactly* recreate the example, we need to add the option "hetstat(p)"
metan  tdeath tnodeath cdeath cnodeath, rd random label(namevar=id, yearvar=year) counts hetstat(p)

// Stata 16
gen id_year = id + " (" + string(year) + ")"
meta esize tdeath tnodeath cdeath cnodeath, esize(rd) studylab(id_year) random(dl)
meta summ
meta forest _id year _plot _data1 _data2 _esci _weight, noohet noohom noosig



* Example 2
// Sort by year, use data columns syntax.
// Text size increased, specify percentage of graph as text and two lines per study; suppress stats, weight, heterogeneity stats and table.

metan9 tdeath tnodeath cdeath cnodeath, sortby(year) lcols(id year country) rcols(population) textsize(110) astext(60) double nostats nowt nohet notable

metan  tdeath tnodeath cdeath cnodeath, sortby(year) lcols(id year country) rcols(population) forestplot(astext(60) leftjustify) nostats nowt nohet notable
// Note slight differences in syntax here, e.g. forestplot() option.
// Also, text size is handled differently by new version, and automatic sizing is better anyway, so textsize(110) is no longer valid.

// N.B. "double" is currently not available, but I can make (re-)adding it a priority.

// Stata 16
meta forest _id year country _plot population, sort(year)



* Example 3
// Analyse continuous data (6 parameter syntax), stratify by type of study, with weights summing to 100 within sub group,
// second analysis specified, display random effects distribution, show raw data counts, display "favours treatment vs. favours control" labels

metan9 tsample tmean tsd csample cmean csd, by(type_study) sgweight fixed second(random) rfdist counts label(namevar=id) ///
	favours(Treatment reduces blood pressure # Treatment increases blood pressure)

metan  tsample tmean tsd csample cmean csd, by(type_study) sgweight fixed second(random) rfdist counts label(namevar=id) ///
	forestplot(favours(Treatment reduces blood pressure # Treatment increases blood pressure))

// Stata 16
meta esize tsample tmean tsd csample cmean csd, studylab(id_year) random(dl)
meta summ, subgroup(type_study)
meta summ, predinterval
meta forest, subgroup(type_study) xtitle(Treatment reduces blood pressure   Treatment increases blood pressure)
meta forest, predinterval xtitle(Treatment reduces blood pressure   Treatment increases blood pressure)
// Cannot specify multiple models (fixed + random) with meta
// Cannot combine prediction intervals with subgroups with meta



* Example 4
// Generate log odds ratio and standard error, analyse with 2 parameter syntax.
// Graph has exponential form, scale is forced within set limits and ticks added, effect label specified.
gen logor = ln( (tdeath*cnodeath)/(tnodeath*cdeath) )
gen selogor = sqrt( (1/tdeath) + (1/tnodeath) + (1/cdeath) + (1/cnodeath) )

metan9 logor selogor, eform xlabel(0.5, 1, 1.5, 2, 2.5) force xtick(0.75, 1.25, 1.75, 2.25) effect(Odds ratio)

metan  logor selogor, or forestplot(xlabel(0.5 1 1.5 2 2.5, force) xtick(0.75 1.25 1.75 2.25))
// Note that xlabel() and xtick() now expect a standard Stata -numlist-  (i.e. not comma-separated)
// ...and "force" is now a sub-option of xlabel() rather than a stand-alone option

// Note: new version does not automatically present a p-value for heterogeneity on the forest plot
// to avoid confusion with significance test of overall effect
// Hence, to *exactly* recreate the example, we need to add the option "hetstat(p)"
metan  logor selogor, or forestplot(xlabel(0.5 1 1.5 2 2.5, force) xtick(0.75 1.25 1.75 2.25) hetstat(p))

// Stata 16
meta set logor selogor, studylab(id_year) random(dl)
meta summ, eform(Odds ratio)
meta forest, eform(Odds ratio) xtick(0.75 1.25 1.75 2.25) crop(.5 2.5)



* Example 5
// Display diagnostic test data with 3 parameter syntax.
// Weight is number of positive diagnoses, axis label set and null specified at 50%.
// Overall effect estimate is not displayed, graph for visual examination only.

metan9 percent lowerci upperci, wgt(n_positives) label(namevar=id) nooverall notable ///
	xlabel(0,10,20,30,40,50,60,70,80,90,100) force null(50) title(Sensitivity, position(6))

metan  percent lowerci upperci, wgt(n_positives) label(namevar=id) nooverall notable nowt ///
	forestplot(xlabel(0(10)100, force) null(50) title(Sensitivity, position(6)))
// Note that xlabel() now expects a standard Stata -numlist-
// ...and "force" is now a sub-option of xlabel() rather than a stand-alone option

// Stata 16
// Cannot easily do this, as percent confidence limits are asymmetrical by definition, but meta will not accept them.
// Would need to use a normalising transform (e.g. arcsine?).
// Also: Cannot use user-specified weights with meta



* Example 6
// User has analysed data with a non-standard technique and supplied effect estimates, weights and description of statistics.
// The scheme "Economist" has been used.
metan9 OR ORlci ORuci, wgt(bweight) first(0.924 0.753 1.095 Bayesian) firststats(param V=3.86, p=0.012) label(namevar=id) ///
	xlabel(0.25, 0.5, 1, 2, 4) force null(1) aspect(1.2) scheme(economist)

metan  OR ORlci ORuci, wgt(bweight) first(0.924 0.753 1.095 Bayesian) firststats(param V=3.86, p=0.012) label(namevar=id) ///
	forestplot(xlabel(0.25 0.5 1 2 4, force) null(1) aspect(1.2) scheme(economist))
// Note that xlabel() now expects a standard Stata -numlist-
// ...and "force" is now a sub-option of xlabel() rather than a stand-alone option

// Stata 16:
// Need to transform the OR and CI limits to make them symmetrical
// (see previous example)
gen lnOR = ln(OR)
gen lnORlci = ln(ORlci)
gen lnORuci = ln(ORuci)
meta set lnOR lnORlci lnORuci, studylab(id)
meta summ, eform(Odds ratio)
meta forest, eform(Odds ratio) customoverall(`=ln(.924)' `=ln(.753)' `=ln(1.095)', label("Bayesian, param V=3.86 p=0.012")) 
// N.B. Cannot use user-specified weights with meta



* Example 7
// Variable "counts" defined showing raw data.
// Options to change the box, effect estimate marker and confidence interval used,
// and the counts variable has been attached to the estimate marker as a label.
gen counts = ". " + string(tdeath) + "/" + string(tdeath+tnodeath) + ", " + string(cdeath) + "/" + string(cdeath+cnodeath)

metan9 tdeath tnodeath cdeath cnodeath, lcols(id year) notable ///
		boxopt( mcolor(forest_green) msymbol(triangle) ) ///
		pointopt( msymbol(triangle) mcolor(gold) msize(tiny) mlabel(counts) mlabsize(vsmall) mlabcolor(forest_green) mlabposition(1) ) ///
		ciopt( lcolor(sienna) lwidth(medium) )

metan  tdeath tnodeath cdeath cnodeath, lcols(id year) notable ///
	forestplot( ///
		boxopt( mcolor(forest_green) msymbol(triangle) ) ///
		pointopt( msymbol(triangle) mcolor(gold) msize(tiny) mlabel(counts) mlabsize(vsmall) mlabcolor(forest_green) mlabposition(1) ) ///
		ciopt( lcolor(sienna) lwidth(medium) ) ///
	)

// Note: new version does not automatically present a p-value for heterogeneity on the forest plot
// to avoid confusion with significance test of overall effect.
// Also, in this context (common effect, 2x2 binary data), the default is to present Q, not I-squared.
// Hence, to *exactly* recreate the example, we need to add the option "hetstat(isq p)"
metan  tdeath tnodeath cdeath cnodeath, lcols(id year) notable hetstat(isq p) ///
	forestplot( ///
		boxopt( mcolor(forest_green) msymbol(triangle) ) ///
		pointopt( msymbol(triangle) mcolor(gold) msize(tiny) mlabel(counts) mlabsize(vsmall) mlabcolor(forest_green) mlabposition(1) ) ///
		ciopt( lcolor(sienna) lwidth(medium) ) ///
	)

// Stata 16
meta esize tdeath tnodeath cdeath cnodeath, studylab(id)
meta forestplot _id year _plot _esci _weight, ///
	markeropts( mcolor(forest_green) msymbol(triangle) ) ///
	insidemarker( msymbol(triangle) mcolor(gold) msize(tiny) ) ///
	ciopts( lcolor(sienna) lwidth(medium) ) ///
	omarkeropts( mcolor(blue) )
// Cannot use "mlabel" options within markeropts()




**************************************************************
* 2. Recreate examples from Harris et al, Stata Journal 2008 *
**************************************************************

use bcgtrial, clear


* 4.2 Display options
* (and Figure 1)

metan9 tcases tnoncases ccases cnoncases, rr fixedi lcols(trialnam startyr) ///
	xlabel(0.1, 10) favours(BCG reduces risk of TB # BCG increases risk of TB)

metan  tcases tnoncases ccases cnoncases, rr fixedi lcols(trialnam startyr) ///
	forestplot(xlabel(0.1 1 10) favours(BCG reduces risk of TB # BCG increases risk of TB))

// Note: new version does not automatically present a p-value for heterogeneity on the forest plot
// to avoid confusion with significance test of overall effect
// Hence, to *exactly* recreate the example, we need to add the option "hetstat(p)"
metan  tcases tnoncases ccases cnoncases, rr fixedi lcols(trialnam startyr) ///
	forestplot(xlabel(0.1 10) favours(BCG reduces risk of TB # BCG increases risk of TB) hetstat(p))

// Stata 16
meta esize tcases tnoncases ccases cnoncases, esize(lnrr) studylab(trialnam) fixed(iv)
meta summ, eform(Risk ratio)
meta forestplot _id startyr _plot _esci _weight, eform(Risk ratio) ///
	nullrefline(favorsleft(BCG reduces risk of TB) favorsright(BCG increases risk of TB))
	


* 4.3 Precalculated effect estimates
gen logRR = ln( (tcases/ttotal) / (ccases/ctotal) )
gen selogRR = sqrt( 1/tcases +1/ccases -1/ttotal -1/ctotal )

metan9 logRR selogRR, fixed eform nograph

metan  logRR selogRR, fixed eform nograph

// Stata 16
meta set logRR selogRR, fixed
meta summ, eform



* 4.4 Specifying two analyses
* (and Figure 2)

metan9 tcases tnoncases ccases cnoncases, rr fixedi second(random) lcols(trialnam authors startyr alloc latitude) counts notable ///
	astext(70) textsize(200) boxsca(80) xlabel(0.1,10) xsize(10) ysize(6)

metan  tcases tnoncases ccases cnoncases, rr fixedi second(random) lcols(trialnam authors startyr alloc latitude) counts notable ///
	forestplot(astext(70) xlabel(0.1 1 10))
// Note that text size is handled differently by new version, and automatic sizing is better anyway, so textsize(200) is no longer valid
// boxsca(), xsize() and ysize() should need to be specified less frequently in the new version, as the default options should work better

// Stata 16
meta esize tcases tnoncases ccases cnoncases, esize(lnrr) studylab(trialnam) fixed(iv)
meta forest _id authors startyr alloc latitude _plot _data1 _data2 _esci _weight, eform(Risk ratio)
// Cannot specify multiple models (fixed + random) with meta



* 6.1 Syntax and options for by()
* (and Figure 3)
gen lat_cat = ""
replace lat_cat = "Tropical, < 23.5 latitude" if latitude <= 23.5
replace lat_cat = "23.5-40 latitude" if latitude > 23.5 & latitude < 40
replace lat_cat = "Northern, > 40 latitude" if latitude >= 40 & latitude < .
assert lat_cat != ""
label var lat_cat "Latitude region"

metan9 tcases tnoncases ccases cnoncases, rr fixedi second(random) nosecsub lcols(trialnam startyr latitude) by(lat_cat) ///
	astext(60) xlabel(0.1,10) xsize(10) ysize(8)

metan  tcases tnoncases ccases cnoncases, rr fixedi second(random) nosecsub lcols(trialnam startyr latitude) by(lat_cat) ///
	forestplot(astext(60) xlabel(0.1 1 10))

// Stata 16
meta forest _id authors startyr latitude _plot _data1 _data2 _esci _weight, eform(Risk ratio) subgroup(lat_cat)
// Cannot specify multiple models (fixed + random) with meta
// Not sure if/how you can specify the sort order of the categories



* 7.2 Pooled estimates
* (and Figure 4)

metan9 logRR selogRR, random second(-.6587 -1.205 -.1937 Bayes) secondstats(Noninformative prior: d~dnorm(0.0, 0.001)) ///
	lcols(trialnam startyr latitude) eform notable astext(60) textsize(130) xlabel(0.1,10)

metan logRR selogRR, random second(-.6587 -1.205 -.1937 Bayes) secondstats(Noninformative prior: d~dnorm(0.0, 0.001)) ///
	lcols(trialnam startyr latitude) eform notable forestplot(astext(60) xlabel(0.1 1 10))

// Stata 16
meta set logRR selogRR, random(dl) studylab(trialnam)
meta forest _id startyr latitude _plot _esci _weight, eform(Risk ratio) customoverall(-.6587 -1.205 -.1937, label("Bayes, Noninformative prior: d~dnorm(0.0, 0.001)")) 



* 9.2 Prediction interval for the random-effects distribution
* (and Figure 5)

metan9 tcases tnoncases ccases cnoncases, rr random rfdist lcols(trialnam startyr latitude) by(lat_cat) notable ///
	astext(60) xlabel(0.1,10) xsize(10) ysize(8)

metan  tcases tnoncases ccases cnoncases, rr random rfdist lcols(trialnam startyr latitude) by(lat_cat) notable ///
	forestplot(astext(60) xlabel(0.1 1 10))

// Note: new version does not automatically present a p-value for heterogeneity on the forest plot
// to avoid confusion with significance test of overall effect
// Hence, to *exactly* recreate the example, we need to add the option "hetstat(p)"
metan  tcases tnoncases ccases cnoncases, rr random rfdist lcols(trialnam startyr latitude) by(lat_cat) notable ///
	forestplot(astext(60) xlabel(0.1 1 10) hetstat(p))

// Note: Slightly different results are seen here in the 2nd or 3rd decimal place
// I think this is due to metan9 using "float" where (new) metan uses "double" throughout.

// Stata 16
meta esize tcases tnoncases ccases cnoncases, esize(lnrr) random(dl) studylab(trialnam)
meta forest _id startyr latitude _plot _esci _weight, subgroup(lat_cat) eform
meta forest _id startyr latitude _plot _esci _weight, predinterval eform
// Cannot combine prediction intervals with subgroups with meta



* 9.3 Vaccine efficacy
* (and Figure 6)

metan9 tcases tnoncases ccases cnoncases, rr random efficacy lcols(trialnam startyr) notable ///
	textsize(150) xlabel(0.1, 10)

metan  tcases tnoncases ccases cnoncases, rr random efficacy lcols(trialnam startyr) notable ///
	forestplot(xlabel(0.1 1 10))

// Note: new version does not automatically present a p-value for heterogeneity on the forest plot
// to avoid confusion with significance test of overall effect.
// Also, in this context (common effect, 2x2 binary data), the default is to present Q, not I-squared.
// Hence, to *exactly* recreate the example, we need to add the option "hetstat(isq p)"
metan  tcases tnoncases ccases cnoncases, rr random efficacy lcols(trialnam startyr) notable ///
	forestplot(xlabel(0.1 1 10) hetstat(isq p) extraline(no))

// Stata 16:  Vaccine efficacy is not automatically calculated
qui gen VacEff = string(100*(1 - exp(_ES)), "%4.0f") + " (" ///
			+ string(100*(1 - exp(_LCI)), "%4.0f") + ", " ///
			+ string(100*(1 - exp(_UCI)), "%4.0f") + ")" if !missing(_ES)
label variable VacEff "Vaccine efficacy (%)"
meta forest _id startyr _plot _esci _weight VacEff, eform


	
* 10.1 metan graph options
* (resulting figure not shown in the article)
gen counts = string(tcases) + "/" + string(tcases+tnoncases) + "," + string(ccases) + "/" + string(ccases+cnoncases)

metan9 tcases tnoncases ccases cnoncases, rr fixedi second(random) nosecsub notable ///
		olineopt(lwidth(thick) lcolor(navy) lpattern(dot)) ///
		boxopt(msymbol(triangle) mcolor(dkgreen)) ///
		pointopt(mlabel(counts) mlabsize(tiny) mlabposition(5))

metan  tcases tnoncases ccases cnoncases, rr fixedi second(random) nosecsub notable ///
	forestplot( ///
		olineopt(lwidth(thick) lcolor(navy) lpattern(dot)) ///
		boxopt(msymbol(triangle) mcolor(dkgreen)) ///
		pointopt(mlabel(counts) mlabsize(tiny) mlabposition(5)) ///
	)

// Stata 16
meta forest _id _plot _esci _weight, eform fixed(iv) ///
	esrefline( lwidth(thick) lcolor(navy) lpattern(dot) ) ///
	markeropts( msymbol(triangle) mcolor(dkgreen) ) ///
	omarkeropts( mcolor(red) )
// Cannot specify multiple models (fixed + random) with meta
// Cannot use "mlabel" options within markeropts()



* 10.3 Notes on graph building
* (and Figure 7)
global metamethod rr fixedi second(random) nosecsub
global metacolumns lcols(trialnam startyr latitude)
global metastyle boxopt(mcolor(forest_green) msymbol(triangle)) ///
	pointopt(msymbol(smtriangle) mcolor(gold) msize(tiny) ///
	mlabel(counts) mlabsize(tiny) mlabposition(2) mlabcolor(brown)) ///
	diamopt(lcolor(black) lwidth(medthick)) graphregion(fcolor(gs10)) boxsca(80)
global metaopts astext(60) favours(decreases TB # increases TB) xlabel(0.1, 0.2, 0.5, 1, 2, 5, 10)

metan9 tcases tnoncases ccases cnoncases, $metamethod $metacolumns $metastyle $metaopts by(lat_cat) xsize(10) ysize(8) notable

// due to xlabel() now accepting a standard Stata numlist, need to redefine this global macro
global metaopts astext(60) favours(decreases TB # increases TB) xlabel(0.1 0.2 0.5 1 2 5 10)

metan  tcases tnoncases ccases cnoncases, $metamethod $metacolumns forestplot($metastyle $metaopts) by(lat_cat) notable

// Stata 16
// Need to completely redefine global macros!!
global metastyle markeropts(mcolor(forest_green) msymbol(triangle) ) ///
	insidemarker( msymbol(smtriangle) mcolor(gold) msize(tiny) ) ///
	omarkeropts( mlcolor(black) mlwidth(medthick) mfcolor(none) ) graphregion(fcolor(gs10))

global metanull nullrefline(favorsleft(decreases TB) favorsright(increases TB))
	
meta forest _id startyr latitude _plot _esci _weight, eform fixed(iv) subgroup(lat_cat) $metastyle $metanull
// Cannot specify multiple models (fixed + random) with meta
// Cannot use "mlabel" options within markeropts()




log close



***************************
***  Validation script  ***
***************************

// Compiled by David Fisher
// for validating  -metan- v3.9 (beta)  05nov2020
// [and previosuly -metan- v3.8 (beta)  19oct2020
// [and previously -metan- v3.8 (beta)  09oct2020
// [and previously -metan- v3.7 (beta)  04jul2020]

** Validation file 1: Backwards compatibility
// - systematically test all options in -metan9- help file
// - comparison with meta (Stata v16) / metan9 / metaprop help files & publications


// Note: This script outputs *three* log files:
//  1. The standard log file
//  2. Log file containing just the -metan9- / -metaan- / -metaprop- output (as appropriate)
//  3. Log file containing just the -metan- v4.0x output
// Log files 2 and 3 can be compared e.g. using MS Word's "compare" function to check differences in numerical output.

global Date = subinstr("$S_DATE", " ", "", .)

// global BaseDir1  `"S:/MRCCTU_Methodology/Software/ipdmetan/Validation/1. Backwards compatibility"'
global Datasets `"$BaseDir1/Example datasets"'
global Graphs   `"$BaseDir1/Graphs_${Date}"'

// cap log close
log using `"$BaseDir1/metan_test_${Date}_log1"', name(log1)
log using `"$BaseDir1/metan_test_${Date}_log2"', name(log2)
log using `"$BaseDir1/metan_test_${Date}_log3"', name(log3)
loc starttime "$S_TIME"

nois di _n(3)
nois di `"Current directory:"' c(pwd)
nois di `"File run: `logname'.do"'
nois di `"Date file produced: $S_DATE $S_TIME"'
nois di `"--------------------------------------"'
nois di _n(3)


** Check that we're using the correct version
// [was -metan-  v3.7 (beta)  04jul2020]
// should be -metan-  v3.8 (beta)  23sep2020  [will be 4.00 upon release]
//       and -metan9- v3.04 (i.e. the version currently available via SSC)
which metan
which metan9



*********************************************
* Part 1: test all -metan9- options in turn *
*********************************************

// This section tests backwards compatibility.
// As requested by JS/JPTH, all syntax in the metan9 help file should continue to run
//   even if no longer "preferred" syntax for metan v4.0x

use "$Datasets/metan_example_data", clear

log off log2
log off log3



*** if, in, weights

// if, in
log on  log2
metan9 tdeath tnodeath cdeath cnodeath in 1/10 if type_study=="1: Cohort Studies", label(namevar=id, yearid=year)
log off log2
log on  log3
metan  tdeath tnodeath cdeath cnodeath in 1/10 if type_study=="1: Cohort Studies", label(namevar=id, yearid=year)
log off log3

// weights: not allowed; listed incorrectly in the -metan9- syntax diagram but now removed
log on  log2
cap nois metan9 tdeath tnodeath cdeath cnodeath [aw=n_pop], label(namevar=id, yearid=year)
log off log2
log on  log3
cap nois metan  tdeath tnodeath cdeath cnodeath [aw=n_pop], label(namevar=id, yearid=year)
log off log3


*** measure_and_model_options: or, rr, rd

log on  log2
metan9 tdeath tnodeath cdeath cnodeath, label(namevar=id, yearid=year) or
log off log2
log on  log3
metan  tdeath tnodeath cdeath cnodeath, label(namevar=id, yearid=year) or
log off log3

// correctly exits with error if combined with mean + sd
log on  log2
cap nois metan9 tsample tmean tsd csample cmean csd, label(namevar=id, yearid=year) or
log off log2
log on  log3
cap nois metan  tsample tmean tsd csample cmean csd, label(namevar=id, yearid=year) or
log off log3

// use two/three variable syntax
gen double logor   = ln( (tdeath*cnodeath)/(tnodeath*cdeath) )
gen double selogor = sqrt( (1/tdeath) + (1/tnodeath) + (1/cdeath) + (1/cnodeath) )
gen double lcilogor = logor - invnorm(.975)*selogor
gen double ucilogor = logor + invnorm(.975)*selogor

quietly metan tdeath tnodeath cdeath cnodeath, or nograph
// rename (_ES _seES _LCI _UCI) (logor selogor lcilogor ucilogor)
assert float(_ES) == float(logor)
assert float(_seES) == float(selogor)
assert float(_LCI) == float(lcilogor)
assert float(_UCI) == float(ucilogor)

log on  log2
cap nois metan9 logor selogor, label(namevar=id, yearid=year) or	// exits with error
log off log2
log on  log3
cap nois metan  logor selogor, label(namevar=id, yearid=year) or	// does not exit, but undocumented (as requested by JS/JH)
log off log3

log on  log2
cap nois metan9 logor lcilogor ucilogor, label(namevar=id, yearid=year) or		// exits with error
log off log2
log on  log3
cap nois metan  logor lcilogor ucilogor, label(namevar=id, yearid=year) or		// does not exit, but undocumented (as requested by JS/JH)
log off log3

// missing data
gen logor2 = logor
replace logor2 = . in 1

log on  log2
cap nois metan9 logor2 lcilogor ucilogor, label(namevar=id, yearid=year)		// exits with error
log off log2
log on  log3
metan  logor2 lcilogor ucilogor, label(namevar=id, yearid=year)					// excludes the study with missing logor, and prints warning message
metan  logor2 lcilogor ucilogor, label(namevar=id, yearid=year) keepall			// shows the missing study in table (as "Insuff data") and prints appropriate message in header
log off log3



*** measure_and_model_options: fixed, random, fixedi, peto

log on  log2
metan9 tdeath tnodeath cdeath cnodeath, label(namevar=id, yearid=year) fixed	// M-H
log off log2
log on  log3
metan  tdeath tnodeath cdeath cnodeath, label(namevar=id, yearid=year) fixed	// M-H
log off log3

log on  log2
metan9 tsample tmean tsd csample cmean csd, label(namevar=id, yearid=year) fixed	// I-V if not 2x2 cell counts
log off log2
log on  log3
metan  tsample tmean tsd csample cmean csd, label(namevar=id, yearid=year) fixed	// I-V if not 2x2 cell counts
log off log3

log on  log2
metan9 tdeath tnodeath cdeath cnodeath, label(namevar=id, yearid=year) random	// D-L
log off log2
log on  log3
metan  tdeath tnodeath cdeath cnodeath, label(namevar=id, yearid=year) random	// D-L
log off log3

log on  log2
metan9 tdeath tnodeath cdeath cnodeath, label(namevar=id, yearid=year) peto	// Peto
log off log2
log on  log3
metan  tdeath tnodeath cdeath cnodeath, label(namevar=id, yearid=year) peto	// Peto
log off log3

// correctly exits with error if combined with non-2x2 count data
log on  log2
cap nois metan9 logor lcilogor ucilogor, label(namevar=id, yearid=year) peto
log off log2
log on  log3
cap nois metan  logor lcilogor ucilogor, label(namevar=id, yearid=year) peto
log off log3



*** measure_and_model_options: cornfield, chi2, breslow 
// Note: check that both -metan- and -metan9- derive I-squared from Breslow statistic if appropriate

log on  log2
cap nois metan9 tdeath tnodeath cdeath cnodeath, label(namevar=id, yearid=year) cornfield	// exits with error as cornfield only valid with OR
metan9 tdeath tnodeath cdeath cnodeath, label(namevar=id, yearid=year) cornfield or
log off log2
log on  log3
metan  tdeath tnodeath cdeath cnodeath, label(namevar=id, yearid=year) cornfield			// *assumes* OR and runs accordingly, rather than exiting with error
log off log3																				// (and prints a message about the OR assumption)

// correctly exits with error if combined with non-odds ratio and 2x2 data
log on  log2
cap nois metan9 logor lcilogor ucilogor, label(namevar=id, yearid=year) cornfield
log off log2
log on  log3
cap nois metan  logor lcilogor ucilogor, label(namevar=id, yearid=year) cornfield or
cap nois metan  tdeath tnodeath cdeath cnodeath, label(namevar=id, yearid=year) cornfield rr
log off log3

log on  log2
cap nois metan9 tdeath tnodeath cdeath cnodeath, label(namevar=id, yearid=year) chi2		// exits with error as chi2 only valid with OR
metan9 tdeath tnodeath cdeath cnodeath, label(namevar=id, yearid=year) chi2 or
log off log2
log on  log3
metan  tdeath tnodeath cdeath cnodeath, label(namevar=id, yearid=year) chi2					// *assumes* OR and runs accordingly, rather than exiting with error
log off log3																				// (and prints a message about the OR assumption)

// correctly exits with error if combined with non-odds ratio and 2x2 data
log on  log2
cap nois metan9 logor lcilogor ucilogor, label(namevar=id, yearid=year) chi2
log off log2
log on  log3
cap nois metan  logor lcilogor ucilogor, label(namevar=id, yearid=year) chi2 or
cap nois metan  tdeath tnodeath cdeath cnodeath, label(namevar=id, yearid=year) chi2 rr
log off log3

log on  log2
cap nois metan9 tdeath tnodeath cdeath cnodeath, label(namevar=id, yearid=year) breslow		// exits with error as breslow only valid with OR
metan9 tdeath tnodeath cdeath cnodeath, label(namevar=id, yearid=year) breslow or
log off log2
log on  log3
metan  tdeath tnodeath cdeath cnodeath, label(namevar=id, yearid=year) breslow				// *assumes* OR and runs accordingly, rather than exiting with error
log off log3																				// (and prints a message about the OR assumption)

// correctly exits with error if combined with non-odds ratio and 2x2 data
log on  log2
cap nois metan9 logor lcilogor ucilogor, label(namevar=id, yearid=year) breslow
log off log2
log on  log3
cap nois metan  logor lcilogor ucilogor, label(namevar=id, yearid=year) breslow or
cap nois metan  tdeath tnodeath cdeath cnodeath, label(namevar=id, yearid=year) breslow rr
log off log3



*** measure_and_model_options: nointeger, cc(#)
gen cdeath2 = cdeath + 0.5

log on  log2
cap nois metan9 tdeath tnodeath cdeath2 cnodeath, label(namevar=id, yearid=year)
metan9 tdeath tnodeath cdeath2 cnodeath, label(namevar=id, yearid=year) nointeger
log off log2
log on  log3
cap nois metan tdeath tnodeath cdeath2 cnodeath, label(namevar=id, yearid=year)
metan tdeath tnodeath cdeath2 cnodeath, label(namevar=id, yearid=year) nointeger
log off log3

cap drop cdeath2
gen cdeath2 = cdeath
replace cdeath2 = 0 if type_study=="1: Cohort Studies"

log on  log2
metan9 tdeath tnodeath cdeath2 cnodeath, label(namevar=id, yearid=year)			// uses corrected M-H
log off log2
log on  log3
metan  tdeath tnodeath cdeath2 cnodeath, label(namevar=id, yearid=year)			// uses *un* corrected M-H by default
metan  tdeath tnodeath cdeath2 cnodeath, label(namevar=id, yearid=year) cc(0.5)	// recovers -metan9- results
log off log3

cap drop tdeath2
gen tdeath2 = tdeath
replace tdeath2 = 0 in 1

log on  log2
metan9 tdeath2 tnodeath cdeath2 cnodeath, label(namevar=id, yearid=year)			// uses corrected M-H; automatically displays the "double zero" study (as "Excluded")
log off log2
log on  log3
metan  tdeath2 tnodeath cdeath2 cnodeath, label(namevar=id, yearid=year)			// uses *un* corrected M-H by default; prints warning message about "double zero" study
metan  tdeath2 tnodeath cdeath2 cnodeath, label(namevar=id, yearid=year) cc(0.5)	// recovers -metan9- results
metan  tdeath2 tnodeath cdeath2 cnodeath, label(namevar=id, yearid=year) keepall	// additionally displays the "double zero" study (as "insufficient data")
log off log3



*** measure_and_model_options: wgt()

// 2x2 data + wgt() switches default model from MH to IV; MH is not allowed
log on  log2
metan9 tdeath tnodeath cdeath cnodeath, label(namevar=id, yearid=year) wgt(n_pos)
log off log2
log on  log3
metan  tdeath tnodeath cdeath cnodeath, label(namevar=id, yearid=year) wgt(n_pos)
cap nois metan  tdeath tnodeath cdeath cnodeath, label(namevar=id, yearid=year) wgt(n_pos) mh
log off log3

// wgt() with random-effects: metan v4 uses weighting formula, metan v3 exits with error
log on  log2
cap nois metan9 logor lcilogor ucilogor, label(namevar=id, yearid=year) wgt(n_pos) random
log off log2
log on  log3
metan  logor lcilogor ucilogor, label(namevar=id, yearid=year) wgt(n_pos) random
log off log3



*** measure_and_model_options: first(), second() [ and firststats(), secondstats() ]
log on  log2
metan9 logor lcilogor ucilogor, label(namevar=id, yearid=year) first(0.924 0.753 1.095 Bayesian) firststats(param V=3.86, p=0.012) wgt(n_pos)
log off log2
log on  log3
metan  logor lcilogor ucilogor, label(namevar=id, yearid=year) first(0.924 0.753 1.095 Bayesian) firststats(param V=3.86, p=0.012) wgt(n_pos)
log off log3

log on  log2
metan9 logor lcilogor ucilogor, label(namevar=id, yearid=year) wgt(n_pos) ///
	first(0.924 0.753 1.095 Bayesian) firststats(param V=3.86, p=0.012) second(0.298 0.101 0.313 FooBar) secondstats(Hello World)
log off log2
log on  log3
metan  logor lcilogor ucilogor, label(namevar=id, yearid=year) wgt(n_pos) ///
	first(0.924 0.753 1.095 Bayesian) firststats(param V=3.86, p=0.012) second(0.298 0.101 0.313 FooBar) secondstats(Hello World)
log off log3

// can't have first=user, second=standard...
log on  log2
cap nois metan9 logor lcilogor ucilogor, label(namevar=id, yearid=year) wgt(n_pos) ///
	first(0.924 0.753 1.095 Bayesian) firststats(param V=3.86, p=0.012) second(random)
log off log2
log on  log3
cap nois metan  logor lcilogor ucilogor, label(namevar=id, yearid=year) wgt(n_pos) ///
	first(0.924 0.753 1.095 Bayesian) firststats(param V=3.86, p=0.012) second(random)
log off log3

// ...but the other way around is fine
log on  log2
metan9 tdeath tnodeath cdeath cnodeath, label(namevar=id, yearid=year) second(0.924 0.753 1.095 Bayesian) secondstats(param V=3.86, p=0.012)
log off log2
log on  log3
metan  tdeath tnodeath cdeath cnodeath, label(namevar=id, yearid=year) second(0.924 0.753 1.095 Bayesian) secondstats(param V=3.86, p=0.012)
log off log3



*** options_for_continuous_data: cohen, hedges, glass, nostandard
log on  log2
cap nois metan9 logor lcilogor ucilogor, label(namevar=id, yearid=year) cohen
log off log2
log on  log3
cap nois metan  logor lcilogor ucilogor, label(namevar=id, yearid=year) cohen
log off log3

log on  log2
cap nois metan9 logor lcilogor ucilogor, label(namevar=id, yearid=year) nostandard
log off log2
log on  log3
cap nois metan  logor lcilogor ucilogor, label(namevar=id, yearid=year) nostandard
log off log3

log on  log2
cap nois metan9 tdeath tnodeath cdeath cnodeath, label(namevar=id, yearid=year) cohen
log off log2
log on  log3
cap nois metan  tdeath tnodeath cdeath cnodeath, label(namevar=id, yearid=year) cohen
log off log3

log on  log2
cap nois metan9 tdeath tnodeath cdeath cnodeath, label(namevar=id, yearid=year) nostandard
log off log2
log on  log3
cap nois metan  tdeath tnodeath cdeath cnodeath, label(namevar=id, yearid=year) nostandard
log off log3

log on  log2
metan9 tsa tmean tsd csa cmean csd, label(namevar=id, yearid=year)
log off log2
log on  log3
metan  tsa tmean tsd csa cmean csd, label(namevar=id, yearid=year)
log off log3



*** output_options: by(byvar) nosubgroup sgweight nosecsub
log on  log2
metan9 logor lcilogor ucilogor, label(namevar=id, yearid=year) by(type_study)
log off log2
log on  log3
metan  logor lcilogor ucilogor, label(namevar=id, yearid=year) by(type_study)
log off log3

log on  log2
metan9 logor lcilogor ucilogor, label(namevar=id, yearid=year) by(type_study) random
log off log2
log on  log3
metan  logor lcilogor ucilogor, label(namevar=id, yearid=year) by(type_study) random	// between-subgroup heterogeneity is now calculated
log off log3

log on  log2
metan9 logor lcilogor ucilogor, label(namevar=id, yearid=year) by(type_study) nosubgroup
log off log2
log on  log3
metan  logor lcilogor ucilogor, label(namevar=id, yearid=year) by(type_study) nosubgroup
log off log3

log on  log2
metan9 logor lcilogor ucilogor, label(namevar=id, yearid=year) by(type_study) sgweight
log off log2
log on  log3
metan  logor lcilogor ucilogor, label(namevar=id, yearid=year) by(type_study) sgweight
log off log3

log on  log2
metan9 tsa tmean tsd csa cmean csd, label(namevar=id, yearid=year) by(type_study) second(random) nosecsub
log off log2
log on  log3
metan  tsa tmean tsd csa cmean csd, label(namevar=id, yearid=year) by(type_study) second(random) nosecsub
log off log3

// with zero cell counts
log on  log2
metan9 tdeath tnodeath cdeath cnodeath, label(namevar=id, yearid=year) by(type_study)
log off log2
log on  log3
metan  tdeath tnodeath cdeath cnodeath, label(namevar=id, yearid=year) by(type_study)
log off log3

log on  log2
metan9 tdeath tnodeath cdeath cnodeath, label(namevar=id, yearid=year) by(type_study) sgweight
log off log2
log on  log3
metan  tdeath tnodeath cdeath cnodeath, label(namevar=id, yearid=year) by(type_study) sgweight
log off log3



*** output_options: log eform efficacy ilevel(#) olevel(#)
log on  log2
metan9 tdeath tnodeath cdeath cnodeath, label(namevar=id, yearid=year) log
log off log2
log on  log3
metan  tdeath tnodeath cdeath cnodeath, label(namevar=id, yearid=year) log
log off log3

log on  log2
cap nois metan9 tdeath tnodeath cdeath cnodeath, label(namevar=id, yearid=year) eform		// eform not allowed
log off log2
log on  log3
cap nois metan  tdeath tnodeath cdeath cnodeath, label(namevar=id, yearid=year) eform		// eform allowed, but has no effect
log off log3

log on  log2
metan9 tdeath tnodeath cdeath cnodeath, label(namevar=id, yearid=year) efficacy
log off log2
log on  log3
metan  tdeath tnodeath cdeath cnodeath, label(namevar=id, yearid=year) efficacy
log off log3

log on  log2
metan9 tdeath tnodeath cdeath cnodeath, label(namevar=id, yearid=year) by(type_study) ilevel(80)
log off log2
log on  log3
metan  tdeath tnodeath cdeath cnodeath, label(namevar=id, yearid=year) by(type_study) ilevel(80)
log off log3

log on  log2
metan9 tdeath tnodeath cdeath cnodeath, label(namevar=id, yearid=year) by(type_study) olevel(80)
log off log2
log on  log3
metan  tdeath tnodeath cdeath cnodeath, label(namevar=id, yearid=year) by(type_study) olevel(80)
log off log3



*** output_options: sortby(varlist) label(namevar yearvar)
// Note, there is some inconsistency in the way this option is documented
//   option list says "yearvar", but Examples and code itself uses "yearid"
// In -metan- v4.0x, either is permitted; but only "yearvar" is documented, to match with "namevar"
log on  log2
metan9 tdeath tnodeath cdeath cnodeath, label(namevar=id, yearid=year) sortby(n_pos)
log off log2
log on  log3
metan  tdeath tnodeath cdeath cnodeath, label(namevar=id, yearid=year) sortby(n_pos)
log off log3

log on  log2
metan9 tdeath tnodeath cdeath cnodeath, label(namevar=id, yearid=year) sortby(n_pos) by(type_study)
log off log2
log on  log3
metan  tdeath tnodeath cdeath cnodeath, label(namevar=id, yearid=year) sortby(n_pos) by(type_study)
log off log3



*** output_options: nokeep notable nograph
// Note: nokeep has been renamed nokeepvars in -metan- v4 (to avoid confusion with option "keepall"), but nokeep still works
// Some differences in naming of stored vars, e.g. no "selogES"; all vars on interval scale; _SS is now _NN
// Also, _rsample is always generated unless "norsample"
log on  log2
cap drop _*
qui metan9 tdeath tnodeath cdeath cnodeath, label(namevar=id, yearid=year) nograph
cap nois desc _*
log off log2

log on  log3
cap drop _*
qui metan tdeath tnodeath cdeath cnodeath, label(namevar=id, yearid=year) nograph
cap nois desc _*
log off log3

log on  log2
cap drop _*
qui metan9 tdeath tnodeath cdeath cnodeath, label(namevar=id, yearid=year) nokeep nograph
cap nois desc _*
log off log2

log on  log3
cap drop _*
qui metan  tdeath tnodeath cdeath cnodeath, label(namevar=id, yearid=year) nokeep nograph
cap nois desc _*

cap drop _*
qui metan  tdeath tnodeath cdeath cnodeath, label(namevar=id, yearid=year) nokeep norsample nograph
cap nois desc _*
log off log3

cap drop _*




*** forest_plot_options: xlabel(#,...) xtick(#,...) boxsca(#) nobox nooverall nowt nostats group1(string) group2(string) effect(string) force
// Note warning messages with -metan- r.e. placing these options in forestplot()

qui metan9 tdeath tnodeath cdeath cnodeath, label(namevar=id, yearid=year) xlabel(.5,1,2) xtick(.75,1.5) force
graph save "$Graphs/graph1_metan9"
qui metan  tdeath tnodeath cdeath cnodeath, label(namevar=id, yearid=year) xlabel(.5,1,2) xtick(.75,1.5) force
graph save "$Graphs/graph1_metan"

qui metan9 tdeath tnodeath cdeath cnodeath, label(namevar=id, yearid=year) nobox
graph save "$Graphs/graph2_metan9"
qui metan  tdeath tnodeath cdeath cnodeath, label(namevar=id, yearid=year) nobox
graph save "$Graphs/graph2_metan"

log on  log2
metan9 tdeath tnodeath cdeath cnodeath, label(namevar=id, yearid=year) nooverall
log off log2
graph save "$Graphs/graph3_metan9"
log on  log3
metan  tdeath tnodeath cdeath cnodeath, label(namevar=id, yearid=year) nooverall
log off log3
graph save "$Graphs/graph3_metan"

log on  log2
metan9 tdeath tnodeath cdeath cnodeath, label(namevar=id, yearid=year) nowt
log off log2
graph save "$Graphs/graph4_metan9"
log on  log3
metan  tdeath tnodeath cdeath cnodeath, label(namevar=id, yearid=year) nowt
log off log3
graph save "$Graphs/graph4_metan"

log on  log2
metan9 tdeath tnodeath cdeath cnodeath, label(namevar=id, yearid=year) nostats
log off log2
graph save "$Graphs/graph5_metan9"
log on  log3
metan  tdeath tnodeath cdeath cnodeath, label(namevar=id, yearid=year) nostats
log off log3
graph save "$Graphs/graph5_metan"

qui metan9 tdeath tnodeath cdeath cnodeath, label(namevar=id, yearid=year) group1(treat) group2(control) counts		// appears on the right
graph save "$Graphs/graph6_metan9"
qui metan  tdeath tnodeath cdeath cnodeath, label(namevar=id, yearid=year) group1(treat) group2(control) counts		// appears on the left, c.f. RevMan
graph save "$Graphs/graph6_metan"

log on  log2
metan9 tdeath tnodeath cdeath cnodeath, label(namevar=id, yearid=year) effect(effect size)
log off log2
graph save "$Graphs/graph7_metan9"
log on  log3
metan  tdeath tnodeath cdeath cnodeath, label(namevar=id, yearid=year) effect(effect size)	// appears in the table as well as the forest plot
log off log3
graph save "$Graphs/graph7_metan"



*** forest_plot_options: lcols(varlist) rcols(varlist) astext(#) double nohet summaryonly rfdist rflevel(#) null(#) nulloff favours(string # string)
qui metan9 tdeath tnodeath cdeath cnodeath, label(namevar=id, yearid=year) lcols(n_pos) rcols(population) double		// ignores label()
graph save "$Graphs/graph8_metan9"
qui metan  tdeath tnodeath cdeath cnodeath, label(namevar=id, yearid=year) lcols(n_pos) rcols(population) double		// includes label() within lcols()
graph save "$Graphs/graph8_metan"

log on  log2
cap nois metan9 tdeath tnodeath cdeath cnodeath, label(namevar=id, yearid=year) astext(0)		// exits with error
log off log2
log on  log3
cap nois metan  tdeath tnodeath cdeath cnodeath, label(namevar=id, yearid=year) astext(0)		// exits with error
log off log3

qui metan9 tdeath tnodeath cdeath cnodeath, label(namevar=id, yearid=year) astext(10)
graph save "$Graphs/graph9_metan9"
qui metan  tdeath tnodeath cdeath cnodeath, label(namevar=id, yearid=year) astext(10)
graph save "$Graphs/graph9_metan"

qui metan9 tdeath tnodeath cdeath cnodeath, label(namevar=id, yearid=year) astext(90)
graph save "$Graphs/graph10_metan9"
qui metan  tdeath tnodeath cdeath cnodeath, label(namevar=id, yearid=year) astext(90)
graph save "$Graphs/graph10_metan"

log on  log2
cap nois metan9 tdeath tnodeath cdeath cnodeath, label(namevar=id, yearid=year) astext(100)		// exits with error
log off log2
log on  log3
cap nois metan  tdeath tnodeath cdeath cnodeath, label(namevar=id, yearid=year) astext(100)		// simply displays the plot, with no lcols/rcols!!
log off log3

log on  log2
metan9 tdeath tnodeath cdeath cnodeath, label(namevar=id, yearid=year) nohet
log off log2
graph save "$Graphs/graph11_metan9"
log on  log3
metan  tdeath tnodeath cdeath cnodeath, label(namevar=id, yearid=year) nohet		// nohet suppresses any on-screen het info, in addition to forest plot
log off log3
graph save "$Graphs/graph11_metan"

log on  log2
metan9 tdeath tnodeath cdeath cnodeath, label(namevar=id, yearid=year) summaryonly by(type_study)
log off log2
graph save "$Graphs/graph12_metan9"
log on  log3
metan  tdeath tnodeath cdeath cnodeath, label(namevar=id, yearid=year) summaryonly by(type_study)	// again: affects on-screen as well as forest plot
log off log3
graph save "$Graphs/graph12_metan"

qui metan9 tdeath tnodeath cdeath cnodeath, label(namevar=id, yearid=year) rfdist by(type_study)
graph save "$Graphs/graph13_metan9"
qui metan  tdeath tnodeath cdeath cnodeath, label(namevar=id, yearid=year) rfdist by(type_study)	// prints message that rfdist will be ignored (common effect)
graph save "$Graphs/graph13_metan"

qui metan9 tdeath tnodeath cdeath cnodeath, label(namevar=id, yearid=year) rfdist by(type_study) random rflevel(80)
graph save "$Graphs/graph14_metan9"
qui metan  tdeath tnodeath cdeath cnodeath, label(namevar=id, yearid=year) rfdist by(type_study) random rflevel(80)		// corrects the error in -metan9-; see comparison with -metafor- in R
graph save "$Graphs/graph14_metan"

qui metan9 tdeath tnodeath cdeath cnodeath, label(namevar=id, yearid=year) null(1.5)	// no idea what this is doing
graph save "$Graphs/graph15_metan9"
qui metan  tdeath tnodeath cdeath cnodeath, label(namevar=id, yearid=year) null(1.5)	// works on the exp scale, same as xlabel() and fp()
graph save "$Graphs/graph15_metan"

qui metan9 tdeath tnodeath cdeath cnodeath, label(namevar=id, yearid=year) nulloff
graph save "$Graphs/graph16_metan9"
qui metan  tdeath tnodeath cdeath cnodeath, label(namevar=id, yearid=year) nulloff
graph save "$Graphs/graph16_metan"

qui metan9 tdeath tnodeath cdeath cnodeath, label(namevar=id, yearid=year) favours(left hand side string # right hand side string)
graph save "$Graphs/graph17_metan9"
qui metan  tdeath tnodeath cdeath cnodeath, label(namevar=id, yearid=year) favours(left hand side string # right hand side string)
graph save "$Graphs/graph17_metan"



*** forest_plot_options: firststats(string) secondstats(string)
// [see above]



*** forest_plot_options: boxopt() diamopt() pointopt() ciopt() olineopt() classic nowarning

qui metan9 tdeath tnodeath cdeath cnodeath, label(namevar=id, yearid=year) boxopt(mcolor(red)) diamopt(lcolor(orange)) pointopt(mcolor(blue)) ///
	ciopt(lcolor(orange)) olineopt(lpattern(dash) lcolor(blue))
graph save "$Graphs/graph18_metan9"
qui metan  tdeath tnodeath cdeath cnodeath, label(namevar=id, yearid=year) boxopt(mcolor(red)) diamopt(lcolor(orange)) pointopt(mcolor(blue)) ///
	ciopt(lcolor(orange)) olineopt(lpattern(dash) lcolor(blue))
graph save "$Graphs/graph18_metan"

qui metan9 tdeath tnodeath cdeath cnodeath, label(namevar=id, yearid=year) classic nowarning
graph save "$Graphs/graph19_metan9"
qui metan  tdeath tnodeath cdeath cnodeath, label(namevar=id, yearid=year) classic nowarning
graph save "$Graphs/graph19_metan"




*******************************************
* Part 2. Recreate examples from metan.hlp *
********************************************

// We proceed by taking the exact command lines from the help file and SJ article,
// only changing the command name "metan" to "metan9" (i.e. the current v3.04)

// We then change the command line minimally, to run using -metan- v4.0x


use "$Datasets/metan_example_data", clear


* Example 1
// Risk difference from raw cell counts, random effects model, "label" specification with counts displayed
log on  log2
forvalues i = 1/15 {
	macro drop S_`i'
}
macro drop S_51
metan9 tdeath tnodeath cdeath cnodeath, rd random label(namevar=id, yearid=year) counts
return list
forvalues i=1/15 {
	cap nois macro list S_`i'
}
cap nois macro list S_51
log off log2
graph save "$Graphs/graph20_metan9"
// Note: the current metan help file gives the label() syntax as (namevar=... , yearvar=... )
// but the *example* has "yearid=..." and this indeed is how the command is actually coded.
// The new v4.00 is documented as "namevar, yearvar" BUT yearid is still valid.

log on  log3
forvalues i = 1/15 {
	macro drop S_`i'
}
macro drop S_51
metan  tdeath tnodeath cdeath cnodeath, rd random label(namevar=id, yearid=year) counts
return list, all
forvalues i=1/15 {
	cap nois macro list S_`i'
}
cap nois macro list S_51
log off log3
graph save "$Graphs/graph20_metan"

// Note: new version does not automatically present a p-value for heterogeneity on the forest plot
// to avoid confusion with significance test of overall effect
// Hence, to *exactly* recreate the example, we need to add the option "hetinfo(isq p)"
metan  tdeath tnodeath cdeath cnodeath, rd random label(namevar=id, yearvar=year) counts hetinfo(isq p)
graph save "$Graphs/graph20_metan_v2"

// Stata 16
gen id_year = id + " (" + string(year) + ")"
meta esize tdeath tnodeath cdeath cnodeath, esize(rd) studylab(id_year) random(dl)
meta summ
meta forest _id year _plot _data1 _data2 _esci _weight, noohet noohom noosig
graph save "$Graphs/graph20_meta16"



* Example 2
// Sort by year, use data columns syntax.
// Text size increased, specify percentage of graph as text and two lines per study; suppress stats, weight, heterogeneity stats and table.
log on  log2
forvalues i = 1/15 {
	macro drop S_`i'
}
macro drop S_51
metan9 tdeath tnodeath cdeath cnodeath, sortby(year) lcols(id year country) rcols(population) ///
	textsize(110) astext(60) double nostats nowt nohet notable
return list
forvalues i=1/15 {
	cap nois macro list S_`i'
}
cap nois macro list S_51
log off log2
graph save "$Graphs/graph21_metan9"

log on  log3
forvalues i = 1/15 {
	macro drop S_`i'
}
macro drop S_51
metan  tdeath tnodeath cdeath cnodeath, sortby(year) lcols(id year country) rcols(population) ///
	textsize(110) astext(60) double nostats nowt nohet notable
return list, all
forvalues i=1/15 {
	cap nois macro list S_`i'
}
cap nois macro list S_51
log off log3
graph save "$Graphs/graph21_metan"
// Note slight differences in syntax here, e.g. forestplot() option and "leftjustify"

// ...Also, text size is handled differently/better by new version, so textsize(110) may not be necessary.
metan  tdeath tnodeath cdeath cnodeath, sortby(year) lcols(id year country) rcols(population) ///
	forestplot(astext(60) double leftjustify) nostats nowt nohet notable
graph save "$Graphs/graph21_metan_v2"

// Stata 16
meta esize tdeath tnodeath cdeath cnodeath, esize(lnrr) studylab(id_year) common(mh)
meta forest _id year country _plot population, eform sort(year)
graph save "$Graphs/graph21_meta16"



* Example 3
// Analyse continuous data (6 parameter syntax), stratify by type of study, with weights summing to 100 within sub group,
// second analysis specified, display random effects distribution, show raw data counts, display "favours treatment vs. favours control" labels
log on  log2
forvalues i = 1/15 {
	macro drop S_`i'
}
macro drop S_51
metan9 tsample tmean tsd csample cmean csd, by(type_study) sgweight fixed second(random) rfdist counts label(namevar=id) ///
	favours(Treatment reduces blood pressure # Treatment increases blood pressure)
return list
forvalues i=1/15 {
	cap nois macro list S_`i'
}
cap nois macro list S_51
log off log2
graph save "$Graphs/graph22_metan9"
log on  log3
forvalues i = 1/15 {
	macro drop S_`i'
}
macro drop S_51
metan  tsample tmean tsd csample cmean csd, by(type_study) sgweight fixed second(random) rfdist counts label(namevar=id) ///
	favours(Treatment reduces blood pressure # Treatment increases blood pressure)
return list, all
forvalues i=1/15 {
	cap nois macro list S_`i'
}
cap nois macro list S_51
log off log3
graph save "$Graphs/graph22_metan"

// Stata 16
meta esize tsample tmean tsd csample cmean csd, studylab(id_year) random(dl)
meta summ, subgroup(type_study)
meta summ, predinterval
meta forest, subgroup(type_study) xtitle(Treatment reduces blood pressure   Treatment increases blood pressure)
graph save "$Graphs/graph22_meta16_part1"
meta forest, predinterval xtitle(Treatment reduces blood pressure   Treatment increases blood pressure)
graph save "$Graphs/graph22_meta16_part2"
// Cannot specify multiple models (fixed + random) with meta
// Cannot combine prediction intervals with subgroups with meta



* Example 4
// Generate log odds ratio and standard error, analyse with 2 parameter syntax.
// Graph has exponential form, scale is forced within set limits and ticks added, effect label specified.
gen logor = ln( (tdeath*cnodeath)/(tnodeath*cdeath) )
gen selogor = sqrt( (1/tdeath) + (1/tnodeath) + (1/cdeath) + (1/cnodeath) )

log on  log2
forvalues i = 1/15 {
	macro drop S_`i'
}
macro drop S_51
metan9 logor selogor, eform effect(Odds ratio) xlabel(0.5, 1, 1.5, 2, 2.5) force xtick(0.75, 1.25, 1.75, 2.25)
return list
forvalues i=1/15 {
	cap nois macro list S_`i'
}
cap nois macro list S_51
log off log2
graph save "$Graphs/graph23_metan9"
log on  log3
forvalues i = 1/15 {
	macro drop S_`i'
}
macro drop S_51
metan  logor selogor, eform effect(Odds ratio) xlabel(0.5, 1, 1.5, 2, 2.5) force xtick(0.75, 1.25, 1.75, 2.25)
return list, all
forvalues i=1/15 {
	cap nois macro list S_`i'
}
cap nois macro list S_51
log off log3
graph save "$Graphs/graph23_metan"

// Note that xlabel() and xtick() now expect a standard Stata -numlist-  (i.e. not comma-separated)
// ...and "force" is now a sub-option of xlabel() rather than a stand-alone option.
// However, the older syntax remains valid.

// Note: new version does not automatically present a p-value for heterogeneity on the forest plot
// to avoid confusion with significance test of overall effect
// Hence, to *exactly* recreate the example, we need to add the option "hetinfo(isq p)"
metan  logor selogor, eform effect(Odds ratio) forestplot(xlabel(0.5 1 1.5 2 2.5, force) xtick(0.75 1.25 1.75 2.25) hetinfo(isq p))
graph save "$Graphs/graph23_metan_v2"

// Stata 16
meta set logor selogor, studylab(id_year) random(dl)
meta summ, eform(Odds ratio)
meta forest, eform(Odds ratio) xtick(0.75 1.25 1.75 2.25) crop(.5 2.5)
graph save "$Graphs/graph23_meta16"



* Example 5
// Display diagnostic test data with 3 parameter syntax.
// Weight is number of positive diagnoses, axis label set and null specified at 50%.
// Overall effect estimate is not displayed, graph for visual examination only.
log on  log2
forvalues i = 1/15 {
	macro drop S_`i'
}
macro drop S_51
metan9 percent lowerci upperci, wgt(n_positives) label(namevar=id) nooverall notable ///
	xlabel(0,10,20,30,40,50,60,70,80,90,100) force null(50) title(Sensitivity, position(6))
return list
forvalues i=1/15 {
	cap nois macro list S_`i'
}
cap nois macro list S_51
log off log2
graph save "$Graphs/graph24_metan9"
log on  log3
forvalues i = 1/15 {
	macro drop S_`i'
}
macro drop S_51
metan  percent lowerci upperci, wgt(n_positives) label(namevar=id) nooverall notable ///
	xlabel(0,10,20,30,40,50,60,70,80,90,100) force null(50) title(Sensitivity, position(6))
return list, all
forvalues i=1/15 {
	cap nois macro list S_`i'
}
cap nois macro list S_51
log off log3
graph save "$Graphs/graph24_metan"

// Note that xlabel() now expects a standard Stata -numlist-
// ...and "force" is now a sub-option of xlabel() rather than a stand-alone option
// However, the older syntax remains valid.
metan  percent lowerci upperci, wgt(n_positives) label(namevar=id) nooverall notable ///
	forestplot(xlabel(0(10)100, force) null(50) title(Sensitivity, position(6)))
graph save "$Graphs/graph24_metan_v2"

// Stata 16
// Cannot easily do this, as percent confidence limits are asymmetrical by definition, but meta will not accept them.
// Would need to use a normalising transform (e.g. arcsine?).
// Also: Cannot use user-specified weights with meta



* Example 6
// User has analysed data with a non-standard technique and supplied effect estimates, weights and description of statistics.
// The scheme "Economist" has been used.
log on  log2
forvalues i = 1/15 {
	macro drop S_`i'
}
macro drop S_51
metan9 OR ORlci ORuci, wgt(bweight) first(0.924 0.753 1.095 Bayesian) firststats(param V=3.86, p=0.012) label(namevar=id) ///
	xlabel(0.25, 0.5, 1, 2, 4) force null(1) aspect(1.2) scheme(economist)
return list
forvalues i=1/15 {
	cap nois macro list S_`i'
}
cap nois macro list S_51
log off log2
graph save "$Graphs/graph25_metan9"
log on  log3
forvalues i = 1/15 {
	macro drop S_`i'
}
macro drop S_51
metan  OR ORlci ORuci, wgt(bweight) first(0.924 0.753 1.095 Bayesian) firststats(param V=3.86, p=0.012) label(namevar=id) ///
	xlabel(0.25, 0.5, 1, 2, 4) force null(1) aspect(1.2) scheme(economist) forestplot(diamopts(lwidth(thick)))
return list, all
forvalues i=1/15 {
	cap nois macro list S_`i'
}
cap nois macro list S_51
log off log3
graph save "$Graphs/graph25_metan"
// N.B. last forestplot option is needed because scheme(economist) treats lines differently from areas
// so default widths are different;  so need to override the scheme default for the diamond.
// (see scheme-economist.scheme)

// Note that xlabel() now expects a standard Stata -numlist-
// ...and "force" is now a sub-option of xlabel() rather than a stand-alone option
// However, the older syntax remains valid.
metan  OR ORlci ORuci, wgt(bweight) first(0.924 0.753 1.095 Bayesian) firststats(param V=3.86, p=0.012) label(namevar=id) ///
	forestplot(xlabel(0.25 0.5 1 2 4, force) null(1) aspect(1.2) scheme(economist))
graph save "$Graphs/graph25_metan_v2"

// Stata 16:
// Need to transform the OR and CI limits to make them symmetrical
// (see previous example)
gen lnOR = ln(OR)
gen lnORlci = ln(ORlci)
gen lnORuci = ln(ORuci)
meta set lnOR lnORlci lnORuci, studylab(id)
meta summ, eform(Odds ratio)
meta forest, eform(Odds ratio) customoverall(`=ln(.924)' `=ln(.753)' `=ln(1.095)', label("Bayesian, param V=3.86 p=0.012")) 
graph save "$Graphs/graph25_meta16"
// N.B. Cannot use user-specified weights with meta



* Example 7
// Variable "counts" defined showing raw data.
// Options to change the box, effect estimate marker and confidence interval used,
// and the counts variable has been attached to the estimate marker as a label.
gen counts = ". " + string(tdeath) + "/" + string(tdeath+tnodeath) + ", " + string(cdeath) + "/" + string(cdeath+cnodeath)

log on  log2
forvalues i = 1/15 {
	macro drop S_`i'
}
macro drop S_51
metan9 tdeath tnodeath cdeath cnodeath, lcols(id year) notable ///
		boxopt( mcolor(forest_green) msymbol(triangle) ) ///
		pointopt( msymbol(triangle) mcolor(gold) msize(tiny) mlabel(counts) mlabsize(vsmall) mlabcolor(forest_green) mlabposition(1) ) ///
		ciopt( lcolor(sienna) lwidth(medium) )
return list
forvalues i=1/15 {
	cap nois macro list S_`i'
}
cap nois macro list S_51
log off log2
graph save "$Graphs/graph26_metan9"
log on  log3
forvalues i = 1/15 {
	macro drop S_`i'
}
macro drop S_51
metan9 tdeath tnodeath cdeath cnodeath, lcols(id year) notable ///
		boxopt( mcolor(forest_green) msymbol(triangle) ) ///
		pointopt( msymbol(triangle) mcolor(gold) msize(tiny) mlabel(counts) mlabsize(vsmall) mlabcolor(forest_green) mlabposition(1) ) ///
		ciopt( lcolor(sienna) lwidth(medium) )
return list, all
forvalues i=1/15 {
	cap nois macro list S_`i'
}
cap nois macro list S_51
log off log3
graph save "$Graphs/graph26_metan"

// Note: new version does not automatically present a p-value for heterogeneity on the forest plot
// to avoid confusion with significance test of overall effect.
// Hence, to *exactly* recreate the example, we need to add the option "hetinfo(isq p)"
metan  tdeath tnodeath cdeath cnodeath, lcols(id year) notable hetinfo(isq p) ///
	forestplot( ///
		boxopt( mcolor(forest_green) msymbol(triangle) ) ///
		pointopt( msymbol(triangle) mcolor(gold) msize(tiny) mlabel(counts) mlabsize(vsmall) mlabcolor(forest_green) mlabposition(1) ) ///
		ciopt( lcolor(sienna) lwidth(medium) ) ///
	)
graph save "$Graphs/graph26_metan_v2"

// Stata 16
meta esize tdeath tnodeath cdeath cnodeath, studylab(id)
meta forestplot _id year _plot _esci _weight, ///
	markeropts( mcolor(forest_green) msymbol(triangle) ) ///
	insidemarker( msymbol(triangle) mcolor(gold) msize(tiny) ) ///
	ciopts( lcolor(sienna) lwidth(medium) ) ///
	omarkeropts( mcolor(blue) )
graph save "$Graphs/graph26_meta16"
// Cannot use "mlabel" options within markeropts()




*******************************************************************
* Part 3. Recreate examples from Harris et al, Stata Journal 2008 *
*******************************************************************

use "$Datasets/bcgtrial", clear


* 4.2 Display options
* (and Figure 1)
log on  log2
forvalues i = 1/15 {
	macro drop S_`i'
}
macro drop S_51
metan9 tcases tnoncases ccases cnoncases, rr fixedi lcols(trialnam startyr) ///
	xlabel(0.1, 10) favours(BCG reduces risk of TB # BCG increases risk of TB)
return list
forvalues i=1/15 {
	cap nois macro list S_`i'
}
cap nois macro list S_51
log off log2
graph save "$Graphs/graph27_metan9"
log on  log3
forvalues i = 1/15 {
	macro drop S_`i'
}
macro drop S_51
metan  tcases tnoncases ccases cnoncases, rr fixedi lcols(trialnam startyr) ///
	xlabel(0.1, 10) favours(BCG reduces risk of TB # BCG increases risk of TB)
return list, all
forvalues i=1/15 {
	cap nois macro list S_`i'
}
cap nois macro list S_51
log off log3
graph save "$Graphs/graph27_metan"

// Note: new version does not automatically present a p-value for heterogeneity on the forest plot
// to avoid confusion with significance test of overall effect
// Hence, to *exactly* recreate the example, we need to add the option "hetinfo(isq p)"
metan  tcases tnoncases ccases cnoncases, rr fixedi lcols(trialnam startyr) ///
	forestplot(xlabel(0.1 10) favours(BCG reduces risk of TB # BCG increases risk of TB) hetinfo(isq p))
graph save "$Graphs/graph27_metan_v2"

// Stata 16
meta esize tcases tnoncases ccases cnoncases, esize(lnrr) studylab(trialnam) fixed(iv)
meta summ, eform(Risk ratio)
meta forestplot _id startyr _plot _esci _weight, eform(Risk ratio) ///
	nullrefline(favorsleft(BCG reduces risk of TB) favorsright(BCG increases risk of TB))
graph save "$Graphs/graph27_meta16"
	


* 4.3 Precalculated effect estimates
gen logRR = ln( (tcases/ttotal) / (ccases/ctotal) )
gen selogRR = sqrt( 1/tcases +1/ccases -1/ttotal -1/ctotal )

log on  log2
forvalues i = 1/15 {
	macro drop S_`i'
}
macro drop S_51
metan9 logRR selogRR, fixed eform nograph
return list
forvalues i=1/15 {
	cap nois macro list S_`i'
}
cap nois macro list S_51
log off log2
// graph save "$Graphs/graph28_metan9"
log on  log3
forvalues i = 1/15 {
	macro drop S_`i'
}
macro drop S_51
metan  logRR selogRR, fixed eform nograph
return list, all
forvalues i=1/15 {
	cap nois macro list S_`i'
}
cap nois macro list S_51
log off log3
// graph save "$Graphs/graph28_metan"

// Stata 16
meta set logRR selogRR, fixed
meta summ, eform



* 4.4 Specifying two analyses
* (and Figure 2)

log on  log2
forvalues i = 1/15 {
	macro drop S_`i'
}
macro drop S_51
metan9 tcases tnoncases ccases cnoncases, rr fixedi second(random) lcols(trialnam authors startyr alloc latitude) counts notable ///
	astext(70) textsize(200) boxsca(80) xlabel(0.1,10) xsize(10) ysize(6)
return list
forvalues i=1/15 {
	cap nois macro list S_`i'
}
cap nois macro list S_51
log off log2
graph save "$Graphs/graph29_metan9"
log on  log3
forvalues i = 1/15 {
	macro drop S_`i'
}
macro drop S_51
metan  tcases tnoncases ccases cnoncases, rr fixedi second(random) lcols(trialnam authors startyr alloc latitude) counts notable ///
	astext(70) textsize(200) boxsca(80) xlabel(0.1,10) xsize(10) ysize(6)
return list, all
forvalues i=1/15 {
	cap nois macro list S_`i'
}
cap nois macro list S_51
log off log3
graph save "$Graphs/graph29_metan"

// Note that automatic sizing of text and graph dimensions is better in the new version,
// so options such as textsize(), boxsca(), xlabel(), xsize() and ysize() may not be necessary
metan  tcases tnoncases ccases cnoncases, rr fixedi second(random) lcols(trialnam authors startyr alloc latitude) counts notable ///
	forestplot(astext(70))
graph save "$Graphs/graph29_metan_v2"

// Stata 16
meta esize tcases tnoncases ccases cnoncases, esize(lnrr) studylab(trialnam) fixed(iv)
meta forest _id authors startyr alloc latitude _plot _data1 _data2 _esci _weight, eform(Risk ratio)
graph save "$Graphs/graph29_meta16"
// Cannot specify multiple models (fixed + random) with meta



* 6.1 Syntax and options for by()
* (and Figure 3)
gen lat_cat = ""
replace lat_cat = "Tropical, < 23.5 latitude" if latitude <= 23.5
replace lat_cat = "23.5-40 latitude" if latitude > 23.5 & latitude < 40
replace lat_cat = "Northern, > 40 latitude" if latitude >= 40 & latitude < .
assert lat_cat != ""
label var lat_cat "Latitude region"

log on  log2
forvalues i = 1/15 {
	macro drop S_`i'
}
macro drop S_51
metan9 tcases tnoncases ccases cnoncases, rr fixedi second(random) nosecsub lcols(trialnam startyr latitude) by(lat_cat) ///
	astext(60) xlabel(0.1,10) xsize(10) ysize(8)
return list
forvalues i=1/15 {
	cap nois macro list S_`i'
}
cap nois macro list S_51
log off log2
graph save "$Graphs/graph30_metan9"
log on  log3
forvalues i = 1/15 {
	macro drop S_`i'
}
macro drop S_51
metan  tcases tnoncases ccases cnoncases, rr fixedi second(random) nosecsub lcols(trialnam startyr latitude) by(lat_cat) ///
	astext(60) xlabel(0.1,10) xsize(10) ysize(8)
return list, all
forvalues i=1/15 {
	cap nois macro list S_`i'
}
cap nois macro list S_51
log off log3
graph save "$Graphs/graph30_metan"

// Note that automatic sizing of graph dimensions is better in the new version,
// so options such as xlabel(), xsize() and ysize() may not be necessary
metan  tcases tnoncases ccases cnoncases, rr fixedi second(random) nosecsub lcols(trialnam startyr latitude) by(lat_cat) ///
	forestplot(astext(60))
graph save "$Graphs/graph30_metan_v2"

// Stata 16
meta forest _id authors startyr latitude _plot _data1 _data2 _esci _weight, eform(Risk ratio) subgroup(lat_cat)
graph save "$Graphs/graph30_meta16"
// Cannot specify multiple models (fixed + random) with meta
// Not sure if/how you can specify the sort order of the categories



* 7.2 Pooled estimates
* (and Figure 4)
log on  log2
forvalues i = 1/15 {
	macro drop S_`i'
}
macro drop S_51
metan9 logRR selogRR, random second(-.6587 -1.205 -.1937 Bayes) secondstats(Noninformative prior: d~dnorm(0.0, 0.001)) ///
	lcols(trialnam startyr latitude) eform notable astext(60) textsize(130) xlabel(0.1,10)
return list
forvalues i=1/15 {
	cap nois macro list S_`i'
}
cap nois macro list S_51
log off log2
graph save "$Graphs/graph31_metan9"
log on  log3
forvalues i = 1/15 {
	macro drop S_`i'
}
macro drop S_51
metan  logRR selogRR, random second(-.6587 -1.205 -.1937 Bayes) secondstats(Noninformative prior: d~dnorm(0.0, 0.001)) ///
	lcols(trialnam startyr latitude) eform notable astext(60) textsize(130) xlabel(0.1,10)
return list, all
forvalues i=1/15 {
	cap nois macro list S_`i'
}
cap nois macro list S_51
log off log3
graph save "$Graphs/graph31_metan"

// Note that text size is handled differently/better by new version, so textsize(130) and xlabel() may not be necessary.
metan logRR selogRR, random second(-.6587 -1.205 -.1937 Bayes) secondstats(Noninformative prior: d~dnorm(0.0, 0.001)) ///
	lcols(trialnam startyr latitude) eform notable forestplot(astext(60))
graph save "$Graphs/graph31_metan_v2"

// Stata 16
meta set logRR selogRR, random(dl) studylab(trialnam)
meta forest _id startyr latitude _plot _esci _weight, eform(Risk ratio) customoverall(-.6587 -1.205 -.1937, label("Bayes, Noninformative prior: d~dnorm(0.0, 0.001)")) 



* 9.2 Prediction interval for the random-effects distribution
* (and Figure 5)
log on  log2
forvalues i = 1/15 {
	macro drop S_`i'
}
macro drop S_51
metan9 tcases tnoncases ccases cnoncases, rr random rfdist lcols(trialnam startyr latitude) by(lat_cat) notable ///
	astext(60) xlabel(0.1,10) xsize(10) ysize(8)
return list
forvalues i=1/15 {
	cap nois macro list S_`i'
}
cap nois macro list S_51
log off log2
graph save "$Graphs/graph32_metan9"
log on  log3
forvalues i = 1/15 {
	macro drop S_`i'
}
macro drop S_51
metan  tcases tnoncases ccases cnoncases, rr random rfdist lcols(trialnam startyr latitude) by(lat_cat) notable ///
	forestplot(astext(60) xlabel(0.1 1 10) xsize(10) ysize(8))
return list, all
forvalues i=1/15 {
	cap nois macro list S_`i'
}
cap nois macro list S_51
log off log3
graph save "$Graphs/graph32_metan"
// Note that automatic sizing of graph dimensions is better in the new version,
// so options such as xsize() and ysize() may not be necessary

// Also note: new version does not automatically present a p-value for heterogeneity on the forest plot
// to avoid confusion with significance test of overall effect
// Hence, to *exactly* recreate the example, we need to add the option "hetinfo(isq p)"
metan  tcases tnoncases ccases cnoncases, rr random rfdist lcols(trialnam startyr latitude) by(lat_cat) notable ///
	forestplot(astext(60) xlabel(0.1 1 10) hetinfo(isq p))
graph save "$Graphs/graph32_metan_v2"

// Note: Slightly different results are seen here in the 2nd or 3rd decimal place
// I think this is due to metan9 using "float" where (new) metan uses "double" throughout.

// Stata 16
meta esize tcases tnoncases ccases cnoncases, esize(lnrr) random(dl) studylab(trialnam)
meta forest _id startyr latitude _plot _esci _weight, subgroup(lat_cat) eform
graph save "$Graphs/graph32_meta16_part1"
meta forest _id startyr latitude _plot _esci _weight, predinterval eform
graph save "$Graphs/graph32_meta16_part2"
// Cannot combine prediction intervals with subgroups with meta



* 9.3 Vaccine efficacy
* (and Figure 6)
log on  log2
forvalues i = 1/15 {
	macro drop S_`i'
}
macro drop S_51
metan9 tcases tnoncases ccases cnoncases, rr random efficacy lcols(trialnam startyr) notable ///
	textsize(150) xlabel(0.1, 10)
return list
forvalues i=1/15 {
	cap nois macro list S_`i'
}
cap nois macro list S_51
log off log2
graph save "$Graphs/graph33_metan9"
log on  log3
forvalues i = 1/15 {
	macro drop S_`i'
}
macro drop S_51
metan  tcases tnoncases ccases cnoncases, rr random efficacy lcols(trialnam startyr) notable ///
	forestplot(textsize(150) xlabel(0.1 1 10))
return list, all
forvalues i=1/15 {
	cap nois macro list S_`i'
}
cap nois macro list S_51
log off log3
graph save "$Graphs/graph33_metan"
// Note that text size is handled differently/better by new version, so textsize(150) may not be necessary.
// Similarly, xlabel(0.1 1 10) may not be necessary, as sensible x-axis value labels are chosen automatically

// Also note: new version does not automatically present a p-value for heterogeneity on the forest plot
//  to avoid confusion with significance test of overall effect.
// Hence, to *exactly* recreate the example, we need to add the option "hetinfo(isq p)"
//  plus extraline(no) to prevent -forestplot- from showing it on a new line
metan  tcases tnoncases ccases cnoncases, rr random efficacy lcols(trialnam startyr) notable ///
	forestplot(hetinfo(isq p) extraline(no))
graph save "$Graphs/graph33_metan_v2"

// Stata 16:  Vaccine efficacy is not automatically calculated
qui gen VacEff = string(100*(1 - exp(_ES)), "%4.0f") + " (" ///
			+ string(100*(1 - exp(_LCI)), "%4.0f") + ", " ///
			+ string(100*(1 - exp(_UCI)), "%4.0f") + ")" if !missing(_ES)
label variable VacEff "Vaccine efficacy (%)"
meta forest _id startyr _plot _esci _weight VacEff, eform
graph save "$Graphs/graph33_meta16"


	
* 10.1 metan graph options
* (resulting figure not shown in the article)
gen counts = string(tcases) + "/" + string(tcases+tnoncases) + "," + string(ccases) + "/" + string(ccases+cnoncases)

log on  log2
forvalues i = 1/15 {
	macro drop S_`i'
}
macro drop S_51
metan9 tcases tnoncases ccases cnoncases, rr fixedi second(random) nosecsub notable ///
		olineopt(lwidth(thick) lcolor(navy) lpattern(dot)) ///
		boxopt(msymbol(triangle) mcolor(dkgreen)) ///
		pointopt(mlabel(counts) mlabsize(tiny) mlabposition(5))
return list
forvalues i=1/15 {
	cap nois macro list S_`i'
}
cap nois macro list S_51
log off log2
graph save "$Graphs/graph34_metan9"
log on  log3
forvalues i = 1/15 {
	macro drop S_`i'
}
macro drop S_51
metan  tcases tnoncases ccases cnoncases, rr fixedi second(random) nosecsub notable ///
	forestplot( ///
		olineopt(lwidth(thick) lcolor(navy) lpattern(dot)) ///
		boxopt(msymbol(triangle) mcolor(dkgreen)) ///
		pointopt(mlabel(counts) mlabsize(tiny) mlabposition(5)) ///
	)
return list, all
forvalues i=1/15 {
	cap nois macro list S_`i'
}
cap nois macro list S_51
log off log3
graph save "$Graphs/graph34_metan"

// Stata 16
meta forest _id _plot _esci _weight, eform fixed(iv) ///
	esrefline( lwidth(thick) lcolor(navy) lpattern(dot) ) ///
	markeropts( msymbol(triangle) mcolor(dkgreen) ) ///
	omarkeropts( mcolor(red) )
graph save "$Graphs/graph34_meta16"
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

log on  log2
forvalues i = 1/15 {
	macro drop S_`i'
}
macro drop S_51
metan9 tcases tnoncases ccases cnoncases, $metamethod $metacolumns $metastyle $metaopts by(lat_cat) xsize(10) ysize(8) notable
return list
forvalues i=1/15 {
	cap nois macro list S_`i'
}
cap nois macro list S_51
log off log2
graph save "$Graphs/graph35_metan9"

// due to xlabel() now accepting a standard Stata numlist, need to redefine this global macro
global metaopts astext(60) favours(decreases TB # increases TB) xlabel(0.1 0.2 0.5 1 2 5 10) xsize(10) ysize(8)

log on  log3
forvalues i = 1/15 {
	macro drop S_`i'
}
macro drop S_51
metan  tcases tnoncases ccases cnoncases, $metamethod $metacolumns forestplot($metastyle $metaopts) by(lat_cat) notable
return list, all
forvalues i=1/15 {
	cap nois macro list S_`i'
}
cap nois macro list S_51
log off log3
graph save "$Graphs/graph35_metan"

// Stata 16
// Need to completely redefine global macros!!
global metastyle markeropts(mcolor(forest_green) msymbol(triangle) ) ///
	insidemarker( msymbol(smtriangle) mcolor(gold) msize(tiny) ) ///
	omarkeropts( mlcolor(black) mlwidth(medthick) mfcolor(none) ) graphregion(fcolor(gs10))

global metanull nullrefline(favorsleft(decreases TB) favorsright(increases TB))
	
meta forest _id startyr latitude _plot _esci _weight, eform fixed(iv) subgroup(lat_cat) $metastyle $metanull
graph save "$Graphs/graph35_meta16"
// Cannot specify multiple models (fixed + random) with meta
// Cannot use "mlabel" options within markeropts()




***************************************
* Recreate examples from metabias.hlp *
***************************************

// -metabias- has had to be (minimally) modified because it includes the line:
// . qui drop _SS _ES _selogES _LCI _UCI _WT
// and in -metan- v4.0+ neither _SS nor _selogES are used.

// Here, "metabias" refers to the updated code;
//  "metabias9" is a (minimally) modfied version of the *original* code, pointing to -metan9- rather than to -metan- .

// N.B. it is unclear which dataset(s) should be used for the example code in this help file.
// Therefore, the example code has been minimally modified so as to run on the standard -metan- example dataset.

use "$Datasets/metan_example_data", clear

log on  log2
metabias9 tdeath tnodeath cdeath cnodeath, or harbord
log off log2
log on  log3
metabias  tdeath tnodeath cdeath cnodeath, or harbord
log off log3

log on  log2
metabias9 tdeath tnodeath cdeath cnodeath, or harbord graph mlabel(id)
log off log2
graph save "$Graphs/graph36_metan9"
log on  log3
metabias  tdeath tnodeath cdeath cnodeath, or harbord graph mlabel(id)
log off log3
graph save "$Graphs/graph36_metan"

log on  log2
metabias9 tdeath tnodeath cdeath cnodeath, or peters
log off log2
log on  log3
metabias  tdeath tnodeath cdeath cnodeath, or peters
log off log3

gen double logor   = ln( (tdeath*cnodeath)/(tnodeath*cdeath) )
gen double selogor = sqrt( (1/tdeath) + (1/tnodeath) + (1/cdeath) + (1/cnodeath) )

log on  log2
metabias9 logor selogor, egger
log off log2
log on  log3
metabias  logor selogor, egger
log off log3




**************************************
* Recreate examples from metacum.hlp *
**************************************

// Risk difference from raw cell counts, random effects model, "label" specification
log on  log2
metacum tdeath tnodeath cdeath cnodeath, nograph ///
	rd random label(namevar=id, yearid=year)
log off log2
log on  log3
metan tdeath tnodeath cdeath cnodeath, cumulative nograph ///
	rd random label(namevar=id, yearid=year)
log off log3

 
// Generate log odds ratio and standard error.
// Graph has exponential form, scale is forced within set limits and ticks added.
// Data columns syntax used and effect label specified.
log on  log2
metacum logor selogor, eform xlabel(0.6, 0.8, 1, 1.2, 1.4, 1.6) ///
	force xtick(0.7, 0.9, 1.1, 1.3, 1.5) lcols(id year country) effect(Odds ratio)
log off log2
graph save "$Graphs/graph37_metan9"
log on  log3
metan logor selogor, cumulative eform xlabel(0.6, 0.8, 1, 1.2, 1.4, 1.6) ///
	force xtick(0.7, 0.9, 1.1, 1.3, 1.5) lcols(id year country) effect(Odds ratio)
log off log3
graph save "$Graphs/graph37_metan"




*****************************************
* Recreate examples from meta(n)inf.hlp *
*****************************************

// N.B. -metainf- is the original program, but no longer runs as it is based on the old command -meta-,
//  which is now in use as part of the official Stata 16 meta-analysis package
// -metaninf- is an update to -metainf- so that the underlying analysis is done by -metan- instead.

// N.B. it is unclear which dataset(s) should be used for the example code in these help files.
// Therefore, the example code has been minimally modified so as to run on the standard -metan- example dataset.

forvalues i = 1/15 {
	macro drop S_`i'
}
macro drop S_51
log on  log2
metaninf logor selogor, label(namevar=id, yearid=year) nograph
log off log2
log on  log3
metan    logor selogor, label(namevar=id, yearid=year) nograph influence
log off log3

forvalues i = 1/15 {
	macro drop S_`i'
}
macro drop S_51
log on  log2
metaninf logor selogor, random label(namevar=id, yearid=year) nograph
log off log2
log on  log3
metan    logor selogor, random label(namevar=id, yearid=year) nograph influence
log off log3

forvalues i = 1/15 {
	macro drop S_`i'
}
macro drop S_51
log on  log2
metaninf logor selogor, random eform label(namevar=id, yearid=year)
log off log2
graph save "$Graphs/graph38_metan9"
log on  log3
metan    logor selogor, random eform label(namevar=id, yearid=year) influence
log off log3
graph save "$Graphs/graph38_metan"

forvalues i = 1/15 {
	macro drop S_`i'
}
macro drop S_51
log on  log2
metaninf tdeath tnodeath cdeath cnodeath, fixedi or label(namevar=id, yearid=year)
log off log2
graph save "$Graphs/graph39_metan9"
log on  log3
metan    tdeath tnodeath cdeath cnodeath, fixedi or label(namevar=id, yearid=year) influence
log off log3
graph save "$Graphs/graph39_metan"

forvalues i = 1/15 {
	macro drop S_`i'
}
macro drop S_51
log on  log2
metaninf tsample tmean tsd csample cmean csd, cohen notable 
log off log2
graph save "$Graphs/graph40_metan9"
log on  log3
metan    tsample tmean tsd csample cmean csd, cohen notable influence
log off log3
graph save "$Graphs/graph40_metan"




***************************************
* Recreate examples from metaprop.hlp *
***************************************

* Example 1
// Pooling proportions from raw cell counts, grouped by triage group, with "label" specification, x-axis label set, ticks on x-axis added,
//   suppressed weights, increased text size, changes on the box effect estimate, a red diamond for the confidence intervals of the pooled
//   effect estimate, a vertical line at zero, a red dashed line, for the pooled effect estimate, e.t.c.

// The dataset used in this example has been to produce figure one in Marc Arbyn et al. (2009).

use "$Datasets/arbyn2009jcellmolmedfig1.dta", clear

log on  log2
metaprop num denom, random by(tgroup) cimethod(exact) ///
	label(namevar=author, yearvar=year) ///
	xlab(.25,0.5,.75,1) xline(0, lcolor(black)) ///
	subti("Atypical cervical cytology", size(4)) ///
	xtitle("Proportion",size(2)) nowt ///
	olineopt(lcolor(red) lpattern(shortdash)) ///
	plotregion(icolor(ltbluishgray)) ///
	diamopt(lcolor(red)) ///
	pointopt(msymbol(x) msize(0)) boxopt(msymbol(S) mcolor(black)) ///
	astext(70) texts(150)
log off log2
graph save "$Graphs/graph41_metan9"
log on  log3
metan num denom, proportion random by(tgroup) cimethod(exact) ///
	label(namevar=author, yearvar=year) ///
	forestplot(xlab(.25 0.5 .75 1) xline(0, lcolor(black)) ///
	subti("Atypical cervical cytology", size(4)) ///
	xtitle("Proportion",size(2)) nowt ///
	olineopt(lcolor(red) lpattern(shortdash)) ///
	plotregion(icolor(ltbluishgray)) ///
	diamopt(lcolor(red)) classic boxsca(50))
log off log3
graph save "$Graphs/graph41_metan"

	
* Example 2
// Pooling proportions from raw cell counts with Freeman-Tukey double arcsine transformation
//   and exact confidence intervals for the individual studies.

// The dataset used in this example produced the top-left graph in figure one in Ioanna Tsoumpou et al. (2009).

use "$Datasets/tsoumpou2009cancertreatrevfig2WNL.dta", clear

log on  log2
metaprop p16p p16tot, random ftt cimethod(exact) ///
	label(namevar=author, yearvar=year) sortby(year author) ///
	xlab(0.1,.2, 0.3,0.4,0.5,0.6,.7,0.8, 0.9, 1) xline(0, lcolor(black)) ///
	ti(Positivity of p16 immunostaining, size(4) color(blue)) ///
	subti("Cytology = WNL", size(4) color(blue)) ///
	xtitle(Proportion, size(3)) nowt nostats ///
	olineopt(lcolor(red) lpattern(shortdash)) ///
	diamopt(lcolor(black)) ///
	pointopt(msymbol(x) msize(0)) boxopt(msymbol(S) mcolor(black)) ///
	astext(70) texts(100)
log off log2
graph save "$Graphs/graph42_metan9"
log on  log3
metan p16p p16tot, proportion random ftt cimethod(exact) ///
	label(namevar=author, yearvar=year) sortby(year author) ///
	forestplot(xlab(0.1(0.1)1) xline(0, lcolor(black)) ///
	ti(Positivity of p16 immunostaining, size(4) color(blue)) ///
	subti("Cytology = WNL", size(4) color(blue)) ///
	xtitle(Proportion, size(3)) nowt nostats ///
	olineopt(lcolor(red) lpattern(shortdash)) ///
	diamopt(lcolor(black)) classic)
log off log3
graph save "$Graphs/graph42_metan"
	

* Example 3
// The dataset used in this example produced the top-left graph in figure one in Ioanna Tsoumpou et al. (2009).

use "$Datasets/tsoumpou2009cancertreatrevfig2HSIL.dta", clear

log on  log2
metaprop p16p p16tot, random ftt cimethod(exact) ///
	label(namevar=author, yearvar=year) sortby(year author) ///
	xlab(0.1,.2, 0.3,0.4,0.5,0.6,.7,0.8, 0.9, 1) xline(0, lcolor(black)) ///
	ti(Positivity of p16 immunostaining, size(4) color(blue)) ///
	subti("Cytology = HSIL", size(4) color(blue)) ///
	xtitle(Proportion,size(3)) nowt nostats ///
	olineopt(lcolor(red) lpattern(shortdash)) ///
	diamopt(lcolor(black)) ///
	pointopt(msymbol(x) msize(0)) boxopt(msymbol(S) mcolor(black)) ///
	astext(70) texts(100)
log off log2
graph save "$Graphs/graph43_metan9"
log on  log3
metan p16p p16tot, proportion random ftt cimethod(exact) ///
	label(namevar=author, yearvar=year) sortby(year author) ///
	forestplot(xlab(0.1(0.1)1) xline(0, lcolor(black)) ///
	ti(Positivity of p16 immunostaining, size(4) color(blue)) ///
	subti("Cytology = HSIL", size(4) color(blue)) ///
	xtitle(Proportion, size(3)) nowt nostats ///
	olineopt(lcolor(red) lpattern(shortdash)) ///
	diamopt(lcolor(black)) classic)
log off log3
graph save "$Graphs/graph43_metan"



*******************************************************

log close log1
log close log2
log close log3

	

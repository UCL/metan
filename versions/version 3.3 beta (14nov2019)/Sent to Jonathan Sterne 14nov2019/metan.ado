* metan.ado
* Study-level (aka "aggregate-data" or "published data") meta-analysis

*! version 4.0  14nov2019
*! Current version by David Fisher
*! Previous versions by Ross Harris and Michael Bradburn




*****************************
* metan.ado version history *
*****************************
// by Ross Harris and others; further details at end

* Based on previous version: 1.86 1Apr2004
* Based on 2.34 11May2007
* Based on 3.01 07Jul2008

* 3.03 19May2009
* fixes prediction interval calculation for non-ratio measures and change of variable type in lcols() rcols()
* think latter is ok actually- check download, perhaps was using old version!

* 3.04 21Sep2010
* fixed small bug in counts option (`rawcounts' var truncated as str20)


*******************************
* admetan.ado version history *
*******************************
// by David Fisher; further details at end

* originally written by David Fisher, June 2013
// to perform the second stage of two-stage IPDMA, using ipdmetan.ado

* version 1.0  David Fisher  31jan2014
* version 2.0  David Fisher  11may2017
// Major update to extend functionality beyond "estimation commands" (cf ipdmetan);
//   now has most of the functionality of -metan-

* version 2.1  David Fisher  14sep2017
// various bug fixes

* version 3.0  David Fisher  08nov2018
// IPD+AD code now moved to ipdmetan.ado
//   so that admetan is completely self-contained, with minimal reference to -ipdmetan-

* version 3.1  David Fisher  04dec2018
// various bug fixes

* version 3.2  David Fisher  28jan2019
// bug fixes;  added some new estimators

* version 3.3 (beta; never released)  David Fisher 30aug2019
// improvements to handling of zero cells;  added Tarone and CMH statistics for M-H


program define metan, rclass

	version 11.0
	local version : di "version " string(_caller()) ":"

	return hidden local metan_version "4.0"
	
	syntax varlist(numeric min=2 max=6) [if] [in] [, ///
		STUDY(passthru) LABEL(passthru) BY(passthru) /// label() for backward-compatibility with metan.ado
		CLEAR FORESTplot(passthru) IPDMETAN(string)  /// forestplot (ultimately -twoway-) options;  options passed through from -ipdmetan- (if applicable)
		 * ]
	
	marksample touse, novarlist		// `novarlist' option so that entirely missing/nonexistent studies/subgroups may be included
	local invlist `varlist'			// list of "original" vars passed by the user to the program 

	
	*******************
	* Initial parsing *
	*******************

	** Parse -forestplot- options to extract those relevant to -metan-
	// N.B. Certain options may be supplied EITHER to metan directly, OR as sub-options to forestplot()
	//  with "forestplot options" prioritised over "metan options" in the event of a clash.
	
	// These options are:
	// effect options parsed by CheckOpts (e.g. `rr', `rd', `md', `smd', `wmd', `log')
	// nograph, nohet, nooverall, nosubgroup, nowarning, nowt, nostats
	// effect, hetstat, lcols, rcols, plotid, ovwt, sgwt, sgweight
	// cumulative, efficacy, influence, interaction
	// counts, group1, group2 (for compatibility with previous version of metan.ado)
	// rfdist, rflevel (for compatibility with previous version of metan.ado)

	// N.B. if -metan- was called by -ipdmetan- , some of this may already have been done
	
	cap nois ParseFPlotOpts, cmdname(`cmdname') mainprog(metan) options(`options') `forestplot'
	if _rc {
		if `"`err'"'==`""' {
			if _rc==1 nois disp as err `"User break in {bf:metan.ParseFPlotOpts}"'
			else nois disp as err `"Error in {bf:metan.ParseFPlotOpts}"'
		}
		c_local err noerr		// tell -ipdmetan- not to also report an "error in {bf:metan}"
		exit _rc
	}
	
	local eform    `s(eform)'
	local log      `s(log)'
	local summstat `s(summstat)'
	local effect     `"`s(effect)'"'
	local opts_adm   `"`s(opts_parsed)' `s(options)'"'		// options as listed above, plus other options supplied directly to -metan-
	local opts_fplot `"`s(opts_fplot)'"'					// other options supplied as sub-options to forestplot() 
		
	
	** -metan- called by -ipdmetan-
	// If -metan- was not called directly, but from within -ipdmetan-,
	//   then the following extra options are needed now (other extra options are parsed later e.g. by BuildResultsSet):
	// `ipdmetan' :  calling program was -ipdmetan-
	// `interaction' :  -ipdmetan- fitted an interaction model (needed for ParseFPlotOpts)
	// `preserve' :  implies that data is already under -preserve- (set by ipdmetan); this will cancel -preserve- if set by -metan-
	// `_USE' may already be defined by -ipdmetan-;  if not, we generate _USE==1 and alter later if necessary
	// Other options passed from -ipdmetan- are stored in `opts_ipdm' for later parsing (e.g. when displaying results text on-screen)
	
	if "`clear'"=="" local preserve preserve	// default is to preserve data later, if forestplot/saving [UNLESS option -clear- is used!]
	
	if trim(`"`ipdmetan'"') != `""' {
		local 0 `", `ipdmetan'"'
		syntax, [USE(varname numeric) SOURCE(varname numeric) STORED(namelist) PRESERVE * ]
		local _USE : copy local use
		local opts_ipdm `"`options' source(`source') ipdmetan"'	// `source' is needed both by main -metan- routine and by BuildResultsSet
																// (N.B. `options' and `ipdmetan' are needed by PrintDesc but *not* necessarily by BuildResultsSet)
		local orbyad `"(or {bf:byad}) "'						// for warning/error text later
	}
	
	// If `_USE' not already defined (e.g. called by -ipdmetan- ), generate it now:
	if `"`_USE'"'==`""' {
		tempvar _USE							// Note that `_USE' is defined if-and-only-if `touse'
		qui gen byte `_USE' = 1 if `touse'		// i.e.  !missing(`_USE') <==> `touse'
	}

	
	
	****************************************
	* Establish basic data characteristics *
	****************************************
	
	** Parse `study' and `by' *
	// Checks for problems with `study' and `by'
	//  and, amongst other things, converts them from string to numeric if necessary
	tempname newstudylab newbylab
	tempvar  newstudy    newby	
	
	local 0 `", `opts_adm'"'
	syntax [, LCols(passthru) * ]
	local opts_adm `"`macval(options)'"'
	
	cap nois ProcessLabels `invlist' if `touse', `study' `label' `by' `lcols' ///
		newstudy(`newstudy')       newby(`newby') ///
		newstudylab(`newstudylab') newbylab(`newbylab')
	
	if _rc {
		if _rc==1 nois disp as err `"User break in {bf:metan.ProcessLabels}"'
		else nois disp as err `"Error in {bf:metan.ProcessLabels}"'
		c_local err noerr		// tell -ipdmetan- not to also report an "error in {bf:metan}"
		exit _rc
	}
	
	// In case `lcols' has been modified, return it back to main routine
	if `"`s(lcols)'"'!=`""' local opts_adm `"`macval(opts_adm)' lcols(`s(lcols)')"'
	local _STUDY `s(study)'
	local _BY `s(by)'
	local bymissing `s(bymissing)'
	if `"`s(sfmtlen)'"'!=`""' local sfmtlen sfmtlen(`s(sfmtlen)')
	
	if `"`s(bymissing)'"'==`""' markout `touse' `_BY'
	if `"`s(smissing)'"'==`""'  markout `touse' `_STUDY'
	
	// `_STUDY' and `_BY' are the "working" variables from now on; guaranteed numeric.
	// We don't need `study' and `by' anymore; instead `_STUDY' and `_BY' indicate the existence of these variables (e.g. in validity checks).

	
	** Process `invlist' to finalise `summstat', and to establish method of constructing _ES, _seES
	// (and also detect observations with insufficient data; _USE==2)
	cap nois ProcessInputVarlist `_USE' `invlist' if `touse', ///
		summstat(`summstat') `eform' `log' `opts_adm'

	if _rc {
		if _rc==2000 nois disp as err "No studies found with sufficient data to be analysed"
		else if _rc==1 nois disp as err `"User break in {bf:metan.ProcessInputVarlist}"'
		else nois disp as err `"Error in {bf:metan.ProcessInputVarlist}"'
		c_local err noerr		// tell -ipdmetan- not to also report an "error in {bf:metan}"
		exit _rc
	}

	local opts_adm `"`s(options)'"'
	if `"`effect'"'==`""' local effect `"`s(effect)'"'						// don't override user-specified value
		
	if `"`s(summorig)'"'!=`""' local summorig summorig(`s(summorig)')
	local summstat    `s(summstat)'
	local params    = `s(params)'
	local eform       `s(eform)'
	local log         `s(log)'
	local citype      `s(citype)'

	return local citype `citype'						// citype is now established
	
	
	** Identify models
	// Process meta-analysis modelling options
	// (random-effects, test & het stats, etc.)
	// Could be given in a variety of ways, e.g. stand-alone options, random(), second(), model() etc.
	cap nois ProcessModelOpts, `opts_adm' summstat(`summstat') `summorig' params(`params')
	if _rc {
		if `"`err'"'==`""' {
			if _rc==1 nois disp as err `"User break in {bf:metan.ProcessModelOpts}"'
			else nois disp as err `"Error in {bf:metan.ProcessModelOpts}"'
		}
		c_local err noerr		// tell -ipdmetan- not to also report an "error in {bf:metan}"
		exit _rc
	}

	local rownames     `s(rownames)'
	local modellist    `s(modellist)'
	local teststatlist `s(teststatlist)'
	local hetoptlist   `s(hetoptlist)'
	local wgtoptlist   `s(wgtoptlist)'		// to send to DrawTableAD
	local m = `s(m)'
	local modeltext  `"`s(modeltext)'"'	
	local opts_adm   `"`s(opts_adm)'"'		// all other options (rationalised); includes `logrank' and `rfdist'
	
	return scalar m = `s(m)'
	return local model `s(modellist)'		// model(s) are now established
	return local hetopt `s(hetoptlist)'		// heterogeneity options are now established
	forvalues j = 1 / `m' {
		local model`j'opts `"`s(model`j'opts)'"'
		if `m'==1 return local modelopts    `"`s(model`j'opts)'"'		// return model options
		else      return local model`j'opts `"`s(model`j'opts)'"'
	}

	if `"`s(summnew)'"'==`"or"' {
		local summstat or
		local effect `"Odds Ratio"'
	}
	if "`summstat'"!="" {								// summstat is now established (can be missing)
		if inlist("`summstat'", "cohen", "glass", "hedges") local usummstat = strproper("`summstat'") + " SMD"
		else local usummstat = upper("`summstat'")
		return local measure `"`usummstat'"'
	}
	local second `s(second)'		// marker of whether "second" option was used
	local userwgt `s(userwgt)'		// marker of whether user-defined weight was used (c.f. previous versions of -metan-)


	***************************************
	* Prepare for meta-analysis modelling *
	***************************************
	
	** Extract options relevant to PerformMetaAnalysis
	//  and carry out basic option compatibility checks
	local 0 `", `opts_adm'"'
	syntax [, CUmulative INFluence SORTBY(varname) noOVerall noSUbgroup noSECsub SUMMARYONLY INTERaction OVWt SGWt ALTWt ///
		LOGRank NPTS(string) noINTeger KEEPOrder KEEPAll noTABle noGRaph noHET SAVING(passthru) ///
		noKEEPVars noRSample        /// whether to leave behind study-estimate variables
		LEVEL(passthru) TSQLEVEL(passthru) RFLEVEL(passthru) * ]

	local keepvars = cond(`"`rsample'"'!=`""', `"nokeepvars"', `"`keepvars'"')			// noRSample implies noKEEPVars
	local opts_adm `"`macval(options)'"'	// remaining options
											// [note that npts(string) is NOT now part of `opts_adm'; it stands alone]	
	
	// cumulative and influence
	// if `by', cumulative *must* be done by subgroup and not overall ==> nooverall is "compulsory"
	if `"`cumulative'"'!=`""' & `"`influence'"'!=`""' {
		disp as err `"Cannot specify both {bf:cumulative} and {bf:influence}; please choose just one"'
		exit 198
	}
	if `"`cumulative'"'!=`""' {
		if `"`subgroup'"'!=`""' {
			disp as err `"Note: {bf:nosubgroup} is not compatible with {bf:cumulative} and will be ignored"'
			local subgroup
		}
		if `"`summaryonly'"'!=`""' {
			disp as err `"Options {bf:cumulative} and {bf:summaryonly} are not compatible"'
			exit 198
		}
		
		if `"`_BY'"'==`""' {
			if `"`overall'"'!=`""' {
				disp as err `"Note: {bf:nooverall} is not compatible with {bf:cumulative} (unless with {bf:by()}) and will be ignored"'
				local overall
			}
		}
		else {
			if `"`overall'"'!=`""' {
				disp as err `"Note: {bf:nooverall} is compulsory with {bf:cumulative} and {bf:by()}"'
			}
		}
	}
	else if `"`influence'"'!=`""' & `"`summaryonly'"'!=`""' {
		disp as err `"Note: {bf:influence} is not compatible with {bf:summaryonly} and will be ignored"'
		local influence
	}
	
	// Multiple models cannot be specified with cumulative or influence (for simplicity)
	if `"`cumulative'`influence'"'!=`""' & `m' > 1 {
		disp as err `"Option {bf:`cumulative'`influence'} cannot be specified with multiple pooling methods"'
		exit 198
	}
	
	// Compatibility tests for ovwt, sgwt, altwt
	if `"`_BY'"'==`""' {
		if `"`subgroup'"'!=`""' {
			nois disp as err `"Note: {bf:nosubgroup} cannot be specified without {bf:by()} `orbyad'and will be ignored"' 
			local subgroup
		}
	
		if `"`sgwt'"'!=`""' {
			disp as err `"Note: {bf:sgwt} is not applicable without {bf:by()} `orbyad'and will be ignored"'
			local sgwt
		}
		local ovwt ovwt
	}
	else {
		if `"`cumulative'"'!=`""' {
			if `"`ovwt'"'!=`""' disp as err `"Note: {bf:ovwt} is not compatible with {bf:cumulative} and {bf:by()}, and will be ignored"'
			local ovwt
			local sgwt sgwt
		}
		else if `"`influence'"'!=`""' & `"`overall'"'==`""' & `"`sgwt'"'==`""' {
			disp as err `"Note: {bf:influence} with {bf:by()} implies {bf:nosubgroup}, unless option {bf:sgwt} also supplied"'
			local subgroup nosubgroup
		}
		if `"`: word 1 of `modellist''"'=="user1" {
			disp as err "Cannot use option {bf:by()} with user-defined main analysis"
			exit 198
		}
	}
	
	// ovwt and sgwt
	if `"`ovwt'"'!=`""' & `"`sgwt'"'!=`""' {
		disp as err `"Cannot specify both {bf:ovwt} and {bf:sgwt}; please choose just one"'
		exit 198
	}
	if `"`altwt'"'!=`""' & `"`cumulative'`influence'"'==`""' {
		disp as err `"Note: {bf:altwt} is not applicable without {bf:cumulative} or {bf:influence}, and will be ignored"'
		local altwt
	}	
	if `"`ovwt'`sgwt'"'==`""' {
		if `"`_BY'"'!=`""' & `"`overall'"'!=`""' & `"`subgroup'"'==`""' local sgwt sgwt
		else local ovwt ovwt
	}
	

	** Setup tempvars
	// The "core" elements of `outvlist' are _ES, _seES, _LCI, _UCI, _WT and _NN
	// By default, these will be left behind in the dataset upon completion of -metan-
	// `tvlist' = list of elements of `outvlist' that need to be generated as *tempvars* (i.e. do not already exist)
	//  (whilst ensuring that any overlapping elements in `invlist' and `outvlist' point to the same actual variables)
	tokenize `invlist'
	local params : word count `invlist'
	
	// Process "npts(varname)": only permitted with 2- or 3-element varlist AD;
	// that is, "ES, seES", "ES, LCI, UCI", or "OE, V"
	if `"`npts'"'!=`""' {
		if `params' > 3 {
			nois disp as err `"option {bf:npts(}{it:varname}{bf:)} syntax only valid with generic inverse-variance model or with logrank (O-E & V) HR"'
			exit 198
		}
		
		local old_integer `integer'
		local 0 `"`npts'"'
		syntax varname(numeric) [, noPlot noINTeger]
		local _NN `varlist'													// the varname which was stored in npts(varname) will now be stored in _NN
		if `"`integer'"'==`""' local integer `old_integer'
		
		if `"`integer'"'==`""' {
			cap assert int(`_NN')==`_NN' if `touse'
			if _rc {
				nois disp as err `"Non-integer counts found in {bf:npts()} option"'
				exit _rc
			}
		}
		if `"`plot'"'==`""' local opts_adm `"`macval(opts_adm)' npts"'		// send simple on/off option to BuildResultsSet (e.g. for forestplot)
	}
	
	if `params'==2 & "`logrank'"=="" {
		args _ES _seES						// `_ES' and `_seES' supplied
		local tvlist _LCI _UCI				// `_LCI', `_UCI' need to be created (at 95%)
	}
	else if `params'==3 {
		args _ES _LCI _UCI					// `_ES', `_LCI' and `_UCI' supplied (assumed 95% CI)
		
		local tvlist _seES						// `_seES' needs to be created
		if `"`level'"'!=`""' {					// but if level() option supplied, requesting coverage other than 95%
			local tvlist `tvlist' _LCI _UCI		// then tempvars for _LCI, _UCI are needed too
		}
	}
	
	// `params'==4 or 6
	else {
		local tvlist _ES _seES _LCI _UCI						// need to create everything
		if `"`logrank'"'==`""' local tvlist `tvlist' _NN		// including _NN unless `logrank' (as that uses optional `npts')

		if `params'==4 {
			tempvar ccvar
			local ccvaropt ccvar(`ccvar')
		}
	}		
	
	// Finally, _WT always needs to be generated as tempvar
	local tvlist `tvlist' _WT
	
	// Create tempvars based on `tvlist'
	//   and finally create `outvlist' = list of "standard" vars (_ES, _seES, _LCI, _UCI, _WT, _NN; see above).
	foreach tv of local tvlist {
		tempvar `tv'
		qui gen double ``tv'' = .
	}
	local outvlist `_ES' `_seES' `_LCI' `_UCI' `_WT' `_NN'
	

	// If cumulative or influence, need to generate additional tempvars.
	// `xoutvlist' ("extra" outvlist) contains results of each individual analysis
	//   to be printed to screen, displayed in forestplot and stored in saved dataset.
	//   (plus Q, tausq, sigmasq, df from each analysis.)
	// Meanwhile `outvlist' contains effect sizes etc. for each individual *study*, as usual,
	//   which will be left behind in the current dataset.
	if `"`cumulative'`influence'"'!=`""' {
		tempvar use3
		qui gen byte `use3' = 0		// identifier of last estimate, for placement of dotted line in forestplot
		local use3opt `"use3(`use3')"'
		
		local nt = `: word count `rownames''
		forvalues i = 1 / `nt' {
			tempvar tv`i'
			qui gen double `tv`i'' = .
			local xoutvlist `xoutvlist' `tv`i''
		}
		local xoutvlistopt xoutvlist(`xoutvlist')
	}
	// N.B. `xoutvlist' now contains the tempvars which will hold the relevant returned stats...
	//  - with the same contents as the elements of `rownames'
	//  - but *without* npts (as _NN is handled separately)
	//  - and with the addition of a separate weight variable (`_WT2') ... so the total is `nt' - 1 + 1 = `nt'.	
	
	
	
	******************************************
	* Run the actual meta-analysis modelling *
	******************************************
	
	// Generate stable ordering to pass to subroutines... (PerformMetaAnalysis, DrawTableAD, BuildResultsSet)
	// (so sortby() is always specified for these subroutines)
	tempvar obs
	qui gen long `obs' = _n

	// Now, first model is "primary" and will be displayed on screen and in forest plot
	// options such as `ovwt', `sgwt', `altwt' apply here
	// Remaining models are simply fitted, and results saved (in matrices ovstats and bystats).	
	
	// Hence, loop over models *backwards* so that "primary" model is done last
	// and hence "correct" outvlist is left behind

	// Before looping, create list of unique colnames (e.g. if same basic model is run twice, with different options)
	local rest : copy local modellist
	while `"`rest'"'!=`""' {
		gettoken model rest : rest
		if `: list model in mcolnames' {
			local mcolnames = subinstr("`mcolnames'", "`model'", "`model'_1", 1)
			assert !`: list model in mcolnames'
			local j=2
			local newname `model'_`j'
			while `: list newname in mcolnames' {
				local ++j
				local newname `model'_`j'				
			}
			local mcolnames `mcolnames' `newname'
		}
		else local mcolnames `mcolnames' `model'
	}		
	
	tempname checkmat ovstats mwt
	forvalues j = `m' (-1) 1 {
		local model    : word `j' of `modellist'
		local teststat : word `j' of `teststatlist'
		local hetopt   : word `j' of `hetoptlist'

		cap nois PerformMetaAnalysis `_USE' `invlist' if `touse', sortby(`sortby' `obs') by(`_BY') ///
			summstat(`summstat') model(`model') teststat(`teststat') hetopt(`hetopt') `model`j'opts' ///
			outvlist(`outvlist') `xoutvlistopt' rownames(`rownames') ///
			`cumulative' `influence' `overall' `subgroup' `secsub' ///
			`ovwt' `sgwt' `altwt' `use3opt' `ccvaropt' ///
			`logrank' `level' `tsqlevel' `rflevel'
			
		if _rc {
			if `"`err'"'==`""' {
				if _rc==1 nois disp as err `"User break in {bf:metan.PerformMetaAnalysis}"'
				else nois disp as err `"Error in {bf:metan.PerformMetaAnalysis}"'
			}
			c_local err noerr		// tell -ipdmetan- not to also report an "error in {bf:metan}"
			exit _rc
		}
		
		local mcolname : word `j' of `mcolnames' 
		if (`"`overall'"'==`""' | `"`ovwt'"'!=`""') & !inlist("`model'", "user1", "user2") {
			mat `checkmat' = r(ovstats)
			cap assert rowsof(`checkmat') > 1
			if _rc {
				disp as err `"Matrix r(ovstats) could not be created"'
				disp as err `"Error in {bf:metan.PerformMetaAnalysis}"'
				c_local err noerr		// tell -ipdmetan- not to also report an "error in {bf:metan}"
				exit _rc
			}
			mat colnames `checkmat' = `mcolname'
			mat `ovstats' = `checkmat', nullmat(`ovstats')
		}

		if ((`"`by'"'!=`""' & `"`subgroup'"'==`""') | `"`sgwt'"'!=`""') & !inlist("`model'", "user1", "user2") {
			tempname bystats`j'
			mat `bystats`j'' = r(bystats)
			cap assert rowsof(`bystats`j'') > 1
			if _rc {
				disp as err `"Matrix r(bystats) could not be created"'
				disp as err `"Error in {bf:metan.PerformMetaAnalysis}"'
				c_local err noerr		// tell -ipdmetan- not to also report an "error in {bf:metan}"
				exit _rc
			}
			mat colnames `bystats`j'' = `mcolname'
		
			if `m'==1 | `"`: word 1 of `modellist''"'=="user1" {
				return matrix bystats = `bystats`j'', copy
			}
			else return matrix bystats`j' = `bystats`j'', copy
			
			local bystatslist `bystats`j'' `bystatslist'	// form list in reverse order
			
			// Model-specific subgroup weights
			mat `mwt' = r(mwt) \ nullmat(`mwt')
		}
	}		// end forvalues j = `m' (-1) 1

	local nby = r(nby)

	cap {
		confirm matrix `ovstats'
		assert rowsof(`ovstats') > 1
	}
	if !_rc {
		return matrix ovstats = `ovstats', copy
	}
	else local ovstats	// marker of whether (valid) matrix exists
	
	// Display error messages: each unique error message is only displayed once
	if (`"`by'"'!=`""' & `"`subgroup'"'==`""') | `"`sgwt'"'!=`""' {
		if `nrc2000' nois disp as err "Note: insufficient data in one or more subgroups"
		if `nrc2002' nois disp as err "Note: pooling failed in one or more subgroups"
		if `ntausq'  nois disp as err "Note: tau{c 178} point estimate not successfully estimated in one or more subgroups"
		if `ntsqlci' nois disp as err "Note: tau{c 178} lower confidence limit not successfully estimated in one or more subgroups"
		if `ntsquci' nois disp as err "Note: tau{c 178} upper confidence limit not successfully estimated in one or more subgroups"
		if `nefflci' nois disp as err "Note: lower confidence limit of effect size not successfully estimated in one or more subgroups"
		if `neffuci' nois disp as err "Note: upper confidence limit of effect size not successfully estimated in one or more subgroups"
	}
	if `nrfd'  nois disp as err "Note: Prediction intervals are undefined if less than three studies"
	if `nsg'   nois disp as err "Note: one or more subgroups contain only a single valid estimate"
	if `nmiss' nois disp as err "Note: Patient numbers are missing in one or more trials"

	
	// Collect numbers of studies and patients (relevant to "primary" model)
	tempname k totnpts
	scalar `k' = r(k)
	scalar `totnpts' = r(n)
	return scalar k = r(k)
	return scalar n = r(n)

	// Subgroup statistics
	if `"`_BY'"'!=`""' & `"`subgroup'`overall'"'==`""' & /*`"`pool'"'==`""'*/ `"`ovstats'"'!=`""' {
		tempname Q_ov Qdf_ov Qsum Qdiff Fstat
		scalar `Q_ov'   = `ovstats'[rownumb(`ovstats', "Q"), 1]
		scalar `Qdf_ov' = `ovstats'[rownumb(`ovstats', "Qdf"), 1]
		scalar `Qsum'  = r(Qsum)					// returned from last call to PerformMetaAnalysis
		scalar `Qdiff' = `Q_ov' - `Qsum'			// between-subgroup heterogeneity (Qsum = within-subgroup het.)
		scalar `Fstat' = (`Qdiff'/(`nby' - 1)) / (`Qsum'/(`Qdf_ov' - `nby' + 1))		// corrected 17th March 2017
		
		return scalar Qdiff = `Qdiff'
		return scalar Qsum  = `Qsum'
		return scalar F = `Fstat'
		return scalar nby = `nby'
	}

	// Return other scalars (relevant to "primary" model)
	//  some of which are also saved in r(ovstats)
	return scalar eff    = r(eff)
	return scalar se_eff = r(se_eff)
	return scalar Q    = r(Q)
	return scalar Isq  = r(Isq)
	return scalar HsqM = r(HsqM)
	
	if `params'==4 {
		return scalar tger = r(tger)
		return scalar cger = r(cger)
		if !missing(r(RR)) return scalar RR = r(RR)
		if !missing(r(OR)) return scalar OR = r(OR)
	}
	
	if `"`ovstats'"'!=`""' {
		if !missing(`ovstats'[rownumb(`ovstats', "tausq"), 1]) {
			return scalar Qr = r(Qr)
			return scalar tausq   = r(tausq)
			return scalar sigmasq = r(sigmasq)
		}
		if !missing(`ovstats'[rownumb(`ovstats', "tsq_lci"), 1]) {
			return scalar tsq_var    = r(tsq_var)
			return scalar rc_eff_lci = r(rc_eff_lci)
			return scalar rc_eff_uci = r(rc_eff_uci)
			return scalar rc_tausq   = r(rc_tausq)
			return scalar rc_tsq_lci = r(rc_tsq_lci)
			return scalar rc_tsq_uci = r(rc_tsq_uci)
		}
	}

	// Historical returned results
	// (for backward compatibility with previous versions of -metan-)
	if "`eform'"!="" {
		return historical scalar ES = exp(r(eff))
		return historical scalar selogES = r(se_eff)
		return historical scalar ci_low = exp(r(eff_lci))
		return historical scalar ci_upp = exp(r(eff_uci))
		
		if "`second'"!="" & "`ovstats'"!="" {
			return historical scalar ES_2      = exp(`ovstats'[rownumb(`ovstats', "eff"), 2])
			return historical scalar selogES_2 =     `ovstats'[rownumb(`ovstats', "se_eff"), 2]
			return historical scalar ci_low_2  = exp(`ovstats'[rownumb(`ovstats', "eff_lci"), 2])
			return historical scalar ci_upp_2  = exp(`ovstats'[rownumb(`ovstats', "eff_uci"), 2])
		}
	}
	else {
		return historical scalar ES = r(eff)
		return historical scalar seES = r(se_eff)
		return historical scalar ci_low = r(eff_lci)
		return historical scalar ci_upp = r(eff_uci)

		if "`second'"!="" & "`ovstats'"!="" {
			return historical scalar ES_2     = `ovstats'[rownumb(`ovstats', "eff"), 2])
			return historical scalar seES_2   = `ovstats'[rownumb(`ovstats', "se_eff"), 2]
			return historical scalar ci_low_2 = `ovstats'[rownumb(`ovstats', "eff_lci"), 2]
			return historical scalar ci_upp_2 = `ovstats'[rownumb(`ovstats', "eff_uci"), 2]
		}		
	}
	if "`second'"!="" & "`ovstats'"!="" {		
		tokenize `modellist'
		if `"`3'"'!=`""' {
			disp as err "Invalid use of option {bf:second()}"
			exit 198
		}
		forvalues i = 1/2 {
			if "``i''"=="mh" local method_`i' "M-H"
			else if "``i''"=="peto" local method_`i' "Peto"
			else if "``i''"=="fe" local method_`i' "I-V"
			else if "``i''"=="dl" local method_`i' "D+L"
			else if "``i''"=="user`i'" local method_`i' "USER"
			else {
				disp as err "Invalid use of option {bf:second()}"
				exit 198
			}
			if `i'==1 & "`userwgt'"!="" local method_1 "*"
			return historical local method_`i' `method_`i''
		}
	}

	if !missing(r(z)) {
		return historical scalar z = r(z)
		return historical scalar p_z = r(pvalue)
	}
	return historical scalar i_sq = r(Isq)
	return historical scalar het = r(Q)
	return historical scalar df = r(Qdf)
	return historical scalar p_het = chi2tail(r(Qdf), r(Q))
	if !missing(r(chi2)) {
		return historical scalar chi2 = r(chi2)
		return historical scalar p_chi2 = chi2tail(1, r(chi2))
	}
	if !missing(r(tausq)) {
		return historical scalar tau2 = r(tausq)
	}
	// END OF RETURN HISTORICAL
	
	
	// Generate study-level CIs for "primary" model
	// (unless pre-specified)
	cap nois GenConfInts `invlist' if `touse' & `_USE'==1, ///
		citype(`citype') `df' `model1opts' outvlist(`outvlist') `level'
	if _rc {
		nois disp as err `"Error in {bf:metan.GenConfInts}"'
		c_local err noerr		// tell -ipdmetan- not to also report an "error in {bf:metan}"
		exit _rc
	}
	
	// Now switch functions of `outvlist' and `xoutvlist',
	//  so that the cumul/infl versions of _ES, _seES etc. are stored in `outvlist' (so overwriting the "standard" _ES, _seES etc.)
	//  for display onscreen, in forest plot and in saved dataset.
	// Then `xoutvlist' just contains the remaining "extra" tempvars _Q, _Qdf, _tausq etc.
	if `"`xoutvlist'"'!=`""' {
	
		// Firstly, tidy up: If nokeepvars *and* altwt not specified, then we can drop
		//   any members of `outvlist' that didn't already exist in the dataset
		if `"`keepvars'"'!=`""' & `"`altwt'"'==`""' {
			foreach v of local outvlist {
				if `: list v in tvlist' {		// i.e. if `v' was created by either -ipdmetan- or -metan-
					drop ``v''
				}
			}
		}

		// Now reset `outvlist'
		// Recall that xoutvlist = rownames - npts + _WT2
		local npts npts
		local rownames : list rownames - npts
		tokenize `xoutvlist'
		args `rownames' _WT2
		
		local outvlist `eff' `se_eff' `eff_lci' `eff_uci' `_WT2' `_NN'
		local xoutvlist : list xoutvlist - outvlist
		
		tokenize `outvlist'
		args _ES _seES _LCI _UCI _WT _NN
	}

	
	
	********************************
	* Print summary info to screen *
	********************************

	* Print number of studies/participants to screen
	//  (NB nos. actually analysed as opposed to the number supplied in original data)

	// If passed from -ipdmetan- with option ad(), need to print non-standard text:
	if `"`source'"'!=`""' {
		tempname KIPD totnptsIPD
		qui count if `touse' & inlist(`_USE', 1, 2) & `source'==1
		scalar `KIPD' = r(N)
		if r(N) {
			if "`_NN'"!="" {
				summ `_NN' if `touse' & inlist(`_USE', 1, 2) & `source'==1, meanonly
				scalar `totnptsIPD' = cond(r(N), r(sum), .)			// if KIPD>0 but no _NN, set to missing
			}
			else scalar `totnptsIPD' = .
		}
		else scalar `totnptsIPD' = 0

		tempname KAD totnptsAD
		qui count if `touse' & inlist(`_USE', 1, 2) & `source'==2
		scalar `KAD' = r(N)
		if r(N) {
			if "`_NN'"!="" {
				summ `_NN' if `touse' & inlist(`_USE', 1, 2) & `source'==2, meanonly
				scalar `totnptsAD' = cond(r(N), r(sum), .)			// if KAD>0 but no _NN, set to missing
			}
			else scalar `totnptsAD' = .
		}
		else scalar `totnptsAD' = 0

		disp as text _n "Studies included from IPD: " as res `KIPD'
		if "`keepall'"!="" {
			qui count if `touse' & `_USE'==2 & `source'==1
			assert r(N) <= `KIPD'
			if r(N) {
				local plural = cond(r(N)==1, "study", "studies")
				disp as text "  plus " as res `r(N)' as text " `plural' with insufficient data"
			}
		}		
		local dispnpts = cond(missing(`totnptsIPD'), "Unknown", string(`totnptsIPD'))
		disp as text "Participants included from IPD: " as res "`dispnpts'"
		if "`keepall'"!="" & !missing(`totnptsIPD') {
			summ `_NN' if `touse' & `_USE'==2 & `source'==1, meanonly
			assert r(sum) <= `totnptsIPD'
			if r(sum) {
				local s = cond(r(sum)>1, "s", "")
				disp as text "  plus " as res `r(sum)' as text " participant`s' with insufficient data"
			}
		}
		
		disp as text _n "Studies included from aggregate data: " as res `KAD'
		if "`keepall'"!="" {
			qui count if `touse' & `_USE'==2 & `source'==2
			assert r(N) <= `KAD'
			if r(N) {
				local plural = cond(r(N)==1, "study", "studies")
				disp as text "  plus " as res `r(N)' as text " `plural' with insufficient data"
			}
		}
		local dispnpts = cond(missing(`totnptsAD'), "Unknown", string(`totnptsAD'))
		disp as text "Participants included from aggregate data: " as res "`dispnpts'"
		if "`keepall'"!="" & !missing(`totnptsAD') {
			summ `_NN' if `touse' & `_USE'==2 & `source'==2, meanonly
			assert r(sum) <= `totnptsAD'
			if r(sum) {
				local s = cond(r(sum)>1, "s", "")
				disp as text "  plus " as res `r(sum)' as text " participant`s' with insufficient data"
			}
		}
	}		// end if `"`source'"'
		
	// Standard -metan- summary text:
	else {
		disp _n _c
		disp as text "Studies included: " as res `k'
		if "`keepall'"!="" {
			qui count if `touse' & `_USE'==2
			if r(N) {
				local plural = cond(r(N)==1, "study", "studies")
				disp as text "  plus " as res `r(N)' as text " `plural' with insufficient data"
			}
		}
		local dispnpts = cond(missing(`totnpts'), "Unknown", string(`totnpts'))
		disp as text "Participants included: " as res "`dispnpts'"
		if "`keepall'"!="" & !missing(`totnpts') {
			summ `_NN' if `touse' & `_USE'==2, meanonly
			if r(sum) {
				local s = cond(r(sum)>1, "s", "")
				disp as text "  plus " as res `r(sum)' as text " participant`s' with insufficient data"
			}
		}
	}	
	

	** Full descriptions of `summstat', `esmethod' and `model' options, for printing to screen	
	// Involves `opts_model', so pass to a subroutine

	// Instead of passing `ovstats' and `bystats' to PrintDesc, pass "`pool'"=="nopool" if neither exist
	if `"`ovstats'`bystatslist'"'==`""' local pool nopool
	
	// For PrintDesc and DrawTableAD:
	// If first model uses continuity correction or user-defined weights, pass these to DrawTableAD
	// Additionally, if user-defined second model, also pass model2opts : contains user2stats(ES LCI UCI) and secondstats() [heterogeneity text]
	if `"`: word 2 of `modellist''"'=="user2" {
		local user2opts : copy local model2opts
	}
	
	PrintDesc, summstat(`summstat') modellist(`modellist') wgtoptlist(`wgtoptlist') ///
		`pool' `ccvaropt' `model1opts' `altwt' ///
		`log' `logrank' `cumulative' `influence' `summaryonly' `table' `opts_ipdm'
	
	if `"`s(fpnote)'"'!=`""' local fpnote `"fpnote(`s(fpnote)')"'

	

	*********************************
	* Print results table to screen *
	*********************************

	// Unless no table AND no graph AND no saving/clear, store study value labels in new var "_LABELS"
	if !(`"`table'"'!=`""' & `"`graph'"'!=`""' & `"`saving'"'==`""' & `"`clear'"'==`""') {

		tempvar _LABELS
		cap decode `_STUDY' if `touse', gen(`_LABELS')					// if value label
		if _rc qui gen `_LABELS' = string(`_STUDY') if `touse'			// if no value label

		// missing values of `_STUDY'
		// string() works with ".a" etc. but not "." -- contrary to documentation??
		qui replace `_LABELS' = "." if `touse' & missing(`_LABELS') & !missing(`_STUDY')
	}
	
	// Now remove studies with insufficient data if appropriate
	if `"`keeporder'"'!=`""' local keepall keepall					// `keeporder' implies `keepall'
	if `"`keepall'"'==`""' qui replace `touse' = 0 if `_USE'==2

	// Titles
	if `"`_BY'"'!=`""' {
		local byvarlab : variable label `_BY'
		if `"`byvarlab'"'==`""' local byvarlab Subgroup
		local bytitle `"`byvarlab' and "'
	}
	local svarlab : variable label `_STUDY'
	if `"`svarlab'"'==`""' {
		local svarlab = cond(`"`summaryonly'"'!=`""', `""', `"Study"')
	}
	local stitle `"`bytitle'`svarlab'"'
	if `"`influence'"'!=`""' local stitle `"`stitle' omitted"'

	// Moved Sep 2018
	if `"`effect'"'==`""'      local effect "Effect"
	if `"`log'"'!=`""'         local effect `"log `effect'"'
	if `"`interaction'"'!=`""' local effect `"Interact. `effect'"'	

	cap nois DrawTableAD `_USE' `outvlist' if `touse' & inlist(`_USE', 1, 2), sortby(`sortby' `obs') ///
		modellist(`modellist') hetoptlist(`hetoptlist') wgtoptlist(`wgtoptlist') teststatlist(`teststatlist') ///
		`cumulative' `influence' `overall' `subgroup' `secsub' `summaryonly' `ccvaropt' ///
		labels(`_LABELS') stitle(`stitle') etitle(`effect') modeltext(`modeltext') `model1opts' `user2opts' ///
		study(`_STUDY') by(`_BY') bystatslist(`bystatslist') mwt(`mwt') ovstats(`ovstats') ///
		`ovwt' `sgwt' `eform' `table' `het' `keepvars' `keeporder' `level' `tsqlevel' `opts_adm'

	if _rc {
		nois disp as err `"Error in {bf:metan.DrawTableAD}"'
		c_local err noerr		// tell -ipdmetan- not to also report an "error in {bf:metan}"
		exit _rc
	}		
	
	if `"`r(coeffs)'"'!=`""' {
		tempname coeffs
		mat `coeffs' = r(coeffs)
		return matrix coeffs = `coeffs'
	}
	
	
	
	********************************
	* Build forestplot results set *
	********************************
	
	* 1. Create the results-set structure
	//  (including some tempvars; hence the subroutine)
	* 2. Send the data to -forestplot- to create the forest plot
	* 3. Save the results-set (in Stata "dta" format)
	//  (after renaming tempvars to permanent names)
	//   and with characteristics set so that "forestplot, useopts" can be called.

	`preserve'		// preserve original data (unless passed from ipdmetan already under -preserve-; see earlier)
	
	// Store contents of existing characteristics
	//  with same names as those to be used by BuildResultsSet
	local char_fpuseopts  `"`char _dta[FPUseOpts]'"'
	local char_fpusevlist `"`char _dta[FPUseVarlist]'"'
	
	if `"`saving'"'!=`""' | `"`clear'"'!=`""' | `"`graph'"'==`""' {

		if `"`_STUDY'"'!=`""' {
			label variable `_STUDY' `"`svarlab'"'
		}
		if `"`_BY'"'!=`""' {
			label variable `_BY' `"`byvarlab'"'
		}
		
		cap nois BuildResultsSet `_USE' `invlist' if `touse', labels(`_LABELS') summstat(`summstat') ///
			modellist(`modellist') modeltext(`modeltext') hetoptlist(`hetoptlist') teststatlist(`teststatlist') ///
			sortby(`sortby' `obs') study(`_STUDY') by(`_BY') bystatslist(`bystatslist') mwt(`mwt') ovstats(`ovstats') ///
			`cumulative' `influence' `subgroup' `overall' `secsub' `het' `summaryonly' ///
			`ovwt' `sgwt' `altwt' effect(`effect') `eform' `logrank' `ccvaropt' `model1opts' `user2opts' ///
			outvlist(`outvlist') xoutvlist(`xoutvlist') use3(`use3') `sfmtlen' ///
			forestplot(`opts_fplot' `interaction') `fpnote' `graph' `saving' `clear' ///
			`keepall' `keeporder' `level' `tsqlevel' `rflevel' `opts_adm' `opts_ipdm'
		
		if _rc {
			if `"`err'"'==`""' {
				if _rc==1 nois disp as err `"User break in {bf:metan.BuildResultsSet}"'
				else nois disp as err `"Error in {bf:metan.BuildResultsSet}"'
				nois disp as err `"(Note: meta-analysis model was fitted successfully)"'
			}
			c_local err noerr		// tell -ipdmetan- not to also report an "error in {bf:metan}"
			local rc = _rc
			
			// in case *not* under -preserve- (e.g. if _rsample required)
			summ `_USE', meanonly
			if r(N) & r(max) > 9 {
				qui replace `_USE' = `_USE' / 10	// in case break was while _USE was scaled up -- see latter part of BuildResultsSet
			}
			qui drop if `touse' & !inlist(`_USE', 1, 2)
			
			// clear/restore characteristics
			char _dta[FPUseOpts]    `char_fpuseopts'
			char _dta[FPUseVarlist] `char_fpusevlist'
			exit `rc'
		}
		
		// Restore original data; but preserve it again temporarily while "stored" variables are processed
		//   if all goes well, this -preserve- will be cancelled later with -restore, not- ...
		if `"`preserve'"'!=`""' {
			restore, preserve
		}
			
	}		// end if `"`saving'"'!=`""' | `"`clear'"'!=`""' | `"`graph'"'==`""'
	
	// If no need for results set, go straight to processing of "stored" variables
	//   (after trapping any invalid options in `opts_adm')
	else {
		local 0 `", `opts_adm'"'
		cap nois syntax [, LCols(passthru) RCols(passthru) COUNTS(passthru) EFFIcacy OEV NPTS ///
			EXTRALine(passthru) HETStat(passthru) OVStat(passthru) noHET noWT noSTATs ///
			KEEPAll KEEPOrder noWARNing PLOTID(passthru) ///
			RFDist RFLEVEL(passthru) TSQLEVEL(passthru) ]

		if _rc {
			if `"`err'"'==`""' {
				nois disp as err `"Error in {bf:metan}"'
				nois disp as err `"(Note: meta-analysis model was fitted successfully)"'
			}
			c_local err noerr		// tell -ipdmetan- not to also report an "error in {bf:metan}"
		}	
	}

	// exit early if no -preserve- (e.g. -clear- option, or if called from -ipdmetan- )
	if `"`preserve'"' == `""' exit
		

	** Stored (left behind) variables
	// Unless -noKEEPVars- (i.e. "`keepvars'"!=""), leave behind _ES, _seES etc. in the original dataset
	// List of these "permanent" names = _ES _seES _LCI _UCI _WT _NN ... plus _CC if applicable
	//   (as opposed to `outvlist', which contains the *temporary* names `_ES', `_seES', etc.)
	//   (N.B. this code applies whether or not cumulative/influence options are present)	
	if `"`keepvars'"'==`""' {
	
		cap confirm numeric var `ccvar'
		if !_rc local _CC `ccvar'
		local tostore _ES _seES _LCI _UCI _WT _NN _CC
		
		foreach v of local tostore {
			if `"``v''"'!=`""' {
				if `"``v''"'!=`"`v'"' {		// If pre-existing var has the same name (i.e. was named _ES etc.), nothing needs to be done.
					cap drop `v'			// Else, first drop any existing var named _ES (e.g. left over from previous analysis)
				
					// If in `tvlist', we can directly rename
					if `: list v in tvlist' {
						qui rename ``v'' `v'
					}
					
					// Otherwise, ``v'' is a pre-existing var which needs to be retained at program termination
					// so, use -clonevar-
					else qui clonevar `v' = ``v'' if `touse'
				}
				local `v' `v'				// for use with subsequent code (local _ES now contains "_ES", etc.)
			}
		}
		qui compress `tvlist'
		order `_ES' `_seES' `_LCI' `_UCI' `_WT' `_NN' `_CC' `_rsample', last
		
		// variable labels
		label variable `_ES' "Effect size"
		label variable `_seES' "Standard error of effect size"
		label variable `_WT' "% Weight"
		format `_WT' %6.2f
		if `"`_NN'"'!=`""' {
			label variable `_NN' "No. pts"
		}
		if `"`_CC'"'!=`""' {
			label variable `_CC' "Continuity correction applied?"
		}
		if `"`_rsample'"'==`""' {
			cap drop _rsample
			qui gen byte _rsample = `_USE'==1		// this shows which observations were used
			label variable _rsample "Sample included in most recent model"
		}
		
		// Obtain `level' for labelling LCI/UCI
		local 0 `", `level'"'
		syntax [, LEVEL(real 95)]
		label variable `_LCI' "`level'% lower confidence limit"
		label variable `_UCI' "`level'% upper confidence limit"
		char define `_LCI'[Level] `level'
		char define `_UCI'[Level] `level'
	}	
	
	// else (if -noKEEPVars- specified), check for existence of pre-existing vars named _ES, _seES etc. and give warning if found
	else {
		cap confirm numeric var `ccvar'
		if !_rc {
			local _CC _CC
			local ortext `", {bf:_NN} or {bf:_CC})"'
		}
		else local ortext `" or {bf:_NN}"'

		// If -noKEEPVars- but not -noRSample-, need to create _rsample as above
		if "`rsample'"=="" {

			// create _rsample
			cap drop _rsample
			qui gen byte _rsample = `_USE'==1		// this shows which observations were used
			label variable _rsample "Sample included in most recent model"
			
			local warnlist
			local rc = 111
			foreach v in _ES _seES _LCI _UCI _WT _NN `_CC' {
				cap confirm var `v'
				if !_rc local warnlist `"`warnlist' {bf:`v'}"'
				local rc = min(`rc', _rc)
			}
			if !`rc' {
				disp as err _n `"Warning: option {bf:nokeepvars} specified, but the following "stored" variables already exist:"'
				disp as err `"`warnlist'"'
				disp as err `"Note that these variables are therefore no longer associated with the most recent analysis"'
				disp as err `"(although {bf:_rsample} {ul:is})."'
			}
		}
				
		// -noKEEPVars- *and* -noRSample-
		else {

			// give warning if variable named _rsample already existed
			cap confirm var _rsample
			if !_rc {
				disp as err _n `"Warning: option {bf:norsample} specified, but "stored" variable {bf:_rsample} already exists"'
			}
			local rsrc = _rc

			local warnlist
			local rc = 111
			foreach v in _ES _seES _LCI _UCI _WT _NN _CC {
				cap confirm var `v'
				if !_rc & !`: list v in stored' {
					local warnlist `"`warnlist' {bf:`v'}"'
					local rc = 0
				}
			}
			if !`rc' {
				if !`rsrc' disp as err `"as do the following "stored" variables:"'
				else disp as err _n `"Warning: option {bf:norsample} specified, but the following "stored" variables already exist:"'
				disp as err `"`warnlist'"'			
			}
			local plural = cond(!`rc', "these variables are", "this variable is")
			if !`rsrc' | !`rc' disp as err `"Note that `plural' therefore NOT associated with the most recent analysis."'
		}
	}		
		
	// Clear/restore characteristics
	char _dta[FPUseOpts]    `char_fpuseopts'
	char _dta[FPUseVarlist] `char_fpusevlist'
	
	// Finally, cancel -preserve-
	restore, not
	
end







********************************************************************************

**********************************************
* Stata subroutines called from main routine *  (and its "minor" subroutines)
**********************************************


* Program to process `study' and `by' labels
// (based on ProcessAD.ado but altered quite a bit)
// (called directly by metan.ado)

program define ProcessLabels, sclass sortpreserve

	syntax varlist(numeric min=2 max=6) [if] [in], ///
		NEWSTUDY(name) NEWSTUDYLAB(name) NEWBY(name) NEWBYLAB(name) ///
		[ STUDY(name) LABEL(string) BY(name) LCols(namelist) * ]

	marksample touse, novarlist		// `novarlist' option so that entirely missing/nonexistent studies/subgroups may be included
	local invlist `varlist'			// list of "original" vars passed by the user to the program 
	
	// Current sort order
	tempvar obs
	qui gen long `obs' = _n

	local opts_adm : copy local options
	sreturn clear
	
	** Parse `by'
	// N.B. do this before `study' in case `by' is string and contains missings.
	// Stata sorts string missings to be *first* rather than last.
	if `"`by'"'!=`""' {
		local 0 `"`by'"'
		syntax name [, Missing]		// only a single (var)name is allowed

		cap confirm var `namelist'
		if _rc {
			nois disp as err `"variable {bf:`namelist'} not found"'
			exit 111
		}
		local _BY `namelist'		// `"`_BY'"'!=`""' is a marker of `by' being present in the current data
		if `"`missing'"'==`""' markout `touse' `_BY', strok
		sreturn local bymissing `missing'
	}
	
	** Now, parse `study'
	// label([namevar=namevar], [yearvar=yearvar]) remains valid for backwards-compatibility
	if `"`label'"'!=`""' {	
		if `"`study'"'!=`""' {
			disp as err `"Cannot specify both {bf:label()} and {bf:study()}; please choose just one"'
			exit 198
		}
		
		// while loop taken directly from metan.ado by Ross Harris:
		tokenize "`label'", parse("=,")
		while "`1'"!="" {
			cap assert inlist(`"`1'"', "namevar", "yearvar")
			if _rc local rc = _rc
			else {
				cap confirm var `3'
				if _rc & `: word count `3''==1 {
					disp as err `"Variable {bf:`3'} not found in option {bf:label()}"'
					exit _rc
				}
				local rc = _rc
			}
			if `rc' {
				disp as err `"Syntax of option {bf:label()} is {bf:label(}[{bf:namevar}={it:namevar}]{bf:,} [{bf:yearvar}={it:yearvar}]{bf:)}"'
				exit _rc
			}
			local `1' "`3'"
			mac shift 4
		}
		
		// put name/year variables into appropriate macros
		if `: word count `namevar' `yearvar''==1 local _STUDY `namevar' `yearvar'
		else {
			tempvar _STUDY
			cap confirm string var `namevar'
			if !_rc local namestr `namevar'
			else {
				tempvar namestr
				cap decode `namevar', gen(`namestr')
				if _rc==182 qui gen `namestr' = string(`namevar')	// no value label
			}
			cap confirm string var `yearvar'
			if !_rc local yearstr `yearvar'
			else {
				tempvar yearstr
				cap decode `yearvar', gen(`yearstr')
				if _rc==182 qui gen `yearstr' = string(`yearvar')	// no value label
			}

			qui gen `_STUDY' = `namestr' + " (" + `yearstr' + ")"
			label variable `_STUDY' `"`: variable label `namevar'' (`: variable label `yearvar'')"'
		}

	}	// end if `"`label'"'!=`""'

	
	// If `study' not supplied:
	// First, look at `lcols' as per -metan- syntax proposed in Harris et al, SJ 2008
	else if `"`study'"'==`""' {
		gettoken _STUDY lcols : lcols		// remove _STUDY from lcols

		if `"`_STUDY'"'!=`""' {
			cap confirm var `_STUDY'
			if _rc {
				disp as err `"option {bf:study()} not supplied, and variable {bf:`_STUDY'} (in option {bf:lcols()} not found"'
				exit _rc
			}
			markout `touse' `_STUDY', strok
			local slcol slcol				// mark as being actually lcols() rather than study(); used later for error message
		}
	
		// Else, start by assuming entire dataset is to be used
		//  and remove any observations with no (i.e. missing) data in `invlist'.
		// (code fragment taken from _grownonmiss.ado)
		else {
			tokenize `invlist'
			qui gen byte `newstudy' = (`1'<.) if `touse'
			mac shift
			while "`1'" != "" {
				qui replace `newstudy' = `newstudy' + (`1'<.) if `touse'
				mac shift
			}
			qui replace `newstudy' = . if `newstudy' == 0		// set to missing for benefit of markout
			markout `touse' `newstudy'
			
			// now generate dummy numbering
			qui bysort `touse' (`obs'): replace `newstudy' = _n if `touse'
			label variable `newstudy' "Study"
			sreturn local study `newstudy'
			local _STUDY `newstudy'
		}
	}
	
	// If study is supplied directly
	else {
		local 0 `"`study'"'
		syntax varname [, Missing]			// only a single (var)name is allowed,
		local _STUDY `varlist'				// and it must exist in the data currently in memory
		if `"`missing'"'==`""' markout `touse' `_STUDY', strok
		sreturn local smissing `missing'
	}
	
	if `"`_STUDY'"'!=`""' {
		local studylab : value label `_STUDY'			// if `study' exists, use its value label (N.B. will be empty if string)
		local svarlab : variable label `_STUDY'			// and *original* variable label...
		if `"`svarlab'"'==`""' local svarlab `_STUDY'	// ...or varname
														
		// If study is string, save format length to apply to _LABELS later
		cap confirm string var `_STUDY'
		if !_rc {
			sreturn local sfmtlen = fmtwidth("`: format `_STUDY''")
		}
	}

	// In case `lcols' has been modified, return it back to main routine
	sreturn local lcols `lcols'

	// Check that `touse' is populated (after markout)
	qui count if `touse'
	if !r(N) {
		if `"`_STUDY'"'!=`""' {
			if `"`slcol'"'!=`""' {
				local errtext `"in first {bf:lcols()} variable"'
			}
			else local errtext `"in {bf:study()} variable"'
			if `"`_BY'"'!=`""' local errtext `"`errtext' or "'
		}
		if `"`_BY'"'!=`""' local errtext `"`errtext'in {bf:by()} variable"'
		nois disp as err `"no valid observations `errtext'"'
		exit 2000
	}	
	local ns = r(N)
	
	
	** Subgroup (`by') labelling (if applicable)
	// N.B. do this first, in case `by' is string and contains missings. Stata sorts string missings to be *first* rather than last.
	if `"`_BY'"'!=`""' {
		cap confirm numeric variable `_BY'
		if _rc {
			qui encode `_BY' if `touse', gen(`newby') label(`newbylab')
			label variable `newby' `"`: variable label `_BY''"'
			sreturn local by `newby'
			local _BY `newby'
		}
	}
	
	
	** Study label
	cap confirm numeric var `_STUDY'
	if _rc {
		qui gen long `newstudy' = .
		qui bysort `touse' (`_BY' `obs') : replace `newstudy' = _n if `touse'
		sort `newstudy'				// studies of interest should now be the first `ns' observations

		// Now generate new label
		forvalues i = 1 / `ns' {
		
			// if `study' not present, create "dummy" label consisting of `si_new' values
			if `"`_STUDY'"'==`""' {
				label define `newstudylab' `i' `"`i'"', add
			}
			
			// else if `study' is string, put `study' strings into `newstudylab' values
			else {
				qui replace `obs' = _n
				summ `obs' if `touse', meanonly
				local offset = r(min)			
				local si = `_STUDY'[`=`i' + `offset' - 1']
				label define `newstudylab' `i' `"`si'"', add
			}
		}
		
		// Apply variable and value label
		label variable `newstudy' `"`svarlab'"'
		label values `newstudy' `newstudylab'
		local _STUDY `newstudy'
	}
	
	// Check that `_STUDY' and `_BY' are not identical
	if `"`_STUDY'"'!=`""' {
		cap assert `"`_STUDY'"'!=`"`_BY'"'
		if _rc {
			disp as err `"the same variable cannot be used in both {bf:study()} and {bf:by()}"'
			exit 198
		}
		confirm numeric variable `_STUDY'
	}
	if `"`_BY'"'!=`""' {
		confirm numeric variable `_BY'
	}
	
	sreturn local by `_BY'
	sreturn local study `_STUDY'
	
end
	




***************************************************

** Routine to parse main options and forestplot options together, and:
//  a. Parse some general options, such as -eform- options and counts()
//  b. Check for conflicts between main options and forestplot() suboptions.
// (called directly by metan.ado)

* Notes:
// N.B. This program is used by both -ipdmetan- and -metan-.
// Certain options may be supplied EITHER to ipdmetan/metan directly, OR as sub-options to forestplot()
//   with "forestplot options" prioritised over "main options" in the event of a clash.
// These options are:
// - effect/eform options parsed by CheckOpts (e.g. `rr', `rd', `md', `smd', `wmd', `log')
// - nograph, nohet, nooverall, nosubgroup, nowarning, nowt
// - effect, hetstat, lcols, rcols, plotid, ovwt, sgwt, sgweight
// - cumulative, efficacy, influence, interaction
// - counts, group1, group2 (for compatibility with metan.ado)
// - rfdist, rflevel (for compatibility with metan.ado)

program define ParseFPlotOpts, sclass

	** Parse top-level summary info and option lists
	syntax [, CMDNAME(string) MAINPROG(string) OPTIONS(string asis) FORESTplot(string asis)]

		
	** Parse "main options" (i.e. options supplied directly to -ipdmetan- or -metan-)
	local 0 `", `options'"'
	syntax [, noGRaph noHET noOVerall noSUbgroup noWARNing noWT noSTATs ///
		EFFect(string asis) COUNTS(string asis) ///
		HETStat(passthru) PLOTID(passthru) LCols(passthru) RCols(passthru) ///
		OVWt SGWt SGWEIGHT CUmulative INFluence INTERaction EFFIcacy RFDist RFLevel(passthru) ///
		COUNTS2 GROUP1(passthru) GROUP2(passthru) * ]

	local opts_main `"`macval(options)'"'
	local sgwt = cond("`sgweight'"!="", "sgwt", "`sgwt'")		// sgweight is a synonym (for compatibility with metan.ado)
	local sgweight

	// Process -counts- options
	if `"`counts'"' != `""' {
		local group1_main : copy local group1
		local group2_main : copy local group2
		local 0 `", `counts'"'
		syntax [, COUNTS GROUP1(passthru) GROUP2(passthru) ]
		foreach opt in group1 group2 {
			if `"``opt''"'!=`""' & `"``opt'_main'"'!=`""' & `"``opt''"'!=`"``opt'_main'"' {
				nois disp as err `"Note: Conflicting option {bf:`opt'()}; {bf:counts()} suboption will take priority"' 
			}
			if `"``opt''"'==`""' & `"``opt'_main'"'!=`""' local `opt' : copy local `opt'_main
			local `opt'_main
		}
	}
	else local counts : copy local counts2
	if `"`counts'"'!=`""' local counts `"counts(counts `group1' `group2')"'		// counts(counts...) so that contents are never null
	local group1
	local group2

	// Process -eform- options
	cap nois CheckOpts, soptions opts(`opts_main')
	if _rc {
		if _rc==1 disp as err `"User break in {bf:`mainprog'.CheckOpts}"'
		else disp as err `"Error in {bf:`mainprog'.CheckOpts}"'
		c_local err noerr		// tell main program not to also report an error in ParseFPlotOpts
		exit _rc
	}

	local opts_main `"`s(options)'"'
	local eform     `"`s(eform)'"'
	local log       `"`s(log)'"'
	local summstat  `"`s(summstat)'"'
	if `"`effect'"'==`""' local effect `"`s(effect)'"'
	// N.B. `s(effect)' contains automatic effect text from -eform-; `effect' contains user-specified text

	sreturn clear
		

	** Now parse "forestplot options" if applicable
	local optlist1 graph het overall subgroup warning wt stats ovwt sgwt
	local optlist1 `optlist1' cumulative efficacy influence interaction rfdist		// "stand-alone" options
	local optlist2 plotid hetstat rflevel counts									// options requiring content within brackets
	local optlist3 lcols rcols 														// options which cannot conflict
	
	if `"`forestplot'"'!=`""' {
	
		// Need to temp rename options which may be supplied as either "main options" or "forestplot options"
		//  (N.B. `effect' should be part of `optlist2', but needs to be treated slightly differently)
		local optlist `optlist1' `optlist2' `optlist3' effect
		foreach opt of local optlist {
			local `opt'_main : copy local `opt'
		}
		
		// (Note: extraline() is a forestplot() suboption only,
		//   but is unique in that it is needed *only* by -metan.BuildResultsSet- and *not* by -forestplot-)
		local 0 `", `forestplot'"'
		syntax [, noGRaph noHET noOVerall noSUbgroup noWARNing noWT noSTATs ///
			EFFect(string asis) COUNTS(string asis) ///
			HETStat(passthru) PLOTID(passthru) LCols(passthru) RCols(passthru) ///
			OVWt SGWt SGWEIGHT CUmulative INFluence INTERaction EFFIcacy RFDist RFLevel(passthru) ///
			COUNTS2 GROUP1(passthru) GROUP2(passthru) EXTRALine(passthru) * ]

		local opts_fplot `"`macval(options)'"'
		local sgwt = cond("`sgweight'"!="", "sgwt", "`sgwt'")		// sgweight is a synonym (for compatibility with metan.ado)
		local sgweight
		
		// counts, group1, group2
		if `"`counts'"' != `""' {
			local group1_main : copy local group1
			local group2_main : copy local group2
			local 0 `", `counts'"'
			syntax [, COUNTS GROUP1(passthru) GROUP2(passthru) ]
			foreach opt in group1 group2 {
				if `"``opt''"'!=`""' & `"``opt'_main'"'!=`""' & `"``opt''"'!=`"``opt'_main'"' {
					nois disp as err `"Note: Conflicting option {bf:`opt'()}; {bf:counts()} suboption will take priority"' 
				}
				if `"``opt''"'==`""' & `"``opt'_main'"'!=`""' local `opt' : copy local `opt'_main
				local `opt'_main
			}
		}
		else local counts : copy local counts2
		if `"`counts'"'!=`""' local counts `"counts(counts `group1' `group2')"'		// counts(counts...) so that contents are never null
		local group1
		local group2
		
		// Process -eform- for forestplot, and check for clashes/prioritisation
		cap nois CheckOpts `cmdname', soptions opts(`opts_fplot')
		if _rc {
			if _rc==1 disp as err `"User break in {bf:`mainprog'.CheckOpts}"'
			else disp as err `"Error in {bf:`mainprog'.CheckOpts}"'
			c_local err noerr		// tell main program not to also report an error in ParseFPlotOpts
			exit _rc
		}
		local opts_fplot `"`s(options)'"'
		
		if `"`summstat'"'!=`""' & `"`s(summstat)'"'!=`""' & `"`summstat'"'!=`"`s(summstat)'"' {
			nois disp as err `"Conflicting summary statistics supplied to {bf:`mainprog'} and to {bf:forestplot()}"'
			exit 198
		}
	}
	
	
	** Finalise locals & scalars as appropriate; forestplot options take priority
	local eform = cond(`"`s(eform)'"'!=`""', `"`s(eform)'"', cond(trim(`"`log'`s(log)'"')!=`""', `""', `"`eform'"'))
	local log = cond(`"`s(log)'"'!=`""', `"`s(log)'"', `"`log'"')
	local summstat = cond(`"`s(summstat)'"'!=`""', `"`s(summstat)'"', `"`summstat'"')
	if `"`effect'"'==`""' local effect `"`s(effect)'"'
	// N.B. `s(effect)' contains automatic effect text from -eform-; `effect' contains user-specified text

	
	// `optlist1' and `optlist2':  allowed to conflict, but forestplot will take priority
	foreach opt of local optlist1 {
		if `"``opt''"'==`""' & `"``opt'_main'"'!=`""' local `opt' : copy local `opt'_main
		if `"``opt''"'!=`""' {
			local opts_parsed `"`macval(opts_parsed)' ``opt''"'
		}
	}
	
	// Display warning for options requiring content within brackets (`optlist2')
	foreach opt in `optlist2' effect {
		if `"``opt'_main'"'!=`""' {
			if `"``opt''"'!=`""' {
				if `"``opt''"'!=`"``opt'_main'"' {
					nois disp as err `"Note: Conflicting option {bf:`opt'()}; {bf:forestplot()} suboption will take priority"' 
				}
			}
			else local `opt' : copy local `opt'_main
		}
		
		// Don't add `effect' to opts_parsed; needed separately in main routine
		if `"``opt''"'!=`""' & "`opt'"!="effect" {
			local opts_parsed = `"`macval(opts_parsed)' ``opt''"'
		}
	}

	// `optlist3':  these *cannot* conflict
	foreach opt in `optlist3' {
		if `"``opt'_main'"'!=`""' {
			if `"``opt''"'!=`""' {
				cap assert `"``opt''"'==`"``opt'_main'"'
				if _rc {
					nois disp as err `"Conflicting option {bf:`opt'} supplied to {bf:`mainprog'} and to {bf:forestplot()}"'
					exit 198
				}
				local `opt'
			}
		}
		if `"``opt'_main'``opt''"'!=`""' {
			local opts_parsed `"`macval(opts_parsed)' ``opt'_main'``opt''"'
		}
	}
	
	// Return locals
	sreturn clear
	sreturn local effect `"`effect'"'
	sreturn local eform    `eform'
	sreturn local log      `log'
	sreturn local summstat `summstat'

	sreturn local options     `"`macval(opts_main)'"'
	sreturn local opts_fplot  `"`macval(opts_fplot)'"'
	sreturn local opts_parsed `"`macval(opts_parsed)' `extraline'"'
	
end



* CheckOpts
// Based on the built-in _check_eformopt.ado,
//   but expanded from -eform- to general effect specifications.
// This program is used by -ipdmetan-, -metan- and -forestplot-
// Not all aspects are relevant to all programs,
//   but easier to maintain just a single subroutine!

// subroutine of ParseFPlotOpts

program define CheckOpts, sclass

	syntax [name(name=cmdname)] [, soptions OPts(string asis) ESTEXP(string) ]		// estexp(string), as could include equation term
	
	if "`cmdname'"!="" {
		_check_eformopt `cmdname', `soptions' eformopts(`opts')
	}
	else _get_eformopts, `soptions' eformopts(`opts') allowed(__all__)
	local summstat = cond(`"`s(opt)'"'==`"eform"', `""', `"`s(opt)'"')

	if "`summstat'"=="rrr" {
		local effect `"Risk Ratio"'		// Stata by default refers to this as a "Relative Risk Ratio" or "RRR"
		local summstat rr				//  ... but in MA context most users will expect "Risk Ratio"
	}
	else if "`summstat'"=="nohr" {		// nohr and noshr are accepted by _get_eformopts
		local effect `"Haz. Ratio"'		//  but are not assigned names; do this manually
		local summstat hr
		local logopt nohr
	}
	else if "`summstat'"=="noshr" {
		local effect `"SHR"'
		local summstat shr
		local logopt noshr
	}
	else local effect `"`s(str)'"'

	if "`estexp'"=="_cons" {			// if constant model, make use of eform_cons_ti if available
		local effect = cond(`"`s(eform_cons_ti)'"'!=`""', `"`s(eform_cons_ti)'"', `"`effect'"')
	}
	
	local 0 `", `s(eform)'"'
	syntax [, EFORM(string asis) * ]
	local eform = cond(`"`eform'"'!=`""', "eform", "")
	
	// Next, parse `s(options)' to extract anything that wouldn't usually be interpreted by _check_eformopt
	//  that is: mean differences (`smd', `wmd' with synonym `md'); `rd' (unless -binreg-);
	//  `coef'/`log' and `nohr'/`noshr' (which all imply `log')
	// (N.B. do this even if a valid option was found by _check_eformopt, since we still need to check for multiple options)
	local 0 `", `s(options)'"'
	syntax [, COEF LOG NOHR NOSHR RD SMD WMD MD * ]

	// identify multiple options; exit with error if found
	opts_exclusive "`coef' `log' `nohr' `noshr'"
	if `"`summstat'"'!=`""' {
		opts_exclusive "`summstat' `md' `smd' `wmd' `rr' `rd' `nohr' `noshr'"
	}
	
	// if "nonstandard" effect option used
	else {
		if `"`md'`wmd'"'!=`""' {		// MD and WMD are synonyms
			local effect WMD
			local summstat wmd
		}
		else {
			local effect = cond("`smd'"!="", `"SMD"', ///
				cond("`rd'"!="", `"Risk Diff."', `"`effect'"'))
			local summstat = cond(`"`summstat'"'==`""', trim(`"`smd'`rd'"'), `"`summstat'"')
		}
		else if "`nohr'"!="" {
			local effect `"Haz. Ratio"'
			local summstat hr
			local logopt nohr
		}
		else if "`noshr'"!="" {
			local effect `"SHR"'
			local summstat shr
			local logopt noshr
		}		

		// now check against program properties and issue warning
		if "`cmdname'"!="" {
			local props : properties `cmdname'
			if "`cmdname'"=="binreg" local props `props' rd
			if !`:list summstat in props' {
				cap _get_eformopts, eformopts(`summstat')
				if _rc {
					disp as err `"Note: option {bf:`summstat'} does not appear in properties of command {bf:`cmdname'}"'
				}
			}
		}
	}
	
	// log always takes priority over eform
	// ==> cancel eform if appropriate
	local log = cond(`"`coef'`logopt'"'!=`""', "log", "`log'")					// `coef' is a synonym for `log'; `logopt' was defined earlier
	if `"`log'"'!=`""' {
		if inlist("`summstat'", "rd", "smd", "wmd") {
			nois disp as err "Log option only appropriate with ratio statistics"
			exit 198
		}
		local eform
	}
	
	sreturn clear
	sreturn local logopt   `coef'`logopt'			// "original" log option
	sreturn local log      `log'					// either "log" or nothing
	sreturn local eform    `eform'					// either "eform" or nothing
	sreturn local summstat `summstat'				// if `eform', original eform option
	sreturn local effect   `"`effect'"'
	sreturn local options  `"`macval(options)'"'

end




*********************************************************************

* Program to parse inputted varlist structure and
// - identify studies with insufficient data (`_USE'==2)
// - check for validity
// (called directly by metan.ado)

/*
Syntax:
a) binary data (4 vars):
		metan #events_research #nonevents_research #events_control #nonevents_control , ...
b) cts data (6 vars):     
		metan #N_research mean_research sd_research  #N_control mean_control sd_control , ...
c) logrank survival (OE & V) (2 vars): 
		metan theta oe v, [NPTS(varname numeric] ...
d) generic inverse-variance (2 vars): 
		metan theta se_theta , [NPTS(varname numeric] ...
e) generic inverse-variance with CI instead of SE (3 vars): 
		metan theta lowerlimit upperlimit , [NPTS(varname numeric] ...
*/

program define ProcessInputVarlist, sclass
	
	syntax varlist(numeric min=3 max=7 default=none) [if] [in], [SUMMSTAT(name) ///
		COHen GLAss HEDges noSTANdard              /// model options
		CORnfield EXact WOolf CItype(name)         /// individual study CI options
		MH PETO BREslow TArone CMH CMHNocc CHI2 CC(passthru) noCC2 /// 
		/*options which can be checked against `summstat' and/or `params' for "quick wins", since not model-dependent*/ ///
		EFORM LOG LOGRank noINTeger ZTOL(real 1e-6) * ]
	
	gettoken _USE invlist : varlist
	local opts_adm `"`macval(options)'"'
	
	// Parse explicitly-specified SMD/WMD options, and store in `summstat'
	opts_exclusive `"`cohen' `glass' `hedges' `standard'"' `""' 184
	if `"`cohen'`glass'`hedges'"'!=`""' {
		if inlist("`summstat'", "", "smd") local summstat `cohen'`glass'`hedges'
		else {
			disp as err `"Option {bf:`cohen'`glass'`hedges'} incompatible with `=upper(`summstat')'s"'
			exit 198
		}
	}
	else if `"`standard'"'!=`""' {
		if inlist("`summstat'", "", "wmd") local summstat wmd
		else {
			disp as err `"Option {bf:`standard'} incompatible with `=upper(`summstat')'s"'
			exit 198
		}
	}
	else if "`summstat'"=="smd" local summstat cohen		// default SMD is Cohen
	
	// Parse explicitly-specified `citype' options
	// [N.B. cornfield, exact, woolf were main options to -metan- so are also allowed here
	//  however the preferred -metan- syntax is "citype()" ]
	opts_exclusive `"`cornfield' `exact' `woolf'"' `""' 184
	local cimainopt `cornfield'`exact'`woolf'					// marker as whether supplied as a "main" option (cf -metan-)
	local 0 `", `citype'"'										// now parse preferred "citype()" syntax
	syntax [, CORnfield EXact WOolf * ]
	cap assert `: word count `cimainopt' `cornfield' `exact' `woolf' `options'' <= 1
	if _rc {
		disp as err `"Conflict between options {bf:citype(`citype')} and {bf:`cimainopt'}"'
		exit _rc
	}
	local citype `citype'`cimainopt'
	if inlist(`"`citype'"', `""', `"z"') local citype normal

	
	** Now begin parsing `invlist'
	marksample touse
	tokenize `invlist'
	
	cap assert "`7'" == ""
    if _rc {
		nois disp as err "Too many variables specified"
		exit _rc
	}

	if "`6'"=="" {

		// input is generic inverse-variance (2 or 3 vars) or HR logrank (2 vars)
		if "`4'"=="" {
			
			// input is HR logrank (2 vars: OE & V)
			if "`logrank'" != "" {
				assert "`3'"=="" & "`2'"!=""
				local summstat hr
				local effect `"Haz. Ratio"'
			}
			
			// incompatible options
			foreach opt in mh peto breslow cc cc2 tarone cmh cmhnocc {
				cap assert `"``opt''"' == `""'
				if _rc {
					nois disp as err `"Note: Option {bf:`opt'} is not appropriate without 2x2 count data"' 
					exit 184
				}
			}
	
			// citype
			cap assert !inlist("`citype'", "cornfield", "exact", "woolf")
			if _rc {
				if `"`cimainopt'"'!=`""' {
					nois disp as err `"Option {bf:`citype'} is not appropriate without 2x2 count data and will be ignored"'
				}
				else nois disp as err `"Note: {bf:citype(`citype')} is not appropriate without 2x2 count data and will be ignored"'
				local citype
			}

			if inlist("`summstat'", "wmd", "cohen", "glass", "hedges") {
				nois disp as err "Specified method of constructing effect size is incompatible with the data"
				exit 184
			}			
			
			// Identify studies with insufficient data (`_USE'==2)
			if "`3'"=="" { 	// input is ES + SE (or if logrank, input is OE and V)
				local params = 2
				args _ES _seES
				qui replace `_USE' = 2 if `touse' & `_USE'==1 & missing(`_ES', `_seES')
				qui replace `_USE' = 2 if `touse' & `_USE'==1 & `_seES'==0
				
				// if logrank, `_seES' actually contains (hypergeometric) `v', so 1/se becomes sqrt(v)
				if "`logrank'"=="" qui replace `_USE' = 2 if `touse' & `_USE'==1 & 1/`_seES' < `ztol'
				else qui replace `_USE' = 2 if `touse' & `_USE'==1 & sqrt(`_seES') < `ztol'
			}

			else { 	// input is ES + 95% CI
				local params = 3
				args _ES _LCI _UCI
				qui replace `_USE' = 2 if `touse' & `_USE'==1 & missing(`_LCI', `_UCI')
				qui replace `_USE' = 2 if `touse' & `_USE'==1 & float(`_LCI')==float(`_UCI')
				cap assert `_UCI'>=`_ES' & `_ES'>=`_LCI' if `touse' & `_USE'==1
				if _rc {
					nois disp as err "Effect size and/or confidence interval limits invalid;"
					nois disp as err `"order should be {it:effect_size} {it:lower_limit} {it:upper_limit}"'
					exit _rc
				}
				qui replace `_USE' = 2 if `touse' & `_USE'==1 & 2*invnormal(.975)/(`_UCI' - `_LCI') < `ztol'
			}
			qui count if `touse' & `_USE'==1
			if !r(N) exit 2000			
			
		}       // end of inverse-variance setup

		// input is 2x2 tables
		else {
			cap assert "`5'"==""
			if _rc {
				nois disp as err "Invalid number of variables specified" 
				exit _rc
			}
			local params = 4
			args e1 f1 e0 f0	// events, non-events in trt group; events, non-events in ctrl group (a.k.a. a b c d)
			
			if "`integer'"=="" {
				cap {
					assert int(`e1')==`e1' if `touse'
					assert int(`f1')==`f1' if `touse'
					assert int(`e0')==`e0' if `touse'
					assert int(`f0')==`f0' if `touse'
				}
				if _rc {
					di as err "Non integer cell counts found" 
					exit _rc
				}
			}
			cap assert `e1'>=0 & `f1'>=0 & `e0'>=0 & `f0'>=0 if `touse'
			if _rc {
				di as err "Non-positive cell counts found" 
				exit _rc
			}

			// citype
			cap assert !inlist("`citype'", "cornfield", "exact", "woolf")
			if _rc {			
				if !inlist("`summstat'", "or", "") {
					if `"`cimainopt'"'!=`""' {
						nois disp as err `"Option {bf:`citype'} is only compatible with odds ratios"'
					}
					else nois disp as err `"Option {bf:citype(`citype')} is only compatible with odds ratios"' 
					exit 184
				}
				else if "`summstat'"=="" {
					if "`citype'"=="cornfield" {
						nois disp as err `"Note: Cornfield-type confidence intervals specified; odds ratios assumed"'
					}
					else if "`citype'"=="exact" {
						nois disp as err `"Note: Exact confidence intervals specified; odds ratios assumed"'
					}
					else nois disp as err `"Note: Woolf-type confidence intervals specified; odds ratios assumed"'
					local summstat or
					local effect `"Odds Ratio"'
				}
			}

			if "`chi2'"!="" {
				if !inlist("`summstat'", "or", "") & !(inlist("`summstat'", "hr", "") & "`logrank'"!="") {
					nois disp as err `"Option {bf:chi2} is only compatible with odds ratios"'
					exit 184
				}
				else if "`summstat'"=="" {
					nois disp as err `"Note: Chi-squared option specified; odds ratios assumed"' 
					local summstat or
					local effect `"Odds Ratio"'
				}
			}
						
			foreach opt in breslow tarone cmh cmhnocc {
				if "``opt''"!="" {
					if !inlist("`summstat'", "or", "") {
						nois disp as err `"Option {bf:`opt'} is only compatible with odds ratios"' 
						exit 184
					}
					else if "`summstat'"=="" {
						local opttxt = cond("`opt'"=="breslow", "Breslow-Day homogeneity test", ///
							cond("`opt'"=="tarone", `"Tarone's adjusted Breslow-Day homogeneity test"', ///
							cond("`opt'"=="cmh", "Cochran-Mantel-Haenszel test", ///
							cond("`opt'"=="cmhnocc", "Cochran-Mantel-Haenszel test with continuity correction", ""))))
						
						nois disp as err `"`opttxt' specified; odds ratios assumed"'
						local summstat or
						local effect `"Odds Ratio"'
					}
				}
			}
			if "`peto'"!="" {
				if !inlist("`summstat'", "or", "") {
					nois disp as err "Peto method option can only be used with odds ratios"
					exit 184
				}
				else if "`summstat'"=="" {
					nois disp as err `"Note: Peto method specified; odds ratios assumed"' 
					local summstat or
					local effect `"Odds Ratio"'
				}
				local chi2 chi2
			}			
			
			if inlist("`summstat'", "hr", "shr", "tr") {
				nois disp as err "Time-to-event outcome types are incompatible with count data"
				exit 184
			}
			else if inlist("`summstat'", "wmd", "cohen", "glass", "hedges") {			
				nois disp as err "Continuous outcome types are incompatible with count data"
				exit 184
			}
			else if "`summstat'"=="" {
				local summorig null			// marker that summstat was *not* specified by user
				local summstat rr
				local effect `"Risk Ratio"'
			}
			
			// Find studies with insufficient data (`_USE'==2)
			qui replace `_USE' = 2 if `touse' & `_USE'==1 & (`e1' + `f1')*(`e0' + `f0')==0		// No data AT ALL in at least one arm
			if "`summstat'"!="rd" {

				// M-H RR: double-zero *non*-event (i.e. ALL events in both arms; unusual but not impossible) is OK
				if "`summstat'"=="rr" qui replace `_USE' = 2 if `touse' & `_USE'==1 & `e1' + `e0'==0
				
				// Else: any double-zero
				else qui replace `_USE' = 2 if `touse' & `_USE'==1 & (`e1' + `e0'==0 | `f1' + `f0'==0)
			}
			qui count if `touse' & inlist(`_USE', 1, 2)
			if !r(N) exit 2000
						
		}	// end of binary variable setup

		// log only allowed if OR, RR, RRR, HR, SHR, TR
		if "`log'"!="" & !inlist("`summstat'", "or", "rr", "rrr", "hr", "shr", "tr") {
			nois disp as err `"{bf:log} may only be specified with 2x2 count data or log-rank HR; option will be ignored"'
			local log
		}			
		
	} // end of all non-6 variable setup

	if "`6'"!="" {
		
		// log not allowed
		if "`log'"!="" {
			nois disp as err `"{bf:log} may only be specified with 2x2 count data or log-rank HR; option will be ignored"'
			local log
		}			

		local params = 6
		args n1 mean1 sd1 n0 mean0 sd0

        // input is form N mean SD for continuous data
		if "`integer'"=="" {
			cap assert int(`n1')==`n1' & int(`n0')==`n0' if `touse'
			if _rc {
				nois disp as err "Non integer sample sizes found"
				exit _rc
			}
		}
        cap assert `n1'>0 & `n0'>0 if `touse'
		if _rc {
			nois disp as err "Non positive sample sizes found" 
			exit _rc
		}

		foreach opt in mh peto breslow cc cc2 chi2 tarone cmh cmhnocc {
			cap assert `"``opt''"' == `""'
			if _rc {
				nois disp as err `"Note: Option {bf:`opt'} is not appropriate without 2x2 count data"' 
				exit 184
			}
		}
		
		// citype
		cap assert !inlist("`citype'", "cornfield", "exact", "woolf")
		if _rc {
			if `"`cimainopt'"'!=`""' {
				nois disp as err `"Option {bf:`citype'} is not appropriate without 2x2 count data"'
			}
			else nois disp as err `"Note: {bf:citype(`citype')} is not appropriate without 2x2 count data"' 
			exit 184
		}
		
		// summstat
		cap assert inlist("`summstat'", "", "wmd", "cohen", "glass", "hedges")
		if _rc {
			nois disp as err "Invalid specifications for combining trials"
			exit 184
		}
		if "`summstat'"=="" {
			local summstat cohen		// default is Cohen SMD
			local effect SMD
		}

		// Find studies with insufficient data (`_USE'==2)
		qui replace `_USE' = 2 if `touse' & `_USE'==1 & missing(`n1', `mean1', `sd1', `n0', `mean0', `sd0')
		qui replace `_USE' = 2 if `touse' & `_USE'==1 & `n1' < 2  | `n0' < 2
		qui replace `_USE' = 2 if `touse' & `_USE'==1 & `sd1'<=0  | `sd0'<=0
		qui count if `touse' & `_USE'==1
		if !r(N) exit 2000

	} // end of 6-var set-up
	
	// If `params'==4, default to eform unless Risk Diff.
	if `params'==4 & `"`summstat'"'!=`"rd"' &  `"`log'"'==`""' {
		local eform eform
	}
	
	// Similarly: if `logrank', default to log
	else if "`logrank'"!="" {
		local log = cond(`"`log'"'!=`""', "log", cond(`"`eform'"'==`""', "log", ""))
	}
	
	// summstat should be NON-MISSING *UNLESS* "generic" es/se
	if `params'>3 | "`logrank'"!="" {
		assert `"`summstat'"'!=`""'
	}
	
	sreturn clear
	
	local options `breslow' `tarone' `cmh' `cmhnocc' `chi2'
	local options `options' `logrank' `mh' `peto' `integer' `cc' `cc2'
	local options = trim(itrim(`"`macval(opts_adm)' `options'"'))
	sreturn local options `"`macval(options)'"'
	
	sreturn local effect `"`effect'"'
	sreturn local summorig `summorig'
	sreturn local summstat `summstat'
	sreturn local params   `params'
	sreturn local citype   `citype'
	sreturn local eform    `eform'
	sreturn local log      `log'
	
end





*****************************************************************

* Parse meta-analysis modelling options (incl. random-effects)
// compatibility, error checking
// (called directly by metan.ado)

program define ProcessModelOpts, sclass

	syntax [, SUMMSTAT(passthru) SUMMORIG(passthru) PARAMS(passthru) ///
		FIRST(string asis) FIRSTSTATS(passthru) SECOND(string asis) SECONDSTATS(passthru) /*legacy -metan-*/ ///
		MODEL(string) RAndom1 RAndom(string) RE1 RE(string) ///
		RANDOMI FIXEDI FIXED COMMON FE IV MH PETO COHen GLAss HEDges noSTANdard ///	// esmethod/model options
		T Z IVHet QE(varname numeric) LOGRank ///									// needed in this subroutine, but will also be stored in `opts_model'
		BREslow TArone COCHranq QProfile HIGgins NCchi2 QGamma ///					// heterogeneity options
		CMH CMHNocc CHI2 DF(passthru) ///	// not needed here, but store in `opts_model' for passing to PerformMetaAnalysis
		CC(string) noCC2 WGT(passthru) RFDist CUmulative INFluence * ]				// non-modelling options, to store in `opts_adm'
		
	local opts_adm `"`macval(options)'"'		// Non-model options	


	** Sort out synonyms and legacy -metan- options
	//   preserving the originally supplied option in case of error checking

	// first, firststats, second, secondstats
	if `"`second'"'!=`""' {	
		gettoken _ES_ rest : second
		cap confirm number `_ES_'
		if !_rc {		// if user-defined pooled effect
			gettoken _LCI_ rest : rest
			gettoken _UCI_ model2text : rest
			cap {
				confirm number `_LCI_'
				confirm number `_UCI_'
				assert `_LCI_' <= `_ES_'
				assert `_UCI_' >= `_ES_'
			}
			if _rc {
				nois disp as err "Must supply user-defined main analysis in the format:"
				nois disp as err "  {it:ES lci uci desc}"
				exit _rc
			}
			local second `"user2, user2stats(`_ES_' `_LCI_' `_UCI_') `secondstats'"'
			local teststat2 user
			local hetopt2 user
		}
		
		// Else, assume that "`second'" is a model name...
		else {
			// ...in which case it must be fixed (common), random, mh, or peto
			cap assert inlist(`"`second'"', "fixed", "common", "random", "mh", "peto") ///
				| inlist(`"`second'"', "mha", "mhae", "mhaens", "mhaensz", "mhaensze", "mhaenszel")
			if _rc {
				disp as err "Option {bf:second()} can be one of {bf:fixed}|{bf:common}, {bf:random}, {bf:{ul:mh}aenzel} or {bf:peto}"
				disp as err " or a user-defined estimate with confidence intervals in the format:  {it:ES lci uci desc}"
				exit 198
			}
			
			// Use existing error message from -metan-
			if `"`first'"'!=`""' {
				disp as err "Cannot have user-defined analysis as main analysis and standard analysis"
				disp as err "as second analysis. You can do it the other way round, or have two user"
				disp as err "defined analyses, but you can't do this particular thing."
				disp as err "Sorry, that's just the way it is."
				exit 198
			}
		}
		
		foreach opt in model re qe {
			if `"``opt''"'!=`""' {
				disp as err `"Cannot specify option {bf:`opt'()} with {bf:second()}"'
				exit 198
			}
		}
	}
	if `"`first'"'!=`""' {
		gettoken _ES_  rest : first
		gettoken _LCI_ rest : rest
		gettoken _UCI_ model1text : rest
		cap {
			confirm number `_ES_'
			confirm number `_LCI_'
			confirm number `_UCI_'
			assert `_LCI_' <= `_ES_'
			assert `_UCI_' >= `_ES_'
		}
		if _rc {
			nois disp as err "Must supply estimate with confidence intervals: ES CIlow CIupp"
			nois disp as err "with user-defined main analysis"
			exit _rc
		}
		local model `"user1, user1stats(`_ES_' `_LCI_' `_UCI_') `firststats'"'
		local teststat1 user
		local hetopt1 user
	}
	
	// NOTE: Only minimal validity checking here; just want to identify single unique model
	opts_exclusive `"`randomi' `fixedi' `fixed' `common' `mh' `iv' `fe' `peto' `re1' `random'"' `""' 184

	// re/re() and random/random() as main options
	if `"`randomi'"'!=`""' local re re

	if `"`random'"'!=`""' local rabr `"()"'
	if `"`re'"'!=`""'     local rebr `"()"'

	if `"`random1'"'!=`""' & `"`random'"'==`""' local random random
	if `"`re1'"'!=`""'     & `"`re'"'==`""'     local re re

	if `"`re'"'!=`""' {
		if `"`random'"'!=`""' {
			nois disp as err `"Cannot specify both {bf:re`rebr'} and {bf:random`rabr'}; please choose just one"'
			exit 198
		}
		local model_orig re`rebr'				// store actual supplied option for error displays
		local newModel : copy local re			// `re' is a synonym for `model'; henceforth use the latter		
	}
	else if `"`random'"'!=`""' {
		local model_orig random`rabr'			// store actual supplied option for error displays
		local newModel : copy local random		// `random' is a synonym for `model'; henceforth use the latter
		
	}

	// Similarly, `fe' is a synonym for model(fe)...
	if `"`fixedi'`fe'`iv'`common'"'!=`""' {
		local model_orig `fixedi'`fe'`iv'`common'	// store actual supplied option for error displays
		local newModel fe
	}
	
	// ...`mh' is a synonym for model(mh)...
	if `"`fixed'`mh'"'!=`""' {
		local model_orig `fixed'`mh'		// store actual supplied option for error displays
		local newModel mh
	}
	
	// and ...`peto' is a synonym for model(peto)
	if `"`peto'"'!=`""' {
		local model_orig peto				// store actual supplied option for error displays
		local newModel peto
	}

	// qe() and ivhet
	if `"`qe'"'!=`""' {
		local model_orig qe()				// store actual supplied option for error displays
		local newModel qe, qwt(`qe')		// format to pass to ParseModel
	}
	if `"`ivhet'"'!=`""' {
		local model_orig ivhet				// store actual supplied option for error displays
		local newModel ivhet
	}
	
	if `"`model_orig'"'!=`""' {
		if `"`model'"'!=`""' {
			nois disp as err `"Cannot specify both {bf:`model_orig'} and {bf:model()}; please choose just one"'
			exit 198
		}
		local model : copy local newModel
	}
	
	
	// AT THIS POINT
	// If there were any stand-alone options fixed, mh, random etc. then these have been converted to model()
	//  (and if model() already existed then exit with error)
	// Therefore we now have just model(), and nothing else to do with pooled effect sizes.
	// We *do* however still need to consider heterogeneity statistics,
	//  and also options such as cc() which also affect pooled effects BUT *are* allowed as main options SO LONG AS `m'==1.
	// Finally, note that second() is a special case of model() with particular considerations [legacy -metan-].
		
	// Now, form one or more "models"
	//  - main options                [legacy metan]
	//  - main options + second()     [legacy metan]
	//  - main options + model()      [single re spec in model(); standard metan if single model]
	//  - *no* main options + model() [multiple specs in model(), including things like cc, breslow etc.]
	
	// - breslow, tarone:                      main options, not in model() brackets.  Only applicable if one of the models is MH
	// - cochranq, qprofile, testbased etc. :  main options, not in model() brackets.  Only applicable if one of the models is "suitable"

	// - cmh, chi2, t, z : have to go with the model(), since they are testing the pooled estimate
	// - cc(), nocc :      have to go with the model(), since they may affect the pooled estimate
	
	
	*****************
	* Parse model() *
	*****************
	
	// Multiple models, separated with a backslash
	local m = 1
	if `"`model'"'!=`""' {
		gettoken model`m' rest : model, parse("\")
		if !strpos(`"`model`m''"', `","') {
			local model`m' `model`m'',					// add comma if no options
		}
		while `"`rest'"'!=`""' {
			gettoken bs rest2 : rest, parse("\")
			assert "`bs'"=="\"
			if `"`rest2'"'!=`""' {
				local ++m
				gettoken model`m' rest : rest2, parse("\")
				if !strpos(`"`model`m''"', `","') {
					local model`m' `model`m'',			// add comma if no options
				}
			}
		}
	}
	
	// Internal macro lists
	local teststat `t' `z' `chi2' `cmh' `cmhnocc'
	local hetopt `breslow' `tarone' `cochranq' `qprofile' `higgins' `ncchi2' `qgamma'
	opts_exclusive `"`teststat'"' `""' 184
	opts_exclusive `"`hetopt'"' `""' 184

	if `"`cc'"'!=`""' local ccopt `"cc(`cc')"'
	else local ccopt `cc2'	
	
	
	// If more than one model*, "stand-alone"/"main" options to do with pooled effect estimation/testing/heterogeneity become the defaults,
	//     which can be overridden by specific model suboptions
	// *except if second()
	if `m' > 1 {
		
		// Now process each model in turn using ParseModel to return "canonical" form
		forvalues j = 1 / `m' {
			cap nois ParseModel `model`j'' `summstat' `summorig' `params' globalopts(`teststat' `hetopt' `ccopt' `wgt')
			
			if _rc {
				if _rc==1 nois disp as err `"User break in {bf:metan.ParseModel}"'
				else nois disp as err `"Error in {bf:metan.ParseModel}"'
				c_local err noerr		// tell -metan- not to also report an "error in metan.ProcessModelOpts"
				exit _rc
			}
	
			cap assert `"`s(model)'"'!=`""'
			if _rc {
				disp as err "No model found"
				exit 198
			}
			
			if `"`summnew'"'==`""' local summnew `s(summnew)'

			local model`j'opts `"`s(modelopts)'"'
			if `"`model`j'text'"'==`""' local model`j'text `"`s(modeltext)'"'		// already parsed if user-defined
			local modellist    `modellist' `s(model)'
			local teststatlist `teststatlist' `s(teststat)'
			local hetoptlist   `hetoptlist' `s(hetopt)'
			local wgtoptlist   `wgtoptlist' `s(wgtopt)'
			
			// Error prompts
			local gl_error `gl_error' `s(gl_error)'
		}
	}
	
	// If a single model*, assemble it from main options and from model() if applicable
	// *or if second()
	else {

		// "main" aka "stand-alone" options:
		// Testing: z t chi2 cmh cmhnocc
		// Heterogeneity: breslow tarone cochranq qprofile higgins ncchi2 qgamma
		if !strpos(`"`model'"', `","') {
			local model `model',					// add comma if no options
		}
		cap nois ParseModel `model' `summstat' `summorig' `params' globalopts(`teststat' `hetopt' `ccopt' `wgt')

		if _rc {
			if _rc==1 nois disp as err `"User break in {bf:metan.ParseModel}"'
			else nois disp as err `"Error in {bf:metan.ParseModel}"'
			c_local err noerr		// tell -metan- not to also report an "error in metan.ProcessModelOpts"
			exit _rc
		}
		
		local model1    `s(model)'
		local model1opts `"`s(modelopts)'"'
		if `"`model1text'"'==`""' local model1text `"`s(modeltext)'"'	// don't overwrite if user-defined
		if `"`teststat1'"'==`""'  local teststat1 `s(teststat)'			// don't overwrite if user-defined
		if `"`hetopt1'"'==`""'    local hetopt1 `s(hetopt)'				// don't overwrite if user-defined
		if `"`summnew'"'==`""'    local summnew `s(summnew)'
		local wgtoptlist `"`s(wgtopt)'"'

		// Error prompts
		local gl_error `gl_error' `s(gl_error)'
		
		if `"`second'"'!=`""' {
			if !strpos(`"`second'"', `","') {
				local second `second',					// add comma if no options
			}
			cap nois ParseModel `second' `summstat' `params'		// Note: globalopts() are *not* passed on to the second model
																	// c.f. previous -metan- behaviour
			if _rc {
				if _rc==1 nois disp as err `"User break in {bf:metan.ParseModel}"'
				else nois disp as err `"Error in {bf:metan.ParseModel}"'
				c_local err noerr		// tell -metan- not to also report an "error in metan.ProcessModelOpts"
				exit _rc
			}			

			local model2     `s(model)'
			local model2opts `"`s(modelopts)'"'
			if `"`model2text'"'==`""' local model2text `"`s(modeltext)'"'	// don't overwrite if user-defined
			if `"`teststat2'"'==`""'  local teststat2 `s(teststat)'			// don't overwrite if user-defined
			if `"`hetopt2'"'==`""'    local hetopt2 `s(hetopt)'				// don't overwrite if user-defined
			if `"`summnew'"'==`""'    local summnew `s(summnew)'
			local wgtoptlist `"`wgtoptlist' `s(wgtopt)'"'
			
			// Error prompts
			local gl_error `gl_error' `s(gl_error)'
	
			local ++m
		}
		
		local modellist    `model1' `model2'
		local teststatlist `teststat1' `teststat2'
		local hetoptlist   `hetopt1' `hetopt2'
	}
	
	// Display error prompts
	local gl_error : list uniq gl_error
	foreach opt of local gl_error {
		if `m'==1 {
			nois disp as err "Option {bf:`opt'} is not compatible with model {bf:`modellist'}"
			exit 198
		}
		nois disp as err "Note: global option {bf:`opt'} is not applicable to all models; local defaults will apply"
	}
	if "`rfdist'"!="" {
		if `"`cumulative'"'!=`""' {
			nois disp as err `"Options {bf:cumulative} and {bf:rfdist} are not compatible"'
			exit 198
		}
		if `"`influence'"'!=`""' {
			nois disp as err `"Options {bf:influence} and {bf:rfdist} are not compatible"'
			exit 198
		}
		
		local nopredint mh peto fe kr gamma hc ivhet qe mu pl
		if `"`: list modellist - nopredint'"'==`""' {
			nois disp as err "Note: prediction interval cannot be estimated under " _c
			if `m'==1 disp as err `"the specified model; {bf:rfdist} will be ignored"'
			else disp as err `"any of the specified models; {bf:rfdist} will be ignored"'
			local rfdist
		}
		else if `"`: list modellist & nopredint'"'!=`""'{
			nois disp as err `"Note: prediction interval cannot be estimated for all models"'
		}
	}
	
	
	***********************
	* Initialise rownames *
	***********************
	// of matrices to hold overall/subgroup pooling results
	// (c.f. r(table) after regression)

	local rownames eff se_eff eff_lci eff_uci npts crit				// effect size; std. err.; conf. limits; no. pts.; critical value

	// test statistics: remove duplicates and order: z t chi2 u
	local rowtest : copy local teststatlist
	local rowtest : list uniq rowtest
	foreach el in z t chi2 u {
		if `: list el in rowtest' local rownames `rownames' `el'	// test statistics, in pre-defined order
	}
	
	local kr kr
	if `: list kr in modellist' local rownames `rownames' df_kr		// effect-size df (Kenward-Roger only)
	local rownames `rownames' pvalue								// p-value

	local peto peto													// logrank and Peto OR only
	if "`logrank'"!="" | `: list peto in modellist' local rownames `rownames' OE V
	
	local rownames `rownames' Q Qdf H Isq HsqM						// standard heterogeneity stats
	local uniqmodels : list uniq modellist
	if !inlist("`uniqmodels'", "mh", "peto", "mh peto", "peto mh") {
		local rownames `rownames' sigmasq tausq			// sigmasq, tausq (unless all models are M-H or Peto)
	}
	
	local refhetlist higgins ncchi2 qgamma qprofile ml reml gamma
	foreach el of local refhetlist {					// H, Isq, HsqM confidence intervals
		if `: list el in hetoptlist' {
			local rownames `rownames' H_lci H_uci Isq_lci Isq_uci HsqM_lci HsqM_uci
			continue, break
		}
	}
	local refhetlist qprofile ml reml gamma
	foreach el of local refhetlist {					// additional tausq confidence interval
		if `: list el in hetoptlist' {
			local rownames `rownames' tsq_lci tsq_uci
			continue, break
		}
	}
	
	if "`rfdist'"!="" local rownames `rownames' rflci rfuci			// if predictive distribution	
	
	// Return models
	sreturn clear
	sreturn local rownames     `rownames'
	sreturn local modellist    `modellist'
	sreturn local teststatlist `teststatlist'
	sreturn local hetoptlist   `hetoptlist'
	sreturn local wgtoptlist   `wgtoptlist'
	forvalues j = 1 / `m' {
		sreturn local model`j'opts `"`model`j'opts'"'
		local modeltext `"`modeltext' model`j'text(`model`j'text')"'
	}
	sreturn local modeltext `"`modeltext'"'
	sreturn local m `m'
	local opts_adm = trim(itrim(`"`macval(opts_adm)' `logrank' `rfdist' `cumulative' `influence'"'))
	sreturn local opts_adm `"`macval(opts_adm)'"'		// non model-specific options

	// Internal macros
	sreturn local summnew `summnew'
	if "`second'"!="" sreturn local second second	// marker that "second()" syntax was used [ as opposed to model(model1 \ model2) ]
	if "`wgt'"!="" sreturn local userwgt userwgt	// marker of user-defined weights, c.f. previous versions of -metan-

end


	

// Simply parse multiple models one-at-a-time, and return the results
// Validity checking is done elsewhere.

program define ParseModel, sclass

	syntax [name(name=model id="meta-analysis model")] ///
		, PARAMS(integer) [ SUMMSTAT(name) SUMMORIG(name) GLOBALOPTS(string) ///	// default/global options [TESTOPT(name) HETOPT(name) CCOPT(string)]
		Z T CHI2 CMH CMHNocc ///													// test statistic options
		BREslow TArone COCHranq QProfile HIGgins NCchi2 QGamma ///					// heterogeneity options
		HKsj BArtlett PETO RObust SKovgaard EIM OIM QWT(varname numeric) /*contains quality weights*/ ///
		INIT(name) CC(passthru) noCC2 ///
		USER1stats(passthru) USER2stats(passthru) FIRSTSTATS(passthru) SECONDSTATS(passthru) ///	// for user-defined models
		WGT(passthru) TRUNCate(passthru) ISQ(string) TAUSQ(string) ITOL(passthru) MAXTausq(passthru) REPS(passthru) MAXITer(passthru) QUADPTS(passthru) ]
		// last line ^^  "global" opts to compare with "specific model" opts


	// tausq() option added 24th July 2017
	// bartlett and z (i.e. "not LR" options) added 5th March 2018; "z" returns signed LR statistic (as opposed to Wald-type) as of Jan 2019
	// robust option added 13th Dec 2018
	// skovgaard option added 5th Jan 2019

	// Test statistic and heterogeneity options: should be unique
	local teststat `t' `z' `chi2' `cmh' `cmhnocc'
	local hetopt `breslow' `tarone' `cochranq' `qprofile' `higgins' `ncchi2' `qgamma'
	opts_exclusive `"`teststat'"' `""' 184
	opts_exclusive `"`hetopt'"' `""' 184
	
	// Tausq estimators, with synonyms
	if inlist("`model'", "iv", "fixed", "commmon") local model fe	// Fixed (common)-effects inverse-variance	
	else if inlist("`model'", "mh", "mha", "mhae", "mhaens", "mhaensz", "mhaensze", "mhaenszel") local model mh	// Mantel-Haenszel
	else if inlist("`model'", "r", "random", "rand", "re", "dl") local model dl	// DerSimonian-Laird
	else if inlist("`model'", "bdl", "dlb") local model dlb			// Bootstrap DerSimonian-Laird (Kontopantelis)
	else if inlist("`model'", "mp", "pm", "q", "gq", "genq", "vb", "eb") local model mp	// Mandel-Paule aka Generalised Q aka Empirical Bayes
	else if inlist("`model'", "vc", "ca", "he") local model vc		// Variance-component aka Cochran's ANOVA-type aka Hedges
	else if inlist("`model'", "sj2", "sj2s") local model sj2s		// Sidik-Jonkman two-step (default init=vc)
	else if inlist("`model'", "dk2", "dk2s") local model dk2s		// DerSimonian-Kacker two-step (default init=vc)
	else if inlist("`model'", "sens", "sa") local model sa			// Sensitivity analysis (at fixed Isq) as suggested by Kontopantelis
		
	// Other model types (with synonyms)
	else if inlist("`model'", "g", "ga", "gam", "gamm", "gamma", "bt", "bs") local model gamma		// Biggerstaff-Tweedie
	else if inlist("`model'", "mu", "mul", "mult", "fv") local model mu								// Multiplicative heterogeneity
	else if inlist("`model'", "ivh", "ivhe", "ivhet") local model ivhet								// Doi's IVHet
	
	// Hartung-Knapp-Sidik-Jonkman variance correction
	if "`hksj'"!="" {
		if inlist("`model'", "dlt", "hk", "hks", "hksj", "kh") local model dl
		else if inlist("`model'", "mu", "gamma", "kr", "hc", "ivhet", "qe", "pl") {
			nois disp as err `"Specified random-effects model is incompatible with Hartung-Knapp-Sidik-Jonkman variance estimator"'
			exit 198
		}
	}
	else if inlist("`model'", "dlt", "hk", "hks", "hksj", "kh") {
		local model dl		// DL is default tausq estimator
		local hksj hksj
	}
			
	// Kenward-Roger variance correction: allow "reml, kr" as an alternative
	if "`kr'"!="" {
		if !inlist("`model'", "", "kr", "reml" {
			nois disp as err "Kenward-Roger variance estimator may only be combined"
			nois disp as err " with the REML estimator of tau{c 178}"
			exit 198
		}
		local model kr
	}
	
	// observed/expected information matrix for Kenward-Roger
	if `"`eim'`oim'"'!=`""' {
		if "`model'"!="kr" {
			nois disp as err `"Note: Options {bf:eim} and {bf:oim} are only relevant to the Kenward-Roger variance estimator and will be ignored"' 
		}
		else {
			cap assert `: word count `eim' `oim'' == 1
			if _rc {
				nois disp as err `"May only specify one of {bf:eim} or {bf:oim}, not both"'
				exit _rc
			}
		}
	}
	else if "`model'"=="kr" local eim eim		// default		
	
	// Sidik-Jonkman robust ("sandwich-like") variance estimator
	if "`robust'"!="" {
		if inlist("`model'", "mu", "gamma", "kr", "hc", "ivhet", "qe", "pl") {
			nois disp as err `"Specified random-effects model is incompatible with Sidik-Jonkman robust variance estimator"'
			exit 198
		}
	}
	
	// Two-step models
	if "`init'"!=`""' {
		if !inlist("`model'", "sj2s", "dk2s") {
			nois disp as err `"Option {bf:init()} is only valid with two-step estimators of tausq"'
			exit 198
		}
	}
	if inlist("`model'", "sj2s", "dk2s") {
		if "`init'"=="" local init vc				// default initial estimate is Hedges/Cochran/Variance-component
		if "`model'"=="dk2s" {						// DerSimonian-Kacker two-step is valid for MM estimators only
			if !(inlist("`init'", "vc", "dl") | substr(trim(`"`init'"'), 1, 2)==`"sa"') {
				nois disp as err `"Option {bf:init()} must be {bf:vc}, {bf:dl} or {bf:sa} with DerSimonian-Kacker two-step estimator"'
				exit 198
			}
		}
		else {
			if !(inlist("`init'", "vc", "dl", "dlb", "ev", "hm") | inlist("`init'", "b0", "bp", "mp", "ml", "reml") | substr("`init'",1,2)=="sa") {
				nois disp as err `"Invalid {bf:init()} option with Sidik-Jonkman two-step estimator"'
				exit 198
			}
		}
		local init_opt `"init(`init')"'
	}
		
	// Quality effects
	if "`model'"=="qe" {
		if `"`qwt'"'==`""' {
			disp as err "Quality-effects model specified but no quality weights found"
			exit 198
		}
		local qe_opt `"qwt(`qwt')"'
	}
	else if `"`qwt'"'!=`""' {
		disp as err "Quality weights cannot be specified without quality-effects model"
		exit 198
	}
		
	// final check for valid random-effects models:
	if !inlist("`model'", "", "user1", "user2", "fe", "mh", "peto") ///		// fixed (common)-effect and Mantel-Haenszel & Peto [PLUS USER-DEFINED]
		& !inlist("`model'", "dl", "dlb", "ev", "vc", "hm", "b0", "bp") ///			// simple tsq estimators (non-iterative)
		& !inlist("`model'", "mp", "ml", "reml") ///								// simple tsq estimators (iterative)
		& !inlist("`model'", "sj2s", "dk2s", "sa") ///								// two-step estimators; sensitivity analysis at fixed tsq/Isq
		& !inlist("`model'", "pl", "kr", "gamma", "hc", "mu", "qe", "ivhet") {		// complex models
		nois disp as err "Invalid random-effects model"
		nois disp as err "Please see {help metan:help metan} for a list of valid model names"
		exit 198
	}

	// conflicting options: variance modifications
	opts_exclusive `"`hksj' `bartlett' `skovgaard' `robust'"' `"`model'"' 184	

	// Bartlett and Skovgaard likelihood corrections: profile likelihood only
	if `"`bartlett'`skovgaard'"'!=`""' {
		cap assert "`model'"=="pl"
		if _rc {
			local errtext = cond(`"`bartlett'"'!=`""', `"Bartlett's"', `"Skovgaard's"')
			nois disp as err `"`errtext' correction is only valid with Profile Likelihood"'
			exit 198
		}
	}
	
	// dependencies
	if inlist("`model'", "mp", "ml", "pl", "reml", "gamma", "hc") | "`hetopt'"=="qprofile" {
		capture mata mata which mm_root()
		if _rc {
			nois disp as err `"Iterative tau-squared calculations require the Mata function {bf:mm_root()} from {bf:moremata}"'
			nois disp as err `"Type {stata ssc describe moremata:ssc install moremata} to install it"'
			exit 499
		}
		if inlist("`model'", "gamma", "hc") {
			capture mata mata which integrate()
			if _rc {
				if "`model'"=="gamma" nois disp as err `"Biggerstaff-Tweedie method requires the Mata function {bf:integrate()}"'
				else nois disp as err `"Henmi-Copas method requires the Mata function {bf:integrate()}"'
				nois disp as err `"Type {stata ssc describe integrate:ssc install integrate} to install it"'
				exit 499
			}
		}
	}
	if "`model'"=="dlb" {
		capture mata mata which mm_bs()
		local rc1 = _rc
		capture mata mata which mm_jk()
		if _rc | `rc1' {
			nois disp as err `"Bootstrap DerSimonian-Laird method requires the Mata functions {bf:mm_bs()} and {bf:mm_jk()} from {bf:moremata}"'
			nois disp as err `"Type {stata ssc describe moremata:ssc install moremata} to install them"'
			exit 499
		}
	}
	
	// default model if none defined
	if `"`model'"'==`""' {
		local modelorig null			// marker that model was *not* specified by user
		local model = cond(`params'==4, "mh", "fe")
	}
	
	if inlist("`model'", "mh", "peto") {
		cap assert `params'==4
		if _rc {
			disp as err "Mantel-Haenszel and Peto options only valid with 2x2 count data"
			exit 198
		}
	}
	
	
	
	** PARSE "GLOBAL" OPTIONS (if applicable)
	// N.B. global opts will already have been checked against the data structure by ProcessInputVarlist
	//  it only remains to check them against the *model* (and teststat/hetopt)
	opts_exclusive `"`cc' `cc2'"' `""' 184
	local old_ccopt `"`cc'`cc2'"'
	foreach opt in wgt truncate isq tausq itol maxtausq reps maxiter quadpts {
		local old_`opt' : copy local `opt'
	}
	
	local 0 `", `globalopts'"'
	syntax [, Z T CHI2 CMH CMHNocc BREslow TArone COCHranq QProfile HIGgins NCchi2 QGamma RFDist ///
		CC(passthru) noCC2 WGT(passthru) TRUNCate(passthru) ISQ(passthru) TAUSQ(passthru) ITOL(passthru) MAXTausq(passthru) REPS(passthru) MAXITer(passthru) QUADPTS(passthru) ]
		// last line ^^  "global" opts to compare with "specific model" opts

	local gTestStat `t' `z' `chi2' `cmh' `cmhnocc'
	local gHetOpt `breslow' `tarone' `cochranq' `qprofile' `higgins' `ncchi2' `qgamma'
	opts_exclusive `"`gTestStat'"' `""' 184
	opts_exclusive `"`gHetOpt'"' `""' 184		
	
	opts_exclusive `"`cc' `cc2'"' `""' 184	
	local ccopt `"`cc'`cc2'"'
	foreach opt in ccopt wgt truncate isq tausq itol maxtausq reps maxiter quadpts {
		if `"`old_`opt''"'!=`""' {
			local `opt' : copy local old_`opt'
		}
	}
	
	// If user-defined weights, summstat not specified and `params'==4, assume Inverse-Variance rather than exit with error
	// (do this now, so that hetopt is processed correctly.  user-defined weights are otherwise processed later)
	if `"`wgt'"'!=`""' & "`model'"=="mh" & "`modelorig'"=="null" local model fe
	
	
	** TEST STATISTICS
	if "`model'"!="mh" | "`summstat'"!="or" {
		if inlist("`teststat'", "cmh", "cmhnocc") {
			disp as err "Cannot specify {bf:`teststat'} test option without Mantel-Haenszel odds ratios"
			exit 198
		}
		else if "`teststat'"=="" & inlist("`gTestStat'", "cmh", "cmhnocc") {
			// disp as err "Note: global option {bf:`gTestStat'} is not applicable to all models; local defaults will apply"
			local gl_error `gl_error' `gTeststat'
		}
	}
	
	// Profile likelihood: cannot use t-based confidence interval
	// (or z with Bartlett's correction)
	if "`model'"=="pl" {
		if "`bartlett'"!="" {
			if "`teststat'"=="z" {
				disp as err `"Cannot specify option {bf:z} with Bartlett's correction"'
				exit 198
			}
			else if "`teststat'"=="" & "`gTestStat'"=="z" {
				// disp as err "Note: global option {bf:z} is not applicable to all models; local defaults will apply"
				local gl_error `gl_error' z
			}
		}
		else if "`teststat'"=="t" {
			disp as err `"Cannot specify option {bf:t} with Profile Likelihood"'
			exit 198
		}
		else if "`teststat'"=="" & "`gTestStat'"=="t" {
			// disp as err "Note: global option {bf:t} is not applicable to all models; local defaults will apply"
			local gl_error `gl_error' t
		}		
	}
	
	// MH, Peto and PL uses chi2 as default; HKSJ and Robust methods use t as default.
	// All three can be overridden with "z" option.
	// (Note that PL with "z" uses signed likelihood statistic.)
	else if "`model'"=="hc" {
		if "`teststat'"!="" {
			disp as err "Cannot specify option {bf:`teststat'} with Henmi-Copas model"
			exit 198
		}
		else if "`gTestStat'"!="" {
			// disp as err "Note: global option {bf:`gTestStat'} is not applicable to all models; local defaults will apply"
			local gl_error `gl_error' `gTestStat'
		}		
		local teststat u
	}
	
	// Default teststat
	if !inlist("`summstat'", "or", "") & !(inlist("`summstat'", "hr", "") & "`logrank'"!="") {
		if "`teststat'"=="chi2" {
			disp as err "Cannot specify {bf:chi2} test option without Mantel-Haenszel odds ratios"
			exit 198
		}
	}	
	if "`teststat'"=="" {
		if "`gTestStat'"!="" local teststat `gTestStat'
		else {
			if "`model'"=="peto" | "`bartlett'"!=""   local teststat chi2
			if "`hksj'`robust'"!="" | "`model'"=="kr" local teststat t
			if "`teststat'"==""                       local teststat z
		}
	}
	if "`model'"=="mh" {
		local cmhnocc
		if inlist("`teststat'", "cmh", "cmhnocc", "chi2") {
			if "`teststat'"!="cmh" local cmhnocc cmhnocc		// MH: cmhnocc is the same as chi2
			local teststat chi2
		}
	}
	
	
	** HETEROGENEITY
	// (in some situations could be overridden by "main" options)
	// (N.B. `petoq', `mhq' and iterative models e.g. `reml' are never allowed as user-specified options; they are defaults for internal use only)
	
	// Peto: default=petoq
	if "`model'"=="peto" {
		if inlist("`hetopt'", "breslow", "tarone") {
			disp as err "cannot specify {bf:`hetopt'} heterogeneity option without Mantel-Haenszel odds ratios"
			exit 198
		}
		else if "`hetopt'"=="" {
			if inlist("`gHetOpt'", "breslow", "tarone") {
				// disp as err "Note: global option {bf:`gHetOpt'} is not applicable to all models; local defaults will apply"
				local gl_error `gl_error' `gHetOpt'
				local hetopt petoq
			}
			else if "`gHetOpt'"=="" local hetopt petoq
			else local hetopt `gHetOpt'
		}
	}

	// Mantel-Haenszel: default=mhq; also allow cochranq, breslow, tarone
	else if "`model'"=="mh" {
		if "`hetopt'"=="qprofile" {
			disp as err "cannot specify {bf:qprofile} heterogeneity option with Mantel-Haenszel methods"
			exit 198
		}
		else if inlist("`hetopt'", "higgins", "ncchi2", "qgamma") {
			disp as err "heterogeneity option {bf:`hetopt'} only applicable with DerSimonian-Laird tausq estimator"
			exit 198
		}
		else if "`hetopt'"=="" {
			if inlist("`gHetOpt'", "higgins", "ncchi2", "qgamma") {
				// disp as err "Note: global option {bf:`gHetOpt'} is not applicable to all models; local defaults will apply"
				local gl_error `gl_error' `gHetOpt'
				local hetopt mhq
			}
			else if "`gHetOpt'"=="" local hetopt mhq
			else local hetopt `gHetOpt'
		}
	}	
	
	// DL-based methods: default=cochranq; also allow qprofile, higgins, ncchi2, qgamma
	else if inlist("`model'", "fe", "dl", "hc") {
		if inlist("`hetopt'", "breslow", "tarone") {
			disp as err "cannot specify {bf:`hetopt'} heterogeneity option without 2x2 data and Mantel-Haenszel odds ratios"
			exit 198
		}
		else if "`hetopt'"=="" {
			if inlist("`gHetOpt'", "breslow", "tarone") {
				// disp as err "Note: global option {bf:`gHetOpt'} is not applicable to all models; local defaults will apply"
				local gl_error `gl_error' `gHetOpt'
				local hetopt cochranq
			}		
			else if "`gHetOpt'"=="" local hetopt cochranq
			else local hetopt `gHetOpt'		
		}
	}
	
	// Heterogeneity CI by profiling likelihood (or tausq distribution in case of B+T Gamma)
	else if inlist("`model'", "mp", "ml", "pl", "reml", "gamma") {
		if inlist("`hetopt'", "higgins", "ncchi2", "qgamma") {
			disp as err "heterogeneity option {bf:`hetopt'} only applicable with DerSimonian-Laird tausq estimator"
			exit 198
		}
		else if inlist("`hetopt'", "breslow", "tarone") {
			disp as err "cannot specify {bf:`hetopt'} heterogeneity option without 2x2 data and Mantel-Haenszel odds ratios"
			exit 198
		}
		else if "`model'"!="mp" & "`hetopt'"=="qprofile" {		// to simplify matters, only allow "natural" CIs
			disp as err "cannot specify {bf:qprofile} heterogeneity option with model {bf:`model'}"
			exit 198
		}
		else if "`hetopt'"=="" {
			if inlist("`gHetOpt'", "higgins", "ncchi2", "qgamma", "breslow", "tarone") {
				// disp as err "Note: global option {bf:`gHetOpt'} is not applicable to all models; local defaults will apply"
				local gl_error `gl_error' `gHetOpt'
				local hetopt = cond("`model'"=="mp", "qprofile", cond("`model'"=="pl", "ml", "`model'"))	// PL & ML give same interval for tsq
			}		
			else if "`gHetOpt'"=="" {
				local hetopt = cond("`model'"=="mp", "qprofile", cond("`model'"=="pl", "ml", "`model'"))	// PL & ML give same interval for tsq
			}
			else local hetopt `gHetOpt'		
		}
	}
	
	// Other models: only allow cochranq
	else {
		if !inlist("`hetopt'", "", "cochranq") {
			disp as err "no heterogeneity options permitted with model {bf:`model'}"
			exit 198
		}
		else if "`hetopt'"=="" & !inlist("`gHetOpt'", "qprofile", "") {
			// disp as err "Note: global option {bf:`gHetOpt'} is not applicable to all models; local defaults will apply"
			local gl_error `gl_error' `gHetOpt'
		}
		local hetopt cochranq
	}
	
	
	** SENSITIVITY ANALYSIS
	if "`model'"=="sa" {
		if `"`tausq'"'!=`""' {
			cap confirm number `tausq'
			if _rc {
				disp as err `"Error in {bf:tausq()} suboption to {bf:sa()}; a single number was expected"'
				exit _rc
			}
			if `tausq'<0 {
				nois disp as err `"tau{c 178} value for sensitivity analysis cannot be negative"'
				exit 198
			}
			local tsqsa_opt `"tsqsa(`tausq')"'
			local modeltext `"SA(tau{c 178}=`tausq')"'
		}
		
		else {
			if `"`isq'"'==`""' local isq = 80
			else {
				cap confirm number `isq'
				if _rc {
					disp as err `"Error in {bf:isq()} suboption to {bf:sa()}; a single number was expected"'
					exit _rc
				}
				if `isq'<0 | `isq'>=100 {
					nois disp as err `"I{c 178} value for sensitivity analysis must be at least 0% and less than 100%"'
					exit 198
				}
			}
			local isqsa_opt `"isqsa(`isq')"'
			local modeltext `"SA(I{c 178}=`isq'%)"'
		}
		
		if `: word count `tsqsa' `isqsa'' >=2 {
			nois disp as err `"Only one of {bf:isq()} or {bf:tausq()} may be supplied as suboptions to {bf:sa()}"'
			exit 184
		}
	}
	
	// if NOT sensitivity analysis
	else {
		if `"`isq'"'!=`""' {
			nois disp as err `"option {bf:isq()} may only be specified when requesting a sensitivity analysis model"'
			exit 198
		}
		if `"`tausq'"'!=`""' {
			nois disp as err `"option {bf:tausq()} may only be specified when requesting a sensitivity analysis model"'
			exit 198
		}
	}
	

	** OTHER OPTIONS
	// Continuity correction: some checks needed for specific option, others for global... do them all together here
	if `"`ccopt'"'!=`""' {
		cap assert `params'==4
		if _rc {
			disp as err "Continuity correction only valid with 2x2 data"
			exit 184		
		}
		
		local 0 `", `ccopt'"'
		syntax [, CC(string) noCC2]

		if `"`cc'"'!=`""' {
			local 0 `"`cc'"'
			syntax [anything(name=ccval)] [, OPPosite EMPirical]
			if `"`ccval'"'!=`""' {
				confirm number `ccval'
			}
			else local ccval = 0.5
			
			if `"`cc2'"'!=`""' & `ccval' != 0 {
				disp as err `"Cannot specify both {bf:cc()} and {bf:nocc}; please choose one or the other"'
				exit 198
			}

			// Empirical CC valid with odds ratio only
			if `"`empirical'"'!=`""' & "`summstat'"!="or" {
				nois disp as err "Empirical continuity correction only valid with odds ratios"
				exit 198
			}

			// ensure continuity correction is valid
			if "`model'"=="peto" {
				nois disp as err "Note: continuity correction is incompatible with Peto method and will be ignored"
				local ccval = 0
			}
			else {
				cap assert `ccval'>=0 & `ccval'<1
				if _rc {
					nois disp as err "Invalid continuity correction: must be in range [0,1)"
					exit _rc
				}
			}
		}
		else local ccval = cond(`"`cc2'"'!=`""' | "`model'"=="peto", 0, 0.5)		// default
		if `ccval' > 0 {
			local ccopt_final `"cc(`ccval', `opposite' `empirical')"'
		}
	}
	
	// chi2 is only valid with:
	// - 2x2 Odds Ratios (including Peto)
	// - logrank HR
	// - Profile Likelihood
	if "`teststat'"=="chi2" {
		if "`summstat'"=="rr" & "`summorig'"=="null" & `params'==4 {
			local summstat or
			local summnew or
			nois disp as err `"Note: Chi-squared option or Peto model specified; odds ratios assumed"'
		}
	
		cap assert (inlist("`summstat'", "or", "") & `params'==4) ///
			| "`logrank'"!="" | "`model'"=="pl"		
		if _rc {
			nois disp as err `"Option {bf:chi2} is incompatible with other options"' 
			exit 184
		}
	}
	
	// User-defined weights
	if `"`wgt'"'!=`""' {
	
		// Only "vanilla" random-effects models are compatible
		// (i.e. those which simply estimate tau-squared and use it in the standard way)
		// cap assert !inlist("`model'", "kr", "gamma", "hc", "ivhet", "qe", "mu", "pl")
		cap {
			assert inlist("`model'", "user1", "fe", "dl", "dlb") ///
				| inlist("`model'", "ev", "vc", "hm", "b0", "bp") ///
				| inlist("`model'", "mp", "ml", "reml", "sj2s", "dk2s", "sa")
		}
		if _rc {
			nois disp as err "User-defined weights can only be used with standard random-effects models"
			nois disp as err "  which does not include {bf:`model'}"
			exit 198
		}
	}
	else if "`model'"=="user1" {
		di as err `"Must supply weight variable with option {bf:wgt(}{it:varname}{bf:)}"'
		di as err "with user-defined main analysis"
		exit 198
	}
	
	
	** COLLECT OPTIONS AND RETURN
	// Model opts	
	local modelopts `"`cmhnocc' `robust' `hksj' `bartlett' `skovgaard' `eim' `oim' `ccopt_final'"'
	local modelopts `"`modelopts' `wgt' `truncate' `tsqlevel' `isqsa_opt' `tsqsa_opt' `qe_opt' `init_opt'"'
	local modelopts `"`modelopts' `itol' `maxtausq' `reps' `maxiter' `quadpts'"'
	local modelopts `"`modelopts' `user1stats' `user2stats' `firststats' `secondstats'"'	// for user-defined models
	local modelopts = trim(itrim(`"`modelopts'"'))
	

	// Model description to display on-screen (shorthand, in case of multiple models)
	if "`model'"=="ivhet" local modeltext IVHet
	else if "`model'"=="dlb" local modeltext DLb
	else if "`model'"=="vc" local modeltext Hedges
	else if "`model'"=="ev" local modeltext "Emp. Var."
	else if "`model'"=="gamma" local modeltext BT
	else if inlist("`model'", "bp", "b0") local modeltext `"Rukhin `=upper("`model'")'"'
	else if "`model'"=="kr" local modeltext "REML+KR"
	else if "`model'"=="peto" local modeltext "Peto"
	else if "`bartlett'"!="" local modeltext "PL+Bart."
	else if "`skovgaard'"!="" local modeltext "PL+Skov."
	else if "`model'"!="sa" local modeltext = upper("`model'")

	if "`hksj'"!=""   local modeltext "`modeltext'+HKSJ"
	if "`robust'"!="" local modeltext "`modeltext'+Rob."
	
	// Return
	sreturn clear
	sreturn local model `model'
	sreturn local modelorig `modelorig'
	sreturn local modeltext `modeltext'
	
	sreturn local modelopts `"`modelopts'"'					// Additional model options (for PerformPooling and/or DrawTableAD)
	sreturn local teststat `teststat'
	sreturn local hetopt  `hetopt'
	if `"`wgt'"'!=`""' sreturn local wgtopt `"`wgt'"'
	else sreturn local wgtopt default

	// Corrective macro
	sreturn local summnew `summnew'

	// Error prompts
	sreturn local gl_error `gl_error'
		
end





********************************************************************************

* PerformMetaAnalysis
// Create list of "pooling" variables
// Run meta-analysis on whole dataset ("overall") and, if requested, by subgroup
// If cumul/influence, subroutine "CumInfLoop" is run first, to handle the intermediate steps
// Then (in any case), subroutine "PerformPooling" is run.
// (called directly by metan.ado)

// N.B. [Sep 2018] takes bits of old (v2.2) MainRoutine and PerformMetaAnalysis subroutines

// SEP 2019:  We are now doing this **one model at a time**


program define PerformMetaAnalysis, rclass sortpreserve

	syntax varlist(numeric min=3 max=7) [if] [in], SORTBY(varlist) MODEL(name) ///
		[BY(string) SUMMSTAT(name) TESTSTAT(name) HETOPT(name) ///
		OUTVLIST(varlist numeric min=5 max=8) XOUTVLIST(passthru) ROWNAMES(namelist) ///
		noOVerall noSUbgroup OVWt SGWt ALTWt WGT(varname numeric) CUmulative INFluence USE3(passthru) ///
		LOGRank LEVEL(passthru) RFDist RFLEVEL(passthru) TSQLEVEL(passthru) CCVAR(passthru) CC(passthru) /// from `opts_model'; needed in main routine
		* ]

	local opts_model `"`macval(options)'"'		// model`j'opts
	marksample touse, novarlist		// -novarlist- option prevents -marksample- from setting `touse' to zero if any missing values in `varlist'
									// we want to control this behaviour ourselves, e.g. by using KEEPALL option
	gettoken _USE invlist : varlist
	tokenize `outvlist'
	args _ES _seES _LCI _UCI _WT _NN

	local nrfd = 0		// initialize marker of "subgroup has < 3 studies" (only for rfdist)
	local nmiss = 0		// initialize marker of "pt. numbers are missing in one or more trials"
	local nsg = 0		// initialize marker of "one or more subgroups contain only a single valid estimate" (only for cumul/infl)
	
	// If model is "user1", normalise user-supplied weights but then exit
	if inlist("`model'", "user1", "user2") {
		if "`model'"=="user1" {
			if "`by'"!="" {
				di as err "Cannot use option {bf:by()} with user-defined main analysis"
				exit 198
			}
			if `"`cumulative'`influence'"'!=`""' {
				di as err "Cannot use option {bf:`cumulative'`influence'} with user-defined main analysis"
				exit 198
			}
		
			// weights
			summ `wgt' if `touse', meanonly
			qui replace `_WT' = 100 * `wgt' / r(sum) if `touse' & `_USE'==1
			
			// total number of patients
			if `"`_NN'"'!=`""' {
				summ `_NN' if `touse' & `_USE'==1, meanonly
				return scalar n = r(sum)
			}
			
			// total number of studies
			qui count if `touse' & `_USE'==1		
			return scalar k = r(N)
		}
		
		c_local nrfd  = `nrfd'
		c_local nsg   = `nsg'
		c_local nmiss = `nmiss'
		exit 0		// exit subroutine and continue without error
	}
	
	// sensitivity analysis
	if "`model'"=="sa" & "`by'"!="" {
		nois disp as err `"Sensitivity analysis cannot be used with {bf:by()}"'
		exit 198
	}	
	

	* Create list of "pooling" tempvars to pass to ProcessPoolingVarlist
	// and thereby create final generic list of "pooling" vars to use within MetaAnalysisLoop
	// (i.e. tempvars that are only needed within this subroutine)
	
	// Logic:
	// If M-H pooling, then M-H heterogeneity
	// If Peto pooling, then Peto heterogeneity
	// If generic I-V with 2x2 count data, then either Cochran or M-H heterogeneity (or Breslow-Day, but only if OR)
	
	// So:
	// M-H heterogeneity if (a) M-H pooling or (b) generic I-V (fe, re) with 2x2 count data and cochran/breslow not specified (M-H is default in this situation)
	// Peto heterogeneity if (a) Peto pooling or (b) generic I-V (fe, re) with 2x2 count data and cochran/breslow not specified AND OR/HR ONLY
	// Breslow-Day heterogeneity only if OR and user-specified
	// Cochran heterogeneity only if generic I-V (and user-specified if necessary)
	
	// So:
	// If OR + M-H then het can be only be M-H
	// If OR + Peto then het can only be Peto
	// If OR + RE I-V then het can be M-H (default), Peto, Breslow or Cochran -- the only situation where "peto" option can be combined
	// If OR + FE I-V then het can be Cochran (default) or Breslow

	// If HR + Peto then het can only be Peto
	// If HR + RE I-V then het can be Peto (default) or Cochran
	
	// If RR/RD + M-H then het can only be M-H
	// If RR/RD + RE I-V then het can be M-H (default) or Cochran
	
	// If anything else + FE I-V then het can only be Cochran
	
	local params : word count `invlist'
	if `params' > 3 | "`logrank'"!="" {			// all except generic inverse-variance input

		if `params' == 4 {		// Binary outcome (OR, Peto, RR, RD)

			if "`summstat'"=="or" {
				if "`model'"=="mh" {							// extra tempvars for Mantel-Haenszel OR and/or het
					tempvar r s pr ps qr qs
					local tvlist `r' `s' `pr' `ps' `qr' `qs'
				}
				if inlist("`teststat'", "chi2", "cmh") {		// extra tempvars for chi-squared test (incl. Peto OR and M-H CMH test)
					tempvar oe va
					local tvlist `tvlist' `oe' `va'
				}
			}

			else if inlist("`summstat'", "rr", "rrr") {				// RR/RRR
				tempvar r s
				local tvlist `r' `s'
				
				if "`model'"=="mh" {							// extra tempvars for Mantel-Haenszel OR and/or het
					tempvar p
					local tvlist `tvlist' `p'
				}
			}
			
			else if "`summstat'" == "rd" & "`model'"=="mh" {		// RD
				tempvar rdwt rdnum vnum
				local tvlist `rdwt' `rdnum' `vnum'
			}
		}
		
		else if "`logrank'"!="" {		// logrank HR (O-E & V -- already supplied in `invlist')
			assert `params'==2
		}

		//  Generate study-level effect size variables `_ES' and `_seES',
		//  plus variables used to generate overall/subgroup statistics
		cap nois ProcessPoolingVarlist `_USE' `invlist' if `touse', ///
			outvlist(`outvlist') summstat(`summstat') model(`model') ///
			tvlist(`tvlist') `logrank' `cc' `ccvar' teststat(`teststat')
		
		if _rc {
			nois disp as err `"Error in {bf:metan.ProcessPoolingVarlist}"'
			c_local err noerr		// tell -metan- not to also report an "error in metan.PerformMetaAnalysis"
			exit _rc
		}

		local oevlist `s(oevlist)'
		local mhvlist `s(mhvlist)'
		
	}	// end if `params' > 3 | "`logrank'"!=""
	
	// Special case:  need to generate `_seES' if ES + CI were provided; assume normal distribution and 95% coverage
	else if `params'==3 {
		if `"`level'"'!=`""' {
			tokenize `invlist'			// if level() option supplied, requesting coverage other than 95%,
			args _ES_ _LCI_ _UCI_		// need to derive _seES from the *original* confidence limits supplied in `invlist' (assumed to be 95% !!)
		}
		else {
			local _LCI_ `_LCI'
			local _UCI_ `_UCI'
		}
		qui replace `_seES' = (`_UCI_' - `_LCI_') / (2*invnormal(.5 + 95/200)) if `touse' & `_USE'==1
	}

	// We should now have _ES and _seES defined throughout.
	// Quick double-check that studies with insufficient data are identified ("`_USE'==2")
	// (should already have been done by either -ipdmetan- or -ProcessInputVarlist-)
	// (but in special cases, e.g. if `nocc', may still be some missings)
	// N.B. THIS IS THE ONLY PLACE OUTSIDE ProcessInputVarlist WHERE _USE MAY BE SET TO 2

	// AMENDED SEP 2019 FOR v3.4:  THIS MIGHT NOW VARY BETWEEN MODELS, SO LEAVE ALONE
	/*
	if "`esmethod'"!="mh" {
		qui replace `_USE' = 2 if `touse' & `_USE'==1 & missing(`_ES', `_seES')
	}
	qui count if `_USE'==1
	if !r(N) exit 2000	
	*/
	
	// if B0 estimator, must have _NN for all studies with an effect size (i.e. `_USE'==1)
	if "`model'"=="b0" {
		cap {
			confirm numeric var `_NN'
			assert `_NN'>=0 & !missing(`_NN') if `_USE'==1
		}
		if _rc {
			nois disp as err `"Participant numbers not available for all studies; cannot calculate tau{c 178} estimator B0"'
			exit 198
		}
		local nptsopt npts(`_NN')	// to send to PerformPooling / CumInfLoop
	}								// N.B. `npts' is otherwise undefined in this subroutine
	
	
	// setup for subgroups and/or cumulative MA
	tempname Q Qsum k n
	scalar `Q'    = 0
	scalar `Qsum' = 0
	scalar `k'    = .
	scalar `n'    = .
	
	
	********************
	* Overall analysis *
	********************
		
	if `"`overall'"'==`""' | `"`ovwt'"'!=`""'  {

		// if ovwt, pass `_WT' to PerformPooling to be filled in
		// otherwise, PerformPooling will generate a tempvar, and `_WT' will remain empty
		local wtvar = cond(`"`ovwt'"'!=`""', `"`_WT'"', `""')
	

		** Cumulative/influence analysis
		// Run extra loop to store results of each iteration within the currrent dataset (`xoutvlist')
		if `"`cumulative'`influence'"' != `""' {

			cap nois CumInfLoop `_USE' `_ES' `_seES' if `touse' & `_USE'==1, sortby(`sortby') ///
				model(`model') summstat(`summstat') teststat(`teststat')  hetopt(`hetopt') ///
				mhvlist(`mhvlist') oevlist(`oevlist') invlist(`invlist') `xoutvlist' ///
				wgt(`wgt') wtvar(`wtvar') rownames(`rownames') `nptsopt' `use3' ///
				`cumulative' `influence' `ovwt' `level' `rfdist' `rflevel' `tsqlevel' `opts_model'
			
			if _rc {
				if `"`err'"'==`""' {
					if _rc==1 nois disp as err `"User break in {bf:metan.CumInfLoop}"'
					else nois disp as err `"Error in {bf:metan.CumInfLoop}"'
				}
				c_local err noerr		// tell -metan- not to also report an "error in metan.PerformMetaAnalysis"
				exit _rc
			}

			local xwt `r(xwt)'			// extract _WT2 from `xoutvlist'
		}
		
		
		** Main meta-analysis
		// If `cumulative', the last iteration of the loop above is equivalent to a standard "overall" pooling;
		//   hence, no need to run PerformPooling again.
		// If `influence', this is not the case.
				
		if `"`cumulative'"'==`""' {

			// If only one study, display warning message if appropriate
			// (the actual change in method is handled by PerformPooling)
			qui count if `touse' & `_USE'==1
			if r(N)==1 {
				if !inlist("`model'", "fe", "mh") {
					nois disp as err "Note: Only one estimate found; random-effects model not used"
				}
			}

			cap nois PerformPooling `_ES' `_seES' if `touse' & `_USE'==1, ///
				model(`model') summstat(`summstat') teststat(`teststat') hetopt(`hetopt') ///
				mhvlist(`mhvlist') oevlist(`oevlist') invlist(`invlist') `nptsopt' wtvar(`wtvar') wgt(`wgt') ///
				`logrank' `rfdist' `rflevel' `tsqlevel' `opts_model'			
			
			if _rc {
				if _rc==1 nois disp as err `"User break in {bf:metan.PerformPooling}"'
				else nois disp as err `"Error in {bf:metan.PerformPooling}"'
				c_local err noerr		// tell -metan- not to also report an "error in metan.MetaAnalysisLoop"
				exit _rc
			}
			
			// pooling failed (may not have caused an actual error)
			if missing(r(eff), r(se_eff)) exit 2002

		}
		
		
		** Save statistics in matrix
		tempname ovstats
		local r : word count `rownames'
		matrix define   `ovstats' = J(`r', 1, .)
		matrix rownames `ovstats' = `rownames'
		
		local toremove
		if inlist("`model'", "mh", "peto", "fe", "ivhet", "qe", "mu") ///
			| inlist("`model'", "kr", "gamma", "hc", "pl") {
			local toremove rflci rfuci
		}
		if inlist("`hetopt'", "petoq", "mhq") local toremove `toremove' tausq sigmasq
		local rownames_reduced : list rownames - toremove
		
		foreach el in `rownames_reduced' {
			local rownumb = rownumb(`ovstats', "`el'")
			if !missing(`rownumb') {
				mat `ovstats'[`rownumb', 1] = r(`el')
			}
		}
		scalar `k' = r(k)			// overall number of studies

		
		// Warning messages & error codes r.e. confidence limits for iterative tausq
		if inlist("`hetopt'", "qprofile", "ml", "reml", "gamma") {		
			local maxtausq2 = r(maxtausq)		// take maxtausq from PerformPooling (10* D+L estimate)
			local 0 `", `opts_model'"'
			syntax [, ITOL(real 1e-8) MAXTausq(real -9) REPS(real 1000) MAXITer(real 1000) QUADPTS(real 100) * ]

			if !inlist("`model'", "dlb", "gamma") {
				if r(rc_tausq)==1 nois disp as err `"Note: tau{c 178} point estimate failed to converge within `maxiter' iterations"'
				else if r(rc_tausq)==3 {
					if `maxtausq'==-9 nois disp as err `"Note: tau{c 178} greater than default value {bf:maxtausq(}`maxtausq2'{bf:)}; try increasing it"'
					else nois disp as err `"Note: tau{c 178} greater than `maxtausq'; try increasing {bf:maxtausq()}"'
				}
				else if missing(r(tausq)) {
					nois disp as err `"Note: tau{c 178} point estimate could not be found; possible discontinuity in search interval"'
					exit 498
				}
				return scalar rc_tausq = r(rc_tausq)		// whether tausq point estimate converged
			}
			
			if "`model'"!="dlb" {
				if r(rc_tsq_lci)==1 nois disp as err `"Note: Lower confidence limit of tau{c 178} failed to converge within `maxiter' iterations; try increasing {bf:maxiter()}"'
				else if missing(r(tsq_lci)) {
					nois disp as err `"Note: Lower confidence limit of tau{c 178} could not be found; possible discontinuity in search interval"'
				}
					
				if r(rc_tsq_uci)==1 nois disp as err `"Note: Upper confidence limit of tau{c 178} failed to converge within `maxiter' iterations; try increasing {bf:maxiter()}"'
				else if r(rc_tsq_uci)==3 {
					if `maxtausq'==-9 nois disp as err `"Note: Upper confidence limit of tau{c 178} greater than default value {bf:maxtausq(}`maxtausq2'{bf:)}; try increasing it"'
					else nois disp as err `"Note: Upper confidence limit of tau{c 178} greater than `maxtausq'; try increasing {bf:maxtausq()}"'
				}
				else if missing(r(tsq_uci)) {
					nois disp as err `"Note: Upper confidence limit of tau{c 178} could not be found; possible discontinuity in search interval"'
				}
				return scalar rc_tsq_lci = r(rc_tsq_lci)		// whether tausq lower confidence limit converged
				return scalar rc_tsq_uci = r(rc_tsq_uci)		// whether tausq upper confidence limit converged
			}

			if "`model'"=="pl" {
				if r(rc_eff_lci)==1 nois disp as err `"Note: Lower confidence limit of effect size failed to converge within `maxiter' iterations; try increasing {bf:maxiter()}"'
				else if r(rc_eff_lci)>1 | missing(`r(eff_lci)') {
					nois disp as err `"Note: Lower confidence limit of effect size could not be found; possible discontinuity in search interval"'
				}
				if r(rc_eff_uci)==1 nois disp as err `"Note: Upper confidence limit of effect size failed to converge within `maxiter' iterations; try increasing {bf:maxiter()}"'
				else if r(rc_eff_uci)>1 | missing(`r(eff_uci)') {
					nois disp as err `"Note: Upper confidence limit of effect size could not be found; possible discontinuity in search interval"'
				}				
				return scalar rc_eff_lci = r(rc_eff_lci)		// whether ES lower confidence limit converged
				return scalar rc_eff_uci = r(rc_eff_uci)		// whether ES upper confidence limit converged					
			}
		}

		return add					// add anything else returned by PerformPooling to return list of PerformMetaAnalysis
									// e.g. r(OR), r(RR); tsq-related stuff; chi2
		
		// Normalise weights overall (if `ovwt')
		if `"`ovwt'"'!=`""' {
			local _WT2 = cond(`"`xwt'"'!=`""', `"`xwt'"', `"`_WT'"')			// use _WT2 from `xoutvlist' if applicable
			summ `_WT' if `touse', meanonly
			qui replace `_WT2' = 100*cond(`"`altwt'"'!=`""', `_WT', `_WT2') / r(sum) ///
				if `touse' & `_USE'==1		// use *original* weights (_WT) rather than cumul/infl weights (_WT2) if `altwt'
		}

		// Find and store number of participants
		if `"`_NN'"'!=`""' {
			summ `_NN' if `touse' & `_USE'==1, meanonly
			mat `ovstats'[rownumb(`ovstats', "npts"), 1] = r(sum)
			scalar `n' = r(sum)
		}
		
		return matrix ovstats = `ovstats'
		
	}		// end if `"`overall'"'==`""' | `"`ovwt'"'!=`""'

	
	
	******************************************
	* Analysis within study subgroups (`by') *
	******************************************
	
	if (`"`by'"'!=`""' & `"`subgroup'"'==`""') | `"`sgwt'"'!=`""' {
	
		// Initialize markers of subgroup-related errors
		// (plus Mata iterative functions failed to converge etc ... this is done on-the-fly for `overall')
		foreach el in nrc2000 nrc2002 ntausq ntsqlci ntsquci nefflci neffuci {
			local `el' = 0
		}

		// Initialize counts of studies and of pts., in case not already counted by `overall'
		tempname kOV nOV
		scalar `kOV' = 0
		scalar `nOV' = .
		
		// Initialise matrix to hold subgroup stats (matrix bystats)
		qui levelsof `by' if `touse' & inlist(`_USE', 1, 2), missing local(bylist)	// "missing" since `touse' should already be appropriate for missing yes/no
		local nby : word count `bylist'
		return local nby = `nby'
		
		// Remove unnecessary rownames (e.g. oe, v; df_kr; tausq, sigmasq, rfdist)
		local toremove
		if !("`logrank'"!="" | "`model'"=="peto") local toremove OE V
		if "`model'"!="kr" local toremove `toremove' df_kr
		if inlist("`model'", "mh", "peto", "fe", "ivhet", "qe", "mu") ///
			| inlist("`model'", "kr", "gamma", "hc", "pl") {
			local toremove `toremove' rflci rfuci
		}
		if !inlist("`hetopt'", "qprofile", "ml", "reml", "gamma") local toremove `toremove' tsq_lci tsq_uci
		if inlist("`hetopt'", "cochranq", "petoq", "mhq") local toremove `toremove' H_lci H_uci Isq_lci Isq_uci HsqM_lci HsqM_uci
		if inlist("`hetopt'", "petoq", "mhq") local toremove `toremove' tausq sigmasq
		foreach el in z t chi2 u {
			if "`teststat'"!="`el'" local toremove `toremove' `el'
		}
		local rownames_reduced : list rownames - toremove
		
		tempname bystats mwt
		matrix define   `bystats' = J(`: word count `rownames_reduced'', `nby', .)
		matrix rownames `bystats' = `rownames_reduced'
		local modelstr = strtoname("`model'", 0)
		matrix coleq    `bystats' = `bylist'
		
		// if sgwt, pass `_WT' to PerformPooling to be filled in
		// otherwise, PerformPooling will generate a tempvar, and `_WT' will remain empty		
		local wtvar = cond(`"`sgwt'"'!=`""', `"`_WT'"', `""')
		
		local i = 0
		foreach byi of local bylist {
		
			** Cumulative/influence analysis
			// Run extra loop to store results of each iteration within the currrent dataset (`xoutvlist')
			if `"`cumulative'`influence'"' != `""' {
			
				cap nois CumInfLoop `_USE' `_ES' `_seES' if `touse' & `_USE'==1 & `by'==`byi', sortby(`sortby') ///
					model(`model') summstat(`summstat') teststat(`teststat')  hetopt(`hetopt') ///
					mhvlist(`mhvlist') oevlist(`oevlist') invlist(`invlist') `xoutvlist' ///
					wgt(`wgt') wtvar(`wtvar') rownames(`rownames') `nptsopt' `use3' ///
					`cumulative' `influence' `sgwt' `level' `rfdist' `rflevel' `tsqlevel' `opts_model'			

				if _rc {
					if _rc==1 {
						nois disp as err "User break in {bf:metan.CumInfLoop}"
						c_local err noerr		// tell -metan- not to also report an "error in metan.PerformMetaAnalysis"
						exit _rc
					}
					else if !inlist(_rc, 2000, 2002) {
						if `"`err'"'==`""' nois disp as err `"Error in {bf:metan.CumInfLoop}"'
						c_local err noerr		// tell -metan- not to also report an "error in metan.PerformMetaAnalysis"
						exit _rc
					}
					else if _rc==2000 local nrc2000 = 2000
					else if _rc==2002 local nrc2002 = 2002
				}
				else {
					if !inlist(r(rc_tausq), 0, .)   local ntausq = 1
					if !inlist(r(rc_tsq_lci), 0, .) local ntsqlci = 1
					if !inlist(r(rc_tsq_uci), 0, .) local ntsquci = 1
					if !inlist(r(rc_eff_lci), 0, .) local nefflci = 1
					if !inlist(r(rc_eff_uci), 0, .) local neffuci = 1
				}
				
				local xwt `r(xwt)'			// extract _WT2 from `xoutvlist'
			}

			
			** Main subgroup meta-analysis
			// If `cumulative', the last iteration of the loop above is equivalent to a standard "overall" pooling;
			//   hence, no need to run PerformPooling again.
			// If `influence', this is not the case.
			cap nois PerformPooling `_ES' `_seES' if `touse' & `_USE'==1 & `by'==`byi', ///
				model(`model') summstat(`summstat') teststat(`teststat')  hetopt(`hetopt') ///
				mhvlist(`mhvlist') oevlist(`oevlist') invlist(`invlist') `nptsopt' wtvar(`wtvar') wgt(`wgt') ///
				`logrank' `rfdist' `rflevel' `tsqlevel' `opts_model'			

			if _rc {
				if _rc==1 {
					nois disp as err "User break in {bf:metan.PerformPooling}"
					c_local err noerr		// tell -metan- not to also report an "error in metan.PerformMetaAnalysis"
					exit _rc
				}
				else if !inlist(_rc, 2000, 2002) {
					if `"`err'"'==`""' nois disp as err `"Error in {bf:metan.PerformPooling}"'
					c_local err noerr		// tell -metan- not to also report an "error in metan.PerformMetaAnalysis"
					exit _rc
				}
				else if _rc==2000 local nrc2000 = 2000
				else if _rc==2002 local nrc2002 = 2002
			}
			else {
				if !inlist(r(rc_tausq), 0, .)   local ntausq = 1
				if !inlist(r(rc_tsq_lci), 0, .) local ntsqlci = 1
				if !inlist(r(rc_tsq_uci), 0, .) local ntsquci = 1
				if !inlist(r(rc_eff_lci), 0, .) local nefflci = 1
				if !inlist(r(rc_eff_uci), 0, .) local neffuci = 1
			}
						
			// update `bystats' matrix and return subgroup stats (if PerformPooling ran successfully)
			local ++i
			
			if !_rc {

				// update `bystats' matrix
				foreach el in `rownames_reduced' {
					local rownumb = rownumb(`bystats', "`el'")
					if !missing(`rownumb') {
						mat `bystats'[rownumb(`bystats', "`el'"), `i'] = r(`el')
					}
				}

				// update running sums
				scalar `Qsum' = `Qsum' + r(Q)
				scalar `kOV'  = `kOV'  + r(k)
				if `"`_NN'"'!=`""' {
					summ `_NN' if `touse' & `_USE'==1 & `by'==`byi', meanonly
					mat `bystats'[rownumb(`bystats', "npts"), `i'] = r(sum)
					scalar `nOV' = cond(missing(`nOV'), 0, `nOV') + r(sum)
				}
				
				// Normalise weights by subgroup (if `sgwt')
				if `"`sgwt'"'!=`""' {
					local _WT2 = cond(`"`xwt'"'!=`""', `"`xwt'"', `"`_WT'"')		// use _WT2 from `xoutvlist' if applicable
					summ `_WT' if `touse' & `_USE'==1 & `by'==`byi', meanonly
					qui replace `_WT2' = 100*cond(`"`altwt'"'!=`""', `_WT', `_WT2') / r(sum) ///
						if `touse' & `_USE'==1 & `by'==`byi'		// use *original* weights (_WT) rather than cumul/infl weights (_WT2) if `altwt'
				}
				
				// Save subgroup weights from multiple models, in matrix `mwt'
				// N.B. Need to do this here, not within PerformPooling, since otherwise it won't have been normalised
				else {
					summ `_WT' if `touse' & `_USE'==1 & `by'==`byi', meanonly
					matrix `mwt' = nullmat(`mwt') , r(sum)
				}
			}
			
		}	// end foreach byi of local bylist

		if (`"`overall'"'==`""' | `"`ovwt'"'!=`""') {
			assert `kOV' == `k'		// check that sum of subgroup `k's = previously-calculated overall `k'
			assert `nOV' == `n'		// check that sum of subgroup `n's = previously-calculated overall `n'
		}
		else {
			scalar `k' = `kOV'		// if no previously-calculated overall `k', *define* it to be sum of subgroup `k's
			scalar `n' = `nOV'		// if no previously-calculated overall `n', *define* it to be sum of subgroup `n's
		}

		// Return `Qsum'
		return scalar Qsum  = `Qsum'
		
		// Return `bystats' matrix
		return matrix bystats = `bystats'

		// Return `mwt' matrix, containing (normalised) model-specific subgroup weights
		cap confirm matrix `mwt'
		if !_rc {
			matrix colnames `mwt' = `bylist'
			matrix rownames `mwt' = `modelstr'
			return matrix mwt = `mwt'
		}
		
		// Error messages: return as c_local, in order that each unique error message is only displayed once
		foreach el in nrc2000 nrc2002 ntausq ntsqlci ntsquci nefflci neffuci {
			c_local `el' = ``el''
		}
		
	}	// end if `"`by'"'!=`""'
	
	c_local nrfd = `nrfd'
	c_local nsg  = `nsg'
	

	** Check numbers of participants
	// This part is now just a check that patients and trials are behaving themselves for each model
	if `"`_NN'"'!=`""' {
		summ `_NN' if `touse' & `_USE'==1, meanonly

		// Check if we have same number of values for _NN as there are trials
		// if not, some _NN values must be missing; display warning
		cap assert `r(N)'==`k'
		if _rc {
			if `"`by'"'!=`""' & `"`subgroup'"'==`""' {
				cap assert !`nmiss'
				if !_rc local nmiss = 1
			}
			if `"`xoutvlist'"'!=`""' {
				disp as err "      " + upper(`cumulative'`influence') + " patient numbers cannot be returned"
				c_local _NN				// clear macro _NN, so that by-trial patient numbers are no longer available
			}
		}
		else scalar `n' = r(sum)
		return scalar n = cond(`n'==0, ., `n')
	}
	c_local nmiss = `nmiss'
	
	return scalar k = `k'

end
	
	
	

	
**********************************************************************

* PrintDesc
// Print descriptive text to screen, above table

program define PrintDesc, sclass
	
	syntax, MODELLIST(namelist) [SUMMSTAT(name) ///
		noPOOL WGTOPTLIST(string) LOG LOGRank CUmulative INFluence INTERaction SUMMARYONLY noTABle ///
		CC(string) CCVAR(name) ISQSA(real 80) TSQSA(real -99) INIT(name) ///
		BYAD SOURCE(passthru) LRVLIST(passthru) ESTEXP(string) EXPLIST(passthru) IPDXLINE(passthru) IPDMETAN ///	/*opts_ipdm*/
		BArtlett HKsj RObust SKovgaard TRUNCate(string) * ]
	
	local m : word count `modellist'
	gettoken model1 rest : modellist
	gettoken model2 rest : rest

	// Build up description of effect estimate type (interaction, cumulative etc.)
	local pooltext = cond(`"`cumulative'"'!=`""', "Cumulative meta-analysis of", ///
		cond(`"`influence'"'!=`""', "Influence meta-analysis of", ///
		cond(`"`pool'"'==`""' | `"`: word 1 of `model''"'=="user1", "Meta-analysis pooling of", "Presented effect estimates are")))
	
	// Again, if passed from -ipdmetan- with "generic" effect measure,
	//   print non-standard text including `estexp':
	if "`estexp'"!="" {
		if `"`interaction'"'!=`""' local pooltext "`pooltext' interaction effect estimate"
		else if `"`explist'"'!=`""' local pooltext "`pooltext' user-specified effect estimate"
		else local pooltext "`pooltext' main (treatment) effect estimate"
		di _n as text "`pooltext'" as res " `estexp'"
	}

	// Standard -metan- text:
	else if `"`summstat'"'==`""' {
		if `"`pool'"'==`""' | `"`model1'"'=="user1" | `"`ipdmetan'"'!=`""' di _n as text "`pooltext' aggregate data"
	}
	else {
		local logtext = cond(`"`log'"'!=`""', `"log "', `""')			// add a space if `log'
		
		if "`summstat'"=="rr" local efftext `"`logtext'Risk Ratios"'
		else if "`summstat'"=="irr" local efftext `"`logtext'Incidence Rate Ratios"'
		else if "`summstat'"=="rrr" local efftext `"`logtext'Relative Risk Ratios"'
		else if "`summstat'"=="or"  local efftext `"`logtext'Odds Ratios"'
		else if "`summstat'"=="rd"  local efftext `" Risk Differences"'
		else if "`summstat'"=="hr"  local efftext `"`logtext'Hazard Ratios"'
		else if "`summstat'"=="shr" local efftext `"`logtext'Sub-hazard Ratios"'
		else if "`summstat'"=="tr"  local efftext `"`logtext'Time Ratios"'
		else if "`summstat'"=="wmd" local efftext `" Weighted Mean Differences"'
		else if inlist("`summstat'", "cohen", "glass", "hedges") {
			local efftext " Standardised Mean Differences"
			local ss_proper = strproper("`summstat'")
			local efftextf `" as text " by the method of " as res "`ss_proper'""'
		}
			
		// Study-level effect derivation method
		if "`logrank'"!="" local efftext "Peto (logrank) `efftext'"
		else if "`model1'"=="peto" & `m'==1 local efftext "Peto `efftext'"
		di _n as text "`pooltext'" as res " `efftext'" `efftextf' `continue'
	}
	
	if `"`pool'"'!=`""' {
		if `"`model1'"'=="user1" {
			disp as text "with " as res "user-specified pooled estimates"	
		}
	}
	else {
		// Need to handle user-defined second model
		if `m' > 1 & "`model2'"!="user2" {
			disp as text "using " as res "multiple analysis methods"
		}
	
		else {	
			// fpnote = "NOTE: Weights are from Mantel-Haenszel model"
			// or "NOTE: Weights are from random-effects model"
			// or "NOTE: Weights are user-defined"
			// NOTE for multiple models: fpnote refers to weights ==> first model only

			// Pooling method (Mantel-Haenszel; fixed-effect; random-effects)
			if "`model1'"=="mh" {
				disp as text "using the " as res "Mantel-Haenszel" as text " method"
				local fpnote "NOTE: Weights are from Mantel-Haenszel model"								// for forestplot
			}
			else if !inlist("`model1'", "ivhet", "qe", "peto") {
				if "`model1'"=="fe" {
					local modeltext "fixed-effect inverse-variance"
					if `m' > 1 local fpnote "NOTE: Weights are from fixed-effects model"				// for forestplot, if multiple models
				}
				else {	
					local modeltext "random-effects inverse-variance"
					if "`model1'"!="sa" local fpnote "NOTE: Weights are from random-effects model"		// for forestplot
				}
				local the = cond("`model1'"=="qe", "", "the ")		
				disp as text `"using `the'"' as res `"`modeltext'"' as text " model"
			}
			
			// Doi's IVHet and Quality Effects models
			else if "`model1'"!="peto" {
				local modeltext = cond("`model1'"=="ivhet", "Doi's IVHet", "Doi's Quality Effects")
				disp as text "using " as res `"`modeltext'"' as text " model"
				local fpnote `"NOTE: Weights are from `modeltext' model"'								// for forestplot
			}	
			
			// Profile likelihood
			if "`model1'"=="pl" {
				local continue = cond(`"`bartlett'`skovgaard'"'!=`""', "_c", "")
				disp as text `"estimated using "' as res "Profile Likelihood" `continue'
				if "`bartlett'"!="" disp as text " with " as res `"Bartlett's correction"'
				else if "`skovgaard'"!="" disp as text " with " as res `"Skovgaard's correction"'
			}	
				
			// Gamma alternative weighting
			else if "`model1'"=="gamma" {
				disp as text `"with "' as res "Biggerstaff-Tweedie approximate Gamma" as text `" weighting"'
			}

			// HKSJ and SJ Robust variance estimators
			else if `"`hksj'`robust'"'!=`""' {
				if "`hksj'"!="" {
					if `"`truncate'"'==`""' local vcetext `" (untruncated)"'
					else if inlist(`"`truncate'"', `"one"', `"1"') local vcetext `" (truncated at 1)"'
					else if `"`truncate'"'==`"zovert"' local vcetext `" (truncated at {it:z}/{it:t})"'
					local vcetext `"Hartung-Knapp-Sidik-Jonkman`vcetext'"'
				}
				else local vcetext "Sidik-Jonkman robust"
				disp as text "with the " as res "`vcetext'" as text " variance estimator"
			}
			
			// Kenward-Roger variance correction
			else if "`model1'"=="kr" {
				disp as text "with " as res "Kenward-Roger" as text " variance correction"
			}		
				
			// Henmi-Copas
			else if "`model1'"=="hc" {
				disp as text "estimated using " as res `"Henmi and Copas's approximate exact distribution"'
			}
			
			// Multiplicative heterogeneity model
			else if "`model1'"=="mu" {
				disp as text "with " as res `"multiplicative heterogeneity"'
			}
			
			// Two-step estimators
			else if "`model1'"=="sj2s" {
				disp as text "with the " as res `"Sidik-Jonkman two-step tau{c 178} estimator"'
			}
			else if "`model1'"=="dk2s" {
				disp as text "with the " as res `"DerSimonian-Kacker two-step tau{c 178} estimator"'
			}

			// Estimators of tausq
			if !inlist("`model1'", "mh", "peto", "fe", "mu") {
				if inlist("`model1'", "dl", "gamma", "ivhet", "qe", "hc") local tsqtext "DerSimonian-Laird"
				else if "`model1'"=="dlb"  local tsqtext "Bootstrap DerSimonian-Laird"
				else if "`model1'"=="mp"   local tsqtext "Mandel-Paule"
				else if "`model1'"=="vc"   local tsqtext `"Cochran's ANOVA-type (Hedges')"'
				else if "`model1'"=="ev"   local tsqtext "Empirical variance"
				else if "`model1'"=="hm"   local tsqtext "Hartung-Makambi"
				else if inlist("`model1'", "ml",   "pl") local tsqtext "ML"
				else if inlist("`model1'", "reml", "kr") local tsqtext "REML"
				else if "`model1'"=="bp"   local tsqtext "Rukhin's BP"
				else if "`model1'"=="b0"   local tsqtext "Rukhin's B0"
			
				local linktext = cond(`"`hksj'`robust'"'!=`""' | inlist("`model1'", "pl", "gamma", "ivhet", "qe", "kr", "hc"), "based on", "with")
				
				// Sensitivity analysis
				if "`model1'"=="sa" {
					disp as text "Sensitivity analysis with user-defined " _c
					if `tsqsa'==-99 {
						disp "I{c 178} = " as res "`isqsa'%"
						local fpnote `"Sensitivity analysis with user-defined I{c 178}"'
					}
					else {
						disp "tau{c 178} = " as res "`tsqsa'"
						local fpnote `"Sensitivity analysis with user-defined tau{c 178}"'
					}
				}
				
				// Two-step estimators
				else if inlist("`model1'", "sj2s", "dk2s") {
					disp as text `"with "' as res upper("`init'") as text `" initial estimate of tau{c 178}"'
				}
				
				// Default
				else disp as text `"`linktext' "' as res `"`tsqtext'"' as text `" estimate of tau{c 178}"'
			}
		}
	}		// end if `"`ovstats'`bystats'"'!=`""' 
	
	// User-defined weights
	local udw = 0
	local j = 1
	tokenize `wgtoptlist'
	while `"`1'"'!=`""' {
		if `"`1'"'!="default" {
			local udw = 1
			local 0 `", `1'"'
			syntax [, WGT(varname numeric) ]
	
			if `m'==1 {
				local wgttitle : variable label `wgt'
				if `"`wgttitle'"'==`""' local wgttitle `wgt'
				if `"`pool'"'==`""' {
					disp as text "and with user-defined weights " as res `"`wgttitle'"'
				}
				else disp as text "Weights " as res `"`wgttitle'"' as text " are user-defined"
				
				if `"`fpnote'"'!=`""' local fpnote `"`fpnote' and with user-defined weights"'
				else local fpnote `"NOTE: Weights are user-defined"'
			}
		}
		macro shift
	}
	if `udw' & `m' > 1 {
		if `"`pool'"'==`""' {
			disp as text "and with user-defined weights for one or more models; see below"
		}
		else disp as text "Weights are user-defined for one or more models; see below"

		if `"`fpnote'"'!=`""' local fpnote `"`fpnote' and with user-defined weights for some models"'
		else local fpnote `"NOTE: Weights for some models are user-defined"'
	}
		
	// Continuity correction
	cap confirm numeric var `ccvar'
	if !_rc {
		local 0 `cc'
		syntax [anything(id="value supplied to {bf:cc()}")] [, OPPosite EMPirical]
		local ccval = `anything'
		
		if `"`opposite'"'!=`""' disp as text _n "Opposite-arm continuity correction" _c
		else if `"`empirical'"'!=`""' disp as text _n "Empirical continuity correction" _c
		else disp as text _n "Continuity correction of " as res %4.2f `ccval' _c
		disp as text " applied to studies with zero cells"
		if `"`summaryonly'`table'"'==`""' {
			disp as text "(marked with " as res "*" as text ")"

			if "`model1'"=="mh" {
				disp as text "Note: M-H pooled effects are estimated from uncorrected counts"
			}
		}
		
		if `"`fpnote'"'!=`""' local fpnote `"`fpnote'; continuity correction applied to studies with zero cells"'
		else local fpnote `"NOTE: Continuity correction applied to studies with zero cells"'
	}
	
	// cumulative/influence notes
	// (N.B. all notes (`fpnote') are passed to -forestplot- regardless of `nowarning'; this is then implemented within forestplot.ado)
	if `"`fpnote'"'!=`""' & !inlist("`model1'", "fe", "mh") & `"`altwt'"'!=`""' {
		if `"`cumulative'"'!=`""' {
			local fpnote `""`fpnote';" "changes in heterogeneity may mean that cumulative weights are not monotone increasing""'
		}
		else if `"`influence'"'!=`""' {
			local fpnote `""`fpnote'," "expressed relative to the total weight in the overall model""'
		}
	}
	
	sreturn clear
	sreturn local fpnote `"`fpnote'"'
	
end





*******************************************************

* Routine to draw output table (metan.ado version)
// Could be done using "tabdisp", but doing it myself means it can be tailored to the situation
// therefore looks better (I hope!)

program define DrawTableAD, rclass sortpreserve

	// N.B. This program is rclass to enable return of matrix r(coeffs); nothing else is returned

	// N.B. no max in varlist() since xoutvlist may contain extra vars e.g. tausq/sigmasq, which are not relevant here
	syntax varlist(numeric min=6) [if] [in], MODELLIST(namelist) ///
		MODELTEXT(string) HETOPTLIST(namelist) WGTOPTLIST(string) TESTSTATLIST(namelist) SORTBY(varlist) ///
		[USER1stats(numlist min=3 max=3) USER2stats(numlist min=3 max=3) FIRSTSTATS(string asis) SECONDSTATS(string asis) ///
		/*legacy -metan- options; firststats/secondstats are het. text */ ///
		CUmulative INFluence noOVerall noSUbgroup noSECsub SUMMARYONLY OVWt SGWt ///
		LABELS(varname string) STITLE(string asis) ETITLE(string asis) CC(string) CCVAR(name) ///
		STUDY(varname numeric) BY(varname numeric) BYSTATSLIST(namelist) MWT(name) OVSTATS(name) ///
		EFORM noTABle noHET noKEEPVars KEEPOrder LEVEL(real 95) TSQLEVEL(real 95) ///
		 * ]			// extra options are from `model1opts' and `opts_adm'
		
	marksample touse, novarlist		// -novarlist- option prevents -marksample- from setting `touse' to zero if any missing values in `varlist'

	// unpack varlist
	tokenize `varlist'
	args _USE _ES _seES _LCI _UCI _WT _NN
	
	// Maintain original order if requested
	if `"`keeporder'"'!=`""' {
		tempvar tempuse
		qui gen byte `tempuse' = `_USE'
		qui replace `tempuse' = 1 if `_USE'==2		// keep "insufficient data" studies in original study order (default is to move to end)
	}
	else local tempuse `_USE'
	
	
	** Now, if `nokeepvars' specified (including if called by -ipdmetan-)
	//  re-create `obs', sorting by `sortby', and create matrix of coefficients
	// (N.B. not done earlier as want to take account of `keepall' & `keeporder')
	tempvar obs	
	if `"`keepvars'"'!=`""' {
		qui count if `touse'
		if r(N) > c(matsize) {
			disp as err `"matsize too small to store matrix of study coefficients; this step will be skipped"'
			disp as err `"  (see {bf:help matsize})"'
			sort `touse' `by' `tempuse' `sortby'			
		}
		
		else {
			// create `study' if missing
			if `"`study'"'==`""' {
				tempvar study
				qui gen long `obs' = _n
				qui bysort `touse' (`obs'): gen long `study' = _n if `touse'
				drop `obs'
			}
			sort `touse' `by' `tempuse' `sortby'
			
			tempname coeffs
			mkmat `by' `study' `_ES' `_seES' `_NN' `_WT' if `touse', matrix(`coeffs')

			local _BYexist = cond( `"`by'"'!=`""', "_BY", "")
			local _NNexist = cond(`"`_NN'"'!=`""', "_NN", "")
			local _WTexist = cond(`"`_WT'"'!=`""', "_WT", "")
			matrix colnames `coeffs' = `_BYexist' _STUDY _ES _seES `_NNexist' `_WTexist'
			return matrix coeffs = `coeffs'
		}
	}
	else sort `touse' `by' `tempuse' `sortby'			// to avoid sorting twice
	qui gen long `obs' = _n

	
	** Create table of results
	// Multiple models
	local m : word count `modellist'
	
	// Subgroups: some work needed beforehand in case of -noTABle-
	local swidth = 0
	tempvar vlablen
	if `"`by'"'!=`""' {
		qui levelsof `by' if `touse', missing local(bylist)		// "missing" since `touse' should already be appropriate for missing yes/no
		local bylab : value label `by'
		
		tempvar bylabels
		cap decode `by', gen(`bylabels')
		if _rc local bylabels `"string(`by')"'
		qui gen long `vlablen' = length(`bylabels')
		summ `vlablen' if `touse', meanonly
		local swidth = r(max)
		cap drop `bylabels'
		qui drop `vlablen'
	}
	local b = max(1, `: word count `bylist'')
	tempname _ES_ _seES_			// will need these two regardless of `table'

	if `"`table'"'==`""' {

		* Find maximum length of labels in LHS column
		qui gen long `vlablen' = length(`labels')
		
		if "`ccopt'"!="" {										// cc used with "primary" model
			qui replace `vlablen' = `vlablen' + 2 if `ccvar'	// for a space and asterisk if cc
		}
		
		* Find maximum length of study title and effect title
		//  Allow them to spread over several lines, but only up to a maximum number of chars
		//  If a single line must be more than 32 chars, truncate and stop
		local uselen = cond("`tarone'"=="", 20, 24)
		
		if `swidth'>`uselen' local uselen = min(`swidth', 31)
		SpreadTitle `"`stitle'"', target(`uselen') maxwidth(31)		// study (+ subgroup) title
		local swidth = 1 + max(`uselen', `r(maxwidth)')
		local slines = r(nlines)
		forvalues i = 1 / `slines' {
			local stitle`i' `"`r(title`i')'"'
		}
		SpreadTitle `"`etitle'"', target(10) maxwidth(15)		// effect title (i.e. "Odds ratio" etc.)
		local ewidth = 1 + max(10, `r(maxwidth)')
		local elines = r(nlines)
		local diff = `elines' - `slines'
		if `diff'<=0 {
			forvalues i = 1 / `slines' {
				local etitle`i' `"`r(title`=`i'+`diff'')'"'		// stitle uses most lines (or equal): line up etitle with stitle
			}
		}
		else {
			forvalues i = `elines' (-1) 1 {				// run backwards, otherwise macros are deleted by the time they're needed
				local etitle`i' `"`r(title`i')'"'
				local stitle`i' = cond(`i'>=`diff', `"`stitle`=`i'-`diff'''"', `""')	// etitle uses most lines: line up stitle with etitle
			}
		}
		
		* Now display the title lines, starting with the "extra" lines and ending with the row including CI & weight
		local wwidth = 11
		
		di as text _n `"{hline `swidth'}{c TT}{hline `=`ewidth'+24+`wwidth''}"'
		local nl = max(`elines', `slines')
		if `nl' > 1 {
			forvalues i = 1 / `=`nl'-1' {
				di as text `"`stitle`i''{col `=`swidth'+1'}{c |} "' %~`ewidth's `"`etitle`i''"'
			}
		}
		di as text `"`stitle`nl''{col `=`swidth'+1'}{c |} "' ///
			%~10s `"`etitle`nl''"' `"{col `=`swidth'+`ewidth'+4'}[`level'% Conf. Interval]{col `=`swidth'+`ewidth'+27'}% Weight"'


		** Loop over studies, and subgroups if appropriate
		tempvar touse2
		gen byte `touse2' = `touse'
		
		tempname _LCI_ _UCI_ _WT_ critval
		local xexp = cond("`eform'"!="", "exp", "")

		forvalues i = 1 / `b' {				// this will be 1/1 if no subgroups

			di as text `"{hline `swidth'}{c +}{hline `=`ewidth'+24+`wwidth''}"'

			if `"`by'"'!=`""' {
				local byi : word `i' of `bylist'
				qui replace `touse2' = `touse' * (`by'==`byi')
				
				if `"`bylab'"'!=`""' {
					local bylabi : label `bylab' `byi'
				}
				else local bylabi `"`byi'"'
				
				if `"`bystats'"'!=`""' {
					if missing(`bystats'[rownumb(`bystats', "Qdf"), `i']) {
						local nodata `"{col `=`swidth'+4'} (No subgroup data)"'
					}
				}
				di as text substr(`"`bylabi'"', 1, `swidth'-1) + `"{col `=`swidth'+1'}{c |}`nodata'"'
				local nodata	// clear macro
			}

			summ `obs' if `touse2' & inlist(`_USE', 1, 2), meanonly
			if r(N) & `"`summaryonly'"'==`""' {
				forvalues k = `r(min)' / `r(max)' {
					if missing(`_ES'[`k']) {
						di as text substr(`labels'[`k'], 1, 32) `"{col `=`swidth'+1'}{c |}{col `=`swidth'+4'} (Insufficient data)"'
					}
					else {
						scalar `_ES_'  = `_ES'[`k']
						scalar `_LCI_' = `_LCI'[`k']
						scalar `_UCI_' = `_UCI'[`k']
						scalar `_WT_'  = `_WT'[`k']
						
						local _labels_ = `labels'[`k']
						local _cc_
						
						local lwidth = 32
						cap confirm numeric var `ccvar'
						if !_rc {
							if `ccvar'[`k'] local _cc_ `" *"'
							local lwidth = 30
						}
						di as text substr(`"`_labels_'"', 1, `lwidth') as res `"`_cc_'"' ///
							as text `"{col `=`swidth'+1'}{c |}{col `=`swidth'+`ewidth'-6'}"' ///
							as res %7.3f `xexp'(`_ES_') `"{col `=`swidth'+`ewidth'+5'}"' ///
							as res %7.3f `xexp'(`_LCI_') `"{col `=`swidth'+`ewidth'+15'}"' ///
							as res %7.3f `xexp'(`_UCI_') `"{col `=`swidth'+`ewidth'+26'}"' ///
							as res %7.2f `_WT_'
					}
				}
			}

			* Subgroup effects
			if `"`by'"'!=`""' & `"`subgroup'"'==`""' & `"`cumulative'"'==`""' {
				di as text `"{col `=`swidth'+1'}{c |}"'

				// Multiple models
				forvalues j = 1 / `m' {
								
					// User-defined second model, or nosecsub
					local model : word `j' of `modellist'
					if (`j' > 1 & "`secsub'"!="") | "`model'"=="user2" {
						continue, break
					}
					
					if `m'==1 local modText " effect"
					else {
						local 0 `", `modeltext'"'
						syntax [, MODEL`j'text(string) * ]
						local modText `", `model`j'text'"'
					}
					
					local wgtstar
					local wgtopt : word `j' of `wgtoptlist'
					if `"`wgtopt'"'!="default" local wgtstar " **"					
				
					local bystats : word `j' of `bystatslist'
					scalar `_ES_' = `bystats'[rownumb(`bystats', "eff"), `i']
					if missing(`_ES_') {
						di as text `"Subgroup`modText'`wgtstar'{col `=`swidth'+1'}{c |}{col `=`swidth'+4'} (Insufficient data)"'
					}
					else {
						scalar `_LCI_' = `bystats'[rownumb(`bystats', "eff_lci"), `i']
						scalar `_UCI_' = `bystats'[rownumb(`bystats', "eff_uci"), `i']

						di as text `"Subgroup`modText'`wgtstar'{col `=`swidth'+1'}{c |}{col `=`swidth'+`ewidth'-6'}"' ///
							as res %7.3f `xexp'(`_ES_') `"{col `=`swidth'+`ewidth'+5'}"' ///
							as res %7.3f `xexp'(`_LCI_') `"{col `=`swidth'+`ewidth'+15'}"' ///
							as res %7.3f `xexp'(`_UCI_') `"{col `=`swidth'+`ewidth'+26'}"' _c
							
						// subgroup sum of (normalised) weights: will be 1 unless `ovwt'
						if `j' > 1 di ""		// cancel the _c
						else {
							scalar `_WT_' = cond(`"`ovwt'"'==`""', 100, `mwt'[`j', `i'])
							di as res %7.2f `_WT_'
						}
					}
				}		// end forvalues j = 1 / `m'
			}		// end if `by'
		}		// end forvalues i = 1 / `b'
		
		drop `touse2'	// tidy up
			

		* Overall effect		
		if `"`overall'"'==`""' & `"`cumulative'"'==`""' {
			if !(`"`summaryonly'"'!=`""' & `b'==1) {
				di as text `"{hline `swidth'}{c +}{hline `=`ewidth'+24+`wwidth''}"'
			}
			
			// Multiple models
			forvalues j = 1 / `m' {
			
				local model : word `j' of `modellist'

				if "`model'"=="user`j'" {
					local 0 `", `modeltext'"'
					syntax [, MODEL`j'text(string) * ]
					local modText `", `model`j'text'"'
					
					tokenize `user`j'stats'
					args _ES_ _LCI_ _UCI_

					di as text %-20s `"Overall`modText'{col `=`swidth'+1'}{c |}{col `=`swidth'+`ewidth'-6'}"' ///
						as res %7.3f `_ES_' `"{col `=`swidth'+`ewidth'+5'}"' ///
						as res %7.3f `_LCI_' `"{col `=`swidth'+`ewidth'+15'}"' ///
						as res %7.3f `_UCI_' _c
					
					if `j'==1 di as res `"{col `=`swidth'+`ewidth'+26'}"' %7.2f 100
					else di ""		// cancel the _c				
				}
				
				else {
					if `m' > 1 {
						local 0 `", `modeltext'"'
						syntax [, MODEL`j'text(string) * ]
						local modText `", `model`j'text'"'
					}
					else local modText " effect"
					
					local wgtstar
					local wgtopt : word `j' of `wgtoptlist'
					if `"`wgtopt'"'!="default" local wgtstar " **"

					scalar `_ES_' = `ovstats'[rownumb(`ovstats', "eff"), `j']
					if missing(`_ES_') {
						di as text `"Overall`modText'`wgtstar'{col `=`swidth'+1'}{c |}{col `=`swidth'+4'} (Insufficient data)"'
					}
					else {
						scalar `_LCI_' = `ovstats'[rownumb(`ovstats', "eff_lci"), `j']
						scalar `_UCI_' = `ovstats'[rownumb(`ovstats', "eff_uci"), `j']
						
						// N.B. sum of (normalised) weights: will be 1 unless `sgwt'
						di as text %-20s `"Overall`modText'`wgtstar'{col `=`swidth'+1'}{c |}{col `=`swidth'+`ewidth'-6'}"' ///
							as res %7.3f `xexp'(`_ES_') `"{col `=`swidth'+`ewidth'+5'}"' ///
							as res %7.3f `xexp'(`_LCI_') `"{col `=`swidth'+`ewidth'+15'}"' ///
							as res %7.3f `xexp'(`_UCI_') _c
							
						if `j'==1 & `"`sgwt'"'==`""' di as res `"{col `=`swidth'+`ewidth'+26'}"' %7.2f 100
						else di ""		// cancel the _c
					}
				}
			}
		}
		di as text `"{hline `swidth'}{c BT}{hline `=`ewidth'+24+`wwidth''}"'
	
	}	// end if `"`table'"'==`""'

	
	** Test statistics and p-values
	if `swidth'==0 local swidth = 21			// define `swidth' in case -noTABle- *and* no `by'

	local xtext = cond(`"`cumulative'"'!=`""', `"cumulative "', `""')		// n/a for influence
	local null = (`"`eform'"'!=`""')										// test of pooled effect equal to zero

	local hasdf t chi2 lr_chi2
	local hasdf : list hasdf & teststatlist
	local pos2 = cond(`"`hasdf'"'!=`""', 24, 14)
	
	tempname testStat df pvalue
	
	* Display by subgroup
	if `"`by'"'!=`""' & `"`subgroup'"'==`""' {
		di as text _n `"Tests of subgroup `xtext'effect size = "' as res `null' as text ":"

		forvalues i = 1 / `b' {
			local byi: word `i' of `bylist'
			if `"`bylab'"'!=`""' {
				local bylabi : label `bylab' `byi'
			}
			else local bylabi `byi'

			// Multiple models
			forvalues j = 1 / `m' {
			
				// User-defined second model, or nosecsub
				local model : word `j' of `modellist'
				if (`j' > 1 & "`secsub'"!="") | "`model'"=="user2" {
					continue, break
				}			
				local teststat : word `j' of `teststatlist'
				local bystats  : word `j' of `bystatslist'

				local wgtstar
				local wgtopt : word `j' of `wgtoptlist'
				if `"`wgtopt'"'!="default" local wgtstar " **"
		
				scalar `testStat' = `bystats'[rownumb(`bystats', "`teststat'"), `i']
				scalar `df' = .
				if "`model'"=="kr" {
					scalar `df' = `bystats'[rownumb(`bystats', "df_kr"), `i']
				}
				else if "`teststat'"=="t" {
					scalar `df' = `bystats'[rownumb(`bystats', "Qdf"), `i']
				}
				scalar `pvalue' = `bystats'[rownumb(`bystats', "pvalue"), `i']
			
				// Text to display: chisq distributions
				local testDist
				if "`teststat'"=="chi2" {
					if "`model'"=="pl" local testDist "LR chi{c 178}"
					else if "`model'"=="mh" & "`cmhnocc'"=="" local testDist "CMH chi{c 178}"
					else local testDist "chi{c 178}"

					local testStatFormat "%6.2f"
					local dfFormat "%1.0f"
					scalar `df' = 1
				}

				// Text to display: t distribution
				else if "`teststat'"=="t" {
					local testStatFormat "%7.3f"
					local dfFormat = cond("`model'"=="kr", "%6.2f", "%3.0f")
				}

				// Other text formatting
				if "`testDist'"=="" local testDist `teststat'
				if "`testStatFormat'"=="" local testStatFormat "%7.3f"
				
				local model`j'text
				if `m'==1 {
					local continue _c
					local pos = 1
				}
				else {
					local 0 `", `modeltext'"'
					syntax [, MODEL`j'text(string) * ]
					local pos = 20 - length(`"`= subinstr("`testDist'", "{c 178}", "c", .)'"') + 1
				}
				if `j'==1 {
					di as text substr("`bylabi'", 1, `swidth'-1) `"{col `=`swidth'+1'}"' `continue'
				}
				di as text `"`model`j'text'`wgtstar' "' _c
				if missing(`testStat') di as text `"{col `pos'}(Insufficient data)"'
				else {
					di as res `"{col `pos'}`testDist'"' as text " = " as res `testStatFormat' `testStat' _c
					if !missing(`df') {
						di as text "{col 32}on " as res `dfFormat' `df' as text " df," _c
					}
					di as text "{col `=20 + `pos2''}p = " as res %5.3f `pvalue'
				}
			}		// end forvalues j = 1 / `m'
		}		// end forvalues i = 1 / `b'
	}		// 	end if `"`by'"'!=`""' & `"`subgroup'"'==`""' {

	
	* Display overall
	if `"`overall'"'==`""' {
	
		// Multiple models
		forvalues j = 1 / `m' {
			local model : word `j' of `modellist'
			if inlist("`model'", "user1", "user2") continue
			local teststat : word `j' of `teststatlist'
			
			// Extract test statistics from `ovstats'
			scalar `testStat' = `ovstats'[rownumb(`ovstats', "`teststat'"), `j']
			scalar `df' = .
			if "`model'"=="kr" {
				scalar `df' = `ovstats'[rownumb(`ovstats', "df_kr"), `j']
			}
			else if "`teststat'"=="t" {
				scalar `df' = `ovstats'[rownumb(`ovstats', "Qdf"), `j']
			}
			scalar `pvalue' = `ovstats'[rownumb(`ovstats', "pvalue"), `j']

			// Text to display: chisq distributions
			local testDist
			if "`teststat'"=="chi2" {
				if "`model'"=="pl" local testDist "LR chi{c 178}"
				else if "`model'"=="mh" & "`cmhnocc'"=="" local testDist "CMH chi{c 178}"
				else local testDist "chi{c 178}"

				local testStatFormat "%6.2f"
				local dfFormat "%1.0f"
				scalar `df' = 1
			}
			
			// Text to display: t distribution
			else if "`teststat'"=="t" {
				local testStatFormat "%7.3f"
				local dfFormat = cond("`modeli'"=="kr", "%6.2f", "%3.0f")
			}

			// Text to display: Signed log-likelihood statistic
			else if "`teststat'"=="z" & "`model'"=="pl" {
				local testDist "LL z"
				local testStatFormat "%7.3f"
			}
			
			// Other text formatting
			if "`testDist'"=="" local testDist `teststat'
			if "`testStatFormat'"=="" local testStatFormat "%7.3f"
	
			if `m'==1 {		// if only one model (default)
				if missing(`testStat') {
					di as text `"Overall{col `=`swidth'+1'}(Insufficient data)"'
				}
				else {
					if `"`by'"'!=`""' & `"`subgroup'"'==`""' {
						di as text `"Overall{col `=`swidth'+1'}"' _c
					}
					else {
						di as text _n `"Test of overall `xtext'effect = "' as res `null' as text ":  " as res "`testDist'" as text " = " ///
							as res `testStatFormat' `testStat' _c
						if !missing(`df') {
							di as text " on " as res `dfFormat' `df' as text " df," _c
						}
						di as text "  p = " as res %5.3f `pvalue'			
					}
				}
			}
		
			else {
				if `j'==1 {		// if multiple models; only display text once
					di as text _n `"Tests of overall `xtext'effect = "' as res `null' as text ":"
				}
				local 0 `", `modeltext'"'
				syntax [, MODEL`j'text(string) * ]
				
				local wgtstar
				local wgtopt : word `j' of `wgtoptlist'
				if `"`wgtopt'"'!="default" local wgtstar " **"
				
				local pos = 20 - length(`"`= subinstr("`testDist'", "{c 178}", "2", .)'"') + 1
				di as text "`model`j'text'`wgtstar'" as res "{col `pos'}`testDist'" as text " = " as res `testStatFormat' `testStat' _c
				if !missing(`df') {
					di as text "{col 32}on " as res `dfFormat' `df' as text " df," _c
				}
				di as text "{col `=20 + `pos2''}p = " as res %5.3f `pvalue'
			}
		}
	}
		
	* User-defined weights
	local udw = 0
	forvalues j = 1 / `m' {
		local 0 `", `modeltext'"'
		syntax [, MODEL`j'text(string) * ]	
		local wgtopt : word `j' of `wgtoptlist'
		if `"`wgtopt'"'!="default" {
			local udw = 1
			local 0 `", `wgtopt'"'
			syntax [, WGT(varname numeric) ]
			local wgttitle : variable label `wgt'
			if `"`wgttitle'"'==`""' local wgttitle `wgt'
			
			if `m'==1 {
				disp as text _n "** Note: pooled using user-defined weights " as res "`wgttitle'"
			}
			else {
				local dnl = cond(`j'==1, "_n", "")
				disp as text `dnl' "** Note: `model`j'text' pooled using user-defined weights " as res "`wgttitle'"
			}
		}
	}

	
	** Heterogeneity statistics
	
	// Present table of Qs if:
	//   - >1 subgroup
	//   - >1 Q-types
	//   - >0 tsqci
	
	// Present table of Isqs (+/- CI) if:
	//   - >1 tausq-type
	
	// Present table of Isqs, Hsqs, tausqs+CI if:
	//   - tsqci == 1

	// Present single table of Q, I2, H2M, tausq if:
	//   - no by() [i.e. single subgroup]
	//   - no tsqci
	//   - single Q-type / tausq-type
	
	// So can have:
	//   - table of Qs +/- table of Isqs (+/-CI) only
	//   - OR table of Qs +/- table of single Isq, Hsq, tausq+CI
	//   - OR single table of Q, Isq, Hsq (+ tausq if applicable)
	
	summ `_ES' if `touse' & `_USE'==1, meanonly
	local het = cond(`r(N)'==1, "nohet", "`het'")		// don't present overall het stats if only one estimate

	gettoken model1 : modellist
	if "`het'"=="" & "`model1'"!="user1" {
		
		
		**************************************
		* Determine combinatons of het stats *
		**************************************

		** How many unique methods for estimating Q or "pseudo-Q"?
		// In this context, "confidence interval" methods are considered the same as "cochranq"
		local refhetlist higgins ncchi2 qgamma qprofile ml reml gamma
		local hetoptlist2 : copy local hetoptlist
		local hetoptlist2 = subinword("`hetoptlist2'", "user", "", .)		// ignore user-defined models
		foreach h of local refhetlist {
			local hetoptlist2 = subinword("`hetoptlist2'", "`h'", "cochranq", .)
		}

		local uniqhet : list uniq hetoptlist2
		local totuniqh : word count `uniqhet'
		

		** How many unique methods for estimating tausq (point estimate and/or CI)?
		// equals the number of unique combinations of `model' and `hetopt'
		// excluding MH, Peto and multiplicative heterogeneity, where tausq is not estimated
		// also, e.g. IVHet and HC use DL estimate of tausq ==> hetopt is the same
		// (but e.g. Gamma also uses DL estimate BUT with a different CI)
		local uniqmodhet
		local uniqhetci
		local totuniqmodh = 0
		local tothetci= 0
		forvalues j = 1 / `m' {
			local h   : word `j' of `hetoptlist'
			local mod : word `j' of `modellist'
			if inlist("`mod'", "ivhet", "hc", "fe") local mod dl
			if !inlist("`mod'", "mh", "peto", "mu") {
				local modhet `mod'`h'
				if !`: list modhet in uniqmodhet' {
					local uniqmodhet `uniqmodhet' `modhet'
					local uniqmodh `uniqmodh' `j'
					local ++totuniqmodh
				}
			}
			
			// ...and how many unique heterogeneity *confidence interval* methods?
			if `: list h in refhetlist' & !`: list el in uniqhetci' {
				local uniqhetci `uniqhetci' `h'
				local uniqci `uniqci' `j'
				local ++tothetci
			}
		}
		assert `tothetci' <= `totuniqmodh'
		

		***************************************
		* Standard heterogeneity measures box *
		***************************************
		// (single Q-type, single tausq estimator with no CI, no study subgroups)

		if `totuniqmodh'<=1 & `totuniqh'==1 & !`tothetci' ///
			& !(`b' > 1 & `"`by'"'!=`""' & `"`subgroup'"'==`""') {
		
			if `"`overall'"'==`""' {
				local c : list posof "`uniqhet'" in hetoptlist2
			
				tempname Q_ov Qdf_ov Qpval_ov
				scalar `Q_ov' =   `ovstats'[rownumb(`ovstats', "Q"), `c']
				scalar `Qdf_ov' = `ovstats'[rownumb(`ovstats', "Qdf"), `c']
				scalar `Qpval_ov' = chi2tail(`Qdf_ov', `Q_ov')

				tempname H Isq HsqM
				scalar `H' =  `ovstats'[rownumb(`ovstats', "H"), `c']
				scalar `Isq' =  `ovstats'[rownumb(`ovstats', "Isq"), `c']
				scalar `HsqM' = `ovstats'[rownumb(`ovstats', "HsqM"), `c']
				
				if "`uniqhet'"=="cochranq" {
					tempname tausq sigmasq
					scalar `tausq'   = `ovstats'[rownumb(`ovstats', "tausq"), `c']
					scalar `sigmasq' = `ovstats'[rownumb(`ovstats', "sigmasq"), `c']
				}
									
				// Sensitivity analysis: only one column
				if "`model'"=="sa" {
					local hetextra `" (user-defined"'
					if `udw' local hetextra `"`hetextra'; based on standard inverse-variance weights"'
					local hetextra `"`hetextra')"'
					di as text _n(2) `"Heterogeneity measures`hetextra'"'
				
					di as text "{hline `swidth'}{c TT}{hline 13}"
					di as text `"{col `=`swidth'+1'}{c |}{col `=`swidth'+7'}Value"'
					di as text "{hline `swidth'}{c +}{hline 13}"
					
					di as text `"H {col `=`swidth'+1'}{c |}{col `=`swidth'+5'}"' as res %7.3f `H'
					di as text `"I{c 178} (%) {col `=`swidth'+1'}{c |}{col `=`swidth'+4'}"' as res %7.1f `Isq' "%"
					di as text `"Modified H{c 178} {col `=`swidth'+1'}{c |}{col `=`swidth'+5'}"' as res %7.3f `HsqM'
					di as text `"tau{c 178} {col `=`swidth'+1'}{c |}{col `=`swidth'+4'}"' as res %8.4f `tausq'
				}
					
				else {
					if "`model1'"=="mu" local hetextra `" (based on Cochran's Q)"'
					else if `udw' local hetextra `" (based on standard inverse-variance weights)"'
					else if "`uniqhet'"=="cochranq" local hetextra `" (based on Cochran's Q)"'
					else if "`uniqhet'"=="mhq"     local hetextra `" (based on Mantel-Haenszel Q)"'
					else if "`uniqhet'"=="breslow" local hetextra `" (based on Breslow-Day statistic)"'
					else if "`uniqhet'"=="tarone"  local hetextra `" (based on Breslow-Day-Tarone statistic)"'
					else if "`uniqhet'"=="petoq"   local hetextra `" (based on Peto Q)"'
					else local hetextra `" (based on tau{c 178})"'
					di as text _n(2) `"Heterogeneity measures`hetextra'"'
				
					if      "`uniqhet'"=="petoq"   local hetText "Peto Q"
					else if "`uniqhet'"=="mhq"     local hetText "Mantel-Haenszel Q"
					else if "`uniqhet'"=="breslow" local hetText "Breslow-Day test"
					else if "`uniqhet'"=="tarone"  local hetText "Breslow-Day-Tarone"
					else local hetText "Cochran's Q"
				
					di as text `"{hline `swidth'}{c TT}{hline 35}"'
					di as text `"{col `=`swidth'+1'}{c |}{col `=`swidth'+7'}Value{col `=`swidth'+18'}df{col `=`swidth'+25'}p-value"'
					di as text `"{hline `swidth'}{c +}{hline 35}"'
					
					di as text `"`hetText' {col `=`swidth'+1'}{c |}{col `=`swidth'+5'}"' ///
						as res %7.2f `Q_ov' `"{col `=`swidth'+16'}"' %3.0f `Qdf_ov' `"{col `=`swidth'+23'}"' %7.3f `Qpval_ov'			
					
					// di as text "I{c 178} (%) {col `=`swidth'+1'}{c |}{col `=`swidth'+4'}" as res %7.1f 100*`isq' "%"		// altered Sep 2017 for v2.1 to match with metan/metaan behaviour
					di as text `"H {col `=`swidth'+1'}{c |}{col `=`swidth'+5'}"' as res %7.3f `H'
					di as text `"I{c 178} (%) {col `=`swidth'+1'}{c |}{col `=`swidth'+4'}"' as res %7.1f `Isq' "%"
					di as text `"Modified H{c 178} {col `=`swidth'+1'}{c |}{col `=`swidth'+5'}"' as res %7.3f `HsqM'
					
					if !missing(rownumb(`ovstats', "tausq")) {
						di as text `"tau{c 178} {col `=`swidth'+1'}{c |}{col `=`swidth'+4'}"' as res %8.4f `tausq'
					}

					local swidth2 = cond("`model'"=="sa", 13, 35)
					di as text `"{hline `swidth'}{c BT}{hline `swidth2'}"'
					
					// Display explanations
					if !missing(rownumb(`ovstats', "tausq")) {
						di as text _n `"H = relative excess in total variance over "typical" within-study variance"'
						di as text `"I{c 178} = between-study variance (tau{c 178}) as a percentage of total variance"'
						di as text `"Modified H{c 178} = ratio of tau{c 178} to "typical" within-study variance"'
					}
					else {
						if "`uniqhet'"=="breslow" local hetText "Breslow-Day statistic"
						else if "`uniqhet'"=="tarone" local hetText "Breslow-Day-Tarone statistic"				
					
						di as text _n `"H = relative excess in `hetText' over its degrees-of-freedom"'
						di as text `"I{c 178} = between-study variance (tau{c 178}) as a percentage of total variance (based on `hetText')"'
						di as text `"Modified H{c 178} = ratio of tau{c 178} to "typical" within-study variance (based on `hetText')"'
					}
				}
			}		// end if `"`overall'"'==`""'
		}		// end "if single Q-type, no CIs, no study subgroups)"

		
		*************************
		* Table of Q statistics *
		*************************
		//   - if >1 subgroup
		//   - if >1 Q-types
		//   - if >0 tsqci
		
		// If multiple heterogeneity stats just list Q stats and p-values;
		//   details, e.g. tausq, can be found in r(ovstats) or r(bystats)

		// FURTHER:  If multiple *subgroups*, just list "primary" Q and p-value.

		else {			
			tempname Q Qdf Qpval Isq Isqmax
			tempname Q_ov Qdf_ov Qpval_ov Qsum Qdiff Qdiffpval Fstat Fpval
			scalar `Qsum' = 0
			scalar `Isqmax' = 0
		
			local hetWidth = 35
			
			if `b' > 1 & `"`by'"'!=`""' & `"`subgroup'"'==`""' {
				local hetopt : word 1 of `hetoptlist2'
				local bystats : word 1 of `bystatslist'
				
				if      "`hetopt'"=="petoq"   di as text _n(2) "Peto Q statistics for heterogeneity"
				else if "`hetopt'"=="mhq"     di as text _n(2) "Mantel-Haenszel Q statistics for heterogeneity"
				else if "`hetopt'"=="breslow" di as text _n(2) "Breslow-Day homogeneity statistics"
				else if "`hetopt'"=="tarone"  di as text _n(2) "Breslow-Day-Tarone homogeneity statistics"
				else di as text _n(2) "Cochran's Q statistics for heterogeneity"
				
				di as text `"{hline `swidth'}{c TT}{hline `hetWidth'}"'
				di as text `"{col `=`swidth'+1'}{c |}{col `=`swidth'+7'}Value{col `=`swidth'+18'}df{col `=`swidth'+25'}p-value"'
				di as text `"{hline `swidth'}{c +}{hline `hetWidth'}"'				
				
				forvalues i = 1 / `b' {					
					local byi : word `i' of `bylist'
					if `"`bylab'"'!=`""' {
						local bylabi : label `bylab' `byi'
					}
					else local bylabi `"`byi'"'
					if `"`bylabi'"'!="." local bylabi = substr(`"`bylabi'"', 1, `swidth'-1)
													
					di as text `"`bylabi'{col `=`swidth'+1'}{c |}"' _c

					scalar `Isq' = `bystats'[rownumb(`bystats', "Isq"), `i']
					scalar `Isqmax' = max(`Isqmax', `Isq')

					scalar `Q'   = `bystats'[rownumb(`bystats', "Q"),   `i']
					scalar `Qdf' = `bystats'[rownumb(`bystats', "Qdf"), `i']
					if !missing(`Q') {
						scalar `Qpval' = chi2tail(`Qdf', `Q')
						local dfcol = cond(`"`overall'"'==`""', 18, 16)
						di as text `"{col `=`swidth'+5'}"' as res %7.2f `Q' `"{col `=`swidth'+`dfcol''}"' %3.0f `Qdf' `"{col `=`swidth'+23'}"' %7.3f `Qpval'
					}
					else di as text `"{col `=`swidth'+5'}(Insufficient data)"'
				
					scalar `Qsum' = `Qsum' + `Q'
				}
			}
			else {
				local s = cond(`totuniqh' > 1, "s", "")
				di as text _n(2) `"Heterogeneity measures: Q statistic`s'"'
				di as text `"{hline `swidth'}{c TT}{hline `hetWidth'}"'
				di as text `"Measure{col `=`swidth'+1'}{c |}{col `=`swidth'+7'}Value{col `=`swidth'+18'}df{col `=`swidth'+25'}p-value"'
				di as text `"{hline `swidth'}{c +}{hline `hetWidth'}"'				
			}
				
			if `"`overall'"'==`""' {
				local h = 0
				foreach hetopt of local uniqhet {
					local ++h
					
					if `b' > 1 & `"`by'"'!=`""' & `"`subgroup'"'==`""' {
						local hetText Overall
					}
					else {
						if      "`hetopt'"=="petoq"   local hetText "Peto Q"
						else if "`hetopt'"=="mhq"     local hetText "Mantel-Haenszel Q"
						else if "`hetopt'"=="breslow" local hetText "Breslow-Day test"
						else if "`hetopt'"=="tarone"  local hetText "Breslow-Day-Tarone test"
						else local hetText "Cochran's Q"
					}

					// Find model column corresponding to `hetopt'
					local c : list posof "`hetopt'" in hetoptlist2

					di as text `"`hetText'{col `=`swidth'+1'}{c |}"' _c
					scalar `Q_ov'   = `ovstats'[rownumb(`ovstats', "Q"),   `c']
					scalar `Qdf_ov' = `ovstats'[rownumb(`ovstats', "Qdf"), `c']
					if !missing(`Q_ov') {
						scalar `Qpval_ov' = chi2tail(`Qdf_ov', `Q_ov')
						local dfcol = cond(`"`overall'"'==`""', 18, 16)
						di as text `"{col `=`swidth'+5'}"' as res %7.2f `Q_ov' `"{col `=`swidth'+`dfcol''}"' %3.0f `Qdf_ov' `"{col `=`swidth'+23'}"' %7.3f `Qpval_ov'
					}
					else di as text `"{col `=`swidth'+5'}(Insufficient data)"'				
			
					if `b' > 1 & `"`by'"'!=`""' & `"`subgroup'"'==`""' {
					
						if inlist("`hetopt'", "breslow", "tarone") {
							di as text "Note: between-group heterogeneity calculation not valid with Mantel-Haenszel method"
						}
						
						else {
							scalar `Qdiff' = `Q_ov' - `Qsum'		// between-subgroup heterogeneity (Qsum = within-subgroup het.)
							scalar `Qdiffpval' = chi2tail(`b' - 1, `Qdiff')
							
							scalar `Fstat' = (`Qdiff'/(`b' - 1)) / (`Qsum'/(`Qdf_ov' - `b' + 1))		// corrected 17th March 2017
							scalar `Fpval' = Ftail(`b' - 1, `Qdf_ov' - `b' + 1, `Fstat')
						
							di as text `"Between{col `=`swidth'+1'}{c |}"' as text `"{col `=`swidth'+5'}"' as res %7.2f `Qdiff' `"{col `=`swidth'+18'}"' ///
								%3.0f `b' - 1 `"{col `=`swidth'+23'}"' %7.3f `Qdiffpval'
							di as text `"Between:Within (F){col `=`swidth'+1'}{c |}"' as text `"{col `=`swidth'+5'}"' as res %7.2f `Fstat' `"{col `=`swidth'+14'}"' ///
								%3.0f `b' - 1 as text "," as res %3.0f `Qdf_ov' - `b' + 1 `"{col `=`swidth'+23'}"' %7.3f `Fpval'
						
							di as text `"{hline `swidth'}{c BT}{hline 35}"'
							local endbox yes

							// DISPLAY BETWEEN-GROUP TEST WARNINGS [taken from -metan- v3.04]
							// (needs to be *outside* the box, hence `endbox')
							if `Isqmax' > 0 {
								if `Isqmax' < 50 {
									di as text "Note: Some heterogeneity observed (I{c 178} up to " as res %4.1f `=`Isqmax'' "%" as text ") in one or more subgroups;"
									di as text "  tests for heterogeneity between subgroups may not be valid"
								}
								else if `Isqmax' < 75 {
									di as text "Note: Moderate heterogeneity observed (I{c 178} up to " as res %4.1f `=`Isqmax'' "%" as text ") in one or more subgroups;"
									di as text "  tests for heterogeneity between subgroups are likely to be invalid"
								}
								else if !missing(`Isqmax') {
									di as text "Note: Considerable heterogeneity observed (I{c 178} up to " as res %4.1f `=`Isqmax'' "%" as text ") in one or more subgroups;"
									di as text "  tests for heterogeneity between subgroups are likely to be invalid"
								}
							}
						
							// exit after h==1
							continue, break
						}
					}
				}
				
				if "`endbox'"!="yes" di as text `"{hline `swidth'}{c BT}{hline 35}"'
			}

			

			*************
			* I-squared *  (+/- confidence interval)
			*************
			// If multiple het methods, just list I-squared
			// Else also list H, H2M, tausq
			// (N.B. don't list at all if also multiple subgroups!)
			
			if (`totuniqmodh' > 1  | `tothetci'==1) & !(`b' > 1 & `"`by'"'!=`""' & `"`subgroup'"'==`""') {
						
				forvalues i = 1 / `totuniqmodh' {
					local c : word `i' of `uniqmodh'
					local hetopt : word `c' of `hetoptlist'					
					local mod : word `c' of `modellist'			
					local 0 `", `modeltext'"'
					syntax [, MODEL`c'text(string) * ]
					
					local tsqciText
					if "`hetopt'"=="higgins" local tsqciText `"Higgins' H-based"'
					else if "`hetopt'"=="ncchi2" local tsqciText `"Non-cent. chi{c 178} for Q"'
					else if "`hetopt'"=="qgamma" local tsqciText `"Gamma-based for Q"'
					else if "`hetopt'"=="qprofile" local tsqciText `"`model`c'text', Q profile"'
					else if "`hetopt'"=="ml" local tsqciText "ML/PL tau{c 178} profile"
					else if "`hetopt'"=="reml" local tsqciText "REML tau{c 178} profile"
					else if "`hetopt'"=="gamma" local tsqciText "BT tau{c 178} profile"
					else {
						if inlist("`mod'", "ivhet", "hc", "fe", "dl") local tsqciText "DL"
						else local tsqciText : copy local model`c'text
						local tsqciText `"`tsqciText' tau{c 178}"'
					}
					// table of Isqs, Hsqs, tausqs+CI needed:
					//   - if tsqci == 1

					// Single CI; display all statistics (Isq, H, H2M, tausq)
					if `tothetci'==1 {
						if !`: list hetopt in uniqhetci' continue
					
						di as text _n `"Confidence Intervals generated using "' as res "`tsqciText'" as text " method"
						di as text `"{hline `swidth'}{c TT}{hline 35}"'
						di as text `"{col `=`swidth'+1'}{c |}{col `=`swidth'+7'}Value{col `=`swidth'+15'}[`tsqlevel'% Conf. Interval]"'
						di as text `"{hline `swidth'}{c +}{hline 35}"'

						foreach x in tausq tsq_lci tsq_uci H H_lci H_uci Isq Isq_lci Isq_uci HsqM HsqM_lci HsqM_uci {
							tempname `x'
							scalar ``x'' = `ovstats'[rownumb(`ovstats', "`x'"), `c']
						}
					
						di as text `"I{c 178} (%) {col `=`swidth'+1'}{c |}{col `=`swidth'+4'}"' ///
							as res %7.1f `Isq' `"%{col `=`swidth'+14'}"' ///
							as res %7.1f `Isq_lci' `"%{col `=`swidth'+24'}"' %7.1f `Isq_uci' "%"

						di as text `"H {col `=`swidth'+1'}{c |}{col `=`swidth'+5'}"' ///
							as res %7.3f `H' `"{col `=`swidth'+15'}"' ///
							as res %7.3f `H_lci' `"{col `=`swidth'+25'}"' %7.3f `H_uci'
						
						di as text `"Modified H{c 178} {col `=`swidth'+1'}{c |}{col `=`swidth'+5'}"' ///
							as res %7.3f `HsqM' `"{col `=`swidth'+15'}"' ///
							as res %7.3f `HsqM_lci' `"{col `=`swidth'+25'}"' %7.3f `HsqM_uci'
					
						if inlist("`hetopt'", "qprofile", "ml", "reml", "gamma") {
							di as text `"tau{c 178} {col `=`swidth'+1'}{c |}{col `=`swidth'+4'}"' ///
								as res %8.4f `tausq' `"{col `=`swidth'+14'}"' ///
								as res %8.4f `tsq_lci' `"{col `=`swidth'+24'}"' %8.4f `tsq_uci'
						}
						
						continue, break
					}
					
					// table of Isqs (+/- CI) needed:
					//   - if >1 combination of model + tsq
					
					// Multiple methods; just display I-squared (+/- CI)
					else {
					
						if `i'==1 {
							di as text _n "Heterogeneity measures: I-squared"
							di as text `"{hline `swidth'}{c TT}{hline 35}"'
							di as text `"Derivation{col `=`swidth'+1'}{c |}{col `=`swidth'+7'}Isq{col `=`swidth'+15'}[`tsqlevel'% Conf. Interval]"'
							di as text `"{hline `swidth'}{c +}{hline 35}"'				
						}
					
						tempname Isq Isq_lci Isq_uci
						scalar `Isq'     = `ovstats'[rownumb(`ovstats', "Isq"),     `c']
						scalar `Isq_lci' = `ovstats'[rownumb(`ovstats', "Isq_lci"), `c']
						scalar `Isq_uci' = `ovstats'[rownumb(`ovstats', "Isq_uci"), `c']

						di as text `"`tsqciText'{col `=`swidth'+1'}{c |}{col `=`swidth'+4'}"' ///
							as res %7.1f `Isq' `"%{col `=`swidth'+14'}"' _c	
						if !missing(`Isq_lci') {
							di as res %7.1f `Isq_lci' `"%{col `=`swidth'+24'}"' %7.1f `Isq_uci' "%"
						}
						else di ""	// cancel _c	
					}
				
				}	// end forvalues i = 1 / `totuniqmodh'

			di as text `"{hline `swidth'}{c BT}{hline 35}"'
			
			}	// end if (`totuniqmodh' > 1  | `tothetci'==1) ...
						
		}	// end else (i.e. if anything other than a single table of Q, Isq, tausq etc.)

	}	// end if `"`het'"'==`""'

end
		



**************************

* Subroutine to "spread" titles out over multiple lines if appropriate
// Updated July 2014
// Copied directly to updated version of admetan.ado September 2015 without modification
// August 2016: identical program now used here, in forestplot.ado, and in ipdover.ado 
// May 2017: updated to accept substrings delineated by quotes (c.f. multi-line axis titles)
// August 2017: updated for better handling of maxlines()
// March 2018: updated to receive text in quotes, hence both avoiding parsing problems with commas, and maintaining spacing
// May 2018 and Nov 2018: updated truncation procedure

// subroutine of DrawTableAD

program define SpreadTitle, rclass

	syntax [anything(name=title id="title string")] [, TArget(integer 0) MAXWidth(integer 0) MAXLines(integer 0) noTRUNCate noUPDATE ]
	* Target = aim for this width, but allow expansion if alternative is wrapping "too early" (i.e before line is adequately filled)
	//         (may be replaced by `titlelen'/`maxlines' if `maxlines' and `notruncate' are also specified)
	* Maxwidth = absolute maximum width ... but will be increased if a "long" string is encountered before the last line
	* Maxlines = maximum no. lines (default 3)
	* noTruncate = don't truncate final line if "too long" (even if greater than `maxwidth')
	* noUpdate = don't update `target' if `maxwidth' is increased (see above)
	
	tokenize `title'
	if `"`1'"'==`""' {
		return scalar nlines = 0
		return scalar maxwidth = 0
		exit
	}
	
	if `maxwidth' & !`maxlines' {
		cap assert `maxwidth'>=`target'
		if _rc {
			nois disp as err `"{bf:maxwidth()} must be greater than or equal to {bf:target()}"'
			exit 198
		}
	}


	** Find length of title string, or maximum length of multi-line title string
	// First run: strip off outer quotes if necessary, but watch out for initial/final spaces!
	gettoken tok : title, qed(qed)
	cap assert `"`tok'"'==`"`1'"'
	if _rc {
		gettoken tok rest : tok, qed(qed)
		assert `"`tok'"'==`"`1'"'
		local title1 title1				// specifies that title is not multi-line
	}
	local currentlen = length(`"`1'"')
	local titlelen   = length(`"`1'"')
	
	// Subsequent runs: successive calls to -gettoken-, monitoring quotes with the qed() option
	macro shift
	while `"`1'"'!=`""' {
		local oldqed = `qed'
		gettoken tok rest : rest, qed(qed)
		assert `"`tok'"'==`"`1'"'
		if !`oldqed' & !`qed' local currentlen = `currentlen' + 1 + length(`"`1'"')
		else {
			local titlelen = max(`titlelen', `currentlen')
			local currentlen = length(`"`1'"')
		}
		macro shift
	}
	local titlelen = max(`titlelen', `currentlen')
	
	// Save user-specified parameter values separately
	local target_orig = `target'
	local maxwidth_orig = `maxwidth'
	local maxlines_orig = `maxlines'
	
	// Now finalise `target' and calculate `spread'
	local maxlines = cond(`maxlines_orig', `maxlines_orig', 3)	// use a default value for `maxlines' of 3 in these calculations
	local target = cond(`target_orig', `target_orig', ///
		cond(`maxwidth_orig', min(`maxwidth_orig', `titlelen'/`maxlines'), `titlelen'/`maxlines'))
	local spread = min(int(`titlelen'/`target') + 1, `maxlines')
	local crit = cond(`maxwidth_orig', min(`maxwidth_orig', `titlelen'/`spread'), `titlelen'/`spread')


	** If substrings are present, delineated by quotes, treat this as a line-break
	// Hence, need to first process each substring separately and obtain parameters,
	// then select the most appropriate overall parameters given the user-specified options,
	// and finally create the final line-by-line output strings.
	tokenize `title'
	local line = 1
	local title`line' : copy local 1				// i.e. `"`title`line''"'==`"`1'"'
	local newwidth = length(`"`title`line''"')

	// if first "word" is by itself longer than `maxwidth' ...
	if `maxwidth' & !(`maxlines' & (`line'==`maxlines')) {
	
		// ... reset parameters and start over
		while length(`"`1'"') > `maxwidth' {
			local maxwidth = length(`"`1'"')
			local target = cond(`target_orig', cond(`"`update'"'!=`""', `target_orig', `target_orig' + `maxwidth' - `maxwidth_orig'), ///
				cond(`maxwidth', min(`maxwidth', `titlelen'/`maxlines'), `titlelen'/`maxlines'))
			local spread = min(int(`titlelen'/`target') + 1, `maxlines')
			local crit = cond(`maxwidth', min(`maxwidth', `titlelen'/`spread'), `titlelen'/`spread')
		}
	}
	
	macro shift
	local next : copy local 1		// i.e. `"`next'"'==`"`1'"' (was `"`2'"' before macro shift!)
	while `"`1'"' != `""' {
		// local check = `"`title`line''"' + `" "' + `"`next'"'			// (potential) next iteration of `title`line''
		local check `"`title`line'' `next'"'							// (amended Apr 2018 due to local x = "" issue with version <13)
		if length(`"`check'"') > `crit' {								// if longer than ideal...
																		// ...and further from target than before, or greater than maxwidth
			if abs(length(`"`check'"') - `crit') > abs(length(`"`title`line''"') - `crit') ///
					| (`maxwidth' & (length(`"`check'"') > `maxwidth')) {
				if `maxlines' & (`line'==`maxlines') {					// if reached max no. of lines
					local title`line' : copy local check				//   - use next iteration anyway (to be truncated)

					macro shift
					local next : copy local 1
					local newwidth = max(`newwidth', length(`"`title`line''"'))		// update `newwidth'					
					continue, break
				}
				else {										// otherwise:
					local ++line							//  - new line
					
					// if first "word" of new line (i.e. `next') is by itself longer than `maxwidth' ...
					if `maxwidth' & (length(`"`next'"') > `maxwidth') {
					
						// ... if we're on the last line or last token, continue as normal ...
						if !((`maxlines' & (`line'==`maxlines')) | `"`2'"'==`""') {
						
							// ... but otherwise, reset parameters and start over
							local maxwidth = length(`"`next'"')
							local target = cond(`target_orig', cond(`"`update'"'!=`""', `target_orig', `target_orig' + `maxwidth' - `maxwidth_orig'), ///
								cond(`maxwidth', min(`maxwidth', `titlelen'/`maxlines'), `titlelen'/`maxlines'))
							local spread = min(int(`titlelen'/`target') + 1, `maxlines')
							local crit = cond(`maxwidth', min(`maxwidth', `titlelen'/`spread'), `titlelen'/`spread')
							
							// restart loop
							tokenize `title'
							local tok = 1
							local line = 1
							local title`line' : copy local 1				// i.e. `"`title`line''"'==`"`1'"'
							local newwidth = length(`"`title`line''"')
							macro shift
							local next : copy local 1		// i.e. `"`next'"'==`"`1'"' (was `"`2'"' before macro shift!)
							continue
						}
					}
					
					local title`line' : copy local next		//  - begin new line with next word
				}
			}
			else local title`line' : copy local check		// else use next iteration
			
		}
		else local title`line' : copy local check			// else use next iteration

		macro shift
		local next : copy local 1
		local newwidth = max(`newwidth', length(`"`title`line''"'))		// update `newwidth'
	}																	// (N.B. won't be done if reached max no. of lines, as loop broken)


	* Return strings
	forvalues i = 1 / `line' {
	
		// truncate if appropriate (last line only)
		if `i'==`line' & "`truncate'"=="" & `maxwidth' {
			local title`i' = substr(`"`title`i''"', 1, `maxwidth')
		}
		return local title`i' `"`title`i''"'
	}
	
	* Return values
	return scalar nlines = `line'
	return scalar maxwidth = min(`newwidth', `maxwidth')
	return scalar target = `target'
	
end





************************************************

** BuildResultsSet

// Having performed the meta-analysis (see PerformMetaAnalysis subroutine)
// ... and displayed results on-screen (see DrawTableAD subroutine)
// ... optionally prepare "results set" for either saving, or for constructing the forest plot (using forestplot.ado).
// The saving and/or running of -forestplot- is done from within this subroutine, due to tempvars being created.
// Note that meta-analysis is now complete, with stats returned in r(); if error in BuildResultsSet, error message explains this.

// (called directly by metan.ado)

// [N.B. mostly end part of old (v2.2) MainRoutine subroutine]

program define BuildResultsSet, rclass

	syntax varlist(numeric min=3 max=7) [if] [in], LABELS(varname) OUTVLIST(varlist numeric min=5 max=8) ///
		MODELLIST(namelist) MODELTEXT(string) HETOPTLIST(namelist) TESTSTATLIST(namelist) SORTBY(varlist) ///
		[SUMMSTAT(string) STUDY(varname numeric) BY(varname numeric) BYSTATSLIST(namelist) MWT(name) OVSTATS(name) ///
		USER1stats(numlist min=3 max=3) USER2stats(numlist min=3 max=3) FIRSTSTATS(string asis) SECONDSTATS(string asis) /// legacy -metan- 
		CUmulative INFluence noOVerall noSUbgroup noSECsub SUMMARYONLY OVWt SGWt ALTWt WGT(varname numeric) ///
		EFORM EFFect(string asis) LOGRank CC(string) CCVAR(name) ///
		LCols(varlist) RCols(varlist) COUNTS(string asis) EFFIcacy OEV NPTS ///
		XOUTVLIST(varlist numeric) RFDist RFLEVEL(real 95) LEVEL(real 95) TSQLEVEL(real 95) ///
		EXTRALine(string) HETStat(string) OVStat(string) noHET noWT noSTATs ///
		KEEPAll KEEPOrder noGRaph noWARNing SAVING(string) CLEAR FORESTplot(string asis) FPNOTE(string asis) ///
		SFMTLEN(integer 0) USE3(varname numeric) PLOTID(passthru) noRSample ///
		BYAD SOURCE(varname numeric) LRVLIST(varlist numeric) IPDXLINE(string) ESTEXP(string) EXPLIST(passthru) IPDMETAN ] /* IPD+AD options */
																							/* Note that additional options are not allowed! */
																							
																							
	*****************
	* Initial setup *	
	*****************
	marksample touse, novarlist	// -novarlist- option prevents -marksample- from setting `touse' to zero if any missing values in `varlist'
								// we want to control this behaviour ourselves, e.g. by using KEEPALL option
	
	gettoken _USE invlist : varlist
	tokenize `outvlist'
	args _ES _seES _LCI _UCI _WT _NN

	if `"`npts'"'!=`""' {
		cap confirm numeric var `_NN'
		if _rc {
			nois disp as err _n "cannot use {bf:npts} option; no patient numbers available"
			nois disp as err "maybe the option {bf:npts(}{it:varname}{bf:)} was intended?"
			exit 198
		}
		local npts npts(`_NN')
		local nptsvar `_NN'
	}
	
	// rename locals for consistency with rest of metan.ado
	local _BY     `by' 
	local _STUDY  `study'
	local _LABELS `labels'
	local _SOURCE `source'
	
	cap confirm numeric var `ccvar'
	if !_rc local _CC `ccvar'


	********************************
	* Test validity of lcols/rcols *
	********************************
	// Cannot be any of the names -metan- (or -ipdmetan- etc.) uses for other things
	// To keep things simple, forbid any varnames:
	//  - beginning with a single underscore followed by a capital letter
	//  - beginning with "_counts" 
	// (Oct 2018: N.B. was `badnames')
	local lrcols `lcols' `rcols'
	local check = 0	
	if trim(`"`lrcols'"') != `""' {
		local cALPHA `c(ALPHA)'

		foreach el of local lrcols {
			local el2 = substr(`"`el'"', 2, 1)
			if substr(`"`el'"', 1, 1)==`"_"' & `: list el2 in cALPHA' {
				nois disp as err _n `"Error in option {bf:lcols()} or {bf:rcols()}:  Variable names such as {bf:`el'}, beginning with an underscore followed by a capital letter,"'
				nois disp as err `" are reserved for use by {bf:ipdmetan}, {bf:ipdover} and {bf:forestplot}."'
				nois disp as err `"In order to save the results set, please rename this variable or use {bf:{help clonevar}}."'
				exit 101
			}
			else if substr(`"`el'"', 1, 7)==`"_counts"' {
				nois disp as err _n `"Error in option {bf:lcols()} or {bf:rcols()}:  Variable names beginning {bf:_counts} are reserved for use by {bf:ipdmetan}, {bf:ipdover} and {bf:forestplot}."'
				nois disp as err `"In order to save the results set, please rename this variable or use {bf:{help clonevar}}."'
				exit 101
			}
		
			// `saving' / `clear' only:
			// Test validity of (value) *label* names: just _BY, _STUDY, _SOURCE as applicable
			// Value labels are unique within datasets. Hence, not a problem for a var in lcols/rcols to have same value label as the by() or study() variable.
			// However, a var in lcols/rcols **cannot** use the label name _BY or _STUDY **unless** the by() or study() variable is already sharing that label name.
			// (Also, cannot use _SOURCE as a value label if `"`_SOURCE'"'!=`""')
			if `"`saving'"'!=`""' | `"`clear'"'!=`""' {
			
				local lrlab : value label `el'
				if `"`lrlab'"'==`"_BY"' {
					if `"`_BY'"'==`""' local check = 1
					else {
						if `"`: value label `_BY''"'!=`"_BY"' local check = 1
					}
				}
				if `"`lrlab'"'==`"_STUDY"' {
					if `"`_STUDY'"'==`""' local check = 1
					else {
						if `"`: value label `_STUDY''"'!=`"_STUDY"' local check = 1
					}
				}
				if `"`lrlab'"'==`"_SOURCE"' {
					if `"`_SOURCE'"'==`""' local check = 1
					else {
						if `"`: value label `_SOURCE''"'!=`"_SOURCE"' local check = 1
					}
				}
				if `check' {
					disp as err _n `"Error in option {bf:lcols()} or {bf:rcols()}:  Label name {bf:`lrlab'} attached to variable {bf:`el'}"'
					disp as err `"  is reserved for use by {bf:metan} and {bf:forestplot}."'
					disp as err `"In order to save the results set, please rename the label attached to this variable (e.g. using {bf:{help label copy}})."'
					exit 101
				}
			}		// end if `"`saving'"'!=`""' | `"`clear'"'!=`""'
		}		// end foreach el of local lrcols
	}		// end if trim(`"`lrcols'"') != `""'		
	
	
	
	********************************
	* Subgroup and Overall effects *
	********************************
	// Create new observations to hold subgroup & overall effects (_USE==3, 5)
	//   (these can simply be removed again to restore the original data.)

	// N.B. Such observations may already have been created if passed through from -ipdmetan-
	//   but in any case, cover all bases by checking for (if applicable) a _USE==3 corresponding to each `by'-value,
	//   plus a single overall _USE==5.

	// If `saving' or `clear', need to -preserve- at this point
	//  (also take the opportunity to test validity of filename)
	if `"`saving'"'!=`""' | `"`clear'"'!=`""'{
	
		if `"`saving'"'!=`""' {
			// use modified version of _prefix_saving.ado to handle `stacklabel' option
			my_prefix_savingAD `saving'
			local saving `"`s(filename)'"'
			local 0 `", `s(options)'"'
			syntax [, STACKlabel * ]
			local saveopts `"`options'"'
		}
		
		// (N.B. if _rsample!="", i.e. no saved vars: already preserved)
		if `"`rsample'"'==`""' preserve
		
		// keep `touse' itself for now to make subsequent coding easier
		qui keep if `touse'

	}		// end if `"`saving'"'!=`""' | `"`clear'"'!=`""'
	
	tempvar obs
	qui gen long `obs' = _n

	if `"`_BY'"'!=`""' {
		qui levelsof `_BY' if `touse' & inlist(`_USE', 1, 2), missing local(bylist)
		local nby : word count `bylist'
	}
	
	// Multiple models
	local m : word count `modellist'
	gettoken model1 : modellist

	// Setup "translation" from ovstats/bystats matrix rownames to stored varnames
	if `"`xoutvlist'"'!=`""' {
		local rownames
		cap local rownames : rownames `ovstats'
		if _rc cap local rownames : rownames `bystatslist'	// if `xoutvlist', multiple models not allowed
															// so `bystatslist' should contain (at most) a single element
		if `"`rownames'"'!=`""' {
			// `rownames' always begins [elements 1-6] = eff se_eff eff_lci eff_uci npts ...
			// But although npts always exists in `rownames' (but might contain only missing values),
			//   varname _NN is *not* in `xoutvlist'.
			// Hence, we want to map from the 6th element onwards (i.e. add 5).
			local nx : word count `xoutvlist'
			assert `nx' == `: word count `rownames'' - 5
			forvalues i = 1 / `nx' {
				local ii = `i' + 5
				local el : word `ii' of `rownames'
				local vnames `vnames' _`el'
				local rnames `rnames'  `el'
			}
			
			tokenize `xoutvlist'
			args `vnames'			// for later
		}
	}
	local vnames _ES  _seES    _LCI    _UCI `vnames'
	local rnames eff se_eff eff_lci eff_uci `rnames'
	
	// if cumulative, don't need _USE==3, 5; remove (e.g. if created by -ipdmetan-)
	if `"`cumulative'"'!=`""' {
		qui drop if `touse' & inlist(`_USE', 3, 5)
	}
	
	else {
	
		// if rfdist, obtain appropriate varnames
		if `"`rfdist'"'!=`""' {			
			tempvar _rfLCI _rfUCI
			qui gen double `_rfLCI' = .
			qui gen double `_rfUCI' = .
			
			local vnames `vnames' _rfLCI _rfUCI
			local rnames `rnames'  rflci  rfuci
		}
		local na : word count `vnames'
		
		// subgroup effects (`_USE'==3)
		if `"`_BY'"'!=`""' & `"`subgroup'"'==`""' {
			local i = 1
			foreach byi of local bylist {
			
				summ `obs' if `touse' & `_USE'==3 & `_BY'==`byi', meanonly
				// should be a maximum of one, if created by -ipdmetan-
				// if none, add to end;  if one, expand.  That way, they are all together
				
				// Also: Note that `obs' will not needed beyond this section, so use it to identify models
				if r(N)==0 {
					local omin = _N
					local omax = `omin' + `m'
					qui set obs `omax'
					local ++omin
					qui replace `_BY' = `byi' in `omin'/`omax'
					qui replace `_USE' = 3 in `omin'/`omax'
					qui replace `touse' = 1 in `omin'/`omax'
				}
				else if r(N)==1 {
					local omin = r(min)
					qui expand `m' if `obs' == `omin'
					local omax = `omin' + `m'
				}
				else {		// this should never actually happen
					disp as err "Error in data structure: more than one observation with _USE==3"
					exit 198
				}
												
				// insert statistics from `bystats'
				forvalues j = 1 / `m' {
					local model : word `j' of `modellist'
					if (`j' > 1 & "`secsub'"!="") | "`model'"=="user`j'" {
						qui drop in `=`omin' - 1 + `j'' / `omax'
						continue, break
					}
					else {
						local bystats : word `j' of `bystatslist'
						if "`bystats'"!="" {
							forvalues k = 1 / `na' {
								local v  : word `k' of `vnames'
								local el : word `k' of `rnames'
								local rownumb = rownumb(`bystats', "`el'")
								if !missing(`rownumb') {
									qui replace ``v'' = `bystats'[`rownumb', `i'] in `=`omin' - 1 + `j''
								}
							}
						}
						if `"`sgwt'"'==`""' {		// `mwt' should always exist if `"`by'"'!=`""' & `"`subgroup'"'==`""' & `"`sgwt'"'==`""'
							qui replace `_WT' = `mwt'[`j', `i'] in `=`omin' - 1 + `j''
						}						
						qui replace `obs' = `j' in `=`omin' - 1 + `j''
					}
				}
				if `"`sgwt'"'!=`""' qui replace `_WT' = 100 in `omin'	// only for the first model
				
				local ++i
				
			}	// end foreach byi of local bylist
		}	// end if `"`_BY'"'!=`""' & `"`subgroup'"'==`""'
		
		
		// overall effect (`_USE'==5)
		if `"`overall'"'==`""' {
			
			summ `obs' if `_USE'==5 & `touse', meanonly
			// should be a maximum of one, if created by -ipdmetan-
			// if none, add to end;  if one, expand.  That way, they are all together
			if r(N)==0 {
				local omin = _N
				local omax = `omin' + `m'
				qui set obs `omax'
				local ++omin
				qui replace `_USE' = 5 in `omin'/`omax'
				qui replace `touse' = 1 in `omin'/`omax'
			}
			else if r(N)==1 {
				local omin = r(min)
				qui expand `m' if `obs' == `omin'
				local omax = `omin' + `m'
			}
			else {		// this should never actually happen
				disp as err "Error in data structure: more than one observation with _USE==5"
				exit 198
			}
			
			forvalues j = 1 / `m' {
				local model : word `j' of `modellist'
				qui replace `obs' = `j' in `=`omin' - 1 + `j''
				
				// insert user-defined stats if appropriate...
				if "`model'"=="user`j'" {
					tokenize `user`j'stats'
					qui replace `_ES'  = `1' in `=`omin' - 1 + `j''
					qui replace `_LCI' = `2' in `=`omin' - 1 + `j''
					qui replace `_UCI' = `3' in `=`omin' - 1 + `j''
				}

				// ... o/w insert statistics from `ovstats'
				else if "`ovstats'"!="" {
					forvalues k = 1 / `na' {
						local v  : word `k' of `vnames'
						local el : word `k' of `rnames'
						local rownumb = rownumb(`ovstats', "`el'")
						if !missing(`rownumb') {
							qui replace ``v'' = `ovstats'[`rownumb', `j'] in `=`omin' - 1 + `j''
						}
					}
				}
			}
			
			if `"`ovwt'"'!=`""' qui replace `_WT' = 100 in `omin'		// only for the first model
			
		}		// end if `"`overall'"'==`""'
	}		// end else (i.e. if `"`cumulative'"'==`""')

	// _BY will typically be missing for _USE==5, so need to be careful when sorting
	// Hence, generate marker of _USE==5 to sort on *before* _BY
	summ `_USE' if `touse', meanonly
	if r(max)==5 & `"`_BY'"'!=`""' {
		tempvar use5
		qui gen byte `use5' = (`_USE'==5)
	}
	local notuse5 = cond("`use5'"=="", "", `"*(!`use5')"')

	// `obs' is not needed anymore, so use it to identify models for USE==3, 5
	qui replace `obs' = 0 if !inlist(`_USE', 3, 5)
	qui assert inrange(`obs', 1, `m') if `obs'>0
	local useModel `obs'				// rename
	
	
	*******************************
	* Fill down counts, npts, oev *
	*******************************	
	local params : word count `invlist'
	
	// Setup `counts' and `oev' options
	if `"`counts'"'!=`""' | `"`oev'"'!=`""' {
		tokenize `invlist'

		if `"`counts'"'!=`""' {
			if `params' == 6 args n1 mean1 sd1 n0 mean0 sd0			// `invlist'
			else {
				tempvar sum_e1 sum_e0
			
				// Log-rank (Peto) HR from -ipdmetan-
				// counts = "events/total in research arm; events/total in control arm"
				if `"`lrvlist'"'!=`""' {
					cap assert `params'==2 & "`logrank'"!=""
					if _rc {
						nois disp as err _n `"Error in communication between {bf:ipdmetan} and {bf:metan}"'
						exit 198
					}
					tokenize `lrvlist'
					args n1 n0 e1 e0
				}

				// Binary outcome (OR, Peto, RR, RD)
				// counts = "events/total in research arm; events/total in control arm"
				else if `params'==4 {
					args e1 f1 e0 f0		// `invlist'
					tempvar n1 n0
					qui gen long `n1' = `e1' + `f1'
					qui gen long `n0' = `e0' + `f0'
				}
				
				else {
					nois disp as err _n `"Note: {bf:counts} is only valid with 2x2 count data or continuous data, so will be ignored"'
					local counts
				}	
			}
		}
		
		if `"`oev'"'!=`""' {
			if "`logrank'"!="" {
				tokenize `invlist'
				args _OE _V
			}
			else if "`model1'"=="peto" {
				tempvar _OE _V
				qui gen double `_OE' = `_ES' / `_seES'^2
				qui gen double `_V'  =    1  / `_seES'^2
			}
			else {
				disp as err _n `"Note: {bf:oev} is not applicable without log-rank data or Peto ORs, so will be ignored"'
				local oev
			}
		}
		if `"`oev'"'!=`""' {
			label variable `_OE' `"O-E(o)"'
			label variable `_V'  `"V(o)"'
			format `_OE' %6.2f
			format `_V' %6.2f
		}
	}			// end if `"`counts'"'!=`""' | `"`oev'"'!=`""'

	// Create `sumvlist' containing list of vars to fill down
	if `"`counts'"'!=`""' {
		local sumvlist n1 n0 
		if inlist(`params', 2, 4) {
			local sumvlist `sumvlist' e1 e0 
		}
		tempvar _counts1 _counts0
	}
	if `"`oev'"'!=`""' local sumvlist `sumvlist' _OE _V
	if "`_NN'"!=""     local sumvlist `sumvlist' _NN
	
	if `"`cumulative'`influence'"'!=`""' & `"`altwt'"'==`""' {
		foreach x of local sumvlist {
			tempvar sum_`x'
		}
	}
	
	// Now do the actual "filling down".
	// If `cumulative' or `influence', keep *both* versions: the original (to be stored in the current dataset, unless `nokeepvars')
	//   and the "filled down" (for the forestplot and/or saved dataset)...
	//   ...unless `altwt', in which case just keep the original.
	qui isid `touse' `use5' `_BY' `_USE' `useModel' `_SOURCE' `sortby', sort missok
	tempvar tempsum
	
	// subgroup totals
	if `"`_BY'"'!=`""' & `"`subgroup'"'==`""' {
		foreach x of local sumvlist {
			local xtype : type ``x''
			local xtype = cond(inlist("`xtype'", "float", "double"), "double", "long")
			qui by `touse' `use5' `_BY' : gen `xtype' `tempsum' = sum(``x'') if `touse'			
			qui replace ``x'' = `tempsum' if `touse' & `_USE'==3 & `useModel'==1	// only for first model, as will repeat
			qui replace ``x'' = .         if `touse' & `_USE'==3 & `useModel' >1

			if `"`cumulative'`influence'"'!=`""' & `"`altwt'"'==`""' {
				if `"`influence'"'!=`""' {
					qui gen `xtype' `sum_`x'' = `tempsum' if `touse' & `_USE'==3	// `useModel' not relevant as cumulative/influence not compatible with multiple models
					
					qui by `touse' `use5' `_BY' : replace `tempsum' = ``x''[_N]
					qui replace `sum_`x'' = `tempsum' - ``x'' if `touse' & `_USE'==1
				}
				else qui gen `xtype' `sum_`x'' = `tempsum'
			}
			drop `tempsum'
		}
	}

	// overall totals
	if `"`overall'"'==`""' {
		foreach x of local sumvlist {
			local xtype : type ``x''
			local xtype = cond(inlist("`xtype'", "float", "double"), "double", "long")
			qui gen `xtype' `tempsum' = sum(``x'') if `touse' & `_USE'!=3
			qui replace ``x'' = `tempsum' if `touse' & `_USE'==5 & `useModel'==1	// only for first model, as will repeat
			qui replace ``x'' = .         if `touse' & `_USE'==5 & `useModel' >1

			if `"`cumulative'`influence'"'!=`""' & `"`altwt'"'==`""' {
				if `"`influence'"'!=`""' {
					summ ``x'' if `touse' & `_USE'==5, meanonly
					
					if !(`"`_BY'"'!=`""' & `"`subgroup'"'==`""') {
						qui gen `xtype' `sum_`x'' = `tempsum' if `touse' & `_USE'==5
						qui replace `sum_`x'' = r(sum) - ``x'' if `touse' & `_USE'==1
					}
					else {
						qui replace `sum_`x'' = `tempsum' if `touse' & `_USE'==5	// `useModel' not relevant as cumulative/influence not compatible with multiple models
					}
				}
				else if `"`_BY'"'==`""' {
					qui gen `xtype' `sum_`x'' = `tempsum'
				}
			}
			drop `tempsum'
		}
	}

	// Reassign locals `x' to reference vars previously referenced by locals `sum_`x''
	// That is, "rename" our filled-down vars to their "original/natural" names.
	// (N.B. vars  n1, n0, e1, e0, _OE, _V are only relevant to *saved* datasets, not to the *original* dataset...
	//  ... but _NN needs to be treated differently)
	if `"`cumulative'`influence'"'!=`""' {
		if `"`altwt'"'==`""' {
			foreach x of local sumvlist {
				local `x' `sum_`x''
			}
		}
	}

	
	** Finally, create `counts' string for forestplot
	if `"`counts'"'!=`""' {

		// option "counts" is guaranteed to be present (see ParseFPlotOpts); hence going forward local counts = "counts"
		local 0 `", `counts'"'
		syntax [, COUNTS GROUP1(string asis) GROUP2(string asis) ]
	
		// Titles
		// amended Feb 2018 due to local x = "" issue with version <13
		// local title1 = cond(`"`group2'"'!=`""', `"`group2'"', `"Treatment"')
		// local title0 = cond(`"`group1'"'!=`""', `"`group1'"', `"Control"')
		if `"`group2'"'!=`""' local title1 `"`group2'"'
		else local title1 "Treatment"
		if `"`group1'"'!=`""' local title0 `"`group1'"'
		else local title0 "Control"	
		
		// Binary data & logrank HR
		if inlist(`params', 2, 4) {
			qui gen `_counts1' = string(`e1') + "/" + string(`n1') if inlist(`_USE', 1, 2) | (inlist(`_USE', 3, 5) & `useModel'==1)
			qui gen `_counts0' = string(`e0') + "/" + string(`n0') if inlist(`_USE', 1, 2) | (inlist(`_USE', 3, 5) & `useModel'==1)
			label variable `_counts1' `"`title1' n/N"'
			label variable `_counts0' `"`title0' n/N"'
			drop `n1' `n0'							// tidy up
		}
		
		// N mean SD for continuous data
		// counts = "N, mean (SD) in research arm; N, mean (SD) events/total in control arm"
		else {
			tempvar _counts1msd _counts0msd

			qui gen long `_counts1' = `n1' if inlist(`_USE', 1, 2) | (inlist(`_USE', 3, 5) & `useModel'==1)
			qui gen `_counts1msd' = string(`mean1', "%7.2f") + " (" + string(`sd1', "%7.2f") + ")" if inlist(`_USE', 1, 2)
			label variable `_counts1' "N"
			label variable `_counts1msd' `"`title1' Mean (SD)"'
					
			qui gen long `_counts0' = `n0' if inlist(`_USE', 1, 2) | (inlist(`_USE', 3, 5) & `useModel'==1)
			qui gen `_counts0msd' = string(`mean0', "%7.2f") + " (" + string(`sd0', "%7.2f") + ")" if inlist(`_USE', 1, 2)
			label variable `_counts0' "N"
			label variable `_counts0msd' `"`title0' Mean (SD)"'
					
			// Find max number of digits in `_counts1', `_counts0'
			summ `_counts1', meanonly
			if r(N) {
				local fmtlen = floor(log10(`r(max)'))
				format `_counts1' %`fmtlen'.0f
			}
			summ `_counts0', meanonly
			if r(N) {
				local fmtlen = floor(log10(`r(max)'))
				format `_counts0' %`fmtlen'.0f
			}
		}

		local countsvl _counts1 _counts0
		if `params'==6 local countsvl `countsvl' _counts1msd _counts0msd

	}	// end if `"`counts'"'!=`""'
	
	// end of "filling-down counts" section

	
	** Vaccine efficacy
	// (carried over from -metan- )
	tempvar strlen
	if `"`efficacy'"'!=`""' {

		// check: OR and RR only
		cap assert inlist("`summstat'", "or", "rr")
		if _rc {
			nois disp as err _n "Vaccine efficacy statistics only possible with odds ratios and risk ratios"
			exit _rc
		}
	
		if `"`saving'"'!=`""' | `"`clear'"'!=`""' {
			cap drop _VE
			local _VE _VE
		}
		else tempvar _VE
		qui gen `_VE' = string(100*(1 - exp(`_ES')), "%4.0f") + " (" ///
			+ string(100*(1 - exp(`_LCI')), "%4.0f") + ", " ///
			+ string(100*(1 - exp(`_UCI')), "%4.0f") + ")" if `_USE'==1 | (inlist(`_USE', 3, 5) & `useModel'==1)
		
		label variable `_VE' "Vaccine efficacy (%)"
		
		qui gen `strlen' = length(`_VE')
		summ `strlen', meanonly
		format %`r(max)'s `_VE'
		qui compress `_VE'
		drop `strlen'
	}

	

	*****************************************
	* Rename tempvars to permanent varnames *
	*****************************************
	
	// Initialize varlists to save in Results Set:
	// `core':  "core" variables (N.B. *excluding* _NN)
	local core _ES _seES _LCI _UCI _WT	
	// tosave':  additional "internal" vars created by specific options
	// [may contain:  _NN;  _OE _V if `oev';  `countsvl' if `counts';  _VE if `efficacy';  _CC if `cc';  _rfLCI _rfUCI if `rfdist']
	if `"`_NN'"'!=`""' local tosave _NN
	if `"`oev'"'!=`""' local tosave `tosave' _OE _V
	if `"`counts'"'!=`""' local tosave `tosave' `countsvl'
	if `"`efficacy'"'!=`""' local tosave `tosave' _VE
	if `"`_CC'"'!=`""' local tosave `tosave' _CC
	if `"`rfdist'"'!=`""' local tosave `tosave' _rfLCI _rfUCI
	if `"`xoutvlist'"'!=`""' local tosave : list tosave | vnames
	local tosave : list tosave - core
	
	// "Labelling" variables: _USE, _STUDY, _BY etc.
	local labelvars _USE
	local _BY = cond(`"`byad'"'!=`""', `""', `"`_BY'"')
	if `"`_BY'"'!=`""'     local labelvars `labelvars' _BY
	if `"`_SOURCE'"'!=`""' local labelvars `labelvars' _SOURCE
	local labelvars `labelvars' _STUDY _LABELS

	// If `saving' / `clear', finish off renaming tempvars to permanent varnames
	// ...in order to store them in the *saved* dataset (NOT the data in memory)
	if `"`saving'"'!=`""' | `"`clear'"'!=`""' {
		
		local tocheck `labelvars' `core' `tosave'
		foreach v of local tocheck {
			if `"``v''"'!=`""' {						// N.B. xoutvlist is independent of [no]keepvars.
				confirm variable ``v''

				// For numeric _STUDY, _BY and _SOURCE,
				//   check if pre-existing var (``v'') has the "correct" value label name (`v').
				// If it does not, drop any existing value label `v', and copy current value label across to `v'.
				if inlist("`v'", "_STUDY", "_BY", "_SOURCE") {
					if `"`: value label ``v'''"' != `""' & `"`: value label ``v'''"' != `"`v'"' {
						cap label drop `v'
						label copy `: value label ``v''' `v'
						label values ``v'' `v'
					}
				}
			
				// Similar logic now applies to variable names:
				// Check if pre-existing var has the same name (i.e. was named _BY, _STUDY etc.)
				// If it does not, first drop any existing var named _BY, _STUDY (e.g. left over from previous -metan- call), then rename.
				if `"``v''"'!=`"`v'"' {
					cap drop `v'
					
					// If ``v'' is in `lrcols', use -clonevar-, so as also to keep original name
					if `: list `v' in lrcols' {
						qui clonevar `v' = ``v'' if `touse'
					}
					else qui rename ``v'' `v'
				}

				// IF RENAMING, DON'T REFORMAT; KEEP ORIGINAL FORMAT
				// SO THAT -forestplot- USES ORIGINAL FORMAT
				// APPLIES TO _ES, _seES, _LCI, _UCI
				else if inlist("`v'", "_ES", "_seES", "_LCI", "_UCI") {
					format  %6.3f ``v''
					label variable ``v'' "`v'"
				}
				
				local `v' `v'				// for use with subsequent code

				// Added Jan 2019 [CHECK IF THERE IS A BETTER WAY TO HANDLE THIS]
				if "`v'"=="_NN" & `"`npts'"'!=`""' {
					local npts npts(_NN)
					local nptsvar _NN
				}				
			}
		}

		// if `byad' (-ipdmetan- option),
		//  `by' has been pointing to `source'. For saving, create a separate _BY variable. 
		if `"`byad'"'!=`""' {
			cap drop _BY
			cap label drop _BY
			qui gen byte _BY = _SOURCE
			label copy _SOURCE _BY
			label values _BY _BY
			local _BY _BY
		}			
				
		// Label variables with short-ish names for display on forest plots
		// Use characteristics to store longer, explanatory names
		char define `_ES'[Desc]  "Effect size"
		char define `_seES'[Desc] "Standard error of effect size"
		char define `_LCI'[Desc] "`level'% lower confidence limit"
		char define `_UCI'[Desc] "`level'% upper confidence limit"
		char define `_LCI'[Level] `level'
		char define `_UCI'[Level] `level'
		
		if `"`rfdlist'"'!=`""' {
			label variable `_rfLCI' "rfLCI"
			label variable `_rfUCI' "rfUCI"
			char define `_rfLCI'[RFLevel] `rflevel'
			char define `_rfUCI'[RFLevel] `rflevel'
			char define `_rfLCI'[Desc] "`rflevel'% lower limit of predictive distribution"
			char define `_rfUCI'[Desc] "`rflevel'% upper limit of predictive distribution"
		}
		
		if `"`xoutvlist'"'!=`""' {
			if `"`_crit'"'!=`""' {
				label variable `_crit' "Crit. val."
				char define `_crit'[Desc] "Critical value"
				format %6.2f `_crit'
			}
			if `"`_chi2'"'!=`""' {
				label variable `_chi2' "chi2"
				char define `_chi2'[Desc] "Chi-square statistic"
				format %6.2f `_chi2'
			}
			if `"`_dfkr'"'!=`""' {
				label variable `_dfkr' "Kenward-Roger df"
				char define `_dfkr'[Desc] "Kenward-Roger degrees of freedom"
				format %6.2f `_dfkr'
			}
			if `"`_pvalue'"'!=`""' {
				label variable `_pvalue' "p"
				char define `_pvalue'[Desc] "p-value for effect size"
				format %05.3f `_pvalue'
			}
			if `"`_Q'"'!=`""' {
				label variable `_Q' "Q"
				char define `_Q'[Desc] "Cochran's Q heterogeneity statistic"
				format %6.2f `_Q'
			}
			if `"`_Qdf'"'!=`""' {
				label variable `_Qdf' "Q df"
				char define `_Qdf'[Desc] "Degrees of freedom for Cochran's Q"
				format %6.0f `_Qdf'
			}
			if `"`_H'"'!=`""' {
				label variable `_H' "H"
				char define `_H'[Desc] "H heterogeneity statistic"
				format %6.2f `_H'
			}
			if `"`_Isq'"'!=`""' {
				label variable `_Isq' "I2"
				char define `_Isq'[Desc] "I-squared heterogeneity statistic"
				format %6.1f `_Isq'
			}
			if `"`_HsqM'"'!=`""' {
				label variable `_HsqM' "H2M"
				char define `_HsqM'[Desc] "Modified H-squared (H^2 - 1) heterogeneity statistic"
				format %6.2f `_HsqM'
			}
			if `"`_sigmasq'"'!=`""' {
				label variable `_sigmasq' "sigma2"
				char define `_sigmasq'[Desc] "Estimated average within-trial heterogeneity"
				format %6.3f `_sigmasq'
			}
			if `"`_tausq'"'!=`""' {
				label variable `_tausq' "tau2"
				char define `_tausq'[Desc] "Estimated between-trial heterogeneity"
				format %6.3f `_tausq'
			}
			if `"`_H_lci'"'!=`""' {
				label variable `_H_lci' "H LCI"
				char define `_H_lci'[Desc] "`tsqlevel'% lower confidence limit for H"
				format %6.2f `_H_lci'
			}
			if `"`_H_uci'"'!=`""' {
				label variable `_H_uci' "H UCI"
				char define `_H_uci'[Desc] "`tsqlevel'% upper confidence limit for H"
				format %6.2f `_H_uci'
			}
			if `"`_Isq_lci'"'!=`""' {
				label variable `_Isq_lci' "I2 LCI"
				char define `_Isq_lci'[Desc] "`tsqlevel'% lower confidence limit for I-squared"
				format %6.1f `_Isq_lci'
			}
			if `"`_Isq_uci'"'!=`""' {
				label variable `_Isq_uci' "I2 UCI"
				char define `_Isq_uci'[Desc] "`tsqlevel'% upper confidence limit for I-squared"
				format %6.1f `_Isq_uci'
			}
			if `"`_HsqM_lci'"'!=`""' {
				label variable `_HsqM_lci' "HsqM LCI"
				char define `_HsqM_lci'[Desc] "`tsqlevel'% lower confidence limit for modified H-squared"
				format %6.2f `_HsqM_lci'
			}
			if `"`_HsqM_uci'"'!=`""' {
				label variable `_HsqM_uci' "HsqM UCI"
				char define `_HsqM_uci'[Desc] "`tsqlevel'% upper confidence limit for modified H-squared"
				format %6.2f `_HsqM_uci'
			}
			if `"`_tsq_lci'"'!=`""' {
				label variable `_tsq_lci' "tau2 LCI"
				char define `_tsq_lci'[Desc] "`tsqlevel'% lower confidence limit for tau-squared"
				format %6.3f `_tsq_lci'
			}
			if `"`_tsq_uci'"'!=`""' {
				label variable `_tsq_uci' "tau2 UCI"
				char define `_tsq_uci'[Desc] "`tsqlevel'% upper confidence limit for tau-squared"
				format %6.3f `_tsq_uci'
			}
		}
	}		// end if `"`saving'"'!=`""' | `"`clear'"'!=`""'
	
	// variable name (title) and format for "_NN" (if appropriate)
	if `"`_NN'"'!=`""' {
		if `"`: variable label `_NN''"'==`""' label variable `_NN' "No. pts"
		qui gen `strlen' = length(string(`_NN'))
		summ `strlen' if `touse', meanonly
		local fmtlen = max(`r(max)', 3)		// min of 3, otherwise title ("No. pts") won't fit
		format `_NN' %`fmtlen'.0f			// right-justified; fixed format (for integers)
		drop `strlen'

		if      `"`cumulative'"'!=`""' label variable `_NN' "Cumulative no. pts"
		else if `"`influence'"'!=`""'  label variable `_NN' "Remaining no. pts"
	}
	
				
	*********************
	* Insert extra rows *
	*********************
	// ... for headings, labels, spacings etc.
	//  Note: in the following routines, "half" values of _USE are used temporarily to get correct order
	//        and are then replaced with whole numbers at the end			
	
	// variable name (titles) for "_LABELS" or `stacklabel'
	if `"`_BY'"'!=`""' {
		local byvarlab : variable label `_BY'
	}
	if `"`summaryonly'"'!=`""' & `"`_BY'"'!=`""' local labtitle `"`byvarlab'"'
	else {
		if `"`_BY'"'!=`""' local bytitle `"`byvarlab' and "'
		if `"`_STUDY'"'!=`""' & `"`summaryonly'"'==`""' {
			local svarlab : variable label `_STUDY'
		}
		local stitle `"`bytitle'`svarlab'"'
		if `"`influence'"'!=`""' local stitle `"`stitle' omitted"'
		local labtitle `"`stitle'"'
	}
	if `"`stacklabel'"'==`""' label variable `_LABELS' `"`labtitle'"'
	else label variable `_LABELS'		// no title if `stacklabel'


	// Extra line for heterogeneity in forest plot:
	//  either specified here, or previously via -ipdmetan- using `ipdxline' option
	local extraline = trim(`"`extraline'"')
	if      inlist(`"`extraline'"', "off", "n", "no", "non", "none") local extraline "no"
	else if inlist(`"`extraline'"', "on", "y", "ye", "yes") local extraline "yes"
	else if `"`extraline'"'!=`""' {
		disp as err `"invalid option {bf:extraline(`extraline')}"'
		exit 198
	}
	if "`extraline'"=="" local extraline : copy local ipdxline
	
	// If `npts', `counts' or `oev' requested for display on forest plot
	//   then heterogeneity stats will need to be on a new line [ unless manually overruled with extraline(off) ]
	if `"`het'`extraline'"'==`""' & `"`npts'`counts'`oev'`efficacy'"'!=`""' local extraline "yes"
	
	// `extraline' then becomes `nolcolscheck' for passing to -forestplot-
	// Logic here is:  `extraline' can be "yes", "no" or missing (i.e. undefined)
	// If definitely "yes", suppress the check in -forestplot- for columns which might clash with heterogeneity info etc.
	// Hence, it is possible to suppress this check *even if* such columns actually exist, if we think they *don't* in fact clash.
	local lcolscheck = cond(`"`extraline'"'==`"yes"', `"nolcolscheck"', `""')

	
	// Now temporarily multiply _USE by 10
	// to enable intermediate numberings for sorting the extra rows
	qui replace `_USE' = `_USE'	* 10
	tempvar expand
	
	* Subgroup headings
	// Idea is to expand for "all values of _BY", but leave the "overall" row(s) alone (_USE==5).
	// _BY is missing for _USE==5, but this won't work as "missing" could equally be a legitimate value for _BY!!
	// So, instead, we use `notuse5', where we have previously generated `use5' to mark those observations (_USE==5)
	//   where we don't want _BY groups to be expanded.
	if `"`_BY'"'!=`""' {
		if `"`summaryonly'"'==`""' {
			qui bysort `touse' `_BY' (`sortby') : gen byte `expand' = 1 + 2*`touse'*(_n==1)`notuse5'
			qui expand `expand'
			qui replace `expand' = !(`expand' > 1)							// `expand' is now 0 if expanded and 1 otherwise (for sorting)
			sort `touse' `_BY' `expand' `_USE' `useModel' `_SOURCE' `sortby'
			qui by `touse' `_BY' : replace `_USE' = 0  if `touse' & !`expand' & _n==2	// row for headings (before)
			qui by `touse' `_BY' : replace `_USE' = 41 if `touse' & !`expand' & _n==3	// row for blank line (after)
		}
		else {
			summ `_BY' if `touse', meanonly
			qui bysort `touse' `_BY' (`sortby') : gen byte `expand' = 1 + `touse'*(`_BY'==`r(max)')*(_n==_N)`notuse5'
			qui expand `expand'
			qui replace `expand' = !(`expand' > 1)							// `expand' is now 0 if expanded and 1 otherwise (for sorting)
			sort `touse' `_BY' `expand' `_USE' `useModel' `_SOURCE' `sortby'
			qui by `touse' `_BY' : replace `_USE' = 41 if `touse' & !`expand' & _n==2	// row for blank line (only after last subgroup)
		}
		drop `expand'
		qui replace `useModel' = . if `_USE'==41	// ensure blank lines come at the end
					
		// Subgroup spacings & heterogeneity
		if "`subgroup'"=="" & `"`extraline'"'==`"yes"' {
			qui bysort `touse' `_BY' (`sortby') : gen byte `expand' = 1 + `touse'*(_n==_N)`notuse5'
			qui expand `expand'
			qui replace `expand' = !(`expand' > 1)						// `expand' is now 0 if expanded and 1 otherwise (for sorting)
			sort `touse' `_BY' `expand' `_USE' `useModel' `_SOURCE' `sortby'
			qui by `touse' `_BY' : replace `_USE' = 39 if `touse' & !`expand' & _n==2		// extra row for het if lcols
			qui replace `useModel'=1 if `_USE'==39											// extra row only needed for first model
			
			// An extra subtlety if `cumulative':
			//  there are no overall diamonds; instead the final _USE==1 observation is marked with `use3'
			// But we *don't* want to mark expanded obs with `use3'.
			if "`use3'"!="" {
				qui by `touse' `_BY' : replace `use3' = 0 if `touse' & !`expand' & _n==2
			}

			drop `expand'
		}
	}
	
	// Prediction intervals
	if `"`rfdist'"'!=`""' {
		local oldN = _N
		qui gen byte `expand' = 1 + `touse'*inlist(`_USE', 30, 50) *!missing(`_rfLCI')
		qui expand `expand'
		drop `expand'
		qui replace `_USE' = 35 if `touse' & _n>`oldN' & `_USE'==30
		qui replace `_USE' = 55 if `touse' & _n>`oldN' & `_USE'==50
	}
	
	// Blank out effect sizes etc. in `expand'-ed rows
	// March 2018: can we generalise this to be "all except..." instead of "all these"
	// answer: no, because there might be other data in memory entirely irrelevant to -metan-
	// Dec 2018: if `_BY' is also in `lrcols', exclude from this procedure
	local lrcols2 : copy local lrcols
	if `"`_BY'"'!=`""' & `"`lrcols'"'!=`""' {
		local lrcols2 : list lrcols - _BY
	}
	
	foreach x in _LABELS `core' `tosave' {
		cap confirm numeric var ``x''
		if !_rc qui replace ``x'' = .  if `touse' & !inlist(`_USE', 10, 20, 30, 50)
		else    qui replace ``x'' = "" if `touse' & !inlist(`_USE', 10, 20, 30, 50)
	}
	
	if `"`summaryonly'"'==`""' {
		if `"`_STUDY'"'!=`""' {
			qui replace `_STUDY' = . if `touse' & !inlist(`_USE', 10, 20)
		}
		
		// don't blank out `_SOURCE' if `byad' and not `saving' / `clear'
		//  since in that case `_SOURCE' is doing the job of `_BY'
		if `"`_SOURCE'"'!=`""' & !(`"`byad'"'!=`""' & `"`saving'"'==`""' & `"`clear'"'==`""') {
			qui replace `_SOURCE' = . if `touse' & !inlist(`_USE', 10, 20)
		}
	}
	
	// extra row to contain what would otherwise be the leftmost column heading if `stacklabel' specified
	// (i.e. so that heading can be used for forestplot stacking)
	if `"`stacklabel'"' != `""' {
		local newN = _N + 1
		qui set obs `newN'
		qui replace `touse' = 1  in `newN'
		qui replace `_USE' = -10 in `newN'
		if "`use5'"=="" {
			tempvar use5						// we need `use5' here, regardless of whether it's needed elsewhere 
			qui gen byte `use5' = 0 if `touse'
		}
		qui replace `use5' = -1 in `newN'
		qui replace `_LABELS' = `"`labtitle'"' in `newN'
	}

	
	** Now insert label info into new rows
	
	// "ovstat" is a synonym for "hetstat"
	local hetstat = cond(`"`hetstat'"'==`""', `"`ovstat'"', `"`hetstat'"')	
	local 0 `", `hetstat'"'
	syntax [, ISQ Q Pvalue]
	opts_exclusive `"`isq' `q'"' hetstat 184
	local hetstat `isq'`q'
	
	tempname Isq Q Qdf Qpval

	// tausq-related stuff (incl. Qr) is meaningless for M-H, and also for Peto unless RE
	// (although this *does* include sensitivity analysis)
	// (also if user-defined weights, tausq-related stuff is meaningless *except* for Qr)
	
	// Multiple models
	forvalues j = 1 / `m' {
		local model : word `j' of `modellist'

		// "overall" labels
		if `"`overall'"'==`""' {
			local ovhetlab
			
			if `"`het'"'==`""' {
				if      "`model'"=="user1" local ovhetlab : copy local firststats
				else if "`model'"=="user2" local ovhetlab : copy local secondstats
				else {
					scalar `Isq' = `ovstats'[rownumb(`ovstats', "Isq"), `j']
					scalar `Q'   = `ovstats'[rownumb(`ovstats', "Q"),   `j']
					scalar `Qdf' = `ovstats'[rownumb(`ovstats', "Qdf"), `j']
					scalar `Qpval' = chi2tail(`Qdf', `Q')

					// tausq-related stuff (incl. Qr) is meaningless for M-H, and also for Peto unless RE
					// (N.B. although this *does* include sensitivity analysis)
					if "`hetstat'"=="q" {
						local ovhetlab = `"Q = "' + string(`Q', "%5.2f") + `" on `=`Qdf'' df"'
					}
					else {
						// altered Sep 2017 for v2.1, to match with metan/metaan behaviour
						// local ovlabel "(I-squared = " + string(100*`Isq', "%5.1f")+ "%)"
						local ovhetlab = `"I-squared = "' + string(`Isq', "%5.1f") + `"%"'
					}
					if "`pvalue'"!=`""' {
						local ovhetlab = `"`ovhetlab', p = "' + string(`Qpval', "%05.3f")
					}					
				}
				if `"`ovhetlab'"'!=`""' local ovhetlab `"(`ovhetlab')"'
				
				// Overall heterogeneity - extra row if lcols (first model only)
				if `"`extraline'"'==`"yes"' & `j'==1 {
					local newN = _N + 1
					qui set obs `newN'
					qui replace `touse' = 1  in `newN'
					qui replace `_USE'  = 59 in `newN'
					qui replace `useModel'=`j' in `newN'
					if "`use5'"!="" {
						qui replace `use5' = 1 in `newN'
					}
					qui replace `_LABELS' = `"`ovhetlab'"' if `_USE'==59 & `useModel'==`j'
					local ovhetlab				// ovlabel on line below so no conflict with lcols; then clear macro
				}
			}		// end else if `"`het'"'==`""'
		
			// Multiple models
			if `m' > 1 | "`model'"=="user1" {
				local 0 `", `modeltext'"'
				syntax [, MODEL`j'text(string) * ]
				local modText `", `model`j'text'"'
			}
			if `"`ovhetlab'"'!=`""' local ovhetlab `" `ovhetlab'"'		// add space
			qui replace `_LABELS' = `"Overall`modText'`ovhetlab'"' if `_USE'==50 & `useModel'==`j'
			
		}

		// subgroup ("by") headings & labels
		if `"`_BY'"'!=`""' {
		
			local i = 1
			foreach byi of local bylist {
				local bystats : word `j' of `bystatslist'
				
				// headings
				local bylabi : label (`_BY') `byi'
				if `"`summaryonly'"'==`""' {
					qui replace `_LABELS' = "`bylabi'" if `_USE'==0 & `_BY'==`byi'
				}
				
				// labels + heterogeneity
				if `"`subgroup'"'==`""' {
					
					// local sglabel = cond(`"`summaryonly'"'!=`""', `"`bylabi'"', `"Subgroup"')
					if `"`summaryonly'"'!=`""' local sglabel `"`bylabi'"'
					else local sglabel "Subgroup"		// amended Feb 2018 due to local x = ... issue with version <13
					local sghetlab
				
					if `"`het'"'==`""' {
						// User-defined second model, or nosecsub
						local model : word `j' of `modellist'
						if (`j' > 1 & "`secsub'"!="") | "`model'"=="user2" {		// "user1" cannot be used with "by"
							continue, break
						}
					
						scalar `Isq'   = `bystats'[rownumb(`bystats', "Isq"), `i']
						scalar `Q'     = `bystats'[rownumb(`bystats', "Q"),   `i']
						scalar `Qdf'   = `bystats'[rownumb(`bystats', "Qdf"), `i']
						scalar `Qpval' = chi2tail(`Qdf', `Q')
					
						// tausq-related stuff (incl. Qr) is meaningless for M-H, and also for Peto unless RE
						// (N.B. although this *does* include sensitivity analysis)
						if "`hetstat'"=="q" {
							local sghetlab = "Q = " + string(`Q', "%5.2f") + `" on `=`Qdf'' df"'
						}
						else {
							// local sghetlab = "(I-squared = " + string(100*`Isqi', "%5.1f")+ "%)"		// altered Sep 2017 for v2.1 to match with metan/metaan behaviour
							local sghetlab = "I-squared = " + string(`Isq', "%5.1f")+ `"%"'
						}
						if "`pvalue'"!=`""' {
							local sghetlab = `"`sghetlab', p = "' + string(`Qpval', "%05.3f")
						}
						if `"`sghetlab'"'!=`""' local sghetlab `"(`sghetlab')"'

						if `"`extraline'"'==`"yes"' & `j'==1 {		// extra row: first model only
							qui replace `_LABELS' = "`sghetlab'" if `_USE'==39 & `_BY'==`byi' & `useModel'==`j'
							local sghetlab			// sghetlab on line below so no conflict with lcols; then clear macro
						}
					}
					
					// Multiple models
					if `m' > 1 {
						local 0 `", `modeltext'"'
						syntax [, MODEL`j'text(string) * ]
						local modText `", `model`j'text'"'
					}
					if `"`sghetlab'"'!=`""' local sghetlab `" `sghetlab'"'		// add space
					qui replace `_LABELS' = `"`sglabel'`modText'`sghetlab'"' if `_USE'==30 & `_BY'==`byi' & `useModel'==`j'
				
				}
				local ++i
				
			}		// end foreach byi of local bylist
		}		// end if `"`_BY'"'!=`""'
	}		// end forvalues i=1/`m'
	
	// add between-group heterogeneity info
	// (N.B. `overall' as o/w `Qdiff' not calculated; `subgroup' as o/w `Qsum' not calculated)
	// ONLY USE "PRIMARY" (FIRST) MODEL
	if `"`_BY'"'!=`""' {
		local bystats : word 1 of `bystatslist'		
	
		if `"`overall'`subgroup'`het'"'==`""' {
			local newN = _N + 1
			qui set obs `newN'
			qui replace `touse' = 1  in `newN'
			qui replace `_USE'  = 49 in `newN'
			if "`use5'"!="" {
				qui replace `use5' = 0 in `newN'
			}
			
			tempname Q_ov Qsum Qdiff Qdiffp
			scalar `Q_ov' = `ovstats'[rownumb(`ovstats', "Q"), 1]
			scalar `Qsum' = 0
			forvalues i = 1 / `nby' {
				scalar `Qsum' = `Qsum' + `bystats'[rownumb(`bystats', "Q"), `i']
			}
			scalar `Qdiff' = `Q_ov' - `Qsum'			// between-subgroup heterogeneity (Qsum = within-subgroup het.)
			scalar `Qdiffp' = chi2tail(`nby'-1, `Qdiff')
			qui replace `_LABELS' = "Heterogeneity between groups: p = " + string(`Qdiffp', "%5.3f") in `newN'
		}
	}		// end if `"`_BY'"'!=`""'

	// Insert prediction interval data (will be checked later)
	if `"`rfdist'"'!=`""' {
		qui replace `_LABELS' = `"with estimated `rflevel'% prediction interval"' if inlist(`_USE', 35, 55)
		qui replace `_LCI' = `_rfLCI' if inlist(`_USE', 35, 55)
		qui replace `_UCI' = `_rfUCI' if inlist(`_USE', 35, 55)
		// qui drop if missing(`_LCI', `_UCI') & inlist(`_USE', 35, 55)		// if prediction interval was undefined
	}
	
	
	*********************
	* Sort, and tidy up *
	*********************

	if `"`keeporder'"'!=`""' {
		tempvar tempuse
		qui gen byte `tempuse' = `_USE'
		qui replace `tempuse' = 10 if `_USE'==20		// keep "insufficient data" studies in original study order (default is to move to end)
	}
	else local tempuse `_USE'
		
	qui isid `touse' `use5' `_BY' `useModel' `tempuse' `_SOURCE' `sortby', sort missok
	cap drop `use5'
	
	// Tidy up `_USE' (and scale back down by 10)
	quietly {
		replace `_USE' =  0 if `_USE' == -10
		replace `_USE' = 60 if `_USE' ==  41
		replace `_USE' = 30 if `_USE' ==  35
		replace `_USE' = 50 if `_USE' ==  55
		replace `_USE' = 40 if inlist(`_USE', 39, 49, 59)
		replace `_USE' = `_USE' / 10
	}	

	// Format and title weights
	label variable `_WT' "% Weight"
	format `_WT' %6.2f
	
	// Check prediction interval data (after sorting and finalising _USE)
	if `"`rfdist'"'!=`""' {
		cap {
			assert `_rfLCI' <= `_LCI'    if `touse' & !missing(`_rfLCI', `_LCI')
			assert `_rfUCI' >= `_UCI'    if `touse' & !missing(`_rfUCI', `_UCI')
			assert  missing(`_ES')       if `touse' & inlist(`_USE', 3, 5) & float(`_rfLCI')==float(`_LCI') & float(`_rfUCI')==float(`_UCI')
			assert !missing(`_ES'[_n-1]) if `touse' & inlist(`_USE', 3, 5) & float(`_rfLCI')==float(`_LCI') & float(`_rfUCI')==float(`_UCI')
		}
		if _rc {
			nois disp as err _n "Error in prediction interval data"
			exit _rc
		}
	}

	// Having added "overall", het. info etc., re-format _LABELS using study names only
	// (otherwise the "adjust" routine in forestplot.ProcessColumns can't have any effect)
	if `sfmtlen'==0 {
		qui gen `strlen' = length(`_LABELS')
		if `"`summaryonly'"'==`""' local anduse `"& inlist(`_USE', 1, 2)"'
		// unless no study estimates (`summaryonly'), limit to _USE==1 or 2
		summ `strlen' if `touse' `anduse', meanonly
		local sfmtlen = r(max)
		drop `strlen'
		
		// Format as left-justified; default length equal to longest study name
		// But, niche case: in case study names are very short, look at title as well
		// If user really wants ultra-short width, they can convert to string and specify %-s format
		tokenize `"`: variable label `_LABELS''"'
		while `"`1'"'!=`""' {
			local sfmtlen = max(`sfmtlen', length(`"`1'"'))
			macro shift
		}
	}
	else local sfmtlen = abs(`sfmtlen')
	format `_LABELS' %-`sfmtlen's		// left justify _LABELS


	// Generate effect-size column *here*,
	//  so that it exists immediately when results-set is opened (i.e. before running -forestplot-)
	//  for user editing e.g. adding p-values etc.
	// However, *if* it is edited, -forestplot- must be called as "forestplot, nostats rcols(_EFFECT)" otherwise it will be overwritten!
	//  (or use option `nokeepvars')
	if `"`saving'"'!=`""' | `"`clear'"'!=`""' {
		
		// need to peek into forestplot options to extract `dp'
		local 0 `", `forestplot'"'
		syntax [, DP(integer 2) * ]		
		if `"`eform'"'!=`""' local xexp exp
		summ `_UCI' if `touse', meanonly
		local fmtx = max(1, ceil(log10(abs(`xexp'(r(max)))))) + 1 + `dp'
			
		cap drop _EFFECT
		qui gen str _EFFECT = string(`xexp'(`_ES'), `"%`fmtx'.`dp'f"') if !missing(`_ES')
		qui replace _EFFECT = _EFFECT + " " if !missing(_EFFECT)
		qui replace _EFFECT = _EFFECT + "(" + string(`xexp'(`_LCI'), `"%`fmtx'.`dp'f"') + ", " + string(`xexp'(`_UCI'), `"%`fmtx'.`dp'f"') + ")"
		qui replace _EFFECT = `""' if !(`touse' & inlist(`_USE', 1, 3, 5))
		qui replace _EFFECT = "(Insufficient data)" if `touse' & `_USE' == 2

		local f = abs(fmtwidth("`: format _EFFECT'"))
		format _EFFECT %-`f's		// left-justify
		label variable _EFFECT `"`effect' (`level'% CI)"'
		local _EFFECT _EFFECT		
	}

	

	***************
	* Forest plot *
	***************
	// Finalise forestplot options
	// (do this whether or not `"`graph'"'==`""', so that options can be stored!)
	
	** Save _dta characteristic containing all the options passed to -forestplot-
	// so that they may be called automatically using "forestplot, useopts"
	// (N.B. `_USE', `_LABELS' and `_WT' should always exist)
	local useopts `"use(`_USE') labels(`_LABELS') wgt(`_WT') `cumulative' `eform' effect(`effect') `keepall' `wt' `stats' `warning' `plotid' `forestplot'"'
	if `"`_BY'"'!=`""' local useopts `"`macval(useopts)' by(`_BY')"'
	if trim(`"`lcols' `nptsvar' `_counts1' `_counts1msd' `_counts0' `_counts0msd' `_OE' `_V'"') != `""' {
		local useopts `"`macval(useopts)' lcols(`lcols' `nptsvar' `_counts1' `_counts1msd' `_counts0' `_counts0msd' `_OE' `_V') `lcolscheck'"'
	}
	if trim(`"`_VE' `rcols'"') != `""' local useopts `"`macval(useopts)' rcols(`_VE' `rcols')"'
	if `"`rfdist'"'!=`""' local useopts `"`macval(useopts)' rfdist(`_rfLCI' `_rfUCI')"'
	if `"`fpnote'"'!=`""' local useopts `"`macval(useopts)' note(`fpnote')"'
	local useopts = trim(itrim(`"`useopts'"'))
	
	// Store data characteristics
	// NOTE: Only relevant if `saving' / `clear' (but setup anyway; no harm done)
	char define _dta[FPUseOpts] `"`useopts'"'
	char define _dta[FPUseVarlist] `_ES' `_LCI' `_UCI'
	
	
	** Pass to forestplot
	if `"`graph'"'==`""' {
		if "`cumulative'"!="" {						// cumulative only; not influence
			qui replace `_USE' = 3 if `use3'==1		// ==1 in case new obs added, with `use3' missing
			drop `use3'
		}
		if "`summaryonly'"!="" {
			qui replace `touse' = 0 if inlist(`_USE', 1, 2)
		}
		
		cap nois forestplot `_ES' `_LCI' `_UCI' if `touse', `useopts'
		
		if _rc {
			if `"`err'"'==`""' {
				if _rc==1 nois disp as err _n `"User break in {bf:forestplot}"'
				else nois disp as err _n `"Error in {bf:forestplot}"'
			}
			c_local err noerr		// tell -metan- not to also report an "error in metan.BuildResultsSet"
			exit _rc
		}

		return add					// add scalars returned by -forestplot-
	}


	** Finally, save dataset
	if `"`saving'"'!=`""' | `"`clear'"'!=`""' {
		keep  `labelvars' `core' `tosave' `_EFFECT' `_WT' `lrcols'
		order `labelvars' `core' `tosave' `_EFFECT' `_WT' `lrcols'
			
		if `"`summaryonly'"'!=`""' qui drop if inlist(`_USE', 1, 2)
			
		local sourceprog = cond(`"`ipdmetan'"'!=`""', "ipdmetan", "metan")
		label data `"Results set created by `sourceprog'"'
		qui compress
		
		if `"`saving'"'!=`""' {
			qui save `"`saving'"', `saveopts'
		}
		if `"`clear'"'!=`""' cap restore, not
	}

end
	


* Modified version of _prefix_saving.ado
// [AD version] modified so as to include `stacklabel' option
// April 2018, for admetan v2.2

// subroutine of BuildResultsSet

program define my_prefix_savingAD, sclass
	 
	cap nois syntax anything(id="file name" name=fname) [, REPLACE * ]
	if !_rc {
		if "`replace'" == "" {
			local ss : subinstr local fname ".dta" ""
			confirm new file `"`ss'.dta"'
		}
	}
	else {
		di as err "invalid saving() option"
		exit _rc
	}
	
	sreturn clear
	sreturn local filename `"`fname'"'
	sreturn local options `"`replace' `options'"'

end






*******************************************************************************

***************************************************
* Stata subroutines called by PerformMetaAnalysis *  (and its subroutines)
***************************************************


* ProcessPoolingVarlist
// subroutine of PerformMetaAnalysis

// subroutine to processes (non-IV) input varlist to create appropriate varlist for the specified pooling method
// That is, generate study-level effect size variables,
// plus variables used to generate overall/subgroup statistics

program define ProcessPoolingVarlist, sclass

	syntax varlist(numeric min=3 max=7 default=none) [if] [in], ///
		SUMMSTAT(name) MODEL(name) TESTSTAT(name) OUTVLIST(varlist numeric min=5 max=8) ///
		[ TVLIST(namelist) LOGRank noINTeger CC(string) CCVAR(name) ]
	
	sreturn clear
	marksample touse, novarlist
	
	// unpack varlists
	tokenize `outvlist'
	args _ES _seES _LCI _UCI _WT _NN

	gettoken _USE invlist : varlist
	tokenize `invlist'
	local params : word count `invlist'
	
	
	** Setup for logrank HR (O-E & V)
	if "`logrank'"!="" {
		cap assert `params' == 2
		if _rc {
			disp as err `"Option {bf:logrank} supplied; {bf:metan} expected a 2-element {it:varlist}"'
			exit 198
		}		
		
		args oe va
		qui replace `_ES'   = `oe'/`va'    if `touse' & `_USE'==1		// logHR
		qui replace `_seES' = 1/sqrt(`va') if `touse' & `_USE'==1		// selogHR
	}

	
	** Otherwise, expect `params' to be 4 or 6
	else {
	
		** Generate effect size vars
		// (N.B. gen as tempvars for now, to accommodate inverse-variance;
		//       but will be renamed to permanent variables later if appropriate)
		
		// Binary outcome (OR, RR, RD)
		if `params' == 4 {
			
			// assert inlist("`summstat'", "or", "rr", "irr", "rrr", "rd")
			// MODIFIED APR 2019 FOR v3.3: REMOVE REFERENCE TO IRR
			assert inlist("`summstat'", "or", "rr", "rrr", "rd")
			args e1 f1 e0 f0		// events & non-events in trt; events & non-events in control (aka a b c d)

			tempvar r1 r0
			local type = cond("`integer'"=="", "long", "double")
			qui gen `type' `r1' = `e1' + `f1' if `touse'		// total in trt arm (aka a + b)
			qui gen `type' `r0' = `e0' + `f0' if `touse'		// total in control arm (aka c + d)
			qui replace   `_NN' = `r1' + `r0' if `touse'		// overall total
			
			// Continuity correction: already prepared by ParseModel
			if `"`cc'"'!=`""' {
				local 0 `"`cc'"'
				syntax [anything(name=ccval)] [, OPPosite EMPirical]

				cap confirm numeric variable `ccvar'
				local genreplace = cond(_rc, "gen byte", "replace")
				qui `genreplace' `ccvar' = `e1'*`f1'*`e0'*`f0'==0 if `touse' & `_USE'==1
				summ `ccvar', meanonly
				local nz = r(sum)
				if !`nz' {
					drop `ccvar'
					local cc
				}						// ... if continuity correction is *applicable*
				else {					// (N.B. from now on, -confirm numeric var `ccvar'- will be used to check if cc was applied)
						
					// Sweeting's "opposite treatment arm" correction
					if `"`opposite'"'!=`""' {
						tempvar cc1 cc0
						qui gen `cc1' = 2*`ccval'*`r1'/(`r1' + `r0')
						qui gen `cc0' = 2*`ccval'*`r0'/(`r1' + `r0')
					}
					
					// Empirical correction
					// (fixed effects only; needs estimate of theta using trials without zero cells)
					else if `"`empirical'"'!=`""' {
						
						// fixed effects only
						if !inlist("`model'", "fe", "mh") {
							nois disp as err "Empirical continuity correction only valid with fixed effects"
							exit 198
						}
												
						// more than one study without zero counts needed to estimate "prior"
						qui count if `touse' & `_USE'==1
						if r(N) == `nz' {
							nois disp as err "Insufficient data to implement empirical continuity correction"
							exit 198
						}						

						tempvar R cc1 cc0
						qui metan `e1' `f1' `e0' `f0' if `touse' & `_USE'==1, model(`model') `summstat' nocc nograph notable nohet
						qui gen `R' = `r0'/`r1'
						qui gen `cc1' = 2*`ccval'*exp(r(eff))/(`R' + exp(r(eff)))
						qui gen `cc0' = 2*`ccval'*`R'        /(`R' + exp(r(eff)))
						drop `R'
					}
					else {
						local cc1 = `ccval'
						local cc0 = `ccval'
					}
				
					tempvar e1_cont f1_cont e0_cont f0_cont t_cont
					qui gen double `e1_cont' = cond(`ccvar', `e1' + `cc1', `e1') if `touse'
					qui gen double `f1_cont' = cond(`ccvar', `f1' + `cc1', `f1') if `touse'
					qui gen double `e0_cont' = cond(`ccvar', `e0' + `cc0', `e0') if `touse'
					qui gen double `f0_cont' = cond(`ccvar', `f0' + `cc0', `f0') if `touse'
						
					tempvar r1_cont r0_cont t_cont
					qui gen double `r1_cont' = `e1_cont' + `f1_cont'
					qui gen double `r0_cont' = `e0_cont' + `f0_cont'
					qui gen double  `t_cont' = `r1_cont' + `r0_cont'
					
					if trim(`"`opposite'`empirical'"') != `""' {
						drop `cc1' `cc0'		// tidy up
					}
				}
			}
			if `"`cc'"'==`""' {
				local e1_cont `e1'
				local f1_cont `f1'
				local e0_cont `e0'
				local f0_cont `f0'
				local r1_cont `r1'
				local r0_cont `r0'
				local t_cont `_NN'
			}
			
			
			** Now branch by outcome measure
			tokenize `tvlist'
			
			if "`summstat'"=="or" {
			
				if `: word count `tvlist'' == 2 args oe va		// i.e. chi2opt (incl. Peto), but *not* M-H
				else args r s pr ps qr qs oe va					// M-H, and optionally also chi2opt
			
				if inlist("`teststat'", "chi2", "cmh") | "`peto'"!="" {
					tempvar c1 c0 ea
					local a `e1'									// synonym; makes it easier to read code involving chi2
					qui gen `type' `c1' = `e1' + `e0'				// total events (aka a + c)
					qui gen `type' `c0' = `f1' + `f0'				// total non-events (aka b + d)
					qui gen double `ea' = (`r1'*`c1')/ `_NN'		// expected events in trt arm, i.e. E(a) where a = e1
					qui gen double `va' = `r1'*`r0'*`c1'*`c0'/( `_NN'*`_NN'*(`_NN' - 1))	// V(a) where a = e1
					qui gen double `oe' = `a' - `ea'										// O - E = a - E(a) where a = e1
					
					sreturn local oevlist `oe' `va'
				}
								
				// M-H or I-V method
				tempvar v
				if "`model'"!="mh" {
					tempvar r s
				}

				// calculate individual ORs and variances using cc-adjusted counts
				// (on the linear scale, i.e. logOR)
				qui gen double `r' = `e1_cont'*`f0_cont' / `t_cont'
				qui gen double `s' = `f1_cont'*`e0_cont' / `t_cont'
				qui gen double `v' = 1/`e1_cont' + 1/`f1_cont' + 1/`e0_cont' + 1/`f0_cont'
				
				qui replace `_ES'   = ln(`r'/`s') if `touse' & `_USE'==1
				qui replace `_seES' = sqrt(`v')   if `touse' & `_USE'==1
				
				// setup for Mantel-Haenszel method
				// N.B. Don't use cc-adjusted counts for M-H pooled estimate
				if "`model'"=="mh" {
					tempvar p q
					qui gen double `p'  = (`e1' + `f0')/`_NN'
					qui gen double `q'  = (`f1' + `e0')/`_NN'

					qui replace `r' = `e1'*`f0' / `_NN'
					qui replace `s' = `f1'*`e0' / `_NN'
					
					qui gen double `pr' = `p'*`r'
					qui gen double `ps' = `p'*`s'
					qui gen double `qr' = `q'*`r'
					qui gen double `qs' = `q'*`s'

					sreturn local mhvlist `r' `s' `pr' `ps' `qr' `qs'		// for M-H pooling
				}
			} 		/* end OR */
			
			// setup for RR/IRR/RRR 
			// else if inlist("`summstat'", "rr", "irr", "rrr") {
			// MODIFIED APR 2019 FOR v3.3: REMOVE REFERENCE TO IRR
			else if inlist("`summstat'", "rr", "rrr") {
				args r s p
				tempvar v
				
				qui gen double `r' = `e1_cont'*`r0_cont' / `t_cont'
				qui gen double `s' = `e0_cont'*`r1_cont' / `t_cont'
				qui gen double `v' = 1/`e1_cont' + 1/`e0_cont' - 1/`r1_cont' - 1/`r0_cont'
				qui replace `_ES'   = ln(`r'/`s') if `touse' & `_USE'==1		// logRR 
				qui replace `_seES' = sqrt(`v')   if `touse' & `_USE'==1		// selogRR
				
				// setup for Mantel-Haenszel method
				// N.B. Don't use cc-adjusted counts for M-H pooled estimate
				if "`model'"=="mh" {
					qui replace `r' = `e1'*`r0' / `_NN'
					qui replace `s' = `e0'*`r1' / `_NN'

					qui gen double `p' = `r1'*`r0'*(`e1' + `e0')/(`_NN'*`_NN') - `e1'*`e0'/`_NN'
					
					sreturn local mhvlist `tvlist'							// for M-H pooling
				}
			}
			
			// setup for RD
			else if "`summstat'" == "rd" {
				args rdwt rdnum vnum
				tempvar v
				
				// N.B. `_ES' is calculated *without* cc adjustment, to ensure 0/n1 vs 0/n2 really *is* RD=0
				qui gen double `v'  = `e1_cont'*`f1_cont'/(`r1_cont'^3) + `e0_cont'*`f0_cont'/(`r0_cont'^3)
				qui replace `_ES'   = `e1'/`r1' - `e0'/`r0' if `touse' & `_USE'==1
				qui replace `_seES' = sqrt(`v')             if `touse' & `_USE'==1

				// setup for Mantel-Haenszel method
				// N.B. `rdwt' and `rdnum' are calculated *without* cc adjustment, to ensure 0/n1 vs 0/n2 really *is* RD=0
				// N.B. Don't use cc-adjusted counts for M-H pooled estimate
				if "`model'"=="mh" {
					qui gen double `rdwt'  = `r1'*`r0'/ `_NN'
					qui gen double `rdnum' = (`e1'*`r0' - `e0'*`r1')/ `_NN'
					qui gen double `vnum'  = (`e1'*`f1'*(`r0'^3) + `e0'*`f0'*(`r1'^3)) /(`r1'*`r0'*`_NN'*`_NN')

					sreturn local mhvlist `tvlist'					// for M-H pooling
				}
			}		// end "rd"
		}		// end if `params' == 4
		
		else {
		
			cap assert `params' == 6
			if _rc {
				disp as err `"Invalid {it:varlist}"'
				exit 198
			}
		
			// N mean SD for continuous data
			assert inlist("`summstat'", "wmd", "cohen", "glass", "hedges")
			args n1 mean1 sd1 n0 mean0 sd0

			qui replace `_NN' = `n1' + `n0' if `touse'
				
			if "`summstat'" == "wmd" {
				qui replace `_ES'   = `mean1' - `mean0'                     if `touse' & `_USE'==1
				qui replace `_seES' = sqrt((`sd1'^2)/`n1' + (`sd0'^2)/`n0') if `touse' & `_USE'==1
			}
			else {				// summstat = SMD
				tempvar s
				qui gen double `s' = sqrt( ((`n1'-1)*(`sd1'^2) + (`n0'-1)*(`sd0'^2) )/( `_NN' - 2) )

				if "`summstat'" == "cohen" {
					qui replace `_ES'   = (`mean1' - `mean0')/`s'                                      if `touse' & `_USE'==1
					qui replace `_seES' = sqrt((`_NN' /(`n1'*`n0')) + (`_ES'*`_ES'/ (2*(`_NN' - 2)) )) if `touse' & `_USE'==1
				}
				else if "`summstat'" == "glass" {
					qui replace `_ES'   = (`mean1' - `mean0')/`sd0'                                    if `touse' & `_USE'==1
					qui replace `_seES' = sqrt(( `_NN' /(`n1'*`n0')) + (`_ES'*`_ES'/ (2*(`n0' - 1)) )) if `touse' & `_USE'==1
				}
				else if "`summstat'" == "hedges" {
					qui replace `_ES'   = (`mean1' - `mean0')*(1 - 3/(4*`_NN' - 9))/`s'                    if `touse' & `_USE'==1
					qui replace `_seES' = sqrt(( `_NN' /(`n1'*`n0')) + (`_ES'*`_ES'/ (2*(`_NN' - 3.94)) )) if `touse' & `_USE'==1
				}
			}			
		}		// end else (i.e. if `params' == 6)
	}		// end if `params' > 3
	
end
	
	


***************************************************************

** Extra loop for cumulative/influence meta-analysis
// - If cumulative, loop over observations one by one
// - If influence, exclude observations one by one

program define CumInfLoop, rclass

	syntax varlist(numeric min=3 max=7) [if] [in], SORTBY(varlist) ///
		MODEL(passthru) XOUTVLIST(varlist numeric) ///
		[CUmulative INFluence OVWt SGWt USE3(varname numeric) ROWNAMES(namelist) * ]
	
	marksample touse, novarlist
	gettoken _USE varlist : varlist
	
	qui count if `touse' & `_USE'==1
	if !r(N) exit 2000	
	
	local npts npts
	local rownames : list rownames - npts
	
	tokenize `xoutvlist'
	args `rownames' _WT2

	tempname critval
	tempvar obsj touse2
	qui bysort `touse' (`_USE' `sortby') : gen long `obsj' = _n if `touse'
	qui count if `touse'
	local jmax = r(N)
	local jmin = cond(`"`sgwt'`ovwt'"'!=`""', 1, `jmax')
	
	forvalues j = `jmin'/`jmax' {

		gen byte `touse2' = `touse' * (`_USE'==1)
	
		// Define `touse' for *input* (i.e. which obs to meta-analyse)
		if `"`cumulative'"'!=`""' qui replace `touse2' = `touse' * inrange(`obsj', 1, `j')		// cumulative: obs from 1 to `j'-1
		else                      qui replace `touse2' = `touse' * (`obsj' != `j')				// influence: all obs except `j'

		// If only one study, return `nsg' to prompt error message at the end of CumInfLoop
		// (N.B. first iteration of cumulative will *always* be a single study, so don't report error in that case)
		local pvlist `varlist'		// default
		qui count if `touse2'
		if r(N)==1 {
			if !(`"`cumulative'"'!=`""' & `j'==1 & `j'<`jmax') c_local nsg = 1
		}

		cap nois PerformPooling `pvlist' if `touse2', `model' `options'

		if _rc {
			if _rc==1 nois disp as err `"User break in {bf:metan.PerformPooling}"'
			else nois disp as err `"Error in {bf:metan.PerformPooling}"'
			c_local err noerr		// tell -metan- not to also report an "error in metan.CumInfLoop"
			exit _rc
		}
	
		// pooling failed (may not have caused an actual error)
		if missing(r(eff), r(se_eff), r(totwt)) exit 2002
		
		
		** Store statistics returned by PerformPooling in the dataset
		// Same statistics as in `rownames', plus (non-normalised) weights
		
		// First, re-define `touse2' for *output* (i.e. where to store the results of the meta-analysis)
		qui replace `touse2' = `touse' * (`obsj'==`j')

		// Store (non-normalised) weight in the dataset
		qui replace `_WT2' = r(totwt) if `touse2'
		
		// Store other returned statistics in the dataset
		foreach el in `rownames' {
			qui replace ``el'' = r(`el') if `touse2'
		}
				
		drop `touse2'	// tidying up

	}		// end forvalues j=`jmin'/`jmax'
	
	// cumulative: identifier of last estimate, for placement of dotted line in forestplot
	if `"`cumulative'"'!=`""' {
		qui replace `use3' = 1 if `touse' & `obsj'==`jmax'
	}
	
	// Return stats from final run of PerformPooling
	local k = r(k)
	return add
	return local xwt `_WT2'		// return name of `_WT2' in `xoutvlist'
		
	// Check consistency of numbers of *studies*
	qui count if `touse' & `_USE'==1
	local n = r(N)
	if `"`influence'"'!=`""' local --n	// if influence, number of studies will be one less than true number, by definition!
	assert `n' == `k'

	
end





*******************************************************************
	
* PerformPooling
// subroutine of PerformMetaAnalysis

// This routine actually performs the pooling itself.
// non-IV calculations are done in Stata (partly using code taken from metan.ado by Ross Harris et al);
//   iterative IV analyses are done in Mata.

// N.B. study-level results _ES, _seES, _LCI, _UCI are assumed *always* to be on the linear scale (i.e. logOR etc.)
// as this makes building the forestplot easier, and keeps things simple in general.
// For non-IV 2x2 count data, however, the exponentiated effect size may also be returned, e.g. r(OR), r(RR).

program define PerformPooling, rclass
	
	syntax varlist [if] [in], MODEL(name) [*]
	
	local nrfd = 0
	cap nois {
		if "`model'"=="mh" PerformPoolingMH `0'
		else PerformPoolingIV `0'
	}
	if _rc {
		c_local err noerr
		exit _rc
	}
	
	c_local nrfd = `nrfd'
	return add

end
	
	

// Inverse variance (i.e. all except Mantel-Haenszel)
program define PerformPoolingIV, rclass

	syntax varlist(numeric min=2 max=2) [if] [in], MODEL(name) ///
		[SUMMSTAT(name) TESTSTAT(name) HETOPT(name) ///
		OEVLIST(varlist numeric min=2 max=2) INVLIST(varlist numeric min=2 max=6) ///
		DF(varname numeric) NPTS(varname numeric) WGT(varname numeric) WTVAR(varname numeric) ///
		HKsj BArtlett SKovgaard RObust LOGRank noINTeger ///
		ISQSA(real 80) TSQSA(real -99) QE(varname numeric) INIT(name) LEVEL(real 95) TSQLEVEL(real 95) RFLEVEL(real 95) ///
		ITOL(real 1e-8) MAXTausq(real -9) REPS(real 1000) MAXITer(real 1000) QUADPTS(real 100) /*noTRUNCate*/ TRUNCate(string) EIM OIM * ]

	// N.B. extra options should just be those allowed for PerformPoolingMH

	marksample touse
	local pvlist `varlist'		// for clarity
	
	// if no wtvar, gen as tempvar
	if `"`wtvar'"'==`""' {
		local wtvar
		tempvar wtvar
		qui gen `wtvar' = .
	}	
	
	// Firstly, check whether only one study
	//   if so, cancel random-effects and t-critval
	qui count if `touse'
	if r(N)==1 {
		local model fe
		local t
	}
		
	
	** Average event rate (binary outcomes only)
	// (do this before any 0.5 adjustments or excluding 0-0 studies)
	local params : word count `invlist'
	if `params'==4 {
		tokenize `invlist'
		args e1 f1 e0 f0
	
		tempname e_sum tger cger
		summ `e1' if `touse', meanonly
		scalar `e_sum' = cond(r(N), r(sum), .)
		summ `f1' if `touse', meanonly
		scalar `tger' = cond(r(N), `e_sum'/(`e_sum' + `r(sum)'), .)
		return scalar tger = `tger'
		
		summ `e0' if `touse', meanonly
		scalar `e_sum' = cond(r(N), r(sum), .)
		summ `f0' if `touse', meanonly
		scalar `cger' = cond(r(N), `e_sum'/(`e_sum' + `r(sum)'), .)
		return scalar cger = `cger'
	}
	

	** Chi-squared test (OR only; includes Peto OR) or logrank HR
	if "`oevlist'"!="" | "`logrank'"!="" {
	
		if "`logrank'"!="" tokenize `invlist'
		else tokenize `oevlist'
		args oe va

		tempname OE VA chi2
		summ `oe' if `touse', meanonly
		scalar `OE' = cond(r(N), r(sum), .)
		summ `va' if `touse', meanonly
		scalar `VA' = cond(r(N), r(sum), .)
		scalar `chi2' = (`OE'^2 )/`VA'

		if "`model'"=="peto" | "`logrank'"!="" {
			return scalar OE = `OE'
			return scalar V = `VA'
			
			// Generate Peto _ES and _seES from OE and V
			if "`model'"=="peto" {
				tempvar _ES _seES
				qui gen double `_ES' = `oe'/`va' if `touse'
				qui gen double `_seES' = 1/sqrt(`va') if `touse'
			}
		}
	}


	*************************************
	* Standard inverse-variance methods *
	*************************************

	tempname k crit pvalue
	qui count if `touse'
	scalar `k' = r(N)
	
	// Converse of above code
	if "`model'"!="peto" {
		tokenize `pvlist'
		args _ES _seES
	}
	
	tempname eff se_eff
	qui replace `wtvar' = 1/`_seES'^2 if `touse'
	
	summ `_ES' [aw=`wtvar'] if `touse', meanonly
	scalar `eff' = r(mean)
	scalar `se_eff' = 1/sqrt(r(sum_w))		// fixed-effects SE

	// Derive Cochran's Q; will be returned as r(Qc), separately from "generic" r(Q) (which may contain e.g. Breslow-Day)	
	tempvar qhet
	qui gen double `qhet' = `wtvar'*((`_ES' - `eff')^2)
	summ `qhet' if `touse', meanonly

	tempname Q Qdf
	if r(N)>1 {
		scalar `Q' = r(sum)
		scalar `Qdf' = r(N) - 1
	}
	else {
		scalar `Q' = .
		scalar `Qdf' = 0
	}
		
	tempname c sigmasq tausq
	summ `wtvar' [aw=`wtvar'] if `touse', meanonly
	scalar `c' = r(sum_w) - r(mean)
	scalar `sigmasq' = `Qdf'/`c'						// [general note: can this be generalised to other (non-IV) methods?]
	scalar `tausq' = max(0, (`Q' - `Qdf')/`c')			// default: D+L estimator
		

	
	**********************************
	* Non-iterative tausq estimators *
	**********************************
	// (other than D+L, already derived above)
	
	** Setup two-stage estimators sj2s and dk2s
	// consider *initial* estimate of tsq
	if inlist("`model'", "sj2s", "dk2s") {
		local final `model'
		local model `"`init'"'
		
		if substr(trim(`"`model'"'), 1, 2)==`"sa"' {
			tempname tausq0
			scalar `tausq0' = `tausq'
		
			_parse comma model 0 : model
			syntax [, ISQ(string) TAUSQ(string)]
			
			if `"`tausq'"'!=`""' & `"`isq'"'==`""' {
				nois disp as err `"Only one of {bf:isq()} or {bf:tausq()} may be supplied as suboptions to {bf:sa()}"'
				exit 184
			}				
		
			else if `"`tausq'"'!=`""' {
				cap confirm number `tausq'
				if _rc {
					disp as err `"Error in {bf:tausq()} suboption to {bf:sa()}; a single number was expected"'
					exit _rc
				}
				if `tausq'<0 {
					nois disp as err `"tau{c 178} value for sensitivity analysis cannot be negative"'
					exit 198
				}
				local tsqsa = `tausq'
				local isqsa
			}
			else {
				if `"`isq'"'==`""' local isq = 80
				else {
					cap confirm number `isq'
					if _rc {
						disp as err `"Error in {bf:isq()} suboption to {bf:sa()}; a single number was expected"'
						exit _rc
					}
					if `isq'<0 | `isq'>=100 {
						nois disp as err `"I{c 178} value for sensitivity analysis must be at least 0% and less than 100%"'
						exit 198
					}
				}
				local isqsa = `isq'
				local tsqsa = -99
			}
			
			tempname tausq
			scalar `tausq' = `tausq0'
		}
	}		
	
	** Hartung-Makambi estimator (>0)
	if "`model'"=="hm" {
		scalar `tausq' = `Q'^2 / (`c'*(`Q' + 2*`Qdf'))
	}
			
	** Non-iterative, making use of the sampling variance of _ES
	else if inlist("`model'", "ev", "vc", "b0", "bp") {
		tempvar residsq v
		tempname var_eff meanv
		
		qui summ `_ES' if `touse'
		qui gen double `residsq' = (`_ES' - r(mean))^2
		scalar `var_eff' = r(Var)
		
		qui gen double `v' = `_seES'^2
		summ `v' if `touse', meanonly
		scalar `meanv' = r(mean)
		
		// empirical variance (>0)
		if "`model'"=="ev" {
			summ `residsq', meanonly
			scalar `tausq' = r(sum)/r(N)
		}
		
		// "variance component" aka Cochran ANOVA-type estimator aka Hedges
		if "`model'"=="vc" scalar `tausq' = `var_eff' - `meanv'
		
		// Rukhin Bayes estimators
		else if inlist("`model'", "b0", "bp") {
			scalar `tausq' = `var_eff'*(`k' - 1)/(`k' + 1)
			if "`model'"=="b0" {
				summ `npts' if `touse', meanonly	
				scalar `tausq' = `tausq' - ( (`r(sum)' - `k')*`Qdf'*`meanv'/((`k' + 1)*(`r(sum)' - `k' + 2)) )
			}
		}
		scalar `tausq' = max(0, `tausq')			// truncate at zero
	}
	
	// Sensitivity analysis: use given Isq/tausq and sigmasq to generate tausq/Isq
	else if "`model'"=="sa" {
		if `tsqsa'==-99 scalar `tausq' = `isqsa'*`sigmasq'/(100 - `isqsa')
		else scalar `tausq' = `tsqsa'
	}
	
	

	******************************
	* Iterative tausq estimators *
	******************************
	
	// Check validity of iteropts
	cap assert (`maxtausq'>=0 & !missing(`maxtausq')) | `maxtausq'==-9
	if _rc {
		disp as err "maxtausq() cannot be negative"
		exit 198
	}			
	cap assert `itol'>=0 & !missing(`itol')
	if _rc {
		disp as err "itol() cannot be negative"
		exit 198
	}
	cap {
		assert (`maxiter'>0 & !missing(`maxiter'))
		assert round(`maxiter')==`maxiter'
	}
	if _rc {
		disp as err "maxiter() must be an integer greater than zero"
		exit 198
	}

	// maxtausq: use 10*`tausq' if not specified
	// (and 10 times that for uci -- done in Mata)
	local maxtausq = cond(`maxtausq'==-9, max(10*`tausq', 100), `maxtausq')
		
	// Iterative, using Mata
	if inlist("`model'", "dlb", "mp", "ml", "pl", "reml", "kr") {
	
		// Bootstrap D+L
		// (Kontopantelis PLoS ONE 2013)
		if "`model'"=="dlb" {
			cap {
				assert (`reps'>0 & !missing(`reps'))
				assert round(`reps')==`reps'
			}
			if _rc {
				disp as err "reps() must be an integer greater than zero"
				exit 198
			}
			cap nois mata: DLb("`_ES' `_seES'", "`touse'", `level', `reps')
		}
		
		// Mandel-Paule aka empirical Bayes
		// (DerSimonian and Kacker CCT 2007)				
		// N.B. Mata routine also performs the Viechtbauer Q-profiling routine for tausq CI
		// (Viechtbauer Stat Med 2007; 26: 37-52)
		else if "`model'"=="mp" {
			cap nois mata: GenQ("`_ES' `_seES'", "`touse'", `tsqlevel', (`maxtausq', `itol', `maxiter'))
		}
		
		// REML
		// N.B. Mata routine also performs likelihood profiling to give tausq CI
		else if inlist("`model'", "reml", "kr") {
			cap nois mata: REML("`_ES' `_seES'", "`touse'", `tsqlevel', (`maxtausq', `itol', `maxiter'))
			return scalar tsq_var = r(tsq_var)
			return scalar ll = r(ll)
		}
		
		// ML, including Profile Likelihood
		// with optional Bartlett's (Huizenga Br J Math Stat Psychol 2011) or Skovgaard's (Guolo Stat Med 2012) correction to the likelihood
		// N.B. Mata routine also performs likelihood profiling to give tausq CI
		else if inlist("`model'", "ml", "pl") {
			local mlpl `model'
			if "`bartlett'"!="" local mlpl plbart
			else if "`skovgaard'"!="" local mlpl plskov
			cap nois mata: MLPL("`_ES' `_seES'", "`touse'", (`level', `tsqlevel'), (`maxtausq', `itol', `maxiter'), "`mlpl'")
			return scalar tsq_var = r(tsq_var)

			if "`model'"=="pl" {
				return scalar eff_lci = r(eff_lci)
				return scalar eff_uci = r(eff_uci)
				return scalar rc_eff_lci = r(rc_eff_lci)
				return scalar rc_eff_uci = r(rc_eff_uci)
				
				// Need to store these as scalars, in order to calculate critical values
				tempname chi2 z
				scalar `chi2' = r(lr)			// Likelihood ratio test statistic
				scalar `z'    = r(sll)			// Signed log-likelihood test statistic
				return scalar ll   = r(ll)		// Log-likelihood

				if "`teststat'"=="chi2" return scalar chi2 = r(lr)		// Bartlett's correction to the likelihood
				else                    return scalar z    = r(sll)		// Skovgaard's correction to the likelihood
			}
		}
		
		if _rc {
			if _rc==1 exit _rc
			else if _rc>=3000 {
				nois disp as err "Mata compile-time or run-time error"
				exit _rc
			}
			else if _rc nois disp as err "Error(s) detected during running of Mata code; please check output"
		}

		scalar `tausq' = r(tausq)

		// check tausq limits and set to missing if necessary
		tempname tsq_lci tsq_uci
		scalar `tsq_lci' = r(tsq_lci)
		scalar `tsq_uci' = r(tsq_uci)
		if "`model'"!="dlb" {
			scalar `tsq_lci' = cond(r(rc_tsq_lci)>1 & r(tsq_lci)!=0, ., r(tsq_lci))
			scalar `tsq_uci' = cond(r(rc_tsq_uci)>1, ., r(tsq_uci))
		}
		
		// return extra scalars
		return scalar maxtausq = `maxtausq'
		return scalar tsq_lci  = `tsq_lci'
		return scalar tsq_uci  = `tsq_uci'
		return scalar rc_tausq   = r(rc_tausq)
		return scalar rc_tsq_lci = r(rc_tsq_lci)
		return scalar rc_tsq_uci = r(rc_tsq_uci)			
		
	}	// end if inlist("`model'", "dlb", "mp", "ml", "pl", "reml")
	
	// Viechtbauer Q-profiling routine for tausq CI, if *not* Mandel-Paule tsq estimator
	// (Viechtbauer Stat Med 2007; 26: 37-52)
	if "`hetopt'"=="qprofile" & "`model'"!="mp" {
		cap nois mata: GenQ("`_ES' `_seES'", "`touse'", `tsqlevel', (`maxtausq', `itol', `maxiter'))
		
		if _rc {
			if _rc==1 exit _rc
			else if _rc>=3000 {
				nois disp as err "Mata compile-time or run-time error"
				exit _rc
			}
			else if _rc nois disp as err "Error(s) detected during running of Mata code; please check output"
		}				
	
		tempname tsq_lci tsq_uci
		scalar `tsq_lci' = cond(r(rc_tsq_lci)>1 & r(tsq_lci)!=0, ., r(tsq_lci))
		scalar `tsq_uci' = cond(r(rc_tsq_uci)>1, ., r(tsq_uci))
		
		// return extra scalars
		return scalar tsq_lci  = `tsq_lci'
		return scalar tsq_uci  = `tsq_uci'
		return scalar rc_tsq_lci = r(rc_tsq_lci)
		return scalar rc_tsq_uci = r(rc_tsq_uci)
	
	}
	// end of "Iterative, using Mata" section

	

	******************************************************
	* User-defined weights; finalise two-step estimators *
	******************************************************

	if `"`wgt'"'!=`""' {
		qui replace `wtvar' = `wgt' if `touse'
	}
	
	if "`final'"!="" {
		local model `final'
		
		tempname Qr0
		qui replace `qhet' = ((`_ES' - `eff')^2)/((`_seES'^2) + `tausq')
		summ `qhet' if `touse', meanonly
		scalar `Qr0' = r(sum)
		
		if "`final'"=="sj2s" {					// two-step Sidik-Jonkman
			// scalar `tausq' = cond(`tausq'==0, `sigmasq'/99, `tausq') * `Qr0'/`Qdf'		// March 2018: if tsq=0, use Isq=1%
			scalar `tausq' = `tausq' * `Qr0'/`Qdf'
			
			/*
			// Sidik-Jonkman's suggested confidence interval for tausq; not recommended for use
			if "`hetopt'"=="qprofile" {
				tempname tsq_lci tsq_uci
				scalar `tsq_lci' = `tausq' * `Qdf' / invchi2(`Qdf', .5 - `level'/200)
				scalar `tsq_uci' = `tausq' * `Qdf' / invchi2(`Qdf', .5 + `level'/200)
			}
			*/
		}
		else if "`final'"=="dk2s" {				// two-step DerSimonian-Kacker (MM only)
			tempname wi1 wi2 wis1 wis2 
			summ `wtvar' if `touse', meanonly
			scalar `wi1' = r(sum)				// sum of weights
			summ `wtvar' [aw=`wtvar'] if `touse', meanonly
			scalar `wi2' = r(sum)				// sum of squared weights				
			summ `wtvar' [aw=`_seES'^2] if `touse', meanonly
			scalar `wis1' = r(sum)				// sum of weight * variance
			summ `wtvar' [aw=`wtvar' * (`_seES'^2)] if `touse', meanonly
			scalar `wis2' = r(sum)				// sum of squared weight * variance
			
			scalar `tausq' = (`Qr0' - (`wis1' - `wis2'/`wi1')) / (`wi1' - `wi2'/`wi1')
			scalar `tausq' = max(0, `tausq')	// truncate at zero
		}
	}	

	

	*********************************
	* Alternative weighting schemes *
	*********************************
	// (not user-defined)
	
	// Quality effects (QE) model (extension of IVHet to incorporate quality scores)
	// (Doi et al, Contemporary Clinical Trials 2015; 45: 123-9)
	if "`model'"=="qe" {
		tempvar newqe tauqe
		
		// re-scale scores relative to highest value
		summ `qe' if `touse', meanonly
		qui gen double `newqe' = `qe'/r(max)

		// taui and tauhati
		qui gen double `tauqe' = (1 - `newqe')/(`_seES'*`_seES'*`Qdf')
		summ `tauqe' if `touse', meanonly
		local sumtauqe = r(sum)

		summ `newqe' if `touse', meanonly
		if r(min) < 1 {				// additional correction if any `newqe' are < 1, to avoid neg. weights
			tempvar newqe_adj
			qui gen double `newqe_adj' = `newqe' + r(sum)*`tauqe'/(`sumtauqe'*`Qdf')
			summ `newqe_adj' if `touse', meanonly
			qui replace `tauqe' = (`sumtauqe'*`k'*`newqe_adj'/r(sum)) - `tauqe'
		}
		else qui replace `tauqe' = (`sumtauqe'*`k'*`newqe'/r(sum)) - `tauqe'
		
		// Point estimate uses weights = qi/vi + tauhati
		qui replace `wtvar' = (`newqe'/(`_seES'^2)) + `tauqe' if `touse'
	}
	
	// Biggerstaff and Tweedie approximate Gamma-based weighting
	// (also derives a variance and confidence interval for tausq_DL)
	else if "`model'"=="gamma" {
		cap nois mata: Gamma("`_ES' `_seES'", "`touse'", "`wtvar'", `tsqlevel', (`maxtausq', `itol', `maxiter', `quadpts'))
		if _rc {
			if _rc==1 exit _rc
			else if _rc>=3000 {
				nois disp as err "Mata compile-time or run-time error"
				exit _rc
			}
			else if _rc nois disp as err "Error(s) detected during running of Mata code; please check output"
		}
		
		// check tausq limits and set to missing if necessary
		tempname tsq_lci tsq_uci
		scalar `tsq_lci' = r(tsq_lci)
		scalar `tsq_uci' = r(tsq_uci)
		scalar `tsq_lci' = cond(r(rc_tsq_lci)>1 & `tsq_lci'!=0, ., `tsq_lci')
		scalar `tsq_uci' = cond(r(rc_tsq_uci)>1, ., `tsq_uci')
	
		// return extra scalars
		return scalar maxtausq = `maxtausq'
		return scalar rc_tausq = r(rc_tausq)
		return scalar tsq_var = r(tsq_var)
		
		if "`hetopt'"=="qprofile" {
			return scalar tsq_lci  = `tsq_lci'
			return scalar tsq_uci  = `tsq_uci'
			return scalar rc_tsq_lci = r(rc_tsq_lci)
			return scalar rc_tsq_uci = r(rc_tsq_uci)
		}
	}
	
	// Henmi and Copas method also belongs here
	//  (Henmi and Copas, Stat Med 2010; DOI: 10.1002/sim.4029)
	// Begins along the same lines as IVHet; that is, a RE model with inv-variance weighting
	//   but goes on to estimate the distribution of pivotal quantity U using a Gamma distribution (c.f. Biggerstaff & Tweedie).
	// `se_eff' is the same as IVHet, but conf. interval around `eff' is different.
	else if "`model'"=="hc" {
		cap nois mata: HC("`_ES' `_seES'", "`touse'", `level', (`itol', `maxiter', `quadpts'))
		if _rc {
			if _rc==1 exit _rc
			else if _rc>=3000 {
				nois disp as err "Mata compile-time or run-time error"
				exit _rc
			}
			else if _rc nois disp as err "Error(s) detected during running of Mata code; please check output"
		}
		
		return scalar u = r(u)
		scalar `crit'   = r(crit)
		scalar `pvalue' = r(p)
	}

	// end of "Alternative weighting schemes" section
	
	

	**********************************
	* Generate pooled eff and se_eff *
	**********************************
	
	// Alternative or user-defined weighting
	if `"`wgt'"'!=`""' | inlist("`model'", "ivhet", "qe", "gamma", "hc") {

		// Apply weighting
		summ `_ES' [aw=`wtvar'] if `touse', meanonly
		scalar `eff' = r(mean)
		
		// Specify underlying model: fixed-effects, or random-effects with additive heterogeneity
		// (N.B. if *multiplicative* heterogeneity, factor simply multiplies the final pooled variance)
		local vi = cond("`model'"=="fe", "`_seES'^2", "`_seES'^2 + `tausq'")
		
		tempvar wtvce
		summ `wtvar' if `touse', meanonly
		qui gen double `wtvce' = (`vi') * `wtvar'^2 / r(sum)^2
		summ `wtvce' if `touse', meanonly
		scalar `se_eff' = sqrt(r(sum))
	}	
	
	// Standard weighting based on additive tau-squared
	// (N.B. if fe or mu, eff and se_eff have already been calculated)
	else if !inlist("`model'", "fe", "mu") {
		qui replace `wtvar' = 1/(`_seES'^2 + `tausq') if `touse'
		summ `_ES' [aw=`wtvar'] if `touse', meanonly
		scalar `eff' = r(mean)
		scalar `se_eff' = 1/sqrt(r(sum_w))
	}
	
	// Return weights for CumInfLoop
	summ `wtvar' if `touse', meanonly
	return scalar totwt = cond(r(N), r(sum), .)		// sum of (non-normalised) weights

	

	*********************************
	* Post-hoc variance corrections *
	*********************************	

	// First, calculate "generalised" (i.e. random-effects) version of Cochran's Q
	// (needed for HKSJ, so do it now)
	local Qr `Q'
	if !inlist("`model'", "fe", "mu") | "`wgt'"!="" {		// if fe, Qr = Q
		tempname Qr
		qui replace `qhet' = `wtvar'*((`_ES' - `eff')^2)
		summ `qhet' if `touse', meanonly
		scalar `Qr' = cond(r(N), r(sum), .)
		return scalar Qr = `Qr'
	}
	
	// Multiplicative heterogeneity (e.g. Thompson and Sharp, Stat Med 1999)
	// (equivalent to the "full variance" estimator suggested by Sandercock
	// (https://metasurv.wordpress.com/2013/04/26/
	//    fixed-or-random-effects-how-about-the-full-variance-model-resolving-a-decades-old-bunfight)
	
	// N.B. the multiplier sqrt(`Q'/`Qdf') is equal to Higgins & Thompson's (Stat Med 2002) `H' statistic

	// Hartung-Knapp-Sidik-Jonkman variance estimator
	// (Roever et al, BMC Med Res Methodol 2015; Jackson et al, Stat Med 2017; van Aert & Jackson, Stat Med 2018)
	
	if "`model'"=="mu" | "`hksj'"!="" {
		tempname H
		scalar `H' = sqrt(`Qr'/`Qdf')
		
		// (e.g.) Roever 2015: truncate at 1
		// i.e. don't use if *under* dispersion present
		if inlist(`"`truncate'"', `"one"', `"1"') scalar `H' = max(1, `H')
		
		// van Aert & Jackson 2019: truncate at z/t
		// N.B. van Aert & Jackson use H* to refer to a "generalised/random-effects" H-statistic, similar to Qr
		else if `"`truncate'"'==`"zovert"' {
			tempname tcrit zcrit
			scalar `zcrit' = invnormal(.5 + `level'/200)
			scalar `tcrit' = invttail(`Qdf', .5 - `level'/200)
			scalar `H' = max(`zcrit'/`tcrit', `H')
		}
		else if `"`truncate'"'!=`""' {
			disp as err `"invalid use of {bf:truncate()} option"'
			exit 184
		}
		
		scalar `se_eff' = `se_eff' * `H'
	}

	// Sidik-Jonkman robust ("sandwich-like") variance estimator
	// (Sidik and Jonkman, Comp Stat Data Analysis 2006)
	// (N.B. HKSJ estimator also described in the same paper)
	else if "`robust'"!="" {
		tempname sumwi
		tempvar vr_part
		summ `wtvar' if `touse', meanonly
		scalar `sumwi' = r(sum)
		qui gen double `vr_part' = `wtvar' * `wtvar' * ((`_ES' - `eff')^2) / (1 - (`wtvar'/`sumwi'))
		summ `vr_part' if `touse', meanonly
		scalar `se_eff' = sqrt(r(sum))/`sumwi'
	}

	// Kenward-Roger variance inflation method
	// (Morris et al, Stat Med 2018)
	else if "`model'"=="kr" {
		tempname wi1 wi2 wi3 nwi2 nwi3
		summ `wtvar' if `touse', meanonly
		scalar `wi1' = r(sum)				// sum of weights
		summ `wtvar' [aw=`wtvar'] if `touse', meanonly
		scalar `wi2' = r(sum)				// sum of squared weights
		summ `wtvar' [aw=`wtvar'^2] if `touse', meanonly
		scalar `wi3' = r(sum)				// sum of cubed weights
		scalar `nwi2' = `wi2'/`wi1'			// "normalised" sum of squared weights [i.e. sum(wi:^2)/sum(wi)]
		scalar `nwi3' = `wi3'/`wi1'			// "normalised" sum of cubed weights [i.e. sum(wi:^3)/sum(wi)]		
			
		// expected information
		tempname I
		scalar `I' = `wi2'/2 - `nwi3' + (`nwi2'^2)/2
		
		// observed information
		if "`oim'"!="" {
			tempvar resid resid2
			tempname q2 q3
			
			qui gen double `resid' = `_ES' - `eff'
			summ `resid' [aw=`wtvar'^2] if `touse', meanonly
			scalar `q2' = r(sum)			// quadratic involving squared weights and residual
			
			qui gen double `resid2' = `resid'^2
			summ `resid2' [aw=`wtvar'^3] if `touse', meanonly
			scalar `q3' = r(sum)			// quadratic involving cubed weights and squared residual
			
			scalar `I' = max(0, (`q2'^2)/`wi1' + `q3' - `I')
		}
		
		// corrected se_eff [sqrt(Phi_A) in Kenward-Roger papers]
		tempname W V
		scalar `W' = 1/`I'		// approximation of var(tausq)
		scalar `V' = (1/`wi1') + 2*`W'*(`wi3' - (`wi2'^2)/`wi1')/(`wi1'^2)
		scalar `se_eff' = sqrt(`V')
		
		// denominator degrees of freedom
		tempname A df_kr
		scalar `A' = `W' * (`V'*`wi2')^2
		scalar `df_kr' = 2 / `A'
		return scalar df_kr = `df_kr'
	}
	
	// check for successful pooling
	if missing(`eff', `se_eff') exit 2002
	
	

	**********************************************
	* Critical values, test statistics, p-values *
	**********************************************
	
	if "`model'"=="pl" {				// N.B. PL confidence limits have already been calculated
		if "`teststat'"=="chi2" {
			scalar `crit' = invchi2(1, `level'/100)
			scalar `pvalue' = chi2tail(1, `chi2')
		}
		else {
			scalar `crit' = invnormal(.5 + `level'/200)
			scalar `pvalue' = 2*normal(-abs(`z'))
		}
	}
	else {
		if "`oevlist'"!="" | "`logrank'"!="" {
			scalar `crit' = invchi2(1, `level'/100)
			scalar `pvalue' = chi2tail(1, `chi2')
			return scalar chi2 = `chi2'
		}
		else if "`model'"=="kr" {
			tempname t
			scalar `crit' = invttail(`df_kr', .5 - `level'/200)
			scalar `t' = `eff'/`se_eff'
			scalar `pvalue' = 2*ttail(`df_kr', abs(`t'))
			return scalar t = `t'
		}
		else if "`teststat'"=="t" {
			tempname t
			scalar `crit' = invttail(`Qdf', .5 - `level'/200)
			scalar `t' = `eff'/`se_eff'
			scalar `pvalue' = 2*ttail(`Qdf', abs(`eff'/`se_eff'))
			return scalar t = `t'
		}
		else if "`model'"!="hc" {		// N.B. HC crit + p-value have already been calculated
			tempname z
			scalar `crit' = invnormal(.5 + `level'/200)
			scalar `z' = `eff'/`se_eff'
			scalar `pvalue' = 2*normal(-abs(`z'))
			return scalar z = `z'
		}
		
		// Confidence intervals
		if "`oevlist'"!="" | "`logrank'"!="" {		// crit.value is chi2, but CI is based on z
			return scalar eff_lci = `eff' - invnormal(.5 + `level'/200) * `se_eff'
			return scalar eff_uci = `eff' + invnormal(.5 + `level'/200) * `se_eff'
		}
		else {								// else we can use crit.value (z or t, or u if HC)
			return scalar eff_lci = `eff' - `crit' * `se_eff'
			return scalar eff_uci = `eff' + `crit' * `se_eff'
		}
	}

	// return scalars
	return scalar eff = `eff'
	return scalar se_eff = `se_eff'
	return scalar crit = `crit'
	return scalar pvalue = `pvalue'

	

	*****************************************
	* Derive other heterogeneity statistics *
	*****************************************
	// e.g. H, I-squared and (modified) H-squared; plus test-based confidence intervals
	
	// Sensitivity analysis
	if "`model'"=="sa" {
		tempname H Isqval HsqM
		if `tsqsa' == -99 {
			scalar `H' = sqrt(1 / (1 - `isqsa'))
			scalar `Isqval' = `isqsa'
			scalar `HsqM' = `isqsa'/(100 - `isqsa')
			return scalar H = `H'
			return scalar Isq = `Isqval'
			return scalar HsqM = float(`HsqM')			// If user-defined I^2 is a round(ish) number, so should H^2 be
		}
		else {
			scalar `H' = sqrt((`tsqsa' + `sigmasq') / `sigmasq')
			scalar `Isqval' = 100*`tsqsa'/(`tsqsa' + `sigmasq')
			scalar `HsqM' = `tsqsa'/`sigmasq'
			return scalar H = `H'
			return scalar Isq = `Isqval'
			return scalar HsqM = `HsqM'
		}
	}
	
	else {	
		cap nois Heterogi `Q' `Qdf' if `touse', hetopt(`hetopt') ///
			se(`_seES') tausq(`tausq' `tsq_lci' `tsq_uci') level(`tsqlevel')
		
		if _rc {
			if _rc==1 nois disp as err `"User break in {bf:metan.Heterogi}"'
			else nois disp as err `"Error in {bf:metan.Heterogi}"'
			c_local err noerr		// tell -metan- not to also report an "error in metan.PerformPoolingIV"
			exit _rc
		}		

		return add
	}

	
	// Return other scalars
	return scalar k   = `k'				// k = number of studies (= count if `touse')
	return scalar Q   = `Q'				// Cochran's Q heterogeneity statistic
	return scalar Qdf = `Qdf'			// Q degrees of freedom (= `k' - 1)
	return scalar sigmasq = `sigmasq'	// "typical" within-study variance (Higgins & Thompson)
	return scalar tausq = `tausq'		// between-study heterogeneity variance

	// Prediction intervals
	// (uses k-2 df, c.f. Higgins & Thompson 2009; but also see e.g. http://www.metafor-project.org/doku.php/faq#for_random-effects_models_fitt)
	if `k' < 3 c_local nrfd = 1		// tell PerformMetaAnalysis to display error
	else {
		tempname rfcritval
		scalar `rfcritval' = invttail(`k'-2, .5 - `rflevel'/200)
		return scalar rflci = `eff' - `rfcritval' * sqrt(`tausq' + `se_eff'^2)
		return scalar rfuci = `eff' + `rfcritval' * sqrt(`tausq' + `se_eff'^2)
	}

end




** Mantel-Haenszel methods (binary outcomes only)
program define PerformPoolingMH, rclass

	syntax varlist(numeric min=2 max=2) [if] [in], ///
		MODEL(name) [ SUMMSTAT(name) TESTSTAT(name) HETOPT(name) ///
		MHVLIST(varlist numeric min=3 max=6) OEVLIST(varlist numeric min=2 max=2) INVLIST(varlist numeric min=4 max=4) ///
		CMHNocc noINTeger WTVAR(varname numeric) LEVEL(real 95) * ]

	// N.B. extra options should just be those allowed for PerformPoolingIV
	
	marksample touse
	local qvlist `varlist'		// for heterogeneity
	
	// if no wtvar, gen as tempvar
	if `"`wtvar'"'==`""' {
		local wtvar
		tempvar wtvar
		qui gen `wtvar' = .
	}	
	
	
	** Average event rate (binary outcomes only)
	// (do this before any 0.5 adjustments or excluding 0-0 studies)
	tokenize `invlist'
	args e1 f1 e0 f0

	tempname e_sum tger cger
	summ `e1' if `touse', meanonly
	scalar `e_sum' = cond(r(N), r(sum), .)
	summ `f1' if `touse', meanonly
	scalar `tger' = cond(r(N), `e_sum'/(`e_sum' + `r(sum)'), .)
	return scalar tger = `tger'
	
	summ `e0' if `touse', meanonly
	scalar `e_sum' = cond(r(N), r(sum), .)
	summ `f0' if `touse', meanonly
	scalar `cger' = cond(r(N), `e_sum'/(`e_sum' + `r(sum)'), .)
	return scalar cger = `cger'

		
	** Mantel-Haenszel OR
	tokenize `mhvlist'
	tempvar qhet
	tempname Q Qdf
	
	if "`summstat'"=="or" {
		args r s pr ps qr qs
		
		tempname R S OR eff
		summ `r' if `touse', meanonly
		scalar `R' = cond(r(N), r(sum), .)
		summ `s' if `touse', meanonly
		scalar `S' = cond(r(N), r(sum), .)
		
		scalar `OR'  = `R'/`S'
		scalar `eff' = ln(`OR')
			
		tempname PR PS QR QS se_eff
		summ `pr' if `touse', meanonly
		scalar `PR' = cond(r(N), r(sum), .)
		summ `ps' if `touse', meanonly
		scalar `PS' = cond(r(N), r(sum), .)
		summ `qr' if `touse', meanonly
		scalar `QR' = cond(r(N), r(sum), .)
		summ `qs' if `touse', meanonly
		scalar `QS' = cond(r(N), r(sum), .)
		
		// selogOR
		scalar `se_eff' = sqrt( (`PR'/(`R'*`R') + (`PS'+`QR')/(`R'*`S') + `QS'/(`S'*`S')) /2 )

		// check for successful pooling
		if missing(`eff', `se_eff') exit 2002
		
		// return scalars
		return scalar OR = `OR'
		return scalar eff = `eff'
		return scalar se_eff = `se_eff'
		
		// weight
		qui replace `wtvar' = `s' if `touse'
		

		* Breslow-Day heterogeneity (M-H Odds Ratios only)
		// (Breslow NE, Day NE. Statistical Methods in Cancer Research: Vol. I - The Analysis of Case-Control Studies.
		//  Lyon: International Agency for Research on Cancer 1980)
		if inlist("`hetopt'", "breslow", "tarone") {		
			local type = cond("`integer'"=="", "long", "double")
			tempvar r1 r0 c1 c0 n
			qui gen `type' `r1' = `e1' + `f1'		// total in research arm (= a + b)
			qui gen `type' `r0' = `e0' + `f0'		// total in control arm (= c + d)
			qui gen `type' `c1' = `e1' + `e0'		// total events (= a + c)
			qui gen `type' `c0' = `f1' + `f0'		// total non-events (= b + d)				
			qui gen `type' `n'  = `r1' + `r0'		// overall total
			
			tempvar bfit cfit dfit
			tempvar sterm cterm root
			qui gen double `sterm' = `r0' - `c1' + `OR'*(`r1' + `c1')
			qui gen double `cterm' = -`OR'*`c1'*`r1'
			qui gen double `root' = (-`sterm' + sqrt(`sterm'*`sterm' - 4*(1 - `OR')*`cterm'))/(2*(1 - `OR'))
			if missing(`root') {
				assert abs(`OR' - 1) < 0.0001
				tempvar afit
				qui gen double afit = `r1'*`c1'/ `n'
				qui gen double bfit = `r1'*`c0'/ `n'
				qui gen double cfit = `r0'*`c1'/ `n'
				qui gen double dfit = `r0'*`c0'/ `n'
			}
			else {
				if `root' < 0 | `root' > `c1' | `root' > `r1' {
					replace `root' = (-`sterm' - sqrt(`sterm'*`sterm' - 4*(1 - `OR')*`cterm'))/(2*(1 - `OR'))				
					if `root' < 0 | `root' > `c1' | `root' > `r1' {
						replace `root' = .
					}
				}
				local afit `root'
				qui gen double `bfit' = `r1' - `afit'
				qui gen double `cfit' = `c1' - `afit'
				qui gen double `dfit' = `r0' - `cfit'
			}
			drop `sterm' `cterm'
			
			qui gen double `qhet' = ((`e1' - `afit')^2) * ((1/`afit') + (1/`bfit') + (1/`cfit') + (1/`dfit'))
			summ `qhet' if `touse', meanonly
			if r(N)>1 {
				scalar `Q' = r(sum)
				scalar `Qdf' = r(N) - 1
			}
		
			// Tarone correction to Breslow-Day statistic
			if "`hetopt'"=="tarone" {
				tempvar tarone_num tarone_denom
				qui gen double `tarone_num' = `e1' - `afit'
				summ `tarone_num' if `touse', meanonly
				local tsum = r(sum)
				qui gen double `tarone_denom' = 1/((1/`afit') + (1/`bfit') + (1/`cfit') + (1/`dfit'))
				summ `tarone_denom' if `touse', meanonly
				scalar `Q' = `Q' - (`tsum')^2 / r(sum)
				drop `tarone_num' `tarone_denom'
			}
			drop `qhet' `afit' `bfit' `cfit' `dfit'  `qhet'			
		}
		
		
		* Cochran-Mantel-Haenszel test
		if "`teststat'"=="chi2" {
			tokenize `oevlist'
			args oe va

			tempname OE VA
			summ `oe' if `touse', meanonly
			scalar `OE' = cond(r(N), r(sum), .)
			summ `va' if `touse', meanonly
			scalar `VA' = cond(r(N), r(sum), .)
		
			tempname chi2 crit pvalue
			scalar `chi2' = ((abs(`OE') - cond("`cmhnocc'"!="", 0, 0.5))^2 ) / `VA'
			scalar `crit' = invchi2(1, `level'/100)
			scalar `pvalue' = chi2tail(1, `chi2')
			
			return scalar `teststat' = `chi2'
			return scalar crit = `crit'
			return scalar pvalue = `pvalue'
		}		
	}		// end M-H OR

	// Mantel-Haenszel RR/IRR/RRR
	// else if inlist("`summstat'", "rr", "irr", "rrr") {
	// MODIFIED APR 2019 FOR v3.3: REMOVE REFERENCE TO IRR
	else if inlist("`summstat'", "rr", "rrr") {
		args r s p

		tempname R S RR eff
		summ `r' if `touse', meanonly
		scalar `R' = cond(r(N), r(sum), .)
		summ `s' if `touse', meanonly
		scalar `S' = cond(r(N), r(sum), .)

		scalar `RR'  = `R'/`S'
		scalar `eff' = ln(`RR')
			
		tempname P se_eff
		summ `p' if `touse', meanonly
		scalar `P' = cond(r(N), r(sum), .)
		
		// selogRR
		scalar `se_eff' = sqrt(`P'/(`R'*`S'))
		
		// check for successful pooling
		if missing(`eff', `se_eff') exit 2002
		
		// return scalars
		return scalar RR = `RR'
		return scalar eff = `eff'
		return scalar se_eff = `se_eff'
		
		// weight
		qui replace `wtvar' = `s' if `touse'
	}

	// Mantel-Haenszel RD
	else if "`summstat'"=="rd" {
		args rdwt rdnum vnum
		
		tempname W eff
		summ `rdwt' if `touse', meanonly
		scalar `W' = cond(r(N), r(sum), .)
		summ `rdnum' if `touse', meanonly
		scalar `eff' = r(sum)/`W'							// pooled RD

		tempname se_eff
		summ `vnum' if `touse', meanonly
		scalar `se_eff' = sqrt( r(sum) /(`W'*`W') )		// SE of pooled RD
		
		// check for successful pooling
		if missing(`eff', `se_eff') exit 2002

		// return scalars
		return scalar eff = `eff'
		return scalar se_eff = `se_eff'
		
		// weight
		qui replace `wtvar' = `rdwt' if `touse'
	}
	
	// Standard heterogeneity
	if "`hetopt'"=="mhq" {
		tokenize `qvlist'
		args _ES _seES				// needed for heterogeneity calculations		
		
		qui gen double `qhet' = ((`_ES' - `eff') / `_seES') ^2
		summ `qhet' if `touse', meanonly

		if r(N)>1 {
			scalar `Q' = r(sum)
			scalar `Qdf' = r(N) - 1
		}
	}
	return scalar Q = `Q'
	return scalar Qdf = `Qdf'
		

	** Critical values, p-values, confidence intervals
	if "`oevlist'"=="" {				// i.e. all unless CMH (done previously)
		tempname crit z pvalue
		scalar `crit' = invnormal(.5 + `level'/200)
		scalar `z' = `eff'/`se_eff'
		scalar `pvalue' = 2*normal(-abs(`z'))
		return scalar crit = `crit'
		return scalar z = `z'
		return scalar pvalue = `pvalue'
	}
	
	// Confidence intervals
	return scalar eff_lci = `eff' - invnormal(.5 + `level'/200) * `se_eff'
	return scalar eff_uci = `eff' + invnormal(.5 + `level'/200) * `se_eff'
	
	
	** Derive and return:  H, I-squared, and (modified) H-squared	
	cap nois Heterogi `Q' `Qdf', hetopt(`hetopt')
	if _rc {
		if _rc==1 nois disp as err `"User break in {bf:metan.Heterogi}"'
		else nois disp as err `"Error in {bf:metan.Heterogi}"'
		c_local err noerr		// tell -metan- not to also report an "error in metan.PerformPoolingMH"
		exit _rc
	}		

	return add
	
	// Return other scalars
	qui count if `touse'
	return scalar k   = r(N)	// k = number of studies (= count if `touse')
	return scalar Q   = `Q'		// generic heterogeneity statistic (incl. Peto, M-H, Breslow-Day)
	return scalar Qdf = `Qdf'	// Q degrees of freedom (= `k' - 1)

	// Return weights for CumInfLoop
	summ `wtvar' if `touse', meanonly
	return scalar totwt = cond(r(N), r(sum), .)		// sum of (non-normalised) weights

end



// Based on heterogi.ado from SSC, with release notes:
// version 2.0 N.Orsini, I. Buchan, 25 Jan 06
// version 1.0 N.Orsini, J.Higgins, M.Bottai, 16 Feb 2005
// (c.f. Higgins & Thompson Stat Med 2002, "Quantifying heterogeneity")

program define Heterogi, rclass
	
	syntax anything [if] [in], HETOPT(string) ///
		[SE(varname numeric) TAUSQ(namelist min=1 max=3) LEVEL(real 95) ]

	marksample touse
	tokenize `anything'
	assert `"`3'"'==`""'
	args Q Qdf
	
	tempname H Isq HsqM
	scalar `H'    = max(1, sqrt(`Q'          / `Qdf'))	
	scalar `Isq'  = max(0, 100*(`Q' - `Qdf') / `Q')
	scalar `HsqM' = max(0,     (`Q' - `Qdf') / `Qdf')
	
	// Q-based heterogeneity confidence intervals
	if inlist("`hetopt'", "higgins", "ncchi2", "qgamma") {
		
		// Higgins and Thompson test-based
		// (Higgins & Thompson, Stats in Medicine 2002)
		if "`hetopt'"=="higgins" {
			tempname H_lci H_uci Isq_lci Isq_uci HsqM_lci HsqM_uci	
			tempname k selogH
			scalar `k' = `Qdf' + 1
			scalar `selogH' = cond(`Q' > `k', ///
				.5*( (ln(`Q') - ln(`Qdf')) / ( sqrt(2*`Q') - sqrt(2*`k' - 3) ) ), ///
				sqrt( ( 1/(2*(`k'-2)) * (1 - 1/(3*(`k'-2)^2)) ) ))
			scalar `H_lci' = max(1, exp( ln(`H') - invnormal(.5 + `level'/200) * `selogH' ))
			scalar `H_uci' =        exp( ln(`H') + invnormal(.5 + `level'/200) * `selogH' )
			
			/*
			if "`hetopt'"=="testbasedci" {
				// CI for I2 based on var(logH), formula not indicated in (Higgins & Thompson, Stats in Medicine)
				//   (but readily re-derivable mathematically using the delta method)
				// Taken from code of heterogi.ado by N.Orsini, J.Higgins, M.Bottai, N.Buchan (2005-2006) BUT not actually outputted!!
				tempname Isqse
				scalar `Isqse'  = 200* `selogH' / exp(2* ln(`H'))
				scalar `Isq_lci' = max(0,   `Isq' - invnormal(.5 + `level'/200) * `Isqse')
				scalar `Isq_uci' = min(100, `Isq' + invnormal(.5 + `level'/200) * `Isqse')
				
				// CI for H2M, derived in a similar way
				// (not previously included in heterogi.ado)
				tempname HsqMse
				scalar `HsqMse'  = 2* `selogH' * exp(2* ln(`H'))
				scalar `HsqM_lci' = max(0, `HsqM' - invnormal(.5 + `level'/200) * `HsqMse')
				scalar `HsqM_uci' =        `HsqM' + invnormal(.5 + `level'/200) * `HsqMse'
			}
			else {
			*/

			// standard, transformed CIs for Isq and HsqM, as outputted by heterogi.ado
			// Taken from heterogi.ado by N.Orsini, J.Higgins, M.Bottai, N.Buchan (2005-2006)
			scalar `Isq_lci' = 100*max(0, (`H_lci'^2 - 1) / `H_lci'^2 )
			scalar `Isq_uci' = 100*min(1, (`H_uci'^2 - 1) / `H_uci'^2 )
			
			scalar `HsqM_lci' = max(0, `H_lci'^2 - 1)
			scalar `HsqM_uci' =        `H_uci'^2 - 1			
		}
		
		// Seek CI for Q using non-central chi-squared or Gamma distribution, thence for H and Isq
		// (method not referenced anywhere apart from in heterogi.ado)	
		else if inlist("`hetopt'", "ncchi2", "qgamma") {
			tempname Q_lci Q_uci

			// using non-centrality parameter nc = Q-df
			if "`hetopt'"=="ncchi2" {
				tempname nc
				scalar `nc' = max(0, `Q' - `Qdf')
	 
				// If Q < df, no need to seek the lower bound
				scalar `Q_lci' = cond(`Q' < `Qdf', 0, invnchi2(`Qdf', `nc', `level'))
				scalar `Q_uci' =                      invnchi2(`Qdf', `nc', `level')
			}

			// alternative version, using Gamma distribution and Biggerstaff-Tweedie Var(Q)		
			else {
				tempvar wtvar
				qui gen double `wtvar' = 1/`se'^2
				
				tempname W1 W2 W3 tsq_dl
				summ `wtvar' if `touse', meanonly
				scalar `W1' = r(sum)				// sum of weights
				summ `wtvar' [aw=`wtvar'] if `touse', meanonly
				scalar `W2' = r(sum)				// sum of squared weights
				summ `wtvar' [aw=`wtvar'^2] if `touse', meanonly
				scalar `W3' = r(sum)				// sum of cubed weights
				scalar `tsq_dl' = (`Q' - `Qdf') / (`W1' - `W2'/`W1')	// non-truncated tsq_DL
			
				tempname btVarQ
				scalar `btVarQ' = 2*`Qdf' + 4*`tsq_dl'*(`W1' - `W2'/`W1') + 2*(`tsq_dl'^2)*(`W2' - 2*`W3'/`W1' + (`W2'/`W1')^2)
				
				// If Q < df, no need to seek the lower bound			
				scalar `Q_lci' = cond(`Q' < `Qdf', 0, invgammap(`Q'^2 / `btVarQ', .025) * `btVarQ' / `Q')
				scalar `Q_uci' =                      invgammap(`Q'^2 / `btVarQ', .975) * `btVarQ' / `Q'
			}

			tempname H_lci H_uci Isq_lci Isq_uci HsqM_lci HsqM_uci	
			scalar `H_lci' = max(1, sqrt(`Q_lci' / `Qdf'))
			scalar `H_uci' =        sqrt(`Q_uci' / `Qdf')

			scalar `Isq_lci' = 100*max(0, (`H_lci'^2 - 1) / `H_lci'^2 )
			scalar `Isq_uci' = 100*min(1, (`H_uci'^2 - 1) / `H_uci'^2 )
			
			scalar `HsqM_lci' = max(0, `H_lci'^2 - 1)
			scalar `HsqM_uci' =        `H_uci'^2 - 1
		}
	}
	
	// tausq-based heterogeneity
	else if "`tausq'"!="" {
		tokenize `tausq'
		args tausq tsq_lci tsq_uci
		confirm numeric variable `se'
		
		tempvar wtvar
		qui gen double `wtvar' = 1/`se'^2
		
		tempname W1 W2
		summ `wtvar' if `touse', meanonly
		scalar `W1' = r(sum)				// sum of weights
		summ `wtvar' [aw=`wtvar'] if `touse', meanonly
		scalar `W2' = r(sum)				// sum of squared weights
		
		tempname sigmasq
		scalar `sigmasq' = (r(N) - 1) / (`W1' - `W2'/`W1')
		
		scalar `H' = sqrt((`tausq' + `sigmasq') / `sigmasq')
		if `"`tsq_lci'"'!=`""' {
			tempname H_lci H_uci
			scalar `H_lci' = sqrt((`tsq_lci' + `sigmasq') / `sigmasq')
			scalar `H_uci' = sqrt((`tsq_uci' + `sigmasq') / `sigmasq')
		}
		
		scalar `Isq' = 100*`tausq' / (`tausq' + `sigmasq')
		if `"`tsq_lci'"'!=`""' {
			tempname Isq_lci Isq_uci
			scalar `Isq_lci' = 100*`tsq_lci' / (`tsq_lci' + `sigmasq')
			scalar `Isq_uci' = 100*`tsq_uci' / (`tsq_uci' + `sigmasq')
		}
				
		scalar `HsqM' = `tausq' / `sigmasq'
		if `"`tsq_lci'"'!=`""' {
			tempname HsqM_lci HsqM_uci
			scalar `HsqM_lci' = `tsq_lci' / `sigmasq'
			scalar `HsqM_uci' = `tsq_uci' / `sigmasq'
		}
	}
	
	// return scalars
	return scalar H = `H'	
	return scalar Isq = `Isq'
	return scalar HsqM = `HsqM'

	if `"`H_lci'"'!=`""' {
		return scalar H_lci = `H_lci'
		return scalar H_uci = `H_uci'
		return scalar Isq_lci = `Isq_lci'
		return scalar Isq_uci = `Isq_uci'
		return scalar HsqM_lci = `HsqM_lci'
		return scalar HsqM_uci = `HsqM_uci'	
	}
	
end	




***********************************************************

* Program to generate confidence intervals for individual studies (NOT pooled estimates)
// subroutine of PerformMetaAnalysis

// N.B. near-identical subroutine also used in ipdover.ado

program define GenConfInts, rclass

	syntax varlist(numeric min=2 max=6 default=none) [if] [in], CItype(string) ///
		OUTVLIST(varlist numeric min=5 max=8) ///
		[ DF(varname numeric) LEVEL(real 95) * ]

	marksample touse, novarlist
	
	// if no data to process, exit without error
	return scalar level = `level'
	qui count if `touse'
	if !r(N) exit

	// Unpack varlists
	tokenize `outvlist'
	args _ES _seES _LCI _UCI _WT _NN
	local params : word count `varlist'		// `varlist' == `invlist'
		
	// Confidence limits need calculating if:
	//  - not supplied by user (i.e. `params'!=3); or
	//  - desired coverage is not 95%
	if `params'==3 & `level'==95 exit
	
	
	* Calculate confidence limits for original study estimates using specified `citype'
	// (unless limits supplied by user)
	if "`citype'"=="normal" {			// normal distribution - default
		tempname critval
		scalar `critval' = invnormal(.5 + `level'/200)
		qui replace `_LCI' = `_ES' - `critval'*`_seES' if `touse'
		qui replace `_UCI' = `_ES' + `critval'*`_seES' if `touse'
	}
		
	else if inlist("`citype'", "t", "logit") {		// t or logit distribution
	
		cap confirm numeric variable `df'
		if !_rc {
			summ `df' if `touse', meanonly			// use supplied df if available
			cap assert r(max) < .
			if _rc {
				nois disp as err `"Degrees-of-freedom variable {bf:`df'} contains missing values;"'
				nois disp as err `"  cannot use {bf:`citype'}-based confidence intervals for study estimates"'
				exit 198
			}
		}
		else {
			cap confirm numeric variable `_NN'
			if !_rc {
				summ `_NN' if `touse', meanonly			// otherwise try using npts
				cap assert r(max) < .
				if _rc {
					nois disp as err `"Participant numbers not available for all studies;"'
					nois disp as err `"  cannot use {bf:`citype'}-based confidence intervals for study estimates"'
					exit 198
				}
				tempvar df
				qui gen `: type `_NN'' `df' = `_NN' - 2			// use npts-2 as df for t distribution of df not explicitly given
				local disperr `"nois disp as err `"Note: Degrees of freedom for {bf:`citype'}-based confidence intervals not supplied; using {it:n-2} as default"'"'
				// delay error message until after checking _ES is between 0 and 1 for logit
			}
			else {
				nois disp as err `"Neither degrees-of-freedom nor participant numbers available;"'
				nois disp as err `"  cannot use {bf:`citype'}-based confidence intervals for study estimates"'
				exit 198
			}
		}
		
		tempvar critval
		qui gen double `critval' = invttail(`df', .5 - `level'/200)
		
		if "`citype'"=="t" {
			qui replace `_LCI' = `_ES' - `critval'*`_seES' if `touse'
			qui replace `_UCI' = `_ES' + `critval'*`_seES' if `touse'
		}
		else {								// logit, proportions only (for formula, see Stata manual for -proportion-)
			summ `_ES' if `touse', meanonly
				if r(min)<0 | r(max)>1 {
				nois disp as err "option {bf:citype(logit)} may only be used with proportions"
				exit 198
			}
			qui replace `_LCI' = invlogit(logit(`_ES') - `critval'*`_seES'/(`_ES'*(1 - `_ES'))) if `touse'
			qui replace `_UCI' = invlogit(logit(`_ES') + `critval'*`_seES'/(`_ES'*(1 - `_ES'))) if `touse'
		}
	}
		
	else if inlist("`citype'", "cornfield", "exact", "woolf") {		// options to pass to -cci-; summstat==OR only
		tokenize `varlist'
		args a b c d		// events & non-events in trt; events & non-events in control (c.f. -metan- help file)

		// sort appropriately, then find observation number of first relevant obs
		tempvar obs
		qui bysort `touse' : gen long `obs' = _n if `touse'			// N.B. MetaAnalysisLoop uses -sortpreserve-
		sort `obs'													// so this sorting should not affect the original data
		summ `obs' if `touse', meanonly
		forvalues j = 1/`r(max)' {
			`version' qui cci `=`a'[`j']' `=`b'[`j']' `=`c'[`j']' `=`d'[`j']', `citype' level(`level')
			qui replace `_LCI' = ln(`r(lb_or)') in `j'
			qui replace `_UCI' = ln(`r(ub_or)') in `j'
		}
	}
	
	// Now display delayed error message if appropriate
	`disperr'

end






****************************************************************************



********************
* Mata subroutines *
********************

mata:


/* Kontopantelis's bootstrap DerSimonian-Laird estimator */
// (PLoS ONE 2013; 8(7): e69930, and also implemented in metaan)
// N.B. using originally estimated ES within the re-samples, as in Kontopantelis's paper */
void DLb(string scalar varlist, string scalar touse, real scalar level, real scalar reps)
{
	// setup
	real colvector yi, se, vi, wi
	varlist = tokens(varlist)
	st_view(yi=., ., varlist[1], touse)
	if(length(yi)==0) exit(error(111))
	st_view(se=., ., varlist[2], touse)
	vi = se:^2
	wi = 1:/vi

	// calculate FE eff
	real scalar eff
	eff = mean(yi, wi)	

	// carry out bootstrap procedure
	transmorphic B, J
	real colvector report
	B = mm_bs(&ftausq(), (yi, vi), 1, reps, 0, 1, ., ., ., eff)
	J = mm_jk(&ftausq(), (yi, vi), 1, 1, ., ., ., ., ., eff)
	report = mm_bs_report(B, ("mean", "bca"), level, 0, J)

	// truncate at zero
	report = report:*(report:>0)
	
	// return tausq and confidence limits
	real scalar tausq
	tausq = report[1]
	st_numscalar("r(tausq)", tausq)
	st_numscalar("r(tsq_lci)", report[2])
	st_numscalar("r(tsq_uci)", report[3])
}

real scalar ftausq(real matrix coeffs, real colvector weight, real scalar eff) {
	real colvector yi, vi, wi
	real scalar k, Q, c, tausq
	yi = select(coeffs[,1], weight)
	vi = select(coeffs[,2], weight)
	k = length(yi)
	wi = 1:/vi
	Q = crossdev(yi, eff, wi, yi, eff)
	c = sum(wi) - mean(wi, wi)
	tausq = max((0, (Q-(k-1))/c))
	return(tausq)
}



/* "Generalised Q" methods */
void GenQ(string scalar varlist, string scalar touse, real scalar tsqlevel, real rowvector iteropts)
{
	// setup
	real colvector yi, se, vi, wi
	varlist = tokens(varlist)
	st_view(yi=., ., varlist[1], touse)
	if(length(yi)==0) exit(error(111))
	st_view(se=., ., varlist[2], touse)
	vi = se:^2
	wi = 1:/vi

	real scalar maxtausq, itol, maxiter
	maxtausq = iteropts[1]
	itol = iteropts[2]
	maxiter = iteropts[3]
	
	real scalar k
	k = length(yi)
	
	/* Mandel-Paule estimator of tausq (J Res Natl Bur Stand 1982; 87: 377-85) */
	// (also DerSimonian & Kacker, Contemporary Clinical Trials 2007; 28: 105-114)
	// ... can be shown to be equivalent to the "empirical Bayes" estimator
	// (e.g. Sidik & Jonkman Stat Med 2007; 26: 1964-81)
	// and converges more quickly
	real scalar rc_tausq, tausq
	rc_tausq = mm_root(tausq=., &Q_crit(), 0, maxtausq, itol, maxiter, yi, vi, k, k-1)
	st_numscalar("r(tausq)", tausq)
	st_numscalar("r(rc_tausq)", rc_tausq)
	

	/* Confidence interval for tausq by generalised Q-profiling */
	// Viechtbauer Stat Med 2007; 26: 37-52
	// (N.B. most natural point estimate is Mandel-Paule, but any estimate will do)
	real scalar eff, Qmin, Qmax
	eff = mean(yi, wi)							// fixed-effects estimate
	Qmin = crossdev(yi, eff, wi, yi, eff)		// Q(0) = standard Cochran's Q heterogeneity statistic (when tausq=0)
	wi = 1:/(vi:+maxtausq)
	eff = mean(yi, wi)
	Qmax = crossdev(yi, eff, wi, yi, eff)
	
	// estimate tausq confidence limits
	real scalar Q_crit_hi, Q_crit_lo, tsq_lci, rc_tsq_lci, tsq_uci, rc_tsq_uci
	Q_crit_hi = invchi2(k-1, .5 + tsqlevel/200)		// higher critical value (0.975) to compare GenQ against (for *lower* bound of tausq)
	Q_crit_lo = invchi2(k-1, .5 - tsqlevel/200)		//  lower critical value (0.025) to compare GenQ against (for *upper* bound of tausq)
	
	if (Qmin < Q_crit_lo) {			// if Q(0) is less the lower critical value, interval is set to null
		rc_tsq_lci = 2
		rc_tsq_uci = 2
		tsq_lci = 0
		tsq_uci = 0
	}	
	else {
		if (Qmax > Q_crit_lo) {		// If Q(maxtausq) is larger than the lower critical value...
			rc_tsq_uci = 2
			tsq_uci = maxtausq		// ...upper bound for tausq is tausqmax
		}
		else {
			rc_tsq_uci = mm_root(tsq_uci=., &Q_crit(), 0, maxtausq, itol, maxiter, yi, vi, k, Q_crit_lo)
		}
	}
	if (Qmax > Q_crit_hi) {			// If Q(maxtausq) is larger than the higher critical value, interval is set to null
		rc_tsq_lci = 2
		rc_tsq_uci = 2
		tsq_lci = maxtausq
		tsq_uci = maxtausq
	}
	else {
		if (Qmin < Q_crit_hi) {		// If Q(0) is less than the higher critical value...
			rc_tsq_lci = 2
			tsq_lci = 0				// ...lower bound for tausq is 0
		}		
		else {
			rc_tsq_lci = mm_root(tsq_lci=., &Q_crit(), 0, maxtausq, itol, maxiter, yi, vi, k, Q_crit_hi)
		}
	}
	
	// return confidence limits and rc codes
	st_numscalar("r(tsq_lci)", tsq_lci)
	st_numscalar("r(tsq_uci)", tsq_uci)
	st_numscalar("r(rc_tsq_lci)", rc_tsq_lci)
	st_numscalar("r(rc_tsq_uci)", rc_tsq_uci)
}

real scalar Q_crit(real scalar tausq, real colvector yi, real colvector vi, real scalar k, real scalar crit) {
	real colvector wi
	real scalar eff, newtausq
	wi = 1:/(vi:+tausq)
	eff = mean(yi, wi)
	newtausq = (k/crit)*crossdev(yi, eff, wi, yi, eff)/sum(wi) - mean(vi, wi)	// corrected June 2015
	return(tausq - newtausq)
}



/* ML + optional PL (for likelihood profiling for ES CI) */
// (N.B. pass wi back-and-forth as it needs to be calculated anyway for tausq likelihood profiling)
void MLPL(string scalar varlist, string scalar touse, real rowvector levels, real rowvector iteropts, string scalar model)
{
	// setup
	real colvector yi, se, vi, wi, eff
	varlist = tokens(varlist)
	st_view(yi=., ., varlist[1], touse)
	if(length(yi)==0) exit(error(111))
	st_view(se=., ., varlist[2], touse)
	vi = se:^2

	real scalar maxtausq, itol, maxiter
	maxtausq = iteropts[1]
	itol = iteropts[2]
	maxiter = iteropts[3]
	
	real scalar level, tsqlevel
	level = levels[1]
	tsqlevel = levels[2]
	
	// Iterative point estimate for tausq using ML
	real scalar tausq, rc_tausq
	rc_tausq = mm_root(tausq=., &ML_est(), 0, maxtausq, itol, maxiter, yi, vi)
	st_numscalar("r(tausq)", tausq)
	st_numscalar("r(rc_tausq)", rc_tausq)
	
	// Point estimate for eff, using ML point estimate of tausq
	// (also, variance of tausq using inverse Fisher information)
	wi = 1:/(vi:+tausq)
	eff = mean(yi, wi)
	tsq_var = 2*sum(wi:^2)^-1
	st_numscalar("r(tsq_var)", tsq_var)	
	
	// Calculate ML log-likelihood value (ignoring constant term)
	// based on ML point estimates of eff and tausq
	// [NOTE: first term is *positive* since wi = 1/(vi+tausq).
	//  In the literature, likelihood is usually stated in terms of **vi**
	//  and hence the term is *negative*.]
	real scalar ll, crit, tsq_lci, rc_tsq_lci, tsq_uci, rc_tsq_uci
	ll = 0.5*sum(ln(wi)) - 0.5*crossdev(yi, eff, wi, yi, eff)
	
	// Confidence interval for tausq using likelihood profiling
	crit = ll - invchi2(1, tsqlevel/100)/2

	rc_tsq_lci = mm_root(tsq_lci=., &ML_profile_tausq(), 0, tausq - itol, itol, maxiter, yi, vi, crit)
	st_numscalar("r(tsq_lci)", tsq_lci)
	st_numscalar("r(rc_tsq_lci)", rc_tsq_lci)
	
	rc_tsq_uci = mm_root(tsq_uci=., &ML_profile_tausq(), tausq + itol, 10*maxtausq, itol, maxiter, yi, vi, crit)
	st_numscalar("r(tsq_uci)", tsq_uci)
	st_numscalar("r(rc_tsq_uci)", rc_tsq_uci)
	
	// Profile likelihood
	if (model!="ml") {
	
		// Bartlett's correction
		// (see e.g. Huizenga et al, Br J Math Stat Psychol 2011)
		real scalar BCFinv
		BCFinv = 1
		if (model=="plbart") {
			BCFinv = 1 + 2*mean(wi, wi:^2)/sum(wi) - 0.5*mean(wi, wi)/sum(wi)
			st_numscalar("r(BCF)", 1/BCFinv)
		}
				
		// Log-likelihood based test statistic
		// (evaluated at b = 0)
		real scalar ll0, lr
		crit = ll - invchi2(1, level/100)*BCFinv/2
		ll0 = ML_profile_eff(0, yi, vi, crit, iteropts)
		ll0 = ll0 + crit
		lr = 2*(ll - ll0) / BCFinv

		// Signed log-likelihood statistic
		// (evaluated at b = 0)
		real scalar sll
		if (lr==0) sll = 0
		else sll = sign(eff)*sqrt(lr)

		// Confidence interval for ES using likelihood profiling
		// (use ten times the ML lci and uci for search limits)
		real scalar llim, ulim, eff_lci, eff_uci, rc_eff_lci, rc_eff_uci
		llim = eff - 19.6/sqrt(sum(wi))
		ulim = eff + 19.6/sqrt(sum(wi))
		
		// Skovgaard's correction to the signed likelihood statistic
		if (model=="plskov") {
		
			// Collect ML values of eff, tausq, ll
			real rowvector params
			params = (eff, tausq, ll)

			//  can't directly correct the critical value, due to the square root (i.e. expression is non-linear)
			//  so instead need to pass the critical value to the iteration procedure, and correct afterwards
			crit = invnormal(.5 + level/200)
			sll = ML_skov(0, yi, vi, wi, params, crit, iteropts)
			sll = sll + crit
			
			rc_eff_lci = mm_root(eff_lci=., &ML_skov(), llim, eff-itol, itol, maxiter, yi, vi, wi, params,  crit, iteropts)
			rc_eff_uci = mm_root(eff_uci=., &ML_skov(), eff+itol, ulim, itol, maxiter, yi, vi, wi, params, -crit, iteropts)
			st_numscalar("r(eff_lci)", eff_lci)
			st_numscalar("r(eff_uci)", eff_uci)
			st_numscalar("r(rc_eff_lci)", rc_eff_lci)
			st_numscalar("r(rc_eff_uci)", rc_eff_uci)		
		}
		
		// Otherwise, use the (squared) likelihood statistic LR = SLL^2
		else {
			rc_eff_lci = mm_root(eff_lci=., &ML_profile_eff(), llim, eff, itol, maxiter, yi, vi, crit, iteropts)
			rc_eff_uci = mm_root(eff_uci=., &ML_profile_eff(), eff, ulim, itol, maxiter, yi, vi, crit, iteropts)
			st_numscalar("r(eff_lci)", eff_lci)
			st_numscalar("r(eff_uci)", eff_uci)
			st_numscalar("r(rc_eff_lci)", rc_eff_lci)
			st_numscalar("r(rc_eff_uci)", rc_eff_uci)
		}
		
		st_numscalar("r(ll)", ll)
		st_numscalar("r(lr)", lr)		
		st_numscalar("r(sll)", sll)
	}
}

real scalar ML_est(real scalar tausq, real colvector yi, real colvector vi, | real scalar eff) {
	real colvector wi
	real scalar newtausq
	wi = 1:/(vi:+tausq)
	if (eff==.) eff = mean(yi, wi)
	newtausq = crossdev(yi, eff, wi:^2, yi, eff)/sum(wi:^2) - mean(vi, wi:^2)
	return(tausq - newtausq)
}

real scalar ML_profile_tausq(real scalar tausq, real colvector yi, real colvector vi, real scalar crit) {
	real colvector wi
	real scalar eff, ll
	wi = 1:/(vi:+tausq)
	eff = mean(yi, wi)
	ll = 0.5*sum(ln(wi)) - 0.5*crossdev(yi, eff, wi, yi, eff)
	return(ll - crit)
}

real scalar ML_profile_eff(real scalar eff, real colvector yi, real colvector vi, real scalar crit, real rowvector iteropts) {
	real scalar maxtausq, itol, maxiter
	maxtausq = iteropts[1]
	itol = iteropts[2]
	maxiter = iteropts[3]

	real colvector wi
	real scalar tausq, rc, ll
	rc = mm_root(tausq=., &ML_est(), 0, maxtausq, itol, maxiter, yi, vi, eff)
	if(rc==2) tausq=0
	else if(rc > 0) exit(error(498))
	
	wi = 1:/(vi:+tausq)
	ll = 0.5*sum(ln(wi)) - 0.5*crossdev(yi, eff, wi, yi, eff)
	return(ll - crit)
}

real scalar ML_skov(real scalar b, real colvector yi, real colvector vi, real colvector wi, real rowvector params, real scalar crit, real rowvector iteropts) {

	// unpack iteropts and params
	real scalar maxtausq, itol, maxiter
	maxtausq = iteropts[1]
	itol = iteropts[2]
	maxiter = iteropts[3]
	
	// unpack params (ML values of eff, tausq, ll)
	real scalar eff, tausq, ll
	eff = params[1]
	tausq = params[2]
	ll = params[3]
	
	// find tausq for fixed b, and hence calculate LL
	real scalar rc, tausq_b
	rc = mm_root(tausq_b=., &ML_est(), 0, 10*maxtausq, itol, maxiter, yi, vi, b)	
	if(rc==2) tausq_b=0
	else if(rc > 0) exit(error(498))
	real colvector wi_b
	real scalar ll_b
	wi_b = 1:/(vi:+tausq_b)
	ll_b = 0.5*sum(ln(wi_b)) - 0.5*crossdev(yi, b, wi_b, yi, b)
	
	// signed likelihood statistic at b
	real scalar sll
	sll = sign(eff - b)*sqrt(2*(ll - ll_b))	
			
	// calculate u for Skovgaard correction
	u = U(yi, wi, wi_b, eff, b)
	
	// Improved (Skovgaard-corrected) signed likelihood statistic
	real scalar sll_new
	sll_new = sll + (1/sll)*ln(abs(u/sll))
	
	// Compare sll_new with sll:
	// If sll was zero, sll_new will be undefined; reset to 0
	// If sll_new is (a) the opposite sign to sll; (b) has larger absolute value than sll
	//   then the correction has "failed"; reset to former sll value
	if (sll==0) sll_new = 0
	else if (sign(sll) != sign(sll_new)) sll_new = sll
	else if (abs(sll) < abs(sll_new)) sll_new = sll
	return(sll_new - crit)
}

real scalar U(real colvector yi, real colvector wi, real colvector wi_b, real scalar eff, real scalar b) {

	// Skovgaard components:
	real colvector wi2, wi3, wi_b2, wi_b3
	wi2 = wi:^2
	wi3 = wi:^3
	wi_b2 = wi_b:^2
	wi_b3 = wi_b:^3

	// Expected (I) & observed (J) information, evaluated at ML estimate
	real matrix Imat
	Imat = (sum(wi), 0 \ 0, .5*sum(wi2))
	
	real matrix Jmat
	Jmat = (sum(wi), sum(wi2:*(yi:-eff)) \ sum(wi2:*(yi:-eff)) , -.5*sum(wi2) + sum(wi3:*((yi:-eff):^2)))
	
	// Observed (J) information under constraint eff = b, corresponding to tausq
	real scalar Jtsq
	Jtsq = -.5*sum(wi_b2) + sum(wi_b3:*((yi:-b):^2))

	// S and q
	real matrix S, Sinvq
	real colvector q
	S = (sum(wi_b), (eff-b)*sum(wi_b2) \ 0, .5*sum(wi_b2))
	q = ((eff-b)*sum(wi_b) \ -.5*sum(wi - wi_b))
	Sinvq = luinv(S)*q
	
	real scalar u
	u = abs(Sinvq[1,1]) * sqrt(abs(det(Jmat))) * abs(det(S)) / (sqrt(abs(Jtsq)) * abs(det(Imat)))
	return(u)
}



/* REML */
// (N.B. pass wi back-and-forth as it needs to be calculated anyway for tausq likelihood profiling)
void REML(string scalar varlist, string scalar touse, real scalar tsqlevel, real rowvector iteropts)
{
	// setup
	real colvector yi, se, vi, wi, eff
	varlist = tokens(varlist)
	st_view(yi=., ., varlist[1], touse)
	if(length(yi)==0) exit(error(111))
	st_view(se=., ., varlist[2], touse)
	vi = se:^2
	
	real scalar maxtausq, itol, maxiter
	maxtausq = iteropts[1]
	itol = iteropts[2]
	maxiter = iteropts[3]
	
	// Iterative tau-squared using REML
	real scalar tausq, rc_tausq
	rc_tausq = mm_root(tausq=., &REML_est(), 0, maxtausq, itol, maxiter, yi, vi)
	st_numscalar("r(tausq)", tausq)
	st_numscalar("r(rc_tausq)", rc_tausq)

	// Variance of tausq (using inverse Fisher information)
	wi = 1:/(vi:+tausq)
	tsq_var = 2*(sum(wi:^2) - 2*mean(wi:^2, wi) + mean(wi, wi)^2)^-1
	st_numscalar("r(tsq_var)", tsq_var)
	
	// Calculate REML log-likelihood value (ignoring constant term)
	// [NOTE: first term is *positive* since wi = 1/(vi+tausq).
	//  In the literature, likelihood is usually stated in terms of **vi**
	//  and hence the term is *negative*.]	
	real scalar ll, tsq_lci, rc_tsq_lci, tsq_uci, rc_tsq_uci
	eff = mean(yi, wi)
	ll = 0.5*sum(ln(wi)) - 0.5*ln(sum(wi)) - 0.5*crossdev(yi, eff, wi, yi, eff)
	crit = ll - (invchi2(1, tsqlevel/100)/2)
	st_numscalar("r(ll)", ll)

	// Confidence interval for tausq using likelihood profiling
	rc_tsq_lci = mm_root(tsq_lci=., &REML_profile_tausq(), 0, tausq - itol, itol, maxiter, yi, vi, crit)
	st_numscalar("r(tsq_lci)", tsq_lci)
	st_numscalar("r(rc_tsq_lci)", rc_tsq_lci)

	rc_tsq_uci = mm_root(tsq_uci=., &REML_profile_tausq(), tausq + itol, 10*maxtausq, itol, maxiter, yi, vi, crit)
	st_numscalar("r(tsq_uci)", tsq_uci)
	st_numscalar("r(rc_tsq_uci)", rc_tsq_uci)
}

real scalar REML_est(real scalar tausq, real colvector yi, real colvector vi) {
	real colvector wi
	real scalar eff, newtausq
	wi = 1:/(vi:+tausq)
	eff = mean(yi, wi)
	newtausq = crossdev(yi, eff, wi:^2, yi, eff)/sum(wi:^2) - mean(vi, wi:^2) + (1/sum(wi))
	return(tausq - newtausq)
}

real scalar REML_profile_tausq(real scalar tausq, real colvector yi, real colvector vi, real scalar crit) {
	real colvector wi
	real scalar eff, ll
	wi = 1:/(vi:+tausq)
	eff = mean(yi, wi)
	ll = 0.5*sum(ln(wi)) - 0.5*ln(sum(wi)) - 0.5*crossdev(yi, eff, wi, yi, eff)
	return(ll - crit)
}


/* Confidence interval for tausq estimated using approximate Gamma distribution for Q */
/* based on paper by Biggerstaff and Tweedie (Stat Med 1997; 16: 753-768) */
// Point estimate of tausq is simply the D+L estimate
void Gamma(string scalar varlist, string scalar touse, string scalar wtvec, real scalar tsqlevel, real rowvector iteropts)
{
	// Setup
	real colvector yi, se, vi, wi
	varlist = tokens(varlist)
	st_view(yi=., ., varlist[1], touse)
	if(length(yi)==0) exit(error(111))
	st_view(se=., ., varlist[2], touse)
	vi = se:^2
	wi = 1:/vi

	real scalar maxtausq, itol, maxiter, quadpts
	maxtausq = iteropts[1]
	itol = iteropts[2]
	maxiter = iteropts[3]
	quadpts	= iteropts[4]

	// Estimate variance of tausq
	real scalar k, eff, Q, c, d, tausq_m, tausq, Q_var, tsq_var
	k = length(yi)
	eff = mean(yi, wi)					// fixed-effects estimate
	Q = crossdev(yi, eff, wi, yi, eff)	// standard Q heterogeneity statistic
	c = sum(wi) - mean(wi,wi)			// c = S1 - (S2/S1)
	d = cross(wi,wi) - 2*mean(wi:^2,wi) + (mean(wi,wi)^2)
	tausq_m = (Q - (k-1))/c				// untruncated D+L tausq

	// Variance of Q and tausq (based on untruncated tausq)
	Q_var = 2*(k-1) + 4*c*tausq_m + 2*d*(tausq_m^2)
	tsq_var = Q_var/(c^2)
	st_numscalar("r(tsq_var)", tsq_var)

	// Find confidence limits for tausq
	real scalar tsq_lci, rc_tsq_lci, tsq_uci, rc_tsq_uci
	rc_tsq_lci = mm_root(tsq_lci=., &Gamma_crit(), 0, maxtausq, itol, maxiter, tausq_m, k, c, d, .5 + tsqlevel/200)
	st_numscalar("r(tsq_lci)", tsq_lci)
	st_numscalar("r(rc_tsq_lci)", rc_tsq_lci)

	rc_tsq_uci = mm_root(tsq_uci=., &Gamma_crit(), tsq_lci + itol, 10*maxtausq, itol, maxiter, tausq_m, k, c, d, .5 - tsqlevel/200)
	st_numscalar("r(tsq_uci)", tsq_uci)
	st_numscalar("r(rc_tsq_uci)", rc_tsq_uci)
		
	// Find and return new weights
	real scalar EQ, VQ, lambda, r, se_eff
	EQ = (k-1) + c*tausq_m
	VQ = 2*(k-1) + 4*c*tausq_m + 2*d*(tausq_m^2)
	lambda = EQ/VQ
	r = lambda*EQ
	
	wsi = wi
	for(i=1; i<=k; i++) {
		params = (vi[i], lambda, r, c, k)
		wsi[i] = integrate(&BTIntgrnd(), 0, ., quadpts, params)
	}
	wi = wi*gammap(r, lambda*(k-1)) :+ wsi								// update weights
	st_store(st_viewobs(yi), wtvec, wi)									// write new weights to Stata
}

real scalar Gamma_crit(real scalar tausq, real scalar tausq_m, real scalar k, real scalar c, real scalar d, real scalar crit) {
	real scalar lambda, r, limit
	lambda = ((k-1) + c*tausq)/(2*(k-1) + 4*c*tausq + 2*d*(tausq^2))
	r = ((k-1) + c*tausq)*lambda
	limit = lambda*(c*tausq_m + (k-1))
	ans = gammap(r, limit) - crit
	return(ans)
}

real rowvector BTIntgrnd(real rowvector t, real rowvector params) {
	real scalar s, lambda, r, c, k, ans
	s = params[1,1]				// vi[i] > 0
	lambda = params[1,2]		// lambda = E(Q)/Var(Q) [N.B. the inverse of this is used in Henmi & Copas]
	r = params[1,3]				// r = [E(Q)^2]/Var(Q)
	c = params[1,4]				// c = f(weights)
	k = params[1,5]				// k = no. studies > 1
	ans = (c:/(s:+t)) :* gammaden(r, 1/lambda, 1-k, c*t)
	return(ans)
}


/* Henmi and Copas method */
// Point estimate of tausq is simply the D+L estimate
void HC(string scalar varlist, string scalar touse, real scalar level, real rowvector iteropts)
{
	// Setup
	real colvector yi, se, vi, wi
	varlist = tokens(varlist)
	st_view(yi=., ., varlist[1], touse)
	if(length(yi)==0) exit(error(111))
	st_view(se=., ., varlist[2], touse)
	vi = se:^2

	real scalar itol, maxiter, quadpts
	itol = iteropts[1]
	maxiter = iteropts[2]
	quadpts = iteropts[3]

	real scalar k, eff, Q, tausq, VR, SDR
	k = length(yi)
	wi = 1:/vi
	eff = mean(yi, wi)							// fixed-effects estimate
	Q = crossdev(yi, eff, wi, yi, eff)			// standard Q heterogeneity statistic
	W1 = sum(wi)
	W2 = mean(wi, wi)
	W3 = mean(wi:^2, wi)
	W4 = mean(wi:^3, wi)
	tausq = max((0, (Q - (k-1))/(W1 - W2)))		// truncated D+L
	VR = 1 + tausq*W2
	SDR = sqrt(VR)
	
	// Coefficients of 1 and (x^2) for the following functions:
	// EQ(x) = conditional mean of Q given R=x
	// VQ(x) = conditional variance of Q given R=x
	// finv(x) = inverse function of f(Q).
	// All three functions are linear combinations of 1 and (x^2),
	//   so all can be represented by a single function, f.
	real scalar aEQ, bEQ
	aEQ = (k - 1) + tausq*(W1 - W2) - (tausq^2)*(W3 - W2^2)/VR
	bEQ = (W3 - W2^2)*(tausq/VR)^2
	
	real scalar aVQ, bVQ
	aVQ = 2*(k - 1) + 4*tausq*(W1 - W2) + 2*(tausq^2)*(W1*W2 - 2*W3 + W2^2)
	aVQ = aVQ - 4*(tausq^2)*(W3 - W2^2)/VR
	aVQ = aVQ - 4*(tausq^3)*(W4 - 2*W2*W3 + W2^3)/VR
	aVQ = aVQ + 2*(tausq^4)*(1/VR^2)*(W3 - W2^2)^2
	
	bVQ = 4*(tausq^2)*((1/VR^2))*(W3 - W2^2)
	bVQ = bVQ + 4*(tausq^3)*(1/VR^2)*(W4 - 2*W2*W3 + W2^3)
	bVQ = bVQ - 2*(tausq^4)*2*(1/VR^3)*(W3 - W2^2)^2
	
	real scalar afinv, bfinv
	afinv = (k-1) - (W1/W2 - 1)
	bfinv = (W1/W2 - 1)

	real rowvector params
	params = (aEQ, bEQ, aVQ, bVQ, afinv, bfinv, SDR)
	
	// Find quantile of approximate distribution
	// (u_alpha/2 in Henmi & Copas)
	real scalar t, rc_t
	rc_t = mm_root(t=., &Eqn(), 0, 2, itol, maxiter, quadpts, level, params)
	if (rc_t > 0) exit(error(498))
	st_numscalar("r(crit)", SDR*t)
	
	// Find test statistic (u) and p-value
	real scalar u, p
	u = eff/sqrt((tausq*W2 + 1)/W1)
	p = 2*integrate(&HCIntgrnd(), abs(u)/SDR, 40, quadpts, (abs(u)/SDR, params))
	st_numscalar("r(p)", p)
	st_numscalar("r(u)", u)
}

// N.B. Integration is from x to 40, since the integrand's value is indistinguishable from zero at this point.
// To see this, note that the integrand is the product of a cumulative Gamma function ==> between 0 and 1
//  and a standard normal density which is indistinguishable from zero at ~40.
// (thanks to Ben Jann for pointing this out)
real scalar Eqn(real scalar x, real scalar quadpts, real scalar level, real rowvector params) {
	real scalar ans
	ans = integrate(&HCIntgrnd(), x, 40, quadpts, (x, params))
	return(ans - (.5 - level/200))
}

real rowvector HCIntgrnd(real rowvector r, real rowvector params) {
	real scalar t, aEQ, bEQ, aVQ, bVQ, afinv, bfinv, SDR
	t     = params[1]
	aEQ   = params[2]
	bEQ   = params[3]
	aVQ   = params[4]
	bVQ   = params[5]
	afinv = params[6]
	bfinv = params[7]
	SDR   = params[8]
	
	real rowvector ans
	ans = gammap((f(r*SDR, aEQ, bEQ):^2):/f(r*SDR, aVQ, bVQ), f(r/t, afinv, bfinv):/(f(r*SDR, aVQ, bVQ):/f(r*SDR, aEQ, bEQ))) :* normalden(r)
	if(t==0) ans = normalden(r)
	return(ans)
}

real rowvector f(real rowvector x, real scalar a, real scalar b) {
	return(a :+ b*(x:^2))
}

end







********************************************************************************

***************************************
* metan.ado detailed revision notes *
***************************************

//	METAN UPDATE
//	ROSS HARRIS, DEC 2006
//	MAIN UPDATE IS GRAPHICS IN THE _dispgby PROGRAM
//	ADDITIONAL OPTIONS ARE lcols AND rcols
//	THESE AFFECT DISPLAY ONLY AND ALLOW USER TO SPECIFY
//	VARIABLES AS A FORM OF TABLE. THIS EXTENDS THE label(namevar yearvar)
//	SYNTAX, ALLOWING AS MANY LEFT COLUMNS AS REQUIRED (WELL, LIMIT IS 10)
//	IF rcols IS OMMITTED DEFAULT IS THE STUDY EFFECT (95% CI) AND WEIGHT
//	AS BEFORE- THESE ARE ALWAYS IN UNLESS OMITTED USING OPTIONS
//	ANYTHING ADDED TO rcols COMES AFTER THIS.

//	v1.4 WAS GETTING THERE
// 	v1.5 SORT OUT EXTRA LINE AT THE TOP AND ALLOW "DOUBLE LINES"
//	FOR FIXED AND RANDOM EFFECTS

//	SOMETHING TO ADD- RETURN ES_2?
//	v1.7 - TRY TO SORT OUT LABELLING
//	CHANGED LABELS TO 7.3g -WORKS NICELY
//	"FAVOURS" NOW WORKS- USES xmlabel
//	v1.8 ADDED IN COUNTS OPTION, SORTED OUT TEXTSIZE, PROPER DEFINITION AND SPLIT OF VAR LABELS

// 	v1.9 DELETE UNECESSARY OPTIONS
//	OH, AND ADD effect OPTION
//	v1.10 FINAL TIDYING, USED Jeff Pitblado's SUGGESTION FOR getWidth

//	v1.11 USE label() OPTIONS IF NO lcols rcols, WORK ON AUTO FIT TEXT
//	v1.12 FURTHER WORK...

//	v1.14 DONE ON 12TH OCTOBER, FINALLY DISCOVERED WHAT IS CAUSING PROBLEM
//	WITH "NON-MATCHING CLOSE BRACE" AT END OF FILE- NO v7 STYLE IF STATEMENTS!
//	EVERYTHING GOES ON A SEPARATE LINE NOW. PHEW.

//	v1.15 NOW ADDING IN OPTIONS TO CONTROL BOXES, CI LINES, OVERALL
//	TITLES WEREN'T SPREADING ACROSS SINCE OPTION TO CONTROL OVERALL TEXT- FIXED AGAIN

//	v1.16 LAST ATTEMPT TO GET TEXT SIZE RIGHT! WORK OUT WHAT ASPECT SHOULD BE AND USE
//	IF ASPECT DEFINED THEN DECREASE TEXT SIZE BY RATIO TO IDEAL ASPECT

//	TEXT SCALING WORKING BETTER NOW
//	LAST THING TO TRY TO SORT IS LOOK FOR LEFT OF DIAMOND AND MOVE HET STATS
//	COULD PUT UNDERNEATH IF NOT MUCH SPACE? THIS WOULD BE GOOD v1.17
//	STILL DEBATING WHETHER TO PUT favours BIT BELOW xticks...

//	V19 LOTS OF COMMENTS FROM JONATHAN AND BITS TO DO. SUMMARY:
//	aspect 				Y
//	note if random weights		Y
//	update to v8			Y
//	graph in mono			Y
//	extend overall text into plot	Y
//	labels				Y
//	help file				not v8 yet

//	v1.21 EVERY PROGRAM NOW CONVERTED TO v9.0, NO "CODE FOLLOWS BRACES!"

//	WHAT ELSE DID PATRICK DO TO UPDATE TO v8?

//	NO "#delimit ;" 					- I QUITE LIKE THIS THOUGH!
//	GLOBALS ARE DECLARED WITHOUT = SIGN		- EXCEPT WHEN USING STRING FUNCTION ETC. IT DOESN'T LIKE THIS!
//								- WILL THIS EVER CAUSE PROBLEMS?
//								- CAN'T BE BOTHERED TO CHANGE ALL THE NUMERIC ONES
//	USE TOKENIZE INSTEAD OF PARSE			- DONE
//	USE di as err NOT in red, EXIT		- DONE, PROPER RETURN CODES STILL NEEDED, MAYBE SOMEDAY!
//	DECENT HELP FILE					- USED, JUST ADD IN NEW BITS

//	v1.22 ENCODE STUFF FOR metanby NOW LABORIOUSLY RECODED SO THAT GROUPS ARE IN ORIGINAL SORT ORDER
//	THIS IS USEFUL IF YOU DON'T WANT STUFF IN ALPHA-NUMERIC ORDER, OR TO PUT "1..." "2..." ETC.

//	counts OPTION DOES NOT WORK WITH MEAN DIFFERENCES- AND LOOKS LIKE IT NEVER DID- PUT IN
//	DO OWN LINES FOR OVERALL ETC. EXTENDS TOO FAR
//	LABELS NEVER RUN TO FOUR LINES- SORT OUT- QUICK SOLU- DO FIVE TIMES AND DROP ONE!

//	v1.23 USES pcspike FOR OVERALL AND NULL LINES TO PREVENT OVER-EXTENDING
//	NOW HAS OPTION FOR USER DEFINED "SECOND" ANALYSIS

//	v1.24 ALLOW USER TO COMPLETELY DEFINE ANALYSIS WITH WEIGHTS

// 	v2.34 problem with nosubgroup nooverall sorted out (this combination failed)

********************
** May 2007 fixes **
********************

//	"nostandard" had disappeared from help file- back in
//	I sq. in return list
//	sorted out the extra top line that appears in column labels
//	fixed when using aspect ratio using xsize and ysize so inner bit matches graph area- i.e., get rid of spaces for long/wide graphs
//	variable display format preserved for lcols and rcols
//	abbreviated varlist now allowed
//	between groups het. only available with fixed
//	warnings if any heterogeneity with fixed (for between group het if any sub group has het, overall est if any het)
// 	nulloff option to get rid of line

// May 2009
//	prediction interval sorted out. Note error in Higgins presentation (too wide- there is no heterogeneity in data!)
//	so don't check with this! George Kelley has sent example data




***************************************
* admetan.ado detailed revision notes *
***************************************

* originally written by David Fisher, June 2013

* version 1.0  David Fisher  31jan2014

* version 2.0  David Fisher  11may2017
* Major update to extend functionality beyond estimation commands; now has most of the functionality of -metan-
//  - Reworked so that ipdmetan.ado does command processing and looping, and admetan.ado does RE estimation (including -metan- functionality)
//  - Hence, ipdover calls ipdmetan (but never admetan); ipdmetan calls admetan (if not called by ipdover); admetan can be run alone.
//      Any of the three may call forestplot, which of course can also be run alone.

* version 2.1  David Fisher  14sep2017
// various bug fixes
// Note: for harmonisation with metan/metaan, Isq input and output is now in the range 0 to 100, rather than 0 to 1.

// Corrected error whereby tausq could not be found by iterative methods if >> 0
//  due to assumptions based on me mostly using ratio statistics, where tausq < 1, and not mean differences where tausq can be any magnitude.


* version 3.0  David Fisher  08nov2018
// IPD+AD code now moved to ipdmetan.ado
//   so that admetan is completely self-contained, with minimal reference to -ipdmetan-
// various bug fixes and minor improvements
// implemented -useopts- facility and _EFFECT variable

* version 3.1  David Fisher  04dec2018
// Allow `oev' with Peto ORs
// Specify default format & title for numeric vars in results sets, so that they display nicely in forestplot
// Fixed bug which meant "HKSJ method" was not displayed on screen (although the method itself was used)
// `hksj' and `bartlett' are returned (if applicable) in r(vce_model)

* version 3.2  David Fisher  28jan2019
// Do not allow `study' and `by' to have the same name
// Added SJ Robust ("sandwich-like") variance estimator (Sidik & Jonkman CompStatDataAnalysis 2006)
// Added Skovgaard's correction to the signed likelihood statistic (Guolo Stat Med 2012)
// Corrected bug when continuity correction options were specified; also added new on-screen text r.e. cc [ADDED FEB 2019]
// Corrected returned statistics for `chi2opts' and Henmi-Copas model
// Corrected bug when specifying npts(varname) with "generic" effect measures
// Generalised the two-step estimators (Sidik-Jonkman and DerSimonian-Kacker)
// `hksj', `bartlett', `skovgaard' and `robust' are returned (if applicable) as part of r(model)
// Some text in help file has been changed/updated

* version 3.3 (beta; never released)  David Fisher 30aug2019

// Zero cells and the Mantel-Haenszel method:  default is to add cc=0.5 for display purposes only...
//   ...including "double-zero" studies, as they still contribute to the MH pooled estimate
// (if M-H) If -nocc- is explicitly requested, *KEEP ALL STUDIES IN* and print appropriate warning message
// Zero-cell studies must be *explicitly excluded* using if/in in this scenario.

// Q, tausq etc. based on MH vs Inverse-Variance pooled estimate:  No tausq/Isq if M-H.

// Also if M-H:  CMH with/without correction; "Old" Breslow/Day; "New" Breslow/Day/Tarone.

* Current version 3.4 (beta; will be 4.0 upon release)  David Fisher 23oct2019

// Fixed bug where main options (e.g. nograph) would be ignored under certain circumstances
// Fixed bug where id would be repeated if given as lcols(id)

// Major addition: multiple models, either as backslash-separated list e.g. model(fe\dl\reml\pl)
//  or, as in -metan-  first() second()

// Also now incorporating code from "heterogi.ado" for calculating CIs for Isq
// (c.f. Higgins & Thompson Stat Med 2002, "Quantifying heterogeneity")
//  - test-based
//  - noncentral Q
//  - Q profiling

* Program to generate forestplots -- used by ipdmetan etc. but can also be run by itself
* April 2013
*   Forked from main ipdmetan code
* September 2013
*   Following UK Stata Users Meeting, reworked the plotid() option as recommended by Vince Wiggins

* version 1.0  David Fisher  31jan2014

* version 1.01  David Fisher  07feb2014
* Reason: fixed bug - random-effects note being overlaid on x-axis labels

* version 1.02  David Fisher  20feb2014
* Reason: allow user to affect null line options

* version 1.03  David Fisher  23jul2014
* Reason: implented a couple of suggestions from Phil Jones
* Weighting is now consistent across plotid groups
* Tidying up some code that unnecessarily restricted where user-defined lcols/rcols could be plotted
* Minor bug fixes and code simplification
* New (improved?) textsize and aspect ratio algorithm

* version 1.04  David Fisher 29jun2015
// Reason: Major update to coincide with publication of Stata Journal article

* Aug 2014: fixed issue with _labels
* updated SpreadTitle to accept null strings
* added 'noBOX' option

* Oct 2014: added "newwt" option to "dataid" to reset weights

* Jan 2015: re-written leftWD/rightWD sections to use variable formats and manually-calculated indents
* rather than using char(160), since this isn't necessarily mapped to "non-breaking space" on all machines

* May 2015: Fixed issue with "clipping" long column headings
* May 2015: Option to save parameters (aspect ratio, text size, positioning of text columns relative to x-axis tickmarks)
* in a matrix, to be used by a subsequent -forestplot- call to maintain consistency

* October 2015: Minor fixes to agree with new ipdmetan/admetan versions

* July 2016: added rfdist

* 30th Sep 2016: added "range(min max)" option so that range = min(_LCI) to max(_UCI)

* Coding of _USE:
* _USE == 0  subgroup labels (headings)
* _USE == 1  successfully estimated trial-level effects
* _USE == 2  unsuccessfully estimated trial-level effects ("Insufficient data")
* _USE == 3  subgroup effects
* _USE == 4  between-subgroup heterogeneity info
* _USE == 5  overall effect
* _USE == 6  blank lines/anything else
* _USE == 9  titles

* version 2.0  David Fisher  11may2017
// Not updated nearly as much as -admetan-, -ipdmetan- and -ipdover-
// but up-versioned to match

*! version 2.1  David Fisher  14sep2017
// various bug fixes
// improvements to range() and cirange()
// improvements to rfopts



program define forestplot, sortpreserve rclass

version 10		// metan is v9 and this doesn't use any more recent commands/syntaxes; v10 used only for sake of help file extension

syntax [varlist(numeric max=5 default=none)] [if] [in] [, WGT(string) USE(varname numeric) ///
	/// /* Sub-plot identifier for applying different appearance options, and dataset identifier to separate plots */
	PLOTID(string) DATAID(string) ///
	/// /* General -forestplot- options */
	BY(varname) CUmulative EFORM EFFect(string) LABels(varname string) RFDIST(varlist numeric min=2 max=2) LCOLs(namelist) RCOLs(namelist) ///
	CLASSIC noDIAmonds DP(integer 2) INTERaction KEEPALL LEFTJustify ///
	noBOX noINSUFficient NULLOFF noNAmes NONUll NUll(string) noOVerall /*noPRESERVE*/ noSTATs noSUbgroup noWT noPC ///
	ADMETAN IPDOVER /// /* these two options indicate that forestplot has not been run stand-alone)
	/// /* x-axis options */
	XLABel(passthru) XTICk(passthru) XTItle(string asis) RAnge(passthru) CIRAnge(passthru) FAVours(string asis) FP(real -9) ///
	/// /* left/right column options (to pass through to ProcessColumns; all undocumented, except noadjust) */
	noADJust TArget(passthru) MAXWidth(passthru) MAXLines(passthru) noTRUNCate ///
	/// /* other "fine-tuning" options */
	ADDHeight(real 0) /*(undocumented)*/ ASPECT(real -9) ASText(real -9) BOXscale(real 100.0) CAPTION(string asis) ///
	SPacing(real -9) SUBtitle(string asis) /*TEXTscale(real 100.0)*/ TItle(string asis) XSIZe(real -9) YSIZe(real -9) ///
	NOTE(string asis) SAVEDIMS(name) USEDIMS(name) ///
	* ]
	
	// if forestplot is being run "stand-alone" (i.e. not called from admetan/ipdover), parse eform option
	if trim("`admetan'`ipdover'") == "" {
		cap nois MyGetEFormOpts, `options' `eform'
		if _rc {
			if _rc==1 disp as err "User break"
			else disp as err `"Error in {bf:forestplot.MyGetEFormOpts}"'
			c_local err "noerr"		// tell calling subroutine not to also report an error
			exit _rc
		}
		local eform = cond(`"`r(eform)'"'!=`""', "eform", "")	// convert to simple on/off option
		local options `"`r(options)'"'

		if `"`effect'"'==`""' {
			local effect = cond(`"`r(effect)'"'=="", "Effect", `"`r(effect)'"')
			if `"`interaction'"'!=`""' local effect `"Interact. `effect'"'
		}
	}
	marksample touse, novarlist				// do this immediately, so that -syntax- can be used again
	local graphopts `"`options'"'			// "graph region" options (also includes plotopts for now)
	
	if "`box'"!="" local oldbox "nobox"		// allow global "nobox" for compatibility with -metan-
											// N.B. can't be used with plotid; instead box`p'opts(msymbol(none)) can be used
	* Unpack `usedims'
	local DXwidthChars = -9			// initialize
	local oldTextSize = -9			// initialize
	if `"`usedims'"'!=`""' {
		local DXwidthChars = `usedims'[1, `=colnumb(`usedims', "cdw")']
		local spacing = cond(`spacing'==-9, `usedims'[1, `=colnumb(`usedims', "spacing")'], `spacing')
		local oldPlotAspect = cond(`aspect'==-9, `usedims'[1, `=colnumb(`usedims', "aspect")'], `aspect')
		local xsize = cond(`xsize'==-9, `usedims'[1, `=colnumb(`usedims', "xsize")'], `xsize')
		local ysize = cond(`ysize'==-9, `usedims'[1, `=colnumb(`usedims', "ysize")'], `ysize')
		local oldTextSize = `usedims'[1, `=colnumb(`usedims', "textsize")']
		
		numlist "`DXwidthChars' `spacing' `oldPlotAspect' `xsize' `ysize' `oldTextSize'", min(6) max(6) range(>=0)
	}
	
	* Set up variable names
	if `"`varlist'"'==`""' {		// if not specified, assume "standard" varnames			
		local _ES "_ES"
		local _LCI "_LCI"
		local _UCI "_UCI"
	}
	else {							// else use supplied varnames
		tokenize `varlist'
		local _ES `1'
		local _LCI `2'
		local _UCI `3'
		
		if `"`4'"'!=`""' {
			nois disp as err "Syntax has changed in latest version (ipdmetan v2.0 09may2017)"
			nois disp as err "_WT and _USE should now be specified using options wgt() and use()"
			exit 198
		}
	}
	if `"`wgt'"'!=`""' {			// Possible work may be to sort out naming here; `wgt' but `nowt' for option not to display them
		local 0 `wgt'				// although to be fair, -metan- used the same conventions and nobody seemed to mind
		syntax [varname(numeric default=none)] [, Left]
		local _WT `varlist'
	}
	else local _WT "_WT"
	local _USE = cond(`"`use'"'!=`""', `"`use'"', `"_USE"')

	quietly {
	
		* Set up data to use
		capture confirm numeric var `_USE'
		if _rc {
			if _rc!=7 {			// if `_USE' does not exist
				tempvar _USE
				gen int `_USE' = cond(missing(`_ES', `_LCI', `_UCI'), 2, 1)
			}
			else {
				nois disp as err `"{bf:_USE}: variable {bf:`_USE'} exists but is not numeric"'
				exit 198
			}
		}
		markout `touse' `_USE'
		if `"`overall'"'!=`""' {
			replace `touse'=0 if inlist(`_USE', 4, 5)	// july 2015 - revisit use of _USE==4
		}
		replace `touse'=0 if `"`subgroup'"'!=`""' & `_USE' == 3
		count if `touse'
		if !r(N) {
			nois disp as err "no observations"
			exit 2000
		}
		if trim("`admetan'`ipdover'")=="" return scalar N = r(N)		// only return this if *not* called from admetan/ipdover
		
		tempvar touse2 allobs obs
		gen long `allobs' = _n
		bysort `touse' (`allobs') : gen long `obs' = _n if `touse'
		drop `allobs'
		
		* Check existence of `_ES', `_LCI', `_UCI' (all required)
		foreach x in _ES _LCI _UCI {
			confirm numeric var ``x''
		}
		
		* Check existence of weighting variable (either _WT or _NN)
		capture confirm numeric var `_WT'
		if _rc {
			if _rc!=7 {
				tempvar _WT
				gen `_WT' = 1 if `touse'	// generate as constant if doesn't exist
			}
			else {
				nois disp as err `"{bf:_WT}: variable {bf:`_WT'} exists but is not numeric"'
				exit 198
			}
		}
		local awweight "[aw= `_WT']"
		
		* Check validity of `_USE' (already sorted out existence)
		capture assert !missing(`_ES', `_LCI', `_UCI') if `touse' & `_USE'==1
		local rctemp = _rc
		capture assert missing(`_ES', `_LCI', `_UCI') if `touse' & `_USE'==2
		if `rctemp' | _rc {
			nois disp as err `"at least one effect size is not consistent with {bf:_USE}"'
			exit 198
		}
		if "`keepall'"=="" qui replace `touse' = 0 if `_USE'==2	// "keepall" option (see ipdmetan)

		* Now check that UCI is greater than LCI
		count if `touse' & `_UCI' < `_LCI' & !missing(`_LCI')
		if r(N) {
			replace `_LCI' = . if `touse' & `_UCI' < `_LCI' & !missing(`_LCI')
			replace `_UCI' = . if `touse' & `_UCI' < `_LCI' & !missing(`_LCI')
			nois disp as err _n "Warning: Potential errors in confidence limits; please check"
		}
		
		* Generate ordering variable (reverse sequential, since y axis runs bottom to top)
		cap assert inrange(`_USE', 0, 6) if `touse'
		if _rc {
			nois disp as err `"{bf:_USE}: values of {bf:`_USE'} should be integers between 0 to 6 inclusive"'
			exit _rc
		}
		tempvar id
		bysort `touse' (`obs') : gen int `id' = _N - _n + 1 if `touse'
		
		* Check existence of `labels' and `by'
		foreach x in labels by {
			if `"``x''"'==`""' {
				local X = upper("`x'")
				cap confirm var _`X'
				if !_rc local `x' "_`X'"			// use default varnames if they exist and option not explicitly given
			}
		}
		
		* Sort out `dataid' and `plotid'
		local nd=1
		local 0 `dataid'
		syntax [varname(default=none)] [, NEWwt]
		if `"`varlist'"'!=`""' {
			tab `varlist' if `touse', m
			if r(r)>20 {
				nois disp as err `"variable {bf:`varlist'} (in option {bf:dataid()}) takes too many values"'
				exit 198
			}
			if `"`newwt'"'==`""' {
				local dataid `varlist'
			}
			else {
				local dataid
				tempvar dtobs dataid					// create ordinal version of dataid
				bysort `touse' `varlist' (`obs') : gen long `dtobs' = `obs'[1] if `touse'
				bysort `touse' `dtobs' : gen long `dataid' = (_n==1) if `touse'
				replace `dataid' = sum(`dataid')
				local nd = `dataid'[_N]					// number of `dataid' levels
				label var `dataid' "dataid"
			}
		}
		
		if `"`plotid'"'==`""' {
			tempvar plotid
			gen byte `plotid' = 1 if `touse'	// create plotid as constant if not specified
			local np = 1
		}
		else {
			if trim("`admetan'`ipdover'")!="" disp _n _c	// spacing if following on from ipdmetan (etc.)
			capture confirm var _OVER
			local over = cond(_rc, "", "_OVER")
			
			local 0 `plotid'
			syntax name(name=plname id="plotid") [, List noGRaph]
			local plotid		// clear macro; will want to define a tempvar named plotid

			if "`plname'"!="_n" {
				confirm var `plname'
				tab `plname' if `touse', m
				if r(r)>20 {
					nois disp as err `"variable {bf:`plname'} (in option {bf:plotid()}) takes too many values"'
					exit 198
				}
				if `"`over'"'==`""' {
					count if `touse' & inlist(`_USE', 1, 2) & missing(`plname')
					if r(N) {
						nois disp as err `"Warning: variable {bf:`plname'} (in option {bf:plotid()}) contains missing values"'
						nois disp as err `"{bf:plotid()} groups and/or allocated numeric codes may not be as expected"'
						if "`list'"=="" nois disp as err `"This may be checked using the {bf:list} suboption to {bf:plotid()}"'
					}
				}
			}
			
			* Create ordinal version of plotid...
			gen byte `touse2' = `touse' * inlist(`_USE', 1, 2, 3, 5)
			local plvar `plname'

			// ...extra tweaking if passed through from admetan/ipdover (i.e. _STUDY, and possibly _OVER, exists)
			if inlist("`plname'", "_STUDY", "_n", "_LEVEL", "_OVER") {
				capture confirm var _STUDY
				local study = cond(_rc, "_LEVEL", "_STUDY")
				tempvar smiss
				gen byte `smiss' = missing(`study')
				
				if inlist("`plname'", "_STUDY", "_n") {
					tempvar plvar
					bysort `touse2' `smiss' (`over' `study') : gen long `plvar' = _n if `touse2' & !`smiss'
				}
				else if "`plname'"=="_LEVEL" {
					tempvar plvar
					bysort `touse2' `smiss' `by' (`over' `study') : gen long `plvar' = _n if `touse2' & !`smiss'
				}
			}
			tempvar plobs plotid
			bysort `touse2' `smiss' `plvar' (`obs') : gen long `plobs' = `obs'[1] if `touse2'
			bysort `touse2' `smiss' `plobs' : gen long `plotid' = (_n==1) if `touse2'
			replace `plotid' = sum(`plotid')
			local np = `plotid'[_N]					// number of `plotid' levels
			label var `plotid' "plotid"
			
			* Optionally list observations contained within each plotid group
			if "`list'" != "" {
				sort `obs'
				nois disp as text _n "plotid: observations marked by " as res "`plname'" as text ":"
				forvalues p=1/`np' {
					nois disp as text _n "-> plotid = " as res `p' as text ":"
					nois list `dataid' `_USE' `by' `over' `labels' if `touse2' & `plotid'==`p', table noobs sep(0)
				}
				if `"`graph'"'!=`""' exit
			}
			drop `touse2' `plobs' `smiss'
		}
		

		** SORT OUT LCOLS AND RCOLS
		//  default "lcol1" (if calling from admetan/ipdover) is list of study names, headed "Study ID"
		// unless noNAMES specified, add `labels' to `lcols' (even if blank)
		if `"`names'"'==`""' local lcols = trim(`"`labels' `lcols'"')

		** EFFECT SIZE AND WEIGHT COLUMNS
		//  by default, rcols 1 and 2 are effect sizes and weights
		//  `stats' and `wt' turn these optionally off.
		if `"`eform'"'!=`""' local xexp "exp"
		if `"`rfdist'"'!=`""' {
			tokenize `rfdist'
			args _rfLCI _rfUCI
			cap {
				assert `_rfLCI' <= `_LCI' if !missing(`_rfLCI', `_LCI')
				assert `_rfUCI' >= `_UCI' if !missing(`_rfUCI', `_UCI')
			}
			if _rc {
				nois disp as err "Error in prediction interval data"
				exit _rc
			}
		}

		if `"`stats'"'==`""' {
		
			// determine format
			summ `_UCI' if `touse', meanonly
			local fmtx = max(1, ceil(log10(abs(`xexp'(r(max)))))) + 1 + `dp'
		
			tempvar estText
			gen str `estText' = string(`xexp'(`_ES'), "%`fmtx'.`dp'f") if !missing(`_ES')
			replace `estText' = `estText' + " " if !missing(`estText')
			replace `estText' = `estText' + "(" + string(`xexp'(`_LCI'), "%`fmtx'.`dp'f") + ", " + string(`xexp'(`_UCI'), "%`fmtx'.`dp'f") + ")" ///
				if `touse' & inlist(`_USE', 1, 3, 5)
				
			if "`insufficient'"=="" replace `estText' = "(Insufficient data)" if `touse' & `_USE' == 2

			local f: format `estText'
			tokenize `"`f'"', parse("%s")
			format `estText' %-`2's		// left-justify
			
			// variable label
			if "`effect'" == "" {
				if "`interaction'"!="" local effect "Interaction effect"
				else local effect `"Effect"'
			}
			label var `estText' `"`effect' (`c(level)'% CI)"'
			if "`wt'" != "" local rcols = trim(`"`estText' `rcols'"')
			
			// if rfdist and passed from -admetan-, check that it appears in the correct places
			if `"`rfdist'"'!=`""' {
				if `"`admetan'"'!=`""' {
					cap {
						assert  missing(`_ES')       if inlist(`_USE', 3, 5) & float(`_rfLCI')==float(`_LCI') & float(`_rfUCI')==float(`_UCI')
						assert !missing(`_ES'[_n-1]) if inlist(`_USE', 3, 5) & float(`_rfLCI')==float(`_LCI') & float(`_rfUCI')==float(`_UCI')
					}
					if _rc {
						nois disp as err "Error in prediction interval data"
						exit _rc
					}
				}
					
				// Generate `rfdindent' to send to ProcessXLabs
				// strwid is width of "_ES[_n-1]" as formatted by "%`fmtx'.`dp'f" so it lines up	
				tempvar rfindent
				qui gen `rfindent' = cond(`touse' * missing(`_ES') * !missing(`_rfLCI', `_rfUCI'), ///
					string(`xexp'(`_ES'[_n-1]), "%`fmtx'.`dp'f"), "")
			}
		}
		
		if "`wt'" == "" {		// i.e. unless "nowt" specified
		
			// If `_WT' is int or long, assume `nopc'
			// Otherwise, unless `nopc', assume weights are between [0, 1] (they may be slightly >1 in some situations)
			//  needing to be converted to percentages
			if "`pc'"=="" & !inlist("`: type `_WT''", "int", "long") {
				tempvar weightpc
				gen double `weightpc' = 100*`_WT' if `touse' & inlist(`_USE', 1, 3, 5) & !missing(`_ES')
				format `weightpc' %6.2f
				if `"`: variable label `_WT''"'!=`""' {
					label var `weightpc' `"`: variable label `_WT''"'
				}
				else label var `weightpc' "% Weight"
			}
			else local weightpc `_WT'
			
			// default placement of weights is to right of plot; may be overridden with `left' option (e.g. if weighted by nos. of pts like in ipdover)
			if `"`left'"'!=`""' {
				local lcols = trim(`"`lcols' `weightpc'"')
				local rcols = trim(`"`estText' `rcols'"')
			}
			else local rcols = trim(`"`estText' `weightpc' `rcols'"')
		}
		
		// Test validity of lcols and rcols
		foreach x of local lcols {					// "lcols" has to exist
			cap confirm var `x' 
			if _rc {
				nois disp as err `"variable {bf:`x'} not found in option {bf:lcols()}"'
				exit _rc
			}
		}
		if `"`rcols'"' != `""' {
			foreach x of local rcols {
				cap confirm var `x' 
				if _rc {
					nois disp as err `"variable {bf:`x'} not found in option {bf:rcols()}"'
					exit _rc
				}
			}
		}
		local lcolsN : word count `lcols'
		local rcolsN : word count `rcols'

		// if rfdist, find which column it should appear in, to apply rfindent
		local rfcol=1
		while `"`: word `rfcol' of `rcols''"'!=`"`estText'"' & `rfcol' <= `rcolsN' {
			local ++rfcol
		}
	}	// end quietly
	
	
	** GET MIN AND MAX DISPLAY
	// [comments from metan.ado follow]
	// SORT OUT TICKS- CODE PINCHED FROM MIKE AND FIDDLED. TURNS OUT I'VE BEEN USING SIMILAR NAMES...
	// AS SUGGESTED BY JS JUST ACCEPT ANYTHING AS TICKS AND RESPONSIBILITY IS TO USER!
	
	// N.B. `DXmin', `DXmax' are the left and right co-ords of the graph part
	// These are NOT NECESSARILY the same as the limits of xlabels, xticks etc.
	// e.g. if range() was specified with values outside the limits of xlabels, xticks etc., then DXmin, DXmax == range.
	
	// First, sort out null-line
	local h0 = 0							// default
	
	if trim(`"`nulloff'`nonull'"') != `""' local null "nonull"
	// "nulloff" and "nonull" are permitted alternatives to null(none|off), for compatability with -metan-
	
	else if `"`null'"'!=`""' {
		if `: word count `null'' > 1 {
			nois disp as err `"option {bf:null()} may only contain a single number"'
			exit 198
		}
		capture confirm number `null'
		if !_rc {
			local h0 = `null'
			local null
		}
		else {
			if inlist("`null'", "none", "off") local null "nonull"
			else {
				nois disp as err `"invalid syntax in option {bf:null}"'
				exit 198
			}
		}
	}
	// N.B. `null' now either contains nothing, or "nonull"
	//  and `h0' contains a number (defaulting to 0), denoting where the null-line will be placed if "`null'"==""
	
	// Now find DXmin, DXmax; xticklist, xlablist, xlablim1
	summ `_LCI' if `touse', meanonly
	local DXmin = r(min)				// minimum confidence limit (N.B. if passed thru from -admetan-, will include `_rfLCI' values if present)
	summ `_UCI' if `touse', meanonly
	local DXmax = r(max)				// maximum confidence limit (N.B. if passed thru from -admetan-, will include `_rfUCI' values if present)

	if `"`admetan'"'==`""' & `"`rfdist'"'!=`""' {	// o/w, may not include rfdist values so need to do separately
		summ `_rfLCI' if `touse', meanonly
		local DXmin = min(`DXmin', r(min))
		summ `_rfUCI' if `touse', meanonly
		local DXmax = max(`DXmax', r(max))
	}
	
	cap nois ProcessXLabs `DXmin' `DXmax', `xlabel' `xtick' `range' `cirange' `eform' h0(`h0') `null'
	if _rc {
		if _rc==1 nois disp as err `"User break in {bf:forestplot.ProcessXLabs}"'
		nois disp as err `"Error in {bf:forestplot.ProcessXLabs}"'
		c_local err "noerr"		// tell calling program (admetan or ipdover) not to also report an error
		exit _rc
	}
	local CXmin = r(CXmin)
	local CXmax = r(CXmax)
	local DXmin = r(DXmin)
	local DXmax = r(DXmax)
	local xtitleval = r(xtitleval)
	local xticklist `"`r(xticklist)'"'
	local xtickopts `"`r(xtickopts)'"'
	local xlablist  `"`r(xlablist)'"'
	local xlabcmd   `"`r(xlabcmd)'"'
	local xlabopts  `"`r(xlabopts)'"'
	local null      `"`r(null)'"'
	
	// 6th Sep 2017 for v2.1
	// If `range' is specified, "noadjust" is implied
	// (unless range uses "min", i.e. only the RHS is to be altered)
	local adjust = cond(`"`range'"'==`""', `"`adjust'"', `"`r(adjust)'"')

	// END OF TICKS AND LABELS

	
	** Need to make changes to pre-existing data now, plus adding new obs to the dataset
	//  so use -preserve- before continuing
	preserve
		
	// if multiple plotids, or if dataid(varname, newwt) specified,
	// create dummy obs with global min & max weights, to maintain correct weighting throughout
	if `np' > 1 | `"`newwt'"'!=`""' {		// Amended June 2015
	
		quietly {
			// create new `touse', including new dummy obs
			tempvar toused
			gen byte `toused' = `touse'		
			
			// find global min & max weights, to maintain consistency across subgroups
			if `"`newwt'"'==`""' {		// weight consistent across dataid, so just use locals
				summ `_WT' if `touse' & inlist(`_USE', 1, 2), meanonly	
				local minwt = r(min)
				local maxwt = r(max)
			}
			else {						// multiple min/max weights required, so use tempvars
				tempvar minwt maxwt
				sort `touse' `dataid' `_WT'
				by `touse' `dataid': gen double `minwt' = `_WT'[1] if `touse'
				by `touse' `dataid': gen double `maxwt' = `_WT'[_N] if `touse'
			}
			local oldN = _N
			local newN = `oldN' + 2*`nd'*`np'	// N.B. `nd' indexes `dataid'; `np' indexes `plotid'
			set obs `newN'
			forvalues i=1/`nd' {
				forvalues j=1/`np' {
					local k = `oldN' + (`i'-1)*2*`np' + 2*`j'
					replace `plotid' = `j' in `=`k'-1' / `k'
					replace `_WT' = `minwt' in `=`k'-1'
					replace `_WT' = `maxwt' in `k'
				}
			}
			replace `_USE'   = 1 in `=`oldN' + 1' / `newN'
			replace `touse'  = 0 in `=`oldN' + 1' / `newN'
			replace `toused' = 1 in `=`oldN' + 1' / `newN'
		}	// end quietly
	}
	// these dummy obs are identifiable by "`toused' & !`touse'"
		
	// otherwise, point `toused' to existing `touse'
	else local toused `"`touse'"'

	

	************************
	* LEFT & RIGHT COLUMNS *
	************************

	// Setup: generate tempvars to send to ProcessColumns
	foreach xx in left right {
		local x = substr("`xx'", 1, 1)		// extract "l" from "left" and "r" from "right"

		forvalues i=1/``x'colsN' {		// N.B. if `lcolsN' or `rcolsN'==0, this loop will be skipped
			tempvar `xx'`i'
			local `x'vallist `"``x'vallist' ``xx'`i''"'		// store x-axis positions of columns
				
			local `x'coli : word `i' of ``x'cols'
			local f: format ``x'coli'
			tokenize `"`f'"', parse("%s.,")
			local flen `2'
			assert `"`flen'"'!=`""'					// (temporary?) error trap
			
			capture confirm string var ``x'coli'
			if !_rc {						// if string
				local `xx'LB`i' : copy local `x'coli
				if `"`leftjustify'"'!=`""' local flen = -abs(`flen')
			}
			else {							// if numeric
				tempvar `xx'LB`i'
				if `"`: value label ``x'coli''"'!=`""' {	// if labelled (10th July 2017)
					qui decode ``x'coli', gen(``xx'LB`i'')
				}
				else qui gen str ``xx'LB`i'' = string(``x'coli', "`f'")
				qui replace ``xx'LB`i'' = "" if ``xx'LB`i'' == "."
				
				local colName: variable label ``x'coli'
				if `"`colName'"' == "" & `"``x'coli'"' !=`"`labels'"' local colName = `"``x'coli'"'
				label var ``xx'LB`i'' `"`colName'"'
			}
			local `x'lablist `"``x'lablist' ``xx'LB`i''"'	// store contents (text/numbers) of columns
			local `x'fmtlist `"``x'fmtlist' `flen'"'		// desired max no. of characters based on format
		}
		
		if !`lcolsN' {
			tempvar left1
			local lvallist `"`left1'"'
		}
	}
		
	// astext
	if `"`usedims'"'!=`""' & `astext'==-9 {
		local dxwidcopt `"dxwidthchars(`DXwidthChars')"'
	}
	else {
		local astext = cond(`astext'==-9, 50, `astext')
		assert `astext' >= 0
		local astextopt `"astext(`astext')"'
	}

	// find `lcimin' = left-most confidence limit among the "diamonds" (including prediction intervals)
	tempvar lci2
	qui gen `lci2' = cond(`"`null'"'==`""', cond(`_LCI'>`h0', `h0', ///
		cond(`_LCI'>`CXmin', `_LCI', `CXmin')), cond(`_LCI'>`CXmin', `_LCI', `CXmin'))
		
	if `"`admetan'"'==`""' & `"`rfdist'"'!=`""' {	// in this case _LCI may not include rfdist values so need to do separately
		qui replace `lci2' = cond(`"`null'"'==`""', cond(`_rfLCI'>`h0', `h0', ///
			cond(`_rfLCI'>`CXmin', `_rfLCI', `CXmin')), cond(`_rfLCI'>`CXmin', `_rfLCI', `CXmin'))
	}
		
	summ `lci2' if `touse' & inlist(`_USE', 3, 5), meanonly
	local lcimin = cond(r(N), r(min), cond(`"`null'"'==`""', `h0', `CXmin'))		// modified 28th June 2017
	drop `lci2'

	local oldN = _N
	cap nois ProcessColumns `_USE' if `touse', lrcolsn(`lcolsN' `rcolsN') lcimin(`lcimin') dx(`DXmin' `DXmax') ///
		lvallist(`lvallist') llablist(`llablist') lfmtlist(`lfmtlist') ///
		rvallist(`rvallist') rlablist(`rlablist') rfmtlist(`rfmtlist') rfindent(`rfindent') rfcol(`rfcol') ///
		`dxwidcopt' `astextopt' `adjust' `target' `maxwidth' `maxlines' `truncate'
	if _rc {
		if _rc==1 nois disp as err `"User break in {bf:forestplot.ProcessColumns}"'
		else nois disp as err `"Error in {bf:forestplot.ProcessColumns}"'
		c_local err "noerr"		// tell calling program (admetan or ipdover) not to also report an error
		exit _rc
	}
	local leftWDtot = r(leftWDtot)
	local rightWDtot = r(rightWDtot)
	local AXmin = r(AXmin)
	local AXmax = r(AXmax)
	local astext = r(astext)

	// Titles
	assert missing(`touse') & `_USE'==9 if _n > `oldN'				// `_USE'==9 identifies these extra obs
	qui replace `touse' = 1 if _n > `oldN'
	
	summ `id', meanonly		// `id' is only defined if `touse'
	local maxid = r(max)
	qui replace `id' = `maxid' + _n - `oldN' + 1 if _n > `oldN'		// "+1" leaves a one-line gap between titles & main data
	local borderline = `maxid' + 1 - 0.25
	

	// FIND OPTIMAL TEXT SIZE AND ASPECT RATIOS (given user input)
	// Notes:  (David Fisher, July 2014)
		
	// Let X, Y be dimensions of graphregion (controlled by xsize(), ysize()); x, y be dimensions of plotregion (controlled by aspect()).
	// `approxChars' is the approximate width of the plot, in "character units" (i.e. width of [LHS text + RHS text] divided by `astext')
		
	// Note that a "character unit" is the width of a character relative to its height; 
	//  hence `height' is the approximate height of the plot, in terms of both rows of text (with zero gap between rows) AND "character units".
		
	// If Y/X = `graphAspect'<1, `textSize' is the height of a row of text relative to Y; otherwise it is height relative to X.
	// (Note that the default `graphAspect' = 4/5.5 = 0.73 < 1)
	// We then let `approxChars' = x, and manipulate to find the optimum text size for the plot layout.

	// FEB 2015: `textscale' is deprecated, since it causes problems with spilling on the RHS.
	// Instead, using `spacing' to fine-tune the aspect ratio (and hence the text size)
	//   or use `aspect' to completely user-define the aspect ratio.
		
	//  - Note that this code has been changed considerably from the original -metan- code.

	* Derive graphAspect = Y/X (defaults to 4/5.5  = 0.727 unless specified)
	local xsize = cond(`"`usedims'"'==`""' & `xsize'==-9, 5.5, `xsize')
	local ysize = cond(`"`usedims'"'==`""' & `ysize'==-9, 4, `ysize')
	numlist "`xsize' `ysize'", range(>0)
	local graphAspect = `ysize'/`xsize'

	* Derive basic height,
	*  plus small amounts `xdelta', `ydelta' to take account of the space taken up by titles etc.
	// Assume that, if plot is "full-width", then X = x * xdelta
	//  and that, if plot is "full-height", then Y = y * ydelta
	qui count if `touse'
	local height = r(N)
	qui count if `touse' & `_USE'==9
	local height = cond(r(N), `height' + 1, `height')	// add 1 to height if titles (to take account of gap)	

	// see help title_options
	// local condtitle = 2*(`"`title'"'!=`""') + (`"`subtitle'"'!=`""') + (`"`caption'"'!=`""') + .5*(`"`note'"'!=`""') + `addheight'
	local condtitle = 2*(`"`title'"'!=`""') + 1.5*(`"`subtitle'"'!=`""') + 1.25*(`"`caption'"'!=`""') + 1.25*(`"`note'"'!=`""')
	local condtitle = `condtitle' + (`"`title'"'!=`""' & `"`subtitle'"'!=`""')		// gap between title and subtitle, if both specified
	local condtitle = `condtitle' + 2 + `addheight'									// add 2 for graphregion(margin())
	
	local ydelta = (`height' + `condtitle' + (`"`xlablist'"'!=`""') + (trim(`"`favours'`xtitle'"')!=`""'))/`height'
	local xdelta = (`height' + `condtitle')/`height'		// Oct 2016: check logic of this, why difference in what is added??
	// Notes Feb 2015:
	// - could maybe be improved, but for now `addheight' option (undocumented) allows user to tweak
	// - also think about line widths (thicknesses), can we keep them constant-ish??
	// May 2016: yes, should be quite easy -- choose a reasonable value based on the height, then amend it in the same way as textsize
		
	* Derive `approxChars', `spacing' and `plotAspect'
	// (possibly using saved "dimensions")
	// (for future: investigate using margins to "centre on DXwidth" within graphregion??)
	if `"`usedims'"'==`""' {
		local approxChars = (`leftWDtot' + `rightWDtot')/(`astext'/100)
		
		if `aspect' != -9 {					// user-specified aspect of plotregion
			local spacing = `aspect' * `approxChars' / `height'
			local plotAspect = `aspect'
		}
		else {								// if not user-specified
			local spacing = cond(`spacing' == -9, cond(`height'/`approxChars' <= .5, 2, 1.5), `spacing')
			// if "natural aspect" (`height'/`approxChars') is 2x1 or wider, use double spacing; else use 1.5-spacing
			// (unless user-specified, in which case use that)
			local plotAspect = `spacing' * `height' / `approxChars'
		}
	}
	else {	// if `usedims' supplied
		local approxChars = `leftWDtot' + `rightWDtot' + `DXwidthChars'
		local plotAspect = cond(`aspect'==-9, `spacing'*`height'/`approxChars' /* using `spacing' from `usedims' */, `aspect')
	}
	numlist "`plotAspect' `spacing'", range(>=0)
		
		
	// July 2015
	* Standard approach is now to use `graphAspect' and `plotAspect' to determine `textSize'.
	if `"`usedims'"'==`""' {
		
		// (1) If y/x < Y/X < 1 (i.e. plot takes up full width of "wide" graph) then X = x * xdelta
		//     ==> `textSize' = 100/Y = 100/(X * `graphAspect') = 100/(`xdelta' * `approxChars' * `graphAspect')
		if `graphAspect' <= 1 & `plotAspect' <= `graphAspect' {
			local textSize = 100 / (`xdelta' * `approxChars' * `graphAspect')
		}
		
		// (2) If Y/X < 1 and y/x > Y/X (i.e. plot is less wide than "wide" graph) then Y = y * ydelta
		//     ==> `textSize' = 100/Y = 100/(ydelta * x * `plotAspect') = 100 / (`ydelta' * `approxChars' * `plotAspect')
		else if `graphAspect' <= 1 & `plotAspect' > `graphAspect' {
			local textSize = 100 / (`ydelta' * `approxChars' * `plotAspect')
		}
			
		// (3) If y/x > Y/X > 1 (i.e. plot takes up full height of "tall" graph) then Y = y * ydelta
		//     ==> `textSize' = 100/X = 100 * `graphAspect'/(y * ydelta) = 100 * `graphAspect' / (`ydelta' * `approxChars' * `plotAspect')
		else if `graphAspect' > 1 & `plotAspect' > `graphAspect' {
			local textSize = (100 * `graphAspect') / (`ydelta' * `approxChars' * `plotAspect')
		}
			
		// (4) If Y/X > 1 and y/x < Y/X (i.e. plot is less tall than "tall" graph) then X = x * xdelta
		//     ==> `textSize' = 100/X = 100 / (`xdelta' * `approxChars')
		else if `graphAspect' > 1 & `plotAspect' <= `graphAspect' {
			local textSize = 100 / (`xdelta' * `approxChars')
		}
	}

	* Else if `usedims' supplied:
	* old graphAspect and plotAspect would have been derived using the rules above
	* we immediately know the new plotAspect = `spacing'*`height'/`approxChars' (using new `approxChars')
	* (assuming the height is the same -- come back to this point maybe)
	* So:
	// (1) old y/x < Y/X < 1 ==> plot takes up full width
	// (a) if newplotAspect is wider still (new y/x < old y/x) then it will have to "shrink" (i.e. lose height)
	//     ==> widen newgraphAspect by the same amount?? (minus delta, because that will be constant)
	//     But, since in all cases Y is less than X, `textSize' is based on Y, so should still be correct.
	// (b) if newplotAspect is less wide (new y/x > old y/x) it will fit fine, so again `textSize' will be fine.
		
	// (2) old Y/X < 1, old y/x > Y/X (i.e. old plot is less wide than "wide" graph)
	// (a) if newplotAspect is wider, then everything is fine UNLESS new y/x ends up <Y/X.
	//     However, we're then in case (1)(a) so once newgraphAspect is widened, `textSize' should be fine.
	// (b) if newplotAspect is less wide, it will fit fine, so again `textSize' will be fine.
		
	// (3) If y/x > Y/X > 1 (i.e. plot takes up full height of "tall" graph)
	// (a) if newplotAspect is wider, then everything is fine UNLESS new y/x ends up <Y/X.
	//     ==> need to widen newgraphAspect (minus delta, because that will be constant)
	//     Then if newgraphAspect is still > 1, we're in case (1)(a) again
	//     BUT if newgraphAspect is now < 1, then we'll need to amend `textSize'.
	// (b) if newplotAspect is less wide, it will fit fine, so again `textSize' will be fine.
		
	// (4) If Y/X > 1 and y/x < Y/X (i.e. plot is less tall than "tall" graph) 
	// (a) if newplotAspect is wider, newgraphAspect will ALWAYS need to be widened to avoid "shrinkage"
	//     Then if newgraphAspect is still > 1, we're in case (1)(a) again
	//     BUT if newgraphAspect is now < 1, then we'll need to amend `textSize'.
	// (b) if newplotAspect is less wide, it will have to "expand" (i.e. gain height)	
	//     ==> *reduce* width of newgraphAspect	by the same amount
	//     But, since in all cases X is less than Y, `textSize' is based on X, so should still be correct.
		
	* So, scenarios in which to take action are:
	// (1)(a): increase width of newgraphAspect;
	//         no change to `textSize'
	// (2)(a): check new y/x: if y/x < Y/X then increase width of newgraphAspect;
	//         no change to `textSize'
	// (3)(a): check new y/x: if y/x < Y/X then increase width of newgraphAspect;
	//         then check new Y/X: if <1 then need to amend `textSize'
	// (4)(a): increase width of newgraphAspect;
	//         check new Y/X: if <1 then need to amend `textSize'
	// (4)(b): reduce width of newgraphAspect;
	//         no change to `textSize'		
		
	else {
		local textSize = `oldTextSize'				// tidy this up
			
		// (1a & 2a)
		if `graphAspect' <= 1 & `plotAspect' <= `graphAspect' {
			local graphAspect = `graphAspect' * `plotAspect' / `oldPlotAspect'
			local xsize = `ysize' / `graphAspect'
		}
			
		// (3a, 4a, 4b)
		else if `graphAspect' > 1 & ///
			((`oldPlotAspect' > `graphAspect' & `plotAspect' <= `graphAspect') ///
			| (`oldPlotAspect' <= `graphAspect')) {

			local oldGraphAspect = `graphAspect'
			local graphAspect = `oldGraphAspect' * `plotAspect' / `oldPlotAspect'
			local xsize = `ysize' / `graphAspect'
				
			// 3a, 4a
			if `graphAspect' <= 1 {
				local textSize = `textSize' / `oldGraphAspect'
			}
		}
	}
		
		
	* Notes: for random-effects analyses, sample-size weights, or user-defined (will overwrite the first two)
	if "`wtn'"!="" & `"`note'"'==`""' local note "NOTE: Point estimates are weighted by sample size"
	if `"`note'"'!=`""' {
		local 0 `note'
		syntax anything(name=notetxt) [, SIze(string) * ]
		if "`size'"=="" local size = `textSize'*.75			// use 75% of text size used for rest of plot
		local note `"`notetxt', size(`size') `options'"'
	}
	local graphopts `"`graphopts' aspect(`plotAspect') caption(`caption') note(`note') subtitle(`subtitle') title(`title') xsize(`xsize') ysize(`ysize')"'
		
	// Return useful quantities
	return scalar aspect = `plotAspect'
	return scalar astext = `astext'
	return scalar ldw = `leftWDtot'			// display width of left-hand side
	return scalar rdw = `rightWDtot'		// display width of right-hand side
	local DXwidthChars = (`leftWDtot' + `rightWDtot')*((100/`astext') - 1)
	return scalar cdw = `DXwidthChars'		// display width of centre (i.e. the "data" part of the plot)
	return scalar height = `height'
	return scalar spacing = `spacing'
	return scalar ysize = `ysize'
	return scalar xsize = `xsize'
	return scalar textsize = `textSize'
		
	// If specified, store in a matrix the quantities needed to recreate proportions in subsequent forestplot(s)
	if "`savedims'" != "" {
		mat `savedims' = `DXwidthChars', `spacing', `plotAspect', `ysize', `xsize', `textSize'
		mat colnames `savedims' = cdw spacing aspect ysize xsize textsize
	}

	
	// PLOT COLUMNS OF TEXT (lcols/rcols)
	forvalues i = 1/`lcolsN' {
		local lcolCommands `"`macval(lcolCommands)' || scatter `id' `left`i'' if `touse', msymbol(none) mlabel(`leftLB`i'') mlabcolor(black) mlabpos(3) mlabgap(0) mlabsize(`textSize')"'
	}
	forvalues i = 1/`rcolsN' {
		local rcolCommands `"`macval(rcolCommands)' || scatter `id' `right`i'' if `touse', msymbol(none) mlabel(`rightLB`i'') mlabcolor(black) mlabpos(3) mlabgap(0) mlabsize(`textSize')"'
	}

	// FAVOURS
	if `"`favours'"' != `""' {

		// continue to allow fp as a main option, but deprecate it in documentation
		// documented way is to specify fp() as a suboption to favours()
		local oldfp `fp'
		local 0 `"`favours'"'
		syntax [anything(everything)] [, FP(real -9) *]
		local fp = cond(`fp'==-9 & `oldfp'>0, `oldfp', `fp')
			
		* Parse text
		gettoken leftfav rest : anything, parse("#") quotes
		if `"`leftfav'"'!=`"#"' {
			while `"`rest'"'!=`""' {
				gettoken next rest : rest, parse("#") quotes
				if `"`next'"'==`"#"' continue, break
				local leftfav `"`leftfav' `next'"'
			}
		}
		else local leftfav `""'
		local rightfav = trim(`"`rest'"')
			
		if trim(`"`leftfav'`rightfav'"') != `""' {
			
			// now check for inappropriate options
			local 0 `", `options'"'
			syntax [, FORMAT(string) ANGLE(string) LABGAP(string) LABSTYLE(string) LABSize(string) LABColor(string) * ]
			if `"`options'"' != `""' {
				nois disp as err `"inappropriate suboptions found in {bf:favours()}"'
				exit 198
			}		
			if `"`labsize'"'!=`""' local labsizeopt `"labsize(`labsize')"'
			else local labsizeopt `"labsize(`textSize')"'
			if `"`labgap'"'!=`""' local labgapopt `"labgap(`labgap')"'
			else local labgapopt `"labgap(5)"'
			local favopts `"`labsizeopt' `labgapopt' `format' `angle' `labstyle' `labcolor'"'
			
			local fp = cond(`fp'==-9, `fp', cond(`"`eform'"'!=`""', ln(`fp'), `fp'))	// fp() should be given on same scale as xlabels
			if `fp'>0 {
				local leftfp  = cond(`DXmin'<=-`fp' & `"`leftfav'"'!=`""', `"-`fp' `"`leftfav'"'"', "")
				local rightfp = cond(`fp'<=`DXmax'  & `"`rightfav'"'!=`""', `"`fp' `"`rightfav'"'"', "")
			}
			else {
				local leftfp  = cond(`CXmin'<=`h0' & `"`leftfav'"'!=`""', `"`=`CXmin' + (`h0'-`CXmin')/2' `"`leftfav'"'"', "")
				local rightfp = cond(`CXmax'>=`h0' & `"`rightfav'"'!=`""',   `"`=`h0' + (`CXmax'-`h0')/2' `"`rightfav'"'"', "")
			}

			local favopt = cond(trim(`"`leftfp'`rightfp'"')=="", "", `"xmlabel(`leftfp' `rightfp', noticks labels norescale `favopts')"')
		}		
	}		// end if `"`favours'"' != `""'
	
	// xtitle - uses 'xmlabel' options, not 'title' options!  Parse all 'title' options to give suitable error message
	else if `"`xtitle'"' != `""' {
		local 0 `"`xtitle'"'
		syntax [anything(everything)] [, LABSIZE(string) LABGAP(string) * ]
		local xtitle `"`anything'"'		// added May 2016
			
		local labsizeopt = cond(`"`labsize'"'!=`""', `"labsize(`labsize')"', `"labsize(`textSize')"')
		local labgapopt  = cond(`"`labgap'"'!=`""',  `"labgap(`labgap')"',   `"labgap(5)"')
			
		* Now check for inappropriate options
		local xtitleopts "`options'"
		local 0 `", `options'"'
		syntax [, TSTYle(string) ORIENTation(string) Justification(string) ///
			ALignment(string) LINEGAP(string) WIDTH(string) HEIGHT(string) BOX NOBOX ///
			BColor(string) FColor(string) LStyle(string) LPattern(string) LWidth(string) LColor(string) ///
			BMargin(string) BEXpand(string) PLACEment(string) SIze(string) Margin(string) *]
		if !(`"`tstyle'"'==`""' & `"`orientation'"'==`""' & `"`justification'"'==`""' ///
			& `"`alignment'"'==`""' & `"`linegap'"'==`""' & `"`width'"'==`""' & `"`height'"'==`""' & `"`box'"'==`""' & `"`nobox'"'==`""' /// 
			& `"`bcolor'"'==`""' & `"`fcolor'"'==`""' & `"`lstyle'"'==`""' & `"`lpattern'"'==`""' & `"`lwidth'"'==`""' & `"`lcolor'"'==`""' ///
			& `"`bmargin'"'==`""' & `"`bexpand'"'==`""' & `"`placement'"'==`""' ///
			& `"`size'"'==`""' & `"`margin'"'==`""') {
			nois disp as err `"option {bf:xtitle()} uses suboptions from {bf:xmlabel()}, not {bf:xtitle()}.  see {help axis_label_options}"'
			exit 198
		}
		if `"`margin'"'!=`""' {
			nois disp as err `"please use {bf:labgap()} suboption instead of {bf:margin()} in option {bf:xtitle()}"'
			exit 198
		}
		if `"`size'"'!=`""' {
			nois disp as err `"please use {bf:labsize()} suboption instead of {bf:size()} in option {bf:xtitle()}"'
			exit 198
		}
			
		local xtitleopt `"xmlabel(`xtitleval' `"`xtitle'"', noticks labels `labsizeopt' `labgapopt' `xtitleopts')"'
	}

	// if both `favours' and `xtitle', `favours' takes precedence.  Print text to explain this
	if `"`favours'"'!=`""' & `"`xtitle'"'!=`""' {
		nois disp as err `"Note: both {bf:favours()} and {bf:xtitle()} were specifed; {bf:favours()} will take precedence"'
	}

	// GRAPH APPEARANCE OPTIONS
	cap assert `boxscale' >=0
	if _rc == 9 {
		disp as err `"value of {bf:boxscale()} must be >= 0"'
		exit 125
	}
	else if _rc {
		disp as err `"error in {bf:boxscale()} option"'
		exit _rc
	}
	local boxSize = `boxscale'/150

	summ `id', meanonly			// `id' is only defined if `touse'
	local DYmin = r(min)-1
	local DYmax = r(max)+1
	
	quietly {

		tempvar useno
		gen byte `useno' = `_USE' * inlist(`_USE', 3, 5) if `touse'

		cap confirm var `dataid'
		if _rc {
			tempvar dataid
			gen byte `dataid'=1 if `touse'
		}
		sort `touse' `dataid' `id'
		replace `useno' = `useno'[_n-1] if `useno'<=`useno'[_n-1] & `dataid'==`dataid'[_n-1]	// find the largest value (from 3 & 5) "so far"

		* Flag obs through which the line should be drawn
		tempvar ovLine
		gen float `ovLine'=.		// need this var to exist regardless

		summ `useno' if `touse', meanonly
		if r(max) {
			tempvar olinegroup check ovMin ovMax
			gen int `olinegroup' = (`_USE'==`useno') * (`useno'>0)
			bysort `touse' `dataid' (`id') : replace `olinegroup' = sum(`olinegroup') if inlist(`_USE', 1, 2, 3, 5)	// study obs & pooled results

			* Store min and max values for later plotting
			gen byte `check' = inlist(`_USE', 1, 2)
			bysort `touse' `dataid' `olinegroup' (`check') : replace `check' = `check'[_N]	// only draw oline if there are study obs in the same olinegroup
			replace `ovLine' = `_ES' if `touse' & `check' & `_USE'==`useno' & `useno'>0 & !(`_ES' > `CXmax' | `_ES' < `CXmin')

			sort `touse' `dataid' `olinegroup' `id'
			by `touse' `dataid' `olinegroup' : gen float `ovMin' = `id'[1] -0.5 if `touse' & `_USE'==`useno' & `useno'>0 & !missing(`ovLine')
			by `touse' `dataid' `olinegroup' : gen float `ovMax' = `id'[_N]+0.5 if `touse' & `_USE'==`useno' & `useno'>0 & !missing(`ovLine')
			drop `useno' `olinegroup' `check'
		}

		if `"`cumulative'"'!=`""' replace `_USE' = 1 if `_USE' == 3
		
	}	// END QUIETLY


	
	***************************************
	* Get options and build plot commands *
	***************************************
	
	** "Global" options (includes null line)
	local 0 `", `graphopts'"'
	syntax [, ///
		/// /* standard options */
		BOXOPts(string asis) DIAMOPts(string asis) POINTOPts(string asis) CIOPts(string asis) OLINEOPts(string asis) NLINEOPts(string asis) ///
		/// /* non-diamond and prediction interval options */
		PPOINTOPts(string asis) PCIOPts(string asis) RFOPts(string asis) * ]

	local rest `"`options'"'
	
	
	* SETUP OFF-SCALE ARROWS -- fairly straightforward
	// (include use==3, 5 in case of pciopts/rfopts)
	qui {
		tempvar offscaleL offscaleR
		gen byte `offscaleL' = inlist(`_USE', 1, 3, 5) * (float(`_LCI') < float(`CXmin'))
		gen byte `offscaleR' = inlist(`_USE', 1, 3, 5) * (float(`_UCI') > float(`CXmax') & !missing(`_UCI'))
	
		// rfdist: only applies to use==3, 5
		// BUT may need up to four tempvars in niche cases (e.g. only part of the rfCI is visible)
		// ==> to save on tempvars, only use them if more than one; o/w use local macros
		if `"`rfdist'"'!=`""' {
			local touse3 `"`touse' & inlist(`_USE', 3, 5)"'
			qui count if `touse3'
			if r(N) > 1 {
				tempvar rfLoffscaleL rfRoffscaleR
				gen byte `rfLoffscaleL' = `touse3' * (float(`_rfLCI') < float(`CXmin'))
				gen byte `rfRoffscaleR' = `touse3' * (float(`_rfUCI') > float(`CXmax') & !missing(`_rfUCI'))
				
				qui count if `touse3' & float(`_UCI') < float(`CXmin')
				if r(N) {
					tempvar rfRoffscaleL
					gen byte `rfRoffscaleL' = `touse3' * (float(`_UCI') < float(`CXmin'))
				}
				qui count if `touse3' & float(`_LCI') > float(`CXmax') & !missing(`_LCI')
				if r(N) {
					tempvar rfLoffscaleR
					gen byte `rfLoffscaleR' = `touse3' * (float(`_LCI') > float(`CXmax') & !missing(`_LCI'))
				}
			}
			else if r(N) {
				local rfLoffscaleL `"`touse3' & float(`_rfLCI') < float(`CXmin')"'
				local rfRoffscaleR `"`touse3' & float(`_rfUCI') > float(`CXmax') & !missing(`_rfUCI')"'
				local rfRoffscaleL `"`touse3' & float(`_LCI') > float(`CXmax') & !missing(`_LCI')"'
				local rfLoffscaleR `"`touse3' & float(`_UCI') < float(`CXmin')"'
			}
		}
	}
	

	// Comment from Ross Harris in metan.ado
	// DIAMONDS TAKE FOREVER...I DON'T THINK THIS IS WHAT MIKE DID

	// David Fisher, August 2016:
	// Check in advance for whether "diamonds" are to be used; if not, don't need to generate their coordinates
	if trim(`"`diamonds'`interaction'`pciopts'`ppointopts'"') == `""' {
		local diamchk = 0
		forvalues p = 1/`np' {
			local 0 `", `rest'"'
			syntax [, PPOINT`p'opts(string asis) PCI`p'opts(string asis) * ]
			local diamchk = max(`diamchk', trim(`"`ppoint`p'opts'`pci`p'opts'"') == `""')
		}
	}
	else local diamchk = 1
	
	if `diamchk' {
		qui {
			tempvar DiamLeftX DiamLeftY1 DiamLeftY2 DiamRightX DiamRightY1 DiamRightY2 DiamBottomY DiamTopY DiamTopX DiamBottomX
			gen byte `touse2' = `touse' * inlist(`_USE', 3, 5)

			gen float `DiamLeftX'  = cond(`offscaleL', `CXmin', `_LCI') if `touse2' & float(`_ES') >= float(`CXmin')
			gen float `DiamLeftY1' = cond(`offscaleL', `id' + 0.4*( abs((`CXmin'-`_LCI')/(`_ES'-`_LCI')) ), `id') if `touse2' & float(`_ES') >= float(`CXmin')
			gen float `DiamLeftY2' = cond(`offscaleL', `id' - 0.4*( abs((`CXmin'-`_LCI')/(`_ES'-`_LCI')) ), `id') if `touse2' & float(`_ES') >= float(`CXmin')
			
			gen float `DiamRightX'  = cond(`offscaleR', `CXmax', `_UCI') if `touse2' & float(`_ES') <= float(`CXmax')
			gen float `DiamRightY1' = cond(`offscaleR', `id' + 0.4*( abs((`_UCI'-`CXmax')/(`_UCI'-`_ES')) ), `id') if `touse2' & float(`_ES') <= float(`CXmax')
			gen float `DiamRightY2' = cond(`offscaleR', `id' - 0.4*( abs((`_UCI'-`CXmax')/(`_UCI'-`_ES')) ), `id') if `touse2' & float(`_ES') <= float(`CXmax')
			
			gen float `DiamBottomY' = `id' - 0.4 if `touse2'
			replace   `DiamBottomY' = `id' - 0.4*( abs((`_UCI'-`CXmin')/(`_UCI'-`_ES')) ) if `touse2' & float(`_ES') < float(`CXmin')
			replace   `DiamBottomY' = `id' - 0.4*( abs((`CXmax'-`_LCI')/(`_ES'-`_LCI')) ) if `touse2' & float(`_ES') > float(`CXmax')
			
			gen float `DiamTopY' = `id' + 0.4 if `touse2'
			replace   `DiamTopY' = `id' + 0.4*( abs((`_UCI'-`CXmin')/(`_UCI'-`_ES')) ) if `touse2' & float(`_ES') < float(`CXmin')
			replace   `DiamTopY' = `id' + 0.4*( abs((`CXmax'-`_LCI')/(`_ES'-`_LCI')) ) if `touse2' & float(`_ES') > float(`CXmax')
			
			gen float `DiamTopX' = `_ES' if `touse2'
			replace `DiamTopX' = `CXmin' if `touse2' & float(`_ES') < `CXmin'
			replace `DiamTopX' = `CXmax' if `touse2' & float(`_ES') > `CXmax'
			replace `DiamTopX' = . if `touse2' & (float(`_UCI') < `CXmin' | float(`_LCI') > `CXmax')
			gen float `DiamBottomX' = `DiamTopX'
			
			drop `touse2'
		}
	}
	
	* Now truncate CIs at CXmin/CXmax
	qui {
		gen byte `touse2' = `touse' * inlist(`_USE', 1, 3, 5)

		replace `_LCI' = `CXmin' if `offscaleL'
		replace `_UCI' = `CXmax' if `offscaleR'
		replace `_LCI' = . if `touse2' & float(`_UCI') < float(`CXmin')
		replace `_UCI' = . if `touse2' & float(`_LCI') > float(`CXmax')
		replace `_ES'  = . if `touse2' & float(`_ES')  < float(`CXmin')
		replace `_ES'  = . if `touse2' & float(`_ES')  > float(`CXmax')

		if `"`rfdist'"'!=`""' {
			
			// Standard case:
			tempvar rflci2
			clonevar `rflci2' = `_rfLCI'
			replace `_rfLCI' = . if `touse2' & (`offscaleL' | float(`_rfLCI') < float(`CXmin'))
			replace `_rfUCI' = . if `touse2' & (`offscaleR' | (float(`rflci2') > float(`CXmax') & !missing(`rflci2')))
			drop `rflci2'
			
			replace `_rfLCI' = `CXmin' if `rfLoffscaleL'
			replace `_rfUCI' = `CXmax' if `rfRoffscaleR'
		
			// Niche case:
			// If one end of both CI and rfCI are offscale in same direction,
			// and the other end of the CI is *also* outside the CXmin/CXmax limits (albeit not marked as offscale)
			// (i.e. the only visible piece will be *part of one end* of the rfCI)
			// then that piece of the rfCI needs an arrow pointing *towards* _ES.
			// (This will need checking for again when it comes to constructing the rfplot)
			cap confirm numeric var `rfRoffscaleL'
			if !_rc {
				replace `_rfLCI' = `CXmin' if `touse2' & `rfRoffscaleL'
				replace `_UCI'   = `CXmin' if `touse2' & `rfRoffscaleL'
			}
			else local rfRoffscaleL = 0
			
			cap confirm numeric var `rfLoffscaleR'
			if !_rc {
				replace `_rfUCI' = `CXmax' if `touse2' & `rfLoffscaleR'
				replace `_LCI'   = `CXmax' if `touse2' & `rfLoffscaleR'
			}
			else local rfLoffscaleR = 0
		}
		drop `touse2'
	}

		
	* "Default" options
	local defShape = cond("`interaction'"!="", "circle", "square")
	local defColor = cond("`classic'"!="", "black", "180 180 180")
	local defBoxOpts = `"mcolor("`defColor'") msymbol(`defShape') msize(`boxSize')"'
	if `"`oldbox'"'!=`""' local defBoxOpts `"msymbol(none)"'	// -metan- "nobox" option
	local defCIOpts `"lcolor(black) mcolor(black)"'				// includes "mcolor" for arrows (doesn't affect rspike/rcap)
	local defPointOpts `"msymbol(diamond) mcolor(black) msize(vsmall)"'
	local defOlineOpts `"lwidth(thin) lcolor(maroon) lpattern(shortdash)"'
	local defNlineOpts `"lwidth(thin) lcolor(black)"'
	
	// ...and for "pooled" estimates
	local defShape = cond("`interaction'"!="", "circle", "diamond")
	local defColor "0 0 100"
	local defDiamOpts `"lcolor("`defColor'")"'
	local defPPointOpts `"msymbol("`defShape'") mlcolor("`defColor'") mfcolor("none")"'	// "pooled" point options (alternative to diamond)
	local defPCIOpts `"lcolor("`defColor'") mcolor("`defColor'")"'						// "pooled" CI options (alternative to diamond)
	local defRFOpts `"`defPCIOpts'"'													// prediction interval options (includes "mcolor" for arrows)

	
	* "Default" line style for CIs
	// N.B. Have to do this here, first, as capped lines require a different -twoway- command
	//      rather than simply a different option.
	// Therefore, may as well parse some of the other options too (including disallowed ones)
	
	if `"`rfdist'"'==`""' & `"`rfopts'"'!=`""' {
		nois disp as err `"prediction interval not specified; relevant options will be ignored"'
		local rfopts
	}

	// Same routine applies to study CIs, "pooled" CIs (alternative to diamond), and to prediction intervals:
	foreach plot in ci pci rf {
		local 0 `", ``plot'opts'"'
		syntax [, HORizontal VERTical Connect(string asis) RCAP LColor(string asis) MColor(string asis) LWidth(string asis) MLWidth(string asis) OVerlay * ]
		if `"`horizontal'"'!=`""' | `"`vertical'"'!=`""' {
			nois disp as err `"suboptions {bf:horizontal} and {bf:vertical} not allowed in option {bf:`plot'opts()}"'
			exit 198
		}			
		if `"`connect'"'!=`""' {
			nois disp as err `"suboption {bf:connect()} not allowed in option {bf:`plot'opts()}"'
			exit 198
		}
		if `"`overlay'"'!=`""' & "`plot'"!="rf" {
			nois disp as err `"suboption {bf:overlay} not allowed in option {bf:`plot'opts()}"'
			exit 198
		}
		if `"`lcolor'"'!=`""' & `"`mcolor'"'==`""' {
			local mcolor `lcolor'						// for pc(b)arrow
		}
		if `"`lwidth'"'!=`""' & `"`mlwidth'"'==`""' {
			local mlwidth `lwidth'						// for pc(b)arrow
		}
		local `plot'opts
		foreach opt in mcolor lcolor mlwidth lwidth {
			if `"``opt''"'!=`""' {
				local `plot'opts `"``plot'opts' `opt'(``opt'')"'
			}
		}
		local `plot'opts `"``plot'opts' `options'"'
		local g_overlay "`overlay'"		// "global" overlay option
		
		local uplot = upper("`plot'")
		local `uplot'PlotType = cond("`rcap'"=="", "rspike", "rcap")
	}
	

	* Loop over possible values of `plotid' and test for plot#opts relating specifically to each value
	numlist "1/`np'"
	local plvals=r(numlist)			// need both of these as explicit numlists,
	local pplvals `plvals'			//    for later macro manipulations to remove specific values if necessary
	forvalues p = 1/`np' {

		local 0 `", `rest'"'
		syntax [, ///
			/// /* standard options */
			BOX`p'opts(string asis) DIAM`p'opts(string asis) POINT`p'opts(string asis) CI`p'opts(string asis) OLINE`p'opts(string asis) ///
			/// /* non-diamond and prediction interval options */
			PPOINT`p'opts(string asis) PCI`p'opts(string asis) RF`p'opts(string asis) * ]

		local rest `"`options'"'

		* Check if any options were found specifically for this value of `p'
		if trim(`"`box`p'opts'`diam`p'opts'`point`p'opts'`ci`p'opts'`oline`p'opts'`ppoint`p'opts'`pci`p'opts'`rf`p'opts'"') != `""' {
			
			local pplvals : list pplvals - p			// remove from list of "default" plotids
			
			* OVERALL LINE(S) (if appropriate)
			summ `ovLine' if `plotid'==`p', meanonly
			if r(N) {
				local olinePlot `"`macval(olinePlot)' || rspike `ovMin' `ovMax' `ovLine' if `touse' & `plotid'==`p', `defOlineOpts' `olineopts' `oline`p'opts'"'
			}
			
			* INDIVIDUAL STUDY MARKERS
			local touse2 `"`touse' & `_USE'==1 & `plotid'==`p'"'		// use local, not tempvar, so conditions are copied into plot commands
			qui count if `touse2'
			if r(N) {
			
				* WEIGHTED SCATTER PLOT
				local 0 `", `box`p'opts'"'
				syntax [, MLABEL(string asis) MSIZe(string asis) * ]			// check for disallowed options
				if `"`mlabel'"' != `""' {
					nois disp as err `"suboption {bf:mlabel()} not allowed in option {bf:box`p'opts()}"'
					exit 198
				}
				if `"`msize'"' != `""' {
					nois disp as err `"suboption {bf:msize()} not allowed in option {bf:box`p'opts()}"'
					exit 198
				}
				local scPlotOpts `"`defBoxOpts' `boxopts' `box`p'opts'"'
				summ `_WT' if `touse2', meanonly
				if !r(N) nois disp as err `"No weights found for {bf:plotid}==`p'"'
				else if `nd'==1 local scPlot `"`macval(scPlot)' || scatter `id' `_ES' `awweight' if `toused' & `_USE'==1 & `plotid'==`p', `macval(scPlotOpts)'"'
				else {
					forvalues d=1/`nd' {
						local scPlot `"`macval(scPlot)' || scatter `id' `_ES' `awweight' if `toused' & `_USE'==1 & `plotid'==`p' & `dataid'==`d', `macval(scPlotOpts)'"'
					}
				}		// N.B. scatter if `toused' <-- "dummy obs" for consistent weighting
				
				* CONFIDENCE INTERVAL PLOT
				local 0 `", `ci`p'opts'"'
				syntax [, HORizontal VERTical Connect(string asis) RCAP LColor(string asis) MColor(string asis) LWidth(string asis) MLWidth(string asis) * ]	// check for disallowed options + rcap
				if `"`horizontal'"'!=`""' | `"`vertical'"'!=`""' {
					nois disp as err `"suboptions {bf:horizontal} and {bf:vertical} not allowed in option {bf:ci`p'opts()}"'
					exit 198
				}			
				if `"`connect'"'!=`""' {
					nois disp as err `"suboption {bf:connect()} not allowed in option {bf:ci`p'opts()}"'
					exit 198
				}
				if `"`lcolor'"'!=`""' & `"`mcolor'"'==`""' {
					local mcolor `lcolor'						// for pc(b)arrow
				}
				if `"`lwidth'"'!=`""' & `"`mlwidth'"'==`""' {
					local mlwidth `lwidth'						// for pc(b)arrow
				}
				local CIPlot`p'Opts
				foreach opt in mcolor lcolor mlwidth lwidth {
					if `"``opt''"'!=`""' {
						local CIPlot`p'Opts `"`CIPlot`p'Opts' `opt'(``opt'')"'
					}
				}
				local CIPlot`p'Opts `"`defCIOpts' `ciopts' `CIPlot`p'Opts' `options'"'		// main options first, then options specific to plot `p'
				local CIPlot`p'Type = cond("`rcap'"=="", "`CIPlotType'", "rcap")
				
				// default: both ends within scale (i.e. no arrows)
				local CIPlot `"`macval(CIPlot)' || `CIPlot`p'Type' `_LCI' `_UCI' `id' if `touse2' & !`offscaleL' & !`offscaleR', hor `macval(CIPlot`p'Opts)'"'

				// if arrows required
				qui count if `touse2' & `offscaleL' & `offscaleR'
				if r(N) {													// both ends off scale
					local CIPlot `"`macval(CIPlot)' || pcbarrow `id' `_LCI' `id' `_UCI' if `touse2' & `offscaleL' & `offscaleR', `macval(CIPlot`p'Opts)'"'
				}
				qui count if `touse2' & `offscaleL' & !`offscaleR'
				if r(N) {													// only left off scale
					local CIPlot `"`macval(CIPlot)' || pcarrow `id' `_UCI' `id' `_LCI' if `touse2' & `offscaleL' & !`offscaleR', `macval(CIPlot`p'Opts)'"'
					if "`CIPlot`p'Type'" == "rcap" {			// add cap to other end if appropriate
						local CIPlot `"`macval(CIPlot)' || rcap `_UCI' `_UCI' `id' if `touse2' & `offscaleL' & !`offscaleR', hor `macval(CIPlot`p'Opts)'"'
					}
				}
				qui count if `touse2' & !`offscaleL' & `offscaleR'
				if r(N) {													// only right off scale
					local CIPlot `"`macval(CIPlot)' || pcarrow `id' `_LCI' `id' `_UCI' if `touse2' & !`offscaleL' & `offscaleR', `macval(CIPlot`p'Opts)'"'
					if "`CIPlot`p'Type'" == "rcap" {			// add cap to other end if appropriate
						local CIPlot `"`macval(CIPlot)' || rcap `_LCI' `_LCI' `id' if `touse2' & !`offscaleL' & `offscaleR', hor `macval(CIPlot`p'Opts)'"'
					}
				}

				* POINT PLOT (point estimates -- except if "classic")
				if "`classic'" == "" {
					local pointPlot `"`macval(pointPlot)' || scatter `id' `_ES' if `touse2', `defPointOpts' `pointopts' `point`p'opts'"'
				}
			}			// end if r(N) [i.e. if any obs with _USE==1 & plotid==`p']

			
			* POOLED EFFECT MARKERS
			local touse2 `"`touse' & inlist(`_USE', 3, 5) & `plotid'==`p'"'		// use local, not tempvar, so conditions are copied into plot commands
			qui count if `touse2'			
			if r(N) {
			
				* DIAMONDS; START FROM 9 O'CLOCK AND WORK ROUND
				* Assume diamond if no "pooled point/CI" options, and no "interaction" option
				if trim(`"`ppointopts'`ppoint`p'opts'`pciopts'`pci`p'opts'`interaction'`diamonds'"') == `""' {
					local diamPlotOpts `"`defDiamOpts' `diamopts' `diam`p'opts'"'
					local diamPlot `"`macval(diamPlot)' || pcspike `DiamLeftY1' `DiamLeftX' `DiamTopY' `DiamTopX' if `touse2', `macval(diamPlotOpts)'"'
					local diamPlot `"`macval(diamPlot)' || pcspike `DiamTopY' `DiamTopX' `DiamRightY1' `DiamRightX' if `touse2', `macval(diamPlotOpts)'"'
					local diamPlot `"`macval(diamPlot)' || pcspike `DiamRightY2' `DiamRightX' `DiamBottomY' `DiamBottomX' if `touse2', `macval(diamPlotOpts)'"'
					local diamPlot `"`macval(diamPlot)' || pcspike `DiamBottomY' `DiamBottomX' `DiamLeftY2' `DiamLeftX' if `touse2', `macval(diamPlotOpts)'"'
				}
				
				* POOLED EFFECTS - PPOINT/PCI
				else {
					if `"`diam`p'opts'"'!=`""' {
						nois disp as err `"Note: suboptions for both diamond and pooled point/CI specified for {bf:plotid}==`p';"'
						nois disp as err `"      diamond suboptions will be ignored"'
					}	
				
					// shouldn't need to bother with arrows etc. here, as pooled effect should always be narrower than individual estimates
					// but do it anyway, just in case of non-obvious use case
					local 0 `", `pci`p'opts'"'
					syntax [, HORizontal VERTical Connect(string asis) RCAP LColor(string asis) MColor(string asis) LWidth(string asis) MLWidth(string asis) * ]	// check for disallowed options + rcap
					if `"`horizontal'"'!=`""' | `"`vertical'"'!=`""' {
						nois disp as err `"suboptions {bf:horizontal} and {bf:vertical} not allowed in option{bf:pci`p'opts()}"'
						exit 198
					}			
					if `"`connect'"' != `""' {
						nois disp as err "suboption {bf:connect()} not allowed in option {bf:pci`p'opts}"'
						exit 198
					}
					if `"`lcolor'"'!=`""' & `"`mcolor'"'==`""' {
						local mcolor `lcolor'						// for pc(b)arrow
					}
					if `"`lwidth'"'!=`""' & `"`mlwidth'"'==`""' {
						local mlwidth `lwidth'						// for pc(b)arrow
					}
					local PCIPlot`p'Opts
					foreach opt in mcolor lcolor mlwidth lwidth {
						if `"``opt''"'!=`""' {
							local PCIPlot`p'Opts `"`PCIPlot`p'Opts' `opt'(``opt'')"'
						}
					}
					local PCIPlot`p'Opts `"`defPCIOpts' `pciopts' `PCIPlot`p'Opts' `options'"'		// main options first, then options specific to plot `p'
					local PCIPlot`p'Type = cond("`rcap'"=="", "`PCIPlotType'", "rcap")
					
					// default: both ends within scale (i.e. no arrows)
					local PCIPlot `"`macval(PCIPlot)' || `PCIPlot`p'Type' `_LCI' `_UCI' `id' if `touse2' & !`offscaleL' & !`offscaleR', hor `macval(PCIPlot`p'Opts)'"'

					// if arrows are required
					qui count if `touse2' & `offscaleL' & `offscaleR'
					if r(N) {													// both ends off scale
						local PCIPlot `"`macval(PCIPlot)' || pcbarrow `id' `_LCI' `id' `_UCI' if `touse2' & `offscaleL' & `offscaleR', `macval(PCIPlot`p'Opts)'"'
					}
					qui count if `touse2' & `offscaleL' & !`offscaleR'
					if r(N) {													// only left off scale
						local PCIPlot `"`macval(PCIPlot)' || pcarrow `id' `_UCI' `id' `_LCI' if `touse2' & `offscaleL' & !`offscaleR', `macval(PCIPlot`p'Opts)'"'
						if "`PCIPlot`p'Type'" == "rcap" {			// add cap to other end if appropriate
							local PCIPlot `"`macval(PCIPlot)' || rcap `_UCI' `_UCI' `id' if `touse2' & `offscaleL' & !`offscaleR', hor `macval(PCIPlot`p'Opts)'"'
						}
					}
					qui count if `touse2' & !`offscaleL' & `offscaleR'
					if r(N) {													// only right off scale
						local PCIPlot `"`macval(PCIPlot)' || pcarrow `id' `_LCI' `id' `_UCI' if `touse2' & !`offscaleL' & `offscaleR', `macval(PCIPlot`p'Opts)'"'
						if "`PCIPlot`p'Type'" == "rcap" {			// add cap to other end if appropriate
							local PCIPlot `"`macval(PCIPlot)' || rcap `_LCI' `_LCI' `id' if `touse2' & !`offscaleL' & `offscaleR', hor `macval(PCIPlot`p'Opts)'"'
						}
					}				
					local ppointPlot `"`macval(ppointPlot)' || scatter `id' `_ES' if `touse2', `defPPointOpts' `ppointopts' `ppoint`p'opts'"'
				}
				
				* PREDICTION INTERVAL
				if `"`rfdist'"'==`""' {
					if `"`rf`p'opts'"'!=`""' {
						nois disp as err `"prediction interval not specified; relevant suboptions for {bf:plotid==`p'} will be ignored"'
					}
				}
				else {
					local 0 `", `rf`p'opts'"'
					syntax [, HORizontal VERTical Connect(string asis) RCAP LColor(string asis) MColor(string asis) LWidth(string asis) MLWidth(string asis) OVerlay * ]	// check for disallowed options + rcap
					if `"`horizontal'"'!=`""' | `"`vertical'"'!=`""' {
						nois disp as err `"suboptions {bf:horizontal} and {bf:vertical} not allowed in option {bf:rf`p'opts}"'
						exit 198
					}			
					if `"`connect'"' != `""' {
						nois disp as err `"suboption {bf:connect()} not allowed in option {bf:rf`p'opts()}"'
						exit 198
					}
					if `"`lcolor'"'!=`""' & `"`mcolor'"'==`""' {
						local mcolor `lcolor'						// for pc(b)arrow
					}
					if `"`lwidth'"'!=`""' & `"`mlwidth'"'==`""' {
						local mlwidth `lwidth'						// for pc(b)arrow
					}
					local RFPlot`p'Opts
					foreach opt in mcolor lcolor mlwidth lwidth {
						if `"``opt''"'!=`""' {
							local RFPlot`p'Opts `"`RFPlot`p'Opts' `opt'(``opt'')"'
						}
					}
					local RFPlot`p'Opts `"`defRFOpts' `rfopts' `RFPlot`p'Opts' `options'"'		// main options first, then options specific to plot `p'
					local RFPlot`p'Type = cond("`rcap'"=="", "`RFPlotType'", "rcap")
				
					// if overlay, use same approach as for CI/PCI
					if trim(`"`overlay'`g_overlay'"') != `""' {
						local touse_add `"float(`_rfUCI')>=float(`CXmin') & float(`_rfLCI')<=float(`CXmax') & float(`_rfLCI')!=float(`_rfUCI')"'
				
						// default: both ends within scale (i.e. no arrows)
						local touse3 `"`touse2' & !`rfLoffscaleL' & !`rfRoffscaleR' & `touse_add'"'
						local RFPlot `"`macval(RFPlot)' || `RFPlot`p'Type' `_rfLCI' `_rfUCI' `id' if `touse3', hor `macval(RFPlot`p'Opts)'"'

						// if arrows required
						local touse3 `"`touse2' & `rfLoffscaleL' & `rfRoffscaleR' & `touse_add'"'
						qui count if `touse3'
						if r(N) {													// both ends off scale
							local RFPlot `"`macval(RFPlot)' || pcbarrow `id' `_rfLCI' `id' `_rfUCI' if `touse3', `macval(RFPlot`p'Opts)'"'
						}
						local touse3 `"`touse2' & `rfLoffscaleL' & !`rfRoffscaleR' & `touse_add'"'
						qui count if `touse3'
						if r(N) {													// only left off scale
							local RFPlot `"`macval(RFPlot)' || pcarrow `id' `_rfUCI' `id' `_rfLCI' if `touse3', `macval(RFPlot`p'Opts)'"'
							if "`RFPlotType'" == "rcap" {			// add cap to other end if appropriate
								local RFPlot `"`macval(RFPlot)' || rcap `_rfUCI' `_rfUCI' `id' if `touse3', hor `macval(RFPlot`p'Opts)'"'
							}
						}
						local touse3 `"`touse2' & !`rfLoffscaleL' & `rfRoffscaleR' & `touse_add'"'
						qui count if `touse3'
						if r(N) {													// only right off scale
							local RFPlot `"`macval(RFPlot)' || pcarrow `id' `_rfLCI' `id' `_rfUCI' if `touse3', `macval(RFPlot`p'Opts)'"'
							if "`RFPlotType'" == "rcap" {			// add cap to other end if appropriate
								local RFPlot `"`macval(RFPlot)' || rcap `_rfLCI' `_rfLCI' `id' if `touse3', hor `macval(RFPlot`p'Opts)'"'
							}
						}
					}
					
					// otherwise, need to do it slightly differently, as we are dealing with two separate (left/right) lines
					else {
					
						// identify special cases where only one line required, with two arrows
						local touse3 `"`touse2' & (`rfLoffscaleL' & `rfLoffscaleR') | (`rfRoffscaleL' & `rfRoffscaleR')"'
						qui count if `touse3'
						if r(N) {
							local RFPlot `"`macval(RFPlot)' || pcbarrow `id' `_rfLCI' `id' `_rfUCI' if `touse3', `macval(RFPlot`p'Opts)'"'
						}
						
						// left-hand line
						local touse_add `"float(`_rfLCI')<=float(`CXmax') & float(`_rfLCI')!=float(`_LCI')"'

						local touse3 `"`touse2' & !`rfLoffscaleL' & !`rfLoffscaleR' & !`offscaleL' & `touse_add'"'
						local RFPlot `"`macval(RFPlot)' || `RFPlot`p'Type' `_LCI' `_rfLCI' `id' if `touse3', hor `macval(RFPlot`p'Opts)'"'
						
						local touse3 `"`touse2' & `rfLoffscaleL' & !`rfLoffscaleR' & !`offscaleL' & `touse_add'"'
						qui count if `touse3'
						if r(N) {										// left-hand end off scale
							local RFPlot `"`macval(RFPlot)' || pcarrow `id' `_LCI' `id' `_rfLCI' if `touse3', `macval(RFPlot`p'Opts)'"'
						}

						local touse3 `"`touse2' & !`rfLoffscaleL' & `rfLoffscaleR' & !`offscaleL' & `touse_add'"'
						qui count if `touse3'
						if r(N) {										// right-hand end off scale
							local RFPlot `"`macval(RFPlot)' || pcarrow `id' `_rfLCI' `id' `_LCI' if `touse3', `macval(RFPlot`p'Opts)'"'
						}

						// right-hand line
						local touse_add `"float(`_rfUCI')>=float(`CXmin') & float(`_rfUCI')!=float(`_UCI')"'
						
						local touse3 `"`touse2' & !`rfRoffscaleL' & !`rfRoffscaleR' & !`offscaleR' & `touse_add'"'
						local RFPlot `"`macval(RFPlot)' || `RFPlot`p'Type' `_UCI' `_rfUCI' `id' if `touse3', hor `macval(RFPlot`p'Opts)'"'
						
						local touse3 `"`touse2' & `rfRoffscaleL' & !`rfRoffscaleR' & !`offscaleR' & `touse_add'"'
						qui count if `touse3'
						if r(N) {										// left-hand end off scale
							local RFPlot `"`macval(RFPlot)' || pcarrow `id' `_rfUCI' `id' `_UCI' if `touse3', `macval(RFPlot`p'Opts)'"'
						}

						local touse3 `"`touse2' & !`rfRoffscaleL' & `rfRoffscaleR' & !`offscaleR' & `touse_add'"'
						qui count if `touse3'
						if r(N) {										// right-hand end off scale
							local RFPlot `"`macval(RFPlot)' || pcarrow `id' `_UCI' `id' `_rfUCI' if `touse3', `macval(RFPlot`p'Opts)'"'
						}
					}
				}			// end else [i.e. if rfdist]
			}			// end if r(N) [i.e. if any obs with _USE==3,5 & plotid==`p']
		}		// end if trim(`"`box`p'opts'`diam`p'opts'`point`p'opts'`ci`p'opts'`oline`p'opts'`ppoint`p'opts'`pci`p'opts'"') != `""'
	}		// end forvalues p = 1/`np'

	
	* Find invalid/repeated options
	// any such options would generate a suitable error message at the plotting stage
	// so just exit here with error, to save the user's time
	if regexm(`"`rest'"', "(box|diam|point|ci|oline|ppoint|pci|rf)([0-9]+)opt") {
		local badopt = regexs(1)
		local badp = regexs(2)
		
		if `: list badp in plvals' nois disp as err "option {bf:`badopt'`badp'opts} supplied multiple times; should only be supplied once"
		else nois disp as err `"`badp' is not a valid {bf:plotid} value"'
		exit 198
	}

	local graphopts `rest'		// this is now *just* the standard "twoway" options
								// i.e. the specialist "forestplot" options have been filtered out

					
	* FORM "DEFAULT" TWOWAY PLOT COMMAND (if appropriate)
	// Changed so that FOR WEIGHTED SCATTER each pplval is plotted separately (otherwise weights are messed up)
	// Other (nonweighted) plots can continue to be plotted as before
	if `"`pplvals'"'!=`""' {

		local pplvals2 : copy local pplvals						// copy, just for use in line 1454
		local pplvals : subinstr local pplvals " " ",", all		// so that "inlist" may be used

		* OVERALL LINE(S) (if appropriate)
		summ `ovLine' if inlist(`plotid', `pplvals'), meanonly
		if r(N) {
			local olinePlot `"`macval(olinePlot)' || rspike `ovMin' `ovMax' `ovLine' if `touse' & inlist(`plotid', `pplvals'), `defOlineOpts' `olineopts'"'
		}
		
		* INDIVIDUAL STUDY MARKERS
		local touse2 `"`touse' & `_USE'==1 & inlist(`plotid', `pplvals')"'		// use local, not tempvar, so conditions are copied into plot commands
		qui count if `touse2'
		if r(N) {
		
			* WEIGHTED SCATTER PLOT
			local 0 `", `boxopts'"'
			syntax [, MLABEL(string asis) MSIZe(string asis) * ]	// check for disallowed options
			if `"`mlabel'"' != `""' {
				disp as err "boxopts: option mlabel() not allowed"
				exit 198
			}
			if `"`msize'"' != `""' {
				disp as err "boxopts: option msize() not allowed"
				exit 198
			}
			local scPlotOpts `"`defBoxOpts' `boxopts'"'
			
			if `"`pplvals'"'==`"`plvals'"' {		// if no plot#opts specified, can plot all plotid groups at once
				summ `_WT' if `touse2', meanonly
				if r(N) {
					if `nd'==1 local scPlot `"`macval(scPlot)' || scatter `id' `_ES' `awweight' if `toused' & `_USE'==1 & inlist(`plotid', `pplvals'), `macval(scPlotOpts)'"'
					else {
						forvalues d=1/`nd' {
							local scPlot `"`macval(scPlot)' || scatter `id' `_ES' `awweight' if `toused' & `_USE'==1 & inlist(`plotid', `pplvals') & `dataid'==`d', `macval(scPlotOpts)'"'
						}
					}
				}
			}
			else {		// else, need to plot each group separately to maintain correct weighting (July 2014)
				foreach p of local pplvals2 {
					summ `_WT' if `touse' & `_USE'==1 & `plotid'==`p', meanonly
					if r(N) {
						if `nd'==1 local scPlot `"`macval(scPlot)' || scatter `id' `_ES' `awweight' if `toused' & `_USE'==1 & `plotid'==`p', `macval(scPlotOpts)'"'
						else {
							forvalues d=1/`nd' {
								local scPlot `"`macval(scPlot)' || scatter `id' `_ES' `awweight' if `toused' & `_USE'==1 & `plotid'==`p' & `dataid'==`d', `macval(scPlotOpts)'"'
							}
						}
					}
				}
			}		// N.B. scatter if `toused' <-- "dummy obs" for consistent weighting
			
			* CONFIDENCE INTERVAL PLOT
			// N.B. options already processed
			local CIPlotOpts `"`defCIOpts' `ciopts'"'
			
			// default: both ends within scale (i.e. no arrows)
			local CIPlot `"`macval(CIPlot)' || `CIPlotType' `_LCI' `_UCI' `id' if `touse2' & !`offscaleL' & !`offscaleR', hor `macval(CIPlotOpts)'"'

			// if arrows required
			qui count if `touse2' & `offscaleL' & `offscaleR'
			if r(N) {													// both ends off scale
				local CIPlot `"`macval(CIPlot)' || pcbarrow `id' `_LCI' `id' `_UCI' if `touse2' & `offscaleL' & `offscaleR', `macval(CIPlotOpts)'"'
			}
			qui count if `touse2' & `offscaleL' & !`offscaleR'
			if r(N) {													// only left off scale
				local CIPlot `"`macval(CIPlot)' || pcarrow `id' `_UCI' `id' `_LCI' if `touse2' & `offscaleL' & !`offscaleR', `macval(CIPlotOpts)'"'
				if "`CIPlotType'" == "rcap" {			// add cap to other end if appropriate
					local CIPlot `"`macval(CIPlot)' || rcap `_UCI' `_UCI' `id' if `touse2' & `offscaleL' & !`offscaleR', hor `macval(CIPlotOpts)'"'
				}
			}
			qui count if `touse2' & !`offscaleL' & `offscaleR'
			if r(N) {													// only right off scale
				local CIPlot `"`macval(CIPlot)' || pcarrow `id' `_LCI' `id' `_UCI' if `touse2' & !`offscaleL' & `offscaleR', `macval(CIPlotOpts)'"'
				if "`CIPlotType'" == "rcap" {			// add cap to other end if appropriate
					local CIPlot `"`macval(CIPlot)' || rcap `_LCI' `_LCI' `id' if `touse2' & !`offscaleL' & `offscaleR', hor `macval(CIPlotOpts)'"'
				}
			}

			* POINT PLOT (point estimates -- except if "classic")
			if "`classic'" == "" {
				local pointPlot `"`macval(pointPlot)' || scatter `id' `_ES' if `touse2', `defPointOpts' `pointopts'"'
			}
		}			// end if r(N) [i.e. if any obs with _USE==1 & plotid==`ppvals']
		
		
		* POOLED EFFECT MARKERS
		local touse2 `"`touse' & inlist(`_USE', 3, 5) & inlist(`plotid', `pplvals')"'		// use local, not tempvar, so conditions are copied into plot commands
		qui count if `touse2'			
		if r(N) {

			* DIAMONDS; START FROM 9 O'CLOCK AND WORK ROUND
			* Assume diamond if no "pooled point/CI" options, and no "interaction" option
			if trim(`"`ppointopts'`pciopts'`interaction'`diamonds'"') == `""' {
				local diamPlotOpts `"`defDiamOpts' `diamopts'"'
				local diamPlot `"`macval(diamPlot)' || pcspike `DiamLeftY1' `DiamLeftX' `DiamTopY' `DiamTopX' if `touse2', `macval(diamPlotOpts)'"'
				local diamPlot `"`macval(diamPlot)' || pcspike `DiamTopY' `DiamTopX' `DiamRightY1' `DiamRightX' if `touse2', `macval(diamPlotOpts)'"'
				local diamPlot `"`macval(diamPlot)' || pcspike `DiamRightY2' `DiamRightX' `DiamBottomY' `DiamBottomX' if `touse2', `macval(diamPlotOpts)'"'
				local diamPlot `"`macval(diamPlot)' || pcspike `DiamBottomY' `DiamBottomX' `DiamLeftY2' `DiamLeftX' if `touse2', `macval(diamPlotOpts)'"'
			}
		
			* POOLED EFFECT - PPOINT/PCI
			else {
				if `"`diamopts'"'!=`""' {
					nois disp as err `"Note: suboptions for both diamond and pooled point/CI specified;"'
					nois disp as err `"      diamond suboptions will be ignored"'
				}	

				// N.B. options already processed
				local PCIPlotOpts `"`defPCIOpts' `pciopts'"'
				
				// default: both ends within scale (i.e. no arrows)
				local PCIPlot `"`macval(PCIPlot)' || `PCIPlotType' `_LCI' `_UCI' `id' if `touse2' & !`offscaleL' & !`offscaleR', hor `macval(PCIPlotOpts)'"'

				// if arrows are required
				qui count if `touse2' & `offscaleL' & `offscaleR'
				if r(N) {													// both ends off scale
					local PCIPlot `"`macval(PCIPlot)' || pcbarrow `id' `_LCI' `id' `_UCI' if `touse2' & `offscaleL' & `offscaleR', `macval(PCIPlotOpts)'"'
				}
				qui count if `touse2' & `offscaleL' & !`offscaleR'
				if r(N) {													// only left off scale
					local PCIPlot `"`macval(PCIPlot)' || pcarrow `id' `_UCI' `id' `_LCI' if `touse2' & `offscaleL' & !`offscaleR', `macval(PCIPlotOpts)'"'
					if "`PCIPlotType'" == "rcap" {			// add cap to other end if appropriate
						local PCIPlot `"`macval(PCIPlot)' || rcap `_UCI' `_UCI' `id' if `touse2' & `offscaleL' & !`offscaleR', hor `macval(PCIPlotOpts)'"'
					}
				}
				qui count if `touse2' & !`offscaleL' & `offscaleR'
				if r(N) {													// only right off scale
					local PCIPlot `"`macval(PCIPlot)' || pcarrow `id' `_LCI' `id' `_UCI' if `touse2' & !`offscaleL' & `offscaleR', `macval(PCIPlotOpts)'"'
					if "`PCIPlotType'" == "rcap" {			// add cap to other end if appropriate
						local PCIPlot `"`macval(PCIPlot)' || rcap `_LCI' `_LCI' `id' if `touse2' & !`offscaleL' & `offscaleR', hor `macval(PCIPlotOpts)'"'
					}
				}				
				local ppointPlot `"`macval(ppointPlot)' || scatter `id' `_ES' if `touse2', `defPPointOpts' `ppointopts'"'		
			}
		
			* PREDICTION INTERVAL
			if `"`rfdist'"'!=`""' {
			
				// N.B. options already processed
				local RFPlotOpts `"`defRFOpts' `rfopts'"'
			
				// if overlay, use same approach as for CI/PCI
				if `"`g_overlay'"'!=`""' {
					local touse_add `"float(`_rfUCI')>=float(`CXmin') & float(`_rfLCI')<=float(`CXmax') & float(`_rfLCI')!=float(`_rfUCI')"'
			
					// default: both ends within scale (i.e. no arrows)
					local touse3 `"`touse2' & !`rfLoffscaleL' & !`rfRoffscaleR' & `touse_add'"'
					local RFPlot `"`macval(RFPlot)' || `RFPlotType' `_rfLCI' `_rfUCI' `id' if `touse3', hor `macval(RFPlotOpts)'"'

					// if arrows required
					local touse3 `"`touse2' & `rfLoffscaleL' & `rfRoffscaleR' & `touse_add'"'
					qui count if `touse3'
					if r(N) {													// both ends off scale
						local RFPlot `"`macval(RFPlot)' || pcbarrow `id' `_rfLCI' `id' `_rfUCI' if `touse3', `macval(RFPlotOpts)'"'
					}
					local touse3 `"`touse2' & `rfLoffscaleL' & !`rfRoffscaleR' & `touse_add'"'
					qui count if `touse3'
					if r(N) {													// only left off scale
						local RFPlot `"`macval(RFPlot)' || pcarrow `id' `_rfUCI' `id' `_rfLCI' if `touse3', `macval(RFPlotOpts)'"'
						if "`RFPlotType'" == "rcap" {			// add cap to other end if appropriate
							local RFPlot `"`macval(RFPlot)' || rcap `_rfUCI' `_rfUCI' `id' if `touse3', hor `macval(RFPlotOpts)'"'
						}
					}
					local touse3 `"`touse2' & !`rfLoffscaleL' & `rfRoffscaleR' & `touse_add'"'
					qui count if `touse3'
					if r(N) {													// only right off scale
						local RFPlot `"`macval(RFPlot)' || pcarrow `id' `_rfLCI' `id' `_rfUCI' if `touse3', `macval(RFPlotOpts)'"'
						if "`RFPlotType'" == "rcap" {			// add cap to other end if appropriate
							local RFPlot `"`macval(RFPlot)' || rcap `_rfLCI' `_rfLCI' `id' if `touse3', hor `macval(RFPlotOpts)'"'
						}
					}
				}
				
				// otherwise, need to do it slightly differently, as we are dealing with two separate (left/right) lines
				else {
				
					// identify special cases where only one line required, with two arrows
					local touse3 `"`touse2' & (`rfLoffscaleL' & `rfLoffscaleR') | (`rfRoffscaleL' & `rfRoffscaleR')"'
					qui count if `touse3'
					if r(N) {
						local RFPlot `"`macval(RFPlot)' || pcbarrow `id' `_rfLCI' `id' `_rfUCI' if `touse3', `macval(RFPlotOpts)'"'
					}
					
					// left-hand line
					local touse_add `"float(`_rfLCI')<=float(`CXmax') & float(`_rfLCI')!=float(`_LCI')"'

					local touse3 `"`touse2' & !`rfLoffscaleL' & !`rfLoffscaleR' & !`offscaleL' & `touse_add'"'
					local RFPlot `"`macval(RFPlot)' || `RFPlotType' `_LCI' `_rfLCI' `id' if `touse3', hor `macval(RFPlotOpts)'"'
					
					local touse3 `"`touse2' & `rfLoffscaleL' & !`rfLoffscaleR' & !`offscaleL' & `touse_add'"'
					qui count if `touse3'
					if r(N) {										// left-hand end off scale
						local RFPlot `"`macval(RFPlot)' || pcarrow `id' `_LCI' `id' `_rfLCI' if `touse3', `macval(RFPlotOpts)'"'
					}

					local touse3 `"`touse2' & !`rfLoffscaleL' & `rfLoffscaleR' & !`offscaleL' & `touse_add'"'
					qui count if `touse3'
					if r(N) {										// right-hand end off scale
						local RFPlot `"`macval(RFPlot)' || pcarrow `id' `_rfLCI' `id' `_LCI' if `touse3', `macval(RFPlotOpts)'"'
					}

					// right-hand line
					local touse_add `"float(`_rfUCI')>=float(`CXmin') & float(`_rfUCI')!=float(`_UCI')"'
					
					local touse3 `"`touse2' & !`rfRoffscaleL' & !`rfRoffscaleR' & !`offscaleR' & `touse_add'"'
					local RFPlot `"`macval(RFPlot)' || `RFPlotType' `_UCI' `_rfUCI' `id' if `touse3', hor `macval(RFPlotOpts)'"'
					
					local touse3 `"`touse2' & `rfRoffscaleL' & !`rfRoffscaleR' & !`offscaleR' & `touse_add'"'
					qui count if `touse3'
					if r(N) {										// left-hand end off scale
						local RFPlot `"`macval(RFPlot)' || pcarrow `id' `_rfUCI' `id' `_UCI' if `touse3', `macval(RFPlotOpts)'"'
					}

					local touse3 `"`touse2' & !`rfRoffscaleL' & `rfRoffscaleR' & !`offscaleR' & `touse_add'"'
					qui count if `touse3'
					if r(N) {										// right-hand end off scale
						local RFPlot `"`macval(RFPlot)' || pcarrow `id' `_UCI' `id' `_rfUCI' if `touse3', `macval(RFPlotOpts)'"'
					}
				}
			}		// end if `"`rfdist'"'!=`""'
		}		// end if r(N) [i.e. if any obs with _USE==3,5 & plotid==`ppvals']
	}		// end if `"`pplvals'"'!=`""'
		
	// END GRAPH OPTS
	
	
	// DF: modified to use added line approach instead of pcspike (less complex & poss. more efficient as fewer vars)
	// null line (unless switched off)
	if "`null'" == "" {
		local 0 `", `nlineopts'"'
		syntax [, HORizontal VERTical Connect(string asis) * ]
		if `"`horizontal'"'!=`""' | `"`vertical'"'!=`""' {
			nois disp as err `"suboptions {bf:horizontal} and {bf:vertical} not allowed in option {bf:nlineopts()}"'
			exit 198
		}			
		if `"`connect'"' != `""' {
			nois disp as err `"suboption {bf:connect()} not allowed in option {bf:nlineopts()}"'
			exit 198
		}
		local nullCommand `"|| function y=`h0', horiz range(`DYmin' `borderline') n(2) `defNlineOpts' `options'"'
	}


	
	***************************
	***     DRAW GRAPH      ***
	***************************
	
	// Re-ordered 28th June 2017 so that all twoway options are given together at the end
	
	#delimit ;

	twoway
	
	/* OVERALL AND NULL LINES FIRST (plus plot-specific options) */ 
		`olinePlot' `nullCommand'
	
	/* WEIGHTED SCATTERPLOT BOXES (plus plot-specific options) */ 
	/*  and CONFIDENCE INTERVALS (incl. "offscale" if necessary) */
		`scPlot' `CIPlot'
	
	/* DIAMONDS (or markers+CIs if appropriate) FOR SUMMARY ESTIMATES */
	/* (and Prediction Intervals if appropriate; plus plot-specific options) */
	/*  then last of all PLOT EFFECT MARKERS to clarify */
		`RFPlot' `PCIPlot' `diamPlot' `pointPlot' `ppointPlot' 
	
	/* COLUMN VARIBLES (including effect sizes and weights on RHS by default) */
		`lcolCommands' `rcolCommands'
	
	/* GLOBAL TWOWAY OPTIONS */
		|| , 
	
	/* Y-axis options */
		yscale(range(`DYmin' `DYmax') noline) ylabel(none) ytitle("")
			yline(`borderline', lwidth(thin) lcolor(gs12))
	
	/* X-axis options */
		xscale(range(`AXmin' `AXmax')) xlabel(`xlabcmd', labsize(`textSize') `xlabopts')
			xtitle(" ") legend(off) xtick(`xticklist', `xtickopts')		
	
	/* FAVOURS OR XTITLE */
		`favopt' `xtitleopt'
	
	/* Other twoway options (`graphopts' = user-specified) */
		`graphopts' plotregion(margin(zero)) ;

	#delimit cr

end





program define getWidth, sortpreserve
version 9.0

//	ROSS HARRIS, 13TH JULY 2006
//	TEXT SIZES VARY DEPENDING ON CHARACTER
//	THIS PROGRAM GENERATES APPROXIMATE DISPLAY WIDTH OF A STRING
//  (in terms of the current graphics font)
//	FIRST ARG IS STRING TO MEASURE, SECOND THE NEW VARIABLE

//	PREVIOUS CODE DROPPED COMPLETELY AND REPLACED WITH SUGGESTION
//	FROM Jeff Pitblado

// Updated August 2016 by David Fisher (added "touse" and "replace" functionality)

syntax anything [if] [in] [, REPLACE]

assert `: word count `anything''==2
tokenize `anything'
marksample touse

if `"`replace'"'==`""' {		// assume `2' is newvar
	confirm new variable `2'
	qui gen `2' = 0 if `touse'
}
else {
	confirm numeric variable `2'
	qui replace `2' = 0 if `touse'
}

qui {
	count if `touse'
	local N = r(N)
	tempvar obs
	bys `touse' : gen int `obs' = _n if `touse'
	sort `obs'
	forvalues i = 1/`N'{
		local this = `1'[`i']
		local width: _length `"`this'"'
		replace `2' =  `width' /*+1*/ in `i'	// "+1" blanked out by DF; add back on at point of use if necessary
	}
} // end qui

end



* exit

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




******************
* DF subroutines *
******************


* MyGetEFormOpts
// Basically _get_eformopts plus a bit extra!
// This program is used by -ipdmetan-, admetan- and -forestplot-
// Not all aspects are relevant to all subroutines, but easier to maintain just a single program!

program define MyGetEFormOpts, rclass
	
	// First, parse RR, since we want to stop it being interpreted as RRR by _get_eformopts
	syntax [name(name=cmdname)] , [ RR * ]

	** Estimation command syntax: use standard _check_eformopt
	if `"`cmdname'"'!=`""' {
		_check_eformopt `cmdname', eformopts(`options') soptions
		local eform = cond(`"`s(eform)'"'!=`""', "eform", "")
		local effect  `"`s(str)'"' 
		local summstat = cond(`"`s(opt)'"'==`"eform"', `""', `"`s(opt)'"')
	}
	
	** Non-estimation command syntax:
	// First, try _get_eformopts
	else {
		_get_eformopts, soptions eformopts(`options') allowed(__all__)
		local eform = cond(`"`s(eform)'"'!=`""', "eform", "")
		local effect  `"`s(str)'"' 
		local summstat = cond(`"`s(opt)'"'==`"eform"', `""', `"`s(opt)'"')
	}
	
	// Next, parse `anything' to extract anything that wouldn't usually be interpreted by _get_eformopts
	//  that is: mean differences (`md', `smd', `wmd'); `rd';
	//  `rr' (since this would usually be interpreted as `rrr' by _get_eformopts)
	//  `coef'/`log', and `nohr' & `noshr' (which imply `log')
	// (N.B. do this even if a valid option was found by _get_eformopts, since we still need to check for multiple options)
	local 0 `", `s(options)' `rr'"'
	syntax , [ COEF LOG MD SMD WMD RR RD NOHR NOSHR * ]

	// identify multiple options; exit with error if found
	if `"`summstat'"'!=`""' & `"`md'`smd'`wmd'`rr'`rd'`nohr'`noshr'"'!=`""' {
		opts_exclusive "`summstat' `md' `smd' `wmd' `rr' `rd' `nohr' `noshr'"
	}
	
	if `"`md'`wmd'"'!=`""' {		// MD and WMD are synonyms
		local effect `"WMD"'
		local summstat "wmd"
	}
	else if "`rr'"!="" {
		local effect `"Risk Ratio"'
		local summstat "rr"
		local eform `"eform"'
	}
	else {
		local effect = cond("`smd'"!="", `"SMD"', ///
			cond("`rd'"!="", `"Risk Diff."', ///
			cond("`rr'"!="", `"Risk Ratio"', `"`effect'"')))
		local summstat = cond(`"`summstat'"'==`""', `"`smd'`rd'`rr'"', `"`summstat'"')
	}
	else local summstat = cond(`"`nohr'"'!=`""', "hr", cond(`"`noshr'"'!=`""', "shr", `"`summstat'"'))

	// log always takes priority over eform
	// ==> cancel eform if appropriate
	local log = cond(`"`coef'"'!=`""', `"log"', `"`log'"')				// `coef' is a synonym for `log'
	if `"`log'"'!=`""' & inlist("`summstat'", "rd", "smd", "wmd") {
		nois disp as err "Log option only appropriate with ratio statistics"
		exit 198
	}
	if `"`log'`nohr'`noshr'"'!=`""' {
		local eform
		local log "log"
	}
	
	return local eform    `"`eform'"'
	return local log      `"`log'"'
	return local summstat `"`summstat'"'
	return local effect   `"`effect'"'
	return local options  `"`options'"'

end



* Subroutine to sort out labels and ticks for x-axis, and find DXmin/DXmax (and CXmin/CXmax if different)
* Created August 2016
* Last modified May 2017

program define ProcessXLabs, rclass

	syntax anything [, XLABEL(string) XTICK(string) RAnge(string) CIRAnge(string) EFORM H0(real 0) noNULL]
	
	tokenize `anything'
	args DXmin DXmax
	

	* Parse `range' and `cirange'
	// in both cases, "min" and "max" refer to range of data in terms of LCI, UCI
	// (that is, initial values of `DXmin', `DXmax')
	if "`range'" != `""' {
		tokenize `range'
		cap {
			assert `"`2'"'!=`""'
			assert `"`3'"'==`""'
		}
		if _rc {
			disp as err "option {bf:range()} must contain exactly two elements"
			exit 198
		}
		
		// if "min", "max" used
		if `"`1'"'!=`"min"' return local adjust "noadjust"		// invoke "noadjust" if range specified, unless "min"
		if inlist(`"`1'"', "min", "max") | inlist(`"`2'"', "min", "max") {
			if `"`eform'"'!=`""' {
				forvalues i=1/2 {
					cap confirm number ``i''
					if !_rc local `i' = ln(``i'')
				}
			}
			local range `"`1' `2'"'
			local range = subinstr(`"`range'"', `"min"', `"`DXmin'"', .)
			local range = subinstr(`"`range'"', `"max"', `"`DXmax'"', .)
			numlist "`range'", min(2) max(2) sort
			local range = r(numlist)
			tokenize "`range'"
			args RXmin RXmax
		}	
		
		else {
			if `"`eform'"'!=`""' {
				numlist "`range'", min(2) max(2) range(>0) sort
				local range `"`=ln(`1')' `=ln(`2')'"'
			}
			else {
				numlist "`range'", min(2) max(2) sort
				local range = r(numlist)
			}
			tokenize "`range'"
			args RXmin RXmax
		}
	}
	
	if "`cirange'" != `""' {
		tokenize `cirange'
		cap {
			assert `"`2'"'!=`""'
			assert `"`3'"'==`""'
		}
		if _rc {
			disp as err "option {bf:cirange()} must contain exactly two elements"
			exit 198
		}
		
		// if "min", "max" used
		if inlist(`"`1'"', "min", "max") | inlist(`"`2'"', "min", "max") {
			if `"`eform'"'!=`""' {
				forvalues i=1/2 {
					cap confirm number ``i''
					if !_rc local `i' = ln(``i'')
				}
			}
			local cirange `"`1' `2'"'
			local cirange = subinstr(`"`cirange'"', `"min"', `"`DXmin'"', .)
			local cirange = subinstr(`"`cirange'"', `"max"', `"`DXmax'"', .)
			numlist "`cirange'", min(2) max(2) sort
			local cirange = r(numlist)
			tokenize "`cirange'"
			args CXmin CXmax
		}	

		else {
			if `"`eform'"'!=`""' {
				numlist "`cirange'", min(2) max(2) range(>0) sort
				local cirange `"`=ln(`1')' `=ln(`2')'"'
			}
			else {
				numlist "`cirange'", min(2) max(2) sort
				local cirange = r(numlist)
			}
			tokenize "`cirange'"
			args CXmin CXmax
		}
	}
	
	
	* Parse xlabel if supplied by user
	local 0 `"`xlabel'"'
	syntax [anything(name=xlablist)] , [FORCE FORMAT(string) * ]	
	local xlabopts `"`options'"'
	
	if `"`xlablist'"' != `""' {
		if `"`eform'"'!=`""' {					// assume given on exponentiated scale if "eform" specified, so need to take logs
			numlist "`xlablist'", range(>0)		// in which case, all values must be greater than zero
			local exlablist `xlablist'
			local xlablist
			foreach xi of numlist `exlablist' {
				local xlablist `"`xlablist' `=ln(`xi')'"'
			}
		}
		
		if "`force'"!=`""' {
			if "`cirange'"!="" {
				disp as err `"Note: both {bf:cirange()} and {bf:xlabel(, force)} were specifed; {bf:cirange()} takes precedence"'
			}
			else {
				numlist "`xlablist'", sort
				local n : word count `r(numlist)'
				
				// added Sep 2017 for v2.1
				if `"`range'"'==`""' {
					local RXmin : word 1 of `r(numlist)'		// if `range' not specified, default to "forced" xlab limits
					local RXmax : word `n' of `r(numlist)'
				}
				else {
					local CXmin : word 1 of `r(numlist)'		// otherwise, set `cirange' instead
					local CXmax : word `n' of `r(numlist)'
				}
			}
		}		
	}

	
	* Parse ticks
	// JUN 2015 -- for future: is there any call for allowing FORCE, or similar, for ticks??
	if "`xtick'" != "" {
		local 0 `"`xtick'"'
		syntax [anything(name=xticklist)] , [ * ]	
		local xtickopts `"`options'"'	
	
		if `"`xticklist'"' != `""' {
			if `"`eform'"'!=`""' {						// assume given on exponentiated scale if "eform" specified, so need to take logs
				numlist "`xticklist'", range(>0)		// ...in which case, all values must be greater than zero
				local exticklist `xticklist'
				local xticklist
				foreach xi of numlist `exticklist' {
					local xticklist `"`xticklist' `=ln(`xi')'"'
				}
			}
			else {
				numlist "`xticklist'"
				local xticklist=r(numlist)
			}
		}
	}

	
	* Check validity of user-defined values
	if `"`range'"'!=`""' & `"`cirange'"'!=`""' {
		cap {
			assert `RXmin' <= `CXmin'
			assert `RXmax' >= `CXmax'
		}
		if _rc {
			disp as err "interval defined by {opt cirange()} (or {bf:xlabel(, force)}) must lie within that defined by {opt range()}"
			exit 198
		}
	}

	// changed Sep 2017 for v2.1
	else if `"`cirange'"'==`""' & `"`range'"'!=`""' {
		local CXmin = max(`RXmin', `DXmin')
		local CXmax = min(`RXmax', `DXmax')
	}
	
	// remove null line if lies outside range of x values to be plotted
	if "`null'"=="" & trim("`cirange'`range'`force'")!="" {
		local removeNull = 0
		if `"`cirange'"'!=`""' {
			local removeNull = (`h0' < `CXmin' | `h0' > `CXmax')
		}
		else local removeNull = (`h0' < `RXmin' | `h0' > `RXmax')
		// else local removeNull = (`h0' < `DXmin' | `h0' > `DXmax')

		if `removeNull' {
			nois disp as err "null line lies outside of user-specified x-axis range and will be suppressed"
			local null "nonull"
		}
	}
	return local null `null'

	
	* If xlabel not supplied by user, need to choose sensible values
	// Default is for symmetrical limits, with 3 labelled values including null
	// N.B. First modified from original -metan- code by DF, March 2013
	//  with further improvements by DF, January 2015
	// Last modifed by DF April 2017 to avoid interminable looping if [base]^`mag' = missing

	local xlablim1=0		// init
	if `"`xlablist'"' == `""' {
	
		// If null line, choose values based around `h0'
		// (i.e. `xlabinit1' = `h0'... but `h0' is automatically selected anyway so no need to explicitly define `xlabinit1')
		if "`null'" == "" {
			local xlabinit2 = max(abs(`DXmin' - `h0'), abs(`DXmax' - `h0'))
			local xlabinit "`xlabinit2'"
		}
		
		// if `nulloff', choose values in two stages: firstly based on the midpoint between CXmin and CXmax (`xlab[init|lim]1')
		//  and then based on the difference between CXmin/CXmax and the midpoint (`xlab[init|lim]2')
		else {
			local xlabinit1 = (`DXmax' + `DXmin')/2
			local xlabinit2 = abs(`DXmax' - `xlabinit1')		// N.B. same as abs(`CXmin' - `xlabinit1')
			if float(`xlabinit1') != 0 {
				local xlabinit "`=abs(`xlabinit1')' `xlabinit2'"
			}
			else local xlabinit `xlabinit2'
		}
		assert "`xlabinit'"!=""
		assert "`xlabinit2'"!=""
		assert `: word count `xlabinit'' == ("`null'"!="")*(float(`DXmax')!=-float(`DXmin')) + 1		// should be >= 1
		
		local counter=1
		foreach xval of numlist `xlabinit' {
		
			if `"`eform'"'==`""' {						// linear scale
				local mag = floor(log10(`xval'))
				local xdiff = abs(`xval'-`mag')
				foreach i of numlist 1 2 5 10 {
					local ii = `i' * 10^`mag'
					if missing(`ii') {
						local ii = `=`i'-1' * 10^`mag'
						local xdiff = abs(float(`xval' - `ii'))
						local xlablim = `ii'
						continue, break
					}
					else if abs(float(`xval' - `ii')) <= float(`xdiff') {
						local xdiff = abs(float(`xval' - `ii'))
						local xlablim = `ii'
					}
				}
			}
			else {										// log scale
				local mag = round(`xval'/ln(2))
				local xdiff = abs(`xval' - ln(2))
				forvalues i=1/`mag' {
					local ii = ln(2^`i')
					if missing(`ii') {
						local ii = ln(2^`=`i'-1')
						local xdiff = abs(float(`xval' - `ii'))
						local xlablim = `ii'
						continue, break
					}
					else if abs(float(`xval' - `ii')) <= float(`xdiff') {
						local xdiff = abs(float(`xval' - `ii'))
						local xlablim = `ii'
					}
				}
				
				// if effect is small, use 1.5, 1.33, 1.25 or 1.11 instead, as appropriate
				foreach i of numlist 1.5 `=1/0.75' 1.25 `=1/0.9' {
					local ii = ln(`i')
					if abs(float(`xval' - `ii')) <= float(`xdiff') {
						local xdiff = abs(float(`xval' - `ii'))
						local xlablim = `ii'
					}
				}	
			}
			
			// if nonull, center limits around `xlablim1', which should have been optimized by the above code
			if "`null'" != "" {		// nonull
				if `counter'==1 {
					local xlablim1 = `xlablim'*sign(`xlabinit1')
				}
				if `counter'>1 | `: word count `xlabinit''==1 {
					local xlablim2 = `xlablim'
					local xlablims `"`=`xlablim1'+`xlablim2'' `=`xlablim1'-`xlablim2''"'
				}
			}
			else local xlablims `"`xlablims' `xlablim'"'
			local ++counter
		}	// end foreach xval of numlist `xlabinit'
			
		// if nulloff, don't recalculate CXmin/CXmax
		if "`null'" != "" numlist `"`xlablim1' `xlablims'"'
		else {
			numlist `"`=`h0' - `xlablims'' `h0' `=`h0' + `xlablims''"', sort	// default: limits symmetrical about `h0'
			tokenize `"`r(numlist)'"'

			// if data are "too far" from null (`h0'), take one limit (but not the other) plus null
			//   where "too far" ==> abs(`CXmin' - `h0') > `CXmax' - `CXmin'
			//   (this works whether data are "too far" to the left OR right, since our limits are symmetrical about `h0')
			if abs(`DXmin' - `h0') > `DXmax' - `DXmin' {
				if `3' > `DXmax'      numlist `"`1' `h0'"'
				else if `1' < `DXmin' numlist `"`h0' `3'"'
			}
			else if trim("`range'`cirange'`force'")=="" {		// "standard" situation
				numlist `"`1' `h0' `3'"'
				local DXmin = `h0' - `xlabinit2'
				local DXmax = `h0' + `xlabinit2'
			}
		}
		local xlablist=r(numlist)

	}		// end if "`xlablist'" == ""

	if "`xticklist'" == "" {
		local xticklist `"`xlablist'"'	// if not specified, default to same as labels	
	}

	numlist `"`xlablist' `xticklist'"', sort
	local n : word count `r(numlist)' 
	local XLmin : word 1 of `r(numlist)'
	local XLmax : word `n' of `r(numlist)'
	
	
	* Use symmetrical plot area (around `h0'), unless data "too far" from null
	if trim(`"`range'`cirange'`force'"')==`""' {

		// if "too far", adjust `CXmin' and/or `CXmax' to reflect this
		//   where "too far" ==> max(abs(`CXmin'-`h0'), abs(`CXmax'-`h0')) > `CXmax' - `CXmin'
		if "`null'"=="" {		
			if `h0' - `DXmax' > `DXmax' - `DXmin' {						// data "too far" to the left
				local DXmax = max(`h0' + .5*(`DXmax'-`DXmin'), `XLmax')	// clip the right-hand side
			}	
			if `DXmin' - `h0' > `DXmax' - `DXmin' {						// data "too far" to the right
				local DXmin = min(`h0' - .5*(`DXmax'-`DXmin'), `XLmin')	// clip the left-hand side
			}
			local toofar "toofar"
		}
	
		if `"`toofar'"'==`""' {
			local DXmin = -max(abs(`DXmin'), abs(`DXmax'))
			local DXmax =  max(abs(`DXmin'), abs(`DXmax'))
		}
	}
	
	* Final calculation of DXmin, DXmax
	if trim(`"`RXmin'`RXmax'"')!=`""' {
		numlist `"`RXmin' `RXmax'"', sort
	}
	else {
		numlist `"`DXmin' `DXmax' `XLmin' `XLmax'"', sort
	}
	local n : word count `r(numlist)' 
	local DXmin : word 1 of `r(numlist)'
	local DXmax : word `n' of `r(numlist)'
	
	if trim(`"`CXmin'`CXmax'"')==`""' {
		local CXmin = `DXmin'
		local CXmax = `DXmax'
	}	
	
	// Position of xtitle
	local xtitleval = cond("`xlablist'"=="", `xlablim1', .5*(`CXmin' + `CXmax'))
	return scalar xtitleval = `xtitleval'	
	
	// Return scalars
	return scalar CXmin = `CXmin'
	return scalar CXmax = `CXmax'
	return scalar DXmin = `DXmin'
	return scalar DXmax = `DXmax'
	
	// if log scale, label with exponentiated values
	if `"`eform'"'!=`""' {
		local xlabcmd
		foreach xi of numlist `xlablist' {
			local lbl = cond("`format'"=="", string(exp(`xi')), string(exp(`xi'), "`format'"))
			local xlabcmd `"`xlabcmd' `xi' `"`lbl'"'"'				
		}
	}
	else {
		local xlabcmd `"`xlablist'"'
	
		// If formatting not used here (for string labelling), return it alongside other `xlabopts' to pass to -twoway-
		if `"`format'"'!=`""' local xlabopts `"`xlabopts' format(`format')"'
	}
	
	return local xlablist  `"`xlablist'"'
	return local xticklist `"`xticklist'"'
	return local xlabcmd   `"`xlabcmd'"'
	return local xlabopts  `"`xlabopts'"'
	return local xtickopts `"`xtickopts'"'
	
end
	
	

	
***************************************************


* Process left and right columns -- obtain co-ordinates etc.
program define ProcessColumns, rclass

	syntax varname [if] [in], LRCOLSN(numlist integer >=0) LCIMIN(real) DX(numlist) ///
		[LVALlist(namelist) LLABlist(varlist) LFMTLIST(numlist integer) ///
		 RVALlist(namelist) RLABlist(varlist) RFMTLIST(numlist integer) RFINDENT(varname) RFCOL(integer 1) ///
		 DXWIDTHChars(real -9) ASText(integer -9) ///
		 noADJust TArget(integer 0) MAXWidth(integer 0) MAXLines(integer 0) noTRUNCate ]
	
	marksample touse
	
	// rename locals for clarity
	local _USE         : copy local varlist
	local DXwidthChars : copy local dxwidthchars

	// unpack `lrcolsn' and `dx'
	tokenize `lrcolsn'
	args lcolsN rcolsN
	local rcolsN = cond(`"`rcolsN'"'==`""', 0, `rcolsN')
	
	tokenize `dx'
	args DXmin DXmax
	
	tempvar strlen strwid
	local digitwid : _length 0		// width of a digit (e.g. "0") in current graphics font = roughly average non-space character width
	local spacewid : _length " "	// width of a space in current graphics font

	quietly {
	
		** Left columns
		local leftWDtot = 0
		local nlines = 0
		forvalues i=1/`lcolsN' {
			local leftLB`i' : word `i' of `llablist'
			
			gen long `strlen' = length(`leftLB`i'')
			summ `strlen' if `touse', meanonly
			local maxlen = r(max)		// max length of existing text

			getWidth `leftLB`i'' `strwid'
			summ `strwid' if `touse', meanonly
			local maxwid = r(max)		// max width of existing text
				
			local fmtlen : word `i' of `lfmtlist'
			local leftWD`i' = cond(abs(`fmtlen') <= `maxlen', `maxwid', ///		// exact width of `maxlen' string
				abs(`fmtlen')*`digitwid')										// approx. max width (based on `digitwid')


			** Check whether title string is longer than the data itself
			// If so, potentially allow spread over a suitable number of lines
			// [DF JAN 2015: Future work might be to re-write (incl. SpreadTitle) to use width rather than length??]

			// If more than one lcol, restrict to width of data (i.e. _USE==1, 2).
			// Otherwise, title may be as long as the max string length in the column.
			// [Note that, as the title isn't stored as data (yet), the max string length does NOT account for the title string itself.]
			if `lcolsN'>1 local anduse `" & inlist(`_USE', 1, 2)"'
			summ `strlen' if `touse' `anduse', meanonly
			local maxlen = r(max)
			
			local colName : variable label `leftLB`i''
			if `"`colName'"'!=`""' {
				
				local target_opt = cond(`target', `target', max(abs(`fmtlen'), `maxlen'))
				local maxwidth_opt = cond(`maxwidth', `maxwidth', `=2*`target_opt'')
				SpreadTitle `"`colName'"', target(`target_opt') maxwidth(`maxwidth_opt') maxlines(`maxlines') `truncate'
				
				if `r(nlines)' > `nlines' {
					local oldN = _N
					set obs `=`oldN' + `r(nlines)' - `nlines''
					local nlines = r(nlines) 
				}
				local l = `nlines' - `r(nlines)'
				forvalues j = `r(nlines)'(-1)1 {
					local k = _N - (`j' + `l') + 1
					replace `leftLB`i'' = `"`r(title`j')'"' in `k'
					replace `_USE' = 9 in `k'
					replace `touse' = 1 in `k'
				}
				
				getWidth `leftLB`i'' `strwid', replace			// re-calculate `strwid' to include titles

				summ `strwid' if `touse', meanonly
				local maxwid = r(max)
				local leftWD`i' = max(`leftWD`i'', `maxwid')	// in case title is necessarily longer than the variable, even after SpreadTitle
			}
			
			tempvar lindent`i' 													// for right-justifying text
			gen `lindent`i'' = cond(`fmtlen'>0, `leftWD`i'' - `strwid', 0)		// indent if right-justified
			
			local leftWD`i' = `leftWD`i'' + (2 - (`i'==`lcolsN'))*`digitwid'	// having calculated the indent, add a buffer (2x except for last col)
			local leftWDtot = `leftWDtot' + `leftWD`i''							// running calculation of total width (including titles)

			drop `strlen' `strwid'
		}		// end of forvalues i=1/`lcolsN'
			
			
		** Right columns
		local rightWDtot = 0
		forvalues i=1/`rcolsN' {		// if `rcolsN'==0, loop will be skipped
			local rightLB`i' : word `i' of `rlablist'

			gen long `strlen' = length(`rightLB`i'')
			summ `strlen' if `touse', meanonly
			local maxlen = r(max)		// max length of existing text

			getWidth `rightLB`i'' `strwid'
			summ `strwid' if `touse', meanonly		
			local maxwid = r(max)		// max width of existing text

			local fmtlen : word `i' of `rfmtlist'
			local rightWD`i' = cond(abs(`fmtlen') <= `maxlen', `maxwid', ///	// exact width of `maxlen' string
				abs(`fmtlen')*`digitwid')										// approx. max width (based on `digitwid')


			** Check whether title string is longer than the data itself
			// If so, potentially allow spread over a suitable number of lines
			// [DF JAN 2015: Future work might be to re-write (incl. SpreadTitle) to use width rather than length??]

			local colName : variable label `rightLB`i''
			if `"`colName'"'!=`""' {
				
				local target_opt = cond(`target', `target', max(abs(`fmtlen'), `maxlen'))
				local maxwidth_opt = cond(`maxwidth', `maxwidth', `=2*`target_opt'')
				SpreadTitle `"`colName'"', target(`target_opt') maxwidth(`maxwidth_opt') maxlines(`maxlines') `truncate'

				if `r(nlines)' > `nlines' {
					local oldN = _N
					set obs `=`oldN' + `r(nlines)' - `nlines''
					local nlines = r(nlines) 
				}
				local l = `nlines' - `r(nlines)'
				forvalues j = `r(nlines)'(-1)1 {
					local k = _N - (`j' + `l') + 1
					replace `rightLB`i'' = `"`r(title`j')'"' in `k'
					replace `_USE' = 9 in `k'
					replace `touse' = 1 in `k'
				}
				getWidth `rightLB`i'' `strwid', replace			// re-calculate `strwid' to include titles
					
				summ `strwid' if `touse', meanonly
				local maxwid = r(max)
				local rightWD`i' = max(`rightWD`i'', `maxwid')		// in case title is necessarily longer than the variable, even after SpreadTitle
			}
			
			tempvar rindent`i' 													// for right-justifying text
			gen `rindent`i'' = .
			
			// rfdist: strwid is width of "_ES[_n-1]" as formatted by "%`fmtx'.`dp'f" so it lines up
			if `"`rfindent'"'!=`""' & `i'==`rfcol' {
				getWidth `rfindent' `rindent`i'' if `touse' & !missing(`rfindent'), replace
				replace `rindent`i'' = `rindent`i'' + `spacewid' if `touse' & !missing(`rfindent')
			}
			replace `rindent`i'' = cond(`fmtlen'>0, `rightWD`i'' - `strwid', 0) if `touse' & missing(`rindent`i'')		// indent if right-justified
			
			local rightWD`i' = `rightWD`i'' + (2 - (`i'==`rcolsN'))*`digitwid'		// having calculated the indent, add a buffer (2x except for last col)
			local rightWDtot = `rightWDtot' + `rightWD`i''							// running calculation of total width (incl. buffer)
			drop `strlen' `strwid'
		}														// end of forvalues i=1/`rcols'

		local rightWDtot = `rightWDtot' + 2*`digitwid'			// add an extra 1x buffer before first RHS column and after last
		local rightWDtot = max(`rightWDtot', `digitwid')		// in case of no `rcols'

		// Unless noadjust, compare current width of left-hand-side text (`leftWDtot')
		//  to the width *excluding* "long headers" (and het. text if on separate line, _USE==4) (`leftWDtotNoTi')
		//  and work out how far into plot this text (i.e. the "long headers") can extend, without overwriting the graph data
		// (N.B. this needs both the left and right columns to have already had their first processing, for which see previous lines)
		if "`adjust'" == "" {
			
			// Re-calculate widths of `lcols' for observations *other* than study estimates (i.e. _USE==0, 3, 4, 5)
			local leftWDtotNoTi = 0			// initialize
			local adjustTot = 0				// initialize
			
			local leftWD0 = 0				// initialize
			local leftWD0NoTi = 0			// initialize
			local adjustNew = 0				// initialize
			
			forvalues i=1/`lcolsN' {

				// firstly, see if there is anything in "non-study" rows
				gen long `strlen' = length(`leftLB`i'')
				summ `strlen' if `touse' & inlist(`_USE', 0, 3, 4, 5), meanonly
				local maxlenTi = r(max)

				// ...if there is, cancel previous adjustment (`adjustOld')
				local adjustOld = `adjustNew'		// update
				local adjustNew = 0					// reset
				
				if `maxlenTi' & `adjustOld' {
					local adjustOld = 0
					local --adjustTot
					local leftWD`=`i'-1'NoTi = `leftWD`=`i'-1''
				}
				local leftWDtotNoTi = `leftWDtotNoTi' + `leftWD`=`i'-1'NoTi'	// running calculation of total width (incl. buffer), but one iteration behind
				
				// initialize for this iteration
				local fmtlen : word `i' of `lfmtlist'	// desired max no. of characters based on format -- also shows whether left- or right-justified
				local leftWD`i'NoTi = `leftWD`i''
				tempvar lindent`i'NoTi					// for right-justifying text (study-name rows only)
				gen `lindent`i'NoTi' = `lindent`i''
				
				// If no previous adjustment AND if current column left-justified (4th Nov 2016 -- why only if left-j?),
				// compare "total width" with "width for study estimates only" for current column only
				// (including titles, UNLESS last column
				//  so that, if multiple columns, adjusted width of first column includes title so that second column doesn't obscure it)
				if !`adjustOld' & `fmtlen'<=0 {
				
					summ `strlen' if `touse' & inlist(`_USE', 1, 2) | (`i'<`lcolsN' & `_USE'==9), meanonly
					if !r(N) local leftWD`i'NoTi = 0		// if summary diamonds only (added Sep 2017 for v2.1)

					else {
						local maxlen = r(max)		// max length of text for study estimates only
						
						getWidth `leftLB`i'' `strwid'
						summ `strwid' if `touse' & inlist(`_USE', 1, 2) | (`i'<`lcolsN' & `_USE'==9), meanonly
						local maxwid = r(max)		// max width of text for study estimates only
						
						local leftWD`i'NoTi = cond(abs(`fmtlen') <= `maxlen', `maxwid', ///		// exact width of `maxlen' string
							abs(`fmtlen')*`digitwid')											// approx. max width (based on `digitwid')
						
						replace `lindent`i'NoTi' = cond(`fmtlen'>0, `leftWD`i'NoTi' - `strwid', 0)	// indent if right-justified
						drop `strwid'
					}
					local leftWD`i'NoTi = `leftWD`i'NoTi' + (2 - (`i'==`lcolsN'))*`digitwid'	// having calculated the indent, add a buffer (2x except for last col)

					local adjustNew = (`leftWD`i'NoTi' < `leftWD`i'')
					if `adjustNew' local ++adjustTot

				}	
				drop `strlen'
			}
			
			assert `adjustTot' >= 0		// temp error trap
			local leftWDtotNoTi = `leftWDtotNoTi' + `leftWD`lcolsN'NoTi'	// add final iteration
					
			
			// If appropriate, allow _USE=0,3,4,5 to extend into main plot by (lcimin-DXmin)/DXwidth
			//  where `lcimin' is the left-most confidence limit among the "diamonds" (including prediction intervals)
			// i.e. 1 + ((`lcimin'-`DXmin')/`DXwidth') * ((100-`astext')/`astext')) is the percentage increase
			// to apply to (`leftWDtot'+`rightWDtot')/(`newleftWDtot'+`rightWDtot').
			// Then rearrange to find `newleftWDtot'.
			if `leftWDtotNoTi' < `leftWDtot' {

				// sort out astext... need to do this now, but will be recalculated later (line 890)
				if `DXwidthChars'!=-9 & `astext'==-9 {
					local astext2 = (`leftWDtot' + `rightWDtot')/`DXwidthChars'
					local astext = 100 * `astext2'/(1 + `astext2')
				}
				else {
					local astext = cond(`astext'==-9, 50, `astext')
					assert `astext' >= 0
					local astext2 = `astext'/(100 - `astext')
				}
				
				// define some additional locals to make final formula clearer
				local totWD = `leftWDtot' + `rightWDtot'
				local lciWD = (`lcimin' - `DXmin')/(`DXmax' - `DXmin')
				local newleftWDtot = cond(`DXwidthChars'==-9, ///
					(`totWD' / ((`lciWD'/`astext2') + 1)) - `rightWDtot', ///
					`leftWDtot' - `lciWD'*`DXwidthChars')
					
				// BUT don't make leftWDtot any *less* than `leftWDtotNoTi', unless there are no obs with inlist(`_USE', 1, 2)
				count if `touse' & inlist(`_USE', 1, 2)
				local leftWDtot = cond(r(N), max(`leftWDtotNoTi', `newleftWDtot'), `newleftWDtot')
				
				forvalues i=1/`lcolsN' {
					local leftWD`i' = `leftWD`i'NoTi'			// replace old values with new
					replace `lindent`i'' = `lindent`i'NoTi'
				}
			}
		}		// end if "`adjust'" == ""

		// Generate position of columns, using `astext' (% of graph width taken by text)
		// N.B. although the "starting positions", `leftWD`i'' and `rightWD`i'', are constants, there will be indents if right-justified
		//      and anyway, all will need to be stored in variables for use with -twoway-
		
		// sep 2017
		if `DXwidthChars'!=-9 & (`astext'==-9 | `"`newleftWDtot'"'!=`""') {
			local astext2 = (`leftWDtot' + `rightWDtot')/`DXwidthChars'
			local astext = 100 * `astext2'/(1 + `astext2')
		}
		else {
			local astext = cond(`astext'==-9, 50, `astext')
			assert `astext' >= 0
			local astext2 = `astext'/(100 - `astext')
		}
		local textWD = `astext2' * (`DXmax' - `DXmin')/(`leftWDtot' + `rightWDtot')

		local leftWDruntot = 0
		forvalues i = 1/`lcolsN' {
			local left`i' : word `i' of `lvallist'
			gen double `left`i'' = `DXmin' - (`leftWDtot' - `leftWDruntot' - `lindent`i'')*`textWD'
			local leftWDruntot = `leftWDruntot' + `leftWD`i''
		}
		if !`lcolsN' {		// Added July 2015
			local left1 : word 1 of `lvallist'
			gen `left1' = `DXmin' - 2*`digitwid'*`textWD'
		}
		local rightWDruntot = `digitwid'
		forvalues i = 1/`rcolsN' {				// if `rcolsN'=0 then loop will be skipped
			local right`i' : word `i' of `rvallist'
			gen double `right`i'' = `DXmax' + (`rightWDruntot' + `rindent`i'')*`textWD'
			local rightWDruntot = `rightWDruntot' + `rightWD`i''
		}		
	}		// end quietly

	// AXmin AXmax ARE THE OVERALL LEFT AND RIGHT COORDS
	summ `left1' if `touse', meanonly
	local AXmin = r(min)
	local AXmax = `DXmax' + `rightWDtot'*`textWD'
	
	return scalar leftWDtot = `leftWDtot'
	return scalar rightWDtot = `rightWDtot'
	return scalar AXmin = `AXmin'
	return scalar AXmax = `AXmax'
	return scalar astext = `astext'

end
	
	

* Subroutine to "spread" titles out over multiple lines if appropriate
* (copied from ipdmetan)
// Updated July 2014
// August 2016: identical program now used here, in admetan.ado, and in ipdover.ado
// May 2017: updated to accept substrings delineated by quotes (c.f. multi-line axis titles)
// August 2017: updated for better handling of maxlines()

// subroutine of ProcessColumns

program define SpreadTitle, rclass

	syntax anything(name=title id="title string"), [TArget(integer 0) MAXWidth(integer 0) MAXLines(integer 0) noTRUNCate ]
	* Target = aim for this width, but allow expansion if alternative is wrapping "too early" (i.e before line is adequately filled)
	//         (may be replaced by `titlelen'/`maxlines' if `maxlines' and `notruncate' are also specified)
	* Maxwidth = absolute maximum width
	* Maxlines = maximum no. lines (default 3)
	* noTruncate = don't truncate final line if "too long" (even if greater than `maxwidth')
	//             (also allows `maxlines' to adjust `target' upwards if necessary)

	if `"`title'"'==`""' {
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
	
	// Finalise `target' and calculate `spread'
	local maxlines_def = cond(`maxlines', `maxlines', 3)	// use a default value for `maxlines' of 3 in these calculations
	local titlelen = length(`title')
	local target = cond(`target', `target', ///
		cond(`maxwidth', min(`maxwidth', `titlelen'/`maxlines_def'), `titlelen'/`maxlines_def'))
	local spread = min(int(`titlelen'/`target') + 1, `maxlines_def')


	** If substrings are present, delineated by quotes, treat this as a line-break
	// Hence, need to first process each substring separately and obtain parameters,
	// then select the most appropriate overall parameters given the user-specified options,
	// and finally create the final line-by-line output strings.
	
	local line = 0
	local rest `title'

	while `"`rest'"'!=`""' {
		gettoken title rest : rest, bind qed(qed)
		if !`qed' {
			local title `"`title'`rest'"'
			local rest
		}
		
		local ++line
		local title`line' = word(`"`title'"', 1)
		local newwidth = length(`"`title`line''"')

		local count = 2
		local next = word(`"`title'"', `count')
		
		while `"`next'"' != "" {
			local check = trim(`"`title`line''"' + " " +`"`next'"')			// (potential) next iteration of `title`line''
			if length(`"`check'"') > `titlelen'/`spread' {					// if longer than ideal...
																			// ...and further from target than before, or greater than maxwidth
				if abs(length(`"`check'"')-(`titlelen'/`spread')) > abs(length(`"`title`line''"')-(`titlelen'/`spread')) ///
						| (`maxwidth' & length(`"`check'"') > `maxwidth') {
					if `maxlines' & `line'==`maxlines'  {					// if reached max no. of lines
						local title`line' `"`check'"'						//   - use next iteration anyway (to be truncated)
						continue, break										//   - break loop
					}
					else {													// otherwise:
						local ++line										//  - new line
						local title`line' `"`next'"'						//  - begin new line with next word
					}
				}
				else local title`line' `"`check'"'		// else use next iteration
				
			}
			else local title`line' `"`check'"'		// else use next iteration

			local ++count
			local next = word(`"`title'"', `count')
			local newwidth = max(`newwidth', length(`"`title`line''"'))		// update `newwidth'
		}																	// (N.B. won't be done if reached max no. of lines, as loop broken)
		
		if `maxlines' & `line'==`maxlines' continue, break					// break out of outer loop too
	}		
		

	* If last string is too long (including in above case), truncate
	if `newwidth' > `target' & "`truncate'"=="" {
		local maxwidth = cond(`maxwidth', min(`newwidth', `maxwidth'), `newwidth')
		if length(`"`title`line''"') > `maxwidth' local title`line' = substr(`"`title`line''"', 1, `maxwidth')
	}
	
	* Return strings
	forvalues i=1/`line' {
		return local title`i' = trim(`"`title`i''"')
	}
	return scalar nlines = `line'
	return scalar maxwidth = min(`newwidth', `maxwidth')
	
end

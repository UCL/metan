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

*! version 1.04  David Fisher 29jun2015
* Reason: Major update to coincide with publication of Stata Journal article

* Aug 2014: fixed issue with _labels
* updated SpreadTitle to accept null strings
* added 'noBOX' option

* Oct 2014: added "newwt" option to "dataid" to reset weights

* Jan 2015: re-written leftWD/rightWD sections to use variable formats and manually-calculated indents
* rather than using char(160), since this isn't necessarily mapped to "non-breaking space" on all machines

* May 2015: Fixed issue with "clipping" long column headings
* May 2015: Option to save parameters (aspect ratio, text size, positioning of text columns relative to x-axis tickmarks)
* in a matrix, to be used by a subsequent -forestplot- call to maintain consistency


* Coding of _USE:
* _USE == 0  subgroup labels (headings)
* _USE == 1  successfully estimated trial-level effects
* _USE == 2  unsuccessfully estimated trial-level effects ("Insufficient data")
* _USE == 3  subgroup effects
* _USE == 4  between-subgroup heterogeneity info
* _USE == 5  overall effect
* _USE == 6  blank lines
* _USE == 9  titles


program define forestplot, sortpreserve rclass

version 10		// metan is v9 and this doesn't use any more recent commands/syntaxes; v10 used only for sake of help file extension

syntax [namelist(min=3 max=5)] [if] [in] [, ///
	/// /* Sub-plot identifier for applying different appearance options, and dataset identifier to separate plots */
	PLOTID(string) DATAID(string) ///
	/// /* -forestplot- options "passed through" from (e.g.) ipdmetan */
	CUMULative /*IPDOVER*/ DISPNN ///
	/// /* General -forestplot- options */
	BY(name) noBOX CLASSIC DP(integer 2) EFORM EFFect(string) noINSUFficient INTERaction KEEPALL LABels(name) ///
	LCOLs(namelist) NULLOFF RCOLs(namelist) noNAmes NONUll NUll(string) noOVerall noPRESERVE noSTATs noSUbgroup noWT ///
	/// /* x-axis options */
	XLABel(string) XTICk(string) XTItle(string asis) Range(numlist min=2 max=2) FAVours(string asis) FP(real -9) ///
	/// /* other "fine-tuning" options */
	ADDHeight(real 0) /*(undocumented)*/ noADJust ASPECT(real -9) ASText(real -9) BOXscale(real 100.0) CAPTION(string asis) ///
	SPacing(real -9) SUBtitle(string asis) /*TEXTscale(real 100.0)*/ TItle(string asis) XSIZe(real -9) YSIZe(real -9) ///
	NOTE(string asis) RENOTE(string) SAVEDIMS(name) USEDIMS(name) ///
	* ]

	local dispNN `dispnn'		// for clarity
	
	// if forestplot is being run "stand-alone" (i.e. not called from ipdmetan etc.), parse eform option
	if "`preserve'" == "" {
		_get_eformopts, soptions eformopts(`eform' `options') allowed(__all__)
		local options `"`s(options)'"'
		local eform = cond(`"`s(eform)'"'=="", "", "eform")
		if `"`effect'"'==`""' {
			local effect = cond(`"`s(str)'"'=="", "Effect", `"`s(str)'"')
			if `"`interaction'"'!=`""' local effect `"Interact. `effect'"'
		}
	}
	marksample touse						// do this immediately, so that -syntax- can be used again
	local graphopts `"`options'"'			// "graph region" options (also includes plotopts for now)

	if "`keepall'"=="" qui replace `touse' = 0 if _USE==2	// "keepall" option (see ipdmetan)
	if "`box'"!="" local oldbox "nobox"		// allow global "nobox" for compatibility with -metan-
											// N.B. can't be used with plotid; instead box`p'opts(msymbol(none)) can be used
	local h0 = 0							// default
	if trim(`"`nulloff'`nonull'"') != `""' local null "nonull"
	// "nulloff" and "nonull" are permitted alternatives to null(none|off), for compatability with -metan-
	else if `"`null'"'!=`""' {
		if `: word count `null'' > 1 {
			disp as err "may only supply a single number to option null"
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
				disp as err "invalid syntax in option null"
				exit 198
			}
		}
	}
	// N.B. `null' now either contains nothing, or "nonull"
	//  and `h0' contains a number (defaulting to 0), denoting where the null-line will be placed if "`null'"==""
	
	* Unpack `usedims'
	local DXwidthChars = 0			// initialize
	local oldTextSize = 0			// initialize
	if `"`usedims'"'!=`""' {
		local DXwidthChars = `usedims'[1, `=colnumb(`usedims', "cdw")']
		local spacing = cond(`spacing'==-9, `usedims'[1, `=colnumb(`usedims', "spacing")'], `spacing')
		local xsize = cond(`xsize'==-9, `usedims'[1, `=colnumb(`usedims', "xsize")'], `xsize')
		local ysize = cond(`ysize'==-9, `usedims'[1, `=colnumb(`usedims', "ysize")'], `ysize')
		local oldTextSize = `usedims'[1, `=colnumb(`usedims', "textsize")']
	}

	* Set up variable names
	if `"`namelist'"'==`""' {		// if not specified, assume "standard" varnames			
		local _ES "_ES"
		local _LCI "_LCI"
		local _UCI "_UCI"
		local _WT = cond("`dispNN'"=="", "_WT", "_NN")
		local _USE "_USE"
	}
	else {							// else check syntax of user-specified varnames
		local 0 `namelist'
		syntax varlist(min=3 max=5 numeric)
		tokenize `varlist'
		local _ES `1'
		local _LCI `2'
		local _UCI `3'
		local _WT = cond(`"`4'"'!=`""', `"`4'"', cond("`dispNN'"=="", "_WT", "_NN"))
		local _USE = cond(`"`5'"'!=`""', `"`5'"', "_USE")
	}

	quietly {
	
		*** Set up data to use
		capture confirm numeric var `_USE'
		if _rc {
			if _rc!=7 {			// if `_USE' does not exist
				tempvar _USE
				gen int `_USE' = cond(missing(`_ES'*`_LCI'*`_UCI'), 2, 1)
			}
			else {
				nois disp as err `"_USE: variable `_USE' exists but is not numeric"'
				exit 198
			}
		}
		markout `touse' `_USE'
		if `"`overall'"'!=`""' {
			replace `touse'=0 if inlist(`_USE', 4, 5)
		}
		replace `touse'=0 if `"`subgroup'"'!=`""' & `_USE' == 3
		count if `touse'
		if !r(N) {
			nois disp as err "no observations"
			exit 2000
		}
		tempvar touse2 allobs obs
		gen long `allobs'=_n
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
				nois disp as err `"_WT: variable `_WT' exists but is not numeric"'
				exit 198
			}
		}
		local awweight "[aw= `_WT']"
		
		* Check validity of `_USE' (already sorted out existence)
		capture assert !missing(`_ES'*`_LCI'*`_UCI') if `touse' & `_USE'==1
		local rctemp = _rc
		capture assert missing(`_ES'*`_LCI'*`_UCI') if `touse' & `_USE'==2
		if `rctemp' | _rc {
			nois disp as err `"effect sizes do not match with value of _USE"'
			exit 198
		}

		* Generate ordering variable (reverse sequential, since y axis runs bottom to top)
		assert inrange(`_USE', 0, 6)
		tempvar id
		bysort `touse' (`obs') : gen int `id' = _N - _n + 1 if `touse'
		
		* Check existence of `labels' and `by'
		foreach x in labels by {
			local X = upper("`x'")
			if `"``x''"'!=`""' {
				confirm var ``x''
				if !_rc local `x' "``x''"	// use given varname if exists
			}
			else {
				cap confirm var _`X'
				if !_rc local `x' "_`X'"	// use default varnames if they exist and option not explicitly given
			}
		}
		
		* Sort out `dataid' and `plotid'
		local nd=1
		local 0 `dataid'
		syntax [varname(default=none)] [, NEWwt]
		if `"`varlist'"'!=`""' {
			tab `varlist' if `touse', m
			if r(r)>20 {
				nois disp as err "dataid: variable takes too many values"
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
			if "`preserve'" != "" disp _n _c	// spacing if following on from ipdmetan (etc.)

			capture confirm var _BY
			local _by = cond(_rc, "", "_BY")
			capture confirm var _OVER
			local _over = cond(_rc, "", "_OVER")
			
			local 0 `plotid'
			syntax name(name=plname id="plotid") [, List noGRaph]
			local plotid		// clear macro; will want to define a tempvar named plotid

			if "`plname'"!="_n" {
				confirm var `plname'
				tab `plname' if `touse', m
				if r(r)>20 {
					nois disp as err "plotid: variable takes too many values"
					exit 198
				}
				if `"`_over'"'==`""' {
					count if `touse' & inlist(`_USE', 1, 2) & missing(`plname')
					if r(N) {
						nois disp as err "Warning: plotid contains missing values"
						nois disp as err "plotid groups and/or allocated ordinal numbers may not be as expected"
						if "`list'"=="" nois disp as err "This may be checked using the 'list' suboption to 'plotid'"
					}
				}
			}
			
			* Create ordinal version of plotid...
			gen byte `touse2' = `touse' * inlist(`_USE', 1, 2, 3, 5)
			local plvar `plname'

			* ...extra tweaking if passed through from ipdmetan/ipdover (i.e. _STUDY, and possibly _OVER, exists)
			if inlist("`plname'", "_STUDY", "_n", "_LEVEL", "_OVER") {
				capture confirm var _STUDY
				local _study = cond(_rc, "_LEVEL", "_STUDY")
				tempvar smiss
				gen byte `smiss' = missing(`_study')
				
				if inlist("`plname'", "_STUDY", "_n") {
					tempvar plvar
					bysort `touse2' `smiss' (`_over' `_study') : gen long `plvar' = _n if `touse2' & !`smiss'
				}
				else if "`plname'"=="_LEVEL" {
					tempvar plvar
					bysort `touse2' `smiss' `_by' (`_over' `_study') : gen long `plvar' = _n if `touse2' & !`smiss'
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
					nois list `dataid' `_USE' `_by' `_over' `labels' if `touse2' & `plotid'==`p', table noobs sep(0)
				}
				if `"`graph'"'!=`""' exit
			}
			drop `touse2' `plobs' `smiss'
		}
		

		** SORT OUT LCOLS AND RCOLS
		//  default "lcol1" (if using ipdmetan) is list of study names, headed "Study ID"
		// unless noNAMES specified, add `labels' to `lcols' (even if blank)
		if `"`names'"'==`""' local lcols = trim(`"`labels' `lcols'"')

		** EFFECT SIZE AND WEIGHT COLUMNS
		//  by default, rcols 1 and 2 are effect sizes and weights
		//  `stats' and `wt' turn these optionally off.
		// (N.B. if "dispNN" specified, _WT is replaced with _NN, and `nowt' turns of _NN instead)
		if "`stats'" == "" {
			tempvar estText
			if `"`eform'"'!=`""' local xexp "exp"
			gen str `estText' = string(`xexp'(`_ES'), "%10.`dp'f") ///
				+ " (" + string(`xexp'(`_LCI'), "%10.`dp'f") + ", " + string(`xexp'(`_UCI'), "%10.`dp'f") + ")" ///
				if `touse' & inlist(`_USE', 1, 3, 5) & !missing(`_ES')
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
		}
		
		if "`wt'" == "" {
			if "`dispNN'"=="" {
				tempvar weightpc
				gen double `weightpc' = 100*`_WT' if `touse' & inlist(`_USE', 1, 3, 5) & !missing(`_ES')
				format `weightpc' %6.2f
				label var `weightpc' "% Weight"
			}
			else local lcols = trim(`"`lcols' `_WT'"')			// if dispNN, place nos. of pts (in `_WT') to left of plot by default...
		}
		local rcols = trim(`"`estText' `weightpc' `rcols'"')	// ...but effect-size text and I-V weights to right of plot.
		
		// Test validity of lcols and rcols
		foreach x of local lcols {					// "lcols" has to exist
			cap confirm var `x' 
			if _rc {
				nois disp as err "variable `x' not found in option lcols"
				exit _rc
			}
		}
		if `"`rcols'"' != `""' {
			foreach x of local rcols {
				cap confirm var `x' 
				if _rc {
					nois disp as err "variable `x' not found in option rcols"
					exit _rc
				}
			}
		}
		local lcolsN : word count `lcols'
		local rcolsN : word count `rcols'

		// Check that UCI is greater than LCI
		count if `touse' & `_UCI' < `_LCI' & !missing(`_LCI')
		if r(N) {
			replace `_LCI' = . if `touse' & `_UCI' < `_LCI' & !missing(`_LCI')
			replace `_UCI' = . if `touse' & `_UCI' < `_LCI' & !missing(`_LCI')
			disp as err _n "potential errors in confidence limits; please check"
		}

		
		** GET MIN AND MAX DISPLAY
		// [comments from metan.ado follow]
		// SORT OUT TICKS- CODE PINCHED FROM MIKE AND FIDDLED. TURNS OUT I'VE BEEN USING SIMILAR NAMES...
		// AS SUGGESTED BY JS JUST ACCEPT ANYTHING AS TICKS AND RESPONSIBILITY IS TO USER!
		// N.B. `DXmin', `DXmax' are the left and right co-ords of the graph part

		// Range: always takes precedence if specified
		if "`range'" != `""' {
			if `"`eform'"'!=`""' {
				numlist "`range'", range(>0) sort
				tokenize "`range'"
				local range `"`=ln(`1')' `=ln(`2')'"'
			}
			else {
				numlist "`range'", sort
				local range=r(numlist)
			}
			
			// remove null line if `range' specified and `h0' lies outside it
			tokenize "`range'"
			if `h0' < `1' | `h0' > `2' {
				disp as err "null line lies outside user-specified x-axis range and will be suppressed"
				local null "nonull"
			}
			local DXmin `1'
			local DXmax `2'
		}
		else {
			summ `_LCI' if `touse', meanonly
			local DXmin = r(min)			// minimum confidence limit
			summ `_UCI' if `touse', meanonly
			local DXmax = r(max)			// maximum confidence limit
		}

		// If xlabel not supplied by user, need to choose sensible values
		//  default is for symmetrical limits, with 3 labelled values including null
		// N.B. First modified from original -metan- code by DF, March 2013
		//  with further improvements by DF, January 2015
		if "`xlabel'" == "" {
		
			// if `nulloff', choose values in two stages: firstly based on the midpoint between DXmin and DXmax (`xlab[init|lim]1')
			//  and then based on the difference between DXmin/max and the midpoint (`xlab[init|lim]2')
			if "`null'" == "" {
				local xlabinit2 = max(abs(`DXmin' - `h0'), abs(`DXmax' - `h0'))
				local xlabinit "`xlabinit2'"
			}
			else {	// nonull
				local xlabinit1 = (`DXmax' + `DXmin')/2
				local xlabinit2 = abs(`DXmax' - `xlabinit1')		// N.B. same as abs(`DXmin' - `xlabinit1')
				local xlabinit "`=abs(`xlabinit1')' `xlabinit2'"
			}
			assert "`xlabinit'"!=""
			assert "`xlabinit2'"!=""
			assert `: word count `xlabinit'' == ("`null'"!="") + 1

			local counter=1
			local xlablim1=0
			foreach xval of numlist `xlabinit' {
			
				if `"`eform'"'==`""' {						// linear scale
					local mag = floor(log10(`xval'))
					local xdiff = abs(`xval'-`mag')
					foreach i of numlist 1 2 5 10 {
						local ii = `i' * 10^`mag'
						if abs(float(`xval' - `ii')) <= float(`xdiff') {
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
						if abs(float(`xval' - `ii')) <= float(`xdiff') {
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
					if `counter'==1 local xlablim1 = `xlablim'*sign(`xlabinit1')
					else {
						local xlablim2 = `xlablim'
						local xlablims `"`=`xlablim1'+`xlablim2'' `=`xlablim1'-`xlablim2''"'
					}
				}
				else local xlablims `"`xlablims' `xlablim'"'		// JAN 2015 - can this bit be tidied up??
				local ++counter
			}
			
			// if nulloff, don't recalculate DXmin/DXmax
			if "`null'" != "" {
				numlist `"`xlablim1' `xlablims'"'
				local adjust "noadjust"
			}
			else {
				numlist `"`=`h0' - `xlablims'' `h0' `=`h0' + `xlablims''"', sort	// default: limits symmetrical about `h0'
				tokenize `"`r(numlist)'"'

				// if data are "too far" from null (`h0'), take one limit (but not the other) plus null
				//   where "too far" ==> abs(`DXmin' - `h0') > `DXmax' - `DXmin'
				// also, unless `range' is specified, adjust `DXmin' and/or `DXmax' to reflect this
				if abs(`DXmin' - `h0') > `DXmax' - `DXmin' {
					if `3' > `DXmax' {
						numlist `"`1' `h0'"'
						local DXmax = cond("`range'"=="", `h0', `DXmax')	// DXmin remains unchanged
					}			
					else if `1' < `DXmin' {
						numlist `"`h0' `3'"'
						local DXmin = cond("`range'"=="", `h0', `DXmin')	// DXmax remains unchanged
						local adjust "noadjust"
					}
				}
				else if "`range'"=="" {					// "standard" situation
					numlist `"`1' `h0' `3'"'
					local DXmin = `h0' - `xlabinit2'
					local DXmax = `h0' + `xlabinit2'
				}
			}	
			local xlablist=r(numlist)
			
		}		// end if "`xlabel'" == ""

		// xlabel supplied by user: parse and apply
		else {
			local 0 `"`xlabel'"'
			syntax anything(name=xlablist) [, FORCE *]
			local xlabopts `"`options'"'			// JUNE 2015: options now passed on to graph command (but check for consequences)

			if `"`eform'"'!=`""' {					// assume given on exponentiated scale if "eform" specified, so need to take logs
				numlist "`xlablist'", range(>0)		// in which case, all values must be greater than zero
				local n : word count `r(numlist)'
				forvalues i=1/`n' {
					local xi : word `i' of `r(numlist)'
					local xlablist2 `"`xlablist2' `=ln(`xi')'"'
				}
				local xlablist "`xlablist2'"
			}
			
			if "`force'" != "" {
				if "`range'" == "" {
					numlist "`xlablist'", sort
					local n : word count `r(numlist)' 
					local DXmin : word 1 of `r(numlist)'
					local DXmax : word `n' of `r(numlist)'
				}
				else disp as err "Note: both range() and xlabel(, force) were specifed; range() takes precedence"
			}
		}		// end else (i.e. if "`xlabel'" != "")
		
		// Ticks
		// JUN 2015 -- for future: is there any call for allowing FORCE, or similar, for ticks??
		if "`xtick'" == "" local xticklist `xlablist'	// if not specified, default to same as labels
		else {
			gettoken xticklist : xtick, parse(",")
			if `"`eform'"'!=`""' {						// assume given on exponentiated scale if "eform" specified, so need to take logs
				numlist "`xticklist'", range(>0)		// ...in which case, all values must be greater than zero
				local n : word count `r(numlist)'
				forvalues i=1/`n' {
					local xi : word `i' of `r(numlist)'
					local xticklist2 `"`xticklist2' `=ln(`xi')'"'
				}
				local xticklist "`xticklist2'"
			}
			else {
				numlist "`xticklist'"
				local xticklist=r(numlist)
			}
		}
		// Final calculation of DXmin and DXmax (under "normal" circumstances)
		if trim(`"`range'`force'`null'"') == `""' & `h0'==0 {
			numlist "`xlablist' `xticklist' `DXmin' `DXmax'", sort
			local n : word count `r(numlist)' 
			local DXmin : word 1 of `r(numlist)'
			local DXmax : word `n' of `r(numlist)'
			local DXmin = -max(abs(`DXmin'), abs(`DXmax'))
			local DXmax = max(abs(`DXmin'), abs(`DXmax'))				// symmetrical plot area (around `h0')
		}

		// If on exponentiated scale, re-label x-axis with exponentiated values (nothing else should need changing)
		if "`eform'" != "" {
			local xlblcmd
			foreach i of numlist `xlablist' {
				local lbl = string(`=exp(`i')',"%7.3g")
				local xlblcmd `"`xlblcmd' `i' "`lbl'""'
			}
		}
		else local xlblcmd `"`xlablist'"'
			
		local DXwidth = `DXmax' - `DXmin'

		// END OF TICKS AND LABELS

		
		** Need to make changes to pre-existing data now, plus adding new obs to the dataset now
		//  so use -preserve- before continuing
		//  (if not already preserved by -ipdmetan- etc.) 
		
		if "`preserve'" == "" preserve
		
		// if multiple plotids, or if dataid(varname, newwt) specified,
		// create dummy obs with global min & max weights, to maintain correct weighting throughout
		if `np' > 1 | `"`newwt'"'!=`""' {		// Amended June 2015
		
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
			set obs `=`oldN' + 2*`nd'*`np''
			forvalues i=1/`nd' {
				forvalues j=1/`np' {
					/*
					local from = `oldN' + (`i'-1)*2*`np' + 2*`j' - 1
					local to = `oldN' + (`i'-1)*2*`np' + 2*`j'
					replace `plotid' = `j' in `from' / `to'
					replace `_WT' = `minwt' in `from'
					replace `_WT' = `maxwt' in `to'
					*/
					local k = `oldN' + (`i'-1)*2*`np' + 2*`j'
					replace `plotid' = `j' in `=`k'-1' / `k'
					replace `_WT' = `minwt' in `=`k'-1'
					replace `_WT' = `maxwt' in `k'
				}
			}
			replace `_USE' = 1 in `=`oldN' + 1' / `=`oldN' + (2*`nd'*`np')'
			replace `touse' = 1 in `=`oldN' + 1' / `=`oldN' + (2*`nd'*`np')'
		}

		// modify `touse' to take into account dummy obs
		tempvar toused
		gen byte `toused' = `touse'							// "touse + dummy obs", for scatter plots only
		replace `touse' = 0 if `touse' & missing(`id')		// general-purpose `touse'
		

		************************
		* LEFT & RIGHT COLUMNS *
		************************

		** Titles
		summ `id'
		local max = r(max)
		local oldN = _N
		set obs `=`oldN'+4'							// create four new observations
		forvalues i = 1/4 {							//   to store up to four lines for titles
			local Nnew`i' = `=`oldN' + `i''
			replace `id' = `max' + `i' + 1 in `Nnew`i''		// "+1" leaves a one-line gap between titles & main data
		}
		local borderline = `max' + 1 - 0.25
		replace `touse' = 1 in `=`oldN' + 1' / `=`oldN' + 4'
		replace `toused' = 1 in `=`oldN' + 1' / `=`oldN' + 4'	// mark these as "dummy obs" too, so they can be removed later
		
		tempvar strlen strwid
		local digitwid : _length 0		// width of a digit (e.g. "0") in current graphics font
		
		** Left columns
		local leftWDtot = 0
		forvalues i=1/`lcolsN' {
			tempvar left`i'				// to store x-axis positions of columns
			tempvar lindent`i' 			// for right-justifying text
			tempvar lindentNoTi`i' 		// for right-justifying text (study-name rows only)
			tempvar lastcol
			qui gen `lindent`i'' = 0
			qui gen `lindentNoTi`i'' = 0
			qui gen int `lastcol' = 0
			
			local lcoli : word `i' of `lcols'
			local f: format `lcoli'
			tokenize `"`f'"', parse("%s.,")
			local fmtlen `2'				// desired max no. of characters based on format		
			
			capture confirm string var `lcoli'
			if !_rc {	// string
				local leftLB`i' : copy local lcoli
			}
			else {		// numeric
				tempvar leftLB`i'
				gen str `leftLB`i'' = string(`lcoli', "`f'")
				replace `leftLB`i'' = "" if `leftLB`i'' == "."
			}
			
			gen long `strlen' = length(`leftLB`i'')
			summ `strlen', meanonly
			local maxlen = r(max)		// max length of existing text

			getWidth `leftLB`i'' `strwid'
			summ `strwid', meanonly
			local maxwid = r(max)		// max width of existing text
			
			local leftWD`i' = 0			// default
			if abs(`fmtlen') <= `maxlen' local leftWD`i' = `maxwid'				// exact width of `maxlen' string
			else local leftWD`i' = abs(`fmtlen')*`digitwid'						// approx. max width (based on `digitwid')
			if `fmtlen'>0 replace `lindent`i'' = `leftWD`i'' - `strwid' 		// indent if right-justified
			local leftWD`i' = `leftWD`i'' + 2*`digitwid'						// having calculated the indent, add a buffer
			local leftWDtot = `leftWDtot' + `leftWD`i''							// running calculation of total width (including titles)
			
			replace `lastcol' = `i' if `strwid'>0								// identify (non study-name) rows with no data
			drop `strlen' `strwid'
		}		// end of forvalues i=1/`lcolsN'

		
		** Right columns
		local rightWDtot = 0
		forvalues i=1/`rcolsN' {		// if `rcols'==0, loop will be skipped
			tempvar right`i'			// to store x-axis positions of columns
			tempvar rindent`i'			// for right-justifying text
			qui gen `rindent`i'' = 0
			
			local rcoli : word `i' of `rcols'
			local f: format `rcoli'
			tokenize `"`f'"', parse("%s.,")
			local fmtlen `2'				// desired max no. of characters based on format
			
			cap confirm string var `rcoli'
			if !_rc {						// if string
				local rightLB`i' : copy local rcoli
			}
			else {							// if numeric
				tempvar rightLB`i'
				gen str `rightLB`i'' = string(`rcoli', "`f'")
				replace `rightLB`i'' = "" if `rightLB`i'' == "."
			}			
			
			gen long `strlen' = length(`rightLB`i'')
			summ `strlen', meanonly
			local maxlen = r(max)		// max length of existing text

			getWidth `rightLB`i'' `strwid'
			summ `strwid', meanonly		
			local maxwid = r(max)		// max width of existing text

			local rightWD`i' = 0		// default
			if abs(`fmtlen')<=`maxlen' local rightWD`i' = `maxwid'				// exact width of `maxlen' string
			else local rightWD`i' = abs(`fmtlen')*`digitwid'					// approx. max width (based on `digitwid')
		
			// WORK OUT IF TITLE IS BIGGER THAN THE VARIABLE
			// SPREAD OVER UP TO FOUR LINES IF NECESSARY
			// JAN 2015: Future work might be to re-write (incl. SpreadTitle) to use width rather than length??
			local colName: variable label `rcoli'
			if `"`colName'"' == "" & `"`rcoli'"' !=`"`labels'"' local colName = `"`rcoli'"'
			local target = max(abs(`fmtlen'), `maxlen', int(length(`"`colName'"')/3))
			* local maxwidth = floor(`target' + 5)		// JAN 2015: consider allowing a bit more leeway?
			
			SpreadTitle `"`colName'"', target(`target') maxwidth(`target') maxline(4) force		// "force" option in case `target'==0
			local nlines = r(nlines)
			forvalues j = `nlines'(-1)1 {
				if `"`r(title`j')'"'!=`""' {
					local k=`nlines'-`j'+1
					replace `rightLB`i'' = `"`r(title`j')'"' in `Nnew`k''
					replace `_USE' = 9 in `Nnew`k''
				}
			}
			
			drop `strwid'
			getWidth `rightLB`i'' `strwid'						// re-calculate `strwid' to include titles
			if `fmtlen'>0 replace `rindent`i'' = `rightWD`i'' - `strwid'	// indent (in units of width) for right-justifying
			local rightWD`i' = `rightWD`i'' + 2*`digitwid'		// having calculated the indent, add a buffer
			local rightWDtot = `rightWDtot' + `rightWD`i''		// running calculation of total width (incl. buffer)
			drop `strlen' `strwid'
		}														// end of forvalues i=1/`rcols'
		local rightWDtot = `rightWDtot' + 2*`digitwid'			// add an extra buffer at far right-hand side
		
		// Unless noadjust, compare width of text in subgroup/overall and header rows (`leftWDtot')
		//  to the width *excluding* "long headers" (`leftWDtotNoTi')
		//  and work out how far into plot this text can extend without overwriting the graph data
		// (N.B. this needs both the left and right columns to have already had their first processing, for which see previous lines)
		if "`adjust'" == "" {
		
			// Re-calculate widths of `lcols' for observations *other* than study estimates (i.e. _USE==0, 3, 4, 5)
			local leftWDtotNoTi = `leftWDtot'
			forvalues i=1/`lcolsN' {
				gen long `strlen' = length(`leftLB`i'')
				getWidth `leftLB`i'' `strwid'
				
				summ `strwid' if `touse' & inlist(`_USE', 0, 3, 4, 5) & `lastcol'>`i'
				local maxNotLast = cond(r(N), r(max), 0)
				summ `strwid' if `touse' & inlist(`_USE', 0, 3, 4, 5) & `lastcol'==`i'
				local minLast = cond(r(N), r(min), 0)
				
				local leftWDnew`i' = `leftWD`i''			// default
				if `minLast' > `maxNotLast' {				// no data to right of long "header"(s), so can re-calculate excluding it/them
					summ `strlen' if inlist(`_USE', 1, 2), meanonly
					local maxlen = r(max)
					summ `strwid' if inlist(`_USE', 1, 2), meanonly
					local maxwid = r(max)

					local lcoli : word `i' of `lcols'
					local f: format `lcoli'
					tokenize `"`f'"', parse("%s.,")
					local fmtlen `2'						// desired max no. of characters based on format		
					
					if abs(`fmtlen')<=`maxlen' local leftWDnew`i' = `maxwid'				// exact width of `maxlen' string
					else local leftWDnew`i' = abs(`fmtlen')*`digitwid'						// approx. max width (based on `digitwid')
					if `fmtlen'>0 replace `lindentNoTi`i'' = `leftWDnew`i'' - `strwid' 		// indent if right-justified
					local leftWDnew`i' = `leftWDnew`i'' + 2*`digitwid'						// having calculated the indent, add a buffer
					local leftWDtotNoTi = `leftWDtotNoTi' - `leftWD`i'' + `leftWDnew`i''	// re-calculate the total width (incl. buffer)
				}
				drop `strlen' `strwid'
			}
			drop `lastcol'		// tidying up
		
			// If appropriate, allow _USE=0,3,4,5 to extend into main plot by (lcimin-DXmin)/DXwidth
			// (where `lcimin' is the left-most confidence limit among the "diamonds")
			// i.e. 1 + ((`lcimin'-`DXmin')/`DXwidth') * ((100-`astext')/`astext')) is the percentage increase
			// to apply to (`leftWDtot'+`rightWDtot')/(`newleftWDtot'+`rightWDtot').
			// Then rearrange to find `newleftWDtot'.
			if `leftWDtotNoTi' < `leftWDtot' {
				tempvar lci2
				gen double `lci2' = cond(`_LCI'>`h0', `h0', `_LCI')			// `h0' must exist if "`adjust'" == ""
				summ `lci2' if `touse' & inlist(`_USE', 3, 5), meanonly
				local lcimin = r(min)
				drop `lci2'

				// sort out astext... need to do this now, but will be recalculated later (line 890)
				if `"`usedims'"'!=`""' & `astext'==-9 {
					local astext2 = (`leftWDtot' + `rightWDtot')/`DXwidthChars'
					local astext = 100 * `astext2'/(1 + `astext2')
				}
				else {
					local astext = cond(`astext'==-9, 50, `astext')
					numlist "`astext'", range(>=0)
					local astext2 = `astext'/(100 - `astext')
				}
				
				// define some additional locals to make final formula clearer
				local totWD = `leftWDtot' + `rightWDtot'
				local lciWD = (`lcimin' - `DXmin')/`DXwidth'
				local astext2 = `astext' / (100 - `astext')
				
				local newleftWDtot = cond(`"`usedims'"'==`""', ///
					(`totWD' / ((`lciWD'/`astext2') + 1)) - `rightWDtot', ///
					`leftWDtot' - `lciWD'*`DXwidthChars')
				
				// BUT don't make leftWDtot any *less* than before, unless there are no obs with inlist(`_USE', 1, 2)
				qui count if `touse' & inlist(`_USE', 1, 2)
				local leftWDtot = cond(r(N), max(`leftWDtotNoTi', `newleftWDtot'), `newleftWDtot')
				
				forvalues i=1/`lcolsN' {
					local leftWD`i' = `leftWDnew`i''			// replace old values with new
					replace `lindent`i'' = `lindentNoTi`i''
					drop `lindentNoTi`i''
				}
			}
		}
		
		// Finally, we can sort out titles for the `lcols'.
		// WORK OUT IF TITLE IS BIGGER THAN THE VARIABLE
		// SPREAD OVER UP TO FOUR LINES IF NECESSARY
		// JAN 2015: Future work might be to re-write (incl. SpreadTitle) to use width rather than length??
		forvalues i=1/`lcolsN' {
			
			local lcoli : word `i' of `lcols'
			local f: format `lcoli'
			tokenize `"`f'"', parse("%s.,")
			local fmtlen `2'				// desired max no. of characters based on format		
			
			// If more than one lcol, restrict to width of study-name data.
			// Otherwise, title may be as long as the max width for the column.
			gen long `strlen' = length(`leftLB`i'')
			if `lcolsN'>1 local anduse `" & inlist(`_USE', 1, 2)"'
			summ `strlen' if `touse' `anduse', meanonly
			local maxlen = r(max)
				
			local colName: variable label `lcoli'
			if `"`colName'"' == "" & `"`lcoli'"' !=`"`labels'"' local colName = `"`lcoli'"'
			local target = max(abs(`fmtlen'), `maxlen', int(length(`"`colName'"')/3))
			
			SpreadTitle `"`colName'"', target(`target') maxwidth(`=2*`target'') maxline(4) force	// "force" option in case `target'==0
			local nlines = r(nlines)																// maxwidth = 2*`target'
			forvalues j = `nlines'(-1)1 {
				if `"`r(title`j')'"'!=`""' {
					local k=`nlines'-`j'+1
					replace `leftLB`i'' = `"`r(title`j')'"' in `Nnew`k''
					replace `_USE' = 9 in `Nnew`k''
				}
			}
			getWidth `leftLB`i'' `strwid'						// re-calculate `strwid' to include titles
			if `fmtlen'>0 replace `lindent`i'' = `leftWD`i'' - (`strwid' + 2*`digitwid') if `_USE' == 9
			
			drop `strlen' `strwid'
		}
		
		// Remove extra title rows if they weren't used
		drop if `toused' & missing(`_USE')
		
		// Generate position of lcols, using `astext' (% of graph width taken by text)
		// N.B. although these are constants, they need to be stored variables for use with -twoway-
		if `"`usedims'"'!=`""' & `astext'==-9 {
			local astext2 = (`leftWDtot' + `rightWDtot')/`DXwidthChars'
			local astext = 100 * `astext2'/(1 + `astext2')
		}
		else {
			local astext = cond(`astext'==-9, 50, `astext')
			numlist "`astext'", range(>=0)
			local astext2 = `astext'/(100 - `astext')
		}
		local textWD = `astext2' * `DXwidth'/(`leftWDtot' + `rightWDtot')

		local leftWDruntot = 0
		forvalues i = 1/`lcolsN' {
			gen double `left`i'' = `DXmin' - (`leftWDtot' - `leftWDruntot')*`textWD'
			replace `left`i'' = `left`i'' + `lindent`i''*`textWD'
			local leftWDruntot = `leftWDruntot' + `leftWD`i''
		}
		if `rcolsN' {
			gen double `right1' = `DXmax' + `rindent1'*`textWD'
			local rightWDruntot = `rightWD1'
			forvalues i = 2/`rcolsN' {						// if `rcolsN'=0 then loop will be skipped
				gen double `right`i'' = `DXmax' + (`rightWDruntot' + `rindent`i'')*`textWD'
				local rightWDruntot = `rightWDruntot' + `rightWD`i''
			}
		}
		
		// AXmin AXmax ARE THE OVERALL LEFT AND RIGHT COORDS
		local AXmin = `left1'
		local AXmax = `DXmax' + `rightWDtot'*`textWD'


		// FIND OPTIMAL TEXT SIZE AND ASPECT RATIOS (given user input)
		// Notes:  (David Fisher, July 2014)
		
		// Let X, Y be dimensions of graphregion (controlled by xsize(), ysize()); x, y be dimensions of plotregion (controlled by aspect()).
		// `approxChars' is the approximate width of the plot, in "character units" (i.e. width of [LHS text + RHS text] divided by `astext')
		// Note that a "character unit" is the width of a character relative to its height; hence...
		//  ...`height' is the approximate height of the plot, in terms of both rows of text (with zero gap between rows) AND "character units".
		// If Y/X = `graphAspect'<1, `textSize' is the height of a row of text relative to Y; otherwise it is height relative to X.
		//  (Note that the default `graphAspect' = 4/5.5 = 0.73 < 1)
		// We then let `approxChars' = x, and manipulate to find the optimum text size for the plot layout.
		// Finally, maximum text size is 100/y.

		// FEB 2015: `textscale' is deprecated, since it causes problems with spilling on the RHS.
		// Instead, using `spacing' to fine-tune the aspect ratio (and hence the text size)
		//   or use `aspect' to completely user-define the aspect ratio.
		
		// If y/x < Y/X < 1 then X = kx (i.e. plot takes up full width of "wide" graph, with an extra margin if overall title specified)
		//   then `textSize' = 100/Y = 100 / (X * `graphAspect') -- but X = kx * `approxChars'
		//   ==> `textSize' = 100/(k * `approxChars' * `graphAspect')
		
		// If Y/X < 1 and y/x > Y/X (i.e. plot is less wide than graph) then Y=ky where k = (`height'+delta)/`height'
		//   then `textSize' = 100/ky = 100 / (x * k * `plotAspect') = 100 / (`approxChars' * k * `plotAspect')
		
		// If y/x > Y/X > 1 then Y = ky
		//   then `textSize' = 100/X = 100 * `graphAspect'/ky = 100 * `graphAspect' / (`approxChars' * k * `plotAspect')
		
		// If Y/X > 1 and y/x < Y/X (i.e. plot is less tall than graph) then assume X = kx
		//   then `textSize' = 100/X = 100 / (k * `approxChars')

		//  - Note that this code has been changed considerably from the original -metan- code.

		* Validate options
		local xsize = cond(`"`usedims'"'==`""' & `xsize'==-9, 5.5, `xsize')
		local ysize = cond(`"`usedims'"'==`""' & `ysize'==-9, 4, `ysize')
		numlist "`xsize' `ysize'", range(>0)
		numlist "`boxscale'", range(>=0)

		count if `touse'
		local height = r(N)
		count if `touse' & `_USE'==9
		local height = cond(r(N), `height' + 1, `height')	// add 1 to height if titles (to take account of gap)	

		local condtitle = 2*(`"`title'"'!=`""') + (`"`subtitle'"'!=`""') + (`"`caption'"'!=`""') + .5*(`"`note'"'!=`""') + `addheight'
		local yk = (`height' + `condtitle' + (`"`xlblcmd'"'!=`""') + (trim(`"`favours'`xtitle'"')!=`""'))/`height'
		local xk = (`height' + `condtitle')/`height'
		// Notes Feb 2015:
		// - could maybe be improved, but for now `addheight' option (undocumented) allows user to tweak
		// - also think about line widths (thicknesses), can we keep them constant-ish??
		
		* Use saved "dimensions"
		// (for future: investigate using margins to "centre on DXwidth" within graphregion??)
		if `"`usedims'"'!=`""' {
			local approxChars = `leftWDtot' + `rightWDtot' + `DXwidthChars'
			local plotAspect = cond(`aspect'==-9, `spacing'*`height'/`approxChars' /* using `spacing' from `usedims' */, `aspect')
		}
		else {
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
		numlist "`plotAspect' `spacing'", range(>=0)
		
		local graphAspect = `ysize'/`xsize'		// aspect of graphregion (defaults to 4/5.5  = 0.727 unless specified)
		
		if `graphAspect' <= 1 & `plotAspect' <= `graphAspect' {
			local textSize = 100 / (`xk' * `approxChars' * `graphAspect')
		}
		else if `graphAspect' <= 1 & `plotAspect' > `graphAspect' {
			local textSize = 100 / (`yk' * `approxChars' * `plotAspect')
		}
		else if `graphAspect' > 1 & `plotAspect' > `graphAspect' {
			local textSize = (100 * `graphAspect') / (`yk' * `approxChars' * `plotAspect')
		}
		else if `graphAspect' > 1 & `plotAspect' <= `graphAspect' {
			local textSize = 100 / (`xk' * `approxChars')
		}

		// if new graph is "too wide" (i.e. textsize is smaller), increase xsize to compensate
		if `"`usedims'"'!=`""' & float(`textSize') < float(`oldTextSize') {
			local xsize = `xsize' * `oldTextSize' / `textSize'
			local graphAspect = `ysize'/`xsize'
			local textSize = `oldTextSize'
		}
		
		* Notes: for random-effects analyses, sample-size weights, or user-defined (will overwrite the first two)
		if "`wtn'"!="" & `"`renote'"'==`""' local renote "NOTE: Point estimates are weighted by sample size"
		if `"`renote'"'!=`""' & `"`note'"'==`""' {
			local note `""`renote'", size(`=`textSize'*.75')"'	// use 75% of text size used for rest of plot
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
			mat `savedims' = `DXwidthChars', `spacing', `ysize', `xsize', `textSize'
			mat colnames `savedims' = cdw spacing ysize xsize textsize
		}
		
		// PLOT COLUMNS OF TEXT (lcols/rcols)
		forvalues i = 1/`lcolsN' {
			local lcolCommands `"`macval(lcolCommands)' || scatter `id' `left`i'' if `toused', msymbol(none) mlabel(`leftLB`i'') mlabcolor(black) mlabpos(3) mlabgap(0) mlabsize(`textSize')"'
		}
		forvalues i = 1/`rcolsN' {
			local rcolCommands `"`macval(rcolCommands)' || scatter `id' `right`i'' if `toused', msymbol(none) mlabel(`rightLB`i'') mlabcolor(black) mlabpos(3) mlabgap(0) mlabsize(`textSize')"'
		}


		// FAVOURS
		if `"`favours'"' != `""' {
		
			// continue to allow fp as a main option, but deprecate it in documentation
			// documented way is to specify fp() as a suboption to favours()
			local oldfp `fp'
			local 0 `"`favours'"'
			syntax [anything] [, FP(real -9) *]
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
				local favopts "`options'"
				local 0 `", `options'"'
				syntax [, FORMAT(string) ANGLE(string) LABGAP(string) LABSTYLE(string) LABSize(string) LABColor(string) * ]
				if `"`options'"' != `""' {
					disp as err `"inappropriate suboptions specified to option favours"'
					exit 198
				}		
				if `"`labsize'"'!=`""' local labsizeopt `"labsize(`labsize')"'
				else local labsizeopt `"labsize(`textSize')"'
				if `"`labgap'"'!=`""' local labgapopt `"labgap(`labgap')"'
				else local labgapopt `"labgap(5)"'
				
				if `fp'>0 {
					local leftfp = cond(`DXmin'<=-`fp', `"-`fp' `"`leftfav'"'"', "")
					local rightfp = cond(`fp'<=`DXmax', `"`fp' `"`rightfav'"'"', "")
				}
				else {
					local leftfp = cond(`DXmin'<=`h0', `"`=`DXmin' + (`h0'-`DXmin')/2' `"`leftfav'"'"', "")
					local rightfp = cond(`DXmax'>=`h0', `"`=`h0' + (`DXmax'-`h0')/2' `"`rightfav'"'"', "")
				}

				local favopt = cond(trim(`"`leftfp'`rightfp'"')=="", "", ///
					`"xmlabel(`leftfp' `rightfp', noticks labels norescale `labsizeopt' `labgapopt' `favopts')"')
			}
		}		// end if `"`favours'"' != `""'

		// xtitle - uses 'xmlabel' options, not 'title' options!  Parse all 'title' options to give suitable error message
		else if `"`xtitle'"' != `""' {
			local 0 `"`xtitle'"'
			syntax [anything] [, LABSIZE(string) SIze(string) Color(string) * ]
			if `"`labsize'"'!=`""' local labsizeopt `"labsize(`labsize')"'
			else local labsizeopt `"labsize(`textSize')"'
			
			* Now check for inappropriate options
			local xtitleopts "`options'"
			local 0 `", `options'"'
			syntax [anything] [, TSTYle(string) ORIENTation(string) Justification(string) ///
				ALignment(string) Margin(string) LINEGAP(string) WIDTH(string) HEIGHT(string) BOX NOBOX ///
				BColor(string) FColor(string) LStyle(string) LPattern(string) LWidth(string) LColor(string) ///
				BMargin(string) BEXpand(string) PLACEment(string) *]
			if !(`"`tstyle'"'==`""' & `"`orientation'"'==`""' & `"`justification'"'==`""' & `"`alignment'"'==`""' ///
				& `"`margin'"'==`""' & `"`linegap'"'==`""' & `"`width'"'==`""' & `"`height'"'==`""' & `"`box'"'==`""' & `"`nobox'"'==`""' ///
				& `"`bcolor'"'==`""' & `"`fcolor'"'==`""' & `"`lstyle'"'==`""' & `"`lpattern'"'==`""' & `"`lwidth'"'==`""' & `"`lcolor'"'==`""' ///
				& `"`bmargin'"'==`""' & `"`bexpand'"'==`""' & `"`placement'"'==`""') {
				disp as err `"option xtitle uses xmlabel suboptions, not xtitle suboptions!  see help axis_label_options"'
				exit 198
			}
			
			local xlabval = cond("`null'"!="", `xlablim1', `h0')
			local xtitleopt `"xmlabel(`xlabval' `"`anything'"', noticks labels `labsizeopt' `xtitleopts')"'
		}

		// if both `favours' and `xtitle', `favours' takes precedence.  Print text to explain this
		if `"`favours'"'!=`""' & `"`xtitle'"'!=`""' {
			disp as err "Note: both favours() and xtitle() were specifed; favours() takes precedence"
		}

		// GRAPH APPEARANCE OPTIONS
		local boxSize = `boxscale'/150

		summ `id', meanonly
		local DYmin = r(min)-1
		local DYmax = r(max)+1

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
			replace `ovLine' = `_ES' if `touse' & `check' & `_USE'==`useno' & `useno'>0 & !(`_ES' > `DXmax' | `_ES' < `DXmin')

			sort `touse' `dataid' `olinegroup' `id'
			by `touse' `dataid' `olinegroup' : gen float `ovMin' = `id'[1]-0.5 if `touse' & `_USE'==`useno' & `useno'>0 & !missing(`ovLine')
			by `touse' `dataid' `olinegroup' : gen float `ovMax' = `id'[_N]+0.5 if `touse' & `_USE'==`useno' & `useno'>0 & !missing(`ovLine')
			drop `useno' `olinegroup' `check' /*`dataid'*/
		}

		if `"`cumulative'"'!=`""' replace _USE = 1 if _USE == 3		// June 2015
		
		** MAKE OFF-SCALE ARROWS -- fairly straightforward
		tempvar offscaleL offscaleR offLeftX offLeftX2 offRightX offRightX2 offYlo offYhi
		gen byte `touse2' = `touse' * (`_USE' == 1)
		gen byte `offscaleL' = `touse2' * (float(`_LCI') < `DXmin')
		gen byte `offscaleR' = `touse2' * (float(`_UCI') > `DXmax')

		replace `_LCI' = `DXmin' if `touse2' & float(`_LCI') < `DXmin'
		replace `_UCI' = `DXmax' if `touse2' & float(`_UCI') > `DXmax'
		replace `_LCI' = . if `touse2' & float(`_UCI') < `DXmin'
		replace `_UCI' = . if `touse2' & float(`_LCI') > `DXmax'
		replace `_ES' = . if `touse2' & float(`_ES') < `DXmin'
		replace `_ES' = . if `touse2' & float(`_ES') > `DXmax'
		drop `touse2'

		
		// Comment from Ross Harris in metan.ado
		// DIAMONDS TAKE FOREVER...I DON'T THINK THIS IS WHAT MIKE DID
		tempvar DiamLeftX DiamRightX DiamBottomX DiamTopX DiamLeftY1 DiamRightY1 DiamLeftY2 DiamRightY2 DiamBottomY DiamTopY
		gen byte `touse2' = `touse' * inlist(`_USE', 3, 5)

		gen float `DiamLeftX' = `_LCI' if `touse2'
		replace `DiamLeftX' = `DXmin' if `touse2' & float(`_LCI') < `DXmin'
		replace `DiamLeftX' = . if `touse2' & float(`_ES') < `DXmin'

		gen float `DiamLeftY1' = `id' if `touse2'
		replace `DiamLeftY1' = `id' + 0.4*( abs((`DXmin'-`_LCI')/(`_ES'-`_LCI')) ) if `touse2' & float(`_LCI') < `DXmin'
		replace `DiamLeftY1' = . if `touse2' & `_ES' < `DXmin'
		
		gen float `DiamLeftY2' = `id' if `touse2'
		replace `DiamLeftY2' = `id' - 0.4*( abs((`DXmin'-`_LCI')/(`_ES'-`_LCI')) ) if `touse2' & float(`_LCI') < `DXmin'
		replace `DiamLeftY2' = . if `touse2' & `_ES' < `DXmin'

		gen float `DiamRightX' = `_UCI' if `touse2'
		replace `DiamRightX' = `DXmax' if `touse2' & `_UCI' > `DXmax'
		replace `DiamRightX' = . if `touse2' & `_ES' > `DXmax'
		
		gen float `DiamRightY1' = `id' if `touse2'
		replace `DiamRightY1' = `id' + 0.4*( abs((`_UCI'-`DXmax')/(`_UCI'-`_ES')) ) if `touse2' & float(`_UCI') > `DXmax'
		replace `DiamRightY1' = . if `touse2' & `_ES' > `DXmax'
		
		gen float `DiamRightY2' = `id' if `touse2'
		replace `DiamRightY2' = `id' - 0.4*( abs((`_UCI'-`DXmax')/(`_UCI'-`_ES')) ) if `touse2' & float(`_UCI') > `DXmax'
		replace `DiamRightY2' = . if `touse2' & `_ES' > `DXmax'
		
		gen float `DiamBottomY' = `id' - 0.4 if `touse2'
		replace `DiamBottomY' = `id' - 0.4*( abs((`_UCI'-`DXmin')/(`_UCI'-`_ES')) ) if `touse2' & float(`_ES') < `DXmin'
		replace `DiamBottomY' = `id' - 0.4*( abs((`DXmax'-`_LCI')/(`_ES'-`_LCI')) ) if `touse2' & float(`_ES') > `DXmax'
		
		gen float `DiamTopY' = `id' + 0.4 if `touse2'
		replace `DiamTopY' = `id' + 0.4*( abs((`_UCI'-`DXmin')/(`_UCI'-`_ES')) ) if `touse2' & float(`_ES') < `DXmin'
		replace `DiamTopY' = `id' + 0.4*( abs((`DXmax'-`_LCI')/(`_ES'-`_LCI')) ) if `touse2' & float(`_ES') > `DXmax'

		gen float `DiamTopX' = `_ES' if `touse2'
		replace `DiamTopX' = `DXmin' if `touse2' & float(`_ES') < `DXmin'
		replace `DiamTopX' = `DXmax' if `touse2' & float(`_ES') > `DXmax'
		replace `DiamTopX' = . if `touse2' & (float(`_UCI') < `DXmin' | float(`_LCI') > `DXmax')
		gen float `DiamBottomX' = `DiamTopX'
		
		drop `touse2'
		
	}	// END QUIETLY
		
	
	***************************************
	* Get options and build plot commands *
	***************************************
	
	** "Global" options (includes null line)
	local 0 `", `graphopts'"'
	syntax [, ///
		/// /* standard options */
		BOXOPts(string asis) DIAMOPts(string asis) POINTOPts(string asis) CIOPts(string asis) OLINEOPts(string asis) ///
		/// /* non-diamond options */
		PPOINTOPts(string asis) PCIOPts(string asis) NLINEOPts(string asis) * ]

	local rest `"`options'"'

	* Global CI style (bare lines or capped lines)
	* (Also test for disallowed options during same parse)
	local 0 `", `ciopts'"'
	syntax [, HORizontal VERTical Connect(string asis) RCAP * ]
	if `"`horizontal'"'!=`""' | `"`vertical'"'!=`""' {
		disp as err "ciopts: options horizontal/vertical not allowed"
		exit 198
	}			
	if `"`connect'"' != `""' {
		disp as err "ciopts: option connect() not allowed"
		exit 198
	}
	local ciopts `"`options'"'
	local CIPlotType = cond("`rcap'"=="", "rspike", "rcap")
	local pCIPlotType `CIPlotType'

	* "Default" options
	local dispShape = cond("`interaction'"!="", "circle", "square")
	local DefColor = cond("`classic'"!="", "black", "180 180 180")
	local DefBoxopts = `"mcolor("`DefColor'") msymbol(`dispShape') msize(`boxSize')"'
	if `"`oldbox'"'!=`""' local DefBoxopts `"msymbol(none)"'	// -metan- "nobox" option
	local DefCIopts `"lcolor(black) mcolor(black)"'				// includes "mcolor" for arrows (doesn't affect rspike/rcap)
	local DefPointopts `"msymbol(diamond) mcolor(black) msize(vsmall)"'
	local DefOlineopts `"lwidth(thin) lcolor(maroon) lpattern(shortdash)"'
	local DefDiamopts `"lcolor("0 0 100")"'
	local DefPPointopts `"msymbol("`dispShape'") mlcolor("0 0 100") mfcolor("none")"'
	local DefPCIopts `"lcolor("0 0 100")"'

	* Loop over possible values of `plotid' and test for plot#opts relating specifically to each value
	numlist "1/`np'"
	local plvals=r(numlist)			// need both of these as numlists,
	local pplvals `plvals'			//    for later macro manipulations
	forvalues p = 1/`np' {

		local 0 `", `rest'"'
		syntax [, ///
			/// /* standard options */
			BOX`p'opts(string asis) DIAM`p'opts(string asis) POINT`p'opts(string asis) CI`p'opts(string asis) OLINE`p'opts(string asis) ///
			/// /* non-diamond options */
			PPOINT`p'opts(string asis) PCI`p'opts(string asis) * ]

		local rest `"`options'"'

		* Check if any options were found specifically for this value of `p'
		if trim(`"`box`p'opts'`diam`p'opts'`point`p'opts'`ci`p'opts'`oline`p'opts'`ppoint`p'opts'`pci`p'opts'"') != `""' {
			
			local pplvals : list pplvals - p			// remove from list of "default" plotids
			
			* WEIGHTED SCATTER PLOT
			local 0 `", `box`p'opts'"'
			syntax [, MLABEL(string asis) MSIZe(string asis) * ]			// check for disallowed options
			if `"`mlabel'"' != `""' {
				disp as error "box`p'opts: option mlabel() not allowed"
				exit 198
			}
			if `"`msize'"' != `""' {
				disp as error "box`p'opts: option msize() not allowed"
				exit 198
			}
			qui count if `touse' & `_USE'==1 & `plotid'==`p'
			if r(N) {
				summ `_WT' if `touse' & `_USE'==1 & `plotid'==`p', meanonly
				if !r(N) disp as err `"No weights found for plotid `p'"'
				else if `nd'==1 local scPlot `"`macval(scPlot)' || scatter `id' `_ES' `awweight' if `toused' & `_USE'==1 & `plotid'==`p', `DefBoxopts' `boxopts' `box`p'opts'"'
				else {
					forvalues d=1/`nd' {
						local scPlot `"`macval(scPlot)' || scatter `id' `_ES' `awweight' if `toused' & `_USE'==1 & `plotid'==`p' & `dataid'==`d', `DefBoxopts' `boxopts' `box`p'opts'"'
					}
				}
			}		// N.B. scatter if `toused' <-- "dummy obs" for consistent weighting
			
			* CONFIDENCE INTERVAL PLOT
			local 0 `", `ci`p'opts'"'
			syntax [, HORizontal VERTical Connect(string asis) RCAP LColor(string asis) MColor(string asis) * ]	// check for disallowed options + rcap
			if `"`horizontal'"'!=`""' | `"`vertical'"'!=`""' {
				disp as error "ci`p'opts: options horizontal/vertical not allowed"
				exit 198
			}			
			if `"`connect'"' != `""' {
				di as error "ci`p'opts: option connect() not allowed"
				exit 198
			}
			if `"`lcolor'"'!=`""' & `"`mcolor'"'==`""' {
				local mcolor `lcolor'						// for pc(b)arrow
			}
			local ci`p'opts `"mcolor(`mcolor') lcolor(`lcolor') `options'"'
			local CIPlot`p'Type `CIPlotType'										// global status
			local CIPlot`p'Type = cond("`rcap'"=="", "`CIPlot`p'Type'", "rcap")		// overwrite global status if appropriate
			local CIPlot `"`macval(CIPlot)' || `CIPlot`p'Type' `_LCI' `_UCI' `id' if `touse' & `_USE'==1 & `plotid'==`p' & !`offscaleL' & !`offscaleR', hor `DefCIopts' `ciopts' `ci`p'opts'"'		

			qui count if `plotid'==`p' & `offscaleL' & `offscaleR'
			if r(N) {													// both ends off scale
				local CIPlot `"`macval(CIPlot)' || pcbarrow `id' `_LCI' `id' `_UCI' if `touse' & `plotid'==`p' & `offscaleL' & `offscaleR', `DefCIopts' `ciopts' `ci`p'opts'"'
			}
			qui count if `plotid'==`p' & `offscaleL' & !`offscaleR'
			if r(N) {													// only left off scale
				local CIPlot `"`macval(CIPlot)' || pcarrow `id' `_UCI' `id' `_LCI' if `touse' & `plotid'==`p' & `offscaleL' & !`offscaleR', `DefCIopts' `ciopts' `ci`p'opts'"'
				if "`CIPlot`p'Type'" == "rcap" {			// add cap to other end if appropriate
					local CIPlot `"`macval(CIPlot)' || rcap `_UCI' `_UCI' `id' if `touse' & `plotid'==`p' & `offscaleL' & !`offscaleR', hor `DefCIopts' `ciopts' `ci`p'opts'"'
				}
			}
			qui count if `plotid'==`p' & !`offscaleL' & `offscaleR'
			if r(N) {													// only right off scale
				local CIPlot `"`macval(CIPlot)' || pcarrow `id' `_LCI' `id' `_UCI' if `touse' & `plotid'==`p' & !`offscaleL' & `offscaleR', `DefCIopts' `ciopts' `ci`p'opts'"'
				if "`CIPlot`p'Type'" == "rcap" {			// add cap to other end if appropriate
					local CIPlot `"`macval(CIPlot)' || rcap `_LCI' `_LCI' `id' if `touse' & `plotid'==`p' & !`offscaleL' & `offscaleR', hor `DefCIopts' `ciopts' `ci`p'opts'"'
				}
			}

			* POINT PLOT (point estimates -- except if "classic")
			if "`classic'" == "" {
				local pointPlot `"`macval(pointPlot)' || scatter `id' `_ES' if `touse' & `_USE'==1 & `plotid'==`p', `DefPointopts' `pointopts' `point`p'opts'"'
			}
			
			* OVERALL LINE(S) (if appropriate)
			summ `ovLine' if `plotid'==`p', meanonly
			if r(N) {
				local olinePlot `"`macval(olinePlot)' || rspike `ovMin' `ovMax' `ovLine' if `touse' & `plotid'==`p', `DefOlineopts' `olineopts' `oline`p'opts'"'
			}		

			* POOLED EFFECT - DIAMOND
			* Assume diamond if no "pooled point/CI" options, and no "interaction" option
			if trim(`"`ppointopts'`ppoint`p'opts'`pciopts'`pci`p'opts'`interaction'"') == `""' {
				local diamPlot `"`macval(diamPlot)' || pcspike `DiamLeftY1' `DiamLeftX' `DiamTopY' `DiamTopX' if `touse' & `plotid'==`p', `DefDiamopts' `diamopts' `diam`p'opts'"'
				local diamPlot `"`macval(diamPlot)' || pcspike `DiamTopY' `DiamTopX' `DiamRightY1' `DiamRightX' if `touse' & `plotid'==`p', `DefDiamopts' `diamopts' `diam`p'opts'"'
				local diamPlot `"`macval(diamPlot)' || pcspike `DiamRightY2' `DiamRightX' `DiamBottomY' `DiamBottomX' if `touse' & `plotid'==`p', `DefDiamopts' `diamopts' `diam`p'opts'"'
				local diamPlot `"`macval(diamPlot)' || pcspike `DiamBottomY' `DiamBottomX' `DiamLeftY2' `DiamLeftX' if `touse' & `plotid'==`p', `DefDiamopts' `diamopts' `diam`p'opts'"'
			}
			
			* POOLED EFFECT - PPOINT/PCI
			else {
				if `"`diam`p'opts'"'!=`""' {
					disp as err `"plotid `p': cannot specify options for both diamond and pooled point/CI"'
					disp as err `"diamond options will be ignored"'
				}	
			
				local 0 `", `pci`p'opts'"'
				syntax [, HORizontal VERTical Connect(string asis) RCAP *]
				if `"`horizontal'"'!=`""' | `"`vertical'"'!=`""' {
					disp as error "pci`p'opts: options horizontal/vertical not allowed"
					exit 198
				}			
				if `"`connect'"' != `""' {
					di as error "pci`p'opts: option connect() not allowed"
					exit 198
				}
				local pCIPlot`p'Type `pCIPlotType'											// global status
				local pCIPlot`p'Type = cond("`rcap'"=="", "`pCIPlot`p'Type'", "rcap")		// overwrite global status if appropriate
				local pCIPlot `"`macval(pCIPlot)' || `pCIPlotType' `_LCI' `_UCI' `id' if `touse' & inlist(`_USE', 3, 5) & `plotid'==`p', hor `DefPCIopts' `pciopts' `pci`p'opts'"'
				local ppointPlot `"`macval(ppointPlot)' || scatter `id' `_ES' if `touse' & inlist(`_USE', 3, 5) & `plotid'==`p', `DefPPointopts' `ppointopts' `ppoint`p'opts'"'
			}
		}		// end if trim(`"`box`p'opts'`diam`p'opts'`point`p'opts'`ci`p'opts'`oline`p'opts'`ppoint`p'opts'`pci`p'opts'"') != `""'
	}		// end forvalues p = 1/`np'

	* Find invalid/repeated options
	* any such options would generate a suitable error message at the plotting stage
	* so just exit here with error, to save the user's time
	if regexm(`"`rest'"', "(box|diam|point|ci|oline|ppoint|pci)([0-9]+)") {
		local badopt = regexs(1)
		local badp = regexs(2)
		
		disp as err `"`badopt'`badp'opts: "' _c
		if `: list badp in plvals' disp as err "option supplied multiple times; should only be supplied once"
		else disp as err `"`badp' is not a valid plotid value"'
		exit 198
	}

	local graphopts `rest'		// this is now *just* the standard "twoway" options
								// i.e. the specialist "forestplot" options have been filtered out

					
	* FORM "DEFAULT" TWOWAY PLOT COMMAND (if appropriate)
	* Changed so that FOR WEIGHTED SCATTER each pplval is plotted separately (otherwise weights are messed up)
	* Other (nonweighted) plots can continue to be plotted as before
	if `"`pplvals'"'!=`""' {

		local pplvals2 : copy local pplvals						// copy, just for use in line 1454
		local pplvals : subinstr local pplvals " " ",", all		// so that "inlist" may be used

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
		if `"`pplvals'"'==`"`plvals'"' {		// if no plot#opts specified, can plot all plotid groups at once
			qui summ `_WT' if `_USE'==1 & inlist(`plotid', `pplvals')
			if r(N) {
				if `nd'==1 local scPlot `"`macval(scPlot)' || scatter `id' `_ES' `awweight' if `toused' & `_USE'==1 & inlist(`plotid', `pplvals'), `DefBoxopts' `boxopts'"'
				else {
					forvalues d=1/`nd' {
						local scPlot `"`macval(scPlot)' || scatter `id' `_ES' `awweight' if `toused' & `_USE'==1 & inlist(`plotid', `pplvals') & `dataid'==`d', `DefBoxopts' `boxopts'"'
					}
				}
			}
		}
		else {		// else, need to plot each group separately to maintain correct weighting (July 2014)
			foreach p of local pplvals2 {
				qui summ `_WT' if `_USE'==1 & `plotid'==`p'
				if r(N) {
					if `nd'==1 local scPlot `"`macval(scPlot)' || scatter `id' `_ES' `awweight' if `toused' & `_USE'==1 & `plotid'==`p', `DefBoxopts' `boxopts'"'
					else {
						forvalues d=1/`nd' {
							local scPlot `"`macval(scPlot)' || scatter `id' `_ES' `awweight' if `toused' & `_USE'==1 & `plotid'==`p' & `dataid'==`d', `DefBoxopts' `boxopts'"'
						}
					}
				}
			}
		}		// N.B. scatter if `toused' <-- "dummy obs" for consistent weighting
		
		* CONFIDENCE INTERVAL PLOT
		local 0 `", `ciopts'"'
		syntax [, HORizontal VERTical Connect(string asis) RCAP LColor(string asis) MColor(string asis) * ]	// check for disallowed options + rcap
		if `"`horizontal'"'!=`""' | `"`vertical'"'!=`""' {
			disp as error "ciopts: options horizontal/vertical not allowed"
			exit 198
		}			
		if `"`connect'"' != `""' {
			di as error "ciopts: option connect() not allowed"
			exit 198
		}
		if `"`lcolor'"'!=`""' & `"`mcolor'"'==`""' {
			local mcolor `lcolor'						// for pc(b)arrow
		}
		local ciopts `"mcolor(`mcolor') lcolor(`lcolor') `options'"'
		local CIPlotType = cond("`rcap'"=="", "`CIPlotType'", "rcap")		// overwrite global status if appropriate
		local CIPlot `"`macval(CIPlot)' || `CIPlotType' `_LCI' `_UCI' `id' if `touse' & `_USE'==1 & inlist(`plotid', `pplvals') & !`offscaleL' & !`offscaleR', hor `DefCIopts' `ciopts'"'

		qui count if inlist(`plotid', `pplvals') & `offscaleL' & `offscaleR'
		if r(N) {													// both ends off scale
			local CIPlot `"`macval(CIPlot)' || pcbarrow `id' `_LCI' `id' `_UCI' if `touse' & inlist(`plotid', `pplvals') & `offscaleL' & `offscaleR', `DefCIopts' `ciopts'"'
		}
		qui count if inlist(`plotid', `pplvals') & `offscaleL' & !`offscaleR'
		if r(N) {													// only left off scale
			local CIPlot `"`macval(CIPlot)' || pcarrow `id' `_UCI' `id' `_LCI' if `touse' & inlist(`plotid', `pplvals') & `offscaleL' & !`offscaleR', `DefCIopts' `ciopts'"'
			if "`CIPlotType'" == "rcap" {			// add cap to other end if appropriate
				local CIPlot `"`macval(CIPlot)' || rcap `_UCI' `_UCI' `id' if `touse' & inlist(`plotid', `pplvals') & `offscaleL' & !`offscaleR', hor `DefCIopts' `ciopts'"'
			}
		}
		qui count if inlist(`plotid', `pplvals') & !`offscaleL' & `offscaleR'
		if r(N) {													// only right off scale
			local CIPlot `"`macval(CIPlot)' || pcarrow `id' `_LCI' `id' `_UCI' if `touse' & inlist(`plotid', `pplvals') & !`offscaleL' & `offscaleR', `DefCIopts' `ciopts'"'
			if "`CIPlotType'" == "rcap" {			// add cap to other end if appropriate
				local CIPlot `"`macval(CIPlot)' || rcap `_LCI' `_LCI' `id' if `touse' & inlist(`plotid', `pplvals') & !`offscaleL' & `offscaleR', hor `DefCIopts' `ciopts'"'
			}
		}

		* POINT PLOT
		local pointPlot `"`macval(pointPlot)' || scatter `id' `_ES' if `touse' & `_USE'==1 & inlist(`plotid', `pplvals'), `DefPointopts' `pointopts'"'

		* OVERALL LINE(S) (if appropriate)
		summ `ovLine' if inlist(`plotid', `pplvals'), meanonly
		if r(N) {
			local olinePlot `"`macval(olinePlot)' || rspike `ovMin' `ovMax' `ovLine' if `touse' & inlist(`plotid', `pplvals'), `DefOlineopts' `olineopts'"'
		}

		* POOLED EFFECT - DIAMOND
		* Assume diamond if no "pooled point/CI" options, and no "interaction" option
		if trim(`"`ppointopts'`pciopts'`interaction'"') == `""' {
			local diamPlot `"`macval(diamPlot)' || pcspike `DiamLeftY1' `DiamLeftX' `DiamTopY' `DiamTopX' if `touse' & inlist(`plotid', `pplvals'), `DefDiamopts' `diamopts'"'
			local diamPlot `"`macval(diamPlot)' || pcspike `DiamTopY' `DiamTopX' `DiamRightY1' `DiamRightX' if `touse' & inlist(`plotid', `pplvals'), `DefDiamopts' `diamopts'"'
			local diamPlot `"`macval(diamPlot)' || pcspike `DiamRightY2' `DiamRightX' `DiamBottomY' `DiamBottomX' if `touse' & inlist(`plotid', `pplvals'), `DefDiamopts' `diamopts'"'
			local diamPlot `"`macval(diamPlot)' || pcspike `DiamBottomY' `DiamBottomX' `DiamLeftY2' `DiamLeftX' if `touse' & inlist(`plotid', `pplvals'), `DefDiamopts' `diamopts'"'
		}
		
		* POOLED EFFECT - PPOINT/PCI
		else {
			if `"`diamopts'"'!=`""' {
				disp as err _n `"plotid: cannot specify options for both diamond and pooled point/CI"'
				disp as err `"diamond options will be ignored"'
			}
			
			local 0 `", `pciopts'"'
			syntax [, HORizontal VERTical Connect(string asis) RCAP *]		// check for disallowed options + rcap
			if `"`horizontal'"'!=`""' | `"`vertical'"'!=`""' {
				disp as error "pciopts: options horizontal/vertical not allowed"
				exit 198
			}			
			if `"`connect'"' != `""' {
				di as error "pciopts: option connect() not allowed"
				exit 198
			}
			local pCIPlotType = cond("`rcap'"=="", "`pCIPlotType'", "rcap")		// overwrite global status if appropriate
			local pCIPlot `"`macval(pCIPlot)' || `pCIPlotType' `_LCI' `_UCI' `id' if `touse' & inlist(`_USE', 3, 5) & inlist(`plotid', `pplvals'), hor `DefPCIopts' `pciopts'"'
			local ppointPlot `"`macval(ppointPlot)' || scatter `id' `_ES' if `touse' & inlist(`_USE', 3, 5) & inlist(`plotid', `pplvals'), `DefPPointopts' `ppointopts'"'
		}
	}		// end if `"`pplvals'"'!=`""'
		
	// END GRAPH OPTS

	// DF: modified to use added line approach instead of pcspike (less complex & poss. more efficient as fewer vars)
	// null line (unless switched off)
	if "`null'" == "" {
		local 0 `", `nlineopts'"'
		syntax [, HORizontal VERTical Connect(string asis) * ]
		if `"`horizontal'"'!=`""' | `"`vertical'"'!=`""' {
			disp as err "nlineopts: options horizontal/vertical not allowed"
			exit 198
		}			
		if `"`connect'"' != `""' {
			disp as err "nlineopts: option connect() not allowed"
			exit 198
		}
		local nullCommand `"|| function y=`h0', horiz range(`DYmin' `borderline') n(2) lwidth(thin) lcolor(black) `options'"'
	}



	***************************
	***     DRAW GRAPH      ***
	***************************

	#delimit ;

	twoway
	/* OVERALL AND NULL LINES FIRST */ 
		`olinePlot' `nullCommand'
	/* PLOT BOXES AND PUT ALL THE GRAPH OPTIONS IN THERE, PLUS NOTE FOR RANDOM-EFFECTS */ 
		`scPlot' `notecmd'
			yscale(range(`DYmin' `DYmax') noline) ylabel(none) ytitle("")
			xscale(range(`AXmin' `AXmax')) xlabel(`xlblcmd', labsize(`textSize') `xlabopts')
			yline(`borderline', lwidth(thin) lcolor(gs12))
	/* FAVOURS OR XTITLE */
			`favopt' `xtitleopt'
	/* PUT LABELS UNDER xticks? Yes as labels now extended */
			xtitle("") legend(off) xtick("`xticklist'")
	/* NEXT, CONFIDENCE INTERVALS (plus offscale if necessary) */
		`CIPlot'
	/* DIAMONDS (or markers+CIs if appropriate) FOR SUMMARY ESTIMATES */
		`diamPlot' `ppointPlot' `pCIPlot'
	/* COLUMN VARIBLES (including effect sizes and weights on RHS by default) */
		`lcolCommands' `rcolCommands'
	/* LAST OF ALL PLOT EFFECT MARKERS TO CLARIFY */
		`pointPlot'
	/* Other options */
		|| , `graphopts' /* RMH added */ plotregion(margin(zero)) ;

	#delimit cr

end





program define getWidth
version 9.0

//	ROSS HARRIS, 13TH JULY 2006
//	TEXT SIZES VARY DEPENDING ON CHARACTER
//	THIS PROGRAM GENERATES APPROXIMATE DISPLAY WIDTH OF A STRING
//  (in terms of the current graphics font)
//	FIRST ARG IS STRING TO MEASURE, SECOND THE NEW VARIABLE

//	PREVIOUS CODE DROPPED COMPLETELY AND REPLACED WITH SUGGESTION
//	FROM Jeff Pitblado

qui {
	gen `2' = 0
	count
	local N = r(N)
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

* Subroutine to "spread" titles out over multiple lines if appropriate
* (copied from ipdmetan)
* Updated July 2014
program SpreadTitle, rclass

	syntax anything(name=title id="title string"), [TArget(integer 0) MAXWidth(integer 0) MAXLines(integer 0) FORCE]
	* Target = aim for this width, but allow expansion if alternative is wrapping "too early" (i.e before line is adequately filled)
	* Maxwidth = absolute maximum width.
	
	if `"`title'"'==`""' {
		return scalar nlines = 0
		return scalar maxwidth = 0
		exit
	}
	if !`target' {
		if !`maxwidth' & "`force'"=="" {
			disp as err "must specify at least one of target or maxwidth"
			exit 198
		}
		local target = `maxwidth'
	}
	
	if `maxwidth' {
		cap assert `maxwidth'>=`target'
		if _rc {
			disp as err "maxwidth value must be greater than or equal to target value"
			exit 198
		}
	}
	
	local titlelen = length(`title')
	local spread = int(`titlelen'/`target') + 1
	
	local line = 1
	local end = 0
	local count = 2

	local title1 = word(`title', 1)
	local newwidth = length(`"`title1'"')
	local next = word(`title', 2)
	
	while `"`next'"' != "" {
		local check = trim(`"`title`line''"' + " " +`"`next'"')			// (potential) next iteration of `title`line''
		if length(`"`check'"') > `titlelen'/`spread' {					// if too long
																		// and further from target than before, or greater than maxwidth
			if abs(length(`"`check'"')-(`titlelen'/`spread')) > abs(length(`"`title`line''"')-(`titlelen'/`spread')) ///
				| (`maxwidth' & length(`"`check'"') > `maxwidth') {
				if `maxlines' & `line'==`maxlines' {					// if reached max no. of lines
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
		local next = word(`title', `count')
		local newwidth = max(`newwidth', length(`"`title`line''"'))		// update `newwidth'
	}

	* If last string is too long (including in above case), truncate
	if `newwidth' <= `target' local maxwidth = `target'
	else local maxwidth = cond(`maxwidth', min(`newwidth', `maxwidth'), `newwidth')
	if length(`"`title`line''"') > `maxwidth' local title`line' = substr(`"`title`line''"', 1, `maxwidth')
	
	* Return strings
	forvalues i=1/`line' {
		return local title`i' = trim(`"`title`i''"')
	}
	return scalar nlines = `line'
	return scalar maxwidth = min(`newwidth', `maxwidth')
	
end

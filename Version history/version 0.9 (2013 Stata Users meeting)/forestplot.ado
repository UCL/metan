** Graphics subroutine -- now also works as stand-alone program
*! version 0.9  10sep2013  David Fisher

* N.B. 1
* use == 0  subgroup labels
* use == 1  successfully estimated trial-level effects
* use == 2  unsuccessfully estimated trial-level effects ("Insufficient data")
* use == 3  subgroup effects
* use == 4  blank line, or between-subgroup heterogeneity info
* use == 5  overall effect
* use == 9  titles

program define forestplot
version 9.0		// metan is v9 and this doesn't use any more recent commands/syntaxes

syntax [namelist(min=3 max=5)] [if] [in] [, ///
	/// /* Filenames to load and use */
	USING(string asis) /*USEDATA*/ ///
	/// /* General -forestplot- options (including any "passed through" from another program, e.g. ipdmetan) */
	PASSthru LABELS(name) BY(name) noNAME noOVERALL noSUBGRoup noSTATS noWT ///
	TYPE(string) EFFECT(string) LCOLS(namelist) RCOLS(namelist) EFORM DP(integer 2) NULLOFF ///
	  /// /* x-axis options */
	XLABEL(string) XTICK(string) XTITLE(string asis) FAVOURS(string asis) RANGE(numlist min=2 max=2) FP(real 999) ///
	  /// /* proportions/aspect options */
	ASTEXT(integer 50) TEXTSize(real 100.0) BOXSCA(real 100.0) ASPECT(real 0) XSIZE(real 0) YSIZE(real 0) ///
	* ]

	local graphopts `"`options'"'		// use different name so that subsequent "syntax" commands will not overwrite	
	
	* Set up variable names
	if `"`namelist'"'==`""' {		// if not specified, assume "standard" varnames			
		local ES "_ES"
		local lci "_lci"
		local uci "_uci"
		local wgt "_wgt"
		local use "_use"
	}
	else {							// else check syntax of user-specified varnames
		local 0 `namelist'
		syntax varlist(min=3 max=5 numeric)
		tokenize `varlist'
		local ES `1'
		local lci `2'
		local uci `3'
		local wgt = cond(`"`4'"'!=`""', `"`4'"', "_wgt")
		local use = cond(`"`5'"'!=`""', `"`5'"', "_use")
	}

	* Check `passthru' and `using' compatibility
	if `"`passthru'"'!=`""'	& `"`using'"'!=`""' {
		disp as err "Cannot use external files if called from within another command"
		exit 198
	}
	if `"`passthru'"'==`""' preserve	// If passthru from another prog (e.g. ipdmetan), data is already preserved


	*** Set up data to use
	tempvar usingN
	local usoptN=1
	/*
	* Data in memory
	if `"`using'"'==`""' local usedata `"usedata"'	// must be using data in memory if no "using"; makes subsequent code simpler
	if `"`usedata'"' != `""' {
		gen int `usingN' = 0
	*/
	if `"`using'"' == `""' { 			// using data in memory
		gen int `usingN' = 1
		capture confirm numeric var `use'
		if _rc>0 & _rc!=7 {
			gen `use' = cond(missing(`ES'*`lci'*`uci'), 2, 1)
		}
		else if _rc==7 {
			disp as err `"variable `use' exists but is not numeric"'
			exit 198
		}
		marksample touse
		qui keep if `touse'
		qui drop `touse'
	}

	* External files
	if `"`using'"' != `""' {
		local options2 `graphopts'
		local 0 `using'							// parse first "using"
		syntax anything(everything) [, *]		// would like to use "syntax anything [if] [in] [, *]"
												// but Stata insists that the "if" variable exists NOW, whereas here it does not.
		local i=1								// so we have to parse the "anything" manually.
		while `"`anything'"' != "" {
			gettoken next anything : anything
			if inlist(`"`next'"', "if", "in", "") {
				if `i'==1 {
					disp as err "must specify a filename to using()"
					exit 198
				}
				local usifin`usoptN' `"`next' `anything'"'		// save `rest' for parsing properly later
				continue, break
			}			
			confirm file `next'
			local using`usoptN' `"`using`usoptN'' `next'"'	// filename list
			local ++i
		}
		local usopts`usoptN' `options'					// "options" list

		local 0 `", `options2'"'			// parse remaining forestplot options to find subsequent "using"s
		syntax [, USING(string asis) *]

		* Form groups based on whether successive "using"s have same options or not
		* If no differences, may as well be part of the same group
		while `"`using'"' != `""' {
			local options2 `options'
			local 0 `using'						// parse subsequent "using"s
			syntax anything(everything) [, *]	// (same as above)
			local i=1
			while `"`anything'"' != "" {
				gettoken next anything : anything
				if inlist(`"`next'"', "if", "in", "") {
					if `i'==1 {
						disp as err "must specify a filename to using()"
						exit 198
					}
					local tempifin `"`next' `anything'"'
					continue, break
				}			
				confirm file `next'
				local tempfilelist `"`tempfilelist' `next'"'	// filename list
				local ++i
			}
			if !(`"`options'"'==`"`usopts`usoptN''"' & trim(itrim(`"`tempifin'"'))==trim(itrim(`"`usifin`usoptN''"'))) {
				local ++usoptN							// if options differ, increment to form new list
				local usopts`usoptN' `"`options'"'		// (new) options list			
				local usifin`usoptN' `"`tempifin'"'		// (new) `if' and `in' (for parsing properly later)
			}
			local using`usoptN' `"`using`usoptN'' `tempfilelist'"'		// filenames list (possibly add to previous list)		

			local 0 `", `options2'"'			// parse remaining forestplot options to find subsequent "using"s
			syntax [, USING(string asis) *]
		}
		local graphopts `options'

		* Now run through each "group", append files, and assign a group ID (`usingN')
		forvalues i=1/`usoptN' {
			local Nusing`i' : word count `using`i''

			forvalues j=1/`Nusing`i'' {
				local x : word `j' of `using`i''

				if `i'*`j'==1 /*& `"`usedata'"'==`""'*/ {		// first time only, if not using data in memory
					qui use `x', clear
					qui gen int `usingN' = 1
					capture confirm numeric var `use'
					if _rc>0 & _rc!=7 {
						gen `use' = cond(missing(`ES'*`lci'*`uci'), 2, 1)
					}
					else if _rc==7 {
						disp as err `"variable `use' exists but is not numeric"'
						exit 198
					}
				}
				else {										// otherwise, append file and assume `use' already exists
					local nobs1 = _N + 1					// add extra row before appending
					qui set obs `nobs1'
					qui replace `use' = 4 in `nobs1'
					qui append using `x'
				}
			}
			qui replace `usingN' = `i' if missing(`usingN')
			qui replace `use' = cond(missing(`ES'*`lci'*`uci'), 2, 1) if missing(`use') & `usingN'==`i'
			
			local 0 `usifin`i''
			syntax [if] [in]
			marksample touse
			qui replace `touse'=1 if `usingN'!=`i'			// don't markout any obs from outside the current group
			qui keep if `touse'
			qui drop `touse'
		}
	}	// end if `"`using'"' != `""'
	
	* Check `use' and `usingN'
	confirm numeric var `use'
	summ `usingN', meanonly
	if r(min) == 0 qui replace `usingN' = `usingN' + 1		// if data in memory, first value is 0. Shift so that first value is 1.
	summ `usingN', meanonly
	assert r(max) == `usoptN'
	
	* Check existence of `ES', `lci', `uci' (required)
	foreach x in ES lci uci {
		confirm numeric var ``x''
	}
	
	* Check existence of `wgt' (optional for user)
	capture confirm numeric var `wgt'
	if _rc & _rc!=7 gen `wgt' = 1			// generate as constant if doesn't exist
	else if _rc==7 {
		disp as err `"variable `wgt' exists but is not numeric"'
		exit 198
	}
	
	* Check validity of `use' (already sorted out existence)
	capture assert !missing(`ES'*`lci'*`uci') if `use' == 1
	local rctemp = _rc
	capture assert missing(`ES'*`lci'*`uci') if `use' == 2
	if `rctemp' | _rc {
		disp as err `"invalid effect sizes with user-defined "_use" variable"'
		exit 198
	}
	
	* Check existence of `labels' and `by'
	foreach x in labels by {
		if `"``x''"'!=`""' confirm var ``x''
		else capture confirm var _`x'
		if !_rc local `x' "_`x'"			// use default varnames if they exist and option not explicitly given
	}

	* Rename `weight' option so that subsequent "syntax" commands will not overwrite it
	local awweight "[aw= `wgt']"
	qui summ `wgt'
	if r(N) == 0 {
		local awweight ""
	}

	* nooverall, nosubgroup
	qui drop if `"`overall'"'!=`""' & `use' == 5
	qui drop if `"`subgroup'"'!=`""' & `use' == 3
	
	* Generate ordering variable (reverse sequential, since y axis runs bottom to top)
	* confirm new var id
	assert inrange(`use', 0, 5)
	tempvar id
	qui gen int `id' = _N - _n + 1
	* replace id = id - 0.5 if use == 5						// DF: extra half-spacing for overall (CHECK EFFECT OF SUBGROUPS HERE)
	* come back to this 
	
	qui replace `wgt'=`wgt'*100
	
	* Test validity of lcols and rcols
	foreach x in lcols rcols {
		if `"``x''"'!=`""' {
			local 0 ``x''
			syntax varlist
		}
	}
	
	* Default "lcol1" (if using ipdmetan) is list of study names, headed "Study ID"
	* If "_labels" exists, check whether labels exist for use==1 | use==2
	if `"`labels'"'==`""' local name `"noname"'		// turn on option noNAME
	else {
		capture assert missing(`labels') if inlist(`use', 1, 2)
		if !_rc local name `"noname"'				// turn on option noNAME
		else {
			tempvar names
			qui clonevar `names' = `labels' if inlist(`use', 1, 2)
		}
	}
	if "`name'" == `""' {
		local lcols = `"`names' `lcols'"'		// N.B. macro `name' is "optionally off"; macro `names' contains study names!
	}

	* ES and weight text columns
	tempvar estText weightText
	if `"`eform'"'!=`""' local xexp "exp"
	qui gen str `estText' = string(`xexp'(`ES'), "%10.`dp'f") ///
		+ " (" + string(`xexp'(`lci'), "%10.`dp'f") + ", " + string(`xexp'(`uci'), "%10.`dp'f") + ")" ///
		if inlist(`use', 1, 3, 5) & !missing(`ES')
	qui replace `estText' = "(Insufficient data)" if `use' == 2
	qui replace `estText' = " " + `estText' if `ES' >= 0 & `use' != 4	// indent by one character if non-negative, to line up

	qui gen str `weightText' = string(`wgt', "%4.2f") if inlist(`use', 1, 3, 5) & !missing(`ES')
	qui replace `weightText' = "" if `use' == 2

	
	// GET MIN AND MAX DISPLAY
	// SORT OUT TICKS- CODE PINCHED FROM MIKE AND FIDDLED. TURNS OUT I'VE BEEN USING SIMILAR NAMES...
	// AS SUGGESTED BY JS JUST ACCEPT ANYTHING AS TICKS AND RESPONSIBILITY IS TO USER!
	summ `lci', meanonly
	local DXmin = r(min)			// minimum confidence limit
	summ `uci', meanonly
	local DXmax = r(max)			// maximum confidence limit
	// DXmin & DXmax ARE THE LEFT AND RIGHT COORDS OF THE GRAPH PART

	local h0=0
	
	* xlabel not supplied by user: choose sensible values
	* default is for symmetrical limits, with 3 labelled values including null
	if "`xlabel'" == "" {
		local Gmodxhi=max(abs(float(`DXmin')), abs(float(`DXmax')))
		if `Gmodxhi'==. {
			local Gmodxhi=2
		}
		local DXmin=-`Gmodxhi'
		local DXmax=`Gmodxhi'
		
		* DF March 2013: choose "sensible" label values for x-axis
		if `"`eform'"'==`""' {		// linear scale
			local mag = ceil(abs(log10(abs(float(`Gmodxhi')))))*sign(log10(abs(float(`Gmodxhi'))))	// order of magnitude
			local xdiff = abs(float(`Gmodxhi')-`mag')
			local xlab = `"`h0'"'
			foreach i of numlist 1 2 5 10 {
				local ii = `i'^`mag'
				if abs(float(`Gmodxhi') - `ii') <= float(`xdiff') {
					local xdiff = abs(float(`Gmodxhi') - `ii')
					local xlab = `"`xlab' `ii' -`ii'"'
				}
			}
		}
		else {						// log scale
			local mag = round(`Gmodxhi'/ln(2))
			local xdiff = abs(float(`Gmodxhi') - float(ln(2)))
			local xlab `"`h0'"'
			forvalues i=1/`mag' {
				local ii = ln(2^`i')
				local xlab = `"`xlab' `ii' -`ii'"'		// display all powers of 2
			}
			
			* If effect is small, use 1.5, 1.33, 1.25 or 1.11 instead, as appropriate
			foreach i of numlist 1.5 `=1/0.75' 1.25 `=1/0.9' {
				local ii = ln(`i')
				if abs(float(`Gmodxhi') - `ii') <= float(`xdiff') {
					local xdiff = abs(float(`Gmodxhi') - `ii')
					local xlab = `"`xlab' `ii' -`ii'"'
				}
			}					
		}
		numlist `"`xlab'"'
		local xlablist=r(numlist)
	}
	
	* xlabel supplied by user: parse and apply
	else {
		local 0 `"`xlabel'"'
		syntax anything(name=xlablist) [, FORCE *]
		local xlabopts `"`options'"'

		if `"`eform'"'!=`""' {					// assume given on exponentiated scale if "eform" specified, so need to take logs
			numlist "`xlablist'", range(>0)		// in which case, all values must be greater than zero
			local n : word count `r(numlist)'
			forvalues i=1/`n' {
				local xi : word `i' of `r(numlist)'
				local xlablist2 `"`xlablist2' `=ln(`xi')'"'
			}
			local xlablist "`xlablist2'"
		}
		if "`force'" == "" {
			numlist "`xlablist' `DXmin' `DXmax'", sort
			local n : word count `r(numlist)' 
			local DXmin2 : word 1 of `r(numlist)'
			local DXmax2 : word `n' of `r(numlist)'
		
			local Gmodxhi=max(abs(`DXmin'), abs(`DXmax'), abs(`DXmin2'), abs(`DXmax2'))	
			if `Gmodxhi'==.  local Gmodxhi=2
			local DXmin=-`Gmodxhi'
			local DXmax=`Gmodxhi'
		}										// "force" option only changes things if user supplies xlabel
		else {
			numlist "`xlablist'", sort
			local n : word count `r(numlist)' 
			local DXmin : word 1 of `r(numlist)'
			local DXmax : word `n' of `r(numlist)'
		}
	}
	
	* Ticks
	if "`xtick'" == "" {
		local xticklist `xlablist'		// if not specified, default to same as labels
	}
	else {
		gettoken xticklist : xtick, parse(",")
		if `"`eform'"'!=`""' {					// assume given on exponentiated scale if "eform" specified, so need to take logs
			numlist "`xticklist'", range(>0)		// in which case, all values must be greater than zero
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
	
	* Range
	if "`range'" != `""' {
		if `"`eform'"'!=`""' {
			numlist "`range'", range(>0)
			tokenize "`range'"
			local range `"`=ln(`1')' `=ln(`2')'"'
		}
		else {
			numlist "`range'"
			local range=r(numlist)
		}
	}

	* Final calculation of DXmin and DXmax
	if "`range'" == `""' {
		numlist "`xlablist' `xticklist' `DXmin' `DXmax'", sort
		local n : word count `r(numlist)' 
		local DXmin : word 1 of `r(numlist)'
		local DXmax : word `n' of `r(numlist)'
	}
	else {
		numlist "`range'", sort
		local n : word count `r(numlist)' 
		local DXmin : word 1 of `r(numlist)'
		local DXmax : word `n' of `r(numlist)'
	}
		
	* If on exponentiated scale, re-label x-axis with exponentiated values (nothing else should need changing)
	if "`eform'" != "" {
		local xlblcmd ""
		foreach i of numlist `xlablist' {
			local lbl = string(`=exp(`i')',"%7.3g")
			local xlblcmd `"`xlblcmd' `i' "`lbl'""'
		}
	}
	else local xlblcmd `"`xlablist'"'
		
	local DXwidth = `DXmax'-`DXmin'
	if `DXmin' > 0 local h0 = 1				// presumably just an extra failsafe


// END OF TICKS AND LABELS

// MAKE OFF-SCALE ARROWS -- fairly straightforward
quietly {
	tempvar offscaleL offscaleR offLeftX offLeftX2 offRightX offRightX2 offYlo offYhi
	gen `offscaleL' = `lci' < `DXmin' & `use' == 1
	gen `offscaleR' = `uci' > `DXmax' & `use' == 1
	
	replace `lci' = `DXmin' if `lci' < `DXmin' & `use' == 1
	replace `uci' = `DXmax' if `uci' > `DXmax' & `use' == 1
	replace `lci' = . if `uci' < `DXmin' & `use' == 1
	replace `uci' = . if `lci' > `DXmax' & `use' == 1
	replace `ES' = . if `ES' < `DXmin' & `use' == 1
	replace `ES' = . if `ES' > `DXmax' & `use' == 1
}	// end quietly



*** Columns 
// OPTIONS FOR L-R JUSTIFY?
// HAVE ONE MORE COL POSITION THAN NECESSARY, COULD THEN R-JUSTIFY
// BY ADDING 1 TO LOOP, ALSO HAVE MAX DIST FOR OUTER EDGE
// HAVE USER SPECIFY % OF GRAPH USED FOR TEXT?

quietly {	// KEEP QUIET UNTIL AFTER DIAMONDS

	// TITLES
	summ `id' if `use' != 9
	local max = r(max)
	local new = r(N)+4
	if `new' > _N set obs `new'		// create four new observations

	forvalues i = 1/4 {				// up to four lines for titles
		local idNew`i' = `max' + `i'
		local Nnew`i'=r(N)+`i'
		replace `id' = `idNew`i'' + 1 in `Nnew`i''
		* replace `use' = 1 in `Nnew`i''
		* if `i' == 1 local borderline = `idNew`i'' - 0.25
	}
	local borderline = `idNew1' - 0.25
	
	// LEFT COLUMNS
	* local maxline = 1
	if `"`lcols'"' != "" {
		local lcolsN = 0
		foreach x of local lcols {
			capture confirm var `x'
			if _rc!=0 {
				disp as err "Variable `x' not defined"
				exit _rc
			}
			local ++lcolsN
			
			tempvar left`lcolsN' leftLB`lcolsN' leftWD`lcolsN'
			capture confirm string var `x'
			if !_rc gen str `leftLB`lcolsN'' = `x'
			else {
				capture decode `x', gen(`leftLB`lcolsN'')
				if _rc {
					local f: format `x'
					gen str `leftLB`lcolsN'' = string(`x', "`f'")
					replace `leftLB`lcolsN'' = "" if `leftLB`lcolsN'' == "."
				}
			}
			* TEMP CHANGE 31st JULY 2013 -- can this be decided before invoking 'forestplot'?
			* replace `leftLB`lcolsN'' = "" if !inlist(`use', 1, 2)	// "lcol" values for trial estimates only
			local colName: variable label `x'
			if `"`colName'"' == "" & `"`x'"' !=`"`names'"' local colName = `"`x'"'
			
			// WORK OUT IF TITLE IS BIGGER THAN THE VARIABLE
			// SPREAD OVER UP TO FOUR LINES IF NECESSARY
			local titleln = length(`"`colName'"')
			tempvar tmpln
			gen `tmpln' = length(`leftLB`lcolsN'')
			qui summ `tmpln' if `use' != 0
			local otherln = r(max)
			drop `tmpln'
			
			// NOW HAVE LENGTH OF TITLE AND MAX LENGTH OF VARIABLE
			local spread = int(`titleln'/`otherln')+1
			if `spread' > 4 local spread = 4

			local line = 1
			local end = 0
			local count = -1
			local c2 = -2

			local first = word(`"`colName'"', 1)
			local last = word(`"`colName'"', `count')
			local nextlast = word(`"`colName'"', `c2')

			while `end' == 0 {
				replace `use' = 9  in `Nnew`line''		// added by DF - use 9 for titles (not used elsewhere)
				replace `leftLB`lcolsN'' = `"`last'"' + " " + `leftLB`lcolsN'' in `Nnew`line''
				local check = `leftLB`lcolsN''[`Nnew`line''] + `" `nextlast'"'	// what next will be

				local --count
				local last = word(`"`colName'"', `count')
				if `"`last'"' == "" local end = 1
	
				if length(`leftLB`lcolsN''[`Nnew`line'']) > `titleln'/`spread' | ///
					length(`"`check'"') > `titleln'/`spread' & `"`first'"' == `"`nextlast'"' {
					if `end' == 0 {
						local ++line
					}
				}
			}
			* if `line' > `maxline'{
			* 	local maxline = `line'
			* }
		}		// end of foreach x of local lcols
	}
	* Now copy across previously generated titles (overall, sub est etc.)
	if `"`leftLB1'"'==`""' {
		tempvar left1 leftLB1 leftWD1
		gen `leftLB1'=""
		local lcolsN=1
	}
	if `"`labels'"'!=`""' replace `leftLB1' = `labels' if inlist(`use', 0, 3, 4, 5)	// "4" ADDED BY DF 31st JULY 2013 - CHECK FOR UNINTENDED CONSEQUENCES

	// RIGHT COLUMNS
	// by default, rcols 1 and 2 are effect sizes and weights
	if "`wt'" == "" {
		local rcols = "`weightText' " + "`rcols'"
		label var `weightText' "% Weight"
	}
	if "`stats'" == "" {
		local rcols = "`estText' " + "`rcols'"
		if "`effect'" == "" local effect `"Effect"'
		label var `estText' "`effect' (`c(level)'% CI)"
	}

	// Is this "sorted out the extra top line that appears in column labels" ??
	// Doesn't seem to do anything, but presumably it does on occasion
	tempvar extra
	gen `extra' = ""
	label var `extra' " "
	local rcols = `"`rcols' `extra'"'

	local rcolsN = 0
	if `"`rcols'"' != "" {
		local rcolsN = 0
		foreach x of local rcols {
			capture confirm var `x'
			if _rc {
				disp as err "Variable `x' not defined"
				exit _rc
			}
			local ++rcolsN
			
			tempvar right`rcolsN' rightLB`rcolsN' rightWD`rcolsN'
			cap confirm string var `x'
			if !_rc gen str `rightLB`rcolsN'' = `x'
			else {
				local f: format `x'
				gen str `rightLB`rcolsN'' = string(`x', "`f'")
				replace `rightLB`rcolsN'' = "" if `rightLB`rcolsN'' == "."
			}
			local colName: variable label `x'
			if `"`colName'"' == "" local colName = `"`x'"'
			
			// WORK OUT IF TITLE IS BIGGER THAN THE VARIABLE
			// SPREAD OVER UP TO FOUR LINES IF NECESSARY
			local titleln = length(`"`colName'"')
			tempvar tmpln
			gen `tmpln' = length(`rightLB`rcolsN'')
			qui summ `tmpln' if `use' != 0
			local otherln = r(max)
			drop `tmpln'
			
			// NOW HAVE LENGTH OF TITLE AND MAX LENGTH OF VARIABLE
			local spread = int(`titleln'/`otherln')+1
			if `spread' > 4 local spread = 4

			local line = 1
			local end = 0
			local count = -1
			local c2 = -2

			local first = word(`"`colName'"', 1)
			local last = word(`"`colName'"', `count')
			local nextlast = word(`"`colName'"', `c2')

			while `end' == 0 {
				replace `use' = 9  in `Nnew`line''		// added by DF - use 9 for titles (not used elsewhere)
				replace `rightLB`rcolsN'' = `"`last'"' + " " + `rightLB`rcolsN'' in `Nnew`line''
				local check = `rightLB`rcolsN''[`Nnew`line''] + `" `nextlast'"'	// what next will be

				local --count
				local last = word(`"`colName'"', `count')
				if `"`last'"' == "" local end = 1
	
				if length(`rightLB`rcolsN''[`Nnew`line'']) > `titleln'/`spread' | ///
					length(`"`check'"') > `titleln'/`spread' & `"`first'"' == `"`nextlast'"' {
					if `end' == 0 {
						local ++line
					}
				}
			}
			* if `line' > `maxline'{
			* 	local maxline = `line'
			* }
		}		// end of foreach x of local rcols
	}
	
	// now get rid of extra title rows if they weren't used
	drop if `use' == .
	
	* if `maxline'==3 drop in `Nnew4'
	* if `maxline'==2 drop in `Nnew3'/`Nnew4'
	* if `maxline'==1 drop in `Nnew2'/`Nnew4'

	/* BODGE SOLU- EXTRA COLS */
	while `rcolsN' < 2 {
		local ++rcolsN
		tempvar right`rcolsN' rightLB`rcolsN' rightWD`rcolsN'
		gen str `rightLB`rcolsN'' = " "
	}
	
	// sort out titles for stats and weight, if there
	local skip = 1
	if "`stats'" == "" & "`wt'" == "" local skip = 3
	if "`stats'" != "" & "`wt'" == "" local skip = 2
	if "`stats'" == "" & "`wt'" != "" local skip = 2
	if "`counts'" != "" local skip = `skip' + 2
	
	/* SET TWO DUMMY RCOLS IF NOSTATS NOWEIGHT */
	forvalues i = `skip'/`rcolsN' {					// get rid of junk if not weight, stats or counts
		replace `rightLB`i'' = "" if !inlist(`use', 1, 2, 3, 5, 9)		// REVISIT: "3", "5" ADDED BY DF 30 MAY 2013 - TEST FOR KNOCK-ON CONSEQUENCES
	}
	forvalues i = 1/`rcolsN' {
		replace `rightLB`i'' = "" if `use' == 0
	}

	// Calculate "leftWDtot" and "rightWDtot" -- the total widths to left and right of graph area
	// Don't use titles or overall stats, just trial stats.
	local leftWDtot = 0
	local rightWDtot = 0
	local leftWDtotNoTi = 0

	forvalues i = 1/`lcolsN' {
		getWidth `leftLB`i'' `leftWD`i''
		qui summ `leftWD`i'' if inlist(`use', 1, 2)
		local maxL = r(max)
		local leftWDtot = `leftWDtot' + `maxL'
		replace `leftWD`i'' = `maxL'
	}
	forvalues i = 1/`rcolsN' {
		getWidth `rightLB`i'' `rightWD`i''
		qui summ `rightWD`i'' if inlist(`use', 1, 2)
		replace `rightWD`i'' = r(max)
		local rightWDtot = `rightWDtot' + r(max)
	}
	
	// CHECK IF NOT WIDE ENOUGH (I.E., OVERALL INFO TOO WIDE)
	// LOOK FOR EDGE OF DIAMOND summ `lci' if `use' == ...
	tempvar maxLeft
	getWidth `leftLB1' `maxLeft'
	qui count if inlist(`use', 0, 3, 5)
	if r(N) > 0 {
		summ `maxLeft' if inlist(`use', 0, 3, 5)	// NOT TITLES THOUGH! -- DF AUG 2013 Why not titles? (use=0) Seems necessary?
		local max = r(max)
		if `max' > `leftWDtot'{
			// WORK OUT HOW FAR INTO PLOT CAN EXTEND
			// We want ymin = x - 1.  Given that, rearrange to find new leftWDtot:
			tempvar lci2
			qui gen `lci2' = cond(`lci'>0, 0, `lci')
			qui summ `lci2' if inlist(`use', 3, 5)
			local lcimin=r(min)
			
			// BUT don't make it any less than before
			local leftWDtot = max(`leftWDtot', ///
				((`max'+`rightWDtot') / ( ( ((`lcimin'-`DXmin')/`DXwidth') * ((100-`astext')/`astext') ) + 1)) - `rightWDtot')

			drop `lci2' `diff' `ileftWD1'
		}
	}
	
	// Generate position of lcols, using user-specified `astext'
	// (% of graph width taken by text)
	local textWD = (`DXwidth'/(1-`astext'/100) - `DXwidth') / (`leftWDtot'+`rightWDtot')

	// Now, carry on as before
	local leftWDtot2 = `leftWDtot'
	forvalues i = 1/`lcolsN'{
		gen `left`i'' = `DXmin' - `leftWDtot2'*`textWD'
		local leftWDtot2 = `leftWDtot2'-`leftWD`i''
	}
	
	gen `right1' = `DXmax'
	forvalues i = 2/`rcolsN'{
		local r2 = `i'-1
		gen `right`i'' = `right`r2'' + `rightWD`r2''*`textWD'
	}

	// AXmin AXmax ARE THE OVERALL LEFT AND RIGHT COORDS
	local AXmin = `left1'
	local AXmax = `DXmax' + `rightWDtot'*`textWD'
	
	// DIAMONDS TAKE FOREVER...I DON'T THINK THIS IS WHAT MIKE DID
	tempvar DIAMleftX DIAMrightX DIAMbottomX DIAMtopX DIAMleftY1 DIAMrightY1 DIAMleftY2 DIAMrightY2 DIAMbottomY DIAMtopY
	tempvar touse2
	gen `touse2' = inlist(`use', 3, 5)

	gen `DIAMleftX' = `lci' if `touse2'
	replace `DIAMleftX' = `DXmin' if `lci' < `DXmin' & `touse2'
	replace `DIAMleftX' = . if `ES' < `DXmin' & `touse2'

	gen `DIAMleftY1' = `id' if `touse2'
	replace `DIAMleftY1' = `id' + 0.4*( abs((`DXmin'-`lci')/(`ES'-`lci')) ) if `lci' < `DXmin' & `touse2'
	replace `DIAMleftY1' = . if `ES' < `DXmin' & `touse2'
	
	gen `DIAMleftY2' = `id' if `touse2'
	replace `DIAMleftY2' = `id' - 0.4*( abs((`DXmin'-`lci')/(`ES'-`lci')) ) if `lci' < `DXmin' & `touse2'
	replace `DIAMleftY2' = . if `ES' < `DXmin' & `touse2'

	gen `DIAMrightX' = `uci' if `touse2'
	replace `DIAMrightX' = `DXmax' if `uci' > `DXmax' & `touse2'
	replace `DIAMrightX' = . if `ES' > `DXmax' & `touse2'
	
	gen `DIAMrightY1' = `id' if `touse2'
	replace `DIAMrightY1' = `id' + 0.4*( abs((`uci'-`DXmax')/(`uci'-`ES')) ) if `uci' > `DXmax' & `touse2'
	replace `DIAMrightY1' = . if `ES' > `DXmax' & `touse2'
	
	gen `DIAMrightY2' = `id' if `touse2'
	replace `DIAMrightY2' = `id' - 0.4*( abs((`uci'-`DXmax')/(`uci'-`ES')) ) if `uci' > `DXmax' & `touse2'
	replace `DIAMrightY2' = . if `ES' > `DXmax' & `touse2'
	
	gen `DIAMbottomY' = `id' - 0.4 if `touse2'
	replace `DIAMbottomY' = `id' - 0.4*( abs((`uci'-`DXmin')/(`uci'-`ES')) ) if `ES' < `DXmin' & `touse2'
	replace `DIAMbottomY' = `id' - 0.4*( abs((`DXmax'-`lci')/(`ES'-`lci')) ) if `ES' > `DXmax' & `touse2'
	
	gen `DIAMtopY' = `id' + 0.4 if `touse2'
	replace `DIAMtopY' = `id' + 0.4*( abs((`uci'-`DXmin')/(`uci'-`ES')) ) if `ES' < `DXmin' & `touse2'
	replace `DIAMtopY' = `id' + 0.4*( abs((`DXmax'-`lci')/(`ES'-`lci')) ) if `ES' > `DXmax' & `touse2'

	gen `DIAMtopX' = `ES' if `touse2'
	replace `DIAMtopX' = `DXmin' if `ES' < `DXmin' & `touse2'
	replace `DIAMtopX' = `DXmax' if `ES' > `DXmax' & `touse2'
	replace `DIAMtopX' = . if (`uci' < `DXmin' | `lci' > `DXmax') & `touse2'
	gen `DIAMbottomX' = `DIAMtopX'

}	// END QUI


// v1.11 TEXT SIZE SOLU
// v1.16 TRYING AGAIN!
// IF aspect IS SPECIFIED THEN THIS HELPS TO CALCULATE TEXT SIZE
// IF NO ASPECT, BUT xsize AND ysize USED THEN FIND RATIO MANUALLY
// STATA ALWAYS TRIES TO PRODUCE A GRAPH WITH ASPECT ABOUT 0.77 - TRY TO FIND "NATURAL ASPECT"
numlist `"`aspect' `xsize' `ysize'"', range(>=0)				// check that all are >=0
if `xsize' > 0 & `ysize' > 0 & `aspect' == 0 local aspect = `ysize'/`xsize'

local approx_chars = (`leftWDtot' + `rightWDtot')/(`astext'/100)
qui count if `use' != 9
local height = r(N)
local natu_aspect = 1.3 * `height'/`approx_chars'

if `aspect' == 0 {
	// sort out relative to text, but not to ridiculous degree
	local new_asp = 0.5 * (`natu_aspect' + 1)
	local graphopts `"`graphopts' aspect(`new_asp')"'			// this will override any previous aspect() option
	local aspectRat = max(`new_asp'/`natu_aspect', `natu_aspect'/`new_asp')
}
else {
	local aspectRat = max(`aspect'/`natu_aspect', `natu_aspect'/`aspect')
}

local adj = 1.25
if `natu_aspect' > 0.7 local adj = 1/(`natu_aspect'^1.3+0.2)

local textSize = `adj' * `textsize' / (`approx_chars' * sqrt(`aspectRat') )

forvalues i = 1/`lcolsN' {
	local lcolCommands`i' `"(scatter `id' `left`i'', msymbol(none) mlabel(`leftLB`i'') mlabcolor(black) mlabpos(3) mlabsize(`textSize')) "'
}
forvalues i = 1/`rcolsN' {
	local rcolCommands`i' `"(scatter `id' `right`i'' if `use' != 4, msymbol(none) mlabel(`rightLB`i'') mlabcolor(black) mlabpos(3) mlabsize(`textSize')) "'
}	// DF: EDITED 30TH MAY 2013


// FAVOURS
if `"`favours'"' != `""' {

	* NEW JAN 2013 - allow multiple lines (cf twoway title option)
	gettoken leftfav rest : favours, parse("#") quotes
	if `"`leftfav'"'!=`"#"' {
		while `"`rest'"'!=`""' {
			gettoken next rest : rest, parse("#") quotes
			if `"`next'"'==`"#"' continue, break
			local leftfav `"`leftfav' `next'"'
		}
	}
	else local leftfav `""'
	local rightfav `"`rest'"'

	if `fp'>0 & `fp'<999 {					// 999 is the "default"
		local leftfp = -`fp'
		local rightfp = `fp'
	}
	else if "`h0'" != "" & "`nulloff'" == "" {
		local leftfp = `DXmin' + (`h0'-`DXmin')/2
		local rightfp = `h0' + (`DXmax'-`h0')/2
	}
	else {
		local leftfp = `DXmin'
		local rightfp = `DXmax'
	}
	local favopt `"xmlabel(`leftfp' `"`leftfav'"' `rightfp' `"`rightfav'"', noticks labels labsize(`textSize') labgap(5))"'
}
else if `"`xtitle'"' != `""' {
	local 0 `"`xtitle'"'
	syntax [anything] [, *]
	local xtitleopt `"xmlabel(`h0' `"`anything'"', noticks labels labsize(`textSize') labgap(5) `options')"'
}


// GRAPH APPEARANCE OPTIONS- ADDED v1.15
// DF August 2013 -- do this by groups of appearance options (`usingN')
// Check for conflicts and apply defaults where appropriate

local boxsize = `boxsca'/150

tempvar id_use1 id_use3 id_use5 id_use35		// in case of "weighted overall" options
qui gen `id_use1'=`id' if `use' == 1
qui gen `id_use3'=`id' if `use' == 3
qui gen `id_use5'=`id' if `use' == 5
qui gen `id_use35'=`id' if inlist(`use', 3, 5)

// quick bodge to get overall- can't find log version!
summ `id', meanonly
local DYmin = r(min)-2
local DYmax = r(max)+1

* Prepare overall lines
* Do these by groups formed by `use'==3 and/or `use'==5, NOT by `usingN'
* but get `olineopts' from the relevant `usingN' group to do the actual plotting.
qui count if `use' == 5
if r(N)>0 local useno = 5			// implies overall line, not subgroup line
else {
	qui count if `use' == 3
	if r(N)>0 local useno = 3		// implies subgroup line
}
if `"`useno'"'!=`""' {
	sort `id'
	tempvar olinegroup
	qui gen `olinegroup' = (`use'==`useno')
	qui replace `olinegroup' = sum(`olinegroup') if !inlist(`use', 4, 9)		// sep 2013 - check this is acceptable in all situations
	
	summ `olinegroup', meanonly
	local omax=r(max)
	tempvar tempOv ovLine ovMin ovMax h0Line
	qui gen `ovLine' = `ES' if `use'==`useno' & !(`ES' > `DXmax' | `ES' < `DXmin')
	qui gen `ovMin'=.
	qui gen `ovMax'=.
		
	summ `id' if `use'!=4, meanonly
	if `use'[`r(min)'] == `useno' & `omax'==1 {			// if only one "overall" obs and is at the bottom of the plot
		qui replace `ovMin' = `DYmin' if `use'==`useno' & !missing(`ovLine')
		qui replace `ovMax' = `borderline' if `use'==`useno' & !missing(`ovLine')
	}
	else {								// otherwise, "play it safe" and draw line(s) just across the olinegroup(s)
		forvalues i=1/`omax' {
			summ `id' if `olinegroup'==`i', meanonly
			qui replace `ovMin' = r(min)-1 if `use'==`useno' & `olinegroup'==`i' & !missing(`ovLine')
			qui replace `ovMax' = r(max) if `use'==`useno' & `olinegroup'==`i' & !missing(`ovLine')
		}
	}
}

*** Get options and store plot commands, by `usingN' group
local options							// clear macro
forvalues i=1/`usoptN' {
	local 0 `", `graphopts' `usopts`i''"'	// "global" graphopts come first so usopts can override (options are rightmost)
	syntax [, ///
		/// /* standard options */
		noBOX BOXOPT(string) DIAMOPT(string) DIAMOPT2(string) POINTOPT(string) CIOPT(string) OLINEOPT(string) ///
		/// /* non-diamond options
		noPBOX PBOXOPT(string) PPOINTOPT(string) PCIOPT(string) noPBOX2 PBOXOPT2(string) PPOINTOPT2(string) PCIOPT2(string) ///
		/// /* "style" options */
		CLASSIC INTERACTION INTERWGT OVWGT OVWGT2 *]
	
	local options2 `options'
	* if `"`interaction'"'!=`""' local interaction1 "interaction1"
	
	* Check for conflicts
	* CLASSIC = solid black boxes/circles
	* INTERACTION = circles instead of squares; small (unweighted) circle for pooled; cannot specify DIAMOND
	* INTERWGT = circles instead of squares; large (weighted) circle for pooled; cannot specify DIAMOND
	if `"`interaction'"'!=`""' & `"`interwgt'"'!=`""' {
		disp as err "cannot specify both interaction and interwgt options; please choose at most one"
		exit 198
	}
	* if `"`interaction'"'!=`""' local interaction1 "interaction1"								// interaction ==> "interaction1"
	* if (`"`interaction1'"'!=`""' | `"`interaction2'"'!=`""') local interaction `"interaction"'	// interaction1 or 2 ==> "interaction"
	
	* interwgt implies weighed overall scatter
	if `"`interwgt'"'!=`""' {
		local ovwgt `"ovwgt"'
		local ovwgt2 `"ovwgt2"'
	}
	
	*** "Study data" options
	* BOX OPTIONS
	local dispShape "square"
	if "`interaction'" != "" local dispShape "circle"
	
	if `"`classic'"'==`""' & `"`box'"'==`""' {
		local 0 `", `boxopt'"'
		syntax [, Msymbol(string asis) MColor(string asis) MSIZe(string asis) MLABEL(string asis) *]
		if `"`msymbol'"' == `""' local msymbol `"`dispShape'"'
		if `"`mcolor'"' == `""' local mcolor `""180 180 180""'
		if `"`mlabel'"' != `""' {
			disp as error "Option mlabel() not allowed in boxopt()"
			exit 198
		}
		if `"`msize'"' != `""' {
			disp as error "Option msize() not allowed in boxopt()"
			exit 198
		}
		local boxopt `"msymbol(`msymbol') mlcolor(`mcolor') mfcolor(`mcolor') msize(`boxsize') `options'"'
	}
	else {
		if `"`box'"'!=`""' local boxopt `"msymbol(none)"'
		else if `"`classic'"'!=`""' local boxopt `"mlcolor(black) mfcolor(black) msymbol(`dispShape') msize(`boxsize')"'
	}

	* POINT OPTIONS
	if `"`classic'"' != `""' & `"`box'"' == `""' {
		local pointopt "msymbol(none)"
	}
	else {
		local 0 `", `pointopt'"'
		syntax [, Msymbol(string asis) MColor(string asis) MSIZe(string asis) *]
		if `"`msymbol'"' == `""' local msymbol `"diamond"'
		if `"`mcolor'"' == `""' local mcolor `"black"'
		if `"`msize'"' == `""' local msize `"vsmall"'
		local pointopt `"msymbol(`msymbol') mcolor(`mcolor') msize(`msize') `options'"'
	}

	* CONFIDENCE INTERVAL (CI) OPTIONS
	local 0 `", `ciopt'"'
	syntax [, LColor(string asis) HORizontal VERTical Connect(string asis) Lpattern(string asis) MColor *]
	if `"`lcolor'"' == `""' local lcolor `"black"'
	local mcolorci `mcolor'
	if `"`mcolorci'"' == `""' local mcolorci `"black"'		// only used for off-scale arrows -- store in different macro name
	if `"`horizontal'"'!=`""' | `"`vertical'"'!=`""' {
		disp as error "Options horizontal/vertical not allowed in ciopt()"
		exit 198
	}			
	if `"`connect'"' != `""' {
		di as error "Option connect() not allowed in ciopt()"
		exit 198
	}
	* if `"`lpattern'"' != `""' {
	* 	di as error "Option lpattern() not allowed in ciopt()"		// DF SEP 2013 -- Why not??
	* 	exit 198
	* }
	local ciopt `"lcolor(`lcolor') `options'"'
	
	* OVERALL LINE OPTIONS
	local 0 `", `olineopt'"'
	syntax [, LWidth(string asis) LColor(string asis) Lpattern(string asis) *]
	if `"`lwidth'"' == `""' local lwidth `"thin"'
	if `"`lcolor'"' == `""' local lcolor `"maroon"'
	if `"`lpattern'"' == `""' local lpattern `"shortdash"'
	local olineopt `"lwidth(`lwidth') lcolor(`lcolor') lpattern(`lpattern') `options'"'
	
	* "Summary statistics" options -- may be called twice (once for subgroup, once for overall)
	forvalues j=1/2 {
		if `j'==1 local j		// instead of 1, just leave blank
		if `"`diamopt`j''"'!=`""' {
			if !(`"`pboxopt`j''"'==`""' & `"`ppointopt`j''"'==`""' & `"`pciopt`j''"'==`""' & `"`ovwgt`j''"'==`""') {
				disp as err "cannot specify options for both diamond pooled effect and box/point pooled effect"
				disp as err "please choose from at most one set of options"
				exit 198
			}
			if !(`"`interaction'"'==`""' & `"`interwgt'"'==`""') {
				disp as err `"cannot specify options for diamond pooled effect with "interaction" style options"'
				exit 198
			}
		}
		
		* Else, if no "pooled box/point/CI" options, and no "interaction" options, draw diamond
		else if `"`pboxopt`j''"'==`""' & `"`ppointopt`j''"'==`""' & `"`pciopt`j''"'==`""' & `"`ovwgt`j''"'==`""' ///
			& `"`interaction'"'==`""' {
		
			* DIAMOND OPTIONS (if appropriate)
			local 0 `", `diamopt`j''"'
			syntax [, LColor(string asis) HORizontal VERTical Connect(string asis) Lpattern(string asis) *]
			if `"`lcolor'"' == `""' local lcolor `""0 0 100""'
			if `"`horizontal'"'!=`""' | `"`vertical'"'!=`""' {
				disp as error "Options horizontal/vertical not allowed in diamopt()"
				exit 198
			}			
			if `"`connect'"' != `""' {
				di as error "Option connect() not allowed in diamopt()"
				exit 198
			}
			* if `"`lpattern'"' != `""' {
			* 	di as error "Option lpattern() not allowed in diamopt()"	// DF SEP 2013 -- Why not??
			* 	exit 198
			* }
			local diamopt`j' `"lcolor(`lcolor') `options'"'
		}
		
		* POOLED BOX (PBOX) OPTIONS (if appropriate)
		if `"`pbox`j''"'!=`""' local pboxopt `"msymbol(none)"'
		else if `"`diamopt`j''"' == `""' {
			local 0 `", `pboxopt`j''"'
			syntax [, Msymbol(string asis) MLColor(string asis) MFColor(string asis) MSIZe(string asis) MLABEL(string asis) *]
			if `"`msymbol'"' == `""' local msymbol `"`dispShape'"'
			if `"`mlcolor'"' == `""' local mlcolor `""0 0 100""'
			if `"`mfcolor'"' == `""' local mfcolor `"none"'
			if `"`mlabel'"' != `""' {
				disp as error "Option mlabel() not allowed in pboxopt()"
				exit 198
			}
			if `"`msize'"' != `""' {
				disp as error "Option msize() not allowed in pboxopt()"
				exit 198
			}
			local pboxopt`j' `"msymbol(`msymbol') mlcolor(`mlcolor') mfcolor(`mfcolor') msize(`boxsize') `options'"'
		}
		else if `"`pboxopt`j''"'!=`""' {
			disp as err "cannot specify options for both diamond pooled effect and box/point pooled effect"
			disp as err "please choose from at most one set of options"
			exit 198
		}
		
		* POOLED POINT (PPOINT) OPTIONS (if appropriate)
		if !(`"`interaction'"'==`""' & `"`interwgt'"'==`""') & `"`pbox`j''"' == `""' {
			local ppointopt`j' `"msymbol(none)"'	
		}
		else if `"`diamopt`j''"' == `""' {
			local 0 `", `ppointopt`j''"'
			syntax [, Msymbol(string asis) MColor(string asis) MSIZe(string asis) *]
			if `"`msymbol'"' == `""' local msymbol `"diamond"'
			if `"`mcolor'"' == `""' local mcolor `"black"'
			if `"`msize'"' == `""' local msize `"vsmall"'
			local ppointopt`j' `"msymbol(`msymbol') mcolor(`mcolor') msize(`msize') `options'"'
		}
		else if `"`ppointopt`j''"'!=`""' {
			disp as err "cannot specify options for both diamond pooled effect and box/point pooled effect"
			disp as err "please choose from at most one set of options"
			exit 198
		}
		
		* POOLED CONFIDENCE INTERVAL (PCI) OPTIONS (if appropriate)
		if `"`diamopt`j''"' == `""' {
			local 0 `", `pciopt`j''"'
			syntax [, LColor(string asis) HORizontal VERTical Connect(string asis) Lpattern(string asis) *]
			if `"`lcolor'"' == `""' local lcolor `""0 0 100""'
			if `"`horizontal'"'!=`""' | `"`vertical'"'!=`""' {
				disp as error "Options horizontal/vertical not allowed in pciopt()"
				exit 198
			}			
			if `"`connect'"' != `""' {
				di as error "Option connect() not allowed in pciopt()"
				exit 198
			}
			* if `"`lpattern'"' != `""' {
			* 	di as error "Option lpattern() not allowed in pciopt()"		// DF SEP 2013 -- Why not??
			* 	exit 198
			* }
			local pciopt`j' `"lcolor(`lcolor') `options'"'
		}
		else if `"`pciopt`j''"'!=`""' {
			disp as err "cannot specify options for both diamond pooled effect and box/point pooled effect"
			disp as err "please choose from at most one set of options"
			exit 198
		}
	}
	
	* MAIN SCATTER PLOT + DIAMOND or "NON-WEIGHTED" OVERALL SCATTER
	* tempvar toused
	* gen `toused' = (`usingN'==`i')*((`use'==3)*(`"`diamopt'"'!=`""') + (`use'==5)*(`"`diamopt2'"'!=`""'))
	
	if `i'>1 local closebracket ")"
	if `"`ovwgt'"'==`""' & `"`ovwgt2'"'==`""' {

		local scatter1 `"`scatter1' `closebracket' (scatter `id_use1' `ES' `awweight' if `usingN'==`i', `boxopt'"'
		
		* if no "secondary" options (or options are the same)
		if `"`diamopt'"'!=`""' & `"`diamopt'"'==`"`diamopt2'"' {		// diamond
			local diamond1 `"`diamond1' (pcspike `DIAMleftY1' `DIAMleftX' `DIAMtopY' `DIAMtopX' if `touse2' & `usingN'==`i', `diamopt') "'
			local diamond2 `"`diamond2' (pcspike `DIAMtopY' `DIAMtopX' `DIAMrightY1' `DIAMrightX' if `touse2' & `usingN'==`i', `diamopt') "'
			local diamond3 `"`diamond3' (pcspike `DIAMrightY2' `DIAMrightX' `DIAMbottomY' `DIAMbottomX' if `touse2' & `usingN'==`i', `diamopt') "'
			local diamond4 `"`diamond4' (pcspike `DIAMbottomY' `DIAMbottomX' `DIAMleftY2' `DIAMleftX' if `touse2' & `usingN'==`i', `diamopt') "'
		}		
		else if `"`pboxopt'"'!=`""' & `"`pboxopt'"'==`"`pboxopt2'"' {	// non-diamond ("non-weighted")
			local scatter2 `"`scatter2' (scatter `id' `ES' if `touse2' & `usingN'==`i', `pboxopt') "'
			if `"`pciopt'"'!=`""' & `"`pciopt'"'==`"`pciopt2'"' {
				local pciplot `"`pciplot' (rspike `lci' `uci' `id' if `touse2' & `usingN'==`i', hor `pciopt') "'
			}
			if `"`ppointopt'"'!=`""' & `"`ppointopt'"'==`"`ppointopt2'"' {
				local ppointplot `"`ppointplot' (scatter `id' `ES' if `touse2' & `usingN'==`i', `ppointopt') "'
			}
		}	
		
		* if (different) "secondary" options specified
		else {
			if `"`diamopt'"'!=`""' {
				local diamond1 `"`diamond1' (pcspike `DIAMleftY1' `DIAMleftX' `DIAMtopY' `DIAMtopX' if `use'==3 & `usingN'==`i', `diamopt') "'
				local diamond2 `"`diamond2' (pcspike `DIAMtopY' `DIAMtopX' `DIAMrightY1' `DIAMrightX' if `use'==3 & `usingN'==`i', `diamopt') "'
				local diamond3 `"`diamond3' (pcspike `DIAMrightY2' `DIAMrightX' `DIAMbottomY' `DIAMbottomX' if `use'==3 & `usingN'==`i', `diamopt') "'
				local diamond4 `"`diamond4' (pcspike `DIAMbottomY' `DIAMbottomX' `DIAMleftY2' `DIAMleftX' if `use'==3 & `usingN'==`i', `diamopt') "'
			}
			if `"`diamopt2'"'!=`""' {
				local diamond1 `"`diamond1' (pcspike `DIAMleftY1' `DIAMleftX' `DIAMtopY' `DIAMtopX' if `use'==5 & `usingN'==`i', `diamopt2') "'
				local diamond2 `"`diamond2' (pcspike `DIAMtopY' `DIAMtopX' `DIAMrightY1' `DIAMrightX' if `use'==5 & `usingN'==`i', `diamopt2') "'
				local diamond3 `"`diamond3' (pcspike `DIAMrightY2' `DIAMrightX' `DIAMbottomY' `DIAMbottomX' if `use'==5 & `usingN'==`i', `diamopt2') "'
				local diamond4 `"`diamond4' (pcspike `DIAMbottomY' `DIAMbottomX' `DIAMleftY2' `DIAMleftX' if `use'==5 & `usingN'==`i', `diamopt2') "'
			}
			if `"`pboxopt'"'!=`""' local scatter2 `"`scatter2' (scatter `id_use3' `ES' if `usingN'==`i', `pboxopt') "'
			if `"`pboxopt2'"'!=`""' local scatter2 `"`scatter2' (scatter `id_use5' `ES' if `usingN'==`i', `pboxopt2') "'
			if `"`pciopt'"'!=`""' local pciplot `"`pciplot' (rspike `lci' `uci' `id_use3' if `usingN'==`i', hor `pciopt') "'
			if `"`pciopt2'"'!=`""' local pciplot `"`pciplot' (rspike `lci' `uci' `id_use5' if `usingN'==`i', hor `pciopt2') "'
			if `"`ppointopt'"'!=`""' local ppointplot `"`ppointplot' (scatter `id_use3' `ES' if `usingN'==`i', `ppointopt') "'
			if `"`ppointopt2'"'!=`""' local ppointplot `"`ppointplot' (scatter `id_use5' `ES' if `usingN'==`i', `ppointopt2') "'
		}
	}
	
	* MAIN SCATTER PLOT + "WEIGHTED" OVERALL SCATTER
	else {

		// don't need 2nd option if same as first
		if `"`pboxopt'"'!=`""' & `"`pboxopt'"'==`"`pboxopt2'"' local pboxopt2

		* Get similar options from "boxopt", "pboxopt" and "pboxopt2"; place them side-by-side in new option "allboxopt"
		* to pass to "scatter y1 y2 y3 x" -type command
		local rest `"`boxopt' `pboxopt' `pboxopt2'"'
		while `"`rest'"'!=`""' {
			gettoken token rest : rest, match(paren) parse(" (")
			if `"`paren'"'==`""' & `"`token'"'!=`""' {			// this should be an option name, e.g. mlcolor
				local is_in : list token in alloptlist
				if !`is_in' {
					local alloptlist `"`alloptlist' `token'"'
					local alloptlist2 `"`alloptlist2' `token'(string asis)"'
				}
			}
		}
		* Now use "syntax" to extract the options themselves and form "bothopt"
		foreach x of local alloptlist {
			local 0 `", `boxopt'"'
			syntax , [`alloptlist2']
			local `x'box `"``x''"'
		
			local 0 `", `pboxopt'"'
			syntax , [`alloptlist2']
			local `x'pbox `"``x''"'

			local 0 `", `pboxopt2'"'
			syntax , [`alloptlist2']
			local `x'pbox2 `"``x''"'
			
			local allboxopt `"`allboxopt' `x'(``x'box' ``x'pbox' ``x'pbox2')"'
		}
		
		if `"`pboxopt2'"'!=`""' {		// different options for subgroup and overall
			local scatter1 `"`scatter1' `closebracket' (scatter `id_use1' `id_use3' `id_use5' `ES' `awweight' if `usingN'==`i', `allboxopt'"'
		}
		else {							// same options for subgroup and overall
			local scatter1 `"`scatter1' `closebracket' (scatter `id_use1' `id_use35' `ES' `awweight' if `usingN'==`i', `allboxopt'"'
		}
		
		* Other options: pooled CI
		if `"`pciopt'"'!=`""' & `"`pciopt'"'==`"`pciopt2'"' local pciopt2
		if `"`pciopt2'"'!=`""' {		// different options for subgroup and overall
			local pciplot `"`pciplot' (rspike `lci' `uci' `id_use3' if `usingN'==`i', hor `pciopt') "'
			local pciplot `"`pciplot' (rspike `lci' `uci' `id_use5' if `usingN'==`i', hor `pciopt2') "'
		}
		else {							// same options for subgroup and overall
			local pciplot `"`pciplot' (rspike `lci' `uci' `id_use35' if `usingN'==`i', hor `pciopt') "'
		}		
		* Other options: pooled points
		if `"`ppointopt'"'!=`""' & `"`ppointopt'"'==`"`ppointopt2'"' local ppointopt2
		if `"`ppointopt2'"'!=`""' {		// different options for subgroup and overall
			local ppointplot `"`ppointplot' (scatter `id_use3' `ES' if `usingN'==`i', `ppointopt') "'
			local ppointplot `"`ppointplot' (scatter `id_use5' `ES' if `usingN'==`i', `ppointopt2') "'
		}
		else {							// same options for subgroup and overall
			local ppointplot `"`ppointplot' (scatter `id_use35' `ES' if `usingN'==`i', `ppointopt') "'
		}	
	}
	
	* CONFIDENCE INTERVALS
	local ciplot `"`ciplot' (rspike `lci' `uci' `id_use1' if `usingN'==`i' & !`offscaleL' & !`offscaleR', hor `ciopt') "'
	
	// use macro name "ciplot2" for off-scale plots to avoid main macro "ciplot" getting too long
	qui count if `usingN'==`i' & `offscaleL' & `offscaleR'
	if r(N)>0 {													// both ends off scale
		local ciplot2 `"`ciplot2' (pcbarrow `id_use1' `lci' `id_use1' `uci' if `usingN'==`i' & `offscaleL' & `offscaleR', mcolor(`mcolorci') `ciopt') "'
	}
	qui count if `usingN'==`i' & `offscaleL' & !`offscaleR'
	if r(N)>0 {													// only left off scale
		local ciplot2 `"`ciplot2' (pcarrow `id_use1' `uci' `id_use1' `lci' if `usingN'==`i' & `offscaleL' & !`offscaleR', mcolor(`mcolorci') `ciopt') "'
	}
	qui count if `usingN'==`i' & !`offscaleL' & `offscaleR'
	if r(N)>0 {													// only right off scale
		local ciplot2 `"`ciplot2' (pcarrow `id_use1' `lci' `id_use1' `uci' if `usingN'==`i' & !`offscaleL' & `offscaleR', mcolor(`mcolorci') `ciopt') "'
	}

	* POINT PLOT
	local pointplot `"`pointplot' (scatter `id_use1' `ES' if `usingN'==`i', `pointopt') "'
	
	* OVERALL LINE(S) (if appropriate)
	summ `ovLine' if `usingN'==`i', meanonly
	if r(N)>0 {
		local overallCommand `"`overallCommand' (rspike `ovMin' `ovMax' `ovLine' if `usingN'==`i', `olineopt') "'
	}
}

local graphopts `options2'		// this is now *just* the standard "twoway" options
								// i.e. the specialist "forestplot" options have been filtered out
	
	
// END GRAPH OPTS

* Note for random-effects analyses
if inlist("`type'", "dl", "vb", "bs") {
	if "`type'"=="dl" local typefull "DerSimonian-Laird"
	else if "`type'"=="vb" local typefull "Generalised Q"
	else if "`type'"=="bs" local typefull "Approx. Gamma"
	summ `id', meanonly
	local noteposy = r(min) -1.5 		// ypos for note is 1.5 lines below last obs
	summ `left1', meanonly
	local noteposx = r(mean) 			// xpos is middle of left-hand-side (but text will be left-justified in scatter)
	local notelab `"NOTE: Weights are from `typefull' analysis"'
	local notecmd `"text(`noteposy' `noteposx' "`notelab'", placement(3) size(`textSize')) "'
}

// DF: modified to use added line approach instead of pcspike (less complex & poss. more efficient as fewer vars)
// null line (unless switched off)
if "`nulloff'" == "" {
	local nullCommand `" (function y=`h0', range(`DYmin' `borderline') horiz n(2) lwidth(thin) lcolor(black)) "'
}

// final addition- if aspect() given but not xsize() ysize(), put these in to get rid of gaps
// need to fiddle to allow space for bottom title
// should this just replace the aspect option?
// suppose good to keep- most people hopefully using xsize and ysize and can always change themselves if using aspect
if `xsize' == 0 & `ysize' == 0 & `aspect' > 0 {
	if `aspect' > 1 {
		local xx = (11.5 + 2*(1-1/`aspect'))/`aspect'
		local yy = 12
	}
	else {
		local yy = 12*`aspect'
		local xx = 11.5 - 2*(1 - `aspect')
	}
	local graphopts `"`graphopts' xsize(`xx') ysize(`yy')"'		// these will override any previous xsize/ysize options
}



***************************
***     DRAW GRAPH      ***
***************************

#delimit ;

twoway
/* OVERALL AND NULL LINES FIRST */ 
	`overallCommand' `nullCommand'
/* PLOT BOXES AND PUT ALL THE GRAPH OPTIONS IN THERE, PLUS NOTE FOR RANDOM-EFFECTS */ 
	`scatter1' `notecmd'
		yscale(range(`DYmin' `DYmax') noline) ylabel(none) ytitle("")
		xscale(range(`AXmin' `AXmax')) xlabel(`xlblcmd', labsize(`textSize'))
		yline(`borderline', lwidth(thin) lcolor(gs12))
/* FAVOURS OR XTITLE */
		`favopt' `xtitleopt'
/* PUT LABELS UNDER xticks? Yes as labels now extended */
		xtitle("") legend(off) xtick("`xticklist'") )
/* END OF FIRST SCATTER (i.e. that opened within `scatter1' or `circle2' */
/* NEXT, CONFIDENCE INTERVALS (plus offscale if necessary) */
	`ciplot' `ciplot2'
/* DIAMONDS (OR BOXES) FOR SUMMARY ESTIMATES (if appropriate) -START FROM 9 O'CLOCK */
	`diamond1' `diamond2' `diamond3' `diamond4' `scatter2'
/* COLUMN VARIBLES (including effect sizes and weights on RHS by default) */
	`lcolCommands1' `lcolCommands2' `lcolCommands3' `lcolCommands4' `lcolCommands5' `lcolCommands6'
	`lcolCommands7' `lcolCommands8' `lcolCommands9' `lcolCommands10' `lcolCommands11' `lcolCommands12'
	`rcolCommands1' `rcolCommands2' `rcolCommands3' `rcolCommands4' `rcolCommands5' `rcolCommands6'
	`rcolCommands7' `rcolCommands8' `rcolCommands9' `rcolCommands10' `rcolCommands11' `rcolCommands12'
/* LAST OF ALL PLOT EFFECT MARKERS TO CLARIFY AND OVERALL EFFECT LINE */
	`pointplot' `ppointplot'
	, `graphopts' /* RMH added */ plotregion(margin(zero));

#delimit cr

end





program define getWidth
version 9.0

//	ROSS HARRIS, 13TH JULY 2006
//	TEXT SIZES VARY DEPENDING ON CHARACTER
//	THIS PROGRAM GENERATES APPROXIMATE DISPLAY WIDTH OF A STRING
//	FIRST ARG IS STRING TO MEASURE, SECOND THE NEW VARIABLE

//	PREVIOUS CODE DROPPED COMPLETELY AND REPLACED WITH SUGGESTION
//	FROM Jeff Pitblado

qui{

gen `2' = 0
count
local N = r(N)
forvalues i = 1/`N'{
	local this = `1'[`i']
	local width: _length `"`this'"'
	replace `2' =  `width' +1 in `i'
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

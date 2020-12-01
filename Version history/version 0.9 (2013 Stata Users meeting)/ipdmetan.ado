* Meta-analysis of main effects or interactions
*! version 0.9  11sep2013  David Fisher

* Originally written July 2008 by David Fisher

* Major updates:
* November 2011/February 2012
*   Screen output coded within -ipdmetan- rather than using -metan- external command
*    to enable specific results to be presented

* September 2012
*   Aggregate data and IPD able to be pooled in same meta-analysis

* November 2012
*   Forest-plot output coded within -ipdmetan- using code modified from metan v9 (SJ9-2: sbe24_3)
*   Acknowledgements and thanks to the authors of this code.

* November 2012
*   "over()" functionality added

* January 2013
*   Changed to "prefix" -style syntax, following discussion with Patrick Royston

* March 2013
*   Functionality of -ipdmetan- and -ipdover- completely separated.
*   -ipdmetan- now ONLY does pooled meta-analysis
*   Anything else, e.g. non trial-level subgroups, over(), general forest plots etc., must be done via -ipdover-
*    and will not use pooling, inverse-variance weights, etc.
*   (although I-V weights still used in forest plots as a visual aid)

* June 2013
*   After discussion with Ross Harris, improved ability to analyse aggregate data alone using separate program -aggmetan-
*   together with some rearrangement of syntax, options & naming conventions


program ipdmetan, rclass sortpreserve

	version 11
	* NOTE: mata requires v9.x
	* factor variable syntax requires 11.0

	// <ipdmetan_stuff> : <command>
	_on_colon_parse `0'

	* Parse estimation command (after colon)
	local 0 `"`s(after)'"'
	syntax [anything(name=command)] [if] [in] [, *]
	local aggif `"`if'"'
	local aggin `"`in'"'			// save these in case needed later for analysis of aggregate data
	marksample touse				// applies to data in memory (but may not be used)
	if `"`options'"'!=`""' local modelopts `", `options'"'	
	
	* Parse ipdmetan options (before colon)
	local 0 `"`s(before)'"'
	syntax [anything(name=exp_list equalok)] , [ ///	
		/// IPD options
		STUDY(name local)			///
		INTeraction					///
		MESSages					///
		KEEPALL						///
		POOLvar(varname)			/// simpler alternative to exp_list
		ZTOL(real 1e-6)				/// 
		OVER(varlist min=2) IPDOVER	/// options passed through from ipdover (see ipdover.ado & help file)
		/// Aggregate options
		AGGONLY						///
		AGGregate(string)			///		
		NPTS(name local) BYAGG VARS(namelist local)	/// for error capture only -- should only appear within "aggregate" option
		/// General options
		FORESTplot(string asis)		///
		noGRAPH						/// can be specified either directly to ipdmetan or as a forestplot() option
		noOVERALL noSUBGroup		/// can be specified either directly to ipdmetan or as a forestplot() option
		noTABLE	noHET noTRUNC		///
		BY(name local)				/// "name" since could be missing in current memory (byagg)
		SAVING(string)				///
		EFFect(string)				///
		EFORM						///
		LCOLS(string asis) RCOLS(string asis)	/// lcols/rcols apply to both ipdmetan coeff matrix or to forestplot
		*							///
	]

	
	* Parse "exp_list"						// ADDED BACK IN AUG 2013 - TEST
	/*
	if `"`exp_list'"'!=`""' {
		_prefix_explist `exp_list', stub(_stat_)
		assert inlist(`s(k_exp)', 2, 3)
		local exp_list = `"`s(explist)'"'
	}
	*/
	
	* Parse command
	* _prefix_command ...   TO DO
	
	
	** Parse random-effects (more than one permitted syntax)
	* (N.B. Stata options are RIGHTMOST)
	if `"`options'"'!=`""' {
		local 0 `", `options'"'
		syntax [, RE RE(string) RANDOM RANDOM(string)]
		if `"`re'"'!=`""' & `"`random'"'!=`""' {
			disp as err "Cannot specify both re and random; please choose just one"
			exit 198
		}
		if `"`re'"'!=`""' local random `re'
		else {
			syntax [, RE(string) RE RANDOM(string) RANDOM]
			if `"`re'"'!=`""' & `"`random'"'!=`""' {
				disp as err "Cannot specify both re and random; please choose just one"
				exit 198
			}
			if `"`re'"'!=`""' local random `re'
		}
		if `"`random'"'!=`""' & `"`trunc'"'!=`""' {
			disp as err "Cannot specify both random-effects and notrunc; please choose just one"
			exit 198
		}
	}
	
	* Parse forest plot options to extract those relevant to ipdmetan
	* N.B. don't need to "put them back" as appropriate dataset will be created by ipdmetan, forestplot doesn't need to know
	foreach x in graph overall subgroup {
		local `x'2 ``x''
	}
	local 0 `", `forestplot'"'
	syntax [, noGRAPH noNAME noOVERALL noSUBGroup OVSTAT(string) *]
	if `"`het'"'!=`""' local ovstat "none"
	local forestplot `"`options'"'
	foreach x in graph overall subgroup {
		if `"``x''"'==`""' local `x' ``x'2'
	}


	*********
	* Setup *
	*********
	
	** Option compatibility tests
	if `"`command'"'!=`""' {
		if `"`study'"'==`""' & `"`ipdover'"'==`""' & `"`aggonly'"'==`""' {
			disp as err `"Must specify option study() for IPD analysis"'
			exit 198
		}
		if `"`exp_list'"'!=`""' & `"`interaction'"'!=`""' {
			disp as err `"Cannot specify both exp_list and interaction"'
			exit 198
		}
		if `"`by'"'!=`""' {
			capture confirm var `by'
			if _rc & !(`"`aggregate'"'!=`""' & `"`aggonly'"'==`""') {	// `by' may only NOT exist in memory
				disp as err "variable `by' not found in option by()"	// if an external aggregate-data file is specified.
				exit 111												// (and even then, it must exist there! - tested for later)
			}
		}
	}
	local type `"fe"'
	
	** General presentation/forest-plot, no pooling (ipdover)
	if `"`ipdover'"'!=`""'  {
		if `"`command'"'==`""' {
			disp as error `"Must supply an estimation command to ipdover"'
			exit 198
		}
		foreach x in study aggregate random {
			if `"``x''"'!=`""' {
				disp as error `"option `x' not allowed with ipdover"'
				exit 198
			}
		}
		local pooltext "Trial subgroup analysis"
		local study `"`over'"'
		local het "nohet"		// cannot have heterogeneity stats with ipdover
	}
	
	** Pooled meta-analysis (IPD, aggregate or both)
	else {	
		if `"`over'"'!=`""' {
			disp as err `"Cannot specify over() with ipdmetan; please use ipdover instead"'
			exit 198
		}
	
		if `"`command'"'==`""' {
			* Implies aggregate data only; so cannot use any of the IPD options (and "aggregate" option must be present)
			if `"`aggregate'"'==`""' {
				if `"`aggonly'"'==`""' disp as err `"IPD estimation command not found"'		// i.e. assume IPD analysis is intended
				else disp as err `"For analysis of aggregate data alone, please use the 'aggmetan' command"'
				exit 198
			}
			local aggonly "aggonly"		// make sure aggonly is specified if no IPD
			
			if `"`exp_list'"'!=`""' {
				disp as err `"exp_list not allowed with aggregate data alone"'
				exit 198
			}
			if `"`interaction'"'!=`""' {
				disp as err `"Option interaction not allowed with aggregate data alone"'
				exit 198
			}
		}
	
		* Parse aggregate() option
		foreach x in npts byagg vars {		// these options should not appear outside of the aggregate() option
			if `"``x''"'!=`""' {
				disp as err `"Suboption `x' can only be supplied to the aggregate() option"'
				exit 198
			}
		}
		if `"`aggonly'"'!=`""' & `"`aggregate'"'==`""' {
			disp as err `"For analysis of aggregate data alone, please use the 'aggmetan' command"'
			exit 198
		}
		if `"`aggregate'"'!=`""' {
			local 0 `"`aggregate'"'
			syntax [anything(name=aggfile id="filename")] [if] [in], [BYAGG NPTS(name local) VARS(namelist local)]
			
			* check valid "using" filename, if supplied
			if `"`aggfile'"'!=`""' {
				confirm file `"`aggfile'"'
				local aggif `"`if'"'
				local aggin `"`in'"'
				
				* If filename supplied, must also have IPD command (agg data alone should use -aggmetan-)
				if `"`command'"'==`""' {
					disp as err `"Cannot specify an aggregate-data filename without IPD estimation command"'
					disp as err `"For analysis of aggregate data alone, please use the 'aggmetan' command"'
					exit 198
				}
			}
			else {
				foreach x in if in {		// test for presence of "if"/"in" in absence of external file
					if `"``x''"'!=`""' {
						disp as err `"'`x'' cannot be supplied to the aggregate option without a filename"'
						local `x'
					}
				}
				* if filename not supplied, assume using data in memory, therefore no IPD
				if `"`command'"'!=`""' {
					disp as err `"aggregate data assumed to be in memory, therefore cannot have IPD data too"'
					disp as err `"Either supply a filename to the aggregate() option, or remove IPD command"'
					exit 198
				}
			}
			
			* byagg conflicts
			if `"`byagg'"'!=`""' {
				if `"`by'"'!=`""' {				// byagg + by
					disp as err `"Cannot specify both byagg and by() options.  Option byagg will be ignored"'
					local byagg
				}
				if `"`command'"'==`""' {		// byagg + no IPD
					disp as err `"Option byagg specified but no IPD command found.  Option byagg will be ignored"'
					local byagg
				}
			}
			
			* Sort out options
			if `"`vars'"'==`""' {
				if `"`npts'"'!=`""' {
					disp as err `"'npts' cannot be supplied to aggregate option without vars()"'
					exit 198
				}
				local vars "_ES _seES"		// assume standard names if vars() not supplied
				local npts "_NN"
			}
		}

		else if `"`command'"'==`""' {								// If no aggregate data, IPD data must exist
			disp as err `"IPD estimation command not found"'		// i.e. assume IPD analysis is intended
			exit 198
		}
		
		* Random-effects
		if `"`random'"'!=`""' {
			local 0 `"`random'"'
			syntax [anything(name=type id=type)] [, noTRUNC]
			if inlist("`type'", "", "r", "random", "rand", "re", "dl") local type "dl"	// DerSimonian & Laird random-effects (default)
			else if inlist("`type'", "q", "genq", "vb") local type "vb"					// Viechtbauer random-effects (generalised Q profiling)
			else if inlist("`type'", "g", "gamma", "bt", "bs") local type "bs"			// Biggerstaff & Tweedie random-effects (approx Gamma)
			else if !inlist("`type'", "f", "fe", "fixed") {
				disp as err "Invalid random-effects type"
				exit 198
			}
		}
		if inlist(`"`type'"', "vb", "bs") {
			capture mata mata which mm_root()
			if _rc {
				disp as err "Iterative tau-squared calculations require mm_root from -moremata-"
				disp as err "Type -ssc install moremata- to obtain it"
				exit 499
			}
		}
		
		local pooltext "Meta-analysis pooling"

	}		// end of if `"`ipdover'"'==`""'

	local overlen: word count `study'		// Equals 1 unless over() is specified
	

	** Analysis of data *in memory* (whether IPD or aggregate)
	confirm var `study'
	tempvar obs
	qui gen long `obs'=_n
		
	* Sort out subgroup identifier (BY) and labels
	local by2
	local nby 1
	local byisstr=0
	if `"`by'"'!=`""' {					// `by' might only exist in an external (aggregate) dataset (see line 149)
		capture confirm var `by'		// if so, do not define by2 yet, to avoid conflicts
		if !_rc {
			markout `touse' `by', strok		// ignore observations with missing "by"

			* Create new subgroup ID `bygroup' based on "natural ordering"
			tempvar bygroup
			qui bysort `touse' `by' : gen int `bygroup' = (_n==1)*`touse'
			qui replace `bygroup' = sum(`bygroup')
			local nby = `bygroup'[_N]				// number of subgroups
					
			* Transfer labels, values or strings to new subgroup ID
			capture confirm numeric variable `by'
			local byisstr = (_rc!=0)

			tempname bylab
			if !`byisstr' {				// numeric
				forvalues i=1/`nby' {
					summ `by' if `bygroup'== `i' & `touse', meanonly
					local bylabi : label (`by') `r(min)'		// will return `r(min)' if no label
					local bylist `"`bylist' `r(min)'"'			// create list of original subgroup IDs
					label define `bylab' `r(min)' `"`bylabi'"', add
				}
				local by2 `by'			// `by2' stores `by' only if it exists in current memory
			}
			else {						// string
				sort `obs'
				forvalues i=1/`nby' {
					summ `obs' if `bygroup'== `i', meanonly
					local bylabi = `by'[`r(min)']
					label define `bylab' `i' `"`bylabi'"', add
				}
				numlist "1/`nby'"
				local bylist=r(numlist)
				local by2 `bygroup'
			}
		}
	}
	* Sort out byagg
	else if `"`byagg'"'!=`""' {
		tempvar byaggvar			// tempvar for agg_prog
	}								// again, do not define by2 yet
	
	else if `"`subgroup'"'!=`""' {
		disp as error "Cannot specify option nosubgroup without option by() (or byagg)"
		local subgroup
	}


	* Sort out study ID (or 'over' vars)
	* If IPD (i.e. not ipdover), create subgroup ID based on order of first occurrence
	* (overh will be a single var, =study, so keep sgroup for later)
	local study2 `"`study'"'				// create copy in case of string variable
	tempvar sgroup sobs
	local matrows=0
	foreach overh of local study {
		capture drop `sobs' `sgroup'
		qui bysort `touse' `overh' (`obs') : gen long `sobs' = `obs'[1]
		qui bysort `touse' `by2' `sobs' : gen long `sgroup' = (_n==1 & `touse' & !missing(`overh'))
		qui replace `sgroup' = sum(`sgroup')
		local ns = `sgroup'[_N]					// number of levels of overh (e.g. number of studies/trials)
		local matrows=`matrows'+`ns'			// this adds up to give the size of the coeff matrix
		
		* If any string variables, "decode" them
		* and replace string var with numeric var in list "study"
		capture confirm string variable `overh'
		if !_rc {
			tempvar `overh'num
			tempname ``overh'num'lab
			encode `overh' if `touse', gen(``overh'num') label(```overh'num'lab')
			local study2 : subinstr local study2 `"`overh'"' `"``overh'num'"', all word
		}
		else {
			local `overh'lab : value label `overh'
			if `"``overh'lab'"'==`""' {					// if no value label, create a dummy one containing raw value
				tempname `overh'lab
				qui levelsof `overh' if `touse', local(ovlist)
				foreach overi of local ovlist {
					label define ``overh'lab' `overi' `"`overi'"', add
				}				
			}
		}
		
		* Store variable labels
		local `overh'varlab : variable label `overh'
		if `"``overh'varlab'"'==`""' local `overh'varlab `"`overh'"'

		* If not ipdover, test to see if subgroup variable varies within studies/trials
		* If it does, exit with error
		if `"`command'"'!=`""' & "`ipdover'"==`""' {
			qui tab `overh' if `touse'
			if `r(r)' != `ns' {					// N.B. `ns' is already stratified by `by'
				disp as err _n "Data is not suitable for meta-analysis" _c
				disp as err " as subgroup variable (in option 'by') is not constant within trials."
				disp as err "Use alternative command 'ipdover' if appropriate." _n
				exit 198
			}
		}
	}
	local study `"`study2'"'


	** Set up lcols and rcols if appropriate (either for a saved dataset or a forestplot)
	* N.B. This section does not apply if ONLY aggregate data exists;
	*      instead, lcols/rcols should be sent straight through to forestplot
	foreach x in na nc ncs nr ni {
		local `x'=0
	}
	if (`"`saving'"'!=`""' | `"`graph'"'==`""') & (`"`lcols'"'!=`""' | `"`rcols'"'!=`""') {

		local rcolsy = (`"`lcols'"'==`""')							// marker of "start with rcols = yes" (i.e. if no lcols)
		parsecols `lcols' : `rcols', rcols(`rcolsy') `byagg'		// option rcols() = "are we currently parsing rcols?"
																	// if no lcols then rcols=1 from the start
																	// otherwise gets changed when appropriate within parsecols.
		local lcols
		local rcols
		local ni = `r(na)' + `r(nc)' + `r(ncs)' + `r(nr)'		// total number of "items"
		local sidelist `"`r(sidelist)'"'
		local lrcols `"`r(newnames)'"'
		forvalues i=1/`ni' {
			if "`: word `i' of `sidelist''"=="0" {
				local lcols `"`lcols' `: word `i' of `lrcols''"'		// form new `lcols'
			}
			else local rcols `"`rcols' `: word `i' of `lrcols''"' 		// ...and `rcols', just containing new varnames
		}
		local itypes=trim(itrim(`"`itypes' `r(itypes)'"'))			// "item types"
		local fmts=trim(itrim(`"`fmts' `r(fmts)'"'))				// formats
		local cclist=trim(itrim(`"`cclist' `r(cclist)'"'))			// clist of expressions for -collapse-
		local statsr=trim(itrim(`"`statsr' `r(rstatlist)'"'))		// list of "as-is" returned stats		
		
		* Store variable labels
		* (numbered sequentially according to their itype)
		foreach y in a c cs r {
			if r(n`y')>0 {		
				forvalues i=1/`=r(n`y')' {
					local n`y'lab`i' `"`r(`y'varlab`i')'"'
				}
			}
		}
	
		* Separate lists of returned stats, "collapse" names, and string vars to "collapse"
		forvalues i=1/`ni' {
			if "`: word `i' of `itypes''"=="a" {
				local ++na
				local aggvars `"`aggvars' `: word `i' of `lrcols''"'
			}
			else if "`: word `i' of `itypes''"=="c" {
				local ++nc
				local namesc `"`namesc' `: word `i' of `lrcols''"'
			}
			else if "`: word `i' of `itypes''"=="cs" {
				local ++ncs
				local svars `"`svars' `: word `i' of `lrcols''"'
			}
			else if "`: word `i' of `itypes''"=="r" {
				local ++nr
				local namesr `"`namesr' `: word `i' of `lrcols''"'
			}
		}

		* Perform -collapse-
		if `"`command'"'!=`""' {
			if `"`cclist'"'!=`""' {
				forvalues h=1/`overlen' {
					local overh : word `h' of `study'
					preserve
						tempfile extra1_`h'
						qui collapse `cclist' if `touse', fast by(`by2' `overh')		// study/over-level
						qui save `extra1_`h''
					restore
				}
				if `"`by2'"'!=`""' & "`subgroup'"==`""' {
					preserve
						tempfile extra1_by
						qui collapse `cclist' if `touse', fast by(`by2') 			// by-level
						qui save `extra1_by'
					restore
				}
				if `"`overall'"'==`""' | (`"`byagg'"'!=`""' & "`subgroup'"==`""') {
					preserve
						tempfile extra1_tot
						qui collapse `cclist' if `touse', fast 					// overall
						qui save `extra1_tot'									// (or "subgroup" level for byagg)
					restore
				}
			}
		
			* Perform manual "collapse" of any string vars in "over" files
			* This could take a bit of to-ing and fro-ing, but it's a niche case
			if `"`svars'"'!=`""' & `overlen'>1 {
				forvalues h=2/`overlen' {
					preserve
						tempvar keepby
						local overh : word `h' of `study'
						qui bysort `overh': keep if _n==_N
						keep `by2' `overh' `svars'
						if `"`cclist'"'!=`""' {					// file already created above
							qui merge 1:1 `by2' `overh' using `extra1_`h'', nogen
							save, replace
						}	
						else {									// file not yet created
							tempfile extra1_`h'
							qui save `extra1_`h''
						}
					restore
				}
			}
	
			** Append files to form a single "extra" file
			if `"`cclist'"'!=`""' | `"`svars'"'!=`""' {	
				preserve
					if `"`svars'"'!=`""' {
						local overh : word 1 of `study'
						qui bysort `overh': keep if _n==_N
						keep `by2' `overh' `svars'
						if `"`cclist'"'!=`""' {					// file already created above
							qui merge 1:1 `by2' `overh' using `extra1_1', nogen
						}
					}
					else use `extra1_1', clear

					if `overlen'>1 {				// if "over", append files
						qui gen _over=1
						forvalues h=2/`overlen' {
							local prevoverh : word `=`h'-1' of `study'
							local overh : word `h' of `study'
							rename `prevoverh' `overh'				// rename study var to match with next dataset
							qui append using `extra1_`h''
							qui replace _over=`h' if missing(_over)
						}
					}
					if `"`cclist'"'!=`""' {			// 'by' and 'overall' sections don't apply if only svars
						if `"`by2'"'!=`""' & "`subgroup'"==`""' {
							qui append using `extra1_by'
						}
						if `"`overall'"'==`""' | (`"`byagg'"'!=`""' & "`subgroup'"==`""') {
							qui append using `extra1_tot'
						}
					}
					/*if `overlen'>1*/ rename `overh' _study		// if "over", rename to "_study"
					
					* Apply variable labels to "collapse" vars
					forvalues i=1/`nc' {
						local temp : word `i' of `namesc'
						label var `temp' `"`nclab`i''"'
					}
					if `"`svars'"'!=`""' {			// ...and "string" collapse vars
						forvalues i=1/`ncs' {
							local temp : word `i' of `svars'
							label var `temp' `"`ncslab`i''"'
						}
					}
					if `"`byagg'"'!=`""' gen `byaggvar' = 1
					
					tempfile extra1
					qui save `extra1'
				restore
				
			}	// end if `"`cclist'"'!=`""' | `"`svars'"'!=`""'
		}	// end if `"`command'"'!=`""'
	}	// end if (`"`saving'"'!=`""' | `"`graph'"'==`""') & (`"`lcols'"'!=`""' | `"`rcols'"'!=`""')
	
	* If lcols/rcols will not be used, clear the macros so that agg_prog does not throw up conflicts
	else {
		local lcols
		local rcols
	}
	
	* Define matrix colnames
	local colnames `"study ES seES"'
	if `"`over'"'!=`""' local colnames `"over `colnames'"'
	if `"`by'"'!=`""' | `"`byagg'"'!=`""' local colnames `"by `colnames'"'		// we need this column even `by' does not exist for IPD
	
	*** Analyse IPD
	if `"`command'"'!=`""' {

		* Initialise matrices
		* General structure of coefficient matrix is: (regardless of aggregate/IPD/over)
		* 1. by - identifies level of by()
		* 2. over - identifies which over() var (or 1 throughout)
		* 3. study - identifies level of `study' or over() var
		* 4. ES - effect size
		* 5. seES - standard error
		
		* Optional
		* 6. NN - no. of observations (done automatically if IPD but e(N) not explicitly requested)
		* 7+ Other requested quantities from lcols/rcols (stored in `namesr', `namesc', `aggvars', `svars')

		* To be added by GetEstimates()
		* a. wgt
		* b. keep

		* "sgmat1" will contain the IPD data. 
		* It is named sgmat1 for potential comparison with aggregate (sgmat2), or as first and only subgroup.
		* If IPD subgroups exist, it will be renamed `allmat' later on.

		local eN "e(N)"
		if `: list eN in statsr'==0 {
			local nptscol "NN"			// add npts column if e(N) not explicitly requested in lcols/rcols
		}
		local colnames `"`colnames' `nptscol' `namesr'"'
		
		tempname sgmat1
		local matcols : word count `colnames'
		mat `sgmat1'=J(`matrows', `matcols', .)
		mat colnames `sgmat1' = `colnames'
		foreach x of local colnames {
			local `x'col = colnumb(`sgmat1', "`x'")
		}

		* Main IPD analysis loop
		capture drop _rsample
		qui gen byte _rsample=0			// this will show which observations were used

		local n=1						// matrix row counter
		forvalues h=1/`overlen' {		// if over() not specified this will be 1/1
										// else, make `sgroup' equal to (`h')th over variable
			local overh : word `h' of `study'

			* If "over" vars, order them "naturally", i.e. numerically or alphabetically
			* Otherwise, use existing `sgroup' and `ns' from earlier loop
			if `"`over'"'!=`""' {
				capture drop `sgroup'
				qui bysort `touse' `by2' `overh' : gen long `sgroup' = (_n==1 & `touse' & !missing(`overh'))
				qui replace `sgroup' = sum(`sgroup')
				local ns = `sgroup'[_N]				// total number of studies (might be repeats if `by' not trial-level)
			}

			* Loop over trial IDs (or levels of `h'th over var)
			forvalues i=1/`ns' {

				if `n' > `matrows' {
					disp as err "Error in matrix structure"
					exit 198
				}

				* Add by() ID (subgroup)
				if `"`by2'"'!=`""' {
					summ `by2' if `touse' & `sgroup'==`i', meanonly
					mat `sgmat1'[`n', `bycol'] = `r(min)'
				}
				else if `"`byagg'"'!=`""' {
					mat `sgmat1'[`n', `bycol'] = 1		// 1 throughout if byagg
				}

				* Add over() var ID
				if `"`over'"'!=`""' {
					mat `sgmat1'[`n', `overcol'] = `h'
				}
			
				* Create label containing original values or strings,
				* then add (original) trial ID
				summ `overh' if `sgroup'==`i', meanonly
				mat `sgmat1'[`n', `studycol'] = `r(min)'
				local trlabi : label (`overh') `r(min)'
				if `"`messages'"'!=`""' disp in gr "Fitting model for `overh' = `trlabi' ... " _c

				local result=1
				capture `command' if `touse' & `sgroup'==`i' & `result'==1 `modelopts'

				if !_rc {			// if !_rc, a model has been fitted in some way.

					if `"`exp_list'"'==`""' {

						* If exp_list not specified, must be eclass
						* Check this by comparing `command' with e(cmdline)
						capture assert e(cmdline) == `"`command' if `touse' & `sgroup'==`i' & `result'==1 `modelopts'"'
						if _rc {
							disp as err "exp_list must be specified if command is not eclass"
							exit 198
						}
					
						* Examine coefficients & check for anomalies
						ipd_prog, `interaction' `messages' ztol(`ztol') result(`result') ///
							poolvar(`poolvar') estvar(`estvar') term1(`term1') term2(`term2')

						if `"`estvar'"'==`""' {
							local estvar `"`r(estvar)'"'
							local term1 `"`r(term1)'"'
							local term2 `"`r(term2)'"'
						}
						local result=r(result)
						if `result'!=0 {
							local beta = _b[`estvar']
							local sebeta = _se[`estvar']
							local nbeta = `e(N)'
						}
					}		// end if `"`exp_list'"'==`""'
					
					else {
						if `"`poolvar'"'!=`""' {
							disp as err "Cannot specify both poolvar and exp_list"
							exit 198
						}
						tokenize `"`exp_list'"'
						local beta `1'
						local sebeta `2'
						local nbeta `3'
					}

					if `result'==1 {
						* Insert results into matrix
						capture mat `sgmat1'[`n', `EScol'] = `beta'
						capture mat `sgmat1'[`n', `seEScol'] = `sebeta'
						capture mat `sgmat1'[`n', `NNcol'] = `nbeta'

						if `nr'>0 {
							* Insert additional user-requested results into matrix
							forvalues j=1/`nr' {
								local userstat : word `j' of `statsr'
								local usname : word `j' of `namesr'
								capture mat `sgmat1'[`n', ``usname'col'] = `=`userstat''
							}
						}
						qui replace _rsample=1 if e(sample)				// won't do anything if e(sample) not set
						if `"`messages'"'!=`""' disp as res "Done"
					}
				}	// end of "if !rc"

				else if `result'==1 & `"`messages'"'!=`""' {
					disp as err "Error: " _c
					capture noisily error _rc
				}

				local ++n

			}	// end forvalues i=1/`ns'
		}		// end forvalues h=1/`overlen'


		* If "ipdover", then "overall" (and "subgroup") result(s) is/are formed
		*   by running model within the whole dataset instead of pooling across trials
		* Also use this section to generate user-defined statistics for subgroups and overall
		*   but not if aggregate data is present, *unless* byagg, in which case subgroups but no overall
		if (`"`ipdover'"'!=`""' | `nr'>0) & !(`"`aggregate'"'!=`""' & `"`byagg'"'==`""') {
			
			forvalues i = 1 / `= `nby' + (`"`overall'"'==`""' & `"`byagg'"'==`""') ' {

				local byi
				if `"`by2'"'!=`""' & `i'<=`nby' {
					local byi `"`by2'==`: word `i' of `bylist'' & "'
				}

				local result=1
				capture `command' if `byi' `touse' `modelopts'
				if !_rc {

					if `"`exp_list'"'==`""' {
						ipd_prog, `interaction' `messages' ztol(`ztol') result(`result') ///
							estvar(`estvar') term1(`term1') term2(`term2')

						local result=r(result)
						if `result'!=0 {
							local beta = _b[`estvar']
							local sebeta = _se[`estvar']
							local nbeta = `e(N)'
						}
					}
					else {
						tokenize `"`exp_list'"'
						local beta `1'
						local sebeta `2'
						local nbeta `3'
					}

					if `result'==1 {
						local ii
						if `i'<=`nby' local ii = `i'		// if overall, no suffix; otherwise it is `i'
						scalar mu_hat`ii' = `beta'
						scalar se_mu_hat`ii' = `sebeta'
						capture scalar totnpts`ii' = `nbeta'	// may not exist

						* Also store user-requested e() results in local macros for later retrieval
						if `nr'>0 {
							forvalues j=1/`nr' {
								local userstat : word `j' of `statsr'
								local us`ii'_`j' = `=`userstat''
							}
						}
					}		// end if `result'==1
				}		// end if !_rc (capture `command' ...)
			}		// end forvalues i=1/`=`nby'+`"`overall'"'==`""''
		}		// end if `"`ipdover'"'!=`""' | `nr'>0
	}		// end if `"`command'"'!=`""'


	*** Analysis of aggregate data (whether in memory or in external dataset)
	if `"`aggregate'"'!=`""' {
	
		* Study value label: need to make a copy trial-by-trial 
		* (in case it is defined beyond the trials included in this analysis -
		*   otherwise it cannot be added to by agg_prog)
		* (N.B. This was not done as part of earlier code so as to accommodate 'ipdover')
		if `"`: value label `study''"'!=`""' {		// Not required if `study' had no value label to begin with!
			tempname `study'lab2
			qui levelsof `study' if `touse', local(trialidlist)
			foreach i of local trialidlist {
				local trialname : label (`study') `i'
				label define ``study'lab2' `i' `"`trialname'"', add				
			}
			local `study'lab ``study'lab2'
		}
		tempfile agglabfile
		qui label save ``study'lab' using `agglabfile'
		local studyopt `"study(`study') studylab(``study'lab') studylabfile(`agglabfile')"'
		
		* Sort out `by'
		if `"`by'"'!=`""' {
			if `"`by2'"'!=`""' {
				tempfile bylabfile
				qui label save `bylab' using `bylabfile'	// use value label created in earlier code
				summ `by2' if `touse', meanonly
				local maxby=r(max)
				if `"`aggfile'"'!=`""' local by3 `by'		// if IPD+agg, send original var so that names match
				else local by3 `by2'						// if agg only (aggmetan), send by2 (never string)
				local byopt `"by(`by3') bylab(`bylab') bylabfile(`bylabfile') maxby(`maxby')"'
			}
			else if `"`byagg'"'==`""' {		// if `by' exists only in aggregate dataset (and not byagg)
				tempname bylab											// ...create tempname for label
				tempfile bylabfile										// and label tempfile
				local byopt `"by(`by') bylab(`bylab') bylabfile(`bylabfile')"'
			}
		}
		else if `"`byagg'"'!=`""' {
			local byopt `"by(`byaggvar')"'		// if `byagg', just send tempvar (plus `byagg' option)
		}
		
		* If IPD+agg, need max study ID
		if `"`aggfile'"'!=`""' {
			summ `study', meanonly
			local smax=r(max)
			local smaxopt `"smax(`smax')"'
		}

		tempname sgmat2
		tempfile extra2
		agg_prog `aggif' `aggin', aggfile(`aggfile') `aggonly' vars(`vars') ///
			`studyopt' `smaxopt' aggnpts(`npts') `byopt' `byagg' byisstr(`byisstr') ///
			extracols(`namesc' `svars' `aggvars') extrafile(`extra2') ztol(`ztol') sortindex(`_sortindex')
		mat `sgmat2'=r(aggmat)
		local aggextra=r(aggextra)
		local bylist2 `"`r(bylist)'"'

		* Sort out `by'
		if `"`by'"'!=`""' {
		
			* If `by' doesn't exist in IPD (but is needed), create it based on max of bylist outputted by agg_prog
			capture confirm var `by'
			if _rc {
				local nby : word count `bylist2'
				numlist "`bylist2'", sort
				local bylist = 1 + `: word `nby' of `r(numlist)''
				mat `sgmat1'[1, `bycol'] = J(`matrows', 1, `bylist')
				label define `bylab' `bylist' "IPD", add
				local by2 `by'
			}
			numlist `"`: list bylist | bylist2'"', sort
			local bylist = r(numlist)
			local nby : word count `bylist'
		}
		
		if `"``study'varlab'"'==`""' local `study'varlab `"`r(trialvarlab)'"'
	}
	
	* Sort out "extra" files
	tempfile extra
	capture confirm file `extra2'
	if _rc {									// if `extra2' does not exist
		capture confirm file `extra1'
		if !_rc {
			local extra `extra1'			// ... but `extra1' does, let `extra' = `extra1'
		}
	}
	else {										// if `extra2' exists
		capture confirm file `extra1'
		if _rc {
			local extra `extra2'				// ... but `extra1' does not, let `extra' = `extra2'
		}
		else {
			preserve							// if both exist, append one to the other and save
						// N.B. onus is on user to consider varnames
						// ...that is, either to avoid conflicts, or to purposefully match up vars from different sources
						// N.B. 2 - a subtlety - if using byagg (& poss just if agg data??),
						//    agg varname must match with NEWNAME from parsecols!!  AUG 2013 - is there any way to avoid this?
						// AUG 2013 - equally, is having parsecols NOT add "_firstnm" to the end a good idea??
				use `extra1', clear
				qui append using `extra2'

				* If byagg, IPD extra dataset (`extra1') may contain subgroup & overall rows
				* Generate corresponding rows for aggregate dataset
				if `"`byagg'"'!=`""' {
					local newobs = (`"`subgroup'"'==`""') + (`"`overall'"'==`""')
					local oldN = _N
					qui set obs `=`oldN' + `newobs''
					if `"`subgroup'"'==`""' {
						qui replace `byaggvar' = 2 in `=`oldN' + 1'
					}
				}
				qui save `extra'
				
			restore
		}
	}

	
	* Create "overall" matrix (allmat) from IPD and aggregate
	* N.B. the errors/warnings here should never appear, as the relevant situations should already have been detected!
	tempname allmat
	if `"`command'"'!=`""' {				// IPD should exist
		capture confirm matrix `sgmat1'
		if _rc {
			disp as err "IPD estimates do not exist or could not be pooled"
			exit 198
		}
		else {
			if `"`aggregate'"'!=`""' {	 	// Aggregate-data should also exist
				capture confirm matrix `sgmat2'
				if _rc {
					disp as err "Aggregate-data estimates do not exist or could not be pooled"
					exit 198
				}
				else {
					local colnames2 : colnames `sgmat2'
					local extracols : list colnames - colnames2
					local addn : word count `extracols'
					if `addn'>0 {			// add extra blank cols to match with IPD (sgmat1)
						mat `sgmat2' = `sgmat2', J(`=rowsof(`sgmat2')', `addn', .)
					}
					mat colnames `sgmat2' = `colnames'
					mat `allmat' = `sgmat1' \ `sgmat2'
				}
			}
			else {
				mat `allmat' = `sgmat1'		// IPD only
				if `"`byagg'"'!=`""' disp as err _n "Option 'byagg' specified but no aggregate data found"
			}
		}
	}
	else {
		mat `allmat' = `sgmat2'			// Aggregate-data only
		if `"`byagg'"'!=`""' disp as err _n "Option 'byagg' specified but no IPD found"
	}

	* Identify or remove "excluded" studies
	qui mata: ExcludeStudy("`allmat'", "`keepall'")
	
	* Check existence of "allmat" before continuing
	capture confirm matrix `allmat'
	if _rc {
		disp as err `"No estimates found. Check:"'
		disp as err `"- specification of interaction option"'
		disp as err `"- model is able to be fitted within the entire dataset and/or a specific trial"'
		exit 198
	}
	if `"`keepall'"'!=`""' local keepall "keep"

	mat colnames `allmat' = `colnames' /*`lnamesr' `rnamesr'*/ `keepall'
	
	* If aggregate but no IPD, then column number macros haven't been defined yet
	if `"`aggregate'"'!=`""' & `"`command'"'==`""' {	
		foreach x of local colnames {
			local `x'col = colnumb(`allmat', "`x'")
		}
	}


	*** Get inverse-variance weights and perform pooled analysis
	
	* For IPD vs aggregate (byagg), the whole of sgmat1 (IPD) is one subgroup, compared to another (aggregate), sgmat2
	if `"`byagg'"'!=`""'{
		local nby=2				// number of "subgroups" to pass to loop
		tempname bylab
		label define `bylab' 1 "IPD" 2 "Aggregate"
		local bylist `"1 2"'
		local by `"`byagg'"'		// From now on, "by" and "byagg" are indistinguishable
		local by2 `byaggvar'
	}
	
	* Subgroups -- loop over bylist first, then over, to match with display output
	scalar Qsum=0
	tempname ovwtvec sgwtvec
	local n=1
	forvalues i=1/`nby' {				// this will be 1/1 if no subgroups
		
		local byi=.
		if `"`by'"'!=`""' local byi: word `i' of `bylist'
			
		forvalues j=1/`overlen' {		// if over() not specified this will be 1/1
	
			tempname sgwtvec`n' rowvec`n'
			mata: GetEstimates("`allmat'", "`keepall'", "`sgwtvec`n''", "`rowvec`n''", "`type'", "`trunc'", `byi', `j')
			
			capture confirm matrix `sgwtvec`n''
			if !_rc {		// if weight vector exists, i.e. if subgroup could be analysed
				
				local nlist `"`nlist' `n'"'		// collect valid subgroup ID numbers
					
				* Convert rowvec to numlist (may be a better way of doing this)
				* This creates a list identifying which subgroups (in terms of `n') are valid
				forvalues k=1/`=rowsof(`rowvec`n'')' {
					local rowlist`n' `"`rowlist`n'' `=`rowvec`n''[`k',1]'"'
				}
				mat `sgwtvec' = nullmat(`sgwtvec') \ `sgwtvec`n''
				mat drop `sgwtvec`n''
				
				* Subgroup results for ipdmetan (already done for ipdover)
				if `"`by'"'!=`""' & `"`ipdover'"'==`""' {
					foreach x in mu_hat se_mu_hat Q k totnpts tausq sigmasq {
						scalar `x'`i'=scalar(`x')			// rename scalars to differentiate subgroups
					}
					if inlist(`"`type'"',"vb","bs") {
						foreach x in tsq_lci tsq_uci {
							scalar `x'`i'=scalar(`x')		// rename scalars to differentiate subgroups
						}
						scalar Isq`i'=scalar(tausq`i')/(scalar(tausq`i')+scalar(sigmasq`i'))	// Iterative/generalised I-squared
					}
					else {
						scalar Isq`i'=(scalar(Q`i')-scalar(k`i')+1)/scalar(Q`i')		// I-squared
						if `"`trunc'"'==`""' scalar Isq`i'=max(0, scalar(Isq`i'))
					}
					if `"`type'"'=="bs" {
						scalar tsq_var`i'=scalar(tsq_var)		// rename scalars to differentiate subgroups
					}
					scalar Qsum = scalar(Qsum) + scalar(Q`i')
				}
			}		// end if !_rc
			local ++n
			
		}		// end forvalues j=1/`overlen'
	}		// end forvalues i=1/`nby'
	
	* Overall (scalars remain un- renamed)
	if `"`over'"'!=`""' {
		forvalues j=1/`overlen' {
			tempname ovwtvec`j'
			qui mata: GetEstimates("`allmat'", "`keepall'", "`ovwtvec`j''", "", "`type'", "`trunc'", ., `j')
			mat `ovwtvec' = nullmat(`ovwtvec') \ `ovwtvec`j''
			mat drop `ovwtvec`j''
		}
	}
	else {
		qui mata: GetEstimates("`allmat'", "`keepall'", "`ovwtvec'", "", "`type'", "`trunc'")
		
		if `"`by'"'==`""' {
			numlist "1/`=rowsof(`allmat')'"
			local rowlist1=r(numlist)				// if no "by", only 1 rowlist is needed, containing all trials
		}
		
		* Heterogeneity stats
		if `"`ipdover'"'==`""' {					// N.B. "ipdover" may be specified even if "over" is not
			if inlist(`"`type'"',"vb","bs") {
				scalar HsqM=scalar(tausq)/scalar(sigmasq)							// Iterative/generalised H-squared (Mittlboek modification)
				scalar HsqM_lci=scalar(tsq_lci)/scalar(sigmasq)						// H-squared lower confidence limit
				scalar HsqM_uci=scalar(tsq_uci)/scalar(sigmasq)						// H-squared upper confidence limit

				scalar Isq=scalar(tausq)/(scalar(tausq)+scalar(sigmasq))			// Iterative/generalised I-squared
				scalar Isq_lci=scalar(tsq_lci)/(scalar(tsq_lci)+scalar(sigmasq))	// I-squared lower confidence limit
				scalar Isq_uci=scalar(tsq_uci)/(scalar(tsq_uci)+scalar(sigmasq))	// I-squared upper confidence limit
			}
			else {
				scalar HsqM=(scalar(Q)-scalar(k)+1)/(scalar(k)-1)		// H-squared (Mittlboek modification)
				if `"`trunc'"'==`""' scalar HsqM=max(0, scalar(HsqM))
				
				scalar Isq=(scalar(Q)-scalar(k)+1)/scalar(Q)			// I-squared
				if `"`trunc'"'==`""' scalar Isq=max(0, scalar(Isq))
			}
		}
	}
	local wtvec = cond(`"`by'"'!=`""' & `"`overall'"'!=`""' & `"`subgroup'"'==`""', `"`sgwtvec'"', `"`ovwtvec'"')
	
	
	
	**************************
	* Print output to screen *
	**************************
	
	* Full method names
	if "`type'"=="fe" local typefull "Fixed-effects"
	if "`type'"=="dl" local typefull "DerSimonian-Laird random-effects"
	if "`type'"=="vb" local typefull "Generalised Q random-effects"
	if "`type'"=="bs" local typefull "Approximate Gamma random-effects"
	* if `"`type'"'==`"reml"' local typefull "REML"
	
	* Print number of trials/patients to screen
	* (NB nos. actually analysed as opposed to the number supplied in original data)
	if `"`byagg'"'==`""' {
		disp _n
		if `"`ipdover'"'==`""' disp in gr "Trials included: " in ye scalar(k)
		local dispnpts=scalar(totnpts)
		if scalar(totnpts)==0 local dispnpts "Unknown"
		disp in gr "Patients included: " in ye "`dispnpts'"
	}
	else {
		disp in gr _n "Trials included from IPD: " in ye scalar(k1)
		local dispnpts=scalar(totnpts1)
		if scalar(totnpts1)==0 local dispnpts "Unknown"
		disp in gr "Patients included: " in ye "`dispnpts'"
		
		disp in gr _n "Trials included from aggregate data: " in ye scalar(k2)
		local dispnpts=scalar(totnpts2)
		if scalar(totnpts2)==0 local dispnpts "Unknown"
		disp in gr "Patients included: " in ye "`dispnpts'"
	}
	
	if `"`interaction'"'!=`""' local pooling "`pooltext' of interaction effect estimate"
	else if `"`exp_list'"'!=`""' local pooling "`pooltext' of user-specified effect estimate"
	else if `"`aggonly'"'!=`""' local pooling "`pooltext' of aggregate data"
	else local pooling "`pooltext' of main (treatment) effect estimate"
	
	di _n as text "`pooling'" as res " `estvar'"
	if `"`ipdover'"'==`""' {
		disp as text "using" as res " `typefull'"
	}
	if `"`exp_list'"'!=`""' {
		disp as text "(caution: estimates not checked for validity!)"
	}

	
	*** Trial effects box
	if `"`table'"'==`""' {
	
		* Find maximum length of study labels
		local lablen 0
		forvalues i=1/`=rowsof(`allmat')' {
			if `"`by'"'!=`""' {
				local byi = `allmat'[`i', `bycol']
				local bylablist : list bylablist | byi
			}
			local overi = 1
			if `"`over'"'!=`""' {
				local overi = `allmat'[`i', `overcol']
				local ovlablist : list ovlablist | overi
			}
			local studyi = `allmat'[`i', `studycol']
			local overvarname : word `overi' of `study'
			local studytext : label (`overvarname') `studyi'
			local len = length(`"`studytext'"')
			if `len'>`lablen' local lablen=`len'
		}
		if `"`by'"'!=`""' {
			foreach x of local bylablist {
				local bytext : label `bylab' `byi'
				local len = length(`"`bytext'"')
				if `len'>`lablen' local lablen=`len'
			}
		}
		if `"`over'"'!=`""' {
			foreach x of local ovlablist {
				local overvarname : word `x' of `study'
				local overtext : variable label `overvarname'
				local len = length(`"`overtext'"')
				if `len'>`lablen' local lablen=`len'	
			}
		}
		local uselen 21							// default (minimum)
		if `lablen'>21 local uselen=`lablen'
		if `lablen'>32 local uselen=32 			// maximum
		
		* Find maximum length of study varname
		* Allow it to spread over several lines, but only up to a maximum of 32 chars
		* If a single line must be more than 32 chars, truncate and stop
		local line = 1
		if `"`ipdover'"'==`""' {
			local title : variable label `study'
			
			if `"`title'"'!=`""' {
				local titlelen = length(`"`title'"')
				local spread = int(`titlelen'/`uselen')+1
				if `spread'>1 {
					local end = 0
					local count = 1
					local last = word(`"`title'"', -1)

					local current = word(`"`title'"', `count')
					local next = word(`"`title'"', `=`count'+1')
					
					while `end' == 0 {
						local title`line' = `"`title`line''"' + " " + `"`current'"'		// add in the next word
						local check = `"`title`line''"' + `" `next'"'					// what next would be

						local ++count
						local current = word(`"`title'"', `count')
						if `"`current'"' == "" | length(`"`title`line''"') > 32 {
							local end = 1
							if length(`"`title`line''"') > 32 {
								local title`line' = substr(`"`title`line''"', 1, 31)
								local uselen=32
							}
						}
						if length(`"`title`line''"') > `titlelen'/`spread' ///
							| length(`"`check'"') > `titlelen'/`spread' {
							if `end' == 0 {
								local ++line
								local next = word(`"`title'"', `=`count'+1')
							}
						}
					}
				}
			}
		}		// end if `"`ipdover'"'==`""'
		
		di as text _n "{hline `uselen'}{c TT}{hline 45}"
		
		if `line'==1 {
			if `"`ipdover'"'==`""' {
				if `"`title'"'==`""' local header "Study"
				else local header `"`title'"'
				local final "% Weight"
			}
			else if `"`ipdover'"'!=`""' {
				local header "Subgroup"
				local final "No. pts"
			}
		}
		else {
			forvalues i=1/`=`line'-1' {
				di as text "`title`i''{col `=`uselen'+1'}{c |}"
			}
			local header `title`line''
			local final "% Weight"
		}
		if `"`effect'"'==`""' local effect "Effect"
		else local effect = substr(`"`effect'"', 1, 10)
		di as text "`header'{col `=`uselen'+1'}{c |}{col `=`uselen'+5'}" %~10s `"`effect'"' "{col `=`uselen'+14'}[`c(level)'% Conf. Interval]{col `=`uselen'+37'}`final'"

		
		
		*** Loop over trials, and subgroups if appropriate
		local n=1
		forvalues i=1/`nby' {				// this will be 1/1 if no subgroups

			di as text "{hline `uselen'}{c +}{hline 45}
			
			if `"`by'"'!=`""' {
				local byi: word `i' of `bylist'
				local bytext : label `bylab' `byi'
				di as text substr(`"`bytext'"', 1, `=`uselen'-1') + "{col `=`uselen'+1'}{c |}"
			}
			if "`eform'"!=`""' local exp `"exp"'

			forvalues j=1/`overlen' {
				if `: list n in nlist'==1 | (`nby'*`overlen' == 1) {		// i.e. EITHER valid subgp combo OR no subgps at all
				
					if `"`over'"'!=`""' {
						local overh : word `j' of `study'
						di as text "{col `=`uselen'+1'}{c |}"
						di as text substr(`"``overh'varlab'"', 1, `=`uselen'-1') "{col `=`uselen'+1'}{c |}"
					}
					else {
						local overh `study'
						local sumwgt`i'=0
					}

					foreach k of numlist `rowlist`n'' {
						local kk = `allmat'[`k', `studycol']
						local ESkk = `allmat'[`k', `EScol']
						local lckk = `allmat'[`k', `EScol'] - invnorm(.5 + `c(level)'/200)*`allmat'[`k', `seEScol']
						local uckk = `allmat'[`k', `EScol'] + invnorm(.5 + `c(level)'/200)*`allmat'[`k', `seEScol']

						if `"`ipdover'"'!=`""' {
							local final `"%7.0f `allmat'[`k', `NNcol']"'
						}
						else local final `"%7.2f 100*`wtvec'[`k',1]"'
						
						local labelvarkk : label ``overh'lab' `kk'
						if missing(`ESkk') {
							di as text substr(`"`labelvarkk'"',1, 32) "{col `=`uselen'+1'}{c |}{col `=`uselen'+4'} (Insufficient data)"
						}
						else {
							di as text substr(`"`labelvarkk'"',1, 32) "{col `=`uselen'+1'}{c |}{col `=`uselen'+4'}" ///
								as res %7.3f `=`exp'(`ESkk')' "{col `=`uselen'+15'}" ///
								as res %7.3f `=`exp'(`lckk')' "{col `=`uselen'+25'}" ///
								as res %7.3f `=`exp'(`uckk')' "{col `=`uselen'+36'}" `final'
						}

						if `"`ipdover'"'==`""' local sumwgt`i' = `sumwgt`i'' + `wtvec'[`k',1]
					}
				}
				local ++n
			}		// end forvalues j=1/`overlen'

			* Subgroup effects
			if `"`by'"'!=`""' & `"`subgroup'"'==`""' {
								
				local mu_lci`i' = scalar(mu_hat`i')-invnorm(.5 + `c(level)'/200)*scalar(se_mu_hat`i')
				local mu_uci`i' = scalar(mu_hat`i')+invnorm(.5 + `c(level)'/200)*scalar(se_mu_hat`i')
				
				if `"`ipdover'"'==`""' {
					local final `"%7.2f `=100*`sumwgt`i'''"'
				}
				else {
					local final `"%7.0f `=scalar(totnpts`i')'"'
				}
				di as text "{col `=`uselen'+1'}{c |}"
				di as text "Subgroup effect{col `=`uselen'+1'}{c |}{col `=`uselen'+4'}" ///
					as res %7.3f `=`exp'(scalar(mu_hat`i'))' "{col `=`uselen'+15'}" ///
					as res %7.3f `=`exp'(`mu_lci`i'')' "{col `=`uselen'+25'}" ///
					as res %7.3f `=`exp'(`mu_uci`i'')' "{col `=`uselen'+36'}" `final'
			}
		}		// end forvalues i=1/`nby'		
		
		
		*** Overall effect
		if `"`overall'"'==`""' {
			di as text "{hline `uselen'}{c +}{hline 45}"
			
			local mu_lci=scalar(mu_hat)-invnorm(.5 + `c(level)'/200)*scalar(se_mu_hat)
			local mu_uci=scalar(mu_hat)+invnorm(.5 + `c(level)'/200)*scalar(se_mu_hat)
			
			if `"`ipdover'"'==`""' {
				local final `"%7.2f 100"'
			}
			else {
				local final `"%7.0f `=scalar(totnpts)'"'
			}
			di as text %-20s "Overall effect{col `=`uselen'+1'}{c |}{col `=`uselen'+4'}" ///
				as res %7.3f `=`exp'(scalar(mu_hat))' "{col `=`uselen'+15'}" ///
				as res %7.3f `=`exp'(`mu_lci')' "{col `=`uselen'+25'}" ///
				as res %7.3f `=`exp'(`mu_uci')' "{col `=`uselen'+36'}" `final'
		}
		di as text "{hline `uselen'}{c BT}{hline 45}"

		* Tests, heterogeneity etc. -- only for pooled analysis (i.e. no ipdover)
		if `"`ipdover'"'==`""' {
		
			* Z-test of pooled effect equal to zero
			local null = cond(`"`eform'"'==`""', 0, 1)
			if `"`by'"'==`""' | `"`subgroup'"'!=`""' {
				if `"`overall'"'==`""' {
					local zvalue=scalar(mu_hat)/scalar(se_mu_hat)
					local pvalue=2*(1-normal(abs(`zvalue')))
					di as text _n "Test of overall effect = `null': z = " as res %7.3f `zvalue' as text "  p = " as res %7.3f `pvalue'
				}
			}
			else {
				local uselen 25							// default (minimum)
				if `lablen'>25 local uselen=`lablen'
				if `lablen'>32 local uselen=32 			// maximum
			
				di as text _n "Tests of effect size = `null':"
				forvalues i=1/`nby' {
					local byi: word `i' of `bylist'
					local zvalue=scalar(mu_hat`i')/scalar(se_mu_hat`i')
					local pvalue=2*(1-normal(abs(`zvalue')))
					local bylabi : label `bylab' `byi'
					di as text substr("`bylabi'", 1, `=`uselen'-1') "{col `=`uselen'+1'}z = " as res %7.3f `zvalue' as text "  p = " as res %7.3f `pvalue'
				}
				if `"`overall'"'==`""' {
					local zvalue=scalar(mu_hat)/scalar(se_mu_hat)
					local pvalue=2*(1-normal(abs(`zvalue')))
					di as text "Overall:{col `=`uselen'+1'}z = " as res %7.3f `zvalue' as text "  p = " as res %7.3f `pvalue'
				}
			}
			
			* Heterogeneity measures box: no subgroups
			if `"`overall'"'==`""' & (`"`by'"'==`""' | `"`subgroup'"'!=`""') {
				di as text _n(2) "Heterogeneity Measures"
					
				local uselen 15							// default (minimum)
				if `lablen'>15 local uselen=`lablen'
				if `lablen'>32 local uselen=32 			// maximum
					
				* Q, I2, H2
				di as text "{hline `uselen'}{c TT}{hline 35}"
				di as text "{col `=`uselen'+1'}{c |}{col `=`uselen'+7'}value{col `=`uselen'+18'}df{col `=`uselen'+25'}p-value"
				di as text "{hline `uselen'}{c +}{hline 35}"
				local df = scalar(k) - 1
				local qpval=chi2tail(`df', scalar(Q))
				di as text "Cochrane Q {col `=`uselen'+1'}{c |}{col `=`uselen'+5'}" ///
					as res %7.2f scalar(Q) "{col `=`uselen'+14'}" %6.0f `df' "{col `=`uselen'+23'}" %7.3f `qpval'
				
				if !inlist("`type'", "vb", "bs") {
					di as text "I`=char(178)' (%) {col `=`uselen'+1'}{c |}{col `=`uselen'+4'}" as res %7.1f 100*scalar(Isq) "%"
					di as text "Modified H`=char(178)' {col `=`uselen'+1'}{c |}{col `=`uselen'+5'}" as res %7.3f scalar(HsqM)
					di as text "tau`=char(178)' {col `=`uselen'+1'}{c |}{col `=`uselen'+4'}" as res %8.4f scalar(tausq)
				}
				else {		// display second box with CIs for tausq etc.
					di as text "{hline `uselen'}{c BT}{hline 35}"
					di as text _n "{hline `uselen'}{c TT}{hline 35}"
					di as text "{col `=`uselen'+1'}{c |}{col `=`uselen'+7'}value{col `=`uselen'+15'}[`c(level)'% Conf. Interval]"
					di as text "{hline `uselen'}{c +}{hline 35}"
					di as text "I`=char(178)' (%) {col `=`uselen'+1'}{c |}{col `=`uselen'+4'}" ///
						as res %7.1f 100*scalar(Isq) "%{col `=`uselen'+14'}" ///
						as res %7.1f 100*scalar(Isq_lci) "%{col `=`uselen'+24'}" %7.1f 100*scalar(Isq_uci) "%"
					di as text "Modified H`=char(178)' {col `=`uselen'+1'}{c |}{col `=`uselen'+5'}" ///
						as res %7.3f scalar(HsqM) "{col `=`uselen'+15'}" ///
						as res %7.3f scalar(HsqM_lci) "{col `=`uselen'+25'}" %7.3f scalar(HsqM_uci)
					di as text "tau`=char(178)' {col `=`uselen'+1'}{c |}{col `=`uselen'+4'}" ///
						as res %8.4f scalar(tausq) "{col `=`uselen'+14'}" ///
						as res %8.4f scalar(tsq_lci) "{col `=`uselen'+24'}" %8.4f scalar(tsq_uci)
				}
				di as text "{hline `uselen'}{c BT}{hline 35}"
				
				* Display explanations
				di as text _n `"I`=char(178)' = between-study variance (tau`=char(178)') as a percentage of total variance"'
				di as text `"Modified H`=char(178)' = ratio of tau`=char(178)' to typical within-study variance"'
			}

			* Heterogeneity measures box: subgroups (just present Q statistics)
			if `"`by'"'!=`""' & `"`subgroup'"'==`""' {
			
				di as text _n(2) "Q statistics for heterogeneity (calculated using Inverse Variance weights)"
				di as text "{hline `uselen'}{c TT}{hline 35}"
				di as text "{col `=`uselen'+1'}{c |}{col `=`uselen'+7'}value{col `=`uselen'+17'}df{col `=`uselen'+24'}p-value"
				di as text "{hline `uselen'}{c +}{hline 35}"

				forvalues i=1/`nby' {
					local byi: word `i' of `bylist'
					local bylabi : label `bylab' `byi'
					local bylabi=abbrev(`"`bylabi'"',32)
					local df = scalar(k`i') - 1
					local qpval=chi2tail(`df', scalar(Q`i'))
					di as text substr("`bylabi'", 1, `=`uselen'-1') "{col `=`uselen'+1'}{c |}{col `=`uselen'+5'}" ///
						as res %7.2f scalar(Q`i') "{col `=`uselen'+14'}" %6.0f `df' "{col `=`uselen'+23'}" %7.3f `qpval'
				}
				
				if `"`overall'"'==`""' {
					local df=scalar(k) - 1
					local qpval=chi2tail(`df', scalar(Q))
					di as text "Overall{col `=`uselen'+1'}{c |}{col `=`uselen'+5'}" ///
						as res %7.2f scalar(Q) "{col `=`uselen'+14'}" %6.0f `df' "{col `=`uselen'+23'}" %7.3f `qpval'

					local qdiff = scalar(Q) - scalar(Qsum)
					local qdiffpval=chi2tail(`=`nby'-1',`qdiff')
					di as text "Between{col `=`uselen'+1'}{c |}{col `=`uselen'+5'}" ///
						as res %7.2f `qdiff' "{col `=`uselen'+14'}" %6.0f 1 "{col `=`uselen'+23'}" %7.3f `qdiffpval'

				}
				di as text "{hline `uselen'}{c BT}{hline 35}"
			}
		}	// end if `"`ipdover'"'==`""'
	}	// end if `"`table'"'==`""'

	* Add weight vector and colname (even for ipdover, as it's still needed for forestplot)
	mat `allmat' = `allmat', `wtvec'
	mat colnames `allmat' = `colnames' `keepall' wgt
	

	*** Graphics
	if `"`saving'"'!=`""' | `"`graph'"'==`""' {
		quietly {
		
			if `"`saving'"'!=`""' {
				local 0 `saving'
				syntax anything(id="file" name=filename) [, REPLACE STACKlabel ASSUBGP]
			}
	
			preserve						// N.B. PRESERVE HERE
			drop _all						// start afresh, so non-tempnames can be used
											// this is important, as not easy to transfer tempnames to a subroutine
			svmat `allmat', names(col)
			foreach x in by over study ES seES wgt NN {
				capture confirm var `x'
				if !_rc rename `x' _`x'
			}
			
			* Labels: study names
			gen str _labels=""
			if `"`name'"'==`""' {
				forvalues h=1/`overlen' {
					local overh : word `h' of `study'
					label values _study ``overh'lab'
					decode _study, gen(label`h')
					if `"`over'"'!=`""' replace _labels=label`h' if _over==`h'
					else replace _labels=label`h'
					drop label`h'
				}
				if `overlen'>1 label values _study		// if "over", remove labels
			}
			
			* Define observation type
			tempvar excl
			gen byte _use = 1
			gen byte `excl' = (_ES>=. | _seES<=0 | _seES>=.)
			
			if `"`by'"'!=`""' local byvar "_by"				// tempvar in case by not present
			if `"`over'"'!=`""' local overvar "_over"		// tempvar in case over not present
			
			* Preserve sort order of studies (so that screen output and forest plot have same order)
			* (N.B. "by" and "over" are ordered naturally
			tempvar obs sobs sgroup
			gen long `obs'=_n
			bysort _study (`obs') : gen long `sobs' = `obs'[1]
			bysort `byvar' `sobs' : gen long `sgroup' = (_n==1)
			qui replace `sgroup' = sum(`sgroup')
			drop `obs' `sobs'
			
			* If "by" only exists in one or other of IPD and aggregate datasets (and not `byagg')
			* then "by" will have missing values
			if `"`by'"'!=`""' {
				summ _by, meanonly
				qui replace _by = r(max) + 1 if missing(_by)
			}
			
			* Add extra lines to hold subgroup & overall data
			if `"`by'"'!=`""' & "`subgroup'"==`""' {
				tempvar expand
				sort `byvar' `overvar'
				by `byvar' : gen byte `expand' = 1 + (_n==_N)
				expand `expand'
				gsort `byvar' -`expand' _use `sgroup'
				by `byvar' : replace _use=3 if `expand'>1 & _n==2
				replace _study=. if _use==3
				drop `expand'
				
				forvalues i=1/`nby' {
						
					local byi: word `i' of `bylist'
						
					replace _ES = scalar(mu_hat`i') if _use==3 & _by==`byi'			// subgroup ES
					replace _seES = scalar(se_mu_hat`i') if _use==3 & _by==`byi'	// subgroup seES
					
					capture confirm var _wgt
					if !_rc & `"`ipdover'"'==`""' {
						replace _wgt = `sumwgt`i'' if _use==3 & _by==`byi'			// subgroup weight (if applicable)
					}
					capture confirm var _NN
					if !_rc & scalar(totnpts`i')>0 {
						replace _NN = scalar(totnpts`i') if _use==3 & _by==`byi'	// subgroup n (if applicable)
					}
					
					* Add user-requested e() stats
					if `nr'>0 {
						forvalues j=1/`nr' {
							local usname : word `j' of `namesr'
							capture confirm var `usname'
							if !_rc & `"`us`i'_`j''"'!=`""' {
								replace `usname' = `us`i'_`j'' if _use==3 & _by==`byi'
							}
						}
					}
				}
			}
			if `"`overall'"'==`""' {
				local nobs1 = _N+1
				set obs `nobs1'
				replace _use = 5 in `nobs1'
				replace _ES = scalar(mu_hat) if _use==5			// overall ES
				replace _seES = scalar(se_mu_hat) if _use==5	// overall se(ES)

				capture confirm var _wgt
				if !_rc replace _wgt = 1 if _use==5				// overall weight (100%) (if applicable)
				
				capture confirm var _NN
				if !_rc replace _NN = scalar(totnpts) if _use==5	// total n (if applicable)

				* Add user-requested e() stats
				if `nr'>0 {
					forvalues j=1/`nr' {
						local usname : word `j' of `namesr'
						capture confirm var `usname'
						if !_rc & `"`us_`j''"'!=`""' {
							replace `usname' = `us_`j'' if _use==5
						}
					}
				}
				if `"`by'"'!=`""' replace _by=. if _use==5
			}
			
			* Apply variable labels and formats to lcols/rcols "returned data"
			forvalues i=1/`nr' {
				local temp : word `i' of `namesr'
				label var `temp' `"`nrlab`i''"'
			}
			
			* Merge in extra data -- still a bit ugly but possibly as good as it's going to get
			* (now that the extra lines have been created to match up with those in the `extra' dataset)
			capture confirm file `extra'
			if !_rc {
				
				* temporarily rename subgp var (if appropriate)
				rename _by `by2'		// `by2' already appropriately defined

				* if `overlen'==1 rename _study `study'
				* else local extraover "_over _study"
				if `overlen'>1 local over2 "_over"
				
				if `"`keepall'"'==`""' local keepmatch `"keep(match master)"'

				merge 1:1 `by2' /*`study' `extraover'*/ `over2' _study using `extra', nogen `keepmatch'
				
				if `"`by2'"'!=`""' rename `by2' _by 		// rename back
				* if `overlen'==1 rename `study' _study		// rename back
			}

			* Variable name (title) for "_labels"
			if `"`ipdover'"'==`""' {
				if `"``study'varlab'"'!=`""' & `"`stacklabel'"'==`""' {
					label var _labels `"``study'varlab'"'
				}
				else label var _labels "Study ID"
			}
			else label var _labels "Subgroup"
			
			* Apply variable labels and formats to lcols/rcols only present in aggregate data
			forvalues i=1/`na' {
				local temp : word `i' of `aggvars'
				label var `temp' `"`nalab`i''"'
			}
			
			* Apply formats to lcols/rcols
			if `"`fmts'"'!=`""' {
				forvalues i=1/`ni' {
					local temp : word `i' of `lrcols'
					local fmti : word `i' of `fmts'
					capture confirm numeric var `temp'
					if !(_rc | `"`fmti'"'==`"null"') {
						format `temp' `fmti'
					}
				}
			}

			
			*** Insert extra rows for headings, labels, spacings etc.
			* Note: in the following routines, "half" values of _use are used temporarily to get correct order
			*       and are then replaced with whole numbers at the end

			* Subgroup headings and spacings ("by", "over", both)
			if `"`by'"'!=`""' | `"`over'"'!=`""' {
				tempvar expand
				bysort `byvar' `overvar' : gen byte `expand' = 1 + 2*(_n==1)*!missing(`byvar')
				expand `expand'
				gsort `byvar' `overvar' -`expand' _use `sgroup'
				by `byvar' `overvar' : replace _use=0 if `expand'>1 & _n==2		/* row for headings */
				by `byvar' `overvar' : replace _use=4 if `expand'>1 & _n==3		/* row for blank line */
				drop `expand'
				
				* Extra "by" subgroup headings if "over" also used
				if `"`by'"'!=`""' & `"`over'"'!=`""' {
					tempvar expand
					bysort `byvar' : gen byte `expand' =  1 + 3*(_n==1)*!missing(`byvar')
					expand `expand'
					gsort `byvar' -`expand' _use `sgroup'
					by `byvar' : replace _use=-1.5 if `expand'>1 & _n==2		/* row for blank line */
					by `byvar' : replace _use=-1 if `expand'>1 & _n==3   		/* row for by label if "over & by" */
					by `byvar' : replace _use=-0.5 if `expand'>1 & _n==4		/* row for blank line */
					drop `expand'
				}
			}
		
			* Subgroup spacings & heterogeneity ("by" only)
			if `"`by'"'!=`""' & "`subgroup'"=="" {
				tempvar expand
				local x=0
				if `"`over'"'!=`""' local x=1
				sort `byvar' `overvar'
				by `byvar' : gen byte `expand' = 1 + (/*`x' + */(`"`het'"'==`""' & `"`lcols'"'!=`""')) * (_n==_N)
				expand `expand'
				gsort `byvar' -`expand' _use `sgroup'
				if `"`over'"'!=`""' {
					by `byvar' : replace _use=2.5 if `expand'>1 & _n==2		/* row for blank line ("over" only) */
				}
				if `"`het'"'==`""' & `"`lcols'"'!=`""' {
					by `byvar' : replace _use=3.5 if `expand'>1 & _n==2 & !missing(`byvar')		/* extra row for het if lcols */
					
					if `"`overall'"'==`""' {
						by `byvar' : replace _use=5.5 if `expand'>1 & _n==2 & missing(`byvar')	/* extra row for het if lcols - OVERALL */
					}
				}
				drop `expand'
			}
			
			* Blank out effect sizes etc. in new rows
			foreach x of varlist _ES-_labels `lrcols' {
				capture confirm numeric variable `x'
				if !_rc replace `x' = . if !inlist(_use, 1, 3, 5)
				else replace `x' = "" if !inlist(_use, 1, 3, 5)
			}
			replace _study=. if !inlist(_use, 1, 2)
			replace _use=2 if _use==1 & `excl'
			drop `excl'
		
			* If requested (& appropriate), make "overall" estimates into "subgroup" estimates
			if `"`assubgp'"'!=`""' {
				qui count if _use==3
				if r(N)==0 {
					qui count if _use==5
					if r(N)>0 {
						replace _use=3 if _use==5
					}
				}
				else {
					disp as err "cannot specify ASSUBGP option if subgroup pooled estimates already exist"
					exit 198
				}
			}
			
			*** Now insert label info into new rows
			* over() labels
			if `"`over'"'!=`""' {
				forvalues h=1/`overlen' {
					local overh : word `h' of `over'
					replace _labels = `"``overh'varlab'"' if _use==0 & _over==`h'
				}
			}
			* Extra row to contain what would otherwise be the leftmost column heading
			*   if `stacklabel' specified (i.e. so that heading can be used for forestplot stacking)
			else if `"`stacklabel'"' != `""' {
				local nobs1 = _N+1
				set obs `nobs1'
				replace _use = -1 in `nobs1'
				replace _labels = `"``study'varlab'"' in `nobs1'
			}
			
			* "Overall" labels
			if `"`overall'"'==`""' {
				local ovlabel
				if `"`het'"'==`""' {				// if "nohet" not specified -- this implies no "over"
					local df=scalar(k) - 1
					local qpval=chi2tail(`df', scalar(Q))
					if "`ovstat'"=="q" {
						local ovlabel "(Q = " + string(`=scalar(Q)', "%5.2f") + " on `df' df, p = " + string(`qpval', "%5.3f") + ")"
					}
					else {
						local ovlabel "(I-squared = " + string(`=100*scalar(Isq)', "%5.1f")+ "%, p = " + string(`qpval', "%5.3f") + ")"
					}
					if `"`lcols'"'!=`""' replace _labels = "`ovlabel'" if _use==5.5		// ovlabel on line below so no conflict with lcols
				}
				replace _labels = "Overall `ovlabel'" if _use==5
			}
			
			* Subgroup ("by") headings & labels
			if `"`by'"'!=`""' {
			
				forvalues i=1/`nby' {
						
					local byi: word `i' of `bylist'
					
					* Headings
					local bytext : label `bylab' `byi'
					if `"`over'"'!=`""' replace _labels = "`bytext'" if _use==-1 & _by==`byi'
					else replace _labels = "`bytext'" if _use==0 & _by==`byi'
					
					* Labels + heterogeneity
					if `"`subgroup'"'==`""' {
					
						local ovlabel
						if `"`het'"'==`""' {		// if "nohet" not specified -- this implies no "over"
							local df = scalar(k`i') - 1
							local qpval=chi2tail(`df', scalar(Q`i'))
								
							/* RMH I-squared added in next line
								RJH- also p-val as recommended by Mike Bradburn */
							if "`ovstat'"=="q" {
								local ovlabel "(Q = " + string(`=scalar(Q`i')', "%5.2f") + " on `df' df, p = " + string(`qpval', "%5.3f") + ")"
							}
							else {
								local ovlabel "(I-squared = " + string(`=100*scalar(Isq`i')', "%5.1f")+ "%, p = " + string(`qpval', "%5.3f") + ")"
							}
							if `"`lcols'"'!=`""' replace _labels = "`ovlabel'" if _use==3.5 & _by==`byi'
							// ovlabel on line below so no conflict with lcols
						}
						replace _labels = "Subtotal `ovlabel'" if _use==3 & _by==`byi'
					}
				}
				
				* Add between-group heterogeneity info if appropriate
				if `"`ipdover'"'==`""' & `"`overall'"'==`""' & "`ovstat'"!="none" {
					local qdiff = scalar(Q) - scalar(Qsum)
					local qdiffpval=chi2tail(`=`nby'-1', `qdiff')
					
					set obs `=_N+1'
					replace _use = 4.5 in `=_N'
					replace _labels = "Heterogeneity between groups: p = " + string(`qdiffpval', "%5.3f") in `=_N'
				}
			}
						
			sort `byvar' `overvar' _use `sgroup'	/* keep */

			replace _use = 0 if _use == -1
			replace _use = 4 if inlist(_use, -0.5, -1.5, 2.5, 3.5, 4.5, 5.5)
			gen _lci = _ES - invnorm(.5 + `c(level)'/200)*_seES
			gen _uci = _ES + invnorm(.5 + `c(level)'/200)*_seES

			drop `sgroup'
			order _use `byvar' `overvar' _study _labels _ES _seES _lci _uci _wgt `lcols' `rcols'

			 											// unless "over", rename back to original varname
			if `overlen'==1 rename _study `study'		// since _study not needed by forestplot
			
		}	// end of quietly
		
		* Save dataset, or run graphics subroutine
		if `"`saving'"'!=`""' {
			qui save `filename', `replace'
		}
		if `"`graph'"'==`""' {
			if `"`ipdover'"'!=`""' {
				local forestplot `"nowt `forestplot'"'
				label var _NN "No. pts"
				local rcols `"_NN `rcols'"'		// Automatically add "_NN" to rcols if ipdover
			}
			forestplot, passthru `name' labels(_labels) by(`byvar') ///
				type(`type') `interaction' `eform' effect(`effect') lcols(`lcols') rcols(`rcols') `forestplot'
		}

		restore
	
	}	// end of "if graph"
	
	
	
	*** Return statistics
	
	return matrix coeffs=`allmat'						// return matrix of coefficients
	
	if `"`byagg'"'==`""' {								// what to return for agg data??
		return scalar k=scalar(k)
		return scalar n=scalar(totnpts)
	}
	if `"`overall'"'==`""' {
		return local estvar=`"`estvar'"'
		if `"`ipdover'"'==`""' return local type=`"`type'"'
		return scalar mu_hat=scalar(mu_hat)
		return scalar se_mu_hat=scalar(se_mu_hat)
		if `"`ipdover'"'==`""' {
			return scalar Q=scalar(Q)					// agg: surely need to return k or df corresponding to this Q?
			return scalar tausq=scalar(tausq)
			return scalar sigmasq=scalar(sigmasq)
			return scalar Isq=scalar(Isq)
			return scalar HsqM=scalar(HsqM)
		}
		if "`type'"=="vb" | "`type'"=="bs" {
			return scalar tsq_lci=scalar(tsq_lci)
			return scalar tsq_uci=scalar(tsq_uci)
		}
		if "`type'"=="bs" {
			return scalar tsq_var=scalar(tsq_var)
		}
	}
	if `"`by'"'!=`""' & `"`over'"'==`""' & `"`subgroup'"'==`""' {
		forvalues i=1/`nby' {
			return scalar mu_hat`i' = mu_hat`i'
			return scalar se_mu_hat`i' = se_mu_hat`i'
		}
	}
	
	
end


********************************************************




*********************
* Stata subroutines *
*********************

* -parsecollapse-
* by David Fisher, August 2013

* Parses a list of "items" and outputs local macros for other programs (e.g. ipdmetan or collapse)
* Written for specific use within -ipdmetan-
*   identifying & returning "returned values" from regression commands (e.g. e(N) )
*   identifying & returning "collapse-style" items to pass to collapse
*   identifying & returning labels (within quotation marks) and formats (%fmt) for later use

* N.B. Originally written (by David Fisher) as -collapsemat- , November 2012
* This did both the parsing AND the "collapsing", including string vars and saving to matrix or file.
* The current program instead *prepares* the data and syntax so that the official -collapse- command can be used.

program define parsecols, rclass
	version 8, missing
	syntax anything(name=clist id=clist equalok), RCOLS(integer) [BYAGG]		// byagg option implies var may not exist in memory
	
	local clist: subinstr local clist "[]" "", all
	local na=0					// counter of vars not in IPD (i.e. in aggregate dataset only)
	local nc=0					// counter of "collapse" vars
	local ncs=0					// counter of "collapse" vars that are strings (cannot be processed by -collapse-)
	local nr=0					// counter of "returned" vars
	local stat "null"			// GetOpStat needs a "placeholder" stat at the very least. Gets changed later if appropriate
	local fmt "null"			// placeholder format
	local fmtnotnull=0			// marker of whether *any* formatting has been specified
	local sortpreserve=0		// marker of whether sortpreserve is needed
	
	while `"`clist'"' != "" {
		
		gettoken next rest : clist, parse(`":"')
		if `"`next'"'==`":"' {
			local rcols=1					// colon indicates partition from lcols to rcols
			local clist `"`rest'"'
		}
		GetOpStat stat clist : "`stat'" `"`clist'"'

		* Get next two tokens -- first should be a name (NOT a label), second might be "=" or a format (%...)
		gettoken next rest : clist, parse(`" ="') bind qed(qed1)
		gettoken tok2 rest2 : rest, parse(`" ="') bind qed(qed2)
		if `qed1' == 1 {			// quotes around first element
			disp as err `"Error in lcols or rcols syntax: check ordering/structure of elements"'
			exit 198
		}

		if "`tok2'" == "=" {
			gettoken newname rest : clist, parse(" =")		// extract `newname'
			gettoken equal clist : rest, parse(" =")		// ...and start loop again
		}
		else if substr(`"`tok2'"',1,1)==`"%"' {				// var followed by format
			confirm format `tok2'
			local fmt `"`tok2'"'
			local fmtnotnull=1
			local clist : subinstr local clist "`tok2'" ""	// remove (first instance of) tok2 from clist and start loop again
		}
		
		* Prepare variable itself (possibly followed with label in quotes)
		else {
			if `qed2' == 1 {			// quotes around second element ==> var followed by "Label"
				gettoken lhs rest : clist, bind
				gettoken rhs clist : rest, bind
			}
			else {						// var not followed by "Label"
				gettoken lhs clist : clist, bind
			}
			
			* Test whether `lhs' is a possible Stata variable name
			* If it is, assume "collapse"; if not, assume "returned statistic"
			local 0 `"`lhs'"'
			capture syntax newvarname(fv ts)
			if _rc {												// assume "returned statistic"
				local ++nr
				local rstatlist `"`rstatlist' `lhs'"'				// add expression "as-is" to overall ordered list
				if `"`rhs'"' != `""' {
					return local rvarlab`nr'=trim(`"`rhs'"')		// return varlab
					local rhs
				}
				if `"`newname'"'==`""' GetNewname newname : `"`lhs'"' `"`newnames'"'
				else if `"`: list newnames & newname'"' != `""' {
					disp as err "name conflict in lcols/rcols option"
					exit 198
				}
				local sidelist `"`sidelist' `rcols'"'				// add to (overall, ordered) list of "sides" (l/r)
				local newnames `"`newnames' `newname'"'				// add to (overall, ordered) list of newnames
				local itypes `"`itypes' r"'							// add to (overall, ordered) list of "item types"
				local newfmts `"`newfmts' `fmt'"'					// add to (overall, ordered) list of formats
			}
			
			* If "collapse", convert "ipdmetan"-style clist into "collapse"-style clist
			else {
				capture syntax varname(fv ts)	// this time test if it's an *existing* variable
				if _rc {
					if `"`byagg'"'!=`""' {		// if "byagg", non-existing variables are permissible
						local ++na
						if `"`newname'"'==`""' GetNewname newname : `"`lhs'"' `"`newnames'"'
						else if `"`: list newnames & newname'"' != `""' {
							disp as err "name conflict in lcols/rcols option"
							exit 198
						}
						local sidelist `"`sidelist' `rcols'"'		// add to (overall, ordered) list of "sides" (l/r)
						local newnames "`newnames' `newname'"		// add to (overall, ordered) list of newnames
						local itypes `"`itypes' a"'					// add to (overall, ordered) list of "item types"
						local newfmts `"`newfmts' `fmt'"'			// add to (overall, ordered) list of formats
						if `"`rhs'"' != `""' {
							return local avarlab`na'=trim(`"`rhs'"')	// return varlab
						}
					}
					else {
						disp as err "variable `lhs' not found"
						exit _rc
					}
				}
				else {
					tsrevar `varlist'
					local oldname `r(varlist)'
					
					* Sort out string vars
					capture confirm string var `oldname'
					if !_rc {
						local ++ncs
						if `"`newname'"'==`""' GetNewname newname : `"`oldname'"' `"`newnames'"'
						else if `"`: list newnames & newname'"' != `""' {
							disp as err "name conflict in lcols/rcols option"
							exit 198
						}
						local sidelist `"`sidelist' `rcols'"'		// add to (overall, ordered) list of "sides" (l/r)
						local newnames "`newnames' `oldname'"		// add to (overall, ordered) list of newnames
						local itypes `"`itypes' cs"'				// add to (overall, ordered) list of "item types"
						local newfmts `"`newfmts' null"'			// add to (overall, ordered) list of formats
						if `"`rhs'"' != `""' {
							return local csvarlab`ncs'=trim(`"`rhs'"')	// return varlab
						}
					}
					
					* Build "clist" expression for -collapse-
					else {
						local ++nc
						if `"`stat'"'==`"null"' {
							local stat "mean"				// otherwise default to "mean"
						}
						local keep `"`keep' `oldname'"'
						if `"`rhs'"' != `""' {
							return local cvarlab`nc'=trim(`"`rhs'"')		// return varlab
							local rhs
						}
						local stat=subinstr(`"`stat'"',`" "',`""',.)		// remove spaces from stat (e.g. p 50 --> p50)
						
						if `"`newname'"'==`""' GetNewname newname : `"`oldname'"' `"`newnames'"'
						else if `"`: list newnames & newname'"' != `""' {
							disp as err "name conflict in lcols/rcols option"
							exit 198
						}					
						if trim(`"`fmt'"')==`"null"' {
							local fmt : format `oldname'					// use format of original var if none specified
						}
						local sidelist `"`sidelist' `rcols'"'				// add to (overall, ordered) list of "sides" (l/r)
						local newnames `"`newnames' `newname'"'				// add to (overall, ordered) list of newnames
						local itypes `"`itypes' c"'							// add to (overall, ordered) list of "item types"
						local newfmts `"`newfmts' `fmt'"'					// add to (overall, ordered) list of formats

						local cclist `"`cclist' (`stat') `newname'=`oldname'"'		// add to "collapse" clist

					}		// end  if !_rc (i.e. is `oldname' string or numeric)
				}		// end else (i.e. if `lhs' found in data currently in memory)
			}		// end else (i.e. if "collapse")

		local fmt = "null"
		local newname
		}		// end else (i.e. "parse variable itself")
		
	}		// end "while" loop
	
	
	
	* Check length of macro lists
	local nnewnames : word count `newnames'
	local nitypes : word count `itypes'
	local nsidelist : word count `sidelist'
	assert `nnewnames' == `nitypes'						// check newnames & itypes equal
	assert `nnewnames' == `nsidelist'					// check newnames & sidelist equal
	assert `nnewnames' == `na' + `nc' + `ncs' + `nr'	// ... and equal to total number of "items"
	
	if `fmtnotnull' {
		local nfmts : word count `newfmts'
		assert `nfmts' == `nnewnames'		// check fmts also equal, if appropriate
	}
	
	* Return macros & scalars
	return local newnames=trim(itrim(`"`newnames'"'))		// overall ordered list of newnames
	return local itypes=trim(itrim(`"`itypes'"'))			// overall ordered list of "item types"
	return local sidelist=trim(itrim(`"`sidelist'"'))		// overall ordered list of "sides" (l/r)
	if `fmtnotnull' {
		return local fmts=trim(itrim(`"`newfmts'"'))		// overall ordered list of formats (if any specified)
	}
	if `nc'>0 {
		return local cclist=trim(itrim(`"`cclist'"'))		// "collapse" clist
	}
	if `nr'>0 {
		return local rstatlist=trim(itrim(`"`rstatlist'"'))	// list of returned stats "as is"
	}
	return scalar na=`na'									// number of vars not in IPD
	return scalar nc=`nc'									// number of "collapse" items
	return scalar nr=`nr'									// number of "returned stat" items
	return scalar ncs=`ncs'									// number of string "collapse" items

end


* The following subroutine has a similar name and function to GetNewnameEq in the official "collapse.ado"
*  but has been re-written by David Fisher, Aug 2013
program GetNewname
	args mnewname colon oldname namelist
	
	local newname=strtoname(`"`oldname'"')		// matrix colname (valid Stata varname)
				
	* Adjust newname if duplicates
	if `"`: list namelist & newname'"' != `""' {
		local j=2
		local newnewname `"`newname'"'
		while `"`: list namelist & newnewname'"' != `""' {
			local newnewname `"`newname'_`j'"'
			local ++j
		}
		local newname `"`newnewname'"'
	}
	
	c_local `mnewname' `"`newname'"'
end
				

* The following subroutine is taken directly from the official "collapse.ado"
program GetOpStat 
	args mstat mrest colon stat line

	gettoken thing nline : line, parse("() ") match(parens)
	
	* If "thing" is not in parentheses, update locals and return to main loop
	if "`parens'"=="" {
		c_local `mstat' "`stat'"
		c_local `mrest' `"`line'"'
		exit
	}
	* Otherwise, "thing" should consist of a single "stat" word in parentheses (or program will exit with error)
	if `:word count `thing'' == 1 {
		local 0 `", `thing'"'
		capture syntax [, mean median sd SEMean SEBinomial SEPoisson ///
			sum rawsum count max min iqr first firstnm last lastnm null]
		
		/* fix thing if abbreviated */
		if "`semean'" != "" {
			local thing "semean"
		}
		if "`sebinomial'" != "" {
			local thing "sebinomial"
		} 
		if "`sepoisson'" != "" {
			local thing "sepoisson"
		}
		/* If syntax executed without error, simply update locals and exit */
		if _rc == 0 {
			c_local `mstat' `thing'
			c_local `mrest' `"`nline'"'
			if ("`median'"!="") c_local `mstat' "p 50"
			exit
		}
		/* Otherwise, assume we've got a percentile stat and act accordingly */
		local thing = trim("`thing'")
		if (substr("`thing'",1,1) == "p") {
			local thing = substr("`thing'",2,.)
			capture confirm integer number `thing'
			if _rc==0 { 
				if 1<=`thing' & `thing'<=99 {
					c_local `mstat' "p `thing'"
					c_local `mrest' `"`nline'"'
					exit
				}
			}
		}
	}
	di as err "(`thing') invalid statistic"
	exit 198
end






* IPD analysis subroutine
* (for processing coefficient names for IPD loop)
prog define ipd_prog, rclass

	syntax, RESULT(integer)				///
			[INTERACTION				///
			POOLVAR(string)				///
			ESTVAR(string)				///
			TERM1(string) TERM2(string) ///
			MESSAGES					///
			ZTOL(real 1e-6)				///
		]

	* Get list of coefficients from e(b)
	mat B = e(b)
	local clist : colfullnames B
	local clistlen: word count `clist'

	* If estvar not found yet (e.g. first trial), then identify it.
	* Otherwise, check using previously identified names.
	* This ensures that we are comparing like-with-like.
	if `"`estvar'"'==`""' {
	
		* If poolvar supplied, try to match it with a coefficient from e(b)
		if `"`poolvar'"'!=`""' {
			
			local regexp "^(([_a-zA-Z][_a-zA-Z0-9]*):)?([0-9co]+\.)?([_a-zA-Z][_a-zA-Z0-9]*)(#([0-9co]+\.)?([_a-zA-Z][_a-zA-Z0-9]*))?$"
			if !regexm("`poolvar'", "`regexp'") {
				disp as err "poolvar() is not in a recognised format; please check"
				exit 198
			}
			else {
				capture local regexs5=regexs(5)
				if `"`regexs5'"'!=`""' & `"`interaction'"'==`""' {
					disp as err "poolvar() represents an interaction; please specify option 'interaction'"
					exit 198
				}
				
				* First, try matching directly
				local j=0
				local clistj
				while `j'<=`clistlen' & "`clistj'"!="`poolvar'" {
					local ++j
					local clistj: word `j' of `clist'
				}
				if `"`clistj'"'!=`""' local estvar "`clistj'"
				else {
					
					* Next, try matching just varnames without adornments (b., c., 1. etc)
					local var1=regexs(4)
					capture local var2=regexs(7)		// may not exist
					local regexp "^(([_a-zA-Z][_a-zA-Z0-9]*):)?([0-9co]+\.)?`var1'(#[0-9co]+\.`var2')?$"
					local j=0
					local clistj
					while `j'<=`clistlen' & !regexm("`clistj'","`regexp'") {
						local ++j
						local clistj: word `j' of `clist'
					}
					if `"`clistj'"'==`""' {
						if `result'!=0 & `"`messages'"'!=`""' disp as err "No valid coefficent found"
						local result=0
					}
					else local estvar "`clistj'"
				}
			}
		}
		
		* Main effect
		else if `"`interaction'"'==`""' {
			
			* Find first "non-base" coefficient within clist (include equation names)
			local j=0
			local clistj
			local regexp "^(([_a-zA-Z][_a-zA-Z0-9]*):)?([0-9co+\.)?[_a-zA-Z][_a-zA-Z0-9]*)$"
			while `j'<=`clistlen' & !regexm("`clistj'","`regexp'") {
				local ++j
				local clistj: word `j' of `clist'
			}
			if `"`clistj'"'==`""' {
				if `result'!=0 & `"`messages'"'!=`""' disp as err "No valid coefficent found"
				local result=0
			}
			else local estvar `clistj'
		}
		
		* Interactions
		else {

			* Find first interaction coefficient within clist using regular expressions
			* Assume "fvvarlist" syntax is used.
			* (Also assume first valid interaction coefficient is the one we want.  User must order vars appropriately.)
			* (REVISIT - Assumes no "equations" -- do any multiple-equation commands permit interactions/factor variables??)
			* (Save this for a later version!)
			local j=0
			local clistj
			local regexp "^([0-9co]+\.[_a-zA-Z][_a-zA-Z0-9]*)#([0-9co]+\.[_a-zA-Z][_a-zA-Z0-9]*)$"
			while `j'<=`clistlen' & !regexm("`clistj'","`regexp'") {
				local ++j
				local clistj: word `j' of `clist'
			}
			if `"`clistj'"'==`""' {
				if `result'!=0 & `"`messages'"'!=`""' disp as err "No valid interaction coefficent found"
				local result=0
			}
			else {
				local estvar `clistj'
				local term1=regexs(1)
				local term2=regexs(2)
			}
			
			* Check that there are no other interactions involving these variables (either treatment or covariate)
			forvalues i=1/2 {	// first, extract "variable names" from full names
				if regexm("`term`i''", "[0-9c]+\.([_a-zA-Z][_a-zA-Z0-9]*)$") local var`i' = regexs(1)
			}
			local clist2 : list clist - estvar
			forvalues j=1/`=`clistlen'-1' {
				local clistj: word `j' of `clist2'
				if regexm("`clistj'","^[0-9c]+\.(`var1'|`var2')#[0-9c]+\.[_a-zA-Z][_a-zA-Z0-9]*$") ///
					| regexm("`clistj'","^[0-9c]+\.[_a-zA-Z][_a-zA-Z0-9]*#[0-9c]+\.(`var1'|`var2')$") {
					disp as err "Multiple interactions involving treatment or covariate of interest"
					disp as err "Please check syntax, or specify estimate to pool using 'exp_list' or poolvar()"
					exit 198
				}
			}
		}		// end if `"`interaction'"'!=`""'
	}			// end if `"`estvar'"'==`""'
	
	else if !`: list estvar in clist' {
		if `result'!=0 & `"`messages'"'!=`""' disp as err "Removed due to poor model fit"
		local result=0
	}
			
	* Check if coefficient can be estimated in this study
	if `result'==1 {
		if _b[`estvar']==0 | _se[`estvar']==0 | abs(_b[`estvar']/_se[`estvar'])<`ztol' {
			if `result'!=0 & `"`messages'"'!=`""' disp as err "Removed due to poor model fit"
			local result=0
		}
		
		* For interactions, check that constituent vars have fitted main effects too
		* These should come earlier in the list of estimates, so only need to check up to interaction itself within clist
		if `"`interaction'"'!=`""' {
			foreach x in term1 term2 {
				
				* If continuous ("c."), strip the c. off, since main effect var will not have that prefix
				if regexm("``x''","^c\.([_a-zA-Z][_a-zA-Z0-9]*)$") local `x' = regexs(1)
								
				if !`: list `x' in clist' {
					if `result'!=0 & `"`messages'"'!=`""' disp as err "Removed due to collinearity within interaction model"
					local result=0
				}
				else {
					if _b[``x'']==0 | _se[``x'']==0 | abs(_b[``x'']/_se[``x''])<`ztol' {
						if `result'!=0 & `"`messages'"'!=`""' disp as err "Removed due to poor model fit"
						local result=0
					}
				}
			}
		}		// end if `"`interaction'"'!=`""'
	}			// end if `result'==1
	
	return scalar result=`result'
	return local estvar `"`estvar'"'
	return local term1 `"`term1'"'
	return local term2 `"`term2'"'

end






** Aggregate data subroutine
prog define agg_prog, rclass

		syntax [if] [in], [ZTOL(real 1e-6) SMAX(integer 0) MAXBy(integer 0) ///
					AGGFILE(string asis) AGGONLY VARS(namelist min=2 max=3 local) AGGNPTS(name local) ///
					EXTRACOLS(namelist local) ///
					STUDY(name local) STUDYLAB(name local) STUDYLABFile(string asis) ///
					BYAGG BY(name local) BYLAB(name local) BYLABFile(string asis) BYISSTR(integer 0) ///
					EXTRAFile(string asis) SORTINDEX(name local)]

		preserve
		
			if `"`aggfile'"'!=`""' {
				use `"`aggfile'"', clear 			// load external data
			}
			marksample touse						// touse marking specific to this subroutine
			tempvar obs
			qui gen int `obs' = _n

			local nostudy=0
			capture confirm var `study'
			if _rc {
				if `"`aggonly'"'!=`""' & `"`study'"'!=`""' {
					disp as err `"variable `study' not found (PLEASE DEBUG)"'	// should this ever actually happen??
					exit 111
				}
				local nostudy=1
			}
			qui do `studylabfile'						// load trial label
			* N.B. only way `study' cannot be present is if in external file (to combine with IPD)
			* in which case, ignore it and proceed with agg_id_new
			* also in which case, variable `study' should exist in current memory at least
			
			capture confirm var `by'
			local byexist = (_rc==0)
			if `byexist' {
				if `"`aggfile'"'==`""' {			// If agg data in memory (aggmetan)
					local by2 `by'					// then `by' has already been sorted out
				}
				capture do `bylabfile'				// load subgroup (`by') label (if exists)
			}
			
			** External aggregate data (to combine with IPD)
			if `"`aggfile'"'!=`""' {
				tempvar agg_id_new
				qui bysort `touse' (`obs') : gen int `agg_id_new' = _n + `smax' if `touse'	// Generate sequential trial ID nos.
																							// following on from those of IPD trials
				qui count if `touse'
				local ni=r(N)
				gsort -`touse' `obs'		// put `touse'==1 first
				
				* Add aggregate data studies to value label
				if `"`studylab'"'!=`""' {
					if `nostudy' {					// if `study' not supplied, create dummy label using `add_id_new' values
						forvalues i=1/`ni' {
							local agg_newlisti = `agg_id_new'[`i']
							label define `studylab' `agg_newlisti' `"`agg_newlisti'"', add
						}
					}
					else {
						capture confirm numeric variable `study'
						forvalues i=1/`ni' {
							local agg_oldlisti = `study'[`i']
							local agg_newlisti = `agg_id_new'[`i']
							if !_rc {								// `study' is numeric
								local aggname : label (`study') `agg_oldlisti'
								label define `studylab' `agg_newlisti' `"`aggname'"', add
							}
							else {									// `study' is string
								label define `studylab' `agg_newlisti' `"`agg_oldlisti'"', add
							}
						}
					}
				}
	
				* Same for subgroup value label (if applicable)
				* Here, if `by' is string, map onto the existing `bylab' using -encode-
				if `byexist' {
					capture confirm numeric var `by'
					if !_rc {								// `by' is numeric
						if `byisstr' {
							disp as err `"Variable `by' is string in IPD dataset but numeric in aggregate dataset"'
							exit 198
						}
						tempvar bygroup
						qui bysort `touse' `by' : gen long `bygroup' = (_n==1)*`touse'
						qui replace `bygroup' = sum(`bygroup')
						
						summ `bygroup' if `touse', meanonly
						local nby = `bygroup'[_N]
						forvalues i=1/`nby' {
							summ `by' if `bygroup'==`i' & `touse', meanonly
							local bylabi : label (`by') `r(min)'
							local bylabi_test : label `bylab' `r(min)'
							if `"`bylabi_test'"'!=`""' & `"`bylabi_test'"'!=`"`bylabi'"' {
								disp as err `"Subgroup value label conflict at value `r(min)'"'
								exit 198
							}
							label define `bylab' `r(min)' "`bylabi'", modify
							local bylist `"`bylist' `r(min)'"'
						}
						local by2 `by'
					}
					else {									// `by' is string
						if !`byisstr' {
							disp as err `"Variable `by' is numeric in IPD dataset but string in aggregate dataset"'
							exit 198
						}
						tempvar by2
						encode `by', gen(`by2') label(`bylab')
						qui levelsof `by2', local(bylist)
					}
					sort `obs'
					
					* Re-save bylab
					qui label save `bylab' using `bylabfile', replace
					return local bylist `"`bylist'"'
				}
				
				else if `"`byagg'"'!=`""' {			// if byagg, create dummy var ( = 2 throughout)
					gen byte `by'=2 if `touse'		// using the tempname stored in `by'
					local by2 `by'
				}
				
				else if `"`by'"'!=`""' {			// if (aggregate) `by' does not exist but is needed
					gen byte `by' = `maxby'+1 if `touse'
					label define `bylab' `=`maxby'+1' "Aggregate", add
					qui label save `bylab' using `bylabfile', replace
					return local bylist `"`=`maxby'+1'"'
					local by2 `by'
				}

				* Re-save studylab
				qui label save `studylab' using `studylabfile', replace
			
			}	// end else (that is, if `"`aggfile'"'!=`""')
			
			else local agg_id_new `study'		// if no IPD, just point agg_id_new to existing study var
			
			
			** Parse `vars' namelist
			* Syntax is vars(ES seES) or vars(ES lci uci); npts(npts) study(study)
			* whether data is external or in memory
			local 0 `"`vars'"'
			syntax varlist(min=2 max=3 numeric)		// confirm that varnames are valid
			local nvars : word count `vars'
			tokenize `vars'
			local aggES `1'
			if `nvars' == 2 local aggse `2'		// 2 vars ==> "ES seES"
			else if `nvars' == 3 {				// 3 vars ==> "ES lci uci"
				local agglci `2'
				local agguci `3'
			}
			else {
				disp as err "Error in specification of aggregate-data effect sizes"
				exit 198
			}
			
			* Derive standard error from confidence limits if appropriate
			if `"`aggse'"' == `""' {
				capture assert `agglci'<=`aggES' if `agglci'<. & `touse'
				if _rc!=0 {
					disp as err "Third variable assumed to contain lower confidence limit; error"
					exit 198
				}
				capture assert `agguci'>=`aggES' if `agguci'<. & `touse'
				if _rc!=0 {
					disp as err "Fourth variable assumed to contain upper confidence limit; error"
					exit 198
				}
				tempvar aggse
				qui gen `aggse'=(`agguci'-`agglci')/(2*invnorm(.5 + `c(level)'/200)) if `touse'
			}
			
			* Identify "bad" estimates and replace with missing if necessary
			qui replace `aggES'=. if `aggES'==. | `aggse'==. | `aggse'==0 | (abs(`aggES'/`aggse')>0 & abs(`aggES'/`aggse')<`ztol') & `touse'
			qui replace `aggse'=. if `aggES'==. | `aggse'==. | `aggse'==0 | (abs(`aggES'/`aggse')>0 & abs(`aggES'/`aggse')<`ztol') & `touse'

			* colnames - this must be done here in case there is no `sgmat1'
			local colnames "study ES seES"
			if `"`by2'"'!=`""' local colnames `"by `colnames'"'
			if `"`aggnpts'"'!=`""' {
				confirm numeric var `aggnpts'
				local colnames `"`colnames' NN"'
			}
			
			* Create matrix of estimates (named `sgmat2')
			tempname aggmat
			mkmat `by2' `agg_id_new' `aggES' `aggse' `aggnpts' if `touse', matrix(`aggmat')		
			mat colnames `aggmat' = `colnames'
			return matrix aggmat=`aggmat'		
			
			* Save "extra data" (for lcols/rcols) in datafile
			if trim(itrim(`"`extracols'"'))!=`""' {
				qui ds
				local allvars=r(varlist)
				local allvars : list allvars & extracols		// lcols/rcols that appear within aggregate data	
				if `"`allvars'"' != `""' {
					qui keep if `touse'
					keep `by2' `agg_id_new' `allvars'
					* capture rename `agg_id_new' `study'			// "capture" since names might already be the same
					rename `agg_id_new' _study
					qui save `extrafile'
					return local aggextra `"`allvars'"'
				}
			}
			
		restore
		
		qui do `studylabfile'						// (re)load IPD trial value label
		if `"`bylab'"'!=`""' qui do `bylabfile'		// (re)load subgroup value label
		
end





***********************************************



********************
* Mata subroutines *
********************


/* Remove or mark "excluded" studies (i.e. rows with no effect size and/or SE) */
mata:
void ExcludeStudy(string scalar st_coeffs, string scalar keepall)
{
	coeffs=st_matrix(st_coeffs)
	colnames=st_matrixcolstripe(st_coeffs)[.,2]
	colsumvec=1::rows(colnames)
	EScol=select(colsumvec, colnames:=="ES")
	seEScol=select(colsumvec, colnames:=="seES")
	
	keep=rowmissing(coeffs[,EScol..seEScol]):==0
	if (keepall=="") {
		coeffs=select(coeffs, keep)		// default: remove excluded studies, do not add column
	}
	else {
		coeffs=coeffs, keep				// if "keepall" specificed: mark excluded studies with extra column
	}
	st_matrix(st_coeffs,coeffs)
}

end


/* Meta-analysis pooling and heterogeneity statistics */
* References:
* Mittlboeck & Heinzl, Stat. Med. 2006; 25: 4321–33 "A simulation study comparing properties of heterogeneity measures in meta-analyses"
* Higgins & Thompson, Stat. Med. 2002; 21: 1539–58 "Quantifying heterogeneity in a meta-analysis"
mata:
void GetEstimates(string scalar st_allmat, string scalar keepall, string scalar st_wtvec, string scalar st_rowvec, /*
	*/ string scalar model, | string scalar trunc, real scalar i, real scalar j)
{
	allmat=st_matrix(st_allmat)
	rowvec=1::rows(allmat)
	colnames=st_matrixcolstripe(st_allmat)[.,2]
	colsumvec=1::rows(colnames)
	
	selectvec=J(rows(allmat),1,1)
	if (i!=.) {
		if (any(colnames:=="by")==0) i=.
		else selectvec = selectvec :* allmat[, select(colsumvec, colnames:=="by")]:==i
	}
	if (j!=.) {
		if (any(colnames:=="over")==0) j=.
		else selectvec = selectvec :* allmat[, select(colsumvec, colnames:=="over")]:==j
	}
	if (!(i==. & j==.)) {
		rowvec=select(rowvec, selectvec)
		allmat=select(allmat, selectvec)		
	}

	yi=allmat[, select(colsumvec, colnames:=="ES")]
	se=allmat[, select(colsumvec, colnames:=="seES")]
	vi=se:^2
	wi=1:/vi
		
	if (keepall=="") keep=J(rows(allmat),1,1)							// all trials "kept"
	else keep=allmat[, select(colsumvec, colnames:=="keep")]
	
	totnpts=sum(allmat[, select(colsumvec, colnames:=="NN")] :* keep)	// total pts (included trials only)
	k=sum(keep)															// n included trials (df=k-1)
	
	fe_mu_hat=sum(wi:*yi)/sum(wi)			// fixed-effects estimate
	se_fe_mu_hat=1/sqrt(sum(wi))			// SE of fixed-effects estimate
	Q=sum(wi:*((yi:-fe_mu_hat):^2))			// standard Q statistic
	
	/* Now find tau-squared */
	if (model=="fe" | model=="dl") {
		tausq=(Q-(k-1))/(sum(wi)-(sum(wi:^2)/sum(wi)))
		if (trunc=="") {
			tausq=max((0,tausq))
		}
	}
	else if (model=="vb") {
		tausq_vb=itausq(yi, vi, k, 50, 1e-10, 0.05, trunc)
		tausq=tausq_vb[1]
		tsq_lci=tausq_vb[2]
		tsq_uci=tausq_vb[3]
	}
	else if (model=="bs") {
		tausq_bs=biggerstaff(yi, vi, k, 50, 1e-10, 0.05, trunc)
		tausq=tausq_bs[1]
		tsq_var=tausq_bs[2]
		tsq_lci=tausq_bs[3]
		tsq_uci=tausq_bs[4]
	}
	/*
	else if (model=="reml") {
		tausq_reml=reml2s(yi, vi, k, 1e-10, 50, 50, 0.05)
		tausq=tausq_reml[1]
		tsq_lci=tausq_reml[3]
		tsq_uci=tausq_reml[4]
	}
	*/
	else {
		printf("Invalid type")
	}

	/* Calculate s-squared */
	/* (N.B. all other heterogeneity stats can be calculated from tausq, tausq CI, and sigmasq */
	/*       so no need to do it within Mata */
	sigmasq=(k-1)*sum(wi)/((sum(wi)^2)+sum(wi:^2))		// "typical" within-study variance (cf Mittlboeck)
	
	/* If not fixed-effects, use tau-squared to calculate weights */
	if (model!="fe" & tausq>0) {				// regardless of trunc/notrunc, only use tau-squared if >0
		wi=1:/(vi:+tausq)
		mu_hat=sum(wi:*yi)/sum(wi)				// random-effects estimate
		se_mu_hat=1/sqrt(sum(wi))				// SE of random-effects estimate
	}
	else {
		mu_hat=fe_mu_hat
		se_mu_hat=se_fe_mu_hat
	}
	
	/* Output matrix plus weights, and summary stats */
	wi=editmissing(wi,0)				// replace missing weights with zeroes (for matrix manipulation in Stata)
	wi=wi/sum(wi)						// normalise weights

	st_matrix(st_wtvec, wi)
	if (st_rowvec!="") st_matrix(st_rowvec, rowvec)
	st_numscalar("mu_hat", mu_hat)
	st_numscalar("se_mu_hat", se_mu_hat)
	st_numscalar("Q", Q)
	st_numscalar("k", k)
	st_numscalar("totnpts", totnpts)
	st_numscalar("tausq", tausq)
	st_numscalar("sigmasq", sigmasq)
	if (model=="vb" | model=="bs") {
		st_numscalar("tsq_lci", tsq_lci)
		st_numscalar("tsq_uci", tsq_uci)
	}
	if (model=="bs") {
		st_numscalar("tsq_var", tsq_var)
	}
}

end



* Program to calculate iterative tau-squared plus confidence interval using Generalised Q

* based on papers by Viechtbauer
* Stat. Med. 2007; 26: 37–52  "Confidence intervals for the amount of heterogeneity in meta-analysis"

* and by DerSimonian & Kacker
* Contemporary Clinical Trials 2007; 28: 105-114 "Random-effects model for meta-analysis of clinical trials: An update"

mata:
function Q_crit(tau2val, yi, vi, crit) {
	wi=1:/(vi:+tau2val)
	mu_hat=sum(wi:*yi)/sum(wi)
	Q=sum(wi:*((yi:-mu_hat):^2))
	return(Q-crit)
}
real rowvector itausq(real colvector yi, real colvector vi, real scalar k, real scalar maxtausq, real scalar tol, real scalar alpha, string scalar trunc) {
	wi=1:/vi
	mu_hat=sum(wi:*yi)/sum(wi)

	/* Find Mandel-Paule estimate */
	/* Paule RC, Mandel J. Consensus values and weighting factors. J Res Natl Bur Stand 1982;87:377–85 */
	/* referenced in DerSimonian & Kacker (2007) */
	rc1=mm_root(tausq_mp=., &Q_crit(), 0, maxtausq, tol, 1000, yi, vi, k-1)
	if (rc1==1) {
		printf("Mandel-Paule estimate: Convergence not reached within 1000 iterations; using current approximation\n")
	}
	else if (rc1==2) {
		if (trunc=="") {
			printf("Mandel-Paule estimate: No solution between specified limits; estimate set to zero\n")
		}
		else {
			printf("Mandel-Paule estimate: solution less than zero; using incremental search\n")
			step=0.00000001
			t=0
			sign=sign(Q_crit(0, yi, vi, k-1))
			do {
				t=t-step
				error=Q_crit(t, yi, vi, k-1)
			} while(error*sign>0)
			tausq_mp=t
		}
	}	
	else if (rc1==3) {
		printf("Mandel-Paule estimate: No solution between specified limits; estimate set to upper bound\n")
	}	

	Q=sum(wi:*((yi:-mu_hat):^2))
	Q_crit_hi=invchi2(k-1, 1-(alpha/2))		// higher critical value to compare Q against (for *lower* bound of tausq)
	Q_crit_lo=invchi2(k-1, alpha/2)			// lower critical value to compare Q against (for *upper* bound of tausq)
	
	if (Q<Q_crit_lo & trunc=="") {			// Q falls below the lower critical value
		printf("Upper and lower bounds are below 0. Therefore, the CI is equal to the null set.\n")
		tausq_lb=0
		tausq_ub=0
	}
	else {
		if (Q>Q_crit_hi) {		// Q is larger than the higher critical value, so can find lower bound using mm_root
			rc1_lb=mm_root(tausq_lb=., &Q_crit(), 0, maxtausq, tol, 1000, yi, vi, Q_crit_hi)
			if (rc1_lb==1) {
				printf("Lower bound: Convergence not reached within 1000 iterations; using current approximation\n")
			}
			else if (rc1_lb==2) {
				if (trunc=="") {
					printf("Lower bound: No solution between specified limits; estimate set to zero\n")
				}
				else {
					// If lower bound of tausq is below zero,
					// use a basic "crawler" algorithm to search backwards from zero in incremental steps until the root is found.
					printf("Lower bound: solution less than zero; using incremental search\n")
					step=0.00000001
					t=0
					sign=sign(Q_crit(0, yi, vi, Q_crit_hi))
					do {
						t=t-step
						error=Q_crit(t, yi, vi, Q_crit_hi)
					} while(error*sign>0)
					tausq_lb=t
				}
			}		
			else if (rc1_lb==3) {
				printf("Lower bound: No solution between specified limits; estimate set to upper bound\n")
			}
		}
		else {
			if (trunc=="") {
				tausq_lb=0			// otherwise, the lower bound for tausq is 0
			}
			else {
				printf("Lower bound: solution less than zero; using incremental search\n")
				step=0.00000001
				t=0
				sign=sign(Q_crit(0, yi, vi, Q_crit_hi))
				do {
					t=t-step
					error=Q_crit(t, yi, vi, Q_crit_hi)
				} while(error*sign>0)
				tausq_lb=t
			}
		}

		/* Now find upper bound for tausq using mm_root */
		rc1_ub=mm_root(tausq_ub=., &Q_crit(), tausq_lb, maxtausq, tol, 1000, yi, vi, Q_crit_lo)
		if (rc1_ub==1) {
			printf("Upper bound: Convergence not reached within 1000 iterations; using current approximation\n")
		}
		else if (rc1_ub==2) {
			if (trunc=="") {
				printf("Upper bound: No solution between specified limits; estimate set to zero\n")
			}
			else {
				printf("Upper bound: solution less than zero; using incremental search\n")
				step=0.00000001
				t=0
				sign=sign(Q_crit(tausq_lb, yi, vi, Q_crit_lo))
				do {
					t=t-step
					error=Q_crit(t, yi, vi, Q_crit_lo)
				} while(error*sign>0)
				tausq_ub=t
			}	
		}	
		else if (rc1_ub==3) {
			printf("Upper bound: No solution between specified limits; estimate set to default upper bound\n")
		}
	}

	return(tausq_mp, tausq_lb, tausq_ub)
}

end



* Program to calculate confidence interval for D&L tau-squared using approximate Gamma distribution for Q

* based on paper by Biggerstaff and Tweedie
* Stat. Med. 1997; 16: 753–68  "Incorporating variability in estimates of heterogeneity in the random effects model in meta-analysis"

mata:
function gamma_crit(tau2val, tausq_m, k, c, S1, S2, S3, crit) {
	lambda=((k-1) + c*tau2val)/(2*(k-1) + 4*c*tau2val + 2*(S2 - 2*(S3/S1) + ((S2^2)/(S1^2)))*(tau2val^2))
	r=((k-1) + c*tau2val)*lambda
	limit=lambda*(c*tausq_m + (k-1))
	return(gammap(r,limit)-crit)
}
real rowvector biggerstaff(real colvector yi, real colvector vi, real scalar k, real scalar maxtausq, real scalar tol, real scalar alpha, string scalar trunc) {
	wi=1:/vi
	mu_hat=sum(wi:*yi)/sum(wi)
	Q=sum(wi:*((yi:-mu_hat):^2))
	
	S1=sum(wi)
	S2=sum(wi:^2)
	S3=sum(wi:^3)
	c=S1-(S2/S1)
	tausq_dl=(Q-(k-1))/c
	if (trunc=="") {
		tausq_dl=max((0,tausq_dl))
	}

	/* Find lower root */
	rc1_lb=mm_root(tausq_lb=., &gamma_crit(), 0, maxtausq, tol, 1000, tausq_dl, k, c, S1, S2, S3, 1-(alpha/2))
	if (rc1_lb==1) {
		printf("Lower bound: Convergence not reached within 1000 iterations; using current approximation\n")
	}
	else if (rc1_lb==2) {
		if (trunc=="") {
			printf("Lower bound: No solution between specified limits; estimate set to zero\n")
		}
		else {
			printf("Lower bound: solution less than zero; using incremental search\n")
			step=0.00000001
			t=0
			sign=sign(gamma_crit(0, tausq_dl, k, c, S1, S2, S3, 1-(alpha/2)))
			do {
				t=t-step
				error=gamma_crit(t, tausq_dl, k, c, S1, S2, S3, 1-(alpha/2))
			} while(error*sign>0)
			tausq_lb=t
		}
	}	
	else if (rc1_lb==3) {
		printf("Lower bound: No solution between specified limits; estimate set to upper bound\n")
	}

	/* Find upper root */
	rc1_ub=mm_root(tausq_ub=., &gamma_crit(), tausq_lb, maxtausq, tol, 1000, tausq_dl, k, c, S1, S2, S3, alpha/2)
	if (rc1_ub==1) {
		printf("Upper bound: Convergence not reached within 1000 iterations; using current approximation\n")
	}
	else if (rc1_ub==2) {
		if (trunc=="") {
			printf("Upper bound: No solution between specified limits; estimate set to zero\n")
		}
		else {
			printf("Upper bound: solution less than zero; using incremental search\n")
			step=0.00000001
			t=0
			sign=sign(gamma_crit(tausq_lb, tausq_dl, k, c, S1, S2, S3, alpha/2))
			do {
				t=t-step
				error=gamma_crit(t, tausq_dl, k, c, S1, S2, S3, alpha/2)
			} while(error*sign>0)
			tausq_lb=t
		}
	}	
	else if (rc1_ub==3) {
		printf("Upper bound: No solution between specified limits; estimate set to upper bound\n")
	}
	
	/* Find variance of tausq */
	Q_var=2*(k-1) + 4*c*tausq_dl + 2*(S2 - 2*(S3/S1) + ((S2^2)/(S1^2)))*(tausq_dl^2)
	tausq_var=Q_var/((S1 - (S2/S1))^2)

	return(tausq_dl, tausq_var, tausq_lb, tausq_ub)
}

end



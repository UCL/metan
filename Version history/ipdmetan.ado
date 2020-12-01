* Meta-analysis of main effects or interactions

* Originally written by David Fisher, July 2008

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
*   After discussion with Ross Harris, improved ability to analyse aggregate data alone using separate program -admetan-
*   together with some rearrangement of syntax, options & naming conventions

* September 2013
*   Following UK Stata Users Meeting, reworked the plotid() option as recommended by Vince Wiggins

*! version 1.0  David Fisher  31jan2014


program ipdmetan, rclass sortpreserve

	version 11.0
	local version : di "version " string(_caller()) ":"
	* NOTE: mata requires v9.x
	* factor variable syntax requires 11.0

	// <ipdmetan_stuff> : <command>
	_on_colon_parse `0'

	local command `"`s(after)'"'
	local 0 `"`s(before)'"'
	
	* Parse ipdmetan options (before colon)
	syntax [anything(name=exp_list equalok)] , [ ///	
		/// IPD options
		Study(string) OVER(string)	/// to be parsed later; over() for error-trapping only
		SORTBY(string)				/// optional sorting varlist
		INTERaction					///
		MEssages					///
		KEEPALL						///
		POOLvar(string)				/// "string" as may include equation names
		IPDOVER	SMISSING BYMISSING	/// options passed through from ipdover (see ipdover.ado & help file)
		/// Aggregate options
		AD(string) ADONLY			///	to be parsed later
		NPTS(name local) BYAD VARS(namelist local)	/// for error capture only -- should only appear within "AD" option
		/// General options
		BY(string)					///
		EFFect(string)				///
		FORESTplot(string asis)		/// options to pass through to forestplot
		LCOLs(string asis) RCOLs(string asis) STRLEN(integer 20)	/// lcols/rcols apply to both ipdmetan coeff matrix or to forestplot
		noGRaph						/// can be specified either directly to ipdmetan or as a forestplot() option
		noOVerall noSUbgroup		/// can be specified either directly to ipdmetan or as a forestplot() option
		noTABle	noHET noTOTal 		///
		PLOTID(string)				///
		SAving(string)				/// specify filename in which to save "resultsset"
		T Z							/// distribution to use for confidence intervals
		/// Undocumented options
		ZTOL(real 1e-6)	LEVEL(passthru)	/// ztol = tolerance for z-score (abs(ES/seES)); level = CI (default 95)
		ITOL(real 1e-8) MAXTAUSQ(real 50) ///
		MAXITER(real 1000)			/// defaults for mm_root (in GetEstimates)
		QUADPTS(real 40)			/// default quadrature points for &Integrate (in GetEstimates)
		*							///
	]
	
	_get_eformopts, soptions eformopts(`options') allowed(__all__)
	local options `"`s(options)'"'
	local eform = cond(`"`s(eform)'"'=="", "", "eform")
	if `"`effect'"'==`""' local effect = cond(`"`s(str)'"'=="", "Effect", `"`s(str)'"')
	if `"`interaction'"'!=`""' local effect `"Interact. `effect'"'
	

	** Parse random-effects option (more than one permitted syntax)
	* (N.B. Stata options are RIGHTMOST)
	local reModel "fe"					// fixed-effects (default)
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
		
		* Sort out synonyms
		if `"`random'"'!=`""' {
			local 0 `"`random'"'
			syntax [anything(name=reModel id="random-effects model")] [, ITOL(real 1e-8) MAXTausq(real 50) MAXITer(real 1000)]
			if inlist("`reModel'", "", "r", "random", "rand", "re", "dl") local reModel "dl"	// DerSimonian/Laird random-effects (default)
			else if inlist("`reModel'", "q", "gq", "genq", "vb", "eb") local reModel "gq"	// Generalised Q aka Empirical Bayes
			else if inlist("`reModel'", "g", "gamma", "bt", "bs") local reModel "bs"		// Biggerstaff/Tweedie random-effects (approx Gamma)
			else if inlist("`reModel'", "f", "fe", "fixed") local reModel "fe"			// Fixed-effects
			else if inlist("`reModel'", "vc", "ca") local reModel "vc"				// Variance-component aka Cochran ANOVA
			else if !inlist("`reModel'", "sj", "ml", "pl", "reml") {				// SJ = (improved) Sidik/Jonkman
				disp as err "Invalid random-effects model"							// ML/REML = (simple) ML/REML
				exit 198															// PL = "Profile" ML
			}
		}
		if inlist("`reModel'", "gq", "bs", "ml", "pl", "reml") {
			capture mata mata which mm_root()
			if _rc {
				disp as err "Iterative tau-squared calculations require mm_root from -moremata-"
				disp as err "Type -ssc install moremata- to obtain it"
				exit 499
			}
			if "`reModel'"=="bs" {
				capture mata mata which integrate()
				if _rc {
					disp as err "Approximate Gamma method requires the mata function integrate"
					disp as err "Type -ssc install integrate- to obtain it"
					exit 499
				}
			}
		}
		if "`reModel'"=="sj" & "`z'"=="" local t "t"
	}
	
	* Parse forest plot options to extract those relevant to ipdmetan
	* But first, temporarily rename options which may be supplied to EITHER ipdmetan OR forestplot.
	* (i.e. not noNAME, OVSTAT, STRLEN or *)
	* "forestplot options" prioritised over "ipdmetan options" in the event of a clash
	* ipdmetan will then pass them back through to forestplot as usual.
	foreach x in graph overall subgroup het eform effect plotid lcols rcols {
		local `x'2 ``x''
	}
	local 0 `", `forestplot'"'
	syntax [, noGRAPH noOVERALL noSUBGroup noHET EFORM EFFECT(string) PLOTID(string asis) LCOLS(string asis) RCOLS(string asis) noNAME OVSTAT(string) STRLEN(integer 20) *]
	local fplotopts `"`options'"'
	foreach x in graph overall subgroup het eform effect plotid lcols rcols {
		if `"``x''"'==`""' local `x' ``x'2'
	}
	if `"`het'"'!=`""' local ovstat "none"

	* Parse the estimation command
	cap `version' _prefix_command ipdmetan, `level' `eform' : `command'
	
	if _rc {
		cap _on_colon_parse `command'	// if error, test for `command' itself being a prefix command, e.g. svy
		if !_rc {
			`version' _prefix_command ipdmetan, `level' `eform' : `s(before)'
			local pcommand `"`s(command)' : "'
			local pcmdname `"`s(cmdname)'"'
		}
		else `version' _prefix_command ipdmetan, `level' `eform' : `command'		// if still can't sort it out, exit with error
	}
	local cmdname	`"`s(cmdname)'"'
	
	if inlist("`cmdname'", "xtmixed", "xtmelogit", "xtmepoisson") ///
		| inlist("`cmdname'", "mixed", "meglm", "melogit", "meqrlogit", "meprobit", "mecloglog") ///
		| inlist("`cmdname'", "meologit", "meoprobit", "mepoisson", "meqrpoisson", "menbreg") {			// if a mixed-effect model
		local anything `s(anything)'
		_parse expand stub1 stub2 : anything
		`version' _prefix_command ipdmetan, `level' `eform' : `s(cmdname)' `stub1_1'
		forvalues i=2/`stub1_n' {
			local cmdrest `"`cmdrest' (`stub1_`i'')"'
		}
		local cmdrest `"|| `cmdrest'"'
	}
	
	local cmdname	`"`s(cmdname)'"'
	if `"`cmdname'"'=="admetan" & "`adonly'"!=`""' {
		local cmdname								// _admetan was just dummy to satisfy _prefix_command
		local ADonly "ADonly"						// identifies that "admetan" was used
		if `"`efopt'"'!=`""' local efopt "eform"	// no command to which to pass other eforms (e.g. HR, OR)
	}
	local cmdargs	`"`s(anything)'"'
	local cmdif		`"`s(if)'"'
	local cmdin		`"`s(in)'"'
	local cmdopts	`"`s(options)'"'
	local level		`"`s(level)'"'
	if "`level'" == "" local level `c(level)'
	* local eform		`"`s(eform)'"'
	local eform		`"`s(efopt)'"'
	local command	`"`s(command)'"'


	*********
	* Setup *
	*********
	
	** Option compatibility tests
	if `"`cmdname'"'!=`""' {
		if `"`study'"'==`""' & `"`ipdover'"'==`""' & `"`ADonly'"'==`""' {
			disp as err `"option study() required for IPD analysis"'
			exit 198
		}
		if `"`exp_list'"'!=`""' & `"`interaction'"'!=`""' {
			disp as err `"options exp_list and interaction may not be combined"'
			exit 184
		}
		if `"`exp_list'"'!=`""' & `"`poolvar'"'!=`""' {
			disp as err "options poolvar and exp_list may not be combined"
			exit 184
			}
		if `"`total'"'!=`""' {
			if `"`exp_list'"'==`""' & `"`poolvar'"'==`""' {
				disp as err "Cannot specify nototal without one of exp_list or poolvar"
				exit 198
			}
			if `"`ipdover'"'!=`""' local overall "nooverall"
		}
	}
	
	** General presentation/forest-plot, no pooling (ipdover)
	if `"`over'"'!=`""' {
		disp as err `"Cannot specify over() with ipdmetan; please use ipdover instead"'
		exit 198
	}
	if `"`ipdover'"'!=`""'  {
		if `"`cmdname'"'==`""' {
			disp as error `"Must supply an estimation command to ipdover"'
			exit 198
		}
		foreach x in ad random {
			if `"``x''"'!=`""' {
				disp as error `"option `x' not allowed with ipdover"'
				exit 198
			}
		}
		local pooltext "Trial subgroup analysis"
		local het "nohet"		// cannot have heterogeneity stats with ipdover
		local z "z"
		local t					// cannot use t-distribution unless pooling
	}
	
	** Pooled meta-analysis (IPD, AD or both)
	else {
	
		* AD only; cannot use any of the IPD options (and "ad" option must be present)
		if `"`cmdname'"'==`""' {
			if `"`ADonly'"'==`""' {
				disp as err `"IPD estimation command not found"'		// i.e. assume IPD analysis is intended
				exit 198
			}
			if `"`ad'"'==`""' {
				disp as err `"For analysis of aggregate data alone, please use admetan"'
				exit 198
			}
			if `"`exp_list'"'!=`""' {
				disp as err `"exp_list not allowed with aggregate data alone"'
				exit 198
			}
			if `"`interaction'"'!=`""' {
				disp as err `"Option interaction not allowed with aggregate data alone"'
				exit 198
			}
		}
		
		if "`z'"!="" & "`t'"!="" {
			disp as err `"Cannot specify both z and t options; please choose just one"'
			exit 198
		}
		if "`t'"=="" local z "z"
	
		* Parse ad() option
		foreach x in npts byad vars {		// these options should not appear outside of the ad() option
			if `"``x''"'!=`""' {
				disp as err `"Suboption `x' can only be supplied to the ad() option"'
				exit 198
			}
		}
		if `"`ADonly'"'!=`""' & `"`ad'"'==`""' {
			disp as err `"For analysis of aggregate data alone, please use admetan"'
			exit 198
		}
		if `"`ad'"'!=`""' {
			local 0 `"`ad'"'
			syntax [anything(name=ADfile id="filename")] [if] [in], [BYAD NPTS(name local) VARS(namelist local)]
			
			* check valid "using" filename, if supplied
			if `"`ADfile'"'!=`""' {
				confirm file `"`ADfile'"'
				local ADif `"`if'"'
				local ADgin `"`in'"'
				
				* If filename supplied, must also have IPD command (AD alone should use -admetan-)
				if `"`cmdname'"'==`""' {
					disp as err `"Cannot specify an aggregate-data filename without IPD estimation command"'
					disp as err `"For analysis of aggregate data alone, please use admetan instead"'
					exit 198
				}
			}
			else {
				foreach x in if in {		// test for presence of "if"/"in" in absence of external file
					if `"``x''"'!=`""' {
						disp as err `"``x'' cannot be supplied to the ad() option without a filename"'
						local `x'
					}
					local AD`x' `cmd`x''
				}
				if `"`cmdname'"'!=`""' {	// if filename not supplied, assume using data in memory, therefore no IPD
					disp as err `"Aggregate data assumed to be in memory, therefore cannot have IPD data too"'
					disp as err `"Either supply a filename to the ad() option, or remove IPD command"'
					exit 198
				}
			}
			
			* byad conflicts
			if `"`byad'"'!=`""' {
				if `"`by'"'!=`""' {				// byad + by
					disp as err `"Cannot specify both byad and by() options.  Option byad will be ignored"'
					local byad
				}
				if `"`cmdname'"'==`""' {		// byad + no IPD
					disp as err `"Option byad specified but no IPD command found.  Option byad will be ignored"'
					local byad
				}
			}
		}

		else if `"`cmdname'"'==`""' {								// If no AD, IPD data must exist
			disp as err `"IPD estimation command not found"'		// i.e. assume IPD analysis is intended
			exit 198
		}

		local pooltext "Meta-analysis pooling"

	}		// end of if `"`ipdover'"'==`""'

	
	** Setup of data *in memory* (whether IPD or AD)
	tempvar touse
	mark `touse' `cmdif' `cmdin'
	
	* Added Jan 2014
	qui count if `touse'
	if !r(N) {
		if `"`ADonly'"'=="" local errtext "in study() variable"
		disp as err "no valid observations `errtext'"
		exit 2000
	}
	
	* parse `by' and `study' if NOT ipdover
	* (this has already been done in ipdover.ado since varname/varlist must be checked at the time)
	if `"`ipdover'"'==`""' {
		if `"`by'"'!=`""' {
			local 0 `"`by'"'
			syntax name(name=by) [, Missing]		// only a single (var)name is allowed
			local bymissing = cond(`"`missing'"'!=`""', "bymissing", "")
			
			cap confirm var `by'
			if _rc & !(`"`ad'"'!=`""' & `"`ADonly'"'==`""') {			// `by' may only NOT exist in memory
				disp as err "variable `by' not found in option by()"	// if an external aggregate-data file is specified.
				exit 111												// (and even then, it must exist there! - tested for later)
			}
		}
		local 0 `"`study'"'
		syntax name(name=study) [, Missing]		// only a single (var)name is allowed
		local smissing = cond(`"`missing'"'!=`""', "smissing", "")
	}
	
	confirm var `study'
	local overlen: word count `study'
	if `overlen'>1 {
		assert `"`ipdover'"'!=`""' 		// `study' can only contain multiple vars if ipdover
		local _over "_OVER"				// ...in which case, create marker
	}
	
	* Sort out subgroup identifier (BY) and labels
	* (N.B. `by' might only exist in an external (aggregate) dataset)
	local nby 1				// default, as used in logical expressions. (N.B. "bylist" is true marker of whether subgroups exist)
	local bystr=0
	tempvar bytemp
	if `"`by'"'!=`""' {
		local _by "_BY"		// marker of "by" (for use later on)
		tempname bylab

		cap confirm numeric var `by'
		if inlist(_rc, 0, 7) {			// var exists in memory
			local byIPD `"`by'"'		// marker of `by' present in memory
			local bystr=_rc				// var exists but is string, not numeric
			
			if `"`bymissing'"'==`""' {
				markout `touse' `by', strok		// ignore observations with missing "by" if appropriate
				qui count if `touse'
				if !r(N) {
					disp as err "no valid observations in by() variable"
					exit 2000
				}
			}
			
			if !`bystr' {
				tempname bymat
				local matrowopt `"matrow(`bymat')"'
			}
			cap tab `by' if `touse', m `matrowopt'
			if _rc {
				disp as err "variable in option by() has too many levels"
				exit 134
			}
			local nby = r(r)

			if `bystr' {
				qui encode `by' if `touse', gen(`bytemp') lab(`bylab')		// save label
				qui levelsof `bytemp' if `touse', miss local(bylist)		// save bylist, allowing for missing values
				
				local byIPD `bytemp'						// refer `byIPD' to new variable
				if `"`ADonly'"'!=`""' local by `bytemp'		// if ADonly, refer `by' too, for ProcessAD
			}
			else {
				cap assert `by'==round(`by')
				if _rc {
					disp as err "variable in option by() must be integer-valued or string"
					exit 198
				}
			
				* form value label and bylist from bymat
				forvalues i=1/`nby' {
					local byi = `bymat'[`i',1]
					local bylist "`bylist' `byi'"
					if `byi'!=. {
						local labname : label (`by') `byi'
						label define `bylab' `byi' "`labname'", add
					}
				}
			}
		}		// end if !_rc (cap confirm numeric var `by')
		
		else local byad "temp"			// if `by' not in memory, this is equivalent to "byad"
										// but value is "temp" so can be reset after ProcessAD
	}		// end if `"`by'"'!=`""'
	
	else if `"`byad'"'==`""' & `"`subgroup'"'!=`""' {
		disp as err "Cannot specify option nosubgroup without option by() (or byad)"
		disp as err "option nosubgroup will be ignored"
		local subgroup
	}
	
	* Sort out `byad' (i.e. if `by' supplied but var not in memory)
	if `"`byad'"'!=`""' {
		local _by "_BY"
		local bylist 1
		tempname bylab
		if `"`byad'"'!=`"temp"' tempvar by
	}
	
	* Parse `plotid' -- but keep original contents to pass to forestplot for later re-parsing
	* Allow "_BYAD" with ipdmetan in case of byad
	* Allow "_LEVEL", "_BY", "_OVER" with ipdover (because data manipulation means can't specify a single current varname)
	local 0 `"`plotid'"'
	syntax [name(name=plname)] [,*]
	local plotidopts `options'
	
	* Sort out `sortby' (and `plotid')
	if `"`ipdover'"'!=`""' {
		if `"`sortby'"'!=`""' {
			disp as err "option sortby may not be used with ipdover"
			exit 198
		}
		if "`plname'" != "" {
			if !inlist("`plname'", "_BY", "_OVER", "_LEVEL", "_n") {
				disp as err "plotid: option must contain one of _BY, _OVER, _LEVEL or _n if using -ipdover-"
				exit 198
			}
			if "`plname'"=="_LEVEL" {
				local plotid `"_LEVEL, `plotidopts'"'
				local _study "_LEVEL"
			}
			if "`plname'"=="_BY" & "`_by'"=="" {
				disp as err "plotid: _BY supplied without main option by()"
				exit 198
			}
		}
		local _study "_LEVEL"
	}
	else {
		if `"`sortby'"'==`""' local sortby `study'
		cap sort `sortby'
		if _rc & `"`sortby'"'!="_n" {
			disp as err `"sortby: variable `sortby' not found or invalid name"'
			exit _rc
		}
		if "`plname'"=="_BYAD" {
			if "`byad'"=="" {
				disp as err "plotid: _BYAD supplied without aggregate-data suboption byad"
				exit 198
			}
			local plotid `"_BY, `plotidopts'"'
		}
		else if "`plname'"=="`by'" & "`by'"!="" local plotid `"_BY, `plotidopts'"'
		else if "`plname'"=="`study'" local plotid `"_STUDY, `plotidopts'"'
		else if "`plname'"!="" {
			local plotvar `plname'				// plotvar contains a variable name other than study/by (not necessarily in current memory)
			local lcols `"`plotvar' `lcols'"'	// add it to lcols for parsing (will strip off again later)
		}
		local _study "_STUDY"
	}
	tempvar obs
	qui gen long `obs'=_n		// observation ID, sorted by `sortby' if appropriate

	* Sort out study ID (or 'over' vars)
	* If IPD meta-analysis (i.e. not ipdover), create subgroup ID based on order of first occurrence
	* (overh will be a single var, =study, so keep sgroup and stouse for later)
	local study2 : copy local study			// create copy in case of string variable
	local overtype "int"	// default
	tempvar stouse sgroup
	forvalues h=1/`overlen' {
		local overh : word `h' of `study'
		
		cap drop `stouse'
		qui gen byte `stouse' = `touse'
		if `"`smissing'"'==`""' markout `stouse' `overh', strok
		
		if `"`_over'"'==`""' {
			tempvar sobs
			qui bysort `stouse' `overh' (`obs') : gen long `sobs' = `obs'[1]
			qui bysort `stouse' `byIPD' `sobs' : gen long `sgroup' = (_n==1)*`stouse'
			qui replace `sgroup' = sum(`sgroup')
			local ns = `sgroup'[_N]					// number of studies
			qui drop `sobs'
		}
			
		* Test to see if subgroup variable varies within studies/trials; if it does, exit with error
		if `"`ipdover'"'==`""' & `"`cmdname'"'!=`""' {
			qui tab `overh' if `stouse', m
			if `r(r)' != `ns' {					// N.B. `ns' is already stratified by `by'
				disp as err "Data is not suitable for meta-analysis"
				disp as err " as subgroup variable (in option 'by') is not constant within trials."
				disp as err "Use alternative command 'ipdover' if appropriate."
				exit 198
			}
				
			* Also test plotvarIPD in the same way (if exists)
			cap confirm var `plotvar'
			if !_rc {
				local plotvarIPD `plotvar'
				tempvar tempgp
				qui bysort `stouse' `plotvar' `overh' : gen long `tempgp' = (_n==1)*`stouse'
				qui replace `tempgp' = sum(`tempgp')
				summ `tempgp', meanonly
				local ntg = r(max)
				drop `tempgp'
				
				qui tab `overh' if `stouse', m
				if `r(r)' != `ntg' {
					disp as err "plotid: variable `plotvar' is not constant within trials"
					exit 198
				}
			}
		}
		
		* Store variable label
		local varlab`h' : variable label `overh'
		if `"`varlab`h''"'==`""' local varlab`h' `"`overh'"'

		* Numeric type
		if `"`overtype'"'=="int" & inlist(`"`: type `overh''"', "long", "float", "double") {
			local overtype `"`: type `overh''"'		// "upgrade" if more precision needed
		}
		
		* If any string variables, "decode" them
		* and replace string var with numeric var in list "study"
		* If numeric, make a copy of each (original) value label value-by-value
		* to avoid unlabelled values being displayed as blanks
		* (also, for `study' with IPD+AD it needs to be added to)
		* Then, store value label
		cap confirm string var `overh'
		if !_rc {
			tempvar overtemp
			tempname `overtemp'lab
			qui encode `overh' if `stouse', gen(`overtemp') label(``overtemp'lab')
			local study2 : subinstr local study2 `"`overh'"' `"`overtemp'"', all word
			local overh `overtemp'
		}
		else {
			cap assert `overh'==round(`overh')
			if _rc {
				if `"`ipdover'"'!=`""' local errtext "variables in option over()"
				else local errtext "variable in option study()"
				disp as err `"`errtext' must be integer-valued or string"'
				exit 198
			}
			qui levelsof `overh' if `stouse', missing local(levels)
			if `"`levels'"'!=`""' {
				tempname `overh'lab
				foreach i of local levels {
					local labname : label (`overh') `i'
					label define ``overh'lab' `i' `"`labname'"', add				
				}
			}
		}
		local lablist `"`lablist' ``overh'lab'"'
	}
	local study : copy local study2
	
	
	*** Sort out and save value labels, to be re-applied later within `pfile'
	if `"`lablist'"'!=`""' {
		tempfile labfile
		qui label save `lablist' using `labfile'		// save "study"/"over" value labels
	}
	if `"`bylab'"'!=`""' {
		tempfile bylabfile								// save "by" value label
		cap label save `bylab' using `bylabfile'		// ("capture" since bylab might not be defined yet)
	}
	
	* Store max "by" value and max study ID for ProcessAD (need to do this now before dataset is changed, starting line 686)
	if `"`ad'"'!=`""' {
		if `"`byIPD'"'!=`""' {
			summ `byIPD' if `touse', meanonly
			local bymax = `r(max)'
			local bymaxopt `"bymax(`bymax')"'
		}
		summ `study' if `touse', meanonly
		local smaxopt `"smax(`r(max)')"'
	}
	
	
	*** Set up lcols and rcols if appropriate (either for a saved dataset or a forestplot)
	foreach x in na nc ncs nr ni HetOnNewLine {
		local `x'=0
	}
	if (`"`saving'"'!=`""' | `"`graph'"'==`""') & (`"`lcols'"'!=`""' | `"`rcols'"'!=`""') {

		local rcolsy = (`"`lcols'"'==`""')						// marker of "start with rcols = yes" (i.e. if no lcols)
		parsecols `lcols' : `rcols', rcols(`rcolsy') `byad'		// option rcols() = "are we currently parsing Rcols? (as opposed to Lcols)"
		local lcols	/*clear macro */							// if no lcols then rcols=1 from the start
		local rcols	/*clear macro */							// otherwise gets changed when appropriate within parsecols.

		local itypes `"`r(itypes)'"'			// item types ("itypes")
		local fmts `"`r(fmts)'"'				// formats
		local cclist `"`r(cclist)'"'			// clist of expressions for -collapse-
		local statsr `"`r(rstatlist)'"'			// list of "as-is" returned stats		
		local sidelist `"`r(sidelist)'"'		// list of "sides" (i.e. left or right)
		local oldnames `"`r(oldnames)'"'		// list of original varnames for strings
		local lrcols `"`r(newnames)'"'			// item names (valid Stata names)
		
		* Test validity of names -- cannot be any of the names ipdmetan uses for other things
		local badnames `"_BY _OVER `_study' _ES _seES _NN"'
		if `"`: list lrcols & badnames'"'!=`""' {
			disp as err "Invalid name in lcols or rcols"
			exit 198
		}
		
		* Get total number of "items" and loop, perfoming housekeeping tasks for each item
		local ni : word count `itypes'
		forvalues i=1/`ni' {
		
			* Form new `lcols' and `rcols', just containing new varnames
			if !`: word `i' of `sidelist'' {
				local lcols `"`lcols' `: word `i' of `lrcols''"'
			}
			else local rcols `"`rcols' `: word `i' of `lrcols''"' 
	
			* Separate lists of names for the different itypes
			if "`: word `i' of `itypes''"=="a" {						// a: AD-only vars, not currently in memory
				local ++na
				local ADvars `"`ADvars' `: word `i' of `lrcols''"'
				local nalab`na' `"`r(avarlab`na')'"'					// store variable label
			}
			else if "`: word `i' of `itypes''"=="c" {					// c: Numeric vars to collapse
				local ++nc
				local namesc `"`namesc' `: word `i' of `lrcols''"'
				local nclab`nc' `"`r(cvarlab`nc')'"'
			}
			else if "`: word `i' of `itypes''"=="cs" {					// cs: String vars to "collapse"
				local ++ncs
				local svars `"`svars' `: word `i' of `lrcols''"'
				local ncslab`ncs' `"`r(csvarlab`ncs')'"'
			}
			else if "`: word `i' of `itypes''"=="r" {					// r: Returned stats (e-class or r-class)
				local ++nr												// (validity to be tested later)
				local namesr `"`namesr' `: word `i' of `lrcols''"'
				local nrlab`nr' `"`r(rvarlab`nr')'"'
			}
			if `"`het'"'==`""' & inlist("`: word `i' of `itypes''", "c", "r") & !`: word `i' of `sidelist'' {
				local HetOnNewLine=1				// if "c" or "r" in "lcols" then het will need to be on a new line (in forestplot)
			}
		}
	}		// end if (`"`saving'"'!=`""' | `"`graph'"'==`""') & (`"`lcols'"'!=`""' | `"`rcols'"'!=`""')

	* If lcols/rcols will not be used, clear the macros
	else {
		local lcols
		local rcols
	}
	
	
	* If IPD, run command on entire dataset
	* (to test validity, and also to find default poolvar and/or store overall returned stats if appropriate)
	tempname totnpts
	scalar `totnpts' = .
	
	if `"`cmdname'"'!=`""' {
	
		// Save any existing estimation results, and clear return & ereturn
		tempname est_hold
		cap estimates store `est_hold'
		if _rc local est_hold
		_prefix_clear, e r

		if `"`total'"'==`""' {		// run the command using the entire dataset
			cap `version' `pcommand' `cmdname' `cmdargs' if `touse', `cmdopts' `cmdrest'
			local rc = _rc
			if `rc' {
				local errtext = cond(`"`pcmdname'"'!=`""', `"`pcmdname'"', `"`cmdname'"')
				_prefix_run_error `rc' ipdmetan `errtext'
			}
		
			// check if e-class
			cap mat list e(b)
			if !_rc {
				if `"`exp_list'"'!=`""' & `"`poolvar'"'==`""' {
					disp as err "cannot specify 'exp_list' with an estimation (e-class) command; please use the 'poolvar' option instead"
					exit 198
				}
				if `"`poolvar'"'!=`""' local exp_list `"(_b[`poolvar']) (_se[`poolvar'])"'		// N.B. e(N) will be added later
			}
			else if `"`poolvar'"'!=`""' {
				disp as err "cannot specify 'poolvar' with a non e-class command; please specify 'exp_list' instead"
				exit 198
			}

			* Parse <exp_list>
			local nexp=0
			local neexp=0
			_prefix_explist `exp_list', stub(_df_) edefault
			if `"`exp_list'"'!=`""' {
				assert `s(k_eexp)'==0 & inlist(`s(k_exp)', 2, 3)		// if exp_list supplied, must be 2 or 3 exps, no eexps
				local nexp = `s(k_exp)'
			}
			else {
				assert `s(k_eexp)'==1 & `s(k_exp)'==0		// otherwise, must be a single eexp (_b) and no exps
				local neexp = `s(k_eexp)'
			}
			local eqlist	`"`s(eqlist)'"'
			local idlist	`"`s(idlist)'"'
			local explist	`"`s(explist)'"'
			local eexplist	`"`s(eexplist)'"'
			
			* Expand <exp_list>	
			tempname b
			cap _prefix_expand `b' `explist' `statsr', stub(_df_) eexp(`eexplist') colna(`idlist') coleq(`eqlist') eqstub(_df)
			if _rc {
				disp as err "explist error. Possible reasons include:"
				disp as err "- coefficient in poolvar() not found in the model"
				disp as err "- an expression in lcols/rcols that evaluates to a string"
				disp as err "- an expression not enclosed in parentheses"
				exit _rc
			}
			local nexp = cond(`neexp', `s(k_eexp)', `nexp')		// if eexps, update `neexp' and rename it to `nexp'
			
			* Form list of "returned statistic" expressions to post
			if `nr' {
				forvalues j=1/`nr' {
					local i = `nexp' + `j'
					local us_`j' `"`s(exp`i')'"'
					local rpostlist `"`rpostlist' `us_`j''"'
				}
			}
			if `"`exp_list'"'!=`""' & `"`poolvar'"'==`""' {		// non e-class
				local beta `"`s(exp1)'"'
				local sebeta `"`s(exp2)'"'
				local nbeta = cond(`nexp'==3, `"`s(exp3)'"', ".")
			}
			else {
				* If e-class, parse e(b) using _ms_parse_parts
				* Choose the first suitable coeff, then check for conflicts with other coeffs (e.g. interactions)
				* Can we also check for badly-fitted coeffs here?  i.e. v high/low b or se?
				local ecolna `"`s(ecolna)'"'	// from _prefix_expand
				local ecoleq `"`s(ecoleq)'"'	// from _prefix_expand
				local colna : colnames e(b)		// from e(b)
				local coleq : coleq e(b)		// from e(b)

				* If not poolvar (i.e. basic syntax), results from _prefix_expand should match those from e(b)
				assert (`"`ecolna'"'!=`""') == (`"`poolvar'"'==`""')
				assert (`"`ecoleq'"'!=`""') == (`"`poolvar'"'==`""')
				if `"`poolvar'"'==`""' {
					assert `"`ecolna'"'==`"`colna'"'
					if substr(`"`coleq'"', 1, 1)!=`"_"' {
						assert `"`ecoleq'"'==`"`coleq'"'
					}
				}
				local estvar `"`poolvar'"'
				local name1
				local name2
				
				forvalues i=1/`nexp' {
					local colnai : word `i' of `colna'
					local coleqi : word `i' of `coleq'

					_ms_parse_parts `colnai'
					if `r(omit)'==0 {
					
						* If estvar already exists, check for conflicts with subsequent coeffs
						* (cannot currently check for difference between, e.g. "arm" and "1.arm"
						*   - i.e. how to tell when a var is factor if not made explicit... is this a problem?)
						if `"`estvar'"'!=`""' {
							if `"`coleqi'"'==`"`estvareq'"' {			// can only be a conflict if same eq
								if `"`r(type)'"'=="interaction" {
									local rname1 = cond(`"`r(op1)'"'==`""', `"`r(name1)'"', `"`r(op1)'.`r(name1)'"')
									local rname2 = cond(`"`r(op2)'"'==`""', `"`r(name2)'"', `"`r(op2)'.`r(name2)'"')
								
									if (`"`interaction'"'!=`""' & ///
											( inlist(`"`name1'"',`"`rname1'"',`"`rname2'"') ///
											| inlist(`"`name2'"',`"`rname1'"',`"`rname2'"') )) ///
										| (`"`interaction'"'==`""' & inlist(`"`estvar'"',`"`rname1'"',`"`rname2'"')) {
										disp as err "Automated identification of estvar failed; please use poolvar() option or supply exp_list"
										exit 198
									}
								}
								else if inlist(`"`r(type)'"',"variable","factor") {
									local rname = cond(`"`r(op)'"'==`""', `"`r(name)'"', `"`r(op)'.`r(name)'"')
								
									if (`"`interaction'"'!=`""' & inlist(`"`rname'"',`"`name1'"',`"`name2'"')) ///
										| (`"`interaction'"'==`""' & `"`rname'"'==`"`estvar'"') {
										disp as err "Automated identification of estvar failed; please use poolvar() option or supply exp_list"
										exit 198
									}
								}
							}
						}		// end if `"`estvar'"'!=`""'
					
						* Else define estvar
						else if `"`interaction'"'!=`""' {
							if `"`r(type)'"'=="interaction" {
								local estvar `colnai'
								local estvareq `coleqi'
								local name1 `"`r(name1)'"'
								local name2 `"`r(name2)'"'
							}
						}
						else {
							local estvar `colnai'
							local estvareq `coleqi'							
						}		// end else
					}		// end if `r(omit)'==0
				}		// end foreach x of local colnames 

				if `"`estvar'"'==`""' {
					disp as err "Automated identification of estvar failed; please use poolvar() option or supply exp_list"
					exit 198
				}
				
				if !inlist(`"`estvareq'"', "_", "") local estvareq `"[`estvareq']"'
				else local estvareq
				
				local beta `"`estvareq'_b[`estvar']"'
				local sebeta `"`estvareq'_se[`estvar']"'
				local nbeta `"e(N)"'

			}		// end else (i.e. if eclass)
		}			// end if `"`total'"'==`""'
		
		else {		// i.e. if noTOTAL
		
			// Define expressions if noTOTAL
			if `"`poolvar'"'!=`""' local exp_list `"(_b[`poolvar']) (_se[`poolvar']) (e(N))"'
			local estvar `poolvar'
			local nexp : word count `exp_list'
			tokenize `exp_list'
			local beta `1'
			local sebeta `2'
			local nbeta = cond(`nexp'==3, `"`3'"', ".")
			
			// cannot use returned stats if noTOTAL since they cannot be pre-checked with _prefix_expand
			if `nr' {
				disp as err "Cannot collect returned statistics with nototal"
				exit 198
			}
		}
	
		** Set up postfile
		tempname postname
		tempfile pfile
		if `"`byIPD'"'!=`""' {
			local byopt `"`: type `byIPD'' _BY"'
		}
		else if `"`byad'"'!=`""' local byopt "byte _BY"
		if `"`_over'"'!=`""' local overopt "byte _OVER"
		postfile `postname' long `sgroup' `byopt' `overopt' `overtype' `_study' byte _USE double(_ES _seES) long _NN `namesr' using `pfile'
			
		* Overall (non-pooled): post values or blanks, as appropriate
		if `"`overall'"'==`""' | (`"`byad'"'!=`""' & `"`subgroup'"'==`""') {
		
			// post "(.) (5)" if overall (or "(.) (3)" if byad, as IPD is treated as subgroup)
			local postreps : di _dup(3) `" (.)"'
			if `"`overall'"'==`""' & `"`byad'"'==`""' {
				if `"`ipdover'"'!=`""' & `"`total'"'==`""' {
					local postexp `"(.) (5) (`beta') (`sebeta') (`nbeta')"'		// only post non-pooled overall stats if ipdover
					scalar `totnpts' = `nbeta'
				}
				else local postexp `"(.) (5) `postreps'"'
			}
			if `"`_over'"'!=`""' local postexp `"(.) `postexp'"'
			if `"`byIPD'"'!=`""' local postexp `"(.) `postexp'"'
			if `"`byad'"'!=`""' & `"`subgroup'"'==`""' {
				local postexp `"(1) (.) (3) `postreps'"'						// "all data in memory" ==> "subgroup" (_USE==3) if byad
			}
			
			if `nr' {
				if `"`total'"'==`""' {
					forvalues j=1/`nr' {			// returned statistics
						local postexp `"`postexp' (`us_`j'')"'
					}
				}
				else {
					local postreps : di _dup(`nr') `" (.)"'
					local postexp `"`postexp' `postreps'"'
				}
			}

			post `postname' (.) `postexp'

		}		// end if `"`overall'"'==`""' | (`"`byad'"'!=`""' & `"`subgroup'"'==`""')
		
		* If byad (i.e. no *overall* yet), generate blank "overall" observation (to hold pooled estimates)
		if `"`byad'"'!=`""' & `"`overall'"'==`""' {
			local postreps : di _dup(3) `" (.)"'
			local postexp `"`postreps' (5) `postreps'"'

			local postreps : di _dup(`nr') `" (.)"'
			local postexp `"`postexp' `postreps'"'
			
			post `postname' `postexp'
		}

		
		*** Analyse IPD

		* Main IPD analysis loop
		cap drop _rsample
		qui gen byte _rsample=0			// this will show which observations were used

		* tempvar obs
		* gen long `obs'=_n
		local n=1						// matrix row counter
		forvalues h=1/`overlen' {		// if over() not specified this will be 1/1
										// else, make `sgroup' equal to (`h')th over variable
			local overh : word `h' of `study'

			* If "over" vars, order them "naturally", i.e. numerically or alphabetically
			* Otherwise, use existing `sgroup' and `ns' from earlier loop
			if `"`_over'"'!=`""' {
				cap drop `stouse'
				qui gen byte `stouse' = `touse'
				if `"`smissing'"'==`""' markout `stouse' `overh', strok
				
				cap drop `sgroup'
				qui bysort `stouse' `byIPD' `overh' : gen long `sgroup' = (_n==1)*`stouse'
				qui replace `sgroup' = sum(`sgroup')
				local ns = `sgroup'[_N]				// total number of studies (might be repeats if `by' not trial-level)
			}
			sort `obs'
		
			* Loop over trial IDs (or levels of `h'th "over" var)
			forvalues i=1/`ns' {
				summ `obs' if `touse' & `sgroup'==`i', meanonly
				
				* Find value of by() for current trial ID
				if `"`byIPD'"'!=`""' {
					local val = `byIPD'[`r(min)']
					local postby `"(`val')"'
				}
				else if `"`byad'"'!=`""' local postby `"(1)"'
				
				* Add over() var ID
				if `"`_over'"'!=`""' local postover `"(`h')"'
				
				* Create label containing original values or strings,
				* then add (original) trial ID
				local val = `overh'[`r(min)']
				local poststudy `"(`val')"'
				local trlabi : label (`overh') `val'
				if `"`messages'"'!=`""' disp as text  "Fitting model for `overh' = `trlabi' ... " _c				
				cap `version' `pcommand' `cmdname' `cmdargs' if `touse' & `sgroup'==`i', `cmdopts' `cmdrest'
				local rc = c(rc)
				if `rc'==1 error 1		// user break
				
				if `rc' {	// if unsuccessful, insert blanks
					if `"`messages'"'!=`""' {
						disp as err "Error: " _c
						cap noisily error _rc
					}
					local reps = 3 + `nr'
					local postreps : di _dup(`reps') `" (.)"'
					local postcoeffs `"(2) `postreps'"'			// (2) for _USE ==> unsuccessful
				}
				else {
					* If fitting successful but coefficient was not estimated
					if `sebeta'==0 | abs(`beta'/`sebeta')<`ztol' {
						if `"`messages'"'!=`""' {
							disp as err "Coefficent cannot be estimated"
						}
						local postcoeffs `"(2) (.) (.) (`nbeta')"'
					}
					else {
						local postcoeffs `"(1) (`beta') (`sebeta') (`nbeta')"'
						if `"`messages'"'!=`""' disp as res "Done"
					}
					qui replace _rsample=1 if e(sample)			// won't do anything if e(sample) not set
					
					if `nr' {
						forvalues j=1/`nr' {
							local postcoeffs `"`postcoeffs' (`us_`j'')"'
						}
					}
				}

				post `postname' (`i') `postby' `postover' `poststudy' `postcoeffs'
				local postby
				local postover
				local postcoeffs

			}	// end forvalues i=1/`ns'
		}		// end forvalues h=1/`overlen'

		* If appropriate, generate blank subgroup observations
		* and fill in with user-requested statistics (and, if ipdover, non-pooled effect estimates)
		if `"`byIPD'"'!=`""' & `"`subgroup'"'==`""' {
			forvalues i = 1/`nby' {
				local byi : word `i' of `bylist'
				local blank=0
				
				if (`"`ipdover'"'!=`""' | `nr') {
					cap `version' `pcommand' `cmdname' `cmdargs' if `byIPD'==`byi' & `touse', `cmdopts' `cmdrest'
					if !_rc {
						local postexp `"(.) (3) (`beta') (`sebeta') (`nbeta')"'
						if `nr' {
							forvalues j=1/`nr' {
								local postexp `"`postexp' (`us_`j'')"'
							}
						}
					}
					else local blank=1
				}
				else local blank=1
				if `blank' {
					local reps = 3 + `nr'
					local postreps : di _dup(`reps') `" (.)"'
					local postexp `"(.) (3) `postreps'"'
				}
				if `"`_over'"'!=`""' local postexp `"(.) `postexp'"'
				local postexp `"(.) (`byi') `postexp'"'
				post `postname' `postexp'

			}		// end forvalues i=1/`nby'
		}		// end if `"`byIPD'"'!=`""' & `"`subgroup'"'==`""'
		
		postclose `postname'
		

		*** Perform -collapse- if data in memory is being used
		if `"`cclist'"'!=`""' {
			preserve
			forvalues h=1/`overlen' {
				local overh : word `h' of `study'
				tempfile extra1_`h'
				qui collapse `cclist' if `touse', fast by(`byIPD' `overh')	// study/over-level
				qui save `extra1_`h''
				restore, preserve
			}
			if `"`byIPD'"'!=`""' & "`subgroup'"==`""' {
				tempfile extra1_by
				qui collapse `cclist' if `touse', fast by(`byIPD') 			// by-level
				qui save `extra1_by'
				restore, preserve
			}
			if `"`overall'"'==`""' | (`"`byad'"'!=`""' & "`subgroup'"==`""') {
				tempfile extra1_tot
				qui collapse `cclist' if `touse', fast 						// overall
				qui save `extra1_tot'										// (or "subgroup" level for byad)
				restore, preserve
			}
		}
	
		* Perform manual "collapse" of any string vars in "over" files
		* This could take a bit of to-ing and fro-ing, but it's a niche case
		if `"`svars'"'!=`""' & `overlen'>1 {
			assert `ncs' == `: word count `oldnames''
			cap preserve
			forvalues i=1/`ncs' {
				cap rename `: word `i' of `oldnames'' `: word `i' of `svars''	// cap because renaming might not be necessary
			}
			forvalues h=2/`overlen' {
				tempvar keepby
				local overh : word `h' of `study'
				qui bysort `touse' `overh': keep if _n==_N & `touse'
				keep `byIPD' `overh' `svars'
				if `"`cclist'"'!=`""' {					// file already created above
					qui merge 1:1 `byIPD' `overh' using `extra1_`h'', nogen
					qui save, replace
				}	
				else {									// file not yet created
					tempfile extra1_`h'
					qui save `extra1_`h''
				}
				restore, preserve
			}
		}
	
		** Append files to form a single "extra" file
		if `"`cclist'"'!=`""' | `"`svars'"'!=`""' {	
			cap preserve
		
			if `"`svars'"'!=`""' {
				forvalues i=1/`ncs' {
					cap rename `: word `i' of `oldnames'' `: word `i' of `svars''	// cap because renaming might not be necessary
				}
				local overh : word 1 of `study'
				qui bysort `touse' `overh': keep if _n==_N & `touse'
				keep `byIPD' `overh' `svars'
				if `"`cclist'"'!=`""' {					// file already created above
					qui merge 1:1 `byIPD' `overh' using `extra1_1', nogen
				}
			}
			else qui use `extra1_1', clear

			if `overlen'>1 {				// if "over", append files
				qui gen _OVER=1
				forvalues h=2/`overlen' {
					local prevoverh : word `=`h'-1' of `study'
					local overh : word `h' of `study'
					rename `prevoverh' `overh'				// rename study var to match with next dataset
					qui append using `extra1_`h''
					qui replace _OVER=`h' if missing(_OVER)
				}
			}
			if `"`cclist'"'!=`""' {			// 'by' and 'overall' sections don't apply if only svars
				if `"`byIPD'"'!=`""' & "`subgroup'"==`""' {
					qui append using `extra1_by'
				}
				if `"`overall'"'==`""' | (`"`byad'"'!=`""' & "`subgroup'"==`""') {
					qui append using `extra1_tot'
				}
			}
					
			* Apply variable labels to "collapse" vars
			forvalues i=1/`nc' {
				label var `: word `i' of `namesc'' `"`nclab`i''"'
			}
			if `"`svars'"'!=`""' {			// ...and "string" collapse vars
				forvalues i=1/`ncs' {
					label var `: word `i' of `svars'' `"`ncslab`i''"'
				}
			}

			cap rename `overh' `_study'		// rename to "_STUDY" or "_LEVEL"
			if `"`byIPD'"'!=`""' & "`subgroup'"==`""' {
				rename `byIPD' _BY
			}
			else if `"`byad'"'!=`""' gen _BY = 1
			
			qui merge 1:1 `_by' `_over' `_study' using `pfile', nogen
			qui save `pfile', replace
			
		}	// end if `"`cclist'"'!=`""' | `"`svars'"'!=`""'
	}		// end if `"`cmdname'"'!=`""'
	
	cap preserve		// original data not needed from now on


	*** Analysis of AD (whether in memory or in external dataset)
	if `"`ad'"'!=`""' {
	
		* Data setup
		local external=0
		if `"`ADfile'"'!=`""' {
		
			* Load external data and apply IPD value labels
			qui use `"`ADfile'"', clear
			cap do `labfile'		// re-load "study" value label
			cap do `bylabfile'		// re-load "by" value label
			local external=1		// marker that data is now from `ADfile' rather than that in original memory
			
			if `"`byad'"'=="temp" {
				cap confirm var `by'
				if _rc {
					disp as err `"variable `by' not found in either IPD or AD dataset"'
					exit 111
				}
			}
			if `"`plotvar'"'!=`""' {
				cap confirm var `plotvar'
				if _rc & `"`plotvarIPD'"'==`""' {
					disp as err `"variable `plotvar' not found in either IPD or AD dataset"'
					exit 111
				}
			}					
		}
		if `"`npts'"'!=`""' {
			cap confirm var `npts'
			if _rc {
				if _rc==111 disp as err "npts: variable `npts' not found in aggregate dataset"
				exit _rc
			}
		}
		
		cap ProcessAD `ADif' `ADin', external(`external') vars(`vars') npts(`npts') keep(`lrcols' `sgroup') ztol(`ztol') level(`level') ///
			study(`study') sortby(`sortby') `smaxopt' studylab(``study'lab') by(`by') `bymaxopt' byad(`byad') bylab(`bylab') bylist(`bylist') ///
			`subgroup' `overall' `smissing' `bymissing'
		* (N.B. `sgroup' in keep() is needed for admetan)
		
		local ADfail = (_rc==2000)
		if !`ADfail' & `external' {
			local bylistAD `"`r(bylistAD)'"'		// only relevant if external
			
			qui gen `sgroup' = _n		// ProcessAD has already put the data into the correct order
			qui merge 1:1 `_by' _STUDY _USE using `pfile', nolabel assert(master using)
					
			if `"`byad'"'!=`"byad"' {
				qui recode _merge (1=2 "Aggregate data") (2=1 "IPD"), gen(_SOURCE) label(_SOURCE)
				local _source "_SOURCE"
			}
			qui drop _merge
			
			if `"`byad'"'!=`""' {
				if `"`byad'"'==`"temp"' {						// shift (non-missing) values back
					numlist "`bylistAD'", integer miss sort
					tokenize `r(numlist)'
					local minADby = `1'							// `minADby' = lowest AD value
					qui replace _BY = _BY - (2 - `minADby') if !missing(_BY)
					local byad									// reset byad (see line 419)
				}
				else local minADby = 2
						
				label define `bylab' `=`minADby'-1' "IPD", add
				local bylist `"`=`minADby'-1' `bylistAD'"'
				local nby : word count `bylist'
			}
			else if `"`bylistAD'"'!=`""' {
				local bylist : list bylist | bylistAD
				numlist `"`bylist'"', integer miss sort
				local bylist=r(numlist)
				local nby : word count `bylist'
			}
		}
		else if `ADfail' {
			use `pfile', clear
			cap do `labfile'
			cap do `bylabfile'
			gen _SOURCE=1
			
			if `"`_by'"'!=`""' & `"`subgroup'"'==`""' {
				local ++bymax
				local bylist `"`bylist' `bymax'"'
				label define `bylab' `bymax' "Aggregate", add		
			}
		}

		* Create observations to hold subgroup effects, if don't exist yet
		* local newobs=0
		if `"`_by'"'!=`""' & `"`subgroup'"'==`""' {
			forvalues i=1/`nby' {
				local byi : word `i' of `bylist'
				qui count if _BY==`byi' & _USE==3	// test for existence -- may have been created already during IPD subroutine
				if !r(N) {
					local newN = `=_N + 1'
					qui set obs `newN'
					qui replace _BY = `byi' in `newN'
					qui replace _USE = 3 in `newN'
				}
			}
		}
		if !`external' & `"`overall'"'==`""' {		// should only be necessary if "AD only" (in memory)
			local newN = `=_N + 1'
			qui set obs `newN'
			qui replace _USE = 5 in `newN'
		}
	}		// end if `"`ad'"'!=`""'
	
	else {
		qui use `pfile', clear
		cap do `labfile'		// re-load "study"/"over" value labels
		cap do `bylabfile'		// re-load "by" value label
	}

	qui count if _USE == 5
	assert `r(N)' == (`"`overall'"'==`""')

	cap confirm var _NN
	if !_rc {
		summ _NN, meanonly
		if !`r(N)' qui drop _NN		// drop if no data (i.e. no. pts not available)
		cap confirm var _NN
		if !_rc local _NN "_NN"
	}

	* Store value labels in new var "_LABELS"
	* (the only way for "over", and needs to be done anyway if graph/saving)
	qui gen _LABELS=""
	forvalues h=1/`overlen' {
		local overh : word `h' of `study'
		if `"``overh'lab'"'!=`""' {
			label values `_study' ``overh'lab'
			qui decode `_study', gen(label`h')
			local replacewith `"label`h'"'
		}
		else local replacewith `"string(`_study')"'
			
		if `"`_over'"'!=`""' qui replace _LABELS=`replacewith' if _OVER==`h'
		else qui replace _LABELS=`replacewith'
		cap drop label`h'
	}
	if `"`ipdover'"'!=`""' label values _LEVEL		// if "ipdover", remove labels
	else {
		cap label copy ``study'lab' _STUDY			// ... otherwise, standardise value label name
		cap label values _STUDY _STUDY				// ("capture" just in case label name is already "_study", which is fine)
	}
	if `"`_by'"'!=`""' {
		label copy `bylab' _BY
		label values _BY _BY			// standardise "by" value label name and apply it
	}
	
	* Remove excluded studies if appropriate, and check that at least one valid estimate exists
	* (N.B. otherwise, identified by "_USE==2")
	if `"`keepall'"'==`""' qui drop if _USE==2
	qui summ _ES, meanonly
	if !`r(N)' {
		disp as err `"No estimates found. Check:"'
		disp as err `"- specification of interaction option"'
		disp as err `"- model is able to be fitted within the entire dataset and/or a specific trial"'
		exit 198
	}
	qui count if _USE==1
	local countK = r(N)

	
	*** Get inverse-variance weights and perform pooled analysis
	
	* Confidence limits for study estimates are based on SE + normal distribution
	* This also applies to subgroup/overall estimates if "ipdover"
	* Hence, just calculate limits for entire dataset, and replace within subsequent subroutines if necessary
	tempname crit
	scalar `crit' = invnorm(.5+`level'/200)	// normal distribution
	qui gen _LCI = _ES - `crit'*_seES
	qui gen _UCI = _ES + `crit'*_seES
	
	* Subgroups -- loop over bylist first, then over, to match with display output
	qui gen sgwt=.
	tempname Q Qsum Qdiff tcrit
	scalar `Q'=0
	scalar `Qsum'=0
	scalar `Qdiff'=0
	local n=1
	forvalues i=1/`nby' {				// this will be 1/1 if no subgroups
		if `"`_by'"'!=`""' {
			local byi : word `i' of `bylist'
		}

		forvalues j=1/`overlen' {		// if over() not specified this will be 1/1

			qui gen `touse' = (_USE==1)
			if `"`_by'"'!=`""' {
				qui replace `touse' = `touse'*(_BY==`byi')
			}
			if `"`_over'"'!=`""' {
				qui replace `touse' = `touse'*(_OVER==`j')
			}
		
			cap mata: GetEstimates("`touse'", "sgwt", "`reModel'", `maxtausq', `itol', `maxiter', `quadpts', `level')
			if _rc>=3000 {
				disp as err "Mata error"
				exit _rc
			}
			else if !_rc {		// if weights were calculated, i.e. if subgroup could be analysed
				
				* Subgroup results for ipdmetan (already done for ipdover)
				if `"`_by'"'!=`""' & `"`ipdover'"'==`""' {

					* Store scalars
					foreach x in mu_hat se_mu_hat Q K tausq sigmasq {
						tempname `x'`i'
						scalar ``x'`i'' = `r(`x')'
					}
					tempname tstat`i'
					scalar `tstat`i'' = `mu_hat`i''/`se_mu_hat`i''
					tempname Qr`i' Isq`i'
					if `"`r(Qr)'"'!=`""' {
						scalar `Qr`i'' = `r(Qr)'
					}
					else scalar `Qr`i'' = `r(Q)' 
					scalar `Isq`i''=`tausq`i''/(`tausq`i''+`sigmasq`i'')	// General formula for Isq (including D+L)
					scalar `Qsum' = `Qsum' + `Q`i''

					* Store effect size, SE and confidence limits in the dataset
					qui replace _ES = `mu_hat`i'' if _USE==3 & _BY==`byi'			// subgroup ES
					qui replace _seES = `se_mu_hat`i'' if _USE==3 & _BY==`byi'		// subgroup seES
					if "`reModel'"=="pl" {
						if "`t'"!="" {
							scalar `tstat`i'' = `tstat`i''*sqrt((`K`i''-1)/`Qr`i'')		// t correction
						}
						qui replace _LCI = `r(mu_hat_lci)' if _USE==3 & _BY==`byi'
						qui replace _UCI = `r(mu_hat_uci)' if _USE==3 & _BY==`byi'
					}
					else if "`t'"!="" {
						scalar `tstat`i'' = `tstat`i''*sqrt((`K`i''-1)/`Qr`i'')		// t correction
						scalar `tcrit' = invttail(`K`i''-1, .5-`level'/200)
						qui replace _LCI = `mu_hat`i'' - `tcrit'*`se_mu_hat`i''*sqrt(`Qr`i''/(`K`i''-1)) if _USE==3 & _BY==`byi'
						qui replace _UCI = `mu_hat`i'' + `tcrit'*`se_mu_hat`i''*sqrt(`Qr`i''/(`K`i''-1)) if _USE==3 & _BY==`byi'
					}
					else {		// use z-value
						qui replace _LCI = `mu_hat`i'' - `crit'*`se_mu_hat`i'' if _USE==3 & _BY==`byi'
						qui replace _UCI = `mu_hat`i'' + `crit'*`se_mu_hat`i'' if _USE==3 & _BY==`byi'
					}

					* Subgroup numbers of patients
					if `"`_NN'"'!=`""' {
						tempname totnpts`i'
						summ _NN if `touse', meanonly
						cap assert `r(N)'==`=`K`i'''
						if !_rc {
							scalar `totnpts`i'' = `r(sum)'
							qui replace _NN = `r(sum)' if _USE==3 & _BY==`byi'
						}
						else scalar `totnpts`i'' = .
					}
				}		// end if `"`_by'"'!=`""' & `"`ipdover'"'==`""' 
			}		// end if !_rc
			
			drop `touse'
			local ++n
			
		}		// end forvalues j=1/`overlen'
	}		// end forvalues i=1/`nby'
	
	* Overall
	tempname K
	qui gen ovwt=.
	if `"`_over'"'!=`""' {
		forvalues j=1/`overlen' {
			qui gen `touse' = (_USE==1)*(_OVER==`j')
			cap mata: GetEstimates("`touse'", "ovwt", "`reModel'", `maxtausq', `itol', `maxiter', `quadpts', `level')
			if _rc>=3000 {
				disp as err "Mata error"
				exit _rc
			}
			drop `touse'
		}
		scalar `K' = `countK'
	}
	else {
		qui gen `touse' = (_USE==1)
		cap mata: GetEstimates("`touse'", "ovwt", "`reModel'", `maxtausq', `itol', `maxiter', `quadpts', `level')
		if _rc>=3000 {
			disp as err "Mata error"
			exit _rc
		}
		scalar `K' = `r(K)'
		
		* Overall results for ipdmetan (already done for ipdover)
		if `"`ipdover'"'==`""' {
		
			* Store scalars
			foreach x in mu_hat se_mu_hat Q Qr /*K*/ sigmasq {
				tempname `x'
				scalar ``x'' = `r(`x')'
			}
			tempname tstat
			scalar `tstat' = `mu_hat'/`se_mu_hat'
			scalar `Qdiff' = `Q' - `Qsum'		// tempname already defined, set to zero as default (line 1333)
		
			* Store effect size and confidence limits in the dataset
			qui replace _ES = `mu_hat' if _USE==5			// overall ES
			qui replace _seES = `se_mu_hat' if _USE==5		// overall seES
			if "`reModel'"=="pl" {
				if "`t'"!="" {
					scalar `tstat' = `tstat'*sqrt((`K'-1)/`Qr')		// t correction	
				}
				tempname mu_hat_lci mu_hat_uci
				scalar `mu_hat_lci' = `r(mu_hat_lci)'
				scalar `mu_hat_uci' = `r(mu_hat_uci)'
				qui replace _LCI = `mu_hat_lci' if _USE==5
				qui replace _UCI = `mu_hat_uci' if _USE==5
			}
			else if "`t'"!="" {
				scalar `tstat' = `tstat'*sqrt((`K'-1)/`Qr')			// t correction	
				scalar `tcrit' = invttail(`K'-1, .5-`level'/200)
				qui replace _LCI = `mu_hat' - `tcrit'*`se_mu_hat'*sqrt(`Qr'/(`K'-1)) if _USE==5
				qui replace _UCI = `mu_hat' + `tcrit'*`se_mu_hat'*sqrt(`Qr'/(`K'-1)) if _USE==5
			}
			else {		// use z-value
				qui replace _LCI = `mu_hat' - `crit'*`se_mu_hat' if _USE==5
				qui replace _UCI = `mu_hat' + `crit'*`se_mu_hat' if _USE==5
			}
		
			* Heterogeneity stats
			tempname tausq HsqM Isq
			scalar `tausq' = `r(tausq)'
			scalar `Isq'=`tausq'/(`tausq'+`sigmasq')	// General formula for Isq (including D+L)
			scalar `HsqM'=`tausq'/`sigmasq'				// General formula for HsqM (including D+L)	
			
			if `"`overall'"'==`""' {
				return scalar Q=`Q'
				return scalar sigmasq=`sigmasq'
				return scalar tausq=`tausq'
				return scalar Isq=`Isq'
				return scalar HsqM=`HsqM'
			}
			
			if inlist(`"`reModel'"', "gq", "bs", "ml", "pl", "reml") {		// confidence limits for tausq, Isq and Hsq
				tempname tausq_lci tausq_uci HsqM_lci HsqM_uci Isq_lci Isq_uci
				scalar `tausq_lci' = `r(tausq_lci)'
				scalar `tausq_uci' = `r(tausq_uci)'
				
				scalar `HsqM_lci'=`tausq_lci'/`sigmasq'
				scalar `HsqM_uci'=`tausq_uci'/`sigmasq'

				scalar `Isq_lci'=`tausq_lci'/(`tausq_lci'+`sigmasq')
				scalar `Isq_uci'=`tausq_uci'/(`tausq_uci'+`sigmasq')
				
				if `"`overall'"'==`""' {
					local rc_tausq = `r(rc_tausq)'
					local rc_tausq_lci = `r(rc_tausq_lci)'
					local rc_tausq_uci = `r(rc_tausq_uci)'
					return scalar rc_tausq=`rc_tausq'					// whether tausq point estimate converged
					return scalar rc_tausq_lci=`rc_tausq_lci'			// whether tausq lower confidence limit converged
					return scalar rc_tausq_uci=`rc_tausq_uci'			// whether tausq upper confidence limit converged
					
					return scalar tausq_lci=`tausq_lci'
					return scalar tausq_uci=`tausq_uci'

					if "`reModel'"=="pl" {
						local rc_mu_lci = `r(rc_mu_lci)'
						local rc_mu_uci = `r(rc_mu_uci)'
						return scalar rc_mu_lci=`rc_mu_lci'				// whether mu_hat lower confidence limit converged
						return scalar rc_mu_uci=`rc_mu_uci'				// whether mu_hat upper confidence limit converged					
						
						return scalar mu_hat_lci=`mu_hat_lci'
						return scalar mu_hat_uci=`mu_hat_uci'					
					}
					if "`reModel'"=="bs" {
						tempname tausq_var
						scalar `tausq_var' = `r(tausq_var)'
						if `"`overall'"'==`""' return scalar tausq_var=`tausq_var'
					}
				}
			}
		}				// end if `"`ipdover'"'==`""'

		drop `touse'
	}					// end else (i.e. if `"`_over'"'==`""')

	* Total number of patients
	if `totnpts'==. & `"`_NN'"'!=`""' {
		summ _NN if _USE==1, meanonly
		cap assert `r(N)'==`=`K''
		if !_rc {
			scalar `totnpts' = `r(sum)'
			qui replace _NN = `r(sum)' if _USE==5
		}
	}
	
	* Finalise weights
	* and keep just one weight variable (drop the other)
	qui replace ovwt = 1 if _USE==5									// "overall weight" total (100%)
	if `"`_by'"'!=`""' {
		forvalues i=1/`nby' {
			local byi : word `i' of `bylist'
			qui replace sgwt = 1 if _USE==3 & _BY==`byi'			// "subgroup weight" total (100%)
			summ ovwt if _BY==`byi', meanonly
			qui replace ovwt = `r(sum)' if _USE==3 & _BY==`byi'		// subgroup-specific "overall weight" totals
		}
	}
	drop `=cond(`"`_by'"'!=`""' & `"`overall'"'!=`""' & `"`subgroup'"'==`""', "ovwt", "sgwt")'
	rename `=cond(`"`_by'"'!=`""' & `"`overall'"'!=`""' & `"`subgroup'"'==`""', "sgwt", "ovwt")' _WT
	
	
	*** Return other statistics

	* For matrix of coefficients, first need to sort.
	* Missing values in _BY may cause problems, so need to be careful!
	tempvar use5
	qui gen `use5' = (_USE==5)
	sort `use5' `_by' `_over' _USE `_source' `sgroup'

	* Return matrix of coefficients (with weights)
	tempname coeffs
	mkmat `_over' `_by' `_study' _ES _seES `_NN' _WT if inlist(_USE, 1, 2), matrix(`coeffs')
	return matrix coeffs=`coeffs'

	if `"`ADonly'"'==`""' {
		return local estvar `"`estvar'"'
		return local command `"`command'"'
	}
	return local dist `"`z'`t'"'			// test distribution for pooled effects - z or t
	if `"`byad'"'!=`""' {
		foreach x in K1 K2 totnpts1 totnpts2 mu_hat1 mu_hat2 se_mu_hat1 se_mu_hat2 {
			if `"``x''"'==`""' local `x'=.
		}
	}
	if `"`byad'"'!=`""' & `"`overall'"'!=`""' {
		return scalar k1=`K1'
		return scalar k2=`K2'
		return scalar n1=`totnpts1'
		return scalar n2=`totnpts2'
		return scalar mu_hat1=`mu_hat1'
		return scalar mu_hat2=`mu_hat2'
		return scalar se_mu_hat1=`se_mu_hat1'
		return scalar se_mu_hat2=`se_mu_hat2'
	}	
	else {
		if `"`totnpts'"'==`""' local totnpts=.
		return scalar k=`K'
		return scalar n=`totnpts'
		
		* Pooled estimates
		if `"`ipdover'"'==`""' & `"`overall'"'==`""' {
			return local re_model "`reModel'"
			return scalar mu_hat=`mu_hat'
			return scalar se_mu_hat=`se_mu_hat'
		}
	}

	

	********************************
	* Print summary info to screen *
	********************************
	
	* Full method names
	if "`reModel'"=="fe" local reDesc "Fixed-effects"
	else if "`reModel'"=="dl" local reDesc "Random-effects; DerSimonian-Laird estimator"
	else if "`reModel'"=="gq" local reDesc "Random-effects; Generalised Q estimator"
	else if "`reModel'"=="bs" local reDesc "Random-effects; Approximate Gamma estimator"
	else if "`reModel'"=="vc" local reDesc "Random-effects; ANOVA-type estimator"
	else if "`reModel'"=="sj" local reDesc "Random-effects; Sidik-Jonkman estimator"
	else if "`reModel'"=="ml" local reDesc "Random-effects; ML estimator"
	else if "`reModel'"=="pl" local reDesc "Random-effects; Profile ML estimator"
	else if "`reModel'"=="reml" local reDesc "Random-effects; REML estimator"
	
	* Print number of trials/patients to screen
	* (NB nos. actually analysed as opposed to the number supplied in original data)
	if `"`ADfile'"'!=`""' {
		if `"`byad'"'==`""' {
			tempname KIPD totnptsIPD
			qui count if inlist(_USE, 1, 2) & _SOURCE==1
			scalar `KIPD' = `r(N)'
			if `r(N)' {
				summ _NN if inlist(_USE, 1, 2) & _SOURCE==1, meanonly
				scalar `totnptsIPD' = cond(`r(N)', `r(sum)', .)			// if KIPD>0 but no _NN, set to missing
			}
			else scalar `totnptsIPD' = 0

			tempname KAD totnptsAD
			qui count if inlist(_USE, 1, 2) & _SOURCE==2
			scalar `KAD' = `r(N)'
			if `r(N)' {
				summ _NN if inlist(_USE, 1, 2) & _SOURCE==2, meanonly
				scalar `totnptsAD' = cond(`r(N)', `r(sum)', .)			// if KAD>0 but no _NN, set to missing
			}
			else scalar `totnptsAD' = 0
		}
		else {
			local KIPD `K1'
			local totnptsIPD `totnpts1'
			local KAD `K2'
			local totnptsAD `totnpts2'
		}
		disp as text _n "Trials included from IPD: " as res `KIPD'
		local dispnpts=cond(missing(`totnptsIPD'), "Unknown", string(`totnptsIPD'))
		disp as text "Patients included: " as res "`dispnpts'"
		
		disp as text _n "Trials included from aggregate data: " as res `KAD'
		local dispnpts=cond(missing(`totnptsAD'), "Unknown", string(`totnptsAD'))
		disp as text "Patients included: " as res "`dispnpts'"
	}
	else {
		disp _n _c
		if `"`ipdover'"'==`""' disp as text "Trials included: " as res `K'
		local dispnpts=cond(missing(`totnpts'), "Unknown", string(`totnpts'))
		disp as text "Patients included: " as res "`dispnpts'"
	}
	
	if `"`interaction'"'!=`""' local pooling "`pooltext' of interaction effect estimate"
	else if `"`exp_list'"'!=`""' local pooling "`pooltext' of user-specified effect estimate"
	else if `"`ADonly'"'!=`""' local pooling "`pooltext' of aggregate data"
	else local pooling "`pooltext' of main (treatment) effect estimate"
	
	di _n as text "`pooling'" as res " `estvareq'`estvar'"
	if `"`ipdover'"'==`""' {
		disp as text "using" as res " `reDesc'"
	}
	if `"`total'"'!=`""' {
		disp as err _n "caution: initial model fitting in full sample was suppressed"
	}
	if `"`pcmdname'"'!=`""' {
		disp as err _n "caution: prefix command supplied to ipdmetan. validity of estimates may be affected"
	}
	
	
	**************************
	* Print output to screen *
	**************************
	
	if `"`table'"'==`""' {

		* Find maximum length of labels in LHS column
		tempvar vlablen
		qui gen `vlablen' = length(_LABELS)
		if `"`_by'"'!=`""' {
			tempvar bylabels
			if `"`bylab'"'!=`""' qui decode _BY, gen(`bylabels')
			else qui gen `bylabels'=_BY
			qui replace `vlablen' = max(`vlablen', length(`bylabels'))
			drop `bylabels'
		}
		summ `vlablen', meanonly
		local lablen=`r(max)'
		drop `vlablen'
		
		if `"`_over'"'!=`""' {						// "over" variable labels
			forvalues h=1/`overlen' {
				local varlabopt `"`varlabopt' varlab`h'(`"`varlab`h''"')"'
				local len = length(`"`varlab`h''"')
				if `len'>`lablen' local lablen=`len'	
			}
		}
		
		* Prepare heterogeneity stats for presentation
		if `"`ipdover'"'==`""' & `"`overall'"'==`""' & (`"`_by'"'==`""' | `"`subgroup'"'!=`""') {
			local isqlist `Isq'
			local hsqlist `HsqM'
			local tausqlist `tausq'
			if inlist("`reModel'", "gq", "bs", "ml", "pl", "reml") {
				local isqlist `"`isqlist' `Isq_lci' `Isq_uci'"'
				local hsqlist `"`hsqlist' `HsqM_lci' `HsqM_uci'"'
				local tausqlist `"`tausqlist' `tausq_lci' `tausq_uci'"'
			}
			local isqlist `"isq(`isqlist')"'
			local hsqlist `"hsq(`hsqlist')"'
			local tausqlist `"tausq(`tausqlist')"'
		}
		if `"`_by'"'!=`""' & `"`subgroup'"'==`""' {
			forvalues i=1/`nby' {
				local Qlist `"`Qlist' `Q`i''"'
				local tstatlist `"`tstatlist' `tstat`i''"'		// test statistics for effect size
			}
		}
		if `"`ipdover'"'==`""' {
			local stitle `"`varlab1'"'
			if `"`stitle'"'==`""' local stitle "Study"
		}
		else local stitle "Subgroup"

		sort `use5' `_by' `_over' _USE `_source' `sgroup'
		qui gen long `obs'=_n
		DrawTable, sortby(`obs') overlen(`overlen') lablen(`lablen') stitle(`stitle') etitle(`effect') `varlabopt' `eform' ///
			bylab(`bylab') bylist(`bylist') q(`Q') qlist(`Qlist') qdiff(`Qdiff') tstat(`tstat') tstatlist(`tstatlist') `z' `t' ///
			`isqlist' `hsqlist' `tausqlist' `overall' `subgroup' `ipdover'
		drop `obs'
		
		if `"`messages'"'!=`""' {
			disp _n _c
			if inlist("`reModel'", "gq", "bs", "ml", "pl", "reml") {
			
				if "`reModel'"!="bs" {		// confidence interval only
					if `rc_tausq'==0 disp as text "tau{c 178} point estimate converged successfully"
					if `rc_tausq'==1 disp as err "tau{c 178} point estimate failed to converge within `maxiter' iterations"
					if `rc_tausq'>1 disp as err "tau{c 178} point estimate failed to converge: invalid search interval"
				}
				if `rc_tausq_lci'==0 disp as text "Lower confidence limit of tau{c 178} converged successfully"
				if `rc_tausq_lci'==1 disp as err "Lower confidence limit of tau{c 178} failed to converge within `maxiter' iterations"
				if `rc_tausq_lci'==2 disp as text "Lower confidence limit of tau{c 178} truncated at zero"
				if `rc_tausq_lci'==3 disp as text "Lower confidence limit of tau{c 178} greater than `maxtausq'"
				
				if `rc_tausq_uci'==0 disp as text "Upper confidence limit of tau{c 178} converged successfully"
				if `rc_tausq_uci'==1 disp as err "Upper confidence limit of tau{c 178} failed to converge within `maxiter' iterations"
				if `rc_tausq_uci'==2 disp as text "Upper confidence limit of tau{c 178} truncated at zero"
				if `rc_tausq_uci'==3 disp as text "Upper confidence limit of tau{c 178} greater than `maxtausq'"
				
				if "`reModel'"=="pl" {
					if `rc_mu_lci'==0 disp as text "Lower confidence limit of ES converged successfully"
					if `rc_mu_lci'==1 disp as err "Lower confidence limit of ES failed to converge within `maxiter' iterations"
					if `rc_mu_lci'>1 disp as err "Lower confidence limit of ES failed to converge: invalid search interval"
					
					if `rc_mu_uci'==0 disp as text "Upper confidence limit of ES converged successfully"
					if `rc_mu_uci'==1 disp as err "Upper confidence limit of ES failed to converge within `maxiter' iterations"
					if `rc_mu_uci'>1 disp as err "Upper confidence limit of ES failed to converge: invalid search interval"
				}
			}
		}
	}
	
			
			
	******************************************
	* Prepare dataset for graphics or saving *
	******************************************
	
	if `"`saving'"'!=`""' | `"`graph'"'==`""' {
		
		quietly {
			
			if `"`saving'"'!=`""' {
				* Would like to use _prefix_saving here,
				* but ipdmetan's 'saving' option has additional sub-options
				* so have to parse manually
				local 0 `saving'
				cap nois syntax anything(id="file name" name=filename) [, REPLACE STACKlabel]
				local rc = `c(rc)'
				if !`rc' {
					if "`replace'" == "" {
						local ss : subinstr local filename ".dta" ""
						confirm new file `"`ss'.dta"'
					}
				}
				if `rc' {
					di as err "invalid saving() option"
					exit `rc'
				}
			}
			
			* Apply variable labels and formats to lcols/rcols "returned data"
			forvalues i=1/`nr' {
				local temp : word `i' of `namesr'
				label var `temp' `"`nrlab`i''"'
			}
			
			* Variable name (title) for "_LABELS"
			if `"`ipdover'"'==`""' {
				if `"`varlab1'"'!=`""' & `"`stacklabel'"'==`""' {
					label var _LABELS `"`varlab1'"'
				}
				else label var _LABELS "Study ID"
			}
			else label var _LABELS "Subgroup"
			
			* Apply variable labels and formats to lcols/rcols only present in aggregate data
			forvalues i=1/`na' {
				local temp : word `i' of `ADvars'
				label var `temp' `"`nalab`i''"'
			}

			* Apply formats to lcols/rcols
			if `"`fmts'"'!=`""' {
				forvalues i=1/`ni' {
					local temp : word `i' of `lrcols'
					local fmti : word `i' of `fmts'
					cap confirm numeric var `temp'
					if !(_rc | `"`fmti'"'==`"null"') {
					format `temp' `fmti'
					}
				}
			}
				
			*** Insert extra rows for headings, labels, spacings etc.
			* Note: in the following routines, "half" values of _USE are used temporarily to get correct order
			*       and are then replaced with whole numbers at the end

			* Subgroup headings and spacings ("by", "over", both)
			if `"`_by'"'!=`""' | `"`_over'"'!=`""' {
				tempvar expand
				bysort `_by' `_over' : gen byte `expand' = 1 + 2*(_n==1)*(!`use5')
				expand `expand'
				gsort `_by' `_over' -`expand' _USE `_source' `sgroup'
				by `_by' `_over' : replace _USE=0 if `expand'>1 & _n==2		/* row for headings */
				by `_by' `_over' : replace _USE=4 if `expand'>1 & _n==3		/* row for blank line */
				if `"`_over'"'!=`""' {
					drop if _USE==0 & missing(_OVER)		/* ...but not needed for missing _over */
				}
				drop `expand'
				
				* Extra "by" subgroup headings if "over" also used
				if `"`_by'"'!=`""' & `"`_over'"'!=`""' {
					tempvar expand
					bysort _BY : gen byte `expand' =  1 + 3*(_n==1)*(!`use5')
					expand `expand'
					gsort _BY -`expand' _USE `_source' `sgroup'
					by _BY : replace _USE=-1 if `expand'>1 & _n==2  		/* row for "by" label (title) */
					by _BY : replace _USE=-0.5 if `expand'>1 & _n==3		/* row for blank line below title */
					by _BY : replace _USE=4.5 if `expand'>1 & _n==4		/* row for blank line to separate "by" groups */
					drop if _USE==4.5 & missing(_OVER)						/* ...but not needed for missing _OVER */
					replace _OVER=. if _USE==4.5
					drop `expand'
				}
			}

			* Subgroup spacings & heterogeneity ("by" only)
			if `"`_by'"'!=`""' & "`subgroup'"=="" {
				tempvar expand
				local x=0
				if `"`_over'"'!=`""' local x=1
				sort _BY `_over'
				by _BY : gen byte `expand' = 1 + (`HetOnNewLine')*(_n==_N)*(!`use5')
				expand `expand'
				gsort _BY -`expand' _USE `_source' `sgroup'
				if `"`_over'"'!=`""' {
					by _BY : replace _USE=2.5 if `expand'>1 & _n==2		/* row for blank line ("over" only) */
				}
				if `HetOnNewLine' {
					by _BY : replace _USE=3.5 if `expand'>1 & _n==2		/* extra row for het if lcols */
				}
				drop `expand'
			}
			
			* Overall heterogeneity - extra row if lcols
			if `"`overall'"'==`""' & `HetOnNewLine' {
				local newN = `=_N+1'
				set obs `newN'
				replace _USE=5.5 in `newN'
			}
			
			* Blank out effect sizes etc. in new rows
			foreach x of varlist _LABELS _ES _seES _LCI _UCI _WT `_NN' `lrcols' {
				cap confirm numeric var `x'
				if !_rc replace `x' = . if !inlist(_USE, 1, 2, 3, 5)
				else replace `x' = "" if !inlist(_USE, 1, 2, 3, 5)
			}
			replace `_study'=. if !inlist(_USE, 1, 2)
			if `"`_source'"'!=`""' replace `_source'=. if !inlist(_USE, 1, 2)

			*** Now insert label info into new rows
			* over() labels
			if `"`_over'"'!=`""' {
				forvalues h=1/`overlen' {
					replace _LABELS = `"`varlab`h''"' if _USE==0 & _OVER==`h'
					label define _OVER `h' `"`varlab`h''"', add
				}
				label values _OVER _OVER
			}
			
			* Extra row to contain what would otherwise be the leftmost column heading
			*   if `stacklabel' specified (i.e. so that heading can be used for forestplot stacking)
			else if `"`stacklabel'"' != `""' {
				local nobs1 = _N+1
				set obs `nobs1'
				replace _USE = -1 in `nobs1'
				replace `use5' = 0 in `nobs1'
				replace _LABELS = `"`varlab1'"' in `nobs1'
			}
			
			* "Overall" labels
			if `"`overall'"'==`""' {
				local ovlabel
				if `"`het'"'==`""' {				// if "nohet" not specified -- this implies no "over"
					local df=`K' - 1
					local qpval=chi2tail(`df', `Q')
					if "`ovstat'"=="q" {
						local ovlabel "(Q = " + string(`Q', "%5.2f") + " on `df' df, p = " + string(`qpval', "%5.3f") + ")"
					}
					else {
						local ovlabel "(I-squared = " + string(100*`Isq', "%5.1f")+ "%, p = " + string(`qpval', "%5.3f") + ")"
					}
					if `HetOnNewLine' {
						replace _LABELS = "`ovlabel'" if _USE==5.5
						local ovlabel				// ovlabel on line below so no conflict with lcols; then clear macro
					}
				}
				replace _LABELS = "Overall `ovlabel'" if _USE==5
			}
					
			* Subgroup ("by") headings & labels
			if `"`_by'"'!=`""' {
				forvalues i=1/`nby' {
					local byi: word `i' of `bylist'
							
					* Headings
					local bytext : label `bylab' `byi'
					if `"`_over'"'!=`""' replace _LABELS = "`bytext'" if _USE==-1 & _BY==`byi'
					else replace _LABELS = "`bytext'" if _USE==0 & _BY==`byi'
							
					* Labels + heterogeneity
					if `"`subgroup'"'==`""' {
					
						local ovlabel
						if `"`het'"'==`""' {		// if "nohet" not specified -- this implies no "over"
							local df = `K`i'' - 1
							local qpval = chi2tail(`df', `Q`i'')
								
							/* RMH I-squared added in next line
								RJH- also p-val as recommended by Mike Bradburn */
							if "`ovstat'"=="q" {
								local ovlabel "(Q = " + string(`Q`i'', "%5.2f") + " on `df' df, p = " + string(`qpval', "%5.3f") + ")"
							}
							else {
								local ovlabel "(I-squared = " + string(100*`Isq`i'', "%5.1f")+ "%, p = " + string(`qpval', "%5.3f") + ")"
							}
							if `HetOnNewLine' {
								replace _LABELS = "`ovlabel'" if _USE==3.5 & _BY==`byi'
								local ovlabel			// ovlabel on line below so no conflict with lcols; then clear macro
							}
						}
						replace _LABELS = "Subtotal `ovlabel'" if _USE==3 & _BY==`byi'
					}
				}
				* Add between-group heterogeneity info if appropriate
				if `"`ipdover'"'==`""' & `"`overall'"'==`""' & `"`het'"'==`""' {
					local nobs1 = _N+1
					set obs `nobs1'
					replace _USE = 4.5 in `nobs1'
					replace `use5' = 0 in `nobs1'
					local Qdiffp = chi2tail(`=`nby'-1', `Qdiff')
					replace _LABELS = "Heterogeneity between groups: p = " + string(`Qdiffp', "%5.3f") in `nobs1'
				}
			}
			
			order _USE `_by' `_over' `_source' `_study' _LABELS _ES _seES _LCI _UCI _WT `lcols' `rcols'
			sort `use5' `_by' `_over' _USE `_source' `sgroup'
			drop `use5' `sgroup'

			replace _USE = 0 if _USE == -1
			replace _USE = 4 if inlist(_USE, -0.5, -1.5, 2.5, 3.5, 4.5, 5.5)
			
		}	// end quietly

		* Save dataset 
		if `"`saving'"'!=`""' {
			qui save `filename', `replace'
		}		
		
		* Pass to forestplot
		if `"`graph'"'==`""' {

			if `"`ipdover'"'!=`""' {
				local fplotopts `"nowt `fplotopts'"'
				label var _NN "No. pts"
				local rcols `"_NN `rcols'"'		// Automatically add "_NN" to rcols if ipdover
			}
			if "`reModel'"=="fe" local reDesc		// no random-effects note if fixed-effects
			
			gettoken word1 rest : lcols
			if `"`word1'"'==`"`plotvar'"' local lcols `"`rest'"'
			
			forestplot, nopreserve `name' labels(_LABELS) by(`_by') plotid(`plotid') ///
				renote(`reDesc') `interaction' `eform' effect(`effect') lcols(`lcols') rcols(`rcols') `fplotopts'
		}

	}	// end if `"`saving'"'!=`""' | `"`graph'"'==`""'
	
	// restore held estimates
	if `:length local est_hold' {
		quietly estimates restore `est_hold' , drop
	}
	
end


********************************************************




*********************
* Stata subroutines *
*********************

* -parsecols-
* by David Fisher, August 2013

* Parses a list of "items" and outputs local macros for other programs (e.g. ipdmetan or collapse)
* Written for specific use within -ipdmetan-
*   identifying & returning expressions (e.g. "returned values" from regression commands)
*   identifying & returning "collapse-style" items to pass to collapse
*   identifying & returning labels (within quotation marks) and formats (%fmt) for later use

* N.B. Originally written (by David Fisher) as -collapsemat- , November 2012
* This did both the parsing AND the "collapsing", including string vars and saving to matrix or file.
* The current program instead *prepares* the data and syntax so that the official -collapse- command can be used.

program define parsecols, rclass
	version 8, missing
	syntax anything(name=clist id=clist equalok), RCOLS(integer) [BYAD]		// byad option implies var may not exist in memory
	
	local clist: subinstr local clist "[]" "", all
	local na=0					// counter of vars not in IPD (i.e. in aggregate dataset only)
	local nc=0					// counter of "collapse" vars
	local ncs=0					// counter of "collapse" vars that are strings (cannot be processed by -collapse-)
	local nr=0					// counter of "returned" vars
	local stat "null"			// GetOpStat needs a "placeholder" stat at the very least. Gets changed later if appropriate
	local fmt "null"			// placeholder format
	local fmtnotnull=0			// marker of whether *any* formatting has been specified
	local sortpreserve=0		// marker of whether sortpreserve is needed
	local flag=0
	
	* Each loop of "while" should process an "item group", defined as
	* [(stat)] [newname=]item [%fmt "label"]
	while `"`clist'"' != "" {
	
		gettoken next rest : clist, parse(`":"')
		if `"`next'"'==`":"' {
			local rcols=1					// colon indicates partition from lcols to rcols
			local clist `"`rest'"'
			if `"`clist'"'==`""' {
				continue, break
			}
		}
		
		* Get statistic
		if `flag'==0 {
			GetOpStat stat clist : "`stat'" `"`clist'"'
			local flag=1
		}

		* Get newname and/or format
		* Get next two tokens -- first should be a (new)name, second might be "=" or a format (%...)
		else if inlist(`flag',1,2) {
			gettoken next rest : clist, parse(`" ="') bind qed(qed1)
			gettoken tok2 rest2 : rest, parse(`" ="') bind qed(qed2)
			if `qed1' == 1 {			// quotes around first element
				disp as err `"Error in lcols or rcols syntax: check ordering/structure of elements"'
				exit 198
			}
			
			if `flag'==1 {
				if "`tok2'" == "=" {
					gettoken newname rest : clist, parse(" =")		// extract `newname'
					gettoken equal clist : rest, parse(" =")		// ...and start loop again
					continue
				}
				local flag=2
			}
			
			if `flag'==2 {
				if substr(`"`tok2'"',1,1)==`"%"' {		// var followed by format
					confirm format `tok2'
					local fmt `"`tok2'"'
					local fmtnotnull=1
					local clist : subinstr local clist "`tok2'" ""	// remove (first instance of) tok2 from clist and start loop again
					continue
				}
				local flag=3
			}
		}
		
		* Prepare variable itself (possibly followed with label in quotes)
		else if `flag'==3 {
		
			if `qed2' == 1 {			// quotes around second element ==> var followed by "Label"
				gettoken lhs rest : clist, bind
				gettoken rhs clist : rest, bind
			}
			else {						// var not followed by "Label"
				gettoken lhs clist : clist, bind
			}
			
			* Test whether `lhs' is a possible Stata variable name
			* If it is, assume "collapse"; if not, assume "returned statistic"
			cap confirm name `lhs'
			if _rc {
				* Assume returned statistic, in which case should be an expression within parentheses
				gettoken tok rest : lhs, parse("()") bind match(par)
				if `"`par'"'=="" {
					cap confirm name `lhs'
					if _rc==7 {
						disp as err "`lhs' invalid name or expression in lcols/rcols option"	// improve error message
						disp as err "check that expressions are enclosed in parentheses"
						exit _rc
					}
					else if _rc confirm name `lhs'
				}
				else {
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
			}
			
			* If "collapse", convert "ipdmetan"-style clist into "collapse"-style clist
			else {
				cap confirm var `lhs'		// this time test if it's an *existing* variable
				if _rc {
					if `"`byad'"'!=`""' {		// if "byad", non-existing variables are permissible
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
							local varlab=trim(`"`rhs'"')
							local rhs
						}
						else local varlab : var label `lhs'
						return local avarlab`na' = `"`varlab'"'		// return varlab
					}
					else confirm var `lhs'	// otherwise, output error message and break
				}
				else {
					* Sort out string vars
					cap confirm string var `lhs'
					if !_rc {
						local ++ncs
						if `"`newname'"'==`""' GetNewname newname : `"`lhs'"' `"`newnames'"'
						else if `"`: list newnames & newname'"' != `""' {
							disp as err "name conflict in lcols/rcols option"
							exit 198
						}
						local sidelist `"`sidelist' `rcols'"'		// add to (overall, ordered) list of "sides" (l/r)
						local newnames "`newnames' `newname'"		// add to (overall, ordered) list of newnames
						local oldnames "`oldnames' `lhs'"			// add to sub-list of original string varnames
						local itypes `"`itypes' cs"'				// add to (overall, ordered) list of "item types"
						local newfmts `"`newfmts' null"'			// add to (overall, ordered) list of formats
						if `"`rhs'"' != `""' {
							local varlab=trim(`"`rhs'"')
							local rhs
						}
						else local varlab : var label `lhs'
						return local csvarlab`ncs' = `"`varlab'"'	// return varlab
					}
					
					* Build "clist" expression for -collapse-
					else {
						local ++nc
						if `"`stat'"'==`"null"' {
							local stat "mean"				// otherwise default to "mean"
						}
						local keep `"`keep' `lhs'"'
						if `"`rhs'"' != `""' {
							local varlab=trim(`"`rhs'"')
							local rhs
						}
						else local varlab : var label `lhs'
						return local cvarlab`nc' = `"`varlab'"'			// return varlab
						local stat=subinstr(`"`stat'"',`" "',`""',.)	// remove spaces from stat (e.g. p 50 --> p50)
						
						if `"`newname'"'==`""' GetNewname newname : `"`lhs'"' `"`newnames'"'
						else if `"`: list newnames & newname'"' != `""' {
							disp as err "name conflict in lcols/rcols option"
							exit 198
						}					
						if trim(`"`fmt'"')==`"null"' {
							local fmt : format `lhs'						// use format of original var if none specified
						}
						local sidelist `"`sidelist' `rcols'"'				// add to (overall, ordered) list of "sides" (l/r)
						local newnames `"`newnames' `newname'"'				// add to (overall, ordered) list of newnames
						local itypes `"`itypes' c"'							// add to (overall, ordered) list of "item types"
						local newfmts `"`newfmts' `fmt'"'					// add to (overall, ordered) list of formats

						local cclist `"`cclist' (`stat') `newname'=`lhs'"'		// add to "collapse" clist

					}		// end  if !_rc (i.e. is `lhs' string or numeric)
				}		// end else (i.e. if `lhs' found in data currently in memory)
			}		// end else (i.e. if "collapse")

		local fmt = "null"
		local newname
		local flag=0
		}		// end else (i.e. "parse variable itself")
		
		else {
			disp as err `"Error in lcols or rcols syntax: check ordering/structure of elements"'
			exit 198
		}
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
	if `nc' {
		return local cclist=trim(itrim(`"`cclist'"'))		// "collapse" clist
	}
	if `ncs' {
		return local oldnames=trim(itrim(`"`oldnames'"'))	// original varnames for strings
	}
	if `nr' {
		return local rstatlist=trim(itrim(`"`rstatlist'"'))	// list of returned stats "as is"
	}

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
				

* The following subroutine has been modified slightly from its equivalent in the official "collapse.ado"
* by David Fisher, Sep 2013
program GetOpStat 
	args mstat mrest colon stat line

	gettoken thing nline : line, parse("() ") match(parens)
	
	* If `thing' is a single word in parentheses, check if it matches a single "stat" word
	if "`parens'"!="" & `:word count `thing'' == 1 {
		local 0 `", `thing'"'
		cap syntax [, mean median sd SEMean SEBinomial SEPoisson ///
			sum rawsum count max min iqr first firstnm last lastnm null]
		
		/* fix thing if abbreviated */
		if "`semean'" != "" local thing "semean"
		if "`sebinomial'" != "" local thing "sebinomial"
		if "`sepoisson'" != "" local thing "sepoisson"

		/* If syntax executed without error, simply update locals and exit */
		if _rc == 0 {
			c_local `mstat' `thing'
			c_local `mrest' `"`nline'"'
			if ("`median'"!="") c_local `mstat' "p 50"
			exit
		}
		
		/* If not, check for percentile stats */
		local thing = trim("`thing'")
		if (substr("`thing'",1,1) == "p") {
			local thing = substr("`thing'",2,.)
			cap confirm integer number `thing'
			if _rc==0 { 
				if 1<=`thing' & `thing'<=99 {
					c_local `mstat' "p `thing'"
					c_local `mrest' `"`nline'"'
					exit
				}
			}
		}
	}
		
	* Otherwise, assume `thing' is an expression (this will be tested later by _prefix_explist)
	* update locals and return to main loop
	c_local `mstat' "`stat'"
	c_local `mrest' `"`line'"'
		
end


* Routine to process aggregate data
prog define ProcessAD, rclass
	
	syntax [if] [in], VARS(varlist min=2 max=3 numeric) EXTERNAL(integer) ZTOL(real) LEVEL(real) ///
		STUDY(name) SORTBY(name) [SMAX(integer 0) STUDYLAB(name) ///
		BY(name) BYAD(string) BYLIST(numlist integer miss) BYMAX(integer 1) BYLAB(name) ///
		KEEP(namelist) NPTS(name) noSUBGROUP noOVERALL SMISSING BYMISSING]

	marksample touse
	cap confirm var `study'
	if !_rc & `"`smissing'"'==`""' markout `touse' `study', strok
	cap confirm var `by'
	if !_rc & `"`bymissing'"'==`""' markout `touse' `by', strok
	
	qui keep if `touse'
	qui count
	if !r(N) {
		disp as err "no valid observations in aggregate dataset"
		exit 2000
	}
	
	tempvar obsAD
	cap sort `sortby'			// if sort fails, defaults to current ordering ("_n")
	gen int `obsAD' = _n

	if `"`bylist'"'==`""' local bylist 1
	local nby : word count `bylist'
	
	** External aggregate data (to combine with IPD)
	if `external' {
		tempvar studyAD
		qui bysort `touse' (`obsAD') : gen int `studyAD' = _n + `smax'		// Generate sequential trial ID nos.
																			// following on from those of IPD trial
		qui count
		local ni=r(N)
		sort `obsAD'
		
		* Add aggregate data studies to value label
		cap confirm numeric var `study'
		if _rc>0 & _rc!=7 {					// if `study' not supplied, create dummy label using `add_id_new' values
			forvalues i=1/`ni' {
				local studyADi = `studyAD'[`i']
				label define `studylab' `studyADi' `"`studyADi'"', add
			}
		}
		else forvalues i=1/`ni' {
			local studyi = `study'[`i']
			local studyADi = `studyAD'[`i']
			if !_rc {								// `study' is numeric in AD dataset
				local studyname : label (`study') `studyi'
				label define `studylab' `studyADi' `"`studyname'"', add
			}
			else {									// `study' is string in AD dataset
				label define `studylab' `studyADi' `"`studyi'"', add
			}
		}
		
		* Same for subgroup value label (if applicable)
		* Here, if `by' is string, map onto the existing `bylab' using -encode-
		local nbyAD = 1					// default
		cap confirm numeric var `by'
		if !_rc {						// `by' is numeric in AD dataset
			tempvar bygroup
			qui bysort `by' : gen long `bygroup' = (_n==1)
			qui replace `bygroup' = sum(`bygroup')
			local nbyAD = `bygroup'[_N]
			
			sort `obsAD'
			forvalues i=1/`nbyAD' {
				summ `obsAD' if `bygroup'==`i', meanonly
				local val = `by'[`r(min)']
				
				local bylabADi : label (`by') `val'					// AD label value (not "strict")
				local bylabIPDi : label `bylab' `val', strict		// IPD label value
				if `"`bylabIPDi'"'==`""' & `val'!=. {
					label define `bylab' `val' "`bylabADi'", add
				}					
				else {
					local bylabADi : label (`by') `val', strict		// "strict" AD label value
					if `"`bylabIPDi'"'!=`"`bylabADi'"' & `"`bylabADi'"'!=`""' {
						disp as err `"Subgroup value label conflict at value `val'"'
						exit 198
					}
				}
				local bylistAD `"`bylistAD' `val'"'
			}
		}
		else if _rc==7 & `"`by'"'!=`""' {					// `by' is string in AD dataset
			tempvar by2
			encode `by', gen(`by2') label(`bylab')
			drop `by'
			rename `by2' `by'
			qui levelsof `by', local(bylistAD) miss
			local nbyAD : word count `bylistAD'
		}	// end if !_rc (cap confirm numeric var `by')

		else if `"`by'"'!=`""' | `"`byad'"'!=`""' {			// if `by' (or `byad') is specified but var does not yet exist in AD
			local ++bymax
			local bylistAD = `bymax'
			label define `bylab' `bymax' "Aggregate", add
			gen byte `by' = `bymax'
		}
		
		* Return modified bylist
		assert (`"`bylistAD'"'!=`""')==(`"`by'"'!=`""' | `"`byad'"'!=`""')	// check for if-and-only-if
		return local bylistAD `bylistAD'
		
	}	// end if `external'

	else local studyAD `study'		// if AD in memory, just point studyAD to existing study var

	
	** Parse `vars' namelist
	* Syntax is vars(ES seES) or vars(ES lci uci)
	* whether data is external or in memory
	local nvars : word count `vars'
	tokenize `vars'
	local _ES `1'
	if `nvars' == 2 local _seES `2'		// 2 vars ==> "ES seES"
	else {								// 3 vars ==> "ES lci uci"
		local _LCI `2'
		local _UCI `3'
	}

	* Derive standard error from confidence limits if appropriate
	cap confirm var `_seES'
	if _rc {
		cap assert `_LCI'<=`_ES' if !missing(`_LCI')
		if _rc {
			disp as err "Second variable assumed to contain lower confidence limit; error"
			exit 198
		}
		cap assert `_UCI'>=`_ES' if !missing(`_UCI')
		if _rc {
			disp as err "Third variable assumed to contain upper confidence limit; error"
			exit 198
		}
		tempvar _seES
		qui gen `_seES' = (`_UCI'-`_LCI')/(2*invnorm(.5 + `level'/200))
	}

	* Identify "bad" estimates and replace with missing if necessary
	qui replace `_ES'=.   if missing(`_ES') | missing(`_seES') | `_seES'==0 | (abs(`_ES'/`_seES')>0 & abs(`_ES'/`_seES')<`ztol')
	qui replace `_seES'=. if missing(`_ES') | missing(`_seES') | `_seES'==0 | (abs(`_ES'/`_seES')>0 & abs(`_ES'/`_seES')<`ztol')

	* Form final dataset
	qui ds
	local allvars `"`r(varlist)'"'
	local allvars : list allvars & keep		// vars passed from main routine
	
	sort `obsAD'
	keep `by' `studyAD' `_ES' `_seES' `npts' `allvars'
	cap rename `studyAD' _STUDY					// use "capture" in case variable already has that name (or doesn't exist, if appropriate)
	if `"`by'"'!=`""' cap rename `by' _BY
	cap rename `_ES' _ES
	cap rename `_seES' _seES
	cap rename `npts' _NN
	qui gen _USE = missing(_ES) + 1

	* `byad' ==> no subgroups in IPD ==> `pfile' has _BY=1.  Check that this doesn't conflict if `"`byad'"'==`"temp"'
	if `"`byad'"' == `"temp"' {
		qui count if _BY == 1
		if `r(N)' {
			qui summ _BY, meanonly
			qui replace _BY = _BY + 2 - `r(min)' if !missing(_BY)		// temporarily shift non-missing values
		}
	}
	
end






* Routine to draw output table
* Could be done using "tabdisp", but doing it myself means it can be tailored to the situation
* therefore looks better (I hope!)
program DrawTable

	syntax, SORTBY(varname) OVERLEN(integer) LABLEN(integer) STITLE(string asis) ETITLE(string asis) ///
		[BYLAB(name) BYLIST(numlist integer miss) Z T TSTAT(name) TSTATLIST(namelist) ///
		Q(name) QLIST(namelist) QDIFF(name) ISQ(namelist) HSQ(namelist) TAUSQ(namelist) ///
		EFORM noOVERALL noSUBGROUP IPDOVER *]
	
	if `overlen'>1 {						// if "over", parse "variable label" options
		forvalues h=1/`overlen' {
			local 0 `", `options'"'
			syntax, VARLAB`h'(string) *
		}
	}

	local uselen 21							// default (minimum)
	if `lablen'>21 local uselen=`lablen'
	if `lablen'>32 local uselen=32 			// maximum
	
	* Find maximum length of study title and effect title
	* Allow them to spread over several lines, but only up to a maximum number of chars
	* If a single line must be more than 32 chars, truncate and stop
	local line = 1
	TitleSpread `"`stitle'"', width(`uselen')
	local sline = `r(k)'
	forvalues i=1/`sline' {
		local stitle`i' `"`r(title`i')'"'
	}
	TitleSpread `"`etitle'"', width(10)
	local eline = `r(k)'
	local diff = `eline' - `sline'
	if `diff'<=0 {
		forvalues i=1/`sline' {
			local etitle`i' `"`r(title`=`i'+`diff'')'"'		// stitle uses most lines (or equal): line up etitle with stitle
		}
	}
	else {
		forvalues i=`eline'(-1)1 {					// run backwards, otherwise macros are deleted by the time they're needed
			local etitle`i' `"`r(title`i')'"'
			local stitle`i' `"`stitle`=`i'-`diff'''"'		// etitle uses most lines: line up stitle with etitle
		}
	}
	
	di as text _n "{hline `uselen'}{c TT}{hline 45}"
	
	local line = max(`eline', `sline')
	if `line'>1 {
		forvalues i=1/`=`line'-1' {
			di as text "`stitle`i''{col `=`uselen'+1'}{c |} " %~10s `"`etitle`i''"'
		}
	}
	if `"`ipdover'"'==`""' local final "% Weight"
	else local final "No. pts"
	di as text "`stitle`line''{col `=`uselen'+1'}{c |} " %~10s `"`etitle`line''"' "{col `=`uselen'+14'}[`c(level)'% Conf. Interval]{col `=`uselen'+37'}`final'"

		
	*** Loop over trials, and subgroups if appropriate
	local nby=1
	cap confirm var _BY
	if !_rc {
		local _by "_BY"
		local nby : word count `bylist'
	}
	cap confirm var _OVER
	if !_rc local _over "_OVER"
	cap confirm var _NN
	if !_rc local _NN "_NN"
	
	sort `sortby'
	tempvar touse
	forvalues i=1/`nby' {				// this will be 1/1 if no subgroups

		di as text "{hline `uselen'}{c +}{hline 45}"
			
		if `"`bylist'"'!=`""' {
			local byi : word `i' of `bylist'
			local bytext : label `bylab' `byi'
			di as text substr(`"`bytext'"', 1, `=`uselen'-1') + "{col `=`uselen'+1'}{c |}"

			summ _ES if _BY==`byi' & _USE==1, meanonly
			local K`i' = `r(N)'
		}
		if "`eform'"!=`""' local xexp `"exp"'

		forvalues h=1/`overlen' {
			
			qui gen `touse' = 1
			if `"`_by'"'!=`""' {
				qui replace `touse' = `touse'*(_BY==`byi')
			}
			if `"`_over'"'!=`""' {
				qui replace `touse' = `touse'*(_OVER==`h')
			}
			summ `sortby' if inlist(_USE, 1, 2) & `touse', meanonly
			if `overlen'>1 {
				di as text "{col `=`uselen'+1'}{c |}"
				if !r(N) local nodata "{col `=`uselen'+4'} (Insufficient data)"
				di as text substr(`"`varlab`h''"', 1, `=`uselen'-1') "{col `=`uselen'+1'}{c |}`nodata'"
				local nodata	// clear macro
			}
			if `r(N)' {
				local min=r(min)
				local max=r(max)
				forvalues k=`min'/`max' {
					local _ES_ = _ES[`k']
					local _LCI_ = _LCI[`k']
					local _UCI_ = _UCI[`k']
					local _labels_ = _LABELS[`k']

					if `"`ipdover'"'!=`""' {
						if `"`_NN'"'!=`""' {
							local final `"%7.0f `=_NN[`k']'"'
						}
					}
					else local final `"%7.2f `=100*_WT[`k']'"'
					
					if missing(`_ES_') {
						di as text substr(`"`_labels_'"',1, 32) "{col `=`uselen'+1'}{c |}{col `=`uselen'+4'} (Insufficient data)"
					}
					else {
						di as text substr(`"`_labels_'"',1, 32) "{col `=`uselen'+1'}{c |}{col `=`uselen'+4'}" ///
							as res %7.3f `=`xexp'(`_ES_')' "{col `=`uselen'+15'}" ///
							as res %7.3f `=`xexp'(`_LCI_')' "{col `=`uselen'+25'}" ///
							as res %7.3f `=`xexp'(`_UCI_')' "{col `=`uselen'+36'}" `final'
					}
				}
			}
			drop `touse'

		}		// end forvalues j=1/`overlen'

		* Subgroup effects
		if `"`bylist'"'!=`""' & `"`subgroup'"'==`""' {
			local byi: word `i' of `bylist'
			summ `sortby' if _BY==`byi' & _USE==3, meanonly
			if !r(N) local _ES_=.
			else {		
				local _ES_ = _ES[`r(min)']
				local _LCI_ = _LCI[`r(min)']
				local _UCI_ = _UCI[`r(min)']
			
				if `"`ipdover'"'!=`""' {
					if `"`_NN'"'!=`""' {
						local final `"%7.0f `=_NN[`r(min)']'"'
					}
				}
				else local final `"%7.2f `=100*_WT[`r(min)']'"'
			}

			di as text "{col `=`uselen'+1'}{c |}"
			if missing(`_ES_') {
				di as text "Subgroup effect{col `=`uselen'+1'}{c |}{col `=`uselen'+4'} (Insufficient data)"
			}
			else {
				di as text "Subgroup effect{col `=`uselen'+1'}{c |}{col `=`uselen'+4'}" ///
					as res %7.3f `=`xexp'(`_ES_')' "{col `=`uselen'+15'}" ///
					as res %7.3f `=`xexp'(`_LCI_')' "{col `=`uselen'+25'}" ///
					as res %7.3f `=`xexp'(`_UCI_')' "{col `=`uselen'+36'}" `final'
			}
		}
	}		// end forvalues i=1/`nby'
		

	*** Overall effect
	if `"`overall'"'==`""' {
		di as text "{hline `uselen'}{c +}{hline 45}"

		summ `sortby' if _USE==1, meanonly
		local K = `r(N)'
		local df = `K' - 1
	
		summ `sortby' if _USE==5, meanonly
		local _ES_ = _ES[`r(min)']
		local _LCI_ = _LCI[`r(min)']
		local _UCI_ = _UCI[`r(min)']
			
		if `"`ipdover'"'!=`""' {
			if `"`_NN'"'!=`""' {
				local final `"%7.0f `=_NN[`r(min)']'"'
			}
		}
		else local final `"%7.2f `=100*_WT[`r(min)']'"'
				
		di as text %-20s "Overall effect{col `=`uselen'+1'}{c |}{col `=`uselen'+4'}" ///
			as res %7.3f `=`xexp'(`_ES_')' "{col `=`uselen'+15'}" ///
			as res %7.3f `=`xexp'(`_LCI_')' "{col `=`uselen'+25'}" ///
			as res %7.3f `=`xexp'(`_UCI_')' "{col `=`uselen'+36'}" `final'
	}
	di as text "{hline `uselen'}{c BT}{hline 45}"

	* Tests, heterogeneity etc. -- only for pooled analysis (i.e. no ipdover)
	if `"`ipdover'"'==`""' {
		
		* Test of pooled effect equal to zero
		local null = (`"`eform'"'!=`""')
		
		if `"`bylist'"'==`""' | `"`subgroup'"'!=`""' {
			if `"`overall'"'==`""' {
				if `"`t'"'!=`""' local pvalue=2*ttail(`K'-1,abs(`tstat'))
				else local pvalue=2*normal(-abs(`tstat'))
				di as text _n "Test of overall effect = " as res `null' as text ":  `z'`t' = " ///
					as res %7.3f `tstat' as text "  p = " as res %7.3f `pvalue'
			}
		}
		else {
			di as text _n "Tests of effect size = " as res `null' as text":"
			forvalues i=1/`nby' {
				local byi: word `i' of `bylist'
				local bylabi : label `bylab' `byi'
			
				local tstati : word `i' of `tstatlist'
				if `"`tstati'"'!=`""' {
					if `"`t'"'!=`""' local pvalue=2*ttail(`K`i''-1,abs(`tstati'))
					else local pvalue=2*normal(-abs(`tstati'))
					di as text substr("`bylabi'", 1, `=`uselen'-1') "{col `=`uselen'+1'}`z'`t' = " as res %7.3f `tstati' as text "  p = " as res %7.3f `pvalue'
				}
				else di as text substr("`bylabi'", 1, `=`uselen'-1') "{col `=`uselen'+1'}(Insufficient data)"
			}

			if `"`overall'"'==`""' {
				if `"`t'"'!=`""' local pvalue=2*ttail(`K'-1,abs(`tstat'))
				else local pvalue=2*normal(-abs(`tstat'))
				di as text "Overall{col `=`uselen'+1'}`z'`t' = " as res %7.3f `tstat' as text "  p = " as res %7.3f `pvalue'
			}
		}
			
		* Heterogeneity measures box: no subgroups
		if `"`overall'"'==`""' & (`"`bylist'"'==`""' | `"`subgroup'"'!=`""') {
			di as text _n(2) "Heterogeneity Measures"

			* Q, I2, H2
			di as text "{hline `uselen'}{c TT}{hline 35}"
			di as text "{col `=`uselen'+1'}{c |}{col `=`uselen'+7'}Value{col `=`uselen'+18'}df{col `=`uselen'+25'}p-value"
			di as text "{hline `uselen'}{c +}{hline 35}"
				
			local qpval = chi2tail(`df', `q')
			di as text "Cochrane Q {col `=`uselen'+1'}{c |}{col `=`uselen'+5'}" ///
				as res %7.2f `q' "{col `=`uselen'+16'}" %3.0f `df' "{col `=`uselen'+23'}" %7.3f `qpval'
				
			if `: word count `tausq''==1 {
				di as text "I{c 178} (%) {col `=`uselen'+1'}{c |}{col `=`uselen'+4'}" as res %7.1f 100*`isq' "%"
				di as text "Modified H{c 178} {col `=`uselen'+1'}{c |}{col `=`uselen'+5'}" as res %7.3f `hsq'
				di as text "tau{c 178} {col `=`uselen'+1'}{c |}{col `=`uselen'+4'}" as res %8.4f `tausq'
			}
			else {		// display second box with CIs for tausq etc.
				foreach x in isq hsq tausq {
					tokenize ``x''
					local `x'_est `1'
					local `x'_lci `2'
					local `x'_uci `3'
				}
				di as text "{hline `uselen'}{c BT}{hline 35}"
				di as text _n "{hline `uselen'}{c TT}{hline 35}"
				di as text "{col `=`uselen'+1'}{c |}{col `=`uselen'+7'}Value{col `=`uselen'+15'}[`level'% Conf. Interval]"
				di as text "{hline `uselen'}{c +}{hline 35}"
				di as text "I{c 178} (%) {col `=`uselen'+1'}{c |}{col `=`uselen'+4'}" ///
					as res %7.1f 100*`isq_est' "%{col `=`uselen'+14'}" ///
					as res %7.1f 100*`isq_lci' "%{col `=`uselen'+24'}" %7.1f 100*`isq_uci' "%"
				di as text "Modified H{c 178} {col `=`uselen'+1'}{c |}{col `=`uselen'+5'}" ///
					as res %7.3f `hsq_est' "{col `=`uselen'+15'}" ///
					as res %7.3f `hsq_lci' "{col `=`uselen'+25'}" %7.3f `hsq_uci'
				di as text "tau{c 178} {col `=`uselen'+1'}{c |}{col `=`uselen'+4'}" ///
					as res %8.4f `tausq_est' "{col `=`uselen'+14'}" ///
					as res %8.4f `tausq_lci' "{col `=`uselen'+24'}" %8.4f `tausq_uci'
			}
			di as text "{hline `uselen'}{c BT}{hline 35}"
				
			* Display explanations
			di as text _n `"I{c 178} = between-study variance (tau{c 178}) as a percentage of total variance"'
			di as text `"Modified H{c 178} = ratio of tau{c 178} to typical within-study variance"'
		}

		* Heterogeneity measures box: subgroups (just present Q statistics)
		if `"`bylist'"'!=`""' & `"`subgroup'"'==`""' {
			
			di as text _n(2) "Q statistics for heterogeneity (calculated using Inverse Variance weights)"
			di as text "{hline `uselen'}{c TT}{hline 35}"
			di as text "{col `=`uselen'+1'}{c |}{col `=`uselen'+7'}Value{col `=`uselen'+17'}df{col `=`uselen'+24'}p-value"
			di as text "{hline `uselen'}{c +}{hline 35}"

			forvalues i=1/`nby' {
				local byi: word `i' of `bylist'
				local bylabi : label `bylab' `byi'
				if `"`bylabi'"'!="." local bylabi = substr(`"`bylabi'"', 1, `=`uselen'-1')
				
				local Q`i' : word `i' of `qlist'
				if `"`Q`i''"'!=`""' {
					local df`i' = `K`i'' - 1
					local qpval = chi2tail(`df`i'', `Q`i'')
					local dfcol = cond(`"`overall'"'==`""', 18, 16)
					di as text "`bylabi'{col `=`uselen'+1'}{c |}{col `=`uselen'+5'}" ///
						as res %7.2f `Q`i'' "{col `=`uselen'+`dfcol''}" %3.0f `df`i'' "{col `=`uselen'+23'}" %7.3f `qpval'
				}
				else di as text "`bylabi'{col `=`uselen'+1'}{c |}{col `=`uselen'+5'}(Insufficient data)"
			}
				
			if `"`overall'"'==`""' {
				local qpval = chi2tail(`df', `q')
				di as text "Overall{col `=`uselen'+1'}{c |}{col `=`uselen'+5'}" ///
					as res %7.2f `q' "{col `=`uselen'+18'}" %3.0f `df' "{col `=`uselen'+23'}" %7.3f `qpval'

				local qdiffpval = chi2tail(`=`nby'-1', `qdiff')
				di as text "Between{col `=`uselen'+1'}{c |}{col `=`uselen'+5'}" ///
					as res %7.2f `qdiff' "{col `=`uselen'+18'}" %3.0f `=`nby'-1' "{col `=`uselen'+23'}" %7.3f `qdiffpval'
					
				tempname Fstat
				scalar `Fstat' = (`qdiff'/(`nby'-1)) / (`q'/`df')
				local Fpval = Ftail(`=`nby'-1', `df', `Fstat')
				di as text "Between:Within (F){col `=`uselen'+1'}{c |}{col `=`uselen'+5'}" ///
					as res %7.2f `Fstat' "{col `=`uselen'+14'}" %3.0f `=`nby'-1' as text "," as res %3.0f `df' "{col `=`uselen'+23'}" %7.3f `Fpval'
			}
			di as text "{hline `uselen'}{c BT}{hline 35}"
		}
	}	// end if `"`ipdover'"'==`""'

end


* Subroutine to DrawTable: "spreads" titles out over multiple lines if appropriate
program TitleSpread, rclass

	syntax anything(name=title id="title string"), width(integer)

	local line = 1
	local titlelen = length(`title')
	local spread = int(`titlelen'/`width')+1
	if `spread'>1 {
		local end = 0
		local count = 1
		local last = word(`title', -1)

		local current = word(`title', `count')
		local next = word(`title', `=`count'+1')
					
		while `end' == 0 {
			local title`line' = `"`title`line''"' + " " + `"`current'"'		// add in the next word
			local check = `"`title`line''"' + `" `next'"'					// what next would be

			local ++count
			local current = word(`title', `count')
			if `"`current'"' == "" | length(`"`title`line''"') > 32 {
				local end = 1
				if length(`"`title`line''"') > 32 {
					local title`line' = substr(`"`title`line''"', 1, 31)
					local width=32
				}
			}
			if length(`"`title`line''"') > `titlelen'/`spread' ///
				| length(`"`check'"') > `titlelen'/`spread' {
				if `end' == 0 {
					local ++line
					local next = word(`title', `=`count'+1')
				}
			}
		}
	}
	else local title1 `title'
	
	* Return strings
	forvalues i=1/`line' {
		return local title`i' `"`title`i''"'
	}
	return scalar k = `line'
	
end




***********************************************



********************
* Mata subroutines *
********************


/* Meta-analysis pooling and heterogeneity statistics */
* References:
* Mittlboeck & Heinzl, Stat. Med. 2006; 25: 4321–33 "A simulation study comparing properties of heterogeneity measures in meta-analyses"
* Higgins & Thompson, Stat. Med. 2002; 21: 1539–58 "Quantifying heterogeneity in a meta-analysis"
mata:
void GetEstimates(string scalar touse, string scalar wtvec, string scalar model, real scalar maxtausq, real scalar itol, real scalar maxiter, real scalar quadpts, real scalar level)
{
	real colvector yi, se, vi, wi
	real scalar k, alpha, mu_hat, se_mu_hat, Q, Qr, c, sigmasq, tausq_m, tausq_dl, tausq

	st_view(yi=., ., "_ES", touse)
	if(length(yi)==0) {
		exit(error(111))
	}
	st_view(se=., ., "_seES", touse)
	vi=se:^2
	wi=1:/vi
	k=length(yi)
	alpha=(100-level)/100
	
	mu_hat = mean(yi, wi)					// fixed-effects estimate
	se_mu_hat = 1/sqrt(sum(wi))				// SE of fixed-effects estimate
	Q = crossdev(yi, mu_hat, wi, yi, mu_hat)	// standard Q statistic
	Qr = Q									// "random-effects" Q statistic
	c = sum(wi) - mean(wi,wi)				// c = S1 - (S2/S1)
	sigmasq = (k-1)/c						// "typical" within-study variance (cf Mittlboeck, Bowden)
	
	/* Initialise tau-squared */
	tausq_m = (Q-(k-1))/c							// untruncated DerSimonian & Laird estimator
	tausq_dl = max((0, tausq_m))
	
	/* Run models */
	real scalar rc_tausq, rc_tausq_lb, rc_tausq_ub, tausq_lci, tausq_uci, tausq_var, Q_crit_hi, Q_crit_lo, Q_var, d, crit

	if (model=="fe" | model=="dl") {
		tausq = tausq_dl
	}
	
	else if (model=="vc" | model=="sj") {		// "variance component" aka Cochran ANOVA-type estimator
		tausq = max((0, variance(yi) - mean(vi)))
		wi = 1:/(vi:+tausq)
		mu_hat = mean(yi, wi)
		se_mu_hat = 1/sqrt(sum(wi))
		Qr = crossdev(yi, mu_hat, wi, yi, mu_hat)
		
		if (model=="sj") {					// Sidik & Jonkman, with Cochran tausq as initial estimate
			tausq = tausq * Qr / (k-1)
		}
	}
	
	else if (model=="gq") {
		// Mandel/Paule method (J Res Natl Bur Stand 1982; 87: 377–85)
		// Generalised Q point estimate (e.g. DerSimonian & Kacker, Contemporary Clinical Trials 2007; 28: 105-114)
		// extension to CI by Viechtbauer (Stat Med 2007; 26: 37–52)
		// ... can be shown to be equivalent to the "empirical Bayes" estimator
		// (e.g. Sidik & Jonkman Stat Med 2007; 26: 1964-81)
		// and converges more quickly
		rc_tausq = mm_root(tausq=., &Q_crit(), 0, maxtausq, itol, maxiter, yi, vi, k, k-1)
	
		Q_crit_hi = invchi2(k-1, 1-(alpha/2))	// higher critical value to compare Q against (for *lower* bound of tausq)
		Q_crit_lo = invchi2(k-1, alpha/2)		// lower critical value to compare Q against (for *upper* bound of tausq)
		if (Q<Q_crit_lo) {			// Q falls below the lower critical value
			rc_tausq_lb = 2
			rc_tausq_ub = 2
			tausq_lci = 0
			tausq_uci = 0
		}		
		else {
			if (Q>Q_crit_hi) {		// Q is larger than the higher critical value, so can find lower bound using mm_root
				rc_tausq_lb = mm_root(tausq_lci=., &Q_crit(), 0, maxtausq, itol, maxiter, yi, vi, k, Q_crit_hi)
			}
			else {
				rc_tausq_lb = 2
				tausq_lci = 0			// otherwise, the lower bound for tausq is 0
			}
			/* Now find upper bound for tausq using mm_root */
			rc_tausq_ub = mm_root(tausq_uci=., &Q_crit(), tausq_lci, maxtausq, itol, maxiter, yi, vi, k, Q_crit_lo)
		}
	}

	else if (model=="bs") {
		// Confidence interval around D&L tau-squared using approximate Gamma distribution for Q
		// based on paper by Biggerstaff and Tweedie (Stat Med 1997; 16: 753–68)
		tausq = tausq_dl

		/* Estimate variance of tausq */
		d = cross(wi,wi) - 2*mean(wi:^2,wi) + (mean(wi,wi)^2)
		Q_var = 2*(k-1) + 4*c*tausq_m + 2*d*(tausq_m^2)
		tausq_var = Q_var/(c^2)
		st_numscalar("r(tausq_var)", tausq_var)

		/* Find confidence limits for tausq */
		rc_tausq_lb = mm_root(tausq_lci=., &gamma_crit(), 0, maxtausq, itol, maxiter, tausq_m, k, c, d, 1-(alpha/2))
		rc_tausq_ub = mm_root(tausq_uci=., &gamma_crit(), tausq_lci, maxtausq, itol, maxiter, tausq_m, k, c, d, alpha/2)
		
		/* Find weights and standard error for mu_hat */
		real scalar lambda, r
		lambda = ((k-1) + c*tausq_m)/(2*(k-1) + 4*c*tausq_m + 2*d*(tausq_m^2))
		r = ((k-1) + c*tausq_m)*lambda
		for(i=1; i<=k; i++) {
			params = (vi[i], lambda, r, c, k)
			if (i==1) wsi = integrate(&Intgrnd(), 0, ., quadpts, params)
			else wsi = wsi \ integrate(&Intgrnd(), 0, ., quadpts, params)
		}
		
		wi = wi*gammap(r, lambda*(k-1)) :+ wsi
		mu_hat = mean(yi, wi)
		se_mu_hat = sqrt(sum(wi:*wi:*(vi :+ tausq_m)) / (sum(wi)^2))
		Qr = crossdev(yi, mu_hat, wi, yi, mu_hat)		// "random-effects Q"
	}

	else if (model=="ml" | model=="pl") {
		real scalar ll_ml, wi_ml, mu_hat_ml
		
		// Iterative point estimates for tausq and mu_hat using ML
		rc_tausq = mm_root(tausq=., &ML_est(), 0, maxtausq, itol, maxiter, yi, vi)
		wi = 1:/(vi:+tausq)
		mu_hat = mean(yi, wi)
		se_mu_hat = 1/sqrt(sum(wi))
		Qr = crossdev(yi, mu_hat, wi, yi, mu_hat)		// "random-effects Q"
			
		// Calculate ML log-likelihood value
		ll_ml = 0.5*sum(ln(wi)) - 0.5*crossdev(yi, mu_hat, wi, yi, mu_hat)
		crit = ll_ml - (invchi2(1, level/100)/2)
		
		// Confidence interval for tausq using likelihood profiling
		rc_tausq_lb = mm_root(tausq_lci=., &ML_profile_tausq(), 0, tausq, itol, maxiter, yi, vi, crit)
		rc_tausq_ub = mm_root(tausq_uci=., &ML_profile_tausq(), tausq, maxtausq, itol, maxiter, yi, vi, crit)
	
		if (model=="pl") {
			// Confidence interval for mu_hat using likelihood profiling
			// (use four times the D+L RE lci and uci for search limits)
			real scalar wi_dl, mu_hat_dl, llim, ulim, mu_hat_lci, mu_hat_uci, rc_mu_lb, rc_mu_ub
			wi_dl = 1:/(vi:+tausq_dl)
			mu_hat_dl = mean(yi, wi)
			llim = mu_hat_dl - 4*1.96/sqrt(sum(wi_dl))
			ulim = mu_hat_dl + 4*1.96/sqrt(sum(wi_dl))
			rc_mu_lb = mm_root(mu_hat_lci=., &ML_profile_mu(), llim, mu_hat_ml, itol, maxiter, yi, vi, crit, maxtausq, itol, maxiter)
			rc_mu_ub = mm_root(mu_hat_uci=., &ML_profile_mu(), mu_hat_ml, ulim, itol, maxiter, yi, vi, crit, maxtausq, itol, maxiter)
		}
	}
	
	else if (model=="reml") {
		real scalar ll_reml, wi_reml, mu_hat_reml

		// Iterative tau-squared using REML
		rc_tausq = mm_root(tausq=., &REML_est(), 0, maxtausq, itol, maxiter, yi, vi)
		wi = 1:/(vi:+tausq)
		mu_hat = mean(yi, wi)
		se_mu_hat = 1/sqrt(sum(wi))
		Qr = crossdev(yi, mu_hat_ml, wi_ml, yi, mu_hat_ml)		// "random-effects Q"
		
		// Confidence interval using likelihood profiling
		ll_reml = 0.5*sum(ln(wi)) - 0.5*ln(sum(wi)) - 0.5*crossdev(yi, mu_hat, wi, yi, mu_hat)
		crit = ll_reml - (invchi2(1, level/100)/2)
		rc_tausq_lb = mm_root(tausq_lci=., &REML_profile(), 0, tausq, itol, maxiter, yi, vi, crit)
		rc_tausq_ub = mm_root(tausq_uci=., &REML_profile(), tausq, maxtausq, itol, maxiter, yi, vi, crit)
	}
	
	else {
		printf("Invalid type")
	}

	/* Calculate final results if not done yet */
	if (model=="dl" | model=="sj" | model=="gq") {
		wi=1:/(vi:+tausq)
		mu_hat=mean(yi, wi)							// random-effects estimate
		se_mu_hat=1/sqrt(sum(wi))					// SE of random-effects estimate
		Qr = crossdev(yi, mu_hat, wi, yi, mu_hat)	// "random-effects Q"
	}
	
	/* Output matrix plus weights, and summary stats */
	/* wi=editmissing(wi,0)*/				// replace missing weights with zeroes (for matrix manipulation in Stata)
	wi=wi/sum(wi)						// normalise weights

	st_store(st_viewobs(yi), wtvec, wi)	// store weights
	st_numscalar("r(mu_hat)", mu_hat)
	st_numscalar("r(se_mu_hat)", se_mu_hat)
	st_numscalar("r(Q)", Q)
	st_numscalar("r(Qr)", Qr)	
	st_numscalar("r(K)", k)
	st_numscalar("r(tausq)", tausq)
	st_numscalar("r(sigmasq)", sigmasq)

	if (model=="gq" | model=="bs" | model=="ml" | model=="pl" | model=="reml") {
		st_numscalar("r(rc_tausq)", rc_tausq)
		st_numscalar("r(tausq_lci)", tausq_lci)
		st_numscalar("r(tausq_uci)", tausq_uci)
		st_numscalar("r(rc_tausq_lci)", rc_tausq_lb)
		st_numscalar("r(rc_tausq_uci)", rc_tausq_ub)

		if (model=="pl") {
			st_numscalar("r(mu_hat_lci)", mu_hat_lci)
			st_numscalar("r(mu_hat_uci)", mu_hat_uci)
			st_numscalar("r(rc_mu_lci)", rc_mu_lb)
			st_numscalar("r(rc_mu_uci)", rc_mu_ub)
		}
	}
}

end


*** Iteration functions

mata:

/* Generalised Q */
real scalar Q_crit(real scalar tausq, real colvector yi, real colvector vi, real scalar k, real scalar crit) {
	real colvector wi
	real scalar mu_hat, newtausq

	wi=1:/(vi:+tausq)
	mu_hat=mean(yi, wi)
	newtausq = (k/crit)*crossdev(yi, mu_hat, wi, yi, mu_hat)/sum(wi) + mean(vi, wi)
	return(tausq-newtausq)
}

/* Approximate Gamma - tausq */
real scalar gamma_crit(real scalar tausq, real scalar tausq_m, real scalar k, real scalar c, real scalar d, real scalar crit) {
	real scalar lambda, r, limit

	lambda=((k-1) + c*tausq)/(2*(k-1) + 4*c*tausq + 2*d*(tausq^2))
	r=((k-1) + c*tausq)*lambda
	limit=lambda*(c*tausq_m + (k-1))
	return(gammap(r,limit)-crit)
}
/* Approximate Gamma - mu_hat */
real rowvector Intgrnd(real rowvector t, real rowvector params) {
	real scalar s, lambda, r, c, k, ans

	s = params[1,1]
	lambda = params[1,2]
	r = params[1,3]
	c = params[1,4]
	k = params[1,5]
	ans = c*(1:/(s:+t)) * (lambda^r / gamma(r)) :* (c*t :+ (k-1)):^(r-1) :* exp(-lambda*(c*t :+ (k-1)))
	return(ans)
}

/* ML */
real scalar ML_est(real scalar tausq, real colvector yi, real colvector vi, | real scalar mu_hat) {
	real colvector wi
	real scalar newtausq

	wi=1:/(vi:+tausq)
	if (mu_hat==.) mu_hat=mean(yi, wi)
	newtausq = crossdev(yi, mu_hat, wi:^2, yi, mu_hat)/sum(wi:^2) - mean(vi, wi:^2)
	return(tausq-newtausq)
}
/* ML profiling - tausq */
real scalar ML_profile_tausq(real scalar tausq, real colvector yi, real colvector vi, real scalar crit) {
	real colvector wi
	real scalar mu_hat, ll

	wi=1:/(vi:+tausq)
	mu_hat=mean(yi, wi)
	ll = 0.5*sum(ln(wi)) - 0.5*crossdev(yi, mu_hat, wi, yi, mu_hat)
	return(ll-crit)
}
/* ML profiling - mu */
real scalar ML_profile_mu(real scalar mu, real colvector yi, real colvector vi, real scalar crit, real scalar maxtausq, real scalar itol, real scalar maxiter) {
	real colvector wi
	real scalar tausq, rc, ll

	rc=mm_root(tausq=., &ML_est(), 0, maxtausq, itol, maxiter, yi, vi, mu)
	wi=1:/(vi:+tausq)
	ll = 0.5*sum(ln(wi)) - 0.5*crossdev(yi, mu, wi, yi, mu)
	return(ll-crit)
}

/* REML */
real scalar REML_est(real scalar tausq, real colvector yi, real colvector vi) {
	real colvector wi
	real scalar mu_hat, newtausq

	wi=1:/(vi:+tausq)
	mu_hat=sum(wi:*yi)/sum(wi)
	newtausq = crossdev(yi, mu_hat, wi:^2, yi, mu_hat)/sum(wi:^2) - mean(vi, wi:^2) + (1/sum(wi)) 
	return(tausq-newtausq)
}
/* REML profiling */
real scalar REML_profile(real scalar tausq, real colvector yi, real colvector vi, real scalar crit) {
	real colvector wi
	real scalar mu_hat, ll
	
	wi=1:/(vi:+tausq)
	mu_hat=sum(wi:*yi)/sum(wi)
	ll = 0.5*sum(ln(wi)) - 0.5*ln(sum(wi)) - 0.5*crossdev(yi, mu_hat, wi, yi, mu_hat)
	return(ll-crit)
}

end



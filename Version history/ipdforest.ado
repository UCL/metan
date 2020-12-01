/*  IPD forest plot post-estimation module by Evangelos Kontopantelis and David Reeves
    creted in STATA v11.2
    v1.1, 10 Feb 2012
        - controlling for trial specific estimates for both intercept and baseline (fets - i. notation not allowed).
        - controlling for fixed common baseline
        - compatible with multiple imputation commands
        - compatible with bootstrap commands
        - increased allowed study label length
    v1.2, 13 Feb 2012
        - interaction terms option added with multiple outputs
        - corrected error when factor control/intervention variable was used (was returning not found as a random-effect error)
        - corrected error when fe variable was inputted in factor notation
        - should not be used with xi: xtmixed or xi: xtlogit since it was causing many problems. Users should generate the interactions manually,
          using the xi command before executing the regression command OR use the latest factor variable notation.
        - gsavedir gsavename eps gph (graph) options added
    v1.3, 12 Apr 2012
        - corrected graphs for binary/continuous interactions (only report studies of respective category)
    v1.4, 27 Sep 2012
        - continuous exposure variable allowed
        - included 'auto' option for running command in an easier way
        - added export option
    v1.5, 21 Jan 2013
        - Corrected tabulation error when exposure is continuous
        - Fixed hardcoded study identifier 'studyid', any acceptable variable name can now be used
        - Added small diamond for effects
    v1.6, 12 Apr 2014
        - Added CIs for I^2 and H^2: calculated from tau^2 estimate"

        need to update mi impute compatibility: currently only using the 1st imputation to calculate the study effects, need to do for all
*/

/*stata version define*/
version 11.2

/*IPDforest displays a forest plot following an xtmixed or similar estimation command*/
program define ipdforest, rclass
    /*command syntax*/
    syntax varlist (fv max=1 numeric), [fe(varlist fv min=1 numeric) re(varlist fv min=1 numeric) fets(namelist min=1 max=2) /*
    */ ia(varlist fv min=1 max=1 numeric) auto label(varlist min=1 max=2) or gsavedir(string) gsavename(string) eps gph export(string)]
    /*temp variables used in all methods*/
    tempvar sidrev weights eff1 lo95CI1 up95CI1 outcvar2 tempdummy lblsize cnst xb1 esample
    tempfile tempf tempsave tempimp

    /*INITIAL STUFF*/
    /*make sure xtmixed or xtmelogit has been executed first*/
    /*take into account multiple imputation commands*/
    di _newline(2)
    local temp = e(mi)
    if "`temp'"=="mi" {
        local cmloc = "cmd_mi"
        /*make sure -mi estimate- has been used with the -post- option*/
        if strpos(e(cmdline_mi),"post")==0 {
            di as error "ipdforest requires the use of the post option in mi estimate"
            error 301
        }
        if strpos(e(cmdline_mi),"esample")==0 {
            di as error "ipdforest requires the use of the esample() option in mi estimate"
            error 301
        }
        /*mi can only be flong or flongsep*/
        qui mi query
        if r(style)!="flong" & r(style)!="flongsep" {
            di as error "mi must be in long or flonsep format"
            error 301
        }
        /*get esample variable name*/
        local i=1
        while strpos(word(e(cmdline_mi),`i'),"esample")==0 {
            local i = `i'+1
        }
        local tvnm = word(e(cmdline_mi),`i')
        scalar startpos = strpos("`tvnm'","(")
        scalar endpos = strpos("`tvnm'",")")
        local tvnm = substr("`tvnm'",`=startpos+1',`=endpos-startpos-1')
        /*need to sort out esample() before converting back - only keeping cases for the first imputation*/
        qui save `tempimp', replace
        qui keep if _mi_m==1
        qui gen `esample'=`tvnm'
    }
    else {
        local cmloc = "cmd"
        qui save `tempimp', replace
        qui gen `esample' = e(sample)
    }
    qui keep if `esample'==1
    if !inlist(e(`cmloc'), "xtmelogit", "xtmixed") {
        di as error "ipdforest works as a post-estimation command for xtmixed or xtmelogit"
        error 301
    }
    local modelsel = e(`cmloc')

    if "`or'"!="" {
        if "`modelsel'"!="xtmelogit" {
    	   di as error "Odds Ratios can only be selected following the xtmelogit command"
    	   error 321
        }
        local orstr = "ORs"
        local plval = 1
    }
    else {
        local orstr = "coefficients"
        local plval = 0
    }
    /*get the study identifier - only one allowed*/
    if `=wordcount(e(ivars))'!=1 {
        di as error "Only a two-level random-effect structure is allowed"
        error 321
    }
    else {
        local clustervar = e(ivars)
    }
    local methtype = e(method)
    /*get outcome name*/
    local outcomevar = e(depvar)
    /*deal with exposure format (i.* or not)*/
    local var1 = "`varlist'"
    scalar vcnt = 1
    scalar ipref = 0
    if strpos("`var1'","i.")==1 {
        scalar ipref = 1
        local var1 = substr("`var1'",3,.)
    }

    /*find beginning of random effects substring*/
    scalar fp = strpos(e(cmdline),"||")
    /*the auto option*/
    if "`auto'"!="" {
        /*make sure conflicting options are not given*/
        if "`fe'"!="" | "`re'"!="" | "`fets'"!="" | "`ia'"!="" {
    	   di as error "auto option cannot be executed with fe(), re(), fets() or ia()"
    	   error 197
        }
        /*check length of command line string - if 245 it's over the limit*/
        scalar cmdstrlen = length("`e(cmdline)'")
        if cmdstrlen >= 244 {
    	   di as error "Previous command info string over the limit and the auto option cannot be used"
    	   error 197
        }
        else {
            /*identify settings - need to be compatible with mi estimate*/
            /*RE*/
            local substr2 = e(revars)
            /*remove exposure*/
            local substr2 = subinstr("`substr2'","`var1'","",.)
            local substr2 = trim("`substr2'")
            local substr2 = itrim("`substr2'")
            /*get in words*/
            forvalues i=1(1)`=wordcount("`substr2'")' {
                local reff`i' = word("`substr2'",`i')
            }
            /*FE and FETS*/
            local substr1 = "`=substr(e(cmdline),1,fp-1)' "
            /*remove model*/
            local substr1 = subinstr("`substr1'","`modelsel'"," ",.)
            /*remove outcome, exposure*/
            foreach x in outcomevar var1 {
                local substr1 = subinstr(" `substr1'"," ``x'' "," ",.)
            }
            /*remove random effects*/
            forvalues i=1(1)`=wordcount("`substr2'")' {
                local substr1 = subinstr(" `substr1'"," `reff`i'' "," ",.)
            }

            /*re - the easiest*/
            local re = "`substr2'"
            /*fets - study intercepts (i* format)*/
            local fets=""
            if strpos("`substr1'", "i."e(ivars))>0 {
                local fets = e(ivars)
                /*remove from independent var string*/
                local substr1 = subinstr(" `substr1'"," i.`=e(ivars)' "," ",.)
            }
            /*everything else is expanded fets (underscore), fixed effect, or interaction*/
            local fe=""
            local ia=""
            scalar dumcnt = 0
            forvalues i=1(1)`=wordcount("`substr1'")' {
                local tempword = word("`substr1'",`i')
                /*need to see if it's an interaction term here since can't use descr*/
                if (strpos("`tempword'","*")>0 & strpos("`tempword'",".")>0) | strpos("`tempword'","#")>0 {
                    local itemp = "`tempword'"
                    foreach x in "*" "#" "i.`var1'" "c." "`var1'" {
                        local itemp = subinstr("`itemp'","`x'","",.)
                    }
                    if "`ia'"=="" {
                        local ia = "`itemp'"
                    }
                    else {
                	    di as error "auto option allows only one interaction term"
                	    di as error "set ipdforest manually if your model includes more"
                	    error 197
                    }
                }
                /*if it's a factor variable - can be fe / ia only*/
                else if (strpos("`tempword'","i.")>0 & strpos("`tempword'","*")==0) & strpos("`tempword'","#")==0 {
                    /*dummy variables interaction notation*/
                    if strpos("`tempword'","X")>0 & strpos("`tempword'","_I")>0 {
                	    di as error "auto option not compatible with dummy variable interaction notation"
                	    di as error "set ipdforest manually or use interaction notation (fv preferable)"
                	    error 197
                    }
                    /*if not interaction probably a fe*/
                    else {
                        if strpos("`tempword'","_")>0 {
                    	    di as error "ipdforest does not allow the use of underscores in fe covariate names"
                    	    di as error "please rename variable `tempword' if included as fixed-effect covariate"
                            di as error "if variable to be included unders fets, include with other study-specific variables,"
                            di as error "e.g. dept0s_* or dept0s_1-dept0s_16"
                    	    error 197
                        }
                        local fe = "`fe' `tempword'"
                    }
                }
                /*for everything else*/
                else {
                    qui descr `tempword', varlist
                    /*get the returned varlist and use - should be either 1 or stdnum (or the string might be 243 char long...)*/
                    local tvlist = r(varlist)
                    scalar tvlistcnt = length(r(varlist))
                    /*if there's only one word - either fixed effect or not allowed interaction*/
                    if wordcount(r(varlist))==1 {
                        /*dummy variables interaction notation*/
                        if strpos("`tempword'","X")>0 & strpos("`tempword'","_I")>0 {
                    	    di as error "auto option not compatible with dummy variable interaction notation"
                    	    di as error "set ipdforest manually or use interaction notation (fv preferable)"
                    	    error 197
                        }
                        /*if not interaction probably a fe*/
                        else {
                            if strpos("`tempword'","_")>0 {
                        	    di as error "ipdforest does not allow the use of underscores in fe covariate names"
                        	    di as error "please rename variable `tempword' if included as fixed-effect covariate"
                                di as error "if variable to be included unders fets, include with other study-specific variables,"
                                di as error "e.g. dept0s_* or dept0s_1-dept0s_16"
                        	    error 197
                            }
                            local fe = "`fe' `tempword'"
                        }
                    }
                    /*if there are more than one words returned in the varlist then fets or fe*/
                    else {
                        /*if underscore in the first variable returned then assume fets*/
                        scalar undpos = strpos("`=word(r(varlist),1)'","_")
                        if undpos>0 {
                            local tmpstr = substr("`=word(r(varlist),1)'",1,undpos)
                            local fets = "`fets' `tmpstr'"
                        }
                        /*if no underscore assume fe*/
                        else {
                            /*need to grab them all*/
                            if tvlistcnt>=243 {
                        	    di as error "the auto option does not support such a long list of fe vars"
                        	    di as error "please set the ipdforest manually"
                        	    error 197
                            }
                            else {
                                forvalues j=1(1)`=wordcount(r(varlist)) {
                                    local fe = "`fe' `=word(r(varlist),`j')'"
                                }
                            }
                        }
                    }
                    /*end for number of returned variables by each word IF clause*/
                }
                /*end of interaction IF clause*/
            }
            /*end of word loop*/
            /*trim final strings just in case*/
            foreach x in fe fets re ia {
                local x = trim("``x''")
                local x = itrim("``x''")
            }
            di in green "Model specification identified through auto option:"
            foreach x in fe fets re ia {
                di in green _col(5) "`x':" _col(12) as res "``x''"
            }
        }
    }

    /*do not allow expanded form to make life simpler (eg checking if has been included as a random effect)*/
    else if strpos("`var1'","_")==1 {
        di as error "Exposure variable cannot be in expanded form"
        di as error "Use factor notation or include as is (eg group or i.group; not _Igroup_1)"
        error 197
    }
    /*make sure intervention variable is binary with value of 0 and 1 if included with an i.* prefix*/
    capture tab `var1'
    if _rc==0 {
        scalar catnum = r(r)
        if catnum<2 {
            di as error "No variation at all in exposure variable `var1'"
            error 197
        }
    }
    else {
        scalar catnum = .
    }
    /*assuming a binary exposure variable for now*/
    scalar binexp = 1
    qui sum `var1'
    /*if i.* prefix can only be binary*/
    if ipref==1 {
        if catnum!=2 | r(min)!=0 | r(max)!=1 {
            di as error "Variable `var1' (exposure) can only be binary in the expanded format (0=control, 1=intervention)"
            error 197
        }
        else {
            di as error "Warning:" _col(12) as res "Binary exposure variable `var1' used"
        }
    }
    /*if no prefix then can be binary or continuous*/
    else {
        if catnum==2 {
            if r(min)!=0 | r(max)!=1 {
                di as error "Binary variable `var1' (exposure)  needs to be coded as 0=control and 1=intervention"
                error 197
            }
            else {
                di as error "Warning:" _col(12) as res "Binary exposure variable `var1' used"
            }
        }
        else {
            di as error "Warning:" _col(12) as res "Continuous exposure variable `var1' used"
            scalar binexp = 0
        }
    }
    /*get all co-variates */
    local allvar = "`clustervar' `outcomevar' `var1'"
    /*add another list with no factor notation which will only be used in the variable comparison so no duplicates are given (var1 has been dealt with)*/
    local allvarnofv = "`clustervar' `outcomevar' `var1'"
    forvalues i=1(1)`=wordcount("`fe'")' {
        scalar vcnt = vcnt + 1
        local var`=vcnt' = word("`fe'",`i')
        local allvar = "`allvar' `var`=vcnt''"
        if strpos("`var`=vcnt''","i.")==1 {
            local allvarnofv = "`allvarnofv' `=substr("`var`=vcnt''",3,.)'"
        }
        else {
            local allvarnofv = "`allvarnofv' `var`=vcnt''"
        }
    }
    forvalues i=1(1)`=wordcount("`re'")' {
        scalar vcnt = vcnt + 1
        local var`=vcnt' = word("`re'",`i')
        local allvar = "`allvar' `var`=vcnt''"
        if strpos("`var`=vcnt''","i.")==1 {
            local tempvar = substr("`var`=vcnt''",3,.)
            local allvarnofv = "`allvarnofv' `tempvar'"
        }
        else {
            local tempvar = "`var`=vcnt''"
            local allvarnofv = "`allvarnofv' `tempvar'"
        }
        /*find if they are included as random effects in the model*/
        if strpos(substr(e(cmdline),fp,.),"`tempvar'")==0 {
            di as error "Warning:" _col(12) as res "RE variable `var`=vcnt'' not included as a random-effects.
            di as res _col(12)"Execution will continue since the command line might have been too long"
        }
    }
    /*fets not counted as variables since they might not be - however add a check*/
    scalar intertype=0  /*assume no interaction term included*/
    qui sum `clustervar' if `esample'==1
    scalar maxnum = r(max)
    scalar minnum = r(min)
    forvalues i=1(1)`=wordcount("`fets'")' {
        local fets`i' = word("`fets'",`i')
        capture confirm var `fets`i'', exact
        if _rc!=0 {
            /*if fets and an interaction assume they are linked, for now - will set to zero later if not the case*/
            if "`ia'"!="" scalar intertype=3
            /*trial specific variables*/
            forvalues j=`=minnum'(1)`=maxnum' {
                qui count if `clustervar'==`j' & `esample'==1
                scalar clsize = r(N)
                if clsize!=0 {
                    capture confirm var `fets`i''`j'
                    if _rc!=0 {
                        di as error "Required dummy variable `fets`i''`j' not found"
                        di as error "in option fets()"
                        error 197
                    }
                    qui sum `fets`i''`j' if `clustervar'!=`j' & `esample'==1
                    if r(min)!=0 | r(max)!=0 {
                        di as error "Required dummy variable `fets`i''`j' not defined correctly"
                        di as error "in option fets()"
                        error 197
                    }
                    qui sum `fets`i''`j' if `clustervar'==`j' & `esample'==1
                    if r(min)==0 & r(max)==0 {
                        di as error "Required dummy variable `fets`i''`j' not defined correctly"
                        di as error "in option fets()"
                        error 197
                    }
                    /*in case an interaction term has been added, run some more checks*/
                    if "`ia'"!="" {
                        local tempia = "`ia'"
                        if strpos("`tempia'","i.")==1 local tempia = substr("`tempia'",3,.)
                        qui count if `fets`i''`j'-`tempia'<10-6 & `clustervar'==`j' & `esample'==1
                        if r(N)!=clsize {
                            scalar intertype=0
                        }
                    }
                }
            }
        }
    }

    /*make sure a user doesn't give the same variable names*/
    forvalues i=1(1)`=wordcount("`allvarnofv'")-1' {
        forvalues j=`=`i'+1'(1)`=wordcount("`allvarnofv'")' {
            if "`=word("`allvarnofv'",`i')'"=="`=word("`allvarnofv'",`j')'" {
                di as error "Outcome, intervention, cluster variables and covariates need to be unique!"
                error 197
            }
        }
    }
    /*for the interaction option though the user must have given the variable either as a fixed or a random effect!*/
    scalar iatype = -1
    if "`ia'"!="" {
        local intervar = "`ia'"
        /*get without the i. prefix*/
        if strpos("`intervar'","i.")==1 {
            local intervar2 = substr("`intervar'",3,.)
            /*ia type clarify*/
            scalar iatype = 1           /*assume binary*/
            qui tab `intervar2' if `esample'==1
            if r(r)>2 scalar iatype = 2 /*categorical*/
        }
        /*if without the i prefix*/
        else {
            qui tab `intervar' if `esample'==1
            if r(r)==2 {
                scalar iatype = 1
            }
            else {
                /*ia type, continuous*/
                scalar iatype = 0
                /*issue a warning if not centered*/
                qui sum `intervar' if `esample'==1
                if abs(r(mean))>10^-3 {
                    di as error "Warning:" _col(12) as res "interaction continuous(?) variable `intervar' is not (sample mean) centered"
                }
            }
            local intervar2 = "`intervar'"
        }

        /*issue error if categorical variable not included in factor form*/
        if strpos("`intervar'","_I")==1 {
            di as error "Interaction variable cannot be in expanded form. Use factor notation for categorical"
            di as error "variables (eg i.age). Factor notation is acceptable for binary variables but not required"
            error 197
        }
        /*do not allow _I prefix at all since we use it later to make life easier (variables that use it will be dropped and will be a problem if
        they are not the ones involved in the interaction - I am using the _I prefix make finding the main effects variable names easier*/
        if  strpos("`fe'", "_I")>0 | strpos("`re'", "_I")>0 {
            di as error "When interaction option used, model variables are not allowed in _I prefix expansion form"
            di as error "Prefix is reserved for the interaction terms: use a different prefix or factor notation"
            error 197
        }
        /*examine if it has been included as a fixed, random or fixed trial specific effect - if neither issue error (must be in identical form)*/
        /*it cannot be both since we have already made sure variables are unique*/
        forvalues i=1(1)`=wordcount("`fe'")' {
            if  word("`fe'",`i')=="`intervar'" {
                scalar intertype=1
            }
        }
        forvalues i=1(1)`=wordcount("`re'")' {
            if  word("`re'",`i')=="`intervar'" {
                scalar intertype=2
            }
        }
        /*fets identification more complicated - has been done earlier in the fets check*/
        if intertype==0 {
            di as error "Var to be interacted with intervention must also be included in fe, re, or fets option"
            di as error "For fets option, dummy variable stub corresponding to the interaction variable is needed"
            di as error "e.g. fets(dept0s_) ia(dept0s) where dept0s_1 the dummy with baseline scores for study 1"
            di as error "in option ia()"
            error 197
        }
    }

    /*some extra checks for the variables*/
    forvalues i=1(1)`=vcnt' {
        local tempstr = "`var`i''"
        if strpos("`var`i''","i.")==1 {
            local tempstr = substr("`var`i''",3,.)
        }
        /*if it is a categorical variable*/
        if strpos("`var`i''","_I")==1 {
            local tempstr = substr("`var`i''",3,`=length("`var`i''")-4')
        }
        /*then check if it's in the regression command line*/
        if strpos(e(cmdline),"`tempstr'")==0 {
            di as error "Warning:" _col(12) as res "variable `tempstr' not found in the `modelsel' command line. Please make sure it was included."
            di as res _col(12)"Execution will continue since the command line might have been too long"
        }
        /*finally check that categorical or continuous variables are treated consistently across the 2 commands*/
        capture tab `tempstr'
        scalar boolval = 0
        if _rc!=0 {
            scalar boolval = 1
        }
        else {
            if r(r)>2 {
                scalar boolval = 1
            }
        }
        if boolval==1 & strpos("`var`i''","i.")==1 & strpos(e(cmdline),"`var`i''")==0 {
            di as error "Variable `tempstr' included as categorical in the forest plot but as continuous in `modelsel'"
            error 197
        }
        if boolval==1 & strpos("`var`i''","i.")==0 & strpos(e(cmdline),"i.`var`i''")>0 {
            di as error "Variable `tempstr' included as categorical in `modelsel' and as continuous in the forest plot"
            error 197
        }
    }
    if "`gsavedir'"!="" {
        /*first bit, needed*/
        capture mkdir "`gsavedir'"
        capture cd "`gsavedir'"
        if _rc!=0 {
            di as error "Specified directory does not exist and could not be created"
            di as error "in option gsave()"
            error 197
        }
        local findir = "`gsavedir'"
    }
    else {
        local findir `c(pwd)'
    }

    /*make sure the intervention variable is included as a random factor and get the location to use in variance grabbing at the end*/
    local foundre = 0
    forvalues i=1(1)`=wordcount(e(revars))' {
        if strpos(word(e(revars),`i'),"`var1'")>0 {
            local foundre = `i'
        }
    }
    if `foundre'==0 {
        di as error "Variable `var1' (exposure) not used as a random-effect in the `modelsel' model"
        error 197
    }
    else {
        /*capture some needed between and within study variance estimates to display in a table at the end*/
        scalar tausq = exp(2*_b[lns1_1_`foundre':_cons])
        scalar tsq_pl_lo = exp(2*(_b[lns1_1_`foundre':_cons] - abs(invnormal(0.5*(1-c(level)/100)))*_se[lns1_1_`foundre':_cons]))
        scalar tsq_pl_up = exp(2*(_b[lns1_1_`foundre':_cons] + abs(invnormal(0.5*(1-c(level)/100)))*_se[lns1_1_`foundre':_cons]))
        if "`modelsel'"=="xtmelogit" {
            scalar ssq = .
        }
        else {
            scalar ssq = exp(2*_b[lnsig_e:_cons])
        }
    }

    /*identify if it's a fixed common intercept model so that individual regressions can take that into account*/
    /*set to no now, but may be changed later*/
    local xtraopt = ""
    scalar regconst=0
    scalar fcint = 0

    estimates store mainreg
    /*supposed to follow an xtmixed command or equivalent - but e(sample) not there for mi estimate in this form and does not make sense to use*/
    if "`cmloc'"=="cmd" {
        qui keep if `esample'==1
    }

    /*create label temp variable*/
    if "`label'"=="" {
        capture decode `clustervar', generate(labelvar) maxlength(30)
        if _rc!=0 {
            capture gen str30 labelvar = string(`clustervar')
            if _rc!=0 {
                di as error "cluster variable `clustervar' cannot be a string variable"
                di as error "use 'encode' and re-run regression model"
                error 108
            }
        }
    }
    else {
        /*convert if needed to string variables*/
        forvalues i=1(1)`=wordcount("`label'")' {
            local lvar`i' = word("`label'",`i')
            qui capture confirm string var `lvar`i''
            if _rc!=0 {
                qui capture drop temp1
                qui gen temp1 = string(`lvar`i'')
                qui drop `lvar`i''
                qui rename temp1 `lvar`i''
            }
        }
        if wordcount("`label'")==1 {
            qui gen str30 labelvar = trim(`lvar1')
        }
        else {
            qui gen str30 labelvar = trim(`lvar1') + ", " + trim(`lvar2')
        }
    }
    /*size of largest string to be used in output widths*/
    qui gen `lblsize' = strlen(labelvar)
    qui sum `lblsize'
    scalar maxstr = max(r(max), 15)
    
    /*interactions? - get the variable names and counts*/
    if intertype!=0 {
        /*variables with the I prefix should not be present - leftovers from xi: xtmixed or xtlogit will be dropped*/
        /*slightly different approach for (binary and continuous) vs categorical - for binary we don't want the i prefix if present*/
        if binexp==1 {
            /*if exposure is binary*/
            if iatype==2 {
                /*interaction with categorical*/
                qui xi i.`var1'*`intervar'
            }
            else {
                /*interaction with binary or continuous*/
                qui xi i.`var1'*`intervar2'
            }
        }
        else {
            /*if exposure is continuous*/
            if iatype==2 {
                /*interaction with categorical*/
                qui xi `intervar'*`var1'
            }
            else if iatype==1 {
                /*interaction with binary*/
                qui xi i.`intervar2'*`var1'
                /*drop the dummy variable, not needed but no big deal - would be collinear in regression anyway*/
                capture drop _I`intervar2'_1
            }
            else {
                /*interaction with continuous - NOTE: the only way for this to work is if user creates interaction variable using certain criteria*/
                capture drop _I`var1'X`intervar2'
                qui gen _I`var1'X`intervar2' = `var1'*`intervar2'
            }
        }
        qui descr _I*, varlist
        local interterms = r(varlist)
        /*identify exposure*/
        forvalues i=1(1)`=wordcount("`interterms'")' {
            if strpos("`=word("`interterms'",`i')'","_I`=substr("`var1'",1,9)'_")==1 {
                local ivA1 = "`=word("`interterms'",`i')'"
            }
        }
        /*identify the second variable*/
        scalar ivBcnt=0
        local ivB=""
        forvalues i=1(1)`=wordcount("`interterms'")' {
            if strpos("`=word("`interterms'",`i')'","_I`=substr("`intervar2'",1,9)'_")==1 {
                scalar ivBcnt=ivBcnt+1
                local ivB`=ivBcnt' = "`=word("`interterms'",`i')'"
                local ivB = "`ivB' `ivB`=ivBcnt''"
            }
        }
        /*remove the items already assigned just in case capital Xs are included in the variables*/
        if "`ivA1'"!="" {
            local interterms = subinstr("`interterms'", "`ivA1'","",.)
        }
        forvalues i=1(1)`=ivBcnt' {
            local interterms = subinstr("`interterms'", "`ivB`i''","",.)
        }
        local interterms = trim("`interterms'")
        /*identify the interaction terms*/
        scalar ivCcnt=0
        local ivC=""
        forvalues i=1(1)`=wordcount("`interterms'")' {
            if strpos("`=word("`interterms'",`i')'","X")>0 {
                scalar ivCcnt=ivCcnt+1
                local ivC`=ivCcnt' = "`=word("`interterms'",`i')'"
                local ivC = "`ivC' `ivC`=ivCcnt''"
            }
        }
    }
    /*debugging*/
    *di "`ivA1'"
    *di "`ivB'"
    *di "`ivC'"

    /*get the overall diamnond first before executing margins command*/
    /*different approach if intervention variable input with an i.*/
    scalar effnum = 1
    local iabits = ""
    capture scalar eff1 = _b[1.`var1']
    if _rc==0 {
        scalar eff1se = _se[1.`var1']
    }
    else {
        capture scalar eff1 = _b[`var1']
        if _rc==0 {
            scalar eff1se = _se[`var1']
        }
        else {
           /*still possible since i only issue a warning earlier if not in the command line*/
    	   di as error "Selected intervention variable not found in previous `modelsel' command"
    	   error 301
        }
    }
    scalar eff1lo = eff1 - abs(invnormal(0.5*(1-c(level)/100)))*eff1se
    scalar eff1up = eff1 + abs(invnormal(0.5*(1-c(level)/100)))*eff1se
    /*standard output when only main effect is nil*/
    local strout1 = ""
    local graphname1 = "main_`var1'"
    /*if interactions are there get the overall effect(s) for the interaction(s)*/
    if intertype!=0 {
        local strout1 = "Main effect (`var1')"
        /*for the forest plots*/
        if "`gsavedir'"!="" | "`gsavename'"!="" | "`eps'"!="" | "`gph'"!=""{
        /*if !missing("`gsavedir'`gsavename'`eps'`gph'") {*/
            set more off
        }
        else {
            set more on
        }

        /*BINARY EXPOSURE - interactions*/
        if binexp==1 {
            /*continuous interaction*/
            if iatype==0 {
                local strout2 = "Interaction effect (`var1' x `intervar2')"
                local graphname2 = "interaction_`var1'X`intervar2'"
                scalar effnum = 2
                tempvar eff2 lo95CI2 up95CI2
                /*identify if it's fv format or xi expansion*/
                capture scalar eff2se = _se[0.`var1'#c.`intervar2']
                if _rc==0 {
                    /*we want the effect for category 1 so will reverse if it's there - if se is zero though pickup the effect from cat 1*/
                    if eff2se==0 {
                        scalar eff2 = _b[1.`var1'#c.`intervar2']
                        scalar eff2se = _se[1.`var1'#c.`intervar2']
                        /*hopefully will never happen*/
                        if eff2se==0 {
                            di as error "Could not find SE for the interaction - it seems to be zero"
                            error 197
                        }
                    }
                    else {
                        scalar eff2 = -_b[0.`var1'#c.`intervar2']
                    }
                }
                else {
                    capture scalar eff2 = _b[`ivC1']
                    if _rc==0 {
                        scalar eff2se = _se[`ivC1']
                    }
                    else {
                       /*probably not possilbe*/
                	   di as error "Interaction variable not found in previous `modelsel' command"
                	   error 301
                    }
                }
                scalar eff2lo = eff2 - abs(invnormal(0.5*(1-c(level)/100)))*eff2se
                scalar eff2up = eff2 + abs(invnormal(0.5*(1-c(level)/100)))*eff2se
                /*extra bit to be added to individual regressions - interaction variable and ia var (it might be added twice in regressions but no prob)*/
                local iabits = "`intervar2' `ivC1'"
            }
            /*categorical or binary interaction*/
            else if iatype==1 | iatype==2 {
                /*over-ride the main effect names*/
                qui sum `intervar2'
                scalar minval = r(min)
                scalar maxval = r(max)
                qui tab `intervar2'
                scalar numtot = r(r)
                scalar numcnt = 0
                /*make sure values are there and assign*/
                forvalues x=`=minval'(1)`=maxval' {
                    qui count if `intervar2'==`x'
                    /*if category found, it's another graph*/
                    if r(N)!=0 {
                        scalar numcnt = numcnt + 1
                        local strout`=numcnt' = "Main effect (`var1'), `intervar2'=`x'"
                        local graphname`=numcnt' = "main_`var1'_`intervar2'eq`x'"
                        /*get the categories into scalars since the numbering might not be continuous*/
                        scalar cat`=numcnt' = `x'
                    }
                }
                /*if not all categories are found, issue an error*/
                if numcnt!=numtot {
                    if iatype==1 {
                        di as error "Binary variable `intervar2' contains non-integer categories"
                    }
                    else {
                        di as error "Categorical variable `intervar2' contains non-integer categories"
                    }
                    error 301
                }
                /*total number of effects to be computed*/
                scalar effnum = numtot
    
                /*but also need to find out the format of the main effect coefficient*/
                capture scalar tempsc = _b[1.`var1']
                if _rc==0 {
                    local meffnot = "1.`var1'"
                }
                else {
                    local meffnot = "`var1'"
                }
                /*temp vars for each category*/
                forvalues x=2(1)`=effnum' {
                    tempvar eff`x' lo95CI`x' up95CI`x'
                }
    
                /*identify if it's fv format or xi expansion*/
                capture scalar tempse = _se[0.`var1'#`=maxval'.`intervar2']
                /*fv format*/
                if _rc==0 {
                    /*it's fv format but if categorical or binary with an i. notation we should use 1.`var' - if binary without i. notation 0.`var*/
                    local tpre = "1."
                    forvalues x=1(1)`=effnum' {
                        if _se[0.`var1'#`=cat`x''.`intervar2']!=0 {
                            local tpre = "0."
                        }
                    }
                    /*if it's fv format go through all categories - overwriting eff1 etc in the process but easier code*/
                    forvalues x=1(1)`=effnum' {
                        if "`tpre'"=="1." {
                            qui test _b[`meffnot'] + _b[`tpre'`var1'#`=cat`x''.`intervar2'] = 0
                            scalar eff`x' = _b[`meffnot'] + _b[`tpre'`var1'#`=cat`x''.`intervar2']
                        }
                        else {
                            qui test _b[`meffnot'] - _b[`tpre'`var1'#`=cat`x''.`intervar2'] = 0
                            scalar eff`x' = _b[`meffnot'] - _b[`tpre'`var1'#`=cat`x''.`intervar2']
                        }
                        scalar zval = sqrt(r(chi2))
                        scalar eff`x'se = abs(eff`x'/zval)
                        scalar eff`x'lo = eff`x' - abs(invnormal(0.5*(1-c(level)/100)))*eff`x'se
                        scalar eff`x'up = eff`x' + abs(invnormal(0.5*(1-c(level)/100)))*eff`x'se
                    }
                }
                /*if it's an xi expansion*/
                else {
                    /*make sure that's the case*/
                    capture scalar tempsc = _b[`ivC1']
                    if _rc==0 {
                        /*if it's expansion format go through all categories - overwriting eff1 etc in the process but easier code*/
                        forvalues x=2(1)`=effnum' {
                            qui test _b[`meffnot'] + _b[`ivC`=`x'-1''] = 0
                            scalar eff`x' = _b[`meffnot'] + _b[`ivC`=`x'-1'']
                            scalar zval = sqrt(r(chi2))
                            scalar eff`x'se = abs(eff`x'/zval)
                            scalar eff`x'lo = eff`x' - abs(invnormal(0.5*(1-c(level)/100)))*eff`x'se
                            scalar eff`x'up = eff`x' + abs(invnormal(0.5*(1-c(level)/100)))*eff`x'se
                        }
                    }
                    else {
                       /*probably not possilbe*/
                	   di as error "Interaction variable not found in previous `modelsel' command"
                	   error 301
                    }
                }
                /*extra bit to be added to individual regressions - interaction variable and ia var (it might be added twice in regressions but no prob)*/
                if iatype==1 {
                    local iabits = "`intervar2' `ivC1'"
                }
                /*extra bit to be added to individual regressions - interaction variable binaries and interaction terms*/
                else {
                    local iabits = "`ivB' `ivC'"
                }
            }
        }
        /*end of binary exposure interactions*/

        /*CONTINUOUS EXPOSURE - interactions*/
        if binexp==0 {
            /*continuous interaction*/
            if iatype==0 {
                local strout2 = "Interaction effect (`var1' x `intervar2')"
                local graphname2 = "interaction_`var1'X`intervar2'"
                scalar effnum = 2
                tempvar eff2 lo95CI2 up95CI2
                /*identify if it's fv format or xi expansion*/
                capture scalar eff2se = _se[c.`var1'#c.`intervar2']
                if _rc==0 {
                    if eff2se==0 {
                        di as error "Could not find SE for the interaction - it seems to be zero"
                        error 197
                    }
                    scalar eff2 = _b[c.`var1'#c.`intervar2']
                }
                else {
                    capture scalar eff2 = _b[`ivC1']
                    if _rc==0 {
                        scalar eff2se = _se[`ivC1']
                    }
                    else {
                       /*just in case*/
                	   di as error "Interaction variable (continuous by continuous) not found in previous `modelsel' command"
                	   di as error "Preferable option is to use the factor variable notation: c.`var1'#c.`intervar2'"
                	   di as error "Also supported a manually created interaction variable _I`var1'X`intervar2'"
                	   error 301
                    }
                }
                scalar eff2lo = eff2 - abs(invnormal(0.5*(1-c(level)/100)))*eff2se
                scalar eff2up = eff2 + abs(invnormal(0.5*(1-c(level)/100)))*eff2se
                /*extra bit to be added to individual regressions - interaction variable and ia var (it might be added twice in regressions but no prob)*/
                local iabits = "`intervar2' `ivC1'"
            }
            /*categorical or binary interaction*/
            /*some code duplication but i'm not bothered...*/
            else if iatype==1 | iatype==2 {
                /*over-ride the main effect names*/
                qui sum `intervar2'
                scalar minval = r(min)
                scalar maxval = r(max)
                qui tab `intervar2'
                scalar numtot = r(r)
                scalar numcnt = 0
                /*make sure values are there and assign*/
                forvalues x=`=minval'(1)`=maxval' {
                    qui count if `intervar2'==`x'
                    /*if category found, it's another graph*/
                    if r(N)!=0 {
                        scalar numcnt = numcnt + 1
                        local strout`=numcnt' = "Main effect (`var1'), `intervar2'=`x'"
                        local graphname`=numcnt' = "main_`var1'_`intervar2'eq`x'"
                        /*get the categories into scalars since the numbering might not be continuous*/
                        scalar cat`=numcnt' = `x'
                    }
                }
                /*if not all categories are found, issue an error*/
                if numcnt!=numtot {
                    if iatype==1 {
                        di as error "Binary variable `intervar2' contains non-integer categories"
                    }
                    else {
                        di as error "Categorical variable `intervar2' contains non-integer categories"
                    }
                    error 301
                }
                /*total number of effects to be computed*/
                scalar effnum = numtot

                /*here the format of the main effect coefficient can only be continuous*/
                /*temp vars for each category*/
                forvalues x=2(1)`=effnum' {
                    tempvar eff`x' lo95CI`x' up95CI`x'
                }
                /*identify if it's fv format or xi expansion*/
                capture scalar tempse = _se[c.`var1'#`=maxval'.`intervar2']
                /*fv format*/
                if _rc==0 {
                    /*if it's fv format go through all categories - overwriting eff1 etc in the process but easier code*/
                    forvalues x=1(1)`=effnum' {
                        qui test _b[`var1'] + _b[`tpre'`var1'#`=cat`x''.`intervar2'] = 0
                        scalar eff`x' = _b[`var1'] + _b[c.`var1'#`=cat`x''.`intervar2']
                        scalar zval = sqrt(r(chi2))
                        scalar eff`x'se = abs(eff`x'/zval)
                        scalar eff`x'lo = eff`x' - abs(invnormal(0.5*(1-c(level)/100)))*eff`x'se
                        scalar eff`x'up = eff`x' + abs(invnormal(0.5*(1-c(level)/100)))*eff`x'se
                    }
                }
                /*if it's an xi expansion*/
                else {
                    /*make sure that's the case*/
                    capture scalar tempsc = _b[`ivC1']
                    if _rc==0 {
                        /*if it's expansion format go through all categories - overwriting eff1 etc in the process but easier code*/
                        forvalues x=2(1)`=effnum' {
                            qui test _b[`var1'] + _b[`ivC`=`x'-1''] = 0
                            scalar eff`x' = _b[`var1'] + _b[`ivC`=`x'-1'']
                            scalar zval = sqrt(r(chi2))
                            scalar eff`x'se = abs(eff`x'/zval)
                            scalar eff`x'lo = eff`x' - abs(invnormal(0.5*(1-c(level)/100)))*eff`x'se
                            scalar eff`x'up = eff`x' + abs(invnormal(0.5*(1-c(level)/100)))*eff`x'se
                        }
                    }
                    else {
                       /*probably not possilbe*/
                	   di as error "Interaction variable not found in previous `modelsel' command"
                	   error 301
                    }
                }
                /*extra bit to be added to individual regressions - interaction variable and ia var (it might be added twice in regressions but no prob)*/
                if iatype==1 {
                    local iabits = "`intervar2' `ivC1'"
                }
                /*extra bit to be added to individual regressions - interaction variable binaries and interaction terms*/
                else {
                    local iabits = "`ivB' `ivC'"
                }
            }
        }
        /*end of continuous exposure interactions*/
    }

    /*CALCULATIONS FOR FE and TRIAL SPECIFIC FE*/
    /*generate the temp outcome variable that will be updated if fe covariates are present - same across all studies*/
    qui gen double `xb1'=0
    if "`fe'"!="" {
        forvalues i=1(1)`=wordcount("`fe'")' {
            local fe`i' = word("`fe'",`i')
            /*try to capture the coefficient in its simplest form*/
            capture scalar feff = _b[`fe`i'']
            if _rc==0 {
                qui replace `xb1' = `xb1' + feff*`fe`i''
            }
            if _rc!=0 {
                if strpos("`fe`i''","i.")==1 {
                    local fe_`i' = substr("`fe`i''",3,.)
                    /*if it's categorical or binary (fv, not with the xi prefix - if xtmixed has been executed with xi: user expected to provide the
                    dummies in the varlist)*/
                    qui sum `fe_`i''
                    scalar mincat = r(min)
                    scalar maxcat = r(max)
                    scalar sumcat = r(N)
                    /*categories need to be integers*/
                    forvalues j=`=mincat'(1)`=maxcat' {
                        capture scalar feff = _b[`j'.`fe_`i'']
                        if _rc==0 {
                            qui gen `tempdummy' = 0
                            qui replace `tempdummy' = 1 if `fe_`i''==`j'
                            qui replace `xb1' = `xb1' + feff*`tempdummy'
                            qui drop `tempdummy'
                            /*make sure all values in the categorical variable have been accounted for - i.e. only integers are used*/
                            qui count if `fe_`i''==`j'
                            scalar sumcat = sumcat - r(N)
                        }
                    }
                    /*if all values not accounted for issue error - although xtmixed/xtlogit do not allow execution for non integer values*/
                    if sumcat!=0 {
              	        di as error "Categorical variable `fe_`i'' cannot contain non-integer values"
              	        di as error "and/or make sure you have included `fe_`i'' as a main effect"
              	        error 197
              	    }
                }
                else {
                    /*estimates not found and not in i. notation - shouldn't ever end up here but...*/
              	    di as error "Estimates for fixed-effect component `fe`i'' not found"
              	    error 301
                }
            }
        }
    }
    /*some info on the studies*/
    qui sum `clustervar'
    scalar maxnum = r(max)
    scalar minnum = r(min)
    scalar duminc = 0
    scalar dumbas = 0
    /*trial specific variables*/
    forvalues j=`=minnum'(1)`=maxnum' {
        /*if number corresponds to a study, go on*/
        qui count if `clustervar'==`j'
        if r(N)!=0 {
            /*now if the study is there see if there are trial specific fixed effects that need to be taken into account*/
            if "`fets'"!="" {
                forvalues i=1(1)`=wordcount("`fets'")' {
                    local fets`i' = word("`fets'",`i')
                    /*identify the format of the variable*/
                    capture confirm var `fets`i''
                    /*exact variable name found so should be intercept variable - the first one is the _cons one*/
                    if _rc==0 {
                        /*_cons is the intercept for the first study (_b[1.studyid]=0). _b[x.studyid]=difference of study x intercept compared to
                        study 1*/
                        capture scalar fets = _b[_cons] + _b[`j'.`fets`i'']
                        /*if saved coeff does not correspond to study numbers, exit with error - impossible t0 happen?*/
                        if _rc!=0 {
                            /*houston we have a problem*/
                            di as error "Estimates for `fets`i'', study #`j', not found"
                            error 301
                        }

                        /*only change the prediction for the respective study*/
                        qui gen `tempdummy' = 0
                        qui replace `tempdummy' = 1 if `fets`i''==`j'
                        qui replace `xb1' = `xb1' + fets*`tempdummy'
                        qui drop `tempdummy'
                        /*if we are in here it can only be the intercept variable - add the nocons option for the individual regressions*/
                        local xtraopt = "nocons"
                    }
                    else {
                        /*the altertative is for the user to provide the common part of the dummy variables, up until the number - but it might be
                        dummies for the intercept OR the baseline scores*/
                        capture scalar fets = _b[`fets`i''`j']
                        scalar rsres=0
                        if _rc!=0 {
                            scalar rsres=1
                        }
                        /*if estimate is there, find out if it is a dummy variable or not - OK if 1st dummy estimate is not there*/
                        scalar dumint = 0
                        if rsres==0 {
                            qui sum `fets`i''`j' if `clustervar'!=`j'
                            scalar t1 = r(min)
                            scalar t2 = r(max)
                            qui sum `fets`i''`j' if `clustervar'==`j'
                            scalar t3 = r(min)
                            scalar t4 = r(max)
                            if t1==0 & t2==0 & t3==1 & t4==1 {
                                scalar dumint = 1
                                /*overall to use later - if intercept dummies were included*/
                                scalar duminc = 1
                            }
                        }
                        /*various issues for the first dummy intercept variable*/
                        if `j'==minnum {
                            /*if it is the first study and an estimate has not been found, assume it's the intercept one*/
                            if rsres==1 {
                                local xtraopt = "nocons"
                                scalar fets = _b[_cons]
                                scalar rsres=0
                            }
                            /*if estimate was found and it was the fist dummy intercept, issue error*/
                            if rsres==0 & dumint==1 {
                                di as error "Please re-run the model, without including the intercept dummy for the first study"
                                error 197
                            }
                            /*note that baseline dummies were found*/
                            if rsres==0 & dumint==0 {
                                scalar dumbas=1
                            }
                        }
                        /*if it isn't the first variable, we need to correct estimates for intercept dummies*/
                        else {
                            if dumint==1 {
                                scalar fets = _b[_cons] + _b[`fets`i''`j']
                            }
                        }
                        /*if at this point there still isn't an estimate then issue error*/
                        if rsres==1 {
                            di as error "Estimate not found for variable `fets`i''`j': dummy numbering must correspond to study numbers!"
                            error 301
                        }
                        /*at this stage we are done*/
                        else {
                            /*variable may not be there for intercept and first study*/
                            if `j'==minnum & dumint==1  {
                                qui gen `tempdummy' = 0
                                qui replace `tempdummy' = 1 if `clustervar'==`j'
                                qui replace `xb1' = `xb1' + fets*`tempdummy'
                                qui drop `tempdummy'
                            }
                            else {
                                /*only change the prediction for the respective study since `fets`i'=0 for all other studies*/
                                qui replace `xb1' = `xb1' + fets*`fets`i''`j'
                            }
                        }
                    }
                }
            }
        }
    }
    /*identify if it's a fixed common intercept model so that individual regression can take that into account*/
    /*if the cluster variable is not included as an independent AND trial-specific intercepts were not added as dummies*/
    if strpos(e(cmdline),"`clustervar'")>fp & strpos(e(cmdline),"nocons")>fp & duminc==0 {
        local xtraopt = "nocons"
        scalar regconst = _b[_cons]
        scalar fcint = 1
    }

    /*some info on the studies and create weights - generate a temp studyid var too*/
    qui tab `clustervar'
    scalar studynum = r(r)
    scalar casesnum = r(N)
    scalar cntr = 0
    scalar sfound = 0
    qui gen `sidrev'=.
    qui gen `weights'=.
    while sfound<studynum {
        scalar cntr = cntr + 1
        qui count if `clustervar'==cntr
        if r(N)>0 {
            scalar sfound = sfound + 1
            scalar stid`=sfound'=cntr
            qui replace `sidrev' = studynum-sfound+1 if `clustervar'==cntr
            qui replace `weights' = r(N)/casesnum if `clustervar'==cntr
        }
    }

    /*execute appropriate regression command - xtlogit or xtmelogit*/
    /*get effects and CIs for studies*/
    qui replace `xb1' = `xb1' + regconst
    qui gen `outcvar2' = `outcomevar' - `xb1'
    *sum `xb1' `outcomevar' `outcvar2'
    forvalues i=1(1)`=studynum' {
        if "`modelsel'"=="xtmixed" {
            /*if xb1=0, no effect*/
            qui regress `outcvar2' `var1' `re' `iabits' if `sidrev'==`i', `xtraopt'
            /*only for regress commands, find df residuals to be used in confidence intervals*/
            scalar dfres = e(df_r)
            scalar mltpl = invttail(e(df_r),0.025)
        }
        else {
            /*if xb1=0, no effect*/
            qui logit `outcomevar' `var1' `re' `iabits' if `sidrev'==`i', offset(`xb1') `xtraopt'
            scalar mltpl = abs(invnormal(0.5*(1-c(level)/100)))
        }
        scalar st`i'_1eff = _b[`var1']
        scalar st`i'_1se = _se[`var1']
        scalar st`i'_1lo = st`i'_1eff-mltpl*st`i'_1se
        scalar st`i'_1up = st`i'_1eff+mltpl*st`i'_1se
        /*if se=0, i.e. coefficient cannot be estimated set to missing*/
        if st`i'_1se<=10^-10 {
            di as error "No variance in main effect for study `=stid`=studynum - `i' + 1'' - needs to be excluded from analysis"
            error 322
        }
        /*if continuous interaction - only one interaction variable*/
        if iatype==0 {
            scalar st`i'_2eff = _b[`ivC1']
            scalar st`i'_2se = _se[`ivC1']
            scalar st`i'_2lo = st`i'_2eff-mltpl*st`i'_2se
            scalar st`i'_2up = st`i'_2eff+mltpl*st`i'_2se
            if st`i'_2se<=10^-10 {
                di as error "No variance in continuous interaction for study `=stid`=studynum - `i' + 1'' - needs to be excluded from analysis"
                error 322
            }
        }
        /*if binary variable*/
        else if iatype==1 {
            qui test _b[`var1'] + _b[`ivC1'] = 0
            if "`modelsel'"=="xtmixed" {
                scalar ztval = sqrt(r(F))
            }
            else {
                scalar ztval = sqrt(r(chi2))
            }
            scalar st`i'_2eff = _b[`var1'] + _b[`ivC1']
            scalar st`i'_2se = abs(st`i'_2eff/ztval)
            scalar st`i'_2lo = st`i'_2eff-mltpl*st`i'_2se
            scalar st`i'_2up = st`i'_2eff+mltpl*st`i'_2se
            if st`i'_2se<=10^-10 {
                di as error "No variance in binary interaction for study `i' - needs to be excluded from analysis"
                error 322
            }
        }
        else if iatype==2 {
            forvalues x=2(1)`=effnum' {
                qui test _b[`var1'] + _b[`ivC`=`x'-1''] = 0
                if "`modelsel'"=="xtmixed" {
                    scalar ztval = sqrt(r(F))
                }
                else {
                    scalar ztval = sqrt(r(chi2))
                }
                scalar st`i'_`x'eff = _b[`var1'] + _b[`ivC`=`x'-1'']
                scalar st`i'_`x'se = abs(st`i'_`x'eff/ztval)
                scalar st`i'_`x'lo = st`i'_`x'eff-mltpl*st`i'_`x'se
                scalar st`i'_`x'up = st`i'_`x'eff+mltpl*st`i'_`x'se
                if st`i'_`x'se<=10^-10 {
                    di as error "No variance in categorical interaction for study `i' - needs to be excluded from analysis"
                    error 322
                }
            }
        }
        /*if binary or continuous variable, set to missing for categories that are not present in study*/
        if iatype==1 | iatype==2 {
            forvalues x=1(1)`=effnum' {
                qui count if `sidrev'==`i' & `intervar2'==cat`x'
                if r(N)==0 {
                    scalar st`i'_`x'eff = .
                    scalar st`i'_`x'se = .
                    scalar st`i'_`x'lo = .
                    scalar st`i'_`x'up = .
                }
            }
        }
    }

    /*if logistic and odds ratio was selected, make appropriate changes*/
    if "`or'"!="" {
        forvalues j=1(1)`=effnum' {
            forvalues i=1(1)`=studynum' {
                scalar st`i'_`j'eff = exp(st`i'_`j'eff)
                scalar st`i'_`j'lo = exp(st`i'_`j'lo)
                scalar st`i'_`j'up = exp(st`i'_`j'up)
            }
            scalar eff`j' = exp(eff`j')
            scalar eff`j'lo = exp(eff`j'lo)
            scalar eff`j'up = exp(eff`j'up)
        }
    }

    /*collapse data*/
    qui collapse (first) labelvar `var1' `sidrev' `weights', by(`clustervar')
    forvalues j=1(1)`=effnum' {
        qui gen `eff`j''=.
        qui gen `lo95CI`j''=.
        qui gen `up95CI`j''=.
        forvalues i=1(1)`=studynum' {
            qui replace `eff`j'' = st`i'_`j'eff if `sidrev'==`i'
            qui replace `lo95CI`j'' = st`i'_`j'lo if `sidrev'==`i'
            qui replace `up95CI`j'' = st`i'_`j'up if `sidrev'==`i'
        }
    }
    sort `sidrev'
    /*debugging*/
    *foreach x in sidrev weights eff1 lo95CI1 up95CI1 eff2 lo95CI2 up95CI2 {
    *    qui gen `x' = ``x''
    *}

    /*forest plot graph*/
    /*add the overall effect(s) as extra observation(s)*/
    qui set obs `=studynum+1'
    forvalues j=1(1)`=effnum' {
        qui replace `eff`j'' = eff`j' in `=studynum+1'
        qui replace `lo95CI`j'' = eff`j'lo in `=studynum+1'
        qui replace `up95CI`j'' = eff`j'up in `=studynum+1'
    }
    qui replace `sidrev' = 0 in `=studynum+1'
    qui replace `clustervar' = 0 in `=studynum+1'
    /*label reversed ID variable*/
    forvalues i=1(1)`=studynum' {
        label define studynames `i' "`=labelvar[`i']'", add
    }
    label define studynames 0 "Overall effect", add
    label val `sidrev' studynames

    /*miltiple forest plots*/
    forvalues j=1(1)`=effnum' {
        /*find the min and max for use in graphs*/
        qui sum `lo95CI`j''
        scalar scmin = r(min)
        qui sum `up95CI`j''
        scalar scmax = r(max)
        if scmax-scmin<=2 {
            local dstep=0.2
            scalar dround=0.5
        }
        else if scmax-scmin>2 & scmax-scmin<=5 {
            local dstep=0.5
            scalar dround=0.5
        }
        else if scmax-scmin>5 & scmax-scmin<=10 {
            local dstep=1
            scalar dround=1
        }
        else if scmax-scmin>10 & scmax-scmin<=30 {
            local dstep=2
            scalar dround=2
        }
        else {
            local dstep=5
            scalar dround=5
        }
        local lbmin = round(scmin,dround)
        if `lbmin'>-dround {
            local lbmin = -dround  /*the scale always goes down to -dround*/
        }
        else {
            if scmin<`lbmin' {
                local lbmin = `lbmin'-dround
            }
        }
        local lbmax = round(scmax,dround)
        if `lbmax'<dround {
            local lbmax = dround  /*the scale always goes up to dround*/
        }
        else {
            if scmax>`lbmax' {
                local lbmax = `lbmax'+dround
            }
        }
        /*if or is selected then lbmin can be not be below zero*/
        if "`or'"!="" {
            local lbmin = 0
        }

        /*save file since we will drop cases to deal with the binary/categorical interactions case*/
        qui save `tempf', replace
        scalar tstudynum = studynum
        qui count if `eff`j''==.
        if r(N)>0 {
            qui drop if `eff`j''==.
            qui count
            scalar tstudynum = r(N)-1
            gsort -`clustervar'
            qui drop `sidrev'
            qui egen `sidrev' = seq(), from(1) to(`=tstudynum')
            qui replace `sidrev'=0 if `clustervar'==0
            /*redo the labels*/
            label drop studynames
            forvalues i=1(1)`=tstudynum' {
                label define studynames `i' "`=labelvar[`i']'", add
            }
            label define studynames 0 "Overall effect", add
            label val `sidrev' studynames
        }

        /*since we have the dataset ready use to export*/
        if "`export'"!="" {
            qui save `tempsave', replace
            qui gen eff=`eff`j''
            qui gen eff_lo=`lo95CI`j''
            qui gen eff_up=`up95CI`j''
            qui gen eff_se = (eff-eff_lo)/abs(invnormal(0.5*(1-c(level)/100)))
            qui gen weight = `weights'
            qui replace weight = 1 if `clustervar'==0
            qui gen efftype = `j'
            qui keep `clustervar' eff* weight
            /*if main effect save first*/
            if `j'==1 {
                qui save "`findir'\\`export'.dta", replace
                if "`strout1'"=="" {
                    local exlblstr = `"1 "Main effect (`var1')""'
                }
                else {
                    local exlblstr = `"1 "`strout1'""'
                }
            }
            else {
                qui append using "`findir'\\`export'.dta"
                qui save "`findir'\\`export'.dta", replace
                local exlblstr = `"`exlblstr' `j' "`strout`j''""'
            }
            /*add overall label at the end*/
            if `j'==`=effnum' {
                label var efftype "Effect type"
                label define efflbl `exlblstr'
                label val efftype efflbl
                sort efftype `clustervar'
                label define stid  0 "Overall", add
                label var eff "Effect"
                label var eff_lo "Effect, lower CI"
                label var eff_up "Effect, upper CI"
                label var eff_se "SE of the effect"
                label var weight "meta-analysis weight"
                qui save "`findir'\\`export'.dta", replace
            }
            qui use `tempsave', clear
        }

        /*the graph*/
        twoway scatter `sidrev' `eff`j'' [aweight=`weights'] if `sidrev'>0 , msymbol(square) mcolor(gs8) /*
        */ || scatter `sidrev' `eff`j'' if `sidrev'>0 , msymbol(diamond) msize(vsmall) mcolor(gs1) /*
        */ || rcap `lo95CI`j'' `up95CI`j'' `sidrev' if `sidrev'>0 , color(black) horizontal /*
        /*overall effect plotting - the diamond*/
        */ || function y= 0.3/(eff`j'-eff`j'lo)*(x-eff`j'lo), range(`=eff`j'lo' `=eff`j'') recast(area) fcolor(maroon) lcolor(maroon) fintensity(inten100) /*
        */ || function y= -0.3/(eff`j'-eff`j'lo)*(x-eff`j'lo), range(`=eff`j'lo' `=eff`j'') recast(area) fcolor(maroon) lcolor(maroon) fintensity(inten100) /*
        */ || function y= -0.3/(eff`j'up-eff`j')*(x-eff`j'up), range(`=eff`j'' `=eff`j'up') recast(area) fcolor(maroon) lcolor(maroon) fintensity(inten100) /*
        */ || function y= 0.3/(eff`j'up-eff`j')*(x-eff`j'up), range(`=eff`j'' `=eff`j'up') recast(area) fcolor(maroon) lcolor(maroon) fintensity(inten100) /*
        /*vertical line and other bits*/
        */ || pci 0.4 `=eff`j'' `=tstudynum' `=eff`j'', lcolor(maroon) lwidth(medium) lpattern(dash) /*
        */ , yscale(range(0 `=tstudynum')) ylabel(0(1)`=tstudynum', valuelabel labsize(2) angle(0)) ytitle("Studies", size(small)) /*
        */ xlabel(`lbmin'(`dstep')`lbmax' `plval',labsize(2.5) angle(0)) xtitle("Effect sizes and CIs (`orstr')" "`strout`j''", size(small)) /*
        */ xline(`plval', lstyle(grid))/*
        */ legend(off) ylabel(,nogrid noticks) nodraw

        /*resize graph if we have many MA*/
        if tstudynum>10 {
            graph display, ysize(`=min(`=tstudynum'/1.5,20)') xsize(7)
        }
        graph display Graph
        di "`findir'"
        di "`gsavename'"
        /*save if one of the save options provided*/
        if "`gsavedir'"!="" | "`gsavename'"!="" | "`eps'"!="" | "`gph'"!="" {
            if "`eps'"!="" {
                qui graph export "`findir'\\`gsavename'_`graphname`j''.eps", replace
            }
            if "`eps'"=="" | "`gph'"!="" {
                qui graph save "`findir'\\`gsavename'_`graphname`j''.gph", replace
            }
        }

        di as text _newline(2) "One-stage meta-analysis results using `modelsel' (`methtype' method) and ipdforest"
        di as text "`strout`j''"
        di as text "{hline `=maxstr+1'}{c TT}{hline `=maxstr+25'}
        di as text "{col 9}Study{col `=maxstr+2'}{c |}{col `=maxstr+6'}Effect{col `=maxstr+15'}[95% Conf. Interval]{col `=maxstr+37'} % Weight"
        di as text "{hline `=maxstr+1'}{c +}{hline `=maxstr+25'}
        forvalues i=`=tstudynum'(-1)1 {
            di as text labelvar[`i'] "{col `=maxstr+2'}{c |}" as result _col(`=maxstr+5') %7.3f `eff`j''[`i'] _col(`=maxstr+16') %7.3f `lo95CI`j''[`i'] /*
            */_col(`=maxstr+26') %7.3f `up95CI`j''[`i'] _col(`=maxstr+37') %7.2f `weights'[`i']*100
        }
        di as text "{hline `=maxstr+1'}{c +}{hline `=maxstr+25'}
        qui sum `weights'
        scalar sumweights = 100*r(sum)
        di as text %-20s "Overall effect {col `=maxstr+2'}{c |}" as result _col(`=maxstr+5') %7.3f `eff`j''[`=tstudynum+1'] _col(`=maxstr+16') /*
        */ %7.3f `lo95CI`j''[`=tstudynum+1'] _col(`=maxstr+26') %7.3f `up95CI`j''[`=tstudynum+1'] _col(`=maxstr+37') %7.2f sumweights
        di as text "{hline `=maxstr+1'}{c BT}{hline `=maxstr+25'}
        /*load file*/
        qui use `tempf', clear
        /*add a pause*/
        if `j'<effnum {
            more
        }
    }
    label drop studynames

    /*display heterogeneity measures*/
    /*I^2 and H^2 - from Higgins 2002 paper */
    /*scalar Hsq=(qw-(k-1))/(k-1)     relies on DL - not used*/
    scalar Hsq = (tausq + ssq)/ssq -1 /*this is H^2M as described by Mittlboeck*/
    scalar Hsqlo = (tsq_pl_lo + ssq)/ssq -1
    scalar Hsqup = (tsq_pl_up + ssq)/ssq -1
    foreach x in Hsq Hsqlo Hsqup {
        if `x'<0 scalar `x'=0
    }
    /*scalar Isq=100*(qw-(k-1))/qw   relies on DL method - not used*/
    scalar Isq = 100*(Hsq)/(Hsq+1)
    scalar Isqlo = 100*(Hsqlo)/(Hsqlo+1)
    scalar Isqup = 100*(Hsqup)/(Hsqup+1)
    foreach x in Isq Isqlo Isqup {
        if `x'<0 scalar `x'=0
    }

    /*more displays*/
    di as text _newline(2) "Heterogeneity Measures"
    /*cochran's Q*/
    di as text "{hline 15}{c TT}{hline 35}
    di as text "{col 16}{c |}{col 22}value{col 30}[95% Conf. Interval]"
    di as text "{hline 15}{c +}{hline 35}
    di as text %15s "I`=char(178)' (%) {col 16}{c |}" as result _col(20) %8.2f Isq /*
    */ _col(29) %8.2f Isqlo _col(38) %8.2f Isqup
    di as text %15s "H`=char(178)' {col 16}{c |}" as result _col(20) %8.2f Hsq /*
    */ _col(29) %8.2f Hsqlo _col(38) %8.2f Hsqup
    di as text %15s "tau`=char(178)' est {col 16}{c |}" as result _col(20) %8.3f tausq /*
    */ _col(29) %8.3f tsq_pl_lo _col(38) %8.3f tsq_pl_up
    di as text "{hline 15}{c BT}{hline 35}

    /*return list*/
    return scalar Isq = Isq
    return scalar Isqlo = Isqlo
    return scalar Isqup = Isqup
    return scalar Hsq = Hsq
    return scalar Hsqlo = Hsqlo
    return scalar Hsqup = Hsqup
    return scalar tausq = tausq
    return scalar tausqlo = tsq_pl_lo
    return scalar tausqup = tsq_pl_up
    forvalues i=`=studynum'(-1)1 {
        local ntmp = studynum - `i' + 1
        return scalar eff1se_st`=stid`ntmp'' = st`i'_1se
        return scalar eff1pe_st`=stid`ntmp'' = st`i'_1eff
    }
    return scalar eff1se_ov = eff1se
    return scalar eff1pe_ov = eff1
    forvalues j=2(1)`=effnum' {
        forvalues i=`=studynum'(-1)1 {
            local ntmp = studynum - `i' + 1
            return scalar eff`j'se_st`=stid`ntmp'' = st`i'_`j'se
            return scalar eff`j'pe_st`=stid`ntmp'' = st`i'_`j'eff
        }
        return scalar eff`j'se_ov = eff`j'se
        return scalar eff`j'pe_ov = eff`j'
    }
    /*load original dataset*/
    qui use `tempimp', clear
    /*return individual study effects in matrix in the future*/
    /*restore xtmixed results*/
    qui estimates restore mainreg
end

/*version 11.1
mata:
void xxx
    X=(st1_1eff,st1_1se)
    for (i=2; i<=studynum; i++) {
        X=(X\(st`=i'_1eff,st`=i'_1se))
    }
end*/






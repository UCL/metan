{smcl}
{* 13Oct2011}{...}
{hline}
help for {hi:ipdforest}
{hline}

{title:Title}

{p2colset 5 15 17 2}{...}
{p2col :{hi:ipdforest} {hline 2}} Forest plot for individual patient data IPD meta-analysis (one stage){p_end}
{p2colreset}{...}


{title:Syntax}

{p 4 8 2}
{cmd:ipdforest}
{it:varname}
[{cmd:,} {it:{help ipdforest##options:options}}]

{p 4 4 2}
where

{p 6 6 2}
{it:varname} the exposure variable (continuous or binary, e.g. intervention/control).

{synoptset 20 tabbed}{...}
{marker options}{...}
{synopthdr}
{synoptline}
{syntab :Options}
{synopt :{opth re(varlist)}}Random effects covariate(s)
{p_end}
{synopt :{opth fe(varlist)}}Fixed effect(s) covariate(s)
{p_end}
{synopt :{opth fets(namelist)}}Fixed study-specific effect(s) covariate(s)
{p_end}
{synopt :{opth ia(varname)}}Interaction covariate
{p_end}
{synopt :{opt auto}}Automatically detect model specification
{p_end}
{synopt :{opth label(varlist)}}Study label variable(s)
{p_end}
{synopt :{opt or}}Report Odds Ratios
{p_end}
{synopt :{opth gsavedir(string)}}Directory to save forest-plot(s)
{p_end}
{synopt :{opth gsavename(string)}}Name prefix for forest-plot(s)
{p_end}
{synopt :{opt eps}}Save forest-plot(s) in eps format
{p_end}
{synopt :{opt gph}}Save forest-plot(s) in gph format (default)
{p_end}
{synopt :{opth export(string)}}Export results to Stata file
{p_end}


{title:Description}

{p 4 4 2}
{cmd:ipdforest} is a post-estimation command which uses the saved estimates of an {cmd:xtmixed} or {cmd:xtmelogit} command for multi-level linear or
logistic regression respectively. It will only work following one of these estimation commands (as stand-alone or as part of a bootstrap or mi estimate
command) and will automatically identify the model and the outcome and cluster variables (both numerical). Only two-level data structures are allowed
(patients nested within studies). The command requires one variable name as input, the exposure grouping variable, and executes a
regression analysis for each study to obtain the study effects. Binary or continuous exposure variables are allowed and if exposure is categorical
users should create dummy variables and focus on the comparison of interest through one of those, under {cmd:ipdforest}. Users need to follow the model
specified in the multi-level regression command and include all independent variables for which the meta-analysis regression was controlled,
using the {opt re()}, {opt fe()}, {opt fets()} or {opt ia()} options. This ensures that covariates are modelled in the individual study regressions as
they were modelled in the multi-level regression (i.e. as random,fixed, or study-specific fixed factors). Alternatively, users can use use the
{opt auto} option and allow {cmd:ipdforest} to automatically detect the exact specification of the preceding regression model and this will work in
most situations (see below).
A table with the study and overall effects is provided along with heterogeneity measures (some can only be calculated for {cmd:xtmixed} since an
estimate of within-study variance is not returned with {cmd:xtmelogit}).

{title:Options}

{phang}
{opth re(varlist)} Covariates to be included as random factors. For each covariate specified, a different regression coefficient is estimated for
each study.

{phang}
{opth fe(varlist)} Covariates to be included as fixed factors. For each covariate specified, the respective coefficient in the study-specific
regressions is fixed to the value returned by the multi-level regression.

{phang}
{opth fets(namelist)} Covariates to be included as study-specific fixed factors (i.e. using the estimated study fixed effects from the main regression
in all individual study regressions). Only baseline scores and/or study identifiers can be included. For each covariate specified, the respective
coefficient in the study-specific regressions is fixed to the  value returned by the multi-level regression, for the specific study. For study-specific
intercepts the study identifier, not in factor variable format (e.g. studyid), or the stub of the dummy variables whould be included (e.g. studyid_ when
dummy study identifiers are studyid_1 studyid_3 etc). For study-specific baseline scores only the stub of the dummy variables is accepted (e.g.
dept0s_ when dummy study baseline scores are dept0s_1 dept0s_3 etc)

{phang}
{opth ia(varname)} Covariate for which the interaction with the exposure variable will be calculated and displayed. The covariate should
also be specified as a fixed, random or study-specific fixed effect. If binary, the command will provide two sets of results, one for each group.
If categorical, it will provide as many sets of results as there are categories. If continuous, it will provide one set of results for the main effect
and one for the interaction.
Although the command will accept a variable to be interacted with the exposure variable as a fixed or study-specific fixed effect, the variable
necessarily will be included as a random effect in the individual regressions (will not run a regression with the interaction term only, the main effects
must be included as well). Therefore, although the overall effect will differ between a model with a fixed effect interacted variable and a random effect
one, the individual study effects will be identical across the two approaches.

{phang}
{opt auto} Allows {cmd:ipdforest} to automatically detect the specification of the preceding model. This option cannot be issued along with options
{opt re()}, {opt fe()}, {opt fets()} or {opt ia()}. The {opt auto} option will work in most situations but it comes with certain limitations. It uses
the returned command string of the preceding command which is effectively constrained to 244 characters and therefore the auto option will return an
error if {cmd:ipdforest} follows a very wide regression model - in such a situation only the manual specification can be used. In addition, the variable
names used in the preceding model must follow certain rules: i) fixed-effect covariates (manually with option {opt fe()}) must not contain underscores,
ii) for study-specific intercepts (manually with option {opt fets()}) factor variable format is allowed or a {it:varlist} (e.g. cons_2-cons_16) but
each variable must contain a single underscore followed by the study number (not necessarily continuous) and iii) for study-specific baseline scores
(manually with option {opt fets()}) each variable must contain a single underscore followed by the study number (again, not necessarily continuous).
Note that there are no restrictions for random-effects covariates (manually with option {opt re()}). For interactions (manually with option {opt ia()})
the factor variable notation should be preferred (e.g. i.group#c.age) and alternatively the older {opt xi} notation. Interactions expanded to dummy
variables cannot be identified with the {opt auto} option and only the manual specification should be used in this case. Variables whose names start
with an '_I' and contain a capital 'X' will be assumed to be expanded interaction terms and, if detected in last model, {cmd:ipdforest} will
terminate with a syntax error.

{phang}
{opth label(varname)} Selects labels for the studies. Up to two variables can be selected and converted to strings. If two variables are selected they
will be separated by a comma. Usually, the author names and the year of study are selected as labels. If this option is not selected the command
automatically uses the value labels of the numeric cluster variable, if any, to label the forest plot. Either way, the final string is truncated to 30
characters.

{phang}
{opt or} Reporting odds ratios instead of coefficients. Can only be used following execution of {cmd:xtmelogit}.

{phang}
{opth gsavedir(string)} The directory where to save the graph(s), if different from the active directory.

{phang}
{opth gsavename(string)} Optional name prefix for the graph(s). Graphs are saved as `gsavename'_`graphname'.gph or `gsavename'_`graphname'.eps
where `graphname' includes a description of the summary effect (e.g. "main_group" for the main effect, if group is the intervention variable)

{phang}
{opt eps} Save the graph(s) in eps format, instead of the default gph.

{phang}
{opt gph} Save the graph(s) in gph format - the default. Use to save in both formats, since inlcluding only
the {opt eps} option will save the graph(s) in eps format only.

{phang}
{opth export(string)} Export the study identifiers, weights, effects and standard errors in a Stata dataset (named after {it:string}). Provided for users
wishing to use other commands or software to draw the forest plots.


{title:Remarks}

{p 4 4 2}
Each study estimate is calculated using a simple linear ({cmd:regress}) or logistic ({cmd:logit}) regression, limited to each study sample. The overall
effect is retrieved from the preceding mixed-effects regression command. Categorical variables can be specified with the 'i.' prefix, since the command
accepts factor variables.
If the multi-level regression was executed with the 'xi:' prefix, the dummy variables included in the model need to be specified individually. However,
we do not recommend this approach since full compatibility with interactions has not been ensured. Users should generate the interactions manually,
using the xi command before executing the regression command OR use the latest factor variable notation, available in v11 or later ({help fvvarlist}).
Although the command does not accept {opt if} or {opt in} options, it makes use of {cmd:e(sample)} in the preceding
regression command to automatically use the selected sample.
The forest plot(s) will be saved to disc if the user provides any of the four optional graph options.
The command is compatible with multiple imputation and bootstrap commands.
In the regression models please make sure you include both main effects and interactions, if you wish to investigate interactions; otherwise the command
will fail to execute.
A description of IPD meta-analysis methods and details in the use of {cmd:ipdforest} have been provided in a Stata Journal paper (http://www.stata-journal.com/article.html?article=st0309).

{title:Examples}

{p 4 4 2}
Assuming a dataset with six studies where {it:deptC} is a continuous outcome, {it:deptB} a binary outcome , {it:group} the intervention/control grouping
variable, {it:studyid} the numeric study cluster variable, {it:deptC_*} the baseline study dummy variables for the continuous outcome and {it:deptB_*}
the baseline study dummy variables for the binary outcome (e.g. if {it:deptC}_ is the outcome baseline variable, {it:deptC_1=deptC_ if studyid==1}).

{p 4 4 2}
Fixed common intercept; random treatment effect; fixed study-specific effect for baseline; fixed effects for age and sex:

{phang2}{cmd:. xtmixed deptC group age sex deptC_1 deptC_2 deptC_3 deptC_4 deptC_5 deptC_6 || studyid:group, nocons}{p_end}
{phang2}{cmd:. ipdforest group, fe(sex age) fets(deptC_)}{p_end}

{p 4 4 2}
Fixed study-specific intercept; random treatment effect; fixed study-specific effect for baseline; random effect sex, fixed for age:

{phang2}{cmd:. xtmelogit deptB group i.studyid age sex deptB_* || studyid:group sex, nocons}{p_end}
{phang2}{cmd:. ipdforest group, re(sex) fe(age) fets(studyid deptB_) label(author year) or}{p_end}

{p 4 4 2}
Random intercept; random treatment effect; fixed study-specific effect for baseline; random effects for age and sex, fixed effect for measure type:

{phang2}{cmd:. xtmelogit deptB group age sex i.measure i.group#i.measure deptB_* || studyid:group sex age, cov(uns)}{p_end}
{phang2}{cmd:. ipdforest group, re(sex age) fe(i.measure) fets(deptB_) ia(i.measure) label(author year) or}{p_end}

{p 4 4 2}
More examples provided in the Stata Journal paper. The data file used for the examples can be obtained from within Stata:

{phang2}{cmd:. net from http://www.stata-journal.com/software/sj13-3/}{p_end}
{phang2}{cmd:. net describe st0309}{p_end}

{title:Saved results}

{pstd}
{cmd:ipdforest} saves the following in {cmd:r()}:

{synoptset 20 tabbed}{...}
{p2col 5 20 24 2: Scalars}{p_end}
{synopt:{cmd:r(Isq)}}Heterogeneity measure I^2{p_end}
{synopt:{cmd:r(Isqlo)}}Heterogeneity measure I^2, lower 95% CI{p_end}
{synopt:{cmd:r(Isqup)}}Heterogeneity measure I^2, upper 95% CI{p_end}
{synopt:{cmd:r(Hsq)}}Heterogeneity measure H^2{p_end}
{synopt:{cmd:r(Hsqlo)}}Heterogeneity measure H^2, lower 95% CI{p_end}
{synopt:{cmd:r(Hsqup)}}Heterogeneity measure H^2, upper 95% CI{p_end}
{synopt:{cmd:r(tausq)}}Between study variance estimate tau^2{p_end}
{synopt:{cmd:r(tausqlo)}}Between study variance estimate tau^2, lower 95% CI{p_end}
{synopt:{cmd:r(tausqup)}}Between study variance estimate tau^2, upper 95% CI{p_end}
{synopt:{cmd:r(eff1pe_overall)}}Overall effect estimate{p_end}
{synopt:{cmd:r(eff1se_overall)}}Standard error of the overall effect{p_end}
{synopt:{cmd:r(eff1pe_st'i')}}Effect estimate for study 'i'{p_end}
{synopt:{cmd:r(eff1se_st'i')}}Standard error of the effect for study 'i'{p_end}

{pstd}
If an interaction with a continuous variable is included in the model the command also returns:

{synopt:{cmd:r(eff2pe_overall)}}Overall interaction effect estimate for{p_end}
{synopt:{cmd:r(eff2se_overall)}}Standard error of the overall interaction effect{p_end}
{synopt:{cmd:r(eff2pe_st'i')}}Interaction effect estimate for study 'i'{p_end}
{synopt:{cmd:r(eff2se_st'i')}}Interaction effect standard error for study 'i'{p_end}

{pstd}
If the variable interacted with the intervention is binary the command returns all the resuls described above, but the first set of effect results
corresponds to the effects for the first category of the binary (e.g. sex=0) and the second set for the second category (e.g. sex=1). If the variable
is categorical the command returns as many sets of effect results as there are categories (with each set corresponding to one category).


{title:Authors}

{p 4 4 2}
Evangelos Kontopantelis, Centre for Primary Care, Institute of Population Health

{p 29 4 2}
University of Manchester, e.kontopantelis@manchester.ac.uk

{p 4 4 2}
David Reeves, Centre for Primary Care, Institute of Population Health, University of Manchester


{title:Reference}

{p 4 4 2}
Kontopantelis E and Reeves D. A short guide and a forest plot command (ipdforest) for one-stage meta-analysis. The Stata Journal, 2013 Oct; 13(3): 574-587.


{title:Also see}

{p 4 4 2}
help for {help xtmixed}, {help xtmelogit}


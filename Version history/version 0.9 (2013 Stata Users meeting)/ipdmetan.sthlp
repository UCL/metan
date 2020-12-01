{smcl}
{* *! version 1.2.0  02jun2011}{...}
{vieweralsosee "metan" "help metan"}{...}
{vieweralsosee "forestplot" "help forestplot"}{...}
{vieweralsosee "aggmetan" "help aggmetan"}{...}
{vieweralsosee "ipdover" "help ipdover"}{...}
{viewerjumpto "Syntax" "ipdmetan##syntax"}{...}
{viewerjumpto "Description" "ipdmetan##description"}{...}
{viewerjumpto "Options" "ipdmetan##options"}{...}
{viewerjumpto "Saved results" "ipdmetan##saved_results"}{...}
{title:Title}

{phang}
{cmd:ipdmetan} {hline 2} Perform inverse-variance individual participant data (IPD) meta-analysis


{marker syntax}{...}
{title:Syntax}

{p 8 18 2}
{cmd:ipdmetan}
	[{it:{help exp_list}}]
	{cmd:, study(}{it:study_ID}{cmd:)} [{it:options}] {cmd::} [{it:command}]

{synoptset 24 tabbed}{...}
{synopthdr}
{synoptline}
{syntab :General}
{synopt :{opt study(study_ID)}}specify study identifier{p_end}
{synopt :{opt by(subgroup_ID)}}group the studies in the output{p_end}
{synopt :{opt effect(string)}}title for "effect size" column in the output{p_end}
{synopt :{opt eform}}exponentiate effect sizes and confidence limits{p_end}
{synopt :{opt keepall}}display all values of study_ID in the output, even if no effect could be estimated{p_end}
{synopt :{opt messages}}print messages relating to success of model fits{p_end}
{synopt :{opt nograph}}suppress the forest plot{p_end}
{synopt :{opt nooverall}}suppress overall pooling{p_end}
{synopt :{opt nosubgroup}}suppress pooling within subgroups{p_end}
{synopt :{opt notable}}suppress printing the table of effect sizes to screen{p_end}
{synopt :{opt re}}specify the DerSimonian & Laird random-effects model{p_end}
{synopt :{opt re(random_effects_model)}}specify alternative random-effects models{p_end}

{syntab :Specific to IPD (or combined IPD/aggregate) analysis}
{synopt :{cmd:aggregate(}{it:{help filename}} {ifin}{cmd:,} {help ipdmetan##aggregate_data_options:{it:aggregate_data_options}}{cmd:)}}
analyse aggregate data stored in {it:filename}{p_end}
{synopt :{opt interaction}}automatically identify and pool a treatment-covariate interaction{p_end}
{synopt :{opt nodepvar}}specify that {it:command} syntax has no dependent variable{p_end}
{synopt :{opt poolvar(coef)}}specify explicitly the coefficient to pool (alternative to {it:exp_list}){p_end}

{syntab :Specific to forest plots}
{synopt :{opt lcols(cols_info)} {opt rcols(cols_info)}}display (and/or save) columns of additional data{p_end}
{synopt :{opt saving(filename, ...)}}save results in the form of a "forest plot dataset" to {it:filename}{p_end}
{synopt :{cmd:forestplot(}{help forestplot##options:{it:forestplot_options}}{cmd:)}}other options to pass to {help forestplot}{p_end}


{synoptline}
	

{marker aggregate_data_options}{...}
{synopthdr :aggregate_data_options}
{synoptline}
{synopt :{opt byagg}}IPD and aggregate data are to be treated as subgroups (rather than as a single set of estimates){p_end}
{synopt :{opt npts(varname)}}specify variable containing patient numbers{p_end}
{synopt :{opt vars(varlist)}}specify variables containing effect size and either standard error or 95% confidence limits, on the normal scale{p_end}

{synoptline}

{p2colreset}{...}
{p 4 6 2}


{marker description}{...}
{title:Description}

{pstd}
{cmd:ipdmetan} performs two-stage individual patient-data (IPD) meta-analysis using the inverse-variance method.  Basic syntax is

{phang2}
{cmd:. ipdmetan}{cmd:,} {opt study(study_ID)} {cmd::} {it:command}

{pstd}
which fits the model {it:command} once within each level of {it:study_ID}
and stores effect sizes and standard errors in a matrix for pooling, output and display of a forest plot.
Any e-class regression command (Stata or user-defined) should be compatible with
{cmd:ipdmetan}.

{pstd}
In the case of user-defined and/or non e-class commands, the effect size and standard error statistics
to be collected from the execution of {it:command} may be specified manually by supplying {it:{help exp_list}}.
If {it:command} changes the contents in {cmd:e(b)}, {it:exp_list} defaults to
{cmd:_b[}{it:varname}{cmd:]} {cmd:_se[}{it:varname}{cmd:]},
where {it:varname} is the first independent variable supplied to {it:command}.

{pstd}
Aggregate (summary) data may be included in the analysis using the syntax:

{phang2}
{cmd:. ipdmetan}{cmd:,} {opt study(study_ID)} {cmd:aggregate(}{it:{help filename}, {help ipdmetan##aggregate_data_options:aggregate_data_options}}{cmd:)} {cmd::} {it:command}

{pstd}If {cmd:aggregate()} is specified, {it:{help filename}} is required.
If no options are supplied, {cmd:ipdmetan} assumes a data file containing variables named "ES" (the effect size)
and "seES" (standard error of effect size).
This may be overridden by specifying the {opt vars(namelist)} option, containing names of variables containing
the effect size and either a standard error or 95% confidence limits, on the normal scale.
If patient numbers are available, the variable containing them may be specified using the {opt npts(name)} option.
Finally, if aggregate and IPD data respectively are to be treated as subgroups, the {opt byagg} option should
be specified.  More generally, subgroups may be analysed in the same way as for IPD - that is, with the {opt by(name)} option.




{marker options}{...}
{title:Options}

{dlgtab:Analysis}

{phang}
{opt study(study_ID)} (required) specifies the variable containing the study identifier.
Any format is permitted (text, integer or non-integer); text or non-integer formats will be converted
to numeric with value labels.

{phang}
{opt by(subgroup_ID)} specifies the variable containing the subgroup identifier,
which must be constant within studies (i.e. it must be a study-level variable).

{phang}
{opt effect(string)} specifies a heading for the effect size column in the output (table and forest plot).

{phang}
{opt eform} specifies that the effect sizes and confidence limits should be exponentiated.
Note that {cmd:ipdmetan} expects effect sizes to be beta coefficients (i.e. on the linear scale)

{phang}
{opt interaction} specifies that {it:command} contains one or more interaction effects
supplied using factor-variable syntax (see {help fvvarlist}),
and that the first valid interaction effect should be pooled across studies.
This is intended as a helpful shortcut for simple interaction analyses, but it is not foolproof or comprehensive.
The alternative is to supply the desired statistics to be pooled directly to ipdmetan using {cmd:poolvar()} or {it:exp_list}.

{phang}
{opt keepall} specifies that all values of {it:study_ID} should be visible in the output (table and forest plot),
even if no effect could be estimated (e.g. due to insufficient observations or missing data).
For such studies, "(Insufficient data)" will appear in place of effect estimates and weights.

{phang}
{opt messages} requests that information is printed to screen regarding whether effect size and standard error statistics
have been successfully obtained from each study.

{phang}
{opt nograph} specifies that construction of the forest plot should be suppressed.

{phang}
{opt nooverall} specifies that overall pooling (and relevant heterogeneity calculations)
should be suppressed.

{phang}
{opt nosubgroup} specifies that within-subgroup pooling (and relevant heterogeneity calculations)
should be suppressed.  This option is ignored if the option {opt by(subgroup_ID)} is absent or invalid.

{phang}
{opt notable} suppresses the printing to screen of the table of effect sizes.

{phang}
{opt poolvar(model_coefficient)} allows the coefficient to be pooled to be explicitly stated in situations where it may not be obvious, or where {cmd:ipdmetan} has made an incorrect assumption. {it:model_coefficient} should be a variable name,
a level indicator, an interaction indicator, or an interaction involving continuous variables (c.f. syntax of {help test}).

{phang}
{opt re} or {opt random} specifies DerSimonian & Laird random-effects

{phang}
{cmd:re(}{it:random_effects_type}{cmd:, notrunc)} or {cmd:random(}{it:random_effects_type}{cmd:, notrunc)} specifies other possible random-effects analyses.  Currently-supported types are:

{tab}{cmd:vb}, {cmd:q} or {cmd:genq}{tab}{tab}Generalised Q random-effects
{tab}{cmd:bs}, {cmd:bt} or {cmd:gamma}{tab}{tab}Approximate Gamma random-effects

{pmore}
If {it:random_effects_type} is not specified, the DerSimonian & Laird random-effects model is assumed.

{pmore}
{opt notrunc} requests that estimates of
tau{char 178} be allowed to be less than zero if appropriate.
This is an option for programmers and for simulation purposes only.



{dlgtab:Forest plots}

{phang}
{opt lcols(cols_info)}, {opt rcols(cols_info)} define columns of additional data to be presented to the left or right of the forest plot.
These options are carried over from {help metan}, but in the IPD context they must first be generated from the existing dataset.
{cmd:ipdmetan} creates a new dataset of effect sizes, weights, labels etc. to pass to {help forestplot},
which may also contain variables representing such additional columns.
Hence, the syntax of {opt lcols(cols_info)} and {opt rcols(cols_info)} allows the user to specify characteristics of new variables
such as name, title and format, which will be carried over to the forest plot.

{pmore}
{it:cols_info} has the following syntax, which is based on that of {help collapse}:

{pmore2}
[{opt (stat)}] [{it:newname}=]{it:item} [{it:%fmt} {cmd:"}{it:label}{cmd:"}] [[{it:newname}=]{it:item} [{it:%fmt} {cmd:"}{it:label}{cmd:"}] ] {it:...} [ [{opt (stat)}] {it:...}]

{pmore}
where {it:stat} is as defined in {help collapse},
{it:newname} is an optional user-specified variable name (see below),
{it:item} is the name of either a numeric returned quantity from {it:command} or a variable currently in memory,
{it:%fmt} is an optional {help format} (existing formatting is used by default wherever possible),
and {cmd:"}{it:label}{cmd:"} is an optional variable label.

{pmore}
Specifying {it:newname} is only necessary in circumstances where the name of the variable in the {help forestplot} dataset is important.
For example, you may have an aggregate dataset with a variable containing data equivalent to an {it:item},
and wish for all such data (whether IPD or aggregate) to appear in a single column in the forest plot.
To achieve this, specify {it:newname} as the name of the relevant variable in the aggregate dataset.

{pmore}
Note that {it:item} may be an existing string variable, in which case the first non-empty observation for each study will be used
- that is, the relevant {it:stat} will default to {cmd:firstnm}.

{pmore}
{cmd:lcols} and {cmd:rcols} may also be supplied directly to {help forestplot}, but as a list of existing variable names only.

{phang}
{cmd:saving(}{it:filename}{cmd:, }{it:saving_options}{cmd:)} saves the "forest plot" data created by {cmd:ipdmetan} in a Stata datafile
for further use or manipulation (e.g. stacking forest plots - see the {opt using} option of {help forestplot})

{pmore}{it:saving_options} are:

{tab}{cmd:replace}{tab}{tab}overwrite {it:filename}
{tab}{cmd:stack}{tab}{tab}moves the left-hand column title inside the plot region
{tab}{tab}{tab}(for later combination with other datasets using {help forestplot}.



{marker saved_results}{...}
{title:Saved results}

{pstd}
{cmd:ipdmetan} saves the following in {cmd:r()} (with some variation):

{synoptset 25 tabbed}{...}
{p2col 5 25 29 2: Scalars}{p_end}
{synopt:{cmd:r(k)}}Number of included studies {it:k}{p_end}
{synopt:{cmd:r(n)}}Number of included patients{p_end}
{synopt:{cmd:r(mu_hat)}}Overall (pooled) effect size{p_end}
{synopt:{cmd:r(se_mu_hat)}}Standard error of overall (pooled) effect size{p_end}
{synopt:{cmd:r(Q)}}Q statistic of heterogeneity (N.B. has degrees of freedom {it:k}–1){p_end}
{synopt:{cmd:r(tausq)}}Between-study variance tau-squared{p_end}
{synopt:{cmd:r(sigmasq)}}‘Typical’ within-study variance{p_end}
{synopt:{cmd:r(Isq)}}Heterogeneity measure I-squared{p_end}
{synopt:{cmd:r(HsqM)}}Heterogeneity measure H-squared (Mittlböck modification){p_end}

{synoptset 25 tabbed}{...}
{p2col 5 25 29 2: Macros}{p_end}
{synopt:{cmd:r(type)}}Analysis method type{p_end}
{synopt:{cmd:r(estvar)}}Name of pooled coefficient{p_end}
{synopt:{cmd:r(se_estvar)}}Name of "pooled" standard error (i.e. that used for inverse-variance weighting){p_end}

{synoptset 25 tabbed}{...}
{p2col 5 25 29 2: Matrices}{p_end}
{synopt:{cmd:r(coeffs)}}Matrix of study effect coefficients, plus any additional data requested
using {opt lcols()} or {opt rcols()} (these latter whether or not a graph was requested){p_end}

{synoptset 25 tabbed}{...}
{p2col 5 25 29 2: Variables}{p_end}
{synopt:{cmd:_rsample}}Observations included in the analysis (c.f. {cmd:e(sample)}){p_end}


{pstd}
If {cmd:type(q)} is specified (or any of its synonyms) the following additional results are saved:

{synoptset 25 tabbed}{...}
{p2col 5 25 29 2: Scalars}{p_end}
{synopt:{cmd:r(tsq_lci)}}Lower confidence limit for tau-squared{p_end}
{synopt:{cmd:r(tsq_uci)}}Upper confidence limit for tau-squared{p_end}

{pstd}
If {cmd:type(gamma)} is specified (or any of its synonyms) the following additional results are saved:

{synoptset 25 tabbed}{...}
{p2col 5 25 29 2: Scalars}{p_end}
{synopt:{cmd:r(tsq_var)}}Estimated variance of tau-squared{p_end}
{synopt:{cmd:r(tsq_lci)}}Lower confidence limit for tau-squared{p_end}
{synopt:{cmd:r(tsq_uci)}}Upper confidence limit for tau-squared{p_end}


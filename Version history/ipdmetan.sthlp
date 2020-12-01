{smcl}
{* *! version 1.0  David Fisher  31jan2014}{...}
{vieweralsosee "metan" "help metan"}{...}
{vieweralsosee "forestplot" "help forestplot"}{...}
{vieweralsosee "admetan" "help admetan"}{...}
{vieweralsosee "ipdover" "help ipdover"}{...}
{viewerjumpto "Syntax" "ipdmetan##syntax"}{...}
{viewerjumpto "Description" "ipdmetan##description"}{...}
{viewerjumpto "Options" "ipdmetan##options"}{...}
{viewerjumpto "Saved results" "ipdmetan##saved_results"}{...}
{title:Title}

{phang}
{cmd:ipdmetan} {hline 2} Perform two-stage inverse-variance individual participant data (IPD) meta-analysis


{marker syntax}{...}
{title:Syntax}

{p 8 18 2}
{cmd:ipdmetan}
	[{it:{help exp_list}}]
	{cmd:, {ul:s}tudy(}{it:varname} [{cmd:, {ul:m}issing}]{cmd:)} [{it:options}] {cmd::} {it:command}

{synoptset 34 tabbed}{...}
{synopthdr}
{synoptline}
{syntab :Main}
{synopt :{cmd:by(}{it:varname} [{cmd:, {ul:m}issing}]{cmd:)}}group the studies in the output{p_end}
{synopt :{it:{help eform_option}}}exponentiate effect sizes and confidence limits{p_end}
{synopt :{opt eff:ect(string)}}title for "effect size" column in the output{p_end}
{synopt :{opt inter:action}}automatically identify and pool a treatment-covariate interaction{p_end}
{synopt :{opt keepall}}display all studies in the output, even those for which no effect could be estimated{p_end}
{synopt :{opt me:ssages}}print messages relating to success of model fits{p_end}
{synopt :{opt nogr:aph}}suppress the forest plot{p_end}
{synopt :{opt nohet}}suppress all heterogeneity statistics{p_end}
{synopt :{opt noov:erall}}suppress overall pooling{p_end}
{synopt :{opt nosu:bgroup}}suppress pooling within subgroups{p_end}
{synopt :{opt notab:le}}suppress printing the table of effect sizes to screen{p_end}
{synopt :{opt notot:al}}suppress fitting of {it:command} to the entire dataset{p_end}
{synopt :{opt pool:var(model_coefficient)}}specify explicitly the coefficient to pool{p_end}
{synopt :{opt re}}specify the DerSimonian & Laird random-effects model{p_end}
{synopt :{cmd:re(}{help ipdmetan##re_model:{it:re_model}}{cmd:)}}specify alternative random-effects models{p_end}
{synopt :{cmd:sortby(}{it:varname}|{cmd:_n)}}specify ordering of studies in table and forestplot{p_end}
{synopt :{opt t z}}specify the distribution for calculating confidence limits{p_end}

{syntab :Combined IPD/aggregate data analysis}
{synopt :{cmd:ad(}{it:{help filename}} {ifin}{cmd:,} {help ipdmetan##aggregate_data_options:{it:aggregate_data_options}}{cmd:)}}
combine IPD with aggregate data stored in {it:filename}{p_end}

{syntab :Forest plots}
{synopt :{cmdab:lcol:s(}{help ipdmetan##cols_info:{it:cols_info}}{cmd:)} {cmdab:rcol:s(}{help ipdmetan##cols_info:{it:cols_info}}{cmd:)}}
display (and/or save) columns of additional data{p_end}
{synopt :{cmd:ovstat(q)}}display Q statistics instead of I-squared{p_end}
{synopt :{cmd:plotid(}{it:varname}{cmd:|_BYAD} [{cmd:, list nograph}]{cmd:)}}
define groups of observations in which to apply specific plot rendition options{p_end}
{synopt :{cmdab:sa:ving(}{it:{help filename}} [{cmd:, replace} {cmdab:stack:label}]{cmd:)}}save results in the form of a "forestplot dataset" to {it:filename}{p_end}
{synopt :{cmdab:forest:plot(}{help forestplot##options:{it:forestplot_options}}{cmd:)}}other options to pass to {help forestplot}{p_end}
{synoptline}

{pstd}
where {it:model_coefficient} is a variable name, a level indicator, an interaction indicator,
or an interaction involving continuous variables (c.f. syntax of {help test})

{marker cols_info}{...}
{pstd}
and where {it:cols_info} has the following syntax, which is based on that of {help collapse}:

{pmore}
[{opt (stat)}] [{it:newname}=]{it:item} [{it:%fmt} {cmd:"}{it:label}{cmd:"}] [[{it:newname}=]{it:item} [{it:%fmt} {cmd:"}{it:label}{cmd:"}] ] {it:...} [ [{opt (stat)}] {it:...}]

{pmore}
where {it:stat} is as defined in {help collapse};
{it:newname} is an optional user-specified variable name;
{it:item} is the name of either a numeric returned quantity from {it:command} (in parentheses, see {help exp_list})
or a variable currently in memory; {it:%fmt} is an optional {help format}; and {cmd:"}{it:label}{cmd:"} is an optional variable label.

{marker re_model}{...}
{synopthdr :re_model}
{synoptline}
{synopt :{opt vb}, {opt q} or {opt genq}}Generalised Q random-effects{p_end}
{synopt :{opt bs}, {opt bt} or {opt gamma}}Approximate Gamma random-effects{p_end}
{synopt :{opt vc} or {opt ca}}Variance-component aka Cochran ANOVA-type random-effects{p_end}
{synopt :{opt sj}}(improved) Sidik-Jonkman random-effects (implies {cmd:t}){p_end}
{synopt :{opt ml}}(simple) maximum likelihood (ML) -based random-effects{p_end}
{synopt :{opt pl}}"Profile" maximum likelihood-based random-effects{p_end}
{synopt :{opt reml}}Restricted maximum likelihood (REML) -based random-effects{p_end}
{synoptline}

{marker aggregate_data_options}{...}
{synopthdr :aggregate_data_options}
{synoptline}
{synopt :{opt byad}}IPD and aggregate data are to be treated as subgroups (rather than as a single set of estimates){p_end}
{synopt :{opt npts(varname)}}specify variable containing participant numbers{p_end}
{synopt :{opt vars(varlist)}}specify variables containing effect size and either standard error or 95% confidence limits, on the normal scale{p_end}
{synoptline}

{p2colreset}{...}
{p 4 6 2}

{marker description}{...}
{title:Description}

{pstd}
{cmd:ipdmetan} performs two-stage individual participant-data (IPD) meta-analysis using the inverse-variance method.  Basic syntax is

{phang2}
{cmd:. ipdmetan}{cmd:,} {opt study(study_ID)} {cmd::} {it:command}

{pstd}
which fits the model {it:command} once within each level of {it:study_ID}
and saves effect sizes and standard errors for pooling, output and displaying in a forest plot.
Any e-class regression command (whether built-in or user-defined) should be compatible with this basic syntax of {cmd:ipdmetan}.

{pstd}
In the case of non e-class commands - those which do not change the contents of {cmd:e(b)} -,
the effect size and standard error statistics to be collected from the execution of {it:command}
must be specified manually by supplying {it:{help exp_list}}.
If {it:command} changes the contents in {cmd:e(b)}, {it:exp_list} defaults to
{cmd:_b[}{it:varname}{cmd:]} {cmd:_se[}{it:varname}{cmd:]},
where {it:varname} is the first independent variable within {it:command}.

{marker options}{...}
{title:Options}

{dlgtab:Main}

{phang}
{cmd:study(}{it:study_ID} [{cmd:, missing}]{cmd:)} (required) specifies the variable containing the study identifier,
which must be either integer-valued or string.

{pmore}
{opt missing} requests that missing values be treated as potential study identifiers (the default is to exclude them).

{phang}
{cmd:by(}{it:subgroup_ID} [{cmd:, missing}]{cmd:)} specifies a variable identifying subgroups of studies (and must therefore be constant within studies),
which must be either integer-valued or string.

{pmore}
{opt missing} requests that missing values be treated as potential subgroup identifiers (the default is to exclude them).

{phang}
{it:{help eform_option}} specifies that effect sizes and confidence limits should be exponentiated in the output (table and forest plot).
The option also generates a heading for the effect size column.

{pmore}
Note that {cmd:ipdmetan} does not check the validity of the particular {it:{help eform_option}} used;
e.g. whether {it:command} is a survival model if {opt hr} is supplied.

{phang}
{opt effect(string)} specifies a heading for the effect size column in the output.
This overrides any heading generated by {it:{help eform_option}}.

{phang}
{opt interaction} specifies that {it:command} contains one or more interaction effects
supplied using factor-variable syntax (see {help fvvarlist}),
and that the first valid interaction effect should be pooled across studies.
This is intended as a helpful shortcut for simple interaction analyses, but it is not foolproof or comprehensive.
The alternative is to supply the desired coefficient to be pooled directly to ipdmetan using {cmd:poolvar()}.

{phang}
{opt keepall} specifies that all values of {it:study_ID} should be visible in the output (table and forest plot),
even if no effect could be estimated (e.g. due to insufficient observations or missing data).
For such studies, "(Insufficient data)" will appear in place of effect estimates and weights.

{phang}
{opt messages} requests that information is printed to screen regarding whether effect size and standard error statistics
have been successfully obtained from each study, and (if applicable) whether the iterative random-effects calculations
converged successfully.

{phang}
{opt nograph}, {opt notable} request the suppression of, respectively,
construction of the forest plot and the table of effect sizes.

{phang}
{opt nohet} suppresses all heterogeneity statistics.

{phang}
{opt nooverall}, {opt nosubgroup} affect which groups of data are pooled, thus affecting both the table of effect sizes
and the forestplot (if applicable).

{pmore} {opt nooverall} suppresses the overall pooled effect, so that (for instance) subgroups are considered entirely
independently. Between-subgroup heterogeneity statistics (if applicable) are also suppressed.

{pmore} {opt nosubgroup} suppresses the within-subgroup pooled effects (if applicable), so that subgroups are displayed
separately but with a single overall pooled effect with associated heterogeneity statistics.

{phang}
{opt nototal} requests that {it:command} not be fitted within the entire dataset, e.g. for time-saving reasons.
By default, such fitting is done to check for problems in convergence and in the validity of requested coefficients and
returned expressions. If {opt nototal} is specified, either {opt poolvar()} or {it:exp_list} must be supplied,
and a message appears above the table of results warning that estimates should be double-checked by the user.

{phang}
{opt poolvar(model_coefficient)} allows the coefficient to be pooled to be explicitly stated in situations where it may not be obvious,
or where {cmd:ipdmetan} has made a previous incorrect assumption. {it:model_coefficient} should be a variable name,
a level indicator, an interaction indicator, or an interaction involving continuous variables (c.f. syntax of {help test}).
To use equations, use the format {cmd:poolvar(}{it:eqname}{cmd::}{it:varname}{cmd:)}.

{phang}
{opt re} or {opt random} specifies DerSimonian & Laird random-effects

{phang}
{cmd:re(}{help ipdmetan##re_model:{it:re_model}}{cmd:)} or {cmd:random(}{help ipdmetan##re_model:{it:re_model}}{cmd:)}
specifies other possible random-effects models. For details of these models,
see the reference list at the end of this page, or the forthcoming Stata Journal article.

{pmore}
If {help ipdmetan##re_model:{it:re_model}} is not specified, the DerSimonian & Laird random-effects model is assumed.

{phang}
{cmd:sortby(}{it:varname}|{cmd:_n)} allows user-specified ordering of studies in the table and forestplot.
The default ordering is by {it:study_ID}. Note that {opt sortby} does not alter the data in memory.

{pmore}
To order the studies by their first appearance in the data (using the current sort order), specify {cmd:sortby(_n)}.

{phang}
{opt t}, {opt z} specify the distribution (Student's t or Normal) to use when calculating confidence limits for the pooled estimate(s).
{opt z} is the default unless {cmd:re(sj)} is specified.

{dlgtab:Combined IPD/aggregate data analysis}

{phang}
{cmd:ad(}{it:{help filename}} {ifin}{cmd:,} {help ipdmetan##aggregate_data_options:{it:aggregate_data_options}}{cmd:)}
allows aggregate (summary) data may be included in the analysis alongside IPD, for example if some studies do not have IPD available.
If {cmd:ad()} is specified, {it:filename} and {opt vars(varlist)} are required.

{pmore}
{opt vars(varlist)} contains the names of variables (within {it:filename})
containing the effect size and either a standard error or lower and upper 95% confidence limits, on the linear scale.

{pmore}
{opt npts(varname)} allows participant numbers (stored in {it:varname} within {it:filename}) to be displayed in tables and forestplots.

{pmore}
{opt byad} specifies that aggregate data and IPD respectively are to be treated as subgroups.

{pmore}
Note that subgroups may be analysed in the same way as for IPD - that is, with the {opt by(varname)} option to {cmd:ipdmetan}.
{it:varname} may be found in either the data in memory (IPD) or in the aggregate dataset, or both.

{dlgtab:Forest plots}

{phang}
{cmd:lcols(}{help ipdmetan##cols_info:{it:cols_info}}{cmd:)}, {cmd:rcols(}{help ipdmetan##cols_info:{it:cols_info}}{cmd:)} define columns of additional data to be presented to the left or right of the forest plot.
These options are carried over from {help metan}, but in the IPD context they must first be generated from the existing dataset.
{cmd:ipdmetan} creates a new dataset of effect sizes, weights, labels etc. to pass to {help forestplot},
which may also contain variables representing such additional columns.
Hence, the syntax of {help ipdmetan##cols_info:{it:cols_info}} allows the user to specify characteristics of new variables
such as name, title and format, which will be carried over to the forest plot.

{pmore}
Specifying {it:newname} is only necessary in circumstances where the name of the variable in the {help forestplot} dataset is important.
For example, you may have an aggregate dataset with a variable containing data equivalent to an {it:item},
and wish for all such data (whether IPD or aggregate) to appear in a single column in the forest plot.
To achieve this, specify {it:newname} as the name of the relevant variable in the aggregate dataset.
Make sure that the variables in the IPD and aggregate datasets do not have conflicting formats (e.g. string and numeric)
or a {help merge} error will be returned.

{pmore}
Note that {it:item} may be an existing string variable, in which case the first non-empty observation for each study will be used,
and the {it:item} will not be displayed alongside overall or subgroup pooled estimates.
To force this behaviour for a numeric variable, it must first be converted to string format using {help recode} or {help tostring}.

{pmore}
{cmd:lcols} and {cmd:rcols} may also be supplied directly to {help forestplot}, but as a list of existing variable names only.

{phang}
{cmd:plotid(}{it:varname}{cmd:|_BYAD} [{cmd:, list nograph}]{cmd:)} is really a {help forestplot} option, but has a slightly
extended syntax when supplied to {cmd:ipdmetan}.  {it:varname} may be replaced with {cmd:_BYAD} if the {opt byad} suboption
is supplied to {opt ad}, since in this case the subgrouping is not defined by an existing variable.

{phang}
{cmd:saving(}{it:{help filename}} [{cmd:, replace} {cmd:stacklabel}]{cmd:)} saves the forestplot "resultsset" created by
{cmd:ipdmetan} in a Stata data file for further use or manipulation. See {help forestplot} for further details.

{pmore}
{opt replace} overwrites {it:filename}

{pmore}
{opt stacklabel} is a subtlety: it takes the {it:{help label:variable label}} from the left-most column variable (usually {it:study_ID}),
which would usually appear outside the plot region as the column heading, and copies it into a new first row in {it:filename}.
This allows multiple such datasets to be {help append}ed without this information being overwritten.


{marker saved_results}{...}
{title:Saved results}

{pstd}
{cmd:ipdmetan} saves the following in {cmd:r()} (with some variation):

{synoptset 25 tabbed}{...}
{p2col 5 25 29 2: Scalars}{p_end}
{synopt:{cmd:r(k)}}Number of included studies {it:k}{p_end}
{synopt:{cmd:r(n)}}Number of included participants{p_end}
{synopt:{cmd:r(mu_hat)}}Overall (pooled) effect size{p_end}
{synopt:{cmd:r(se_mu_hat)}}Standard error of overall (pooled) effect size{p_end}
{synopt:{cmd:r(Q)}}Q statistic of heterogeneity (N.B. has degrees of freedom {it:k}–1){p_end}
{synopt:{cmd:r(tausq)}}Between-study variance tau-squared{p_end}
{synopt:{cmd:r(sigmasq)}}Average within-study variance{p_end}
{synopt:{cmd:r(Isq)}}Heterogeneity measure I-squared{p_end}
{synopt:{cmd:r(HsqM)}}Heterogeneity measure H-squared (Mittlböck modification){p_end}

{synoptset 25 tabbed}{...}
{p2col 5 25 29 2: Macros}{p_end}
{synopt:{cmd:r(re_model)}}Random-effects model used{p_end}
{synopt:{cmd:r(estvar)}}Name of pooled coefficient{p_end}

{synoptset 25 tabbed}{...}
{p2col 5 25 29 2: Matrices}{p_end}
{synopt:{cmd:r(coeffs)}}Matrix of study and subgroup identifers, effect coefficients, numbers of participants, and weights{p_end}

{synoptset 25 tabbed}{...}
{p2col 5 25 29 2: Variables}{p_end}
{synopt:{cmd:_rsample}}Observations included in the analysis (c.f. {cmd:e(sample)}){p_end}


{pstd}
Certain iterative random-effects models may save the following additional results:{p_end}
{pstd}
(see {help mf_mm_root} for interpretations of convergence success values)

{synoptset 25 tabbed}{...}
{p2col 5 25 29 2: Scalars}{p_end}
{synopt:{cmd:r(tsq_var)}}Estimated variance of tau-squared{p_end}
{synopt:{cmd:r(tsq_lci)}}Lower confidence limit for tau-squared{p_end}
{synopt:{cmd:r(tsq_uci)}}Upper confidence limit for tau-squared{p_end}
{synopt:{cmd:r(rc_tausq)}}Whether tau-squared point estimate converged successfully{p_end}
{synopt:{cmd:r(rc_tausq_lci)}}Whether tau-squared lower confidence limit converged successfully{p_end}
{synopt:{cmd:r(rc_tausq_uci)}}Whether tau-squared upper confidence limit converged successfully{p_end}
{synopt:{cmd:r(rc_mu_lci)}}Whether mu_hat lower confidence limit converged successfully{p_end}
{synopt:{cmd:r(rc_mu_uci)}}Whether mu_hat upper confidence limit converged successfully{p_end}


{title:Examples}

{pstd}
Examples (including an example dataset) will be available soon.
In the meantime, please contact the author.


{title:Author}

{p}
David Fisher, MRC Clinical Trials Unit at UCL, London, UK.

Email {browse "mailto:d.fisher@ucl.ac.uk":d.fisher@ucl.ac.uk}


{title:Acknowledgments}

{pstd}
Thanks to the authors of {help metan}, upon which this code is based;
paticularly Ross Harris for his comments and good wishes.

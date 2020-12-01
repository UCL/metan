{smcl}
{* *! version 3.4 (beta)  David Fisher  12dec2019}{...}
{vieweralsosee "metan_model" "help metan_model"}{...}
{vieweralsosee "metan_binary" "help metan_binary"}{...}
{vieweralsosee "metan_continuous" "help metan_continuous"}{...}
{vieweralsosee "metan_proportion" "help metan_proportion"}{...}
{vieweralsosee "forestplot" "help forestplot"}{...}
{vieweralsosee "metani" "help metani"}{...}
{vieweralsosee "" "--"}{...}
{vieweralsosee "ipdmetan" "help ipdmetan"}{...}
{vieweralsosee "ipdover" "help ipdover"}{...}
{vieweralsosee "metabias" "help metabias"}{...}
{vieweralsosee "metaan" "help metaan"}{...}
{vieweralsosee "metandi" "help metandi"}{...}
{vieweralsosee "metaprop_one" "help metaprop_one"}{...}
{viewerjumpto "Syntax" "metan##syntax"}{...}
{viewerjumpto "Description" "metan##description"}{...}
{viewerjumpto "Options" "metan##options"}{...}
{viewerjumpto "Saved results" "metan##saved_results"}{...}
{viewerjumpto "Saved datasets" "metan##saved_datasets"}{...}
{viewerjumpto "Note: differences from previous version of metan" "metan##diffs_metan"}{...}
{viewerjumpto "Examples" "metan##examples"}{...}
{viewerjumpto "References" "metan##refs"}{...}
{title:Title}

{phang}
{hi:metan} {hline 2} Perform meta-analysis of aggregate (summary) data


{marker syntax}{...}
{title:Syntax}

{pstd}
{cmd:metan} has the following general syntax:

{p 8 18 2}
{cmd:metan} {varlist} {ifin} [{cmd:,} {it:{help metan##options:options}}]

{pstd}
where {varlist} may contain two, three, four or six numeric variables depending upon the structure of the data.


{pstd}
More specifically, {cmd:metan} may have any of the following syntaxes:

{pmore}
Meta-analysis of generic (pre-calculated) effect sizes and their standard errors, with option to supply participant numbers via {opt npts(varname)}.
Effect sizes must be based on a normal distribution; for example, log odds-ratios rather than odds ratios.

{pmore2}
{cmd:metan} {it:ES seES} {ifin} [{cmd:,} {opt npts(varname)} {it:{help metan_model:model_spec}} {it:{help metan##options_main:options_main}}]


{pmore}
Meta-analysis of generic (pre-calculated) effect sizes and their 95% confidence limits, with option to supply participant numbers via {opt npts(varname)}.
Confidence intervals are assumed to be symmetric, and the standard error is derived as {bf:(}{it:UCI} {bf:-} {it:LCI}{bf:) / 2*1.96}.
Hence, confidence limits must be based on a normal distribution, and have 95% level, or the pooled result will not be accurate.

{pmore2}
{cmd:metan} {it:ES lci uci} {ifin} [{cmd:,} {opt npts(varname)} {it:{help metan_model:model_spec}} {it:{help metan##options_main:options_main}}]


{pmore}
Meta-analysis of {help metan_binary:two-group comparison of binary outcomes},
using the number of events and non-events in the treatment and control groups

{pmore2}
{cmd:metan} {it:event_treat} {it:noevent_treat} {it:event_ctrl} {it:noevent_ctrl} {ifin}
[{cmd:,} {it:{help metan_model:model_spec}} {it:{help metan_binary##options:options_binary}} {it:{help metan##options_main:options_main}}]


{pmore}
Meta-analysis of {help metan_continuous:two-group comparison of continuous outcomes},
using the sample size, mean and standard deviation in the treatment and control groups

{pmore2}
{cmd:metan} {it:n_treat} {it:mean_treat} {it:sd_treat} {it:n_ctrl} {it:mean_ctrl} {it:sd_ctrl} {ifin}
[{cmd:,} {it:{help metan_model:model_spec}} {it:{help metan_continuous##options:options_continuous}} {it:{help metan##options_main:options_main}}]


{pmore}
Meta-analysis of {help metan_proportion:proportions in a single group}; note that option {opt proportion} {ul:must} be supplied in this case

{pmore2}
{cmd:metan} {it:n_events} {it:n_total} {ifin} , {opt pr:oportion}
[{it:{help metan_model:model_spec}} {it:{help metan_proportion##options:options_proportion}} {it:{help metan##options_main:options_main}}]


{marker model_spec}{...}
{pstd}
{it:{help metan_model:model_spec}} specifies method(s) for meta-analytic pooling,
including {help metan_model##options_test:test statistics} for the pooled effect,
and estimates and confidence intervals for {help metan_model##options_het:between-study heterogeneity parameters}.
If no {it:model_spec} is supplied, the default for two-group comparison of binary outcomes is {opt mh:aenszel};
otherwise the default is {opt common}.  The simplest alternative is {opt random}, specifying the DerSimonian-Laird random-effects model.

{pstd}
{it:{help metan_model:model_spec}} has various other options and syntaxes, which are explained {help metan_model:on a separate page}.
This incorporates all the functionalities of the previous version of {cmd:metan}, including the ability to present multiple results
(e.g. common-effect and random-effects) in the same table and/or forest plot.
 


{marker options}{...}
{synoptset 24 tabbed}{...}
{synopthdr :options}
{synoptline}
{synopt :{it:{help metan##options_main:options_main}}}general options, appropriate with any of the data input syntaxes for {cmd:metan} as described above{p_end}
{synopt :{it:{help metan_binary##options:options_binary}}}additional options specific to meta-analysis of two-group comparisons of binary outcomes{p_end}
{synopt :{it:{help metan_continuous##options:options_continuous}}}additional options specific to meta-analysis of two-group comparisons of continuous outcomes{p_end}
{synopt :{it:{help metan_proportion##options:options_proportion}}}additional options specific to meta-analysis of single-group proportion data{p_end}
{synopt :{it:{help metan_model:model_spec}}}specify a model or method for meta-analytic pooling; test statistics; heterogeneity parameters{p_end}
{synoptline}


{marker options_main}{...}
{synoptset 24 tabbed}{...}
{synopthdr :options_main}
{synoptline}
{syntab :Main}
{synopt :{opt lcol:s}{cmd:(}{it:study_id} [{it:varlist}]{cmd:)}}variable to be used to label studies (see also {opt lcols()} under "Forest plot and/or saved data"){p_end}
{synopt :{cmd:study(}{it:study_id} [{cmd:, {ul:m}issing}]{cmd:)}}alternative way to specify the variable used to label studies{p_end}
{synopt :{cmd:by(}{it:subgroup_id} [{cmd:, {ul:m}issing}]{cmd:)}}subgroup meta-analysis{p_end}
{synopt :{opt cumul:ative}}cumulative meta-analysis{p_end}
{synopt :{opt inf:luence}}investigate influence of each study in turn on the overall estimate{p_end}
{synopt :{opt altw:t}}display study weights from the standard (non-cumulative or influence) meta-analysis{p_end}

{syntab :Options}
{synopt :{opt ci:type(ci_type)}}method of constructing confidence intervals for reporting of individual studies ({ul:not} pooled results){p_end}
{synopt :{opt level(#)}}set confidence level for reporting confidence intervals ({ul:including} pooled results); default is {cmd:level(95)}{p_end}
{synopt :{opt coef}, {opt log}}display log effect sizes and confidence limits{p_end}
{synopt :{it:{help eform_option}}}display exponentiated (antilog) effect sizes and confidence limits{p_end}
{synopt :{opt eff:ect(string)}}title for "effect size" column in the output{p_end}
{synopt :{opt keepa:ll}}display all studies in the output, even those for which no effect could be estimated{p_end}
{synopt :{opt keepo:rder}}display "no effect" studies in the order in which they would otherwise appear (by default these are moved to the end){p_end}
{synopt :{opt nogr:aph}}suppress the forest plot{p_end}
{synopt :{opt notab:le}}suppress printing the table of effect sizes to screen; see also {opt summaryonly}{p_end}
{synopt :{opt nohet}}suppress all heterogeneity statistics{p_end}
{synopt :{opt nokeep:vars}}do not add {help metan##saved_results:new variables} to the dataset{p_end}
{synopt :{opt nors:ample}}do not even add new variable {bf:_rsample} recording which observations were used (cf. {help f_e:e(sample)}){p_end}
{synopt :{opt noov:erall} {opt nosu:bgroup} {opt nosec:sub}}suppress overall pooling, or pooling within subgroups{p_end}
{synopt :{opt ovwt sgwt}}override default choice of whether to display overall weights or subgroup weights{p_end}
{synopt :{cmd:sortby(}{it:varname}|{cmd:_n)}}ordering of studies in table and forest plot, and for cumulative meta-analysis{p_end}
{synopt :{opt wgt(varname)}}specify a variable containing user-defined weights{p_end}

{syntab :Forest plot and/or saved data}
{synopt :{opt effi:cacy}}additionally display odds ratios or risk ratios expressed in terms of vaccine efficacy{p_end}
{synopt :{opt hets:tat(het_spec)}}specify heterogeneity information to display on the forest plot{p_end}
{synopt :{cmd:extraline(yes|no)}}override the default placement of heterogeneity information in the forest plot{p_end}
{synopt :{opt rfdist}, {opt rflevel(#)}}display approximate predictive interval, with optional coverage level (default is 95%){p_end}
{synopt :{opt lcol:s(varlist)}, {opt rcol:s(varlist)}}display (and/or save) columns of additional data{p_end}
{synopt :{opt plotid(varname)}}define groups of observations in which to apply specific plot rendition options{p_end}
{synopt :{opt summaryonly}}show only summary estimates (diamonds) in the forest plot and on screen{p_end}
{synopt :{cmdab:sa:ving(}{it:{help metan##fplotopts:saving_option}}{cmd:)}}save data in the form of a "forestplot results set" to {it:filename}{p_end}
{synopt :{opt clear}}replace the data in memory with the "results set", instead of saving to a separate file{p_end}
{synopt :{opt nowarn:ing}}suppress the default display of a note warning that studies are weighted from random effects anaylses{p_end}

{synopt	:{cmd:{ul:forest}plot(}{help forestplot##options:{it:forestplot_options}}{cmd:)}}other options as described under {bf:{help forestplot}}{p_end}
{synoptline}



{marker description}{...}
{title:Description}

{pstd}
{cmd:metan} performs meta-analysis of aggregate data; that is, data in which each observation represents a summary of a larger study.
Aggregate data may consist of cell counts from a two-group comparison of binary outcomes; sample sizes, means and standard deviations
from a two-group comparison of continuous outcomes; single-group proportion data; or generic (pre-calculated) effect sizes and their standard errors
or 95% confidence limits.

{pstd}
Much of the basic theory of aggregate-data meta-analysis, as implemented in {cmd:metan}, is described in {help metan##refs:Deeks et al (2001)}.



{marker options}{...}
{title:Options}

{dlgtab:Main}

{phang}
{cmd:study(}{it:study_id} [{cmd:, missing}]{cmd:)} specifies the variable used for labelling the studies in all output,
which must be either integer-valued or string.

{pmore}
{opt missing} requests that missing values be treated as potential study identifiers; the default is to exclude them.

{pmore}
The variable used for labelling the studies may alternatively be specified as the first {it:varname} in {opt lcols(varlist)}
(see {help metan##fplotopts:Forest plot and/or saved data options}), in which case
{opt study()} is not necessary and {opt missing} may not be specified.

{pmore}
Alternatively, the older {cmd:metan} syntax {cmd:label(}[{cmd:namevar=}{it:namevar}]{cmd:,} [{cmd:yearvar=}{it:yearvar}]{cmd:)} may be used.

{pmore}
If none of these are supplied, studies will simply be labelled sequentially as "1", "2", etc.
In the absence of {ifin}, the entire dataset in memory will be included
except observations for which {it:varlist} is entirely {help missing:system missing}.

{phang}
{cmd:by(}{it:subgroup_id} [{cmd:, missing}]{cmd:)} specifies a variable identifying subgroups of studies (and must therefore be constant within studies),
which must be either integer-valued or string.

{pmore}
{opt missing} requests that missing values be treated as potential subgroup identifiers; the default is to exclude them.

{pmore}
If {it:subgroup_id} is a string variable, then subgroups are displayed in the order they appear among the included observations
(according to their requested ordering within the analysis; see {cmd:sortby()}).
If {it:subgroup_id} is numeric, then subgroups are displayed in numeric order.
Within each subgroup, studies appear in the order they would appear if the entire analysis were restricted to that subgroup.

{phang}
{opt cumulative} requests that the meta-analysis be performed cumulatively; that is, performed repeatedly with one study being added each time.
Studies will be added in the order specified by {cmd:sortby()} if present, or else in the order they appear in the dataset.
Pooled effect information (tests of {it:z} = 0, heterogeneity etc.) will be based on the model following the addition of the final study.

{phang}
{opt influence} requests that each study in turn is removed from the meta-analysis to investigate its influence on the overall result.
Pooled effect information remains identical to that if {opt influence} were not specified.

{pmore}
Note that for both {opt cumulative} and {opt influence}, use of random-effects and variance-correction models
may result in weights greater than 100%, since weights are expressed relative to the total weight in the model with all studies included.
Only a single pooling method may be used with {opt cumulative} and {opt influence}.

{phang2}
{opt altwt} does not alter the effect estimates, but presents the original weights (and participant numbers in the forest plot, if applicable)
corresponding to each individual study, rather than the relative weights of each fitted {opt cumulative} or {opt influence} model.
In other words, presented weights are as if {opt cumulative} or {opt influence} were not specified.


{dlgtab:Options}

{phang}
{opt citype(ci_type)} specifies how confidence limits for individual studies should be constructed for display purposes.
This option acts independently of both the data input {varlist} (i.e. data may be presented in a different -- though consistent --
manner to that in which it was supplied) and of how confidence limits for {ul:pooled} results are constructed
(which will depend upon {it:{help metan##model_spec:model_spec}}). See also {bf:{help metan##options_main:level(#)}}

{phang2}
{it:ci_type} is {opt normal} by default, specifying use of the normal distribution (i.e. a {it:z}-statistic).

{phang2}
{it:ci_type} may also be {opt t}, specifying use of the ("Student's") {it:t} distribution.
Degrees of freedom may be specified using the {opt df(varname)} option;
otherwise degrees of freedom of {it:n-2} are assumed, where {it:n} is the study sample size.

{phang2}
Additional {it:ci_type}s may also be specified with {help metan_binary:two-group binary comparisons} or {help metan_proportion:proportion data}.

{phang}
{it:{help eform_option}} specifies that effect sizes and confidence limits should be exponentiated in the table and forest plot.
The option also generates a heading for the effect size column, e.g. "Odds Ratio" if {it:eform_option} is {cmd:or}

{phang}
{opt coef} or {opt log} are synonyms, and report results on the log scale (valid for ratio statistics only, that is OR, RR, HR etc).
If both {it:eform_option} and {opt log} are supplied, {opt log} takes priority.

{phang}
{opt effect(string)} specifies a heading for the effect size column in the output.
This overrides any heading generated by {it:{help eform_option}}.

{phang}
{opt keepall}, {opt keeporder} request that all values of {it:study_id} should be visible in the table and forest plot,
even if no effect could be estimated (e.g. due to insufficient observations or missing data).
For such studies, "(Insufficient data)" will appear in place of effect estimates and weights.

{pmore}
{opt keeporder} requests such studies are displayed in their "natural" sort order.
By default, such studies are moved to the end.

{phang}
{opt nograph}, {opt notable} request the suppression of, respectively,
construction of the forest plot and the table of effect sizes.
Additionally, the forest plot option {opt summaryonly} has a similar effect to {opt notable} on the printed output.

{phang}
{opt nohet} suppresses heterogeneity statistics in both table and forest plot.

{phang}
{opt nokeepvars}, {opt norsample} specify that {help metan##saved_results:new variables}
should {ul:not} be added to the dataset upon conclusion of the routine.

{pmore}
{opt nokeepvars} suppresses the addition of effect statistics such as {bf:_ES}, {bf:_seES} and {bf:_NN} but retains {bf:_rsample}.
If appropriate, effect statistics are instead returned in the matrix {bf:r(coeffs)}.

{pmore}
{opt norsample} further suppresses the addition of the variable {bf:_rsample},
an analogue of {help f_e:e(sample)} which identifies which observations were included in the analysis.
Therefore, {opt norsample} requests that the data in memory not be changed {ul:in any way}
upon conclusion of the routine.

{phang}
{opt nooverall}, {opt nosubgroup} and {opt nosecsub} affect which groups of data are pooled, thus affecting both the table of effect sizes
and the forest plot (if applicable).

{pmore}
{opt nooverall} suppresses the overall pooled effect, so that (for instance) subgroups are considered entirely
independently. Between-subgroup heterogeneity statistics are also suppressed.

{pmore}
{opt nosubgroup} suppresses the within-subgroup pooled effects, so that subgroups are displayed
separately but with a single overall pooled effect with associated heterogeneity statistics.

{pmore}
{opt nosecsub} prevents the display of subgroup effect estimates using the second or further methods, if applicable.

{phang}
{opt ovwt}, {opt sgwt} override the default choice of whether to display overall weights or within-subgroup weights
in the screen output and forest plot. Note that this makes no difference to calculations of pooled effect estimates,
as weights are normalised anyway.

{phang}
{opt sortby(varname)} allows user-specified ordering of studies in the table and forest plot, without altering the data in memory.
See also {opt cumulative}.

{phang}
{opt wgt(varname)} specifies user-defined weighting for any data type. You should only use this option if you are satisfied that the weights are meaningful.

{pmore}
Regardless of whether a fixed- or random-effects model is specified, pooled effects are calculated as:

{pmore2}
{it:theta} = {cmd:sum(}{it:w_i y_i}{cmd:)} / {cmd:sum(}{it:w_i}{cmd:)}

{pmore}
For a fixed-effect model, pooled effect variances are calculated as:

{pmore2}
{cmd:Var(}{it:theta}{cmd:)} = {cmd:sum(}{it:w_i}^2 {it:v_i}{cmd:)} / {cmd:sum(}{it:w_i}{cmd:)}^2

{pmore}
and for a random-effects model:

{pmore2}
{cmd:Var(}{it:theta}{cmd:)} = {cmd:sum(}{it:w_i}^2 ({it:v_i} + {it:tau}^2){cmd:)} / {cmd:sum(}{it:w_i}{cmd:)}^2

{pmore}
where {it:v_i} are the individual study variances and {it:w_i} are the user-defined weights.

{pmore}
Note that the scale of user-defined weights is immaterial, since individual weights are normalised.
Hence, once run, an analysis may be recreated using the option {cmd:wgt(_WT)}.
The raw (non-normalised) numbers stored in {it:varname} may also be saved and/or displayed using the forest plot options {opt lcols()} or {opt rcols()}.


{marker fplotopts}{...}
{dlgtab:Forest plot and/or saved data}

{phang}
{opt efficacy} expresses results as the vaccine efficacy (the proportion of cases that would have been prevented
in the placebo group that would have been prevented had they received the vaccination).
Only available with odds ratios (OR) or risk ratios (RR).

{phang}
{opt hetstat(het_spec)} alters the default heterogeneity information displayed on the forest plot. {it:het_spec} can be either, or both, of:

{pmore}
{opt p:value} requests that the p-value associated with the relevant heterogeneity test be displayed.

{pmore}
{opt q} or {opt isq} changes the heterogeneity statistic that appears on the forest plot.
For inverse-variance models, the default is I-squared which can optionally be changed to Cochran's Q statistic.
Otherwise, the default is the Cochran, Mantel-Haenszel or Peto Q statistic (as appropriate), from which I-squared can optionally be derived
as ({it:Q} - {it:Qdf})/{it:Q}.

{phang}
{cmd:extraline(yes|no)} affects the placement of the heterogeneity information (see {opt hetstat()}) within the plot.
By default, heterogeneity information is displayed in brackets following the description of the pooled effect
(e.g. "Overall").  However, if columns of data are to be displayed on the left-hand side (see e.g. {opt lcols()})
which would cause text to be overwritten, then the heterogeneity information is moved to a new line immediately below.
{cmd:extraline(yes)} forces a new line to be used when it would otherwise not be;
{cmd:extraline(no)} forces a new line {ul:not} to be used when it would otherwise would.

{phang}
{opt rfdist} displays the confidence interval of the approximate predictive distribution of a future trial, based on the extent of heterogeneity.
This incorporates uncertainty in the location and spread of the random effects distribution
using the formula {bf:t * sqrt(}{it:SE}^2 {bf:+} {it:tau}^2{bf:)}, where {bf:t} is the critical value from the Student's {it:t} distribution with {it:k}-2 degrees of freedom,
{it:SE}^2 is the squared standard error and {it:tau}^2 is the heterogeneity statistic.
The CI is then displayed with lines extending from the diamond.
Note that with <3 studies the distribution is inestimable and hence not displayed (this behaviour differs from that in {cmd:metan});
and where heterogeneity is zero there is still a slight extension as the t-statistic is always greater than the corresponding normal deviate.
For further information see {help metan##refs:Higgins and Thompson (2009)}.

{pmore}
{opt rflevel(#)} specifies the coverage (e.g. 95 percent) for the confidence interval of the predictive distribution.
Default is {help creturn##output:c_level}.  See {help set level}.

{phang}
{opt lcols(varlist)}, {opt rcols(varlist)} define columns of additional data to the left or right of the graph.
By default, the first two columns on the right contain the effect size and weight. If {opt counts} is used this will be set as the third column.
Columns are titled with the variable label, or the variable name if a label is not defined.

{pmore}
Note: the first variable specified in {opt lcols()} is assumed to be the study identifier if not otherwise specified.

{phang}
{opt summaryonly} shows only summary estimates in the graph.
This may be of use for multiple subgroup analyses; see also {opt stacklabel}.

{phang}
{cmd:saving(}{it:{help filename}} [{cmd:, replace} {cmd:stacklabel}]{cmd:)} saves the forestplot "results set" created by
{cmd:ipdmetan} in a Stata data file for further use or manipulation; see {help metan##saved_datasets:saved datasets}.

{pmore}
{opt replace} overwrites {it:filename}

{pmore}
{opt stacklabel} takes the {help label:variable label} of the left-most column variable (usually {it:study_id}),
which would usually appear outside the plot region as the column heading, and instead stores it in the first observation of {bf:_LABELS}.
This allows multiple such datasets to be {bf:{help append}}ed without this information being lost.

{phang}
{opt clear} is an alternative to {cmd:saving()} which replaces the data in memory with the "results set" data.



{marker saved_results}{...}
{title:Saved results}

{pstd}
By default, {cmd:metan} adds new variables to the dataset corresponding to the individual study effect sizes,
standard errors, confidence intervals and weights used by the program.
Amongst other things, this provides a method of obtaining effect sizes and standard errors from other data structures such as 2x2 cell counts.
These new variables may be suppressed using the {opt nokeepvars} or {opt norsample} options.

{pstd}
(Note that, if {opt cumulative} or {opt influence}, these variables will still contain 
individual study characteristics, which will therefore {ul:not} agree with the results table shown on-screen,
except for the first iteration of a {opt cumulative} analysis.)

{pstd}
The following new variables may be added:

{p2col 7 32 36 2:{cmd:_ES}}Effect size (ES) on the interval scale (e.g. log odds ratio, or transformed proportion){p_end}
{p2col 7 32 36 2:{cmd:_seES}}Standard error of ES{p_end}
{p2col 7 32 36 2:{cmd:_LCI}}Lower confidence limit for ES{p_end}
{p2col 7 32 36 2:{cmd:_UCI}}Upper confidence limit for ES{p_end}
{p2col 7 32 36 2:{cmd:_WT}}Study percentage weight (between 0 and 100){p_end}
{p2col 7 32 36 2:{cmd:_NN}}Study sample size{p_end}
{p2col 7 32 36 2:{cmd:_CC}}Marker of whether continuity correction was applied{p_end}
{p2col 7 32 36 2:{cmd:_rsample}}Marker of which observations were used in the analysis{p_end}


{pstd}{cmd:metan} also saves the following in {cmd:r()}:{p_end}
{pstd}(with some variation, and in addition to any scalars saved by {bf:{help forestplot}}){p_end}

{synoptset 25 tabbed}{...}
{p2col 5 20 24 2: Scalars}{p_end}
{synopt:{cmd:r(k)}}Number of included studies {it:k}{p_end}
{synopt:{cmd:r(n)}}Number of included participants{p_end}
{synopt:{cmd:r(eff)}}Overall pooled effect size{p_end}
{synopt:{cmd:r(se_eff)}}Standard error of overall pooled effect size{p_end}
{synopt:{cmd:r(Q)}}Q statistic of heterogeneity (with degrees of freedom {it:k-1}){p_end}

{synoptset 25 tabbed}{...}
{p2col 5 25 29 2: Macros}{p_end}
{synopt:{cmd:r(measure)}}Name of effect measure{p_end}
{synopt:{cmd:r(citype)}}Method of constructing study-level confidence intervals{p_end}
{synopt:{cmd:r(model)}}Pooling method(s) used (e.g. Mantel-Haenszel, fixed-effect, DerSimonian-Laird){p_end}
{synopt:{cmd:r(model}{it:#}{cmd:opts)}}Options relating to pooling method(s), if applicable{p_end}

{synoptset 25 tabbed}{...}
{p2col 5 25 29 2: Matrices}{p_end}
{synopt:{cmd:r(ovstats)}}Matrix of overall effects, test statistics, heterogeneity statistics, p-values etc.{p_end}
{synopt:{cmd:r(bystats)}}Matrix of effects, heterogeneity statistics etc. by subgroup, for a single pooling method{p_end}
{synopt:{cmd:r(bystats}{it:#}{cmd:)}}Matrices of subgroup statistics for multiple pooling methods, if applicable{p_end}


{pstd}
The following results may also be saved, depending on the combination of effect measure and model:

{p2col 5 20 24 2: Scalars (specific effect measures only)}{p_end}
{synopt:{cmd:r(OR)}, {cmd:r(RR)}}Mantel-Haenszel estimates of Odds Ratio or Risk Ratio (if appropriate){p_end}
{synopt:{cmd:r(chi2)}}Chi-squared test statistic (if requested){p_end}
{synopt:{cmd:r(OE)}, {cmd:r(V)}}Overall pooled {it:OE} and {it:V} statistics (if appropriate){p_end}
{synopt:{cmd:r(cger)}, {cmd:r(tger)}}Average event rate in control and treatment groups{p_end}

{p2col 5 20 24 2: Scalars (inverse-variance models only)}{p_end}
{synopt:{cmd:r(tausq)}}Between-study variance tau-squared{p_end}
{synopt:{cmd:r(sigmasq)}}Average within-study variance{p_end}
{synopt:{cmd:r(Isq)}}Heterogeneity measure I-squared{p_end}
{synopt:{cmd:r(HsqM)}}Heterogeneity measure H-squared (Mittlb{c o:}ck modification){p_end}
{synopt:{cmd:r(Qr)}}"Generalised" Q, i.e. Cochran's Q calculated using random-effects weights and pooled estimate{p_end}

{synoptset 25 tabbed}{...}
{p2col 5 25 29 2: Scalars (iterative random-effects models only; see {help mf_mm_root} for interpretations of convergence success values)}{p_end}
{synopt:{cmd:r(tsq_var)}}Estimated variance of tau-squared{p_end}
{synopt:{cmd:r(tsq_lci)}}Lower confidence limit for tau-squared{p_end}
{synopt:{cmd:r(tsq_uci)}}Upper confidence limit for tau-squared{p_end}
{synopt:{cmd:r(rc_tausq)}}Whether tau-squared point estimate converged successfully{p_end}
{synopt:{cmd:r(rc_tsq_lci)}}Whether tau-squared lower confidence limit converged successfully{p_end}
{synopt:{cmd:r(rc_tsq_uci)}}Whether tau-squared upper confidence limit converged successfully{p_end}
{synopt:{cmd:r(rc_eff_lci)}}Whether effect estimate lower confidence limit converged successfully{p_end}
{synopt:{cmd:r(rc_eff_uci)}}Whether effect estimate upper confidence limit converged successfully{p_end}


{marker saved_datasets}{...}
{title:Saved datasets}

{pstd}
In order to construct a forest plot, {cmd:metan} manipulates the data originally in memory into a format that {cmd:forestplot} understands.
This "forestplot results set" can be saved to a Stata data file using the {opt saving()} or {opt clear} options, allowing the user to further manipulate it and
hence create highly customised forest plots.

{pstd}
The structure of these "results sets" is such that each row of data will appear in the plot, in the same order (top to bottom).
Variable labels will appear above columns of data within the {help region_options:plot region}; value labels and formats (including string justification)
are honoured where possible. See {bf:{help forestplot}} for further details of how such data is interpreted, and for additional options.

{pstd}
Variables specified in {opt lcols()} or {opt rcols()} will have their variable names, labels and formats preserved within "results sets".
Otherwise, variables are given standardised names, as follows:

{p2col 5 20 24 2: Core variables}{p_end}
{synopt:{cmd:_USE}}Indicates the type of content in each observation (e.g. study effect, pooled effect); see {bf:{help forestplot}}{p_end}
{synopt:{cmd:_STUDY}}Value-labelled numeric variable identifying the studies{p_end}
{synopt:{cmd:_LABELS}}String containing general information to be displayed on the left-hand side of the forestplot, including study names{p_end}
{synopt:{cmd:_ES}}Effect size (ES) on the interval scale (see {help metan##saved_results:saved results}){p_end}
{synopt:{cmd:_seES}}Standard error of ES (see {help metan##saved_results:saved results}){p_end}
{synopt:{cmd:_LCI}}Lower confidence limit for ES (see {help metan##saved_results:saved results}){p_end}
{synopt:{cmd:_UCI}}Upper confidence limit for ES (see {help metan##saved_results:saved results}){p_end}
{synopt:{cmd:_NN}}Study sample size (see {help metan##saved_results:saved results}){p_end}
{synopt:{cmd:_WT}}Study percentage weight (see {help metan##saved_results:saved results}){p_end}
{synopt:{cmd:_EFFECT}}String containing the effect size and confidence limits together, on the display scale (i.e. exponentiated if specified){p_end}

{p2col 5 20 24 2: Option-dependent variables}{p_end}
{synopt:{cmd:_BY}}Value-labelled numeric variable identifying study subgroups (see {opt by()} option){p_end}
{synopt:{cmd:_CC}}Marker of whether continuity correction was applied (see {opt cc} option){p_end}
{synopt:{cmd:_counts1}}String containing "events/total" numbers in the research arm (see {opt counts} option){p_end}
{synopt:{cmd:_counts0}}String containing "events/total" numbers in the control arm (see {opt counts} option){p_end}
{synopt:{cmd:_counts1msd}}String containing "mean (SD)" in the research arm (see {opt counts} option){p_end}
{synopt:{cmd:_counts0msd}}String containing "mean (SD)" in the control arm (see {opt counts} option){p_end}
{synopt:{cmd:_OE}}Logrank {it:O-E}{p_end}
{synopt:{cmd:_V}}Logrank {it:V}{p_end}
{synopt:{cmd:_VE}}String containing vaccine efficacy and confidence limits (see {opt efficacy} option){p_end}
{synopt:{cmd:_rfLCI}}Lower confidence limit of approximate predictive distribution (see {opt rfdist} option){p_end}
{synopt:{cmd:_rfUCI}}Upper confidence limit of approximate predictive distribution (see {opt rfdist} option){p_end}
{synopt:{cmd:_Prop_ES}}Proportion (see {opt proportion} option){p_end}
{synopt:{cmd:_Prop_LCI}}Lower confidence limit of proportion (see {opt proportion} option){p_end}
{synopt:{cmd:_Prop_UCI}}Upper confidence limit of proportion (see {opt proportion} option){p_end}

{pstd}
Some of these variables have associated characteristics; type {bf:{help char:char list}} to see these.


{marker diffs_metan}{...}
{title:Note: Differences from previous versions of {cmd:metan}}

{pstd}
This latest version of {cmd:metan} is designed to run under Stata version 11 and upwards,
with the exception of the {help metan_model##model_name:Biggerstaff-Tweedie} and {help metan_model##model_name:Henmi-Copas} models
which require an additional user-contributed Mata function written for Stata version 12.
The previous version of {bf:metan}, v3.04 written for Stata v9 ({help metan##refs:Harris et al 2008}),
remains available within this package under the name {help metan9:metan9}.
A still older version, v1.86 written for Stata v7 ({help metan##refs:Bradburn et al 1998}),
also remains available under the name {help metan7:metan7}.

{pstd}
This version of {cmd:metan} has been designed with consistency and backwards-compatibility in mind.
However, there are some differences in syntax and operation from previous versions.  In particular:

{phang2}
Most options specific to the forest plot (i.e. those that do not affect the results appearing in the Results Window)
now need to be placed within the {opt forestplot()} option rather than directly to {cmd:metan}. This includes {opt nostats} and {opt nowt}.
Also, the forest plot option {opt double} is not currently implemented.

{phang2}
{opt nooverall} no longer automatically enforces {opt nowt}.

{phang2}
Prediction intervals ({opt rfdist}) are no longer displayed with dotted lines if the number of studies is less than three;
instead, the interval is simply not displayed at all. A message is printed in the Results Window explaining this.

{phang2}
If a random-effects model or {opt wgt()} is specified,
the displayed Q statistic will still be based on the inverse-variance common-effect model.
However, if {it:{help metan_model##model_name:model_name}} is {cmd:mhaenzsel} or {cmd:peto}, the displayed Q statistic
will be based on the pooled effect and weights from the specified model.

{phang2}
If {it:{help metan_model##model_name:model_name}} is {cmd:mhaenzsel}, continuity correction is not necessary for pooling and will not be applied.
However, for display purposes only, continuity correction {ul:will} be applied to the individual study estimates.

{phang2}
Note that the first listed syntax for {it:{help metan_model##model_spec:model_spec}} reflects the previous syntax of {cmd:metan} (see {help metan9:metan9}),
under which {it:{help metan_model##options_test:options_test}} and {it:{help metan_model##options_het:options_het}} were supplied as main options.
However, in fact {it:{help metan_model##options_test:options_test}} and {it:{help metan_model##options_het:options_het}} may also be supplied
as main options with the {ul:second} listed syntax for {it:{help metan_model##model_spec:model_spec}}.
In that case, such options will act "globally", but may be over-ridden by model-specific options.
In particular, if a "global" option is invalid for a particular model,
a notification will be printed to screen, and the option will revert to the model-specific default value.


{marker examples}{...}
{title:Examples}

{pstd}
All examples but the last are taken directly from the previous version of {cmd:metan}, and use a simulated example dataset (Ross Harris 2006)

{pmore}
{stata "use http://fmwww.bc.edu/repec/bocode/m/metan_example_data, clear":. use http://fmwww.bc.edu/repec/bocode/m/metan_example_data, clear}


{pstd}
Risk difference from {help metan_binary:raw cell counts}, random effects model, "label" specification with counts displayed

{cmd}{...}
{* example_start - metan_ex1}{...}
{pmore}
. metan tdeath tnodeath cdeath cnodeath,{* ///}{p_end}
{p 16 20 2}
rd random label(namevar=id, yearvar=year) counts{p_end}
{* example_end}{...}
{txt}{...}
{pmore}
{it:({stata metan_hlp_run metan_ex1 using metan.sthlp, restpres:click to run})}{p_end}


{pstd}
Same again, but now with {help metan_model:three different pooling methods}: Mantel-Haenszel common-effect,
DerSimonian-Laird random-effects, and REML random-effects with Hartung-Knapp-Sidik-Jonkman variance correction.
We also request the test-based confidence interval for {it:H} proposed by {help metan_model##refs:Higgins and Thompson (2002)}

{cmd}{...}
{* example_start - metan_ex1a}{...}
{pmore}
. metan tdeath tnodeath cdeath cnodeath,{* ///}{p_end}
{p 16 20 2}
rd label(namevar=id, yearvar=year) counts{* ///}{p_end}
{p 16 20 2}
. model(mh \ dl \ reml, hksj) higgins{p_end}
{* example_end}{...}
{txt}{...}
{pmore}
{it:({stata metan_hlp_run metan_ex1a using metan.sthlp, restpres:click to run})}{p_end}


{pstd}
Sort by year, use data columns syntax with all column data left-justified. Specify percentage of graph as text;
 suppress stats, weight, heterogeneity stats and table.

{cmd}{...}
{* example_start - metan_ex2}{...}
{phang2}
. metan tdeath tnodeath cdeath cnodeath, notable{* ///}{p_end}
{p 16 20 2}
sortby(year) lcols(id year country) rcols(population){* ///}{p_end}
{p 16 20 2}
forestplot(astext(60) nostats nowt nohet leftjustify){p_end}
{* example_end}{...}
{txt}{...}
{pmore}
{it:({stata metan_hlp_run metan_ex2 using metan.sthlp, restpres:click to run})}{p_end}


{pstd}
Analyse {help metan_continuous:continuous data (six-parameter syntax)}, stratify by type of study, with weights summing to 100% within sub group,
display random-effects predictive distribution, show raw data counts, display "favours treatment vs. favours control" labels

{cmd}{...}
{* example_start - metan_ex3}{...}
{phang2}
. metan tsample tmean tsd csample cmean csd,{* ///}{p_end}
{p 16 20 2}
study(id) by(type_study) sgwt random rfdist counts{* ///}{p_end}
{p 16 20 2}
forestplot(favours(Treatment reduces blood pressure # Treatment increases blood pressure)){p_end}
{* example_end}{...}
{txt}{...}
{pmore}
{it:({stata metan_hlp_run metan_ex3 using metan.sthlp, restpres:click to run})}{p_end}


{pstd}
Use {cmd:metan} to generate log odds ratio and standard error from the {help metan_binary:raw cell counts},
then analyse with two-parameter syntax. Graph has exponential form,
scale is forced within set limits and ticks added, effect label specified.

{cmd}{...}
{* example_start - metan_ex4}{...}
{phang2}
. quietly metan tdeath tnodeath cdeath cnodeath, or nograph{p_end}
{phang2}
. rename _ES logor{p_end}
{phang2}
. rename _seES selogor{p_end}
{phang2}
. metan logor selogor, or {* ///}{p_end}
{p 16 20 2}
forestplot(xlabel(0.5 1 1.5 2 2.5, force) xtick(0.75 1.25 1.75 2.25)){p_end}
{* example_end}{...}
{txt}{...}
{pmore}
{it:({stata metan_hlp_run metan_ex4 using metan.sthlp, restpres:click to run})}{p_end}


{pstd}
Analyse the number of deaths in the control arm as {help metan_proportion:proportion data},
using the {help metan_proportion##refs:Freeman-Tukey double-arcsine transformation}.
The null line is removed, and the x-axis range is forced to be from zero to one.

{cmd}{...}
{* example_start - metan_ex5}{...}
{phang2}
. metan cdeath csample, proportion transform(ftukey) lcols(id) forestplot(nonull range(0 1)){p_end}
{* example_end}{...}
{txt}{...}
{pmore}
{it:({stata metan_hlp_run metan_ex5 using metan.sthlp, restpres:click to run})}{p_end}


{pstd}
Display diagnostic test data with three-parameter syntax. Weight is number of positive diagnoses, axis label set
and null specified at 50%. Overall effect estimate is not displayed, graph for visual examination only.

{cmd}{...}
{* example_start - metan_ex6}{...}
{phang2}
. metan percent lowerci upperci, wgt(n_positives) study(id) nooverall notable {* ///}{p_end}
{p 16 20 2}
forestplot( xlabel(0(10)100, force) null(50) title(Sensitivity, position(6)) ){p_end}
{* example_end}{...}
{txt}{...}
{pmore}
{it:({stata metan_hlp_run metan_ex6 using metan.sthlp, restpres:click to run})}{p_end}


{pstd}
User has analysed data with a non-standard technique and supplied effect estimates, weights and description of statistics.
The scheme "Economist" has been used.

{cmd}{...}
{* example_start - metan_ex7}{...}
{phang2}
. metan OR ORlci ORuci, label(namevar=id) wgt(bweight){* ///}{p_end}
{p 16 20 2}
first(0.924 0.753 1.095 Bayesian) firststats(param V=3.86, p=0.012){* ///}{p_end}
{p 16 20 2}
forestplot(xlabel(0.25 0.5 1 2 4, force) null(1) scheme(economist)){p_end}
{* example_end}{...}
{txt}{...}
{pmore}
{it:({stata metan_hlp_run metan_ex7 using metan.sthlp, restpres:click to run})}{p_end}


{pstd}
Same example again, but this time instead of using {opt first()} or {opt firststats()},
we save and load a "forest plot results set", which is then edited to contain our user-defined estimates.
Finally, the forest plot is generated, and appears identical to that in the previous example.
This demonstrates some of the flexibility of being able to edit (and otherwise manipulate) forest plot results sets.

{pstd}
Note: the option {opt useopts} ensures that no information is lost between the initial call to {cmd:metan}
and the final call to {cmd:forestplot}. For further details, see {bf:{help forestplot}}.

{cmd}{...}
{* example_start - metan_ex8}{...}
{phang2}
. tempfile myfile{p_end}
{phang2}
. quietly metan OR ORlci ORuci, wgt(bweight) study(id) nograph saving(`myfile'){p_end}
{phang2}
. preserve{p_end}
{phang2}
. use `myfile', clear{p_end}
{phang2}
. replace _ES  = 0.924 if _USE == 5{p_end}
{phang2}
. replace _LCI = 0.753 if _USE == 5{p_end}
{phang2}
. replace _UCI = 1.095 if _USE == 5{p_end}
{phang2}
. replace _LABELS = "Overall, Bayesian (param V=3.86, p=0.012)" if _USE == 5{p_end}
{phang2}
. forestplot, useopts xlabel(0.25 0.5 1 2 4, force) null(1) scheme(economist){p_end}
{phang2}
. restore{p_end}
{* example_end}{...}
{txt}{...}
{pmore}
{it:({stata metan_hlp_run metan_ex8 using metan.sthlp, restpres:click to run})}{p_end}


{pstd}
Variable "counts" defined showing raw data. Options to change the box, effect estimate marker and confidence interval are used,
and the counts variable has been attached to the estimate marker as a label.

{cmd}{...}
{* example_start - metan_ex9}{...}
{phang2}
. gen counts = ". " + string(tdeath) + "/" + string(tdeath+tnodeath){* ///}{p_end}
{p 16 20 2}
+ ", " + string(cdeath) + "/" + string(cdeath+cnodeath){p_end}
{phang2}
. metan tdeath tnodeath cdeath cnodeath, lcols(id year) notable{* ///}{p_end}
{p 16 20 2}
forestplot(range(.3 3) boxopt( mcolor(forest_green) msymbol(triangle)){* ///}{p_end}
{p 16 20 2}
pointopt( msymbol(triangle) mcolor(gold) msize(tiny){* ///}{...}
mlabel(counts) mlabsize(vsmall) mlabcolor(forest_green) mlabposition(1)){* ///}{p_end}
{p 16 20 2}
ciopt( lcolor(sienna) lwidth(medium))){p_end}
{* example_end}{...}
{txt}{...}
{pmore}
{it:({stata metan_hlp_run metan_ex9 using metan.sthlp, restpres:click to run})}{p_end}



{title:Authors}

{pstd}
Original authors:
Michael J Bradburn, Jonathan J Deeks, Douglas G Altman.
Centre for Statistics in Medicine, University of Oxford, Oxford, UK

{pstd}
{cmd:metan} v3.04 for Stata v9:
Ross J Harris, Roger M Harbord, Jonathan A C Sterne.
Department of Social Medicine, University of Bristol, Bristol, UK

{pstd}
Current version, {cmd:metan} v4.00:
David Fisher, MRC Clinical Trials Unit at UCL, London, UK.

{pstd}
Email {browse "mailto:d.fisher@ucl.ac.uk":d.fisher@ucl.ac.uk}



{title:Acknowledgments}

{pstd}
Thanks to Patrick Royston (MRC Clinical Trials Unit at UCL, London, UK) for suggestions of improvements to the code and help file.

{pstd}
Thanks to Vince Wiggins, Kit Baum and Jeff Pitblado of Statacorp who offered advice and helped facilitate the version 9 update.

{pstd}
Thanks to Julian Higgins and Jonathan A C Sterne (University of Bristol, Bristol, UK) who offered advice and helped facilitate this latest update.

{pstd}
The "click to run" element of the examples in this document is handled using an idea originally developed by Robert Picard.



{marker refs}{...}
{title:References}

{phang}
Bradburn MJ, Deeks JJ, Altman DG. 1998.
metan — an alternative meta-analysis command.
Stata Technical Bulletin 44: sbe24 (pp 4-15)

{phang}
Harris RJ, Bradburn MJ, Deeks JJ, Harbord RM, Altman DG, Sterne JAC. 2008.
metan: fixed- and random-effects meta-analysis.
Stata Journal 8: 3-28

{phang}
Deeks JJ, Altman DG, Bradburn MJ. 2001.
Statistical methods for examining heterogeneity and combining results from several studies in meta-analysis.
In Systematic Reviews in Health Care: Meta-analysis in Context, ed. Egger M, Davey Smith G, Altman DG, 2nd ed., 285-312. London: BMJ Books.

{phang}
Higgins JPT, Thompson SG, Spiegelhalter DJ. 2009.
A re-evaluation of random-effects meta-analysis.
JRSS Series A 172: 137-159

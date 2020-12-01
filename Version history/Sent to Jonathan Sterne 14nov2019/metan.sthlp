{smcl}
{* *! version 4.0  David Fisher  14nov2019}{...}
{vieweralsosee "forestplot" "help forestplot"}{...}
{vieweralsosee "metani" "help metani"}{...}
{vieweralsosee "ipdmetan" "help ipdmetan"}{...}
{vieweralsosee "ipdover" "help ipdover"}{...}
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
{cmd:metan} {hline 2} Perform meta-analysis of aggregate (summary) data


{marker syntax}{...}
{title:Syntax}

{pstd}
{cmd:metan} has the following general syntax:

{p 8 18 2}
{cmd:metan} {varlist} {ifin} [{cmd:,} {it:options}]

{pstd}
where {varlist} may contain two, three, four or six numeric variables depending upon the structure of the data.
More specifically, {cmd:metan} may have any of the following syntaxes:


{pstd}
Meta-analysis of generic (pre-calculated) effect sizes and their standard errors, with option to supply participant numbers via {opt npts(varname)}

{p 8 18 2}
{cmd:metan} {it:ES seES} {ifin} [{cmd:,} {opt npts(varname)} {it:{help metan##model_spec:model_spec}} {it:{help metan##options_main:options_main}}]

{pstd}
Meta-analysis of generic (pre-calculated) effect sizes and their 95% confidence limits, with option to supply participant numbers via {opt npts(varname)}

{p 8 18 2}
{cmd:metan} {it:ES lci uci} {ifin} [{cmd:,} {opt npts(varname)} {it:{help metan##model_spec:model_spec}} {it:{help metan##options_main:options_main}}]

{pstd}
Meta-analysis of two-group comparison of binary outcomes,
using the number of events and non-events in the treatment and control groups

{p 8 18 2}
{cmd:metan} {it:event_treat} {it:noevent_treat} {it:event_ctrl} {it:noevent_ctrl} {ifin}
[{cmd:,} {it:{help metan##model_spec:model_spec}} {it:{help metan##options_binary:options_binary}} {it:{help metan##options_main:options_main}}]

{pstd}
Meta-analysis of two-group comparison of continuous outcomes,
using the sample size, mean and standard deviation in the treatment and control groups

{p 8 18 2}
{cmd:metan} {it:n_treat} {it:mean_treat} {it:sd_treat} {it:n_ctrl} {it:mean_ctrl} {it:sd_ctrl} {ifin}
[{cmd:,} {it:{help metan##model_spec:model_spec}} {it:{help metan##options_continuous:options_continuous}} {it:{help metan##options_main:options_main}}]


{marker model_spec}{...}
{pstd}
where either {it:model_spec} is

{p 8 18 2}
[ {it:model_name} ] [ {cmd:second(}{it:model_name}{cmd:)} ] [ {it:{help metan##options_user:options_user}}
{it:{help metan##options_test:options_test}} {it:{help metan##options_het:options_het}} ]

{pstd}
and {it:model_name} is {opt mh:aenszel} | {opt peto} | {opt common} | {opt random}

{pstd}
or {it:model_spec} is

{p 8 18 2}
{cmd:model(} {it:model} [ {cmd:\} {it:model} [ {cmd:\} {it:model} [...]]] {cmd:)}

{pstd}
and {it:model} is

{p 8 18 2}
{it:{help metan##model_name:model_name}} [ {cmd:,} {it:{help metan##options_model:options_model}}
{it:{help metan##options_test:options_test}} {it:{help metan##options_het:options_het}} ]


{pstd}
{it:model_spec} specifies method(s) for meta-analytic pooling.
If no {it:model_spec} is supplied, the default for two-group comparison of binary outcomes is {opt mh:aenszel};
otherwise the default is {opt common}.

{pstd}
The first syntax for {it:model_spec} directly maintains the previous syntax of {cmd:metan} v3.04 (for Stata v9).
In particular, note that the synonyms {opt fixed} for {opt mhaenszel} and {opt fixedi} for {opt common} still work.

{pstd}
The second syntax for {it:model_spec} is more general, and allows the results of multiple different pooling methods (or variations of methods) to be displayed simultaneously.
Such methods include alternative estimators of the between-study heterogeneity (tau-squared),
methods which correct {it:post hoc} the variance of the pooled estimate, or those which use alternative weighting systems
in order to improve statistical performance in the suspected presence of publication bias; see {it:{help metan##model_name:model_name}}.


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
{synopt :{opt nokeepv:ars}}do not add {help metan##saved_results:new variables} to the dataset{p_end}
{synopt :{opt nors:ample}}do not even add new variable {bf:_rsample} recording which observations were used (cf. {help f_e:e(sample)}){p_end}
{synopt :{opt noov:erall} {opt nosu:bgroup} {opt nosec:sub}}suppress overall pooling, or pooling within subgroups{p_end}
{synopt :{opt ovwt sgwt}}override default choice of whether to display overall weights or subgroup weights{p_end}
{synopt :{cmd:sortby(}{it:varname}|{cmd:_n)}}ordering of studies in table and forest plot{p_end}
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

{marker options_binary}{...}
{synopthdr :options_binary}
{synoptline}
{syntab :Options}
{synopt :{opt rd}}pool risk differences (see also {it:{help eform_option}}){p_end}
{synopt :{cmd:cc(}[{it:#}] [{cmd:,} {it:{help metan##cc_method:cc_method}}]{cmd:)}}use continuity correction value other than 0.5 for zero cells;
implement alternative methods of {help metan##refs:Sweeting (2004)}{p_end}
{synopt :{opt nocc}}suppress continuity correction entirely{p_end}
{synopt :{opt cor:nfield}}compute confidence intervals for odds ratios by method of Cornfield{p_end}
{synopt :{opt noint:eger}}allow cell counts to be non-integers{p_end}

{syntab :Forest plot and/or saved data}
{synopt :{opt co:unts}}display data counts ({it:n}/{it:N}) for treatment and control group{p_end}
{synopt :{opt group1(string)}, {opt group2(string)}}specify title text for the two columns created by {opt counts}{p_end}
{synopt :{opt npts}}display participant numbers in the forest plot{p_end}
{synopt :{opt oev}}display columns containing {it:O_E} and {it:V}{p_end}
{synoptline}

{marker options_continuous}{...}
{synopthdr :options_continuous}
{synoptline}
{syntab :Options}
{synopt :{opt coh:en}}pool standardised mean differences (SMDs) by the method of Cohen (default){p_end}
{synopt :{opt hed:ges}}pool SMDs by the method of Hedges{p_end}
{synopt :{opt gla:ss}}pool SMDs by the method of Glass{p_end}
{synopt :{opt md}, {opt wmd}, {opt nostan:dard}}pool unstandardised ("weighted") mean differences (WMDs){p_end}

{syntab :Forest plot and/or saved data}
{synopt :{opt co:unts}}display data counts ({it:N}, {it:mean}, {it:SD}) for treatment and control group{p_end}
{synopt :{opt group1(string)}, {opt group2(string)}}specify title text for the two columns created by {opt counts}{p_end}
{synopt :{opt npts}}display participant numbers in the forest plot{p_end}
{synoptline}

{marker model_name}{...}
{synopthdr :model_name}
{synoptline}
{pstd}
Note: references for particular models may be found in {help metan##refs:Fisher 2015}
if not specifically referenced below.{p_end}

{syntab :Methods applicable to two-group comparison of binary outcomes only}
{synopt :{opt mh:aenszel}}Mantel-Haenszel model (default){p_end}
{synopt :{opt peto}}common-effect pooling of Peto odds ratios{p_end}

{syntab :Tau-squared estimators for standard inverse-variance random-effects model}
{synopt :{opt common} | {opt fe} | {opt iv}}Common (aka "fixed") effects inverse-variance (default unless two-group comparison of binary outcomes){p_end}
{synopt :{opt random} | {opt re} | {opt dl}}DerSimonian-Laird estimator{p_end}
{synopt :{opt bdl} | {opt dlb}}Bootstrap DerSimonian-Laird estimator{p_end}
{synopt :{opt ca} | {opt he} | {opt vc}}Cochran ANOVA-like estimator aka Hedges aka "variance component" estimator{p_end}
{synopt :{opt eb} | {opt gq} | {opt mp}}Mandel-Paule aka Generalised Q aka "empirical Bayes" estimator{p_end}
{synopt :{opt ml}}Maximum likelihood (ML) estimator{p_end}
{synopt :{opt reml}}Restricted maximum likelihood (REML) estimator{p_end}
{synopt :{opt hm}}Hartung-Makambi estimator ({help metan##refs:Hartung and Makambi 2003}){p_end}
{synopt :{opt b0} {opt bp}}Rukhin B0 and BP estimators{p_end}
{synopt :{opt sj2s}}Sidik-Jonkman two-step estimator{p_end}
{synopt :{opt dk2s}}DerSimonian-Kacker two-step estimator ({help metan##refs:DerSimonian and Kacker 2007}){p_end}
{synopt :{opt sa}}Sensitivity analysis with user-defined I-squared or tau-squared{p_end}

{syntab :Non-standard models, or modifications to standard models}
{synopt :{opt hk:sj}}Hartung-Knapp-Sidik-Jonkman (HKSJ) variance correction to DerSimonian-Laird estimator{p_end}
{synopt :{opt pl}}Estimation using profile likelihood{p_end}
{synopt :{opt kr}}Kenward-Roger variance-corrected REML model ({help metan##refs:Morris et al 2018}){p_end}
{synopt :{opt bt}}Biggerstaff-Tweedie approximate Gamma model{p_end}
{synopt :{opt hc}}Henmi-Copas approximate Gamma model ({help metan##refs:Henmi and Copas 2010}){p_end}
{synopt :{opt mu:lt}}Multiplicative heterogeneity model ({help metan##refs:Thompson and Sharp 1999}){p_end}
{synopt :{opt ivh:et}}"Inverse-variance heterogeneity" (IVHet) model ({help metan##refs:Doi et al 2015a}){p_end}
{synopt :{opt qe}}Quality Effects model ({help metan##refs:Doi et al 2015b}){p_end}
{synoptline}

{marker options_model}{...}
{synopthdr :options_model}
{synoptline}
{synopt :{opt wgt(varname)}}specify a variable containing user-defined weights applicable to a specific model{p_end}
{synopt :{opt hk:sj}}Hartung-Knapp-Sidik-Jonkman (HKSJ) variance correction, applicable to any standard tau-squared estimator{p_end}
{synopt :{opt ro:bust}}Sidik-Jonkman robust (sandwich-like) variance estimator ({help metan##refs:Sidik and Jonkman 2006}){p_end}

{syntab :model-specific options}
{synopt :{opt tsqlevel(#)}}set confidence level for reporting confidence intervals for tau-squared; default is {cmd:tsqlevel(95)}{p_end}
{synopt :{opt init(model_name)}}initial estimate of tau-squared for two-step estimators {opt sj2s} and {opt dk2s}, with default {it:{help metan##model_name:model_name}} {opt vc}.
For Sidik-Jonkman, any standard tau-squared estimator may instead be used; for DerSimonian-Kacker the only alternative is {opt dl}.{p_end}
{synopt :{opt isq(real)} {opt tausq(real)}}user-defined I-squared (taking values between 0 and 100; default is 80%) or tau-squared (>=0)
values for sensitivity analysis{p_end}
{synopt :{opt tru:ncate}{cmd:(one} | {cmd:zovert)}}optional truncation of the Hartung-Knapp-Sidik-Jonkman correction factor either at 1
({help metan##refs:R{c o:}ver et al 2015}) or at the ratio of the {it:z}-based to the {it:t}-based critical values
({help metan##refs:van Aert et al 2019}){p_end}
{synopt :{opt ba:rtlett} {opt sk:ovgaard}}Bartlett's ({help metan##refs:Huizenga et al 2011}) or Skovgaard's ({help metan##refs:Guolo 2012}) corrections
to the likelihood, for use with the profile likelihood model{p_end}
{synopt: {opt eim} {opt oim}}use expected (default) or observed information matrix to compute degrees of freedom for Kenward-Roger model
({help metan##refs:Morris et al 2018}){p_end}
{synopt :{opt qwt(varname)}}variable containing Quality Effect weights (model {cmd:qe} only; see {help metan##refs:Doi et al 2015b}){p_end}

{syntab :options for iteration, replication or numerical integration}
{synopt :{opt itol(#)}}tolerance for iteration convergence (with {opt eb}, {opt ml}, {opt reml}, {opt pl}, {opt kr}, {opt bt} or {opt hc}){p_end}
{synopt :{opt maxit:er(#)}}maximum number of iterations (as above){p_end}
{synopt :{opt maxt:ausq(#)}}upper bound of search interval; may need to be raised in extreme cases (as above){p_end}
{synopt :{opt quadpts(#)}}number of quadrature points to use in numerical integration (with {opt bt} or {opt hc}; see {help integrate}){p_end}
{synopt :{opt reps(#)}}number of replications for Bootstrap DerSimonian-Laird estimator (with {opt bdl}){p_end}
{synoptline}

{marker options_test}{...}
{synopthdr :options_test}
{synoptline}
{synopt :{opt z} {opt t} {opt chi2}}specify distribution for testing significance of pooled result{p_end}
{synopt :{opt cmh}}Cochran-Mantel-Haenszel test statistic for pooled Mantel-Haenszel odds ratios{p_end}
{synoptline}

{marker options_het}{...}
{synopthdr :options_het}
{synoptline}
{synopt :{opt bre:slow}}Breslow-Day test for homogeneity of odds ratios{p_end}
{synopt :{opt ta:rone}}Breslow-Day-Tarone test for homogeneity of odds ratios (preferred; see e.g. {help metan##refs:Breslow 1996})){p_end}
{synopt :{opt coch:ranq}}Cochran's Q heterogeneity statistic (default, unless Mantel-Haenszel){p_end}
{synopt :{opt qp:rofile}}confidence interval for tau-squared using Q Profile method ({help metan##refs:Viechtbauer 2007}){p_end}
{synopt :{opt hig:gins}}test-based confidence interval for {it:H} ({help metan##refs:Higgins and Thompson 2002}){p_end}
{synopt :{opt nc:chi2}}heterogeneity confidence intervals based on a non-central chi-squared distribution for {it:Q}{p_end}
{synopt :{opt qg:amma}}heterogeneity confidence intervals based on a Gamma distribution for {it:Q}{p_end}
{synoptline}

{marker options_user}{...}
{synopthdr :options_user}
{synoptline}
{synopt :{opt first(ES lci uci desc)}}specify externally-derived pooled estimate{p_end}
{synopt :{opt firststats(string)}}further description of externally-derived estimate, taking the place of heterogeneity statistics in the forest plot{p_end}
{synopt :{opt second(ES lci uci desc)}}specify externally-derived pooled estimate as a second analysis{p_end}
{synopt :{opt secondstats(string)}}further description of externally-derived estimate as a second analysis, taking the place of heterogeneity statistics in the forest plot{p_end}
{synoptline}

{pstd}
where {it:ES lci uci} are an effect estimate and 95% confidence interval based on calculations performed externally to {cmd:metan},
and {it:desc} is a brief description of the calculation, similar to {it:model_name} (e.g. "Bayesian")

{pstd}
If {opt first()} is specified, then {opt wgt(varname)} must also be supplied.
However, {opt second()} may not be used in this case, and neither may {opt by()} for obvious reasons.
If {opt second(ES lci uci desc)} is specified then {opt nosecsub} is invoked for obvious reasons.
The contents of {opt firststats()} and {opt secondstats()} will appear on the forest plot in place of the standard heterogeneity text,
but will not appear in the output printed to the Results Window.


{marker description}{...}
{title:Description}

{pstd}
{cmd:metan} performs meta-analysis of aggregate data; that is, data in which each observation represents a summary of a larger study.
Aggregate data may consist of cell counts from a two-group comparison of binary outcomes; sample sizes, means and standard deviations
from a two-group comparison of continuous outcomes; or generic (pre-calculated) effect sizes and their standard errors
or 95% confidence limits.

{pstd}
If confidence intervals are supplied, they are assumed to be symmetric, and the standard error is derived as {bf:(}{it:UCI} {bf:-} {it:LCI}{bf:) / 2*1.96}.
Hence, supplied confidence limits must be based on a normal distribution, and have 95% level, or the pooled result will not be accurate.


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

{phang}
{opt cumulative} requests that the meta-analysis be performed cumulatively; that is, performed repeatedly with one study being added each time, in the order specified by {cmd:sortby()}.
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
With two-group comparison of binary outcomes, {it:ci_type} may also be {opt cornfield},
specifying confidence intervals for odds ratios by the method of Cornfield, rather than the (default) Woolf method
(see help for {bf:{help cc}}; not to be confused with {cmd:metan}'s {cmd:cc()} option!).

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
Effect statistics are instead returned in the matrix {bf:r(coeffs)} (see {help ipdmetan##saved_results:{bf:ipdmetan}}).

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
{opt sortby(varname)} allows user-specified ordering of studies in the table and forest plot,
without altering the data in memory.

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


{dlgtab:Two-group comparison of binary outcomes}

{pstd}
If no options are supplied, the default analysis is a Risk Ratio (Relative Risk),
pooled using the Mantel-Haenszel method with the associated Mantel-Haenszel heterogeneity statistic.

{marker cc_method}{...}
{phang}
{cmd:cc(}[{it:#}] [{cmd:,} {it:cc_method}]{cmd:)} controls continuity correction in the case where studies contain zero cells.

{pmore}
By default, {cmd:metan} adds 0.5 to each cell of a trial where a zero is encountered, to enable finite variance estimators to be calculated.
The exception to this is if {cmd:mhaenzsel} methods or {cmd:peto} Odds Ratios are used; in these cases, continuity correction is unnecessary.
Note that corrections are applied during computation only; the original data are not modified.
However, the {help metan##saved_results:new variable} {cmd:_CC} may be added to the dataset or {help metan##saved_datasets:results set},
to indicate the studies to which a correction was applied. See also the {opt nointeger} option.

{pmore2}
{it:#} is the correction factor.  The default is 0.5, but other factors may also be used, including zero.
In that case, studies containing zero cells may be excluded from the analysis.

{pmore}
{help metan##refs:Sweeting (2004)} discusses the issue of continuity correction, and suggested two alternative approaches.
In both cases a constant of proportionality is required, which again defaults to 0.5 but may instead be user-specified.
These approaches are specified using {it:cc_method} as follows:

{pmore2}
{opt opp:osite} uses the reciprocal of the opposite group arm size as the correction term.

{pmore2}
{opt emp:irical} uses an empirical estimate for the correction term, derived from the remaining studies.
This approach may only be used with Odds Ratios, and requires at least one study {ul:not} to contain any zero cells.

{phang}
{opt nocc} is synonymous with {cmd:cc(0)} and suppresses continuity correction.
Studies containing zero cells may be excluded from the analysis.

{phang}
{opt nointeger} allows cell counts or sample sizes to be non-integers.
This may be useful when a variable continuity correction is sought for studies containing zero cells;
but may also be used in other circumstances, such as where a cluster-randomised trial is to be incorporated
and the "effective sample size" is less than the total number of observations.

{phang}
{opt counts} displays data counts {it:n}/{it:N} for each group in columns to the left of the forest plot.

{pmore}
{opt group1(string)}, {opt group2(string)} are for use with the {opt counts} option, and contain names for the two groups.
If these are not supplied, the default names "Treatment" and "Control" are used.

{phang}
{opt npts} displays participant numbers in a column to the left of the forest plot.

{phang}
{opt oev} displays the statistics {it:O-E} and {it:V} in columns to the right of the graph.


{dlgtab:Two-group comparison of continuous outcomes}

{pstd}
If no options are supplied, the default analysis is a common-effect inverse-variance analysis
of Standardised Mean Difference (SMD) by the method of Cohen.

{phang}
{opt cohen}, {opt hedges}, {opt glass} pool standardised mean differences by the methods of Cohen (default),
Hedges and Glass respectively ({help metan##refs:Deeks, Altman and Bradburn 2001}).

{phang}
{opt md}, {opt wmd}, {opt nostandard} are synonyms, and pool unstandardised ("weighted") mean differences.

{phang}
{opt counts} displays the sample size, mean and SD for each group in columns to the left of the forest plot. 

{pmore}
{opt group1(string)}, {opt group2(string)} are for use with the {opt counts} option, and contain names for the two groups.
If these are not supplied, the default names "Treatment" and "Control" are used.

{phang}
{opt npts} displays participant numbers in a column to the left of the graph.



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

{p2col 7 32 36 2:{bf:_ES}}Effect size (ES) on the interval scale (e.g. log odds ratio){p_end}
{p2col 7 32 36 2:{bf:_seES}}Standard error of ES{p_end}
{p2col 7 32 36 2:{bf:_LCI}}Lower confidence limit for ES{p_end}
{p2col 7 32 36 2:{bf:_UCI}}Upper confidence limit for ES{p_end}
{p2col 7 32 36 2:{bf:_WT}}Study percentage weight (between 0 and 100){p_end}
{p2col 7 32 36 2:{bf:_NN}}Study sample size{p_end}
{p2col 7 32 36 2:{bf:_CC}}Marker of whether continuity correction was applied{p_end}
{p2col 7 32 36 2:{bf:_rsample}}Marker of which observations were used in the analysis{p_end}


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

{pstd}
Some of these variables have associated characteristics; type {bf:{help char:char list}} to see these.


{marker diffs_metan}{...}
{title:Note: Differences from previous versions of {cmd:metan}}

{pstd}
This latest version of {cmd:metan} is designed to run under Stata version 11 and upwards, with the exception of the Biggerstaff-Tweedie and Henmi-Copas models
which require an additional user-contributed Mata function written for Stata version 12.
The previous version of {bf:metan}, v3.04 written for Stata v9, remains available within this package under the name {help metan9:metan9}.
A still older version, v1.86 written for Stata v7, also remains available under the name {help metan7:metan7}.

{pstd}
This version of {cmd:metan} has been designed with consistency and backwards-compatibility in mind.
However, there are some differences in syntax and operation from previous versions.  In particular:

{phang}
Most options specific to the forest plot (i.e. those that do not affect the results appearing in the Results Window)
now need to be placed within the {opt forestplot()} option rather than directly to {cmd:metan}. This includes {opt nostats} and {opt nowt}.
Also, the forest plot option {opt double} is not currently implemented.

{phang}
{opt nooverall} no longer automatically enforces {opt nowt}.

{phang}
Prediction intervals ({opt rfdist}) are no longer displayed with dotted lines if the number of studies is less than three;
instead, the interval is simply not displayed at all. A message is printed in the Results Window explaining this.

{phang}
If a random-effects model or {opt wgt()} is specified,
the displayed Q statistic will still be based on the inverse-variance common-effect model.
However, if {it:{help metan##model_name:model_name}} is {cmd:mhaenzsel} or {cmd:peto}, the displayed Q statistic
will be based on the pooled effect and weights from the specified model.

{phang}
If {it:{help metan##model_name:model_name}} is {cmd:mhaenzsel}, continuity correction is not necessary for pooling and will not be applied.
However, for display purposes only, continuity correction {ul:will} be applied to the individual study estimates.

{phang}
Note that the first listed syntax for {it:{help metan##model_spec:model_spec}} reflects the previous syntax of {cmd:metan} (see {help metan9:metan9}),
under which {it:{help metan##options_test:options_test}} and {it:{help metan##options_het:options_het}} were supplied as main options.
However, in fact {it:{help metan##options_test:options_test}} and {it:{help metan##options_het:options_het}} may also be supplied
as main options with the {ul:second} listed syntax for {it:{help metan##model_spec:model_spec}}.
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
Risk difference from raw cell counts, random effects model, "label" specification with counts displayed

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
Same again, but now with three different pooling methods: Mantel-Haenszel common-effect,
DerSimonian-Laird random-effects, and REML random-effects with Hartung-Knapp-Sidik-Jonkman variance correction.
We also request the test-based confidence interval for {it:H} proposed by {help metan##refs:Higgins and Thompson (2002)}

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
Analyse continuous data (six-parameter syntax), stratify by type of study, with weights summing to 100% within sub group,
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
Use {cmd:metan} to generate log odds ratio and standard error from the raw cell counts,
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
Display diagnostic test data with three-parameter syntax. Weight is number of positive diagnoses, axis label set
and null specified at 50%. Overall effect estimate is not displayed, graph for visual examination only.

{cmd}{...}
{* example_start - metan_ex5}{...}
{phang2}
. metan percent lowerci upperci, wgt(n_positives) study(id) nooverall notable {* ///}{p_end}
{p 16 20 2}
forestplot( xlabel(0(10)100, force) null(50) title(Sensitivity, position(6)) ){p_end}
{* example_end}{...}
{txt}{...}
{pmore}
{it:({stata metan_hlp_run metan_ex5 using metan.sthlp, restpres:click to run})}{p_end}


{pstd}
User has analysed data with a non-standard technique and supplied effect estimates, weights and description of statistics.
The scheme "Economist" has been used.

{cmd}{...}
{* example_start - metan_ex6}{...}
{phang2}
. metan OR ORlci ORuci, label(namevar=id) wgt(bweight){* ///}{p_end}
{p 16 20 2}
first(0.924 0.753 1.095 Bayesian) firststats(param V=3.86, p=0.012){* ///}{p_end}
{p 16 20 2}
forestplot(xlabel(0.25 0.5 1 2 4, force) null(1) scheme(economist)){p_end}
{* example_end}{...}
{txt}{...}
{pmore}
{it:({stata metan_hlp_run metan_ex6 using metan.sthlp, restpres:click to run})}{p_end}


{pstd}
Same example again, but this time instead of using {opt first()} or {opt firststats()},
we save and load a "forest plot results set", which is then edited to contain our user-defined estimates.
Finally, the forest plot is generated, and appears identical to that in the previous example.
This demonstrates some of the flexibility of being able to edit (and otherwise manipulate) forest plot results sets.

{pstd}
Note: the option {opt useopts} ensures that no information is lost between the initial call to {cmd:metan}
and the final call to {cmd:forestplot}. For further details, see {bf:{help forestplot}}.

{cmd}{...}
{* example_start - metan_ex6a}{...}
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
{it:({stata metan_hlp_run metan_ex6a using metan.sthlp, restpres:click to run})}{p_end}


{pstd}
Variable "counts" defined showing raw data. Options to change the box, effect estimate marker and confidence interval are used,
and the counts variable has been attached to the estimate marker as a label.

{cmd}{...}
{* example_start - metan_ex7}{...}
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
{it:({stata metan_hlp_run metan_ex7 using metan.sthlp, restpres:click to run})}{p_end}



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
Thanks to Julian Higgins and Jonathan A C Sterne (University of Bristol, Bristol, UK) offered advice and helped facilitate this latest update.

{pstd}
The "click to run" element of the examples in this document is handled using an idea originally developed by Robert Picard.



{marker refs}{...}
{title:References}

{phang}
Breslow NE, Day NE. 1980. Statistical Methods in Cancer Research: Vol. I - The Analysis of Case-Control Studies.
Lyon: International Agency for Research on Cancer.

{phang}
Deeks JJ, Altman DG, Bradburn MJ. 2001.
Statistical methods for examining heterogeneity and combining results from several studies in meta-analysis.
In Systematic Reviews in Health Care: Meta-analysis in Context, ed. Egger M, Davey Smith G, Altman DG, 2nd ed., 285-312. London: BMJ Books.

{phang}
DerSimonian R, Kacker R. 2007.
Random-effects model for meta-analysis of clinical trials: An update.
Contemporary Clinical Trials 28: 105-114. doi: 10.1016/j.cct.2006.04.004

{phang}
Doi SAR, Barendregt JJ, Khan S, Thalib L, Williams GM. 2015a.
Advances in the meta-analysis of heterogeneous clinical trials I: The inverse variance heterogeneity model.
Contemporary Clinical Trials 45: 130-138

{phang}
Doi SAR, Barendregt JJ, Khan S, Thalib L, Williams GM. 2015b.
Advances in the meta-analysis of heterogeneous clinical trials II: The quality effects model.
Contemporary Clinical Trials 45: 123-129

{phang}
Fisher DJ. 2015.
Two-stage individual participant data meta-analysis and generalized forest plots.
Stata Journal 15: 369-396

{phang}
Guolo A. 2012.
Higher-order likelihood inference in meta-analysis and meta-regression.
Statistics in Medicine 31: 313-327. doi: 10.1002/sim.4451

{phang}
Hartung J, Makambi KH. 2003.
Reducing the number of unjustified significant results in meta-analysis.
Communications in Statistics - Simulation and Computation 32: 1179-1190. doi: 10.1081/SAC-120023884

{phang}
Henmi M, Copas JB. 2010.
Confidence intervals for random effects meta-analysis and robustness to publication bias.
Statistics in Medicine 29: 2969-2983. doi: 10.1002/sim.4029

{phang}
Higgins JPT, Thompson SG. 2002.
Quantifying heterogeneity in a meta-analysis.
Statistics in Medicine 21: 1539-1558

{phang}
Higgins JPT, Thompson SG, Spiegelhalter DJ. 2009.
A re-evaluation of random-effects meta-analysis.
JRSS Series A 172: 137-159

{phang}
Huizenga HM, Visser I, Dolan CV. 2011.
Testing overall and moderator effects in random effects meta-regression.
British Journal of Mathematical and Statistical Psychology 64: 1-19

{phang}
Morris TP, Fisher DJ, Kenward MG, Carpenter JR. 2018.
Meta-analysis of quantitative individual patient data: two stage or not two stage?
Statistics in Medicine. doi: 10.1002/sim.7589

{phang}
R{c o:}ver C, Knapp G, Friede T. 2015.
Hartung-Knapp-Sidik-Jonkman approach and its modification for random-effects meta-analysis with few studies.
BMC Medical Research Methodology 15: 99-105

{phang}
Sidik K, Jonkman JN. 2006.
Robust variance estimation for random effects meta-analysis.
Computational Statistics & Data Analysis 50: 3681-3701. doi: 10.1016/j.csda.2005.07.019

{phang}
Sweeting MJ, Sutton AJ, Lambert PC. 2004.
What to add to nothing? Use and avoidance of continuity corrections in meta-analysis of sparse data.
Statistics in Medicine 23: 1351-1375. doi: 10.1002/sim.1761

{phang}
Thompson SG, Sharp SJ. 1999.
Explaining heterogeneity in meta-analysis: a comparison of methods.
Statistics in Medicine 18: 2693-2708

{phang}
van Aert RCM, Jackson D. 2019.
A new justification of the Hartung-Knapp method for random-effects meta-analysis
based on weighted least squares regression.
Research Synthesis Methods... DOI: 10.1002/jrsm.1356

{phang}
Viechtbauer W. 2007.
Confidence intervals for the amount of heterogeneity in meta-analysis.
Statistics in Medicine 26: 37-52. doi: 10.1002/sim.2514

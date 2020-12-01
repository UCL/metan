{smcl}
{* *! version 3.4 (beta)  David Fisher  12dec2019}{...}
{vieweralsosee "metan" "help metan"}{...}
{vieweralsosee "metan_binary" "help metan_binary"}{...}
{vieweralsosee "metan_continuous" "help metan_continuous"}{...}
{vieweralsosee "metan_proportion" "help metan_proportion"}{...}
{vieweralsosee "forestplot" "help forestplot"}{...}
{vieweralsosee "metani" "help metani"}{...}
{vieweralsosee "" "--"}{...}
{vieweralsosee "ipdmetan" "help ipdmetan"}{...}
{vieweralsosee "ipdover" "help ipdover"}{...}
{vieweralsosee "metaan" "help metaan"}{...}
{vieweralsosee "metandi" "help metandi"}{...}
{vieweralsosee "metaprop_one" "help metaprop_one"}{...}
{hi:help metan_model}
{hline}

{title:Title}

{phang}
{it:model_spec} {hline 2} Specify models and methods for meta-analytic pooling of aggregate (summary) data with {bf:{help metan}}


{marker syntax}{...}
{title:Syntax}

{pstd}
{it:model_spec} can either be:

{pmore2}
[ {it:model_name} ] [ {cmd:second(}{it:model_name}{cmd:)} ] [ {it:{help metan_model##options_user:options_user}}
{it:{help metan_model##options_test:options_test}} {it:{help metan_model##options_het:options_het}} ]

{pmore}
where {it:model_name} is {opt mh:aenszel} | {opt peto} | {opt common} | {opt random}

{pstd}
or {it:model_spec} can be:

{pmore2}
{cmd:model(} {it:model} [ {cmd:\} {it:model} [ {cmd:\} {it:model} [...]]] {cmd:)}

{pmore}
where {it:model} is

{pmore2}
{it:{help metan_model##model_name:model_name}} [ {cmd:,} {it:{help metan_model##options_model:options_model}}
{it:{help metan_model##options_test:options_test}} {it:{help metan_model##options_het:options_het}} ]


{marker description}{...}
{title:Description}

{pstd}
{it:model_spec} specifies method(s) for meta-analytic pooling with {bf:{help metan}}.
If no {it:model_spec} is supplied, the default for two-group comparison of binary outcomes is {opt mh:aenszel};
otherwise the default is {opt common}.

{pstd}
The first syntax for {it:model_spec} directly maintains the previous syntax of {cmd:metan} {help metan##diffs_metan:v3.04 for Stata v9}.
In particular, note that the synonyms {opt fixed}, {opt fixedi} and {opt randomi} (for {opt mhaenszel}, {opt common} and {opt random}, respectively) still work.
Previous options for user-supplied estimates, such as {opt first()} and {opt second()}, also continue to be supported; see {it:{help metan_model##options_user:options_user}}.

{pstd}
The second syntax for {it:model_spec} is more general, and allows the results of multiple different pooling methods (or variations of methods) to be displayed simultaneously.
Such methods include alternative estimators of the between-study heterogeneity (tau-squared),
methods which correct {it:post hoc} the variance of the pooled estimate, or those which use alternative weighting systems
in order to improve statistical performance in the suspected presence of publication bias; see {it:{help metan_model##model_name:model_name}}.

{pstd}
{help metan:Click here} to return to the main {bf:{help metan}} help page.



{marker model_name}{...}
{synoptset 24 tabbed}{...}
{synopthdr :model_name}
{synoptline}
{pstd}
Note: references for particular models may be found in {help metan_model##refs:Fisher 2015}
if not specifically referenced below.{p_end}

{syntab :Methods applicable to {help metan_binary:two-group comparison of binary outcomes} only}
{synopt :{opt mh:aenszel}}Mantel-Haenszel model (default){p_end}
{synopt :{opt peto}}common-effect pooling of Peto odds ratios{p_end}

{syntab :Tau-squared estimators for standard inverse-variance random-effects model}
{synopt :{opt common} | {opt fe} | {opt iv}}Common (aka "fixed") effects inverse-variance (default unless two-group comparison of binary outcomes){p_end}
{synopt :{opt random} | {opt re} | {opt dl:aird}}DerSimonian-Laird estimator{p_end}
{synopt :{opt bdl} | {opt dlb}}Bootstrap DerSimonian-Laird estimator{p_end}
{synopt :{opt he:dges}}Hedges estimator aka "Cochran ANOVA-like" aka "variance component" estimator{p_end}
{synopt :{opt mp:aule} | {opt eb:ayes}}Mandel-Paule aka "empirical Bayes" estimator{p_end}
{synopt :{opt ml:e}}Maximum likelihood (ML) estimator{p_end}
{synopt :{opt reml}}Restricted maximum likelihood (REML) estimator{p_end}
{synopt :{opt hm:akambi}}Hartung-Makambi estimator ({help metan_model##refs:Hartung and Makambi 2003}){p_end}
{synopt :{opt b0} {opt bp}}Rukhin B0 and BP estimators{p_end}
{synopt :{opt sj2s}}Sidik-Jonkman two-step estimator{p_end}
{synopt :{opt dk2s}}DerSimonian-Kacker two-step estimator ({help metan_model##refs:DerSimonian and Kacker 2007}){p_end}
{synopt :{opt sa}}Sensitivity analysis with user-defined I-squared or tau-squared{p_end}

{syntab :Non-standard models, or modifications to standard models}
{synopt :{opt hk:sj}}Hartung-Knapp-Sidik-Jonkman (HKSJ) variance correction to DerSimonian-Laird estimator{p_end}
{synopt :{opt pl}}Estimation using profile likelihood{p_end}
{synopt :{opt kr:oger}}Kenward-Roger variance-corrected REML model ({help metan_model##refs:Morris et al 2018}){p_end}
{synopt :{opt bt:weedie}}Biggerstaff-Tweedie approximate Gamma model{p_end}
{synopt :{opt hc:opas}}Henmi-Copas approximate Gamma model ({help metan_model##refs:Henmi and Copas 2010}){p_end}
{synopt :{opt mu:lt}}Multiplicative heterogeneity model ({help metan_model##refs:Thompson and Sharp 1999}){p_end}
{synopt :{opt ivh:et}}"Inverse-variance heterogeneity" (IVHet) model ({help metan_model##refs:Doi et al 2015a}){p_end}
{synopt :{opt qe}}Quality Effects model ({help metan_model##refs:Doi et al 2015b}){p_end}
{synoptline}


{marker options_model}{...}
{synopthdr :options_model}
{synoptline}
{synopt :{opt wgt(varname)}}specify a variable containing user-defined weights applicable to a specific model
(for more details see {it:{help metan##options_main:options_main}}){p_end}
{synopt :{opt hk:sj}}Hartung-Knapp-Sidik-Jonkman (HKSJ) variance correction, applicable to any standard tau-squared estimator{p_end}
{synopt :{opt ro:bust}}Sidik-Jonkman robust (sandwich-like) variance estimator ({help metan_model##refs:Sidik and Jonkman 2006}){p_end}

{syntab :model-specific options}
{synopt :{opt tsqlevel(#)}}set confidence level for reporting confidence intervals for tau-squared; default is {cmd:tsqlevel(95)}{p_end}
{synopt :{opt init(model_name)}}initial estimate of tau-squared for two-step estimators {opt sj2s} and {opt dk2s}, with default {it:{help metan_model##model_name:model_name}} {opt hedges}.
For Sidik-Jonkman, any standard tau-squared estimator may instead be used; for DerSimonian-Kacker the only alternative is {opt dlaird}.{p_end}
{synopt :{opt isq(real)} {opt tausq(real)}}user-defined I-squared (taking values between 0 and 100; default is 80%) or tau-squared (>=0)
values for sensitivity analysis{p_end}
{synopt :{opt tru:ncate}{cmd:(one}|{cmd:zovert)}}optional truncation of the Hartung-Knapp-Sidik-Jonkman correction factor either at 1
({help metan_model##refs:R{c o:}ver et al 2015}) or at the ratio of the {it:z}-based to the {it:t}-based critical values
({help metan_model##refs:van Aert et al 2019}){p_end}
{synopt :{opt ba:rtlett} {opt sk:ovgaard}}Bartlett's ({help metan_model##refs:Huizenga et al 2011})
or Skovgaard's ({help metan_model##refs:Guolo 2012}) corrections to the likelihood, for use with the profile likelihood model{p_end}
{synopt: {opt eim} {opt oim}}use expected (default) or observed information matrix to compute degrees of freedom for Kenward-Roger model
({help metan_model##refs:Morris et al 2018}){p_end}
{synopt :{opt qwt(varname)}}variable containing Quality Effect weights (model {cmd:qe} only; see {help metan_model##refs:Doi et al 2015b}){p_end}

{syntab :options for iteration, replication or numerical integration}
{synopt :{opt itol(#)}}tolerance for iteration convergence (with {opt mpaule}, {opt mle}, {opt reml}, {opt pl}, {opt kroger}, {opt btweedie} or {opt hcopas}){p_end}
{synopt :{opt maxit:er(#)}}maximum number of iterations (as above){p_end}
{synopt :{opt maxt:ausq(#)}}upper bound of search interval; may need to be raised in extreme cases (as above){p_end}
{synopt :{opt quadpts(#)}}number of quadrature points to use in numerical integration (with {opt bt} or {opt hc}; see help for {bf:{help integrate}}){p_end}
{synopt :{opt reps(#)}}number of replications for Bootstrap DerSimonian-Laird estimator (with {opt bdl}){p_end}
{synoptline}


{marker options_test}{...}
{synopthdr :options_test}
{synoptline}
{synopt :{opt z} {opt t} {opt chi2}}specify distribution for testing significance of pooled result{p_end}
{synopt :{opt cmh}}Cochran-Mantel-Haenszel test statistic (Mantel-Haenszel odds ratios only){p_end}
{synoptline}


{marker options_het}{...}
{synopthdr :options_het}
{synoptline}
{synopt :{opt coch:ranq}}Cochran's Q heterogeneity statistic (default, unless Mantel-Haenszel){p_end}
{synopt :{opt qp:rofile}}confidence interval for tau-squared using Q Profile method ({help metan_model##refs:Viechtbauer 2007}){p_end}
{synopt :{opt hig:gins}}test-based confidence interval for {it:H} ({help metan_model##refs:Higgins and Thompson 2002}){p_end}
{synopt :{opt nc:chi2}}heterogeneity confidence intervals based on a non-central chi-squared distribution for {it:Q}{p_end}
{synopt :{opt qg:amma}}heterogeneity confidence intervals based on a Gamma distribution for {it:Q}{p_end}
{synopt :{opt bre:slow}}Breslow-Day test for homogeneity of odds ratios (Mantel-Haenszel only){p_end}
{synopt :{opt ta:rone}}Breslow-Day-Tarone test for homogeneity of odds ratios
(Mantel-Haenszel only; preferred to {opt breslow}, see e.g. {help metan_model##refs:Breslow 1996}){p_end}
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
Schwarzer G, Chemaitelly H, Abu-Raddad LJ, R{c u:}cker G. 2019.
Seriously misleading results using inverse of Freeman-Tukey double arcsine transformation
in meta-analysis of single proportions.
Research Synthesis Methods 10: 476–483. doi: 10.1002/jrsm.1348

{phang}
Sidik K, Jonkman JN. 2006.
Robust variance estimation for random effects meta-analysis.
Computational Statistics & Data Analysis 50: 3681-3701. doi: 10.1016/j.csda.2005.07.019

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
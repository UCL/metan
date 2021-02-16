{smcl}
{* *! version 4.01  David Fisher  12feb2021}{...}
{vieweralsosee "metan" "help metan"}{...}
{vieweralsosee "metan_model" "help metan_model"}{...}
{vieweralsosee "metan_continuous" "help metan_continuous"}{...}
{vieweralsosee "metan_proportion" "help metan_proportion"}{...}
{vieweralsosee "forestplot" "help forestplot"}{...}
{vieweralsosee "metani" "help metani"}{...}
{vieweralsosee "" "--"}{...}
{vieweralsosee "ipdmetan" "help ipdmetan"}{...}
{vieweralsosee "ipdover" "help ipdover"}{...}
{vieweralsosee "metabias" "help metabias"}{...}
{vieweralsosee "metatrim" "help metatrim"}{...}
{vieweralsosee "metaan" "help metaan"}{...}
{vieweralsosee "metandi" "help metandi"}{...}
{vieweralsosee "metaprop_one" "help metaprop_one"}{...}
{hi:help metan_binary}
{hline}

{title:Title}

{phang}
{hi:metan} {hline 2} Perform meta-analysis of two-group binary data


{marker syntax}{...}
{title:Syntax}

{p 8 18 2}
{cmd:metan} {it:event_treat} {it:noevent_treat} {it:event_ctrl} {it:noevent_ctrl} {ifin}
[{cmd:,} {it:{help metan_model:model_spec}} {it:{help metan_binary##options_binary:options_binary}} {it:{help metan##options_main:options_main}}]


{marker options_binary}{...}
{synoptset 24 tabbed}{...}
{synopthdr :options_binary}
{synoptline}
{syntab :Options}
{synopt :{opt rr}}pool risk ratios (default){p_end}
{synopt :{opt or}}pool odds ratios{p_end}
{synopt :{opt rd}}pool risk differences{p_end}
{synopt :{opt effect(string)}}user-defined title for "effect size" column in the output{p_end}
{synopt :{opt ci:type(ci_type)}}method of constructing confidence intervals for reporting of individual studies ({ul:not} pooled results){p_end}
{synopt :{cmd:cc(}[{it:#}] [{cmd:,} {it:{help metan_binary##cc_opts:cc_opts}}]{cmd:)}}use continuity correction value other than 0.5 for zero cells;
implement alternative methods of {help metan_binary##refs:Sweeting (2004)}{p_end}
{synopt :{opt nocc}}suppress continuity correction entirely{p_end}
{synopt :{opt noint:eger}}allow cell counts to be non-integers{p_end}
{synopt :{opt log}}display log effect sizes and confidence limits (for ratio measures only){p_end}

{syntab :Additional options specific to Mantel-Haenszel odds ratios}
{syntab :(strictly speaking part of {it:{help metan_model##options_het:model_spec}}, but repeated here for convenience)}
{synopt :{opt cmh}}Cochran-Mantel-Haenszel test statistic{p_end}
{synopt :{opt bre:slow}}Breslow-Day test for homogeneity of odds ratios ({help metan_model##refs:Breslow and Day 1980}){p_end}
{synopt :{opt ta:rone}}Breslow-Day-Tarone test for homogeneity of odds ratios
(preferred to {opt breslow}; see e.g. {help metan_binary##refs:Breslow 1996}){p_end}

{syntab :Forest plot and/or saved data}
{synopt :{opt effi:cacy}}additionally display odds ratios or risk ratios expressed in terms of vaccine efficacy{p_end}
{synopt :{opt co:unts}}display data counts ({it:n}/{it:N}) for treatment and control group{p_end}
{synopt :{opt group1(string)}, {opt group2(string)}}specify title text for the two columns created by {opt counts}{p_end}
{synopt :{opt npts}}display participant numbers in the forest plot{p_end}
{synopt :{opt oev}}display columns containing {it:O_E} and {it:V}{p_end}
{synoptline}


{marker description}{...}
{title:Description}

{pstd}
{cmd:metan} performs meta-analysis of aggregate data; that is, data in which each observation represents a summary of a larger study.
This page describes options specific to a meta-analysis of studies comparing a binary outcome between two treatment groups.
If no options are supplied, the default analysis is to pool Risk Ratios (Relative Risks)
using the Mantel-Haenszel method, and with the associated Mantel-Haenszel heterogeneity statistic.

{pstd}
{help metan:Click here} to return to the main {bf:{help metan}} help page
and to find documentation for {it:{help metan_model:model_spec}} and {it:{help metan##options_main:options_main}}.


{dlgtab:Options}

{phang}
{opt effect(string)} overrides the default title for the "effect size" column in the output generated by {opt rr}, {opt or} and {opt rd}.

{phang}
{opt citype(ci_type)} specifies how confidence limits for individual studies should be constructed for display purposes.
This option acts independently of how confidence limits for {ul:pooled} results are constructed
(which will depend upon {it:{help metan##model_spec:model_spec}}). See also {bf:{help metan##options_main:level(#)}}

{pmore}
With two-group comparison of binary outcomes, the default {it:ci_type} is {opt exact} giving an exact interval.
Alternatively, options {opt woolf} or {opt cornfield} may be specified; see {bf:{help cc}}
(not to be confused with {cmd:metan}'s {cmd:cc()} option!).

{phang2}
If {opt nointeger} is specified, a Woolf-type interval will be constructed and {opt citype()} may not be specified.

{marker cc_opts}{...}
{phang}
{cmd:cc(}[{it:#}] [{cmd:,} {it:cc_opts}]{cmd:)} controls continuity correction in the case where studies contain zero cells.

{pmore}
If {it:{help metan_model:model_spec}} is {cmd:mhaenzsel} or {cmd:peto}, continuity correction is unnecessary
(see, for example, {help metan_binary##refs:Bradburn et al 2007}) and {it:#} is set to zero by default.
Otherwise, {cmd:metan} adds 0.5 to each cell of a study where a zero is encountered, to enable finite variance estimators to be calculated.
Any default behaviour may be over-ridden by explicit specification of {it:#} and/or {it:cc_opts}.
(Note that studies with zero cells in {ul:both} treatment groups are always excluded from the analysis.)

{pmore}
Note that corrections are applied during computation only; the original data are not modified.
However, the {help metan##saved_results:new variable} {cmd:_CC} may be added to the dataset or {help metan##saved_datasets:results set},
to indicate the studies to which a correction was applied. See also the {opt nointeger} option.

{pmore2}
{it:#} is the correction factor.  The default is (usually) 0.5, but other factors may also be used;
including zero, which is equivalent to specifying {opt nocc} (see below).

{pmore}
{help metan_binary##refs:Sweeting (2004)} discusses the issue of continuity correction, and suggested two alternatives
to the default approach of adding a constant value to all four cells in the 2x2 table.
In both cases a constant of proportionality is required, which again defaults to 0.5 but may instead be user-specified.
These approaches are specified using {it:cc_opts} as follows (not appropriate with {opt proportion}):

{pmore2}
{opt opp:osite} uses the reciprocal of the opposite group arm size as the correction term.

{pmore2}
{opt emp:irical} uses an empirical estimate for the correction term, derived from the remaining studies.
This approach may only be used with Odds Ratios, and requires at least one study {ul:not} to contain any zero cells.

{pmore}
Finally: the usual, and generally recommended, strategy is to add correction factors only to those studies where a zero cell is found.
However, the following alternative strategy is sometimes used:

{pmore2}
{opt all:ifzero} adds correction factors to {ul:all} studies if at least one study with a zero cell is found.

{phang}
{opt nocc} is synonymous with {cmd:cc(0)} and suppresses continuity correction.
Studies containing zero cells may be excluded from the analysis.

{phang}
{opt nointeger} allows cell counts or sample sizes to be non-integers.
This may be useful for manually implementing other corrective strategies for zero cells;
but may also be used in other circumstances, such as where a cluster-randomised trial is to be incorporated
and the "effective sample size" is less than the total number of observations.

{phang}
{opt log} requests that results using ratio statistics (that is, OR or RR) are reported on the log scale.
The analysis itself is unchanged.


{dlgtab:Forest plot and/or saved data}

{phang}
{opt efficacy} expresses results as the vaccine efficacy (the proportion of cases that would have been prevented
in the placebo group that would have been prevented had they received the vaccination).
Only available with odds ratios or risk ratios.

{phang}
{opt counts} displays data counts {it:n}/{it:N} for each group in columns to the left of the forest plot.

{pmore}
{opt group1(string)}, {opt group2(string)} are for use with the {opt counts} option, and contain names for the two groups.
If these are not supplied, the default names "Treatment" and "Control" are used.

{phang}
{opt npts} displays participant numbers in a column to the left of the forest plot.

{phang}
{opt oev} displays the statistics {it:O-E} and {it:V} in columns to the right of the graph.



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
Current version, {cmd:metan} v4.01:
David Fisher, MRC Clinical Trials Unit at UCL, London, UK.

{pstd}
Email {browse "mailto:d.fisher@ucl.ac.uk":d.fisher@ucl.ac.uk}



{title:Acknowledgments}

{pstd}
Thanks to Patrick Royston (MRC Clinical Trials Unit at UCL, London, UK) for suggestions of improvements to the code and help file.

{pstd}
Thanks to Vince Wiggins, Kit Baum and Jeff Pitblado of Statacorp who offered advice and helped facilitate the version 9 update.

{pstd}
Thanks to Julian Higgins and Jonathan A C Sterne (University of Bristol, Bristol, UK) offered advice and helped facilitate this latest update,
and thanks to Daniel Klein (Universit{c a:}t Kassel, Germany) for assistance with testing under older Stata versions.

{pstd}
The "click to run" element of the examples in this document is handled using an idea originally developed by Robert Picard.



{marker refs}{...}
{title:References}

{phang}
Bradburn MJ, Deeks JJ, Berlin JA, Localio AR. 2007.
Much ado about nothing: a comparison of the performance of meta-analytical methods with rare events.
Statistics in Medicine 26: 53-77. doi: 10.1002/sim.2528

{phang}
Breslow NE, Day NE. 1980. Statistical Methods in Cancer Research: Vol. I - The Analysis of Case-Control Studies.
Lyon: International Agency for Research on Cancer.

{phang}
Breslow NE. 1996.
Statistics in epidemiology: The case-control study.
Journal of the American Statistical Association 91: 14-28

{phang}
Higgins JPT, Thomas J, Chandler J, Cumpston M, Li T, Page MJ, Welch VA (editors). 2019.
Cochrane Handbook for Systematic Reviews of Interventions, 2nd Edition. Chichester: Wiley.

{phang}
Sweeting MJ, Sutton AJ, Lambert PC. 2004.
What to add to nothing? Use and avoidance of continuity corrections in meta-analysis of sparse data.
Statistics in Medicine 23: 1351-1375. doi: 10.1002/sim.1761


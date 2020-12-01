{smcl}
{* *! version 3.6 (beta)  David Fisher  22may2020}{...}
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
{synopt :{opt rd}}pool risk differences (see also {it:{help eform_option}}){p_end}
{synopt :{cmd:cc(}[{it:#}] [{cmd:,} {it:{help metan##cc_method:cc_method}}]{cmd:)}}use continuity correction value other than 0.5 for zero cells;
implement alternative methods of {help metan##refs:Sweeting (2004)}{p_end}
{synopt :{opt nocc}}suppress continuity correction entirely{p_end}
{synopt :{opt noint:eger}}allow cell counts to be non-integers{p_end}

{syntab :Additional options specific to Mantel-Haenszel odds ratios}
{syntab :(strictly speaking part of {it:{help metan_model##options_het:model_spec}}, but repeated here for convenience)}
{synopt :{opt cmh}}Cochran-Mantel-Haenszel test statistic{p_end}
{synopt :{opt bre:slow}}Breslow-Day test for homogeneity of odds ratios{p_end}
{synopt :{opt ta:rone}}Breslow-Day-Tarone test for homogeneity of odds ratios
(preferred to {opt breslow}; see e.g. {help metan_model##refs:Breslow 1996}){p_end}

{syntab :Forest plot and/or saved data}
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
{opt citype(ci_type)} specifies how confidence limits for individual studies should be constructed for display purposes.
This option acts independently of how confidence limits for {ul:pooled} results are constructed
(which will depend upon {it:{help metan##model_spec:model_spec}}). See also {bf:{help metan##options_main:level(#)}}

{pmore}
With two-group comparison of binary outcomes, the default {it:ci_type} is {opt exact} giving an exact interval.
Alternatively, options {opt woolf} or {opt cornfield} may be specified; see {bf:{help cc}}
(not to be confused with {cmd:metan}'s {cmd:cc()} option!).

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
These approaches are specified using {it:cc_method} as follows (not appropriate with {opt proportion}):

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


{dlgtab:Forest plot and/or saved data}

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
Sweeting MJ, Sutton AJ, Lambert PC. 2004.
What to add to nothing? Use and avoidance of continuity corrections in meta-analysis of sparse data.
Statistics in Medicine 23: 1351-1375. doi: 10.1002/sim.1761

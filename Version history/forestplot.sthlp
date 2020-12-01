{smcl}
{* *! version 1.0  David Fisher  30jan2014}{...}
{vieweralsosee "metan" "help metan"}{...}
{vieweralsosee "ipdmetan" "help ipdmetan"}{...}
{vieweralsosee "admetan" "help admetan"}{...}
{vieweralsosee "ipdover" "help ipdover"}{...}
{viewerjumpto "Syntax" "forestplot##syntax"}{...}
{viewerjumpto "Description" "forestplot##description"}{...}
{viewerjumpto "Options" "forestplot##options"}{...}
{title:Title}

{phang}
{bf:forestplot} {hline 2} Create forest plots from data currently in memory


{marker syntax}{...}
{title:Syntax}

{p 8 18 2}
{cmd:forestplot} [{varlist}] {ifin} [{cmd:, }
{it:options}]

{pstd}
where {it:varlist} is:

{p 16 24 2}
{it:ES} {it:lci} {it:uci} [{it:wt}] [{it:use}]

{synoptset 30 tabbed}{...}
{synopthdr}
{synoptline}
{syntab: Main}
{synopt :{opt dataid(varname)}}define groups of observations making up a complete forestplot{p_end}
{synopt :{opt dp(#)}}number of decimal places to display{p_end}
{synopt :{it:{help eform_option}}}exponentiate effect sizes and confidence limits{p_end}
{synopt :{opt eff:ect(string)}}title for "effect size" column in the output{p_end}
{synopt :{opt fav:ours(string)}}x-axis labelling specific to forest plots{p_end}
{synopt :{opt lab:els(varname)}} variable containing labels e.g. subgroup headings, heterogeneity info, study names{p_end}
{synopt :{opt lcol:s(varlist)} {opt rcol:s(varlist)}}display columns of additional data{p_end}
{synopt :{opt nona:me}}suppress display of study names in left-hand column{p_end}
{synopt :{opt nonu:ll}}suppress null hypothesis line{p_end}
{synopt :{opt noov:erall}, {opt nosu:bgroup}}suppress display of overall and/or subgroup pooled estimates{p_end}
{synopt :{opt nostat:s}, {opt nowt}}suppress display of effect estimates and/or weights in right-hand columns{p_end}
{synopt :{cmd:plotid(}{it:varlist} [{cmd:, {ul:l}ist {ul:nogr}aph}]{cmd:)}}define groups of observations in which
to apply specific plot rendition options{p_end}
{synopt :{it:{help twoway_options}}}other Stata twoway graph options, as appropriate{p_end}

{syntab: Fine-tuning}
{synopt :{opt noadj:ust}}suppress "space-saving" adjustment to text/data placement{p_end}
{synopt :{opt ast:ext(#)}}percentage of the graph to be taken up by text{p_end}
{synopt :{opt box:scale(#)}}box size scaling{p_end}
{synopt :{opt r:ange(numlist)}}x-axis limits of data plotting area, independently of axis ticks/labels{p_end}
{synopt :{opt text:scale(#)}}font size scaling for text display on graph{p_end}
{synopt :{cmdab:xlab:el(}{it:numlist}{cmd:, force)}}force x-axis limits of data plotting area together with ticks/labels{p_end}

{syntab: Plot rendition}
{synopt :{it:plot}{cmd:{ul:op}ts(}{it:plot_options}{cmd:)}}affect rendition of all observations{p_end}
{synopt :{it:plot{ul:#}}{cmd:opts(}{it:plot_options}{cmd:)}}affect rendition of observations in {it:#}th {cmd:plotid} group{p_end}
{synopt :{opt classic}}use "classic" set of plot options, as in {help metan}{p_end}
{synopt :{opt interaction}}use "interaction" set of plot options{p_end}
{synoptline}

{pstd}
where {it:plot} may be {cmd:{ul:box}}, {cmd:{ul:ci}}, {cmd:{ul:diam}}, {cmd:{ul:oline}}, {cmd:{ul:point}}, {cmd:{ul:pci}} or {cmd:{ul:ppoint}}

{pstd}
and {it:plot_options} are either {it:{help marker_options}} or {it:{help line_options}}, as appropriate.

{p2colreset}{...}
{p 4 6 2}

{marker description}{...}
{title:Description}

{pstd}
{cmd:forestplot} creates forest plots from variables containing point estimates and lower/upper confidence limits.
The meta-analysis program {help ipdmetan} (and its sister programs {help admetan} and {help ipdover})
call {cmd:forestplot} by default, and pass on any relevant options.
They can also save datasets with additional information that {cmd:forestplot} recognises.
Alternatively, {cmd:forestplot} can simply be run on data currently in memory.

{pstd}
{cmd:forestplot} requires three variables corresponding to {it:ES}, {it:lci} and {it:uci},
representing the effect size (point estimate) and lower/upper confidence limits on the normal scale
(i.e. log odds or log hazards, not exponentiated).
These may be supplied manually (in the above order) using {it:varlist}.
Otherwise, {cmd:forestplot} will expect to find variables in memory named {bf:_ES}, {bf:_LCI} and {bf:_UCI}.

{pstd}
{cmd:forestplot} will also check for variables corresponding to {it:wt} and {it:use},
respectively representing the meta-analysis weight (relative marker size),
and an indicator of the contents of each observation (study effects, titles, spacing, description of heterogeneity, etc).
The default names for these variables are {bf:_WT} and {bf:_USE} respectively (although this may be overridden);
{cmd:forestplot} will assume they are constant if not found.

{pstd}
The values of the variable {it:use} are interpreted by {cmd:forestplot} in the following way:
{p_end}

	0 = subgroup labels
	1 = non-missing study estimates
	2 = missing study estimates
	3 = subgroup pooled effects
	4 = blank line, or other text (e.g. description of heterogeneity)
	5 = overall pooled effect



{marker options}{...}
{title:Options}

{dlgtab:Main}

{phang}
{opt dataid(varname)} define groups of observations making up a complete forest plot.
It may be that the data in memory comes from multiple separate meta-analyses, whose forest plots
it is desired to plot within the same {it:plot region} (see {it:{help region_options}}).
Specifiying {opt dataid()} tells {cmd:forestplot} where the data from one meta-analysis ends
and the next begins, and results in correct placement of the overall effect line(s).

{phang}
{opt dp(#)} specifies the number of decimal places to format the effect sizes.
This option is carried over from {help metan}, but was previously undocumented.

{phang}
{it:{help eform_option}} specifies that the effect sizes and confidence limits should be exponentiated.
The option also generates a heading for the effect size column in the output (table and forest plot).

{pmore}
Note that {cmd:forestplot} expects effect sizes to be beta coefficients (i.e. on the linear scale).

{phang}
{opt effect(string)} specifies a heading for the effect size column in the output.
This overrides any heading generated by {it:{help eform_option}}.

{phang}
{opt favours(string)} applies a label saying something about the treatment effect to either side of the graph
(strings are separated by the # symbol).  This option is carried over from {help metan}.

{pmore}
Note that {opt favours()} and {opt xtitle()} use {it:{help axis_label_options}} rather than the usual {it:{help axis_title_options}}.
In the case of {opt favours()}, suboptions must be passed to a separate {opt xmlabel()} option.
For example: ...{opt favours(string)} {cmd:xmlabel(, labgap(*1.5))}.

{phang}
{opt labels(varname)} specify (text) variable containing labels for the left-hand side of the graph,
e.g. subgroup titles, heterogeneity details and, usually, study names.

{phang}
{opt lcols(string)}, {opt rcols(string)} define columns of additional data to the left or right of the graph.
These options work in exactly the same way as in {help metan} when specified directly to {cmd:forestplot}.
(N.B. when specified to {help ipdmetan##forestplot_options:ipdmetan} they have a different syntax.)

{pmore}
The first two columns on the right are automatically set to effect size and weight, and the first on the left
to study name/identifier (unless suppressed using the options {opt nostats}, {opt nowt} and {opt noname} respectively).
{opt textsize()} can be used to fine-tune the size of the text in order to achieve a satisfactory appearance.
The columns are labelled with the name of the variable or macro.

{phang}
{opt nulloff} removes the null hypothesis line from the graph.  This option is carried over from {help metan}.

{phang}
{cmd:plotid(}{it:varlist} [{cmd:, list nograph}]{cmd:)} specifies one or more categorical variables to form a series of groups of observations
in which specific aspects of plot rendition may be affected using {bf:box}{it:#}{bf:opts}, {bf:ci}{it:#}{bf:opts} etc.
The groups of observations will automatically be assigned ordinal labels (1, 2, ...) based on the ordering of {it:varlist}.

{pmore}
The contents of each group may be inspected with the {bf:list} option.
In complex situations it may be helpful to view this list without creating the plot itself;
this may be achieved using the {bf:nograph} option.

{pmore}
Note that {opt plotid} does not alter the placement or ordering of data within the plot.


{dlgtab:Fine-tuning}

{pstd}
These options allow fine-tuning of the plot construction and text/data placement.
Forestplots are non-standard twoway plots, and have features that Stata graphs were not designed to accommodate
(e.g. columns of text, and a wide range of potential aspect ratios). Occasionally, therefore, {cmd:forestplot}
will produce unsatisfactory results, which may be improved by use of one or more of these options.

{phang}
{opt noadjust} suppresses a calculation within {cmd:forestplot} which may result in labelling text
overlapping a pooled-estimate diamond.

{pmore}
(N.B. This calculation, included in {help metan}, attempts to take advantage of the fact that pooled-estimate
diamonds have less width than indiviudal study estimates, whilst their labelling text is often longer,
to "compress" the plot and make it more aesthetic.)

{phang}
{opt astext(#)} specifies the percentage of the graph to be taken up by text.
The default is 50 and the percentage must be in the range 10-90.  This option is carried over from {help metan}.

{phang}
{opt boxscale(#)} controls box scaling.  The default is 100 (as in a percentage) and may be increased
or decreased as such (e.g., 80 or 120 for 20% smaller or larger respectively).
This option is carried over from {help metan}.

{phang}
{opt range(numlist)} specifies the range of the x-axis containing data, independently of {it:{help axis_options}},
for the purposes of text placement.
For instance, a large blank space between the data and either the left or right stats columns can be reduced (or created).
Effect sizes or confidence limits outside the range will be represented by off-scale arrows.

{phang}
{opt textscale(#)} specifies font size for text display on graph.  The default is 100 (as in a percentage)
and may be increased or decreased as such (e.g., 80 or 120 for 20% smaller or larger respectively).
This option is carried over from {help metan}.

{phang}
{cmd:xlabel(}{it:numlist}{cmd:, force)} with the {opt force} option operates similarly to {opt range()} - 
the smallest and largest values in {it:numlist} become the range (unless {opt range()} itself is also specified).
{opt xlabel()} otherwise functions in the standard way.
This option is carried over from {help metan} but with modifications.


{dlgtab:Plot rendition}

{pstd}
These options specify sub-options for individual components making up the forest plot,
each of which is drawn using a separate {help twoway} command.
Any sub-options associated with a particular {help twoway} command may be used,
unless they would conflict with the appearance of the graph as a whole.
For example, diamonds are plotted using the {help twoway_pcspike} command, so {it:{help line_options}} may be used,
but not {opt horizontal} or {opt vertical}.
These options are carried over from {help metan}, but with modifications.

{pmore}
{cmd:boxopts(}{it:{help marker_options}}{cmd:)} and {cmd:box}{it:#}{cmd:opts(}{it:{help marker_options}}{cmd:)}
affect the rendition of weighted boxes representing point estimates
and use options for a weighted marker (e.g., shape, colour; but not size).

{pmore}
{cmd:ciopts(}{it:{help line_options}} [{cmd:rcap}]{cmd:)} and {cmd:ci}{it:#}{cmd:opts(}{it:{help line_options}} [{cmd:rcap}]{cmd:)}
affect the rendition of confidence intervals. The additional option {cmd:rcap} requests capped spikes.

{pmore}
{cmd:diamopts(}{it:{help line_options}}{cmd:)} and {cmd:diam}{it:#}{cmd:opts(}{it:{help line_options}}{cmd:)}
affect the rendition of diamonds representing pooled estimates.

{pmore}
{cmd:olineopts(}{it:{help line_options}}{cmd:)} and {cmd:oline}{it:#}{cmd:opts(}{it:{help line_options}}{cmd:)}
affect the rendition of overall effect lines.

{pmore}
{cmd:pointopts(}{it:{help marker_options}}{cmd:)} and {cmd:point}{it:#}{cmd:opts(}{it:{help marker_options}}{cmd:)}
affect the rendition of (unweighted) point estimate markers
(e.g. to clarify the precise point within a larger weighted box).

{pstd}
If it is desired that pooled estimates not be represented by diamonds,
but rather by point estimates plus confidence intervals as for the individual estimates
(albeit with different {it:plot_options}), this may be achieved by specifying
the following options as replacements for {cmd:diamopts()} or {cmd:diam}{it:#}{cmd:opts()}:

{pmore}
{cmd:pciopts(}{it:{help line_options}}{cmd:)} and {cmd:pci}{it:#}{cmd:opts(}{it:{help line_options}}{cmd:)}
affect the rendition of confidence intervals for pooled estimates.

{pmore}
{cmd:ppointopts(}{it:{help marker_options}}{cmd:)} and {cmd:ppoint}{it:#}{cmd:opts(}{it:{help marker_options}}{cmd:)}
affect the rendition of (unweighted) pooled estimate markers.


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
Also thanks to Vince Wiggins at Statacorp for advice.

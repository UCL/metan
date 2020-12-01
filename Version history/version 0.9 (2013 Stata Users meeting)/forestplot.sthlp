{smcl}
{* *! version 1.2.0  02jun2011}{...}
{vieweralsosee "metan" "help metan"}{...}
{vieweralsosee "ipdmetan" "help ipdmetan"}{...}
{vieweralsosee "admetan" "help admetan"}{...}
{vieweralsosee "ipdover" "help ipdover"}{...}
{viewerjumpto "Syntax" "forestplot##syntax"}{...}
{viewerjumpto "Description" "forestplot##description"}{...}
{viewerjumpto "Options" "forestplot##options"}{...}
{viewerjumpto "Remarks" "forestplot##remarks"}{...}
{title:Title}

{phang}
{bf:forestplot} {hline 2} Create forest plots


{marker syntax}{...}
{title:Syntax}

{p 8 18 2}
{cmd:forestplot} [{varlist}] {ifin} [{cmd:, }
{it:plot_options graph_options using_option}]

{pstd}
where {it:varlist} is:

{p 16 24 2}
{it:ES} {it:lci} {it:uci} [{it:wgt}] [{it:use}]

{pstd}
and the syntax of {it:using_option} is:

{p 16 24 2}
{cmd:using(}{it:filenamelist} {ifin} [{cmd:, }{it:plot_options}]{cmd:)}
[{cmd:using(}{it:filenamelist} {ifin} [{cmd:, }{it:plot_options}{cmd:)}] ...]

{pstd}
where {it:filenamelist} is a list of one or more Stata-format filenames. 
{p_end}


{synoptset 24 tabbed}{...}
{synopthdr:graph_options}
{synoptline}
{synopt :{opt astext(#)}}percentage of the graph to be taken up by text{p_end}
{synopt :{opt boxsca(#)}}box scaling{p_end}
{synopt :{opt dp(#)}}number of decimal places to display{p_end}
{synopt :{opt favours(string)}}x-axis labelling specific to forest plots{p_end}
{synopt :{opt labels(varname)}} variable containing labels e.g. subgroup headings, heterogeneity info, study names{p_end}
{synopt :{opt lcols(varlist)} {opt rcols(varlist)}}display columns of additional data{p_end}
{synopt :{opt nulloff}}remove the null hypothesis line from the graph{p_end}
{synopt :{opt range(numlist)}}limits of plotting area, independently of axis ticks/labels{p_end}
{synopt :{opt textsize(#)}}font size for text display on graph (percentage of default){p_end}
{synopt :{it:{help twoway_options}}}other Stata twoway graph options affecting the graph as a whole{p_end}
{synoptline}


{synopthdr:plot_options}
{synoptline}
{synopt :{opt nobox}}suppresses weighted boxes for each study{p_end}
{synopt :{opt boxopt(marker_options)}}box appearance options{p_end}
{synopt :{opt ciopt(line_options)}}confidence interval appearance options{p_end}
{synopt :{opt classic}}use "classic" set of appearance options{p_end}
{synopt :{opt diamopt(line_options)}}diamond appearance options{p_end}
{synopt :{opt interaction}}use "interaction" set of appearance options{p_end}
{synopt :{opt olineopt(line_options)}}"overall line" appearance options{p_end}
{synopt :{opt pointopt(marker_options | marker_label_options)}}point appearance options{p_end}
{synopt :{it:{help twoway_options}}}other Stata twoway graph options affecting the plotted data specifically{p_end}
{synoptline}

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
Otherwise, {cmd:forestplot} will expect to find variables in memory named {bf:_ES}, {bf:_lci} and {bf:_uci}.

{pstd}
{cmd:forestplot} will also check for variables corresponding to {it:wgt} and {it:use},
respectively representing the meta-analysis weight (relative marker size),
and an indicator of the contents of each observation (study effects, titles, spacing, description of heterogeneity, etc).
Again, these may either be supplied manually as the fourth and fifth variables in {it:varlist}
(note that if {it:wgt} and/or {it:use} are to be supplied manually, so must all five variables)
or automatically as variables named {bf:_wgt} and {bf:_use}.
They may also be omitted entirely; {cmd:forestplot} will assume they are constant.

{pstd}
The values of the variable {it:use} are interpreted by {cmd:forestplot} in the following way:
{p_end}

	0 = subgroup labels
	1 = valid study estimates
	2 = missing study estimates
	3 = subgroup pooled effects
	4 = blank line, or other text (e.g. description of heterogeneity)
	5 = overall pooled effect



{marker options}{...}
{title:Options}

{dlgtab:Graph options}

{phang}
{opt astext(#)} specifies the percentage of the graph to be taken up by text.
The default is 50 and the percentage must be in the range 10-90.  This option is carried over from {help metan}.

{phang}
{opt boxsca(#)} controls box scaling.  The default is 100 (as in a percentage) and may be increased
or decreased as such (e.g., 80 or 120 for 20% smaller or larger respectively).
This option is carried over from {help metan}.

{phang}
{opt dp(#)} specifies the number of decimal places to format the effect sizes.
This option is carried over from {help metan}, but was previously undocumented.

{phang}
{opt range(numlist)} specifies the range of the x-axis containing data, independently of {it:{help axis_options}},
for the purposes of text placement.
For instance, a large blank space between the data and either the left or right stats columns can be reduced (or created).

{phang}
{opt textsize(#)} specifies font size for text display on graph.  The default is 100 (as in a percentage)
and may be increased or decreased as such (e.g., 80 or 120 for 20% smaller or larger respectively).
This option is carried over from {help metan}.

{phang}
{cmd:xlabel(}{it:numlist}{cmd:, force)} forces the x-axis scale to be in the range specified by {it:numlist}.
This option is carried over from {help metan} but with modifications.  {opt xlabel()} otherwise functions in the standard way.


{dlgtab:Plot options}

{pstd}
{opt boxopt()}, {opt diamopt()}, {opt pointopt()}, {opt ciopt()} and {opt olineopt()} specify options
for individual components making up the forest plot, each of which is drawn using a separate {help twoway} command.
Any options associated with a particular {help twoway} command may be used,
unless they would conflict with the appearance of the graph as a whole.
For example, diamonds are plotted using the {help twoway_pcspike} command, so options for
line styles are available; however, altering the orientation with the options {opt horizontal} or {opt vertical}
is not allowed. So, {cmd:diamopt(lcolor(green) lwidth(thick))} feeds into a command such as
{cmd:pcspike(y1 x1 y2 x2, lcolor(green) lwidth(thick))}.
These options are carried over from {help metan}, but with some modifications.

{pmore}
{opt boxopt()} controls the boxes and uses options for a weighted marker (e.g., shape, colour; but not size).
If {opt interaction} is specified and any options are repeated (that is, two colour options, whether the same
or different values) they will be applied to study effect markers and pooled effect markers respectively;
otherwise options will be applied to both.

{pmore}
{opt diamopt()} controls the diamonds and uses options for {help twoway pcspike}
(except {opt horizontal} or {opt vertical}). If {opt interaction} is specified, the default is to plot
clear circles with dark blue outlines instead of diamonds;
this can be over-ridden by specifying {opt diamopt()} with or without an argument.

{pmore}
{opt pointopt()} controls the point estimate using marker options (see {manhelpi marker_options G-3}).

{pmore}
{opt ciopt()} controls the confidence intervals for studies using options for {help twoway pcspike}
(except {opt horizontal} or {opt vertical}).

{pmore}
{opt olineopt()} controls the overall effect line using options for {help twoway line}.

{phang}
{opt favours(string)} applies a label saying something about the treatment effect to either side of the graph
(strings are separated by the # symbol).  This option is carried over from {help metan}.

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



{marker remarks}{...}
{title:Remarks on use of the using() option}

{pstd}
The {cmd:using()} option syntax allows the user to indefinitely append pieces of similarly-formatted data
to create a single dataset, which is then plotted.  Each {cmd:using()} option can be modified by {ifin} directives
and/or {it:plot_options}, which will be applied to each of the datasets in {it:filenamelist}.
Hence, for example, if different {it:plot_options} are desired for different parts of a single {it:filename},
it can be specified repeatedly in different {cmd:using()} options using {ifin}:

{phang2}
{cmd:...using(file1.dta if subgroup==1, boxopt(mcolor(red))) using(file1.dta if subgroup==2, boxopt(mcolor(green)))}

{pstd}
Note that {it:plot_options} may be specified either directly to {cmd:forestplot}
or as sub-options to a {cmd:using()} option.
If the former, they will apply to the entire plot;
if the latter, they will apply only to data derived from {it:filenamelist}.
This is intended to be analogous to the standard {help twoway} multiple-plot syntax.
{it:graph_options}, by contrast, apply to the graph as a whole and may only be supplied directly to {cmd:forestplot}.

{pstd}
Variables {it:ES}, {it:lci}, {it:uci}, {it:wgt} and {it:use} should have the same names in each of the datasets, although
{it:wgt} and {it:use} need not be present in all (or, indeed, any) datasets.




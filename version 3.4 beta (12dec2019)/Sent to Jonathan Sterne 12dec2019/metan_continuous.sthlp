{smcl}
{* *! version 4.00  David Fisher  03dec2019}{...}
{vieweralsosee "metan" "help metan"}{...}
{vieweralsosee "metan_model" "help metan_model"}{...}
{vieweralsosee "metan_binary" "help metan_binary"}{...}
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
{hi:help metan_continuous}
{hline}

{title:Title}

{phang}
{hi:metan} {hline 2} Perform meta-analysis of two-group continuous data


{marker syntax}{...}
{title:Syntax}

{p 8 18 2}
{cmd:metan} {it:n_treat} {it:mean_treat} {it:sd_treat} {it:n_ctrl} {it:mean_ctrl} {it:sd_ctrl} {ifin}
[{cmd:,} {it:{help metan_model:model_spec}} {it:{help metan_continuous##options:options_continuous}} {it:{help metan##options_main:options_main}}]


{marker options_continuous}{...}
{synoptset 24 tabbed}{...}
{synopthdr :options_continuous}
{synoptline}
{syntab :Options}
{synopt :{opt coh:end}}pool Cohen's d standardised mean differences (default){p_end}
{synopt :{opt hed:gesg}}pool Hedges's g standardised mean differences{p_end}
{synopt :{opt gla:ssd}}pool Glass's delta mean differences, standardised by the control group{p_end}
{synopt :{opt md}, {opt wmd}, {opt nostan:dard}}pool unstandardised ("weighted") mean differences{p_end}

{syntab :Forest plot and/or saved data}
{synopt :{opt co:unts}}display data counts ({it:N}, {it:mean}, {it:SD}) for treatment and control group{p_end}
{synopt :{opt group1(string)}, {opt group2(string)}}specify title text for the two columns created by {opt counts}{p_end}
{synopt :{opt npts}}display participant numbers in the forest plot{p_end}
{synoptline}


{marker description}{...}
{title:Description}

{pstd}
{cmd:metan} performs meta-analysis of aggregate data; that is, data in which each observation represents a summary of a larger study.
This page describes options specific to a meta-analysis of studies comparing a continuous outcome between two treatment groups.
If no options are supplied, the default analysis is a common-effect inverse-variance analysis of Cohen's d standardised mean differences.

{pstd}
{help metan:Click here} to return to the main {bf:{help metan}} help page
and to find documentation for {it:{help metan_model:model_spec}} and {it:{help metan##options_main:options_main}}.


{dlgtab:Options}

{phang}
{opt cohend}, {opt hedgesg}, {opt glassd} pool standardised mean differences by the methods of Cohen (default),
Hedges and Glass respectively ({help metan##refs:Deeks, Altman and Bradburn 2001}).

{phang}
{opt md}, {opt wmd}, {opt nostandard} are synonyms, and pool unstandardised ("weighted") mean differences.


{dlgtab:Forest plot and/or saved data}

{phang}
{opt counts} displays the sample size, mean and SD for each group in columns to the left of the forest plot. 

{pmore}
{opt group1(string)}, {opt group2(string)} are for use with the {opt counts} option, and contain names for the two groups.
If these are not supplied, the default names "Treatment" and "Control" are used.

{phang}
{opt npts} displays participant numbers in a column to the left of the graph.



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


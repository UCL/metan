{smcl}
{* *! version 1.0  David Fisher  31jan2014}{...}
{vieweralsosee "ipdmetan" "help ipdmetan"}{...}
{vieweralsosee "forestplot" "help forestplot"}{...}
{vieweralsosee "ipdover" "help ipdover"}{...}
{vieweralsosee "metan" "help metan"}{...}
{viewerjumpto "Syntax" "admetan##syntax"}{...}
{viewerjumpto "Description" "admetan##description"}{...}
{viewerjumpto "Options" "admetan##options"}{...}
{viewerjumpto "Saved results" "admetan##saved_results"}{...}
{title:Title}

{phang}
{cmd:admetan} {hline 2} Perform two-stage inverse-variance meta-analysis of aggregate data


{marker syntax}{...}
{title:Syntax}

{p 8 18 2}
{cmd:admetan} {varlist} {ifin}
[{cmd:,} {it:options}]

{pstd}
where {it:varlist} is 
{p_end}
		{it:ES} {it:seES}
{pstd}
or
{p_end}
		{it:ES} {it:lci} {it:uci}


{synoptset 24 tabbed}{...}
{synopthdr}
{synoptline}

{synopt :{opt npts(varname)}}specify variable containing participant numbers{p_end}
{synopt :{opt study(varname)}}specify study (trial) identifier{p_end}
{p2col  :{help ipdmetan##options:{it:ipdmetan_options}}}any {cmd:ipdmetan} options except those listed below{p_end}
{synopt	:{cmd:forestplot(}{help forestplot##options:{it:forestplot_options}}{cmd:)}}forestplot options{p_end}


{marker description}{...}
{title:Description}

{pstd}
{cmd:admetan} represents the special case of {help ipdmetan} where no IPD data is available, only aggregate data.
As such, it may be seen as a direct alternative to {help metan}.
{it:varlist} must be supplied, and must contain either two or three variables:
the effect size (on the normal scale), followed by either the standard error of the effect size
or the lower and upper 95% confidence limits.

{pstd}
Any {help ipdmetan##options:ipdmetan} options may be used with the exception of {opt ad()}, {opt nototal}, and {cmd:plotid(_BYAD)}.

{marker options}{...}
{title:Options}

{dlgtab:Options specific to admetan}

{phang}
{opt npts(varname)} specifies a variable containing numbers of participants in each study, for display in tables and forestplots.

{phang}
{opt study(varname)} specifies the variable containing the study identifier, which must be either integer-valued or string.
If {opt study()} is not supplied, the studies will simply be labelled sequentially as "1", "2", etc.

{phang} For other options (given the caveat in the {help ipdover##description:Description}),
see {help ipdmetan##options:ipdmetan} or {help forestplot##options:forestplot}.


{marker saved_results}{...}
{title:Saved results}

{pstd}
{cmd:admetan} saves the same results in {cmd:r()} as {help ipdmetan}.


{title:Author}

{p}
David Fisher, MRC Clinical Trials Unit at UCL, London, UK.

Email {browse "mailto:d.fisher@ucl.ac.uk":d.fisher@ucl.ac.uk}

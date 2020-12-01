{smcl}
{* *! version 1.0  David Fisher  30jan2014}{...}
{vieweralsosee "sts test" "help sts test"}{...}
{vieweralsosee "ipdmetan" "help ipdmetan"}{...}
{vieweralsosee "forestplot" "help forestplot"}{...}
{vieweralsosee "metan" "help metan"}{...}
{viewerjumpto "Syntax" "ipdmetan##syntax"}{...}
{viewerjumpto "Description" "ipdmetan##description"}{...}
{viewerjumpto "Options" "ipdmetan##options"}{...}
{viewerjumpto "Saved results" "ipdmetan##saved_results"}{...}
{title:Title}

{phang}
{bf:petometan} {hline 2} Perform meta-analysis using the Peto (log-rank) method


{marker syntax}{...}
{title:Syntax}

{p 8 18 2}
{cmd:petometan }{it:trt_var} [{cmd:,} {it:options}]

{synoptset 24 tabbed}{...}
{synopthdr}
{synoptline}
{synopt :{opt stu:dy(varname)}}specify the study identifier, to be displayed on screen and in the forestplot{p_end}
{synopt :{opt by(varname)}}specify an optional study-level subgroup identifier{p_end}
{synopt :{opt mat:save(name)}}store data in matrix {it:name}{p_end}
{synopt :{opt nogr:aph}}suppress forest plot{p_end}
{synopt :{opt nohet}}suppress heterogeneity statistics in forest plot{p_end}
{synopt :{opt noov:erall}}suppress overall pooling{p_end}
{synopt :{opt nosu:bgroup}}suppress pooling within subgroups{p_end}
{synopt :{cmd:ovstat(q)}}display Q statistics in forest plot instead of I-squared{p_end}
{synopt :{opt str:ata(varlist)}}specify further variables by which to stratify the log-rank calculations{p_end}

{synoptline}

{p2colreset}{...}
{p 4 6 2}


{marker description}{...}
{title:Description}

{pstd}
{cmd:petometan} performs aggregate or individual participant-data (IPD) meta-analysis using the Peto (log-rank) method.
It may be seen as a meta-analytic extension (or alternative) to {cmd:sts test}, but only for two-arm, single-failure data.
Data must be {help stset}.


{marker options}{...}
{title:Options}

{dlgtab:Analysis}

{phang}
{opt study(varname)} specifies the meta-analysis study identifier, to be presented on screen and in the forestplot.
In terms of log-rank calculation of the overall effect statistics, it is equivalent to specifying {opt strata(varname)}.

{phang}
{opt matsave(name)} requests that study-level data be stored in {it:name}. The stored data consists of: study/subgroup
identifiers; numbers of participants and observed events by treatment arm; expected events; O-E and V.

{phang}
{opt nograph}, {opt nohet}, {opt nooverall} and {opt nosubgroup} suppress, respectively: the forestplot;
display of heterogeneity statistics in the plot; overall pooling; and pooling within subgroups.

{phang}
{cmd:ovstat(q)} requests that Q statistics be used in the forestplot in preference to I-squared statistics.

{phang}
{opt by(varname)} specifies a trial-level subgroup identifier.
It does not affect the calculation of the overall pooled effect, but will be presented in the outout along with relevant
subgroup effects and heterogeneity statistics.

{phang}
{opt strata(varlist)} specifies further variables to be used in log-rank calculations but not be presented in the output.


{marker saved_results}{...}
{title:Saved results}

{pstd}
{cmd:petometan} saves the following in {cmd:r()}:

{synoptset 25 tabbed}{...}
{p2col 5 25 29 2: Scalars}{p_end}
{synopt:{cmd:r(OE)}}Total difference (O-E) of observed and expected numbers of events{p_end}
{synopt:{cmd:r(V)}}Total variance of the observed number of events (hypergeometric variance){p_end}
{synopt:{cmd:r(N)}}Total number of participants{p_end}
{synopt:{cmd:r(o)}}Total observed number of events{p_end}
{synopt:{cmd:r(chi2)}}Overall chi-squared statistic{p_end}
{synopt:{cmd:r(lnHR)}}Overall hazard ratio{p_end}
{synopt:{cmd:r(selnHR)}}Standard error of overall hazard ratio{p_end}
{synopt:{cmd:r(Q)}}Overall Q statistic for heterogeneity{p_end}
{synopt:{cmd:r(k)}}Number of studies{p_end}


{title:Author}

{p}
David Fisher, MRC Clinical Trials Unit at UCL, London, UK.

Email {browse "mailto:d.fisher@ucl.ac.uk":d.fisher@ucl.ac.uk}


{title:Acknowledgments}

{pstd}
The code is based heavily on the built-in Stata command {help sts test}.

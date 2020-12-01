{smcl}
{* *! version 1.01  David Fisher  15apr2014}{...}
{vieweralsosee "ipdmetan" "help ipdmetan"}{...}
{vieweralsosee "forestplot" "help forestplot"}{...}
{vieweralsosee "metan" "help metan"}{...}
{vieweralsosee "admetan" "help admetan"}{...}
{viewerjumpto "Syntax" "ipdover##syntax"}{...}
{viewerjumpto "Description" "ipdover##description"}{...}
{viewerjumpto "Options" "ipdover##options"}{...}
{viewerjumpto "Saved results" "ipdover##saved_results"}{...}
{title:Title}

{phang}
{cmd:ipdover} {hline 2} Generate data for forest plots outside of the context of meta-analysis


{marker syntax}{...}
{title:Syntax}

{p 8 18 2}
{cmd:ipdover}
	[{it:{help exp_list}}]
	{cmd:,} {opt over(varlist)} [{opt over(varname)} {it:options}] {cmd::} {it:command}



{marker description}{...}
{title:Description}

{pstd}
{cmd:ipdover} extends the forest-plot functionality of {help ipdmetan} outside the context of meta-analysis.
It does not perform any pooling or heterogeneity calculations; rather, its intended use is subgroup analyses
within a single trial dataset.
Basic syntax is

{phang2}
{cmd:. ipdover}{cmd:,} {opt over(varlist)} {cmd::} {it:command}

{pstd}
which fits the model {it:command} once within each level of each variable in {it:varlist}
and saves effect sizes and standard errors for screen output and display of a forest plot.
Any e-class regression command (whether built-in or user-defined) should be compatible
with this basic syntax of {cmd:ipdover}; see {help ipdmetan} for help with using non e-class
commands.

{pstd}
{cmd:ipdover} functions in a similar way to {help ipdmetan}, with the following main differences:
the {cmd:over()} option replaces {cmd:study()} and {cmd:by()};
the {cmd:ad()} and {cmd:re()} options are not permitted;
and the {cmd:plotid()} option has a slightly different syntax (see below).

{pstd}
Saved datasets include the following identifier variables:{p_end}
{p2colset 8 24 24 8}
{p2col:{cmd:_BY}}subset of data (c.f. {help by}){p_end}
{p2col:{cmd:_OVER}}{cmd:over()} variable; must be integer-valued or string{p_end}
{p2col:{cmd:_LEVEL}}level of {cmd:over()} variable.{p_end}


{marker options}{...}
{title:Options}

{dlgtab:Options specific to ipdover}

{phang}
{opt over(varlist)} [{opt over(varname)}] specifies the variable(s) whose levels {it:command} is to be fitted within.
The option may be repeated at most once, in which case the second option must contain a single {it:varname} defining
subsets of the data (c.f. {help by}).

{pmore} All variables must be either integer-valued or string.
Variable and value labels will appear in output where appropriate.

{phang}
{cmd:plotid(_BY|_OVER|_LEVEL|_n} [{cmd:, list nograph}]{cmd:)} functions in basically the same way as in {help ipdmetan},
but instead of a {it:varname}, it accepts one of the following values, corresponding to variables created in saved
datasets created by {cmd:ipdover}:{p_end}
{p2colset 8 24 24 8}
{p2col:{cmd:_BY}}group observations by levels of {cmd:_BY}{p_end}
{p2col:{cmd:_OVER}}group observations by levels of {cmd:_OVER}{p_end}
{p2col:{cmd:_LEVEL}}group observations by levels of {cmd:_LEVEL}{p_end}
{p2col:{cmd:_n}}allow each observation to be its own group.{p_end}

{phang}For other options (given the caveat in the {help ipdover##description:Description}),
see {help ipdmetan##options:ipdmetan} or {help forestplot##options:forestplot}.


{marker saved_results}{...}
{title:Saved results}

{pstd}
{cmd:ipdover} saves the following in {cmd:r()}:

{synoptset 25 tabbed}{...}
{p2col 5 25 29 2: Scalars}{p_end}
{synopt:{cmd:r(k)}}Number of included trials {it:k}{p_end}
{synopt:{cmd:r(n)}}Number of included patients{p_end}
{synopt:{cmd:r(mu_hat)}}Overall effect size{p_end}
{synopt:{cmd:r(se_mu_hat)}}Standard error of overall effect size{p_end}

{synoptset 25 tabbed}{...}
{p2col 5 25 29 2: Macros}{p_end}
{synopt:{cmd:r(estvar)}}Name of stored coefficient{p_end}

{synoptset 25 tabbed}{...}
{p2col 5 25 29 2: Matrices}{p_end}
{synopt:{cmd:r(coeffs)}}Matrix of study and subgroup identifers, effect coefficients, and numbers of participants{p_end}

{synoptset 25 tabbed}{...}
{p2col 5 25 29 2: Variables}{p_end}
{synopt:{cmd:_rsample}}Observations included in the analysis (c.f. {cmd:e(sample)}){p_end}


{title:Author}

{p}
David Fisher, MRC Clinical Trials Unit at UCL, London, UK.

Email {browse "mailto:d.fisher@ucl.ac.uk":d.fisher@ucl.ac.uk}

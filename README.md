<a href ="https://www.ctu.mrc.ac.uk/"><img src="MRCCTU_at_UCL_Logo.png" width="50%" /></a>

# metan
 4.0

# A Stata package to perform meta-analysis of aggregate (summary) data

Meta-analysis is a statistical technique for combining results from multiple independent studies, with the aim of estimating a single overall effect. 

The routines in this package provide facilities to conduct meta-analyses of binary (event) or continuous data from two groups, or intervention effect estimates with corresponding standard errors or confidence intervals.  This is an updated version of metan as published in Stata Journal Issue 8, and prior to that in STB-44, authored by Michael J Bradburn, Jonathan J Deeks and Douglas G Altman. 

Updates include a wide range of random-effects models, cumulative and influence analysis, and better handling of heterogeneity, continuity correction and returned values. The routine for constructing forest plots has been separated off and hugely extended; extremely flexible and generalised forest plots may now be produced. 

Also included is an “immediate” command ‘metani’, which accepts numlists or matrices as input rather than variables in memory; and the ‘metannt’ program for binary data, which displays estimated intervention effects in terms of the absolute reduction in risk and number needed to treat.  Other graphical commands produce funnel plots to assess small study effects, and L’Abbe plots to examine whether the assumption of a common odds ratio, risk ratio or risk difference is reasonable.

A description of available Stata meta-analysis commands may be found at  http://www.stata.com/support/faqs/stat/meta.html.

# installation

to install , within Stata type .ssc install metan

# usage

# examples

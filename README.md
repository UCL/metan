<a href ="https://www.ctu.mrc.ac.uk/"><img src="MRCCTU_at_UCL_Logo.png" width="50%" /></a>

# metan
 4.0

# A Stata package to perform meta-analysis of aggregate (summary) data

Meta-analysis is a statistical technique for combining results from multiple independent studies, with the aim of estimating a single overall effect. 

The routines in this package provide facilities to conduct meta-analyses of binary (event) or continuous data from two groups, or intervention effect estimates with corresponding standard errors or confidence intervals.  This is an updated version of **metan** as originally developed by Michael J Bradburn, Jonathan J Deeks and Douglas G Altman and published in [STB-44](https://www.stata.com/products/stb/journals/stb44.html), and more recently updated by Ross J Harris and published in [Stata Journal Issue 8](https://www.stata-journal.com/article.html?article=sbe24_2). Both that previous update and the present one have the support of the original authors. 

Updates include a wide range of random-effects models, cumulative and influence analysis, and better handling of heterogeneity, continuity correction and returned values. The routine for constructing forest plots has been separated off and hugely extended; extremely flexible and generalised forest plots may now be produced via the **forestplot** command.

Also included is an “immediate” command **metani**, which accepts numlists or matrices as input rather than variables in memory; and the **metannt** program for use as a post-analysis command with binary data, to display estimated intervention effects in terms of the absolute reduction in risk and number needed to treat.  Other graphical commands produce funnel plots to assess small study effects, and L’Abbe plots to examine whether the assumption of a common odds ratio, risk ratio or risk difference is reasonable.

A description of available Stata meta-analysis commands may be found at  http://www.stata.com/support/faqs/stat/meta.html.

# installation

We currently recommend installation via the Stata Statistical Software Components (SSC) archive.
To see a package description, including contents, type within Stata:
    .ssc describe metan
...and to perform the installation, type:
    .ssc install metan
Future updates are handled using the **ado update** command; see the built-in Stata documentation.

# usage

# examples

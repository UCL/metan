<a href ="https://www.mrcctu.ucl.ac.uk/"><img src="logo_ukri-mrc-ctu_transparent-background.png" width="50%" /></a>

# metan
Current release: v4.07  05sep2023

# A Stata package to perform meta-analysis of aggregate (summary) data

Meta-analysis is a statistical technique for combining results from multiple independent studies, with the aim of estimating a single overall effect. 

The routines in this package provide facilities to conduct meta-analyses of binary (event) or continuous data from two groups, or intervention effect estimates with corresponding standard errors or confidence intervals.  This is an authorized update to the `metan` package as originally developed by Michael J Bradburn, Jonathan J Deeks and Douglas G Altman and published in [STB-44](https://www.stata.com/products/stb/journals/stb44.html) (1998), and more recently updated by Ross J Harris and published in [Stata Journal Issue 8](https://www.stata-journal.com/article.html?article=sbe24_2) (2008). Both the 2008 update and the current (2020) update have the support of the original authors. 

Updates include a wide range of random-effects models, cumulative and influence analysis, and better handling of heterogeneity, continuity correction and returned values. The routine for constructing forest plots has been separated off and hugely extended; extremely flexible and generalised forest plots may now be produced via the `forestplot` command.

Also included is an “immediate” command `metani`, which accepts numlists or matrices as input rather than variables in memory; and the `metannt` program for use as a post-analysis command with binary data, to display estimated intervention effects in terms of the absolute reduction in risk and number needed to treat.  Other graphical commands produce funnel plots to assess small study effects, and L’Abbe plots to examine whether the assumption of a common odds ratio, risk ratio or risk difference is reasonable.

Below is a full list of files included in the package:

File name | Description | Associated documentation
----------|-------------|-------------------------
metan.ado | Core `metan` functionality | metan.sthlp<br>metan_binary.sthlp<br>metan_continuous.sthlp<br>metan_proportion.sthlp<br>metan_model.sthlp
lmetan.mlib | Mata code to run iterative random-effects models | metan_model.sthlp<br>metan.mata (uncompiled source code, for reference)
metan_pooling.ado | Alternative "in-line" Mata code for use with Stata versions earlier than 16.1 (the version under which lmetan.mata was compiled) | metan_model.sthlp
forestplot.ado | Creates a forest plot from appropriately structured data | forestplot.sthlp
metani.ado | "Immediate" form of `metan` | metani.sthlp
metannt.ado | Post-analysis command to display absolute risk reduction and NNT (binary data only) | metannt.sthlp
labbe.ado | Draws a L'Abbe plot for event data (proportion of successes in the two groups) | labbe.sthlp
metan9.ado | Previous version of `metan` as described in [Stata Journal Issue 8](https://www.stata-journal.com/article.html?article=sbe24_2) (2008) | metan9.sthlp
metan7.ado | Previous version of `metan` as described in [STB-44](https://www.stata.com/products/stb/journals/stb44.html) (1998) | metan7.sthlp
metan_hlp_run.ado | Enables the "click to run" element of the examples in metan.sthlp and elsewhere. A specific implementation of an idea originally developed by Robert Picard. | None; command not for general use

A description of other available Stata meta-analysis commands may be found at  http://www.stata.com/support/faqs/stat/meta.html.

# Repository structure and contents

This GitHub repository is structured as follows:
* **src:** The current "source code" for the `metan` package
* **Examples:** Some examples of advanced uses of the `forestplot` command (part of the `metan` package)
* **Releases:** An archive of formal releases via SSC or the Stata Journal
* **Validation:** A set of do-files and log-files demonstrating validation of results from `metan` v4.0 compared to v3.04 and to Stata 16's `meta` suite, and testing that all options work correctly

# Installation

We currently recommend installation via the Stata Statistical Software Components (SSC) archive.
To see a package description, including contents, type within Stata:

    . ssc describe metan

...and to perform the installation, type:

    . ssc install metan

Package updates via SSC are handled using the `ado update` command; see the built-in Stata documentation.

Alternatively, the package may be installed directly from GitHub, by typing:

    . net describe metan, from("https://raw.githubusercontent.com/UCL/metan/master/src/")

# Usage and documentation

Currently, documentation on usage and options may be found in the documentation files within Stata.  After installation, type in Stata:

    . help metan

# Differences from previous versions of metan

This latest version of `metan` is designed to run under Stata version 11 and upwards, (with the exception of two random-effects models requiring numerical integration, handled via an additional user-contributed Mata function written for Stata version 12).  The previous 2008 version of `metan` (v3.04) remains available within this package under the name `metan9`, and the original 1998 version (v1.86), written for Stata version 7, remains available under the name `metan7`.

The 2020 version of `metan` has been designed with consistency and backwards-compatibility in mind.  However, there are some differences in syntax and behaviour from previous versions.  In particular:
* The preferred, documented syntax is for most options specific to the forest plot (i.e. those that do not affect the results appearing in the Results Window) to be placed within the `forestplot()` option rather than directly to `metan`.  Previously-valid syntax continues to be supported, but with a message printed to the Results Window as a reminder that the documented syntax has changed.
  * Certain options now affect the output in the Results Window as well as the forest plot
  * For details of other, more specific, changes to syntax and behaviour relating to the forest plot, see `forestplot`
* Prediction intervals are no longer displayed with dotted lines if the number of studies is less than three; instead, the interval is simply not displayed at all. Amessage is printed to the Results Window explaining this.
  * If `rflevel()` is different from `olevel()`, then results from `metan` may differ slightly from `metan9` due to a subtle error in the older code, which has been corrected
* Saved results are now always on the interval scale.  For example: if using binary outcome data, `_ES` and `_seES` might contain the individual study *log* Risk Ratios and standard errors, and `r(eff)` and `r(se_eff)` the pooled log Risk Ratio and its standard error.  (The variable name `_selogES` is therefore no longer used.) This has the advantage of consistency across outcome types, and means that saved results may be passed directly to `forestplot` or elsewhere without needing to take logarithms.
  * The names of all returned values saved in `r()` by `metan9` continue to be honoured.  (For those interested: this is done using `return historical`)
* With the Mantel-Haenszel and Peto common-effect models, continuity correction is generally not necessary for pooling, and may in fact lead to increased bias (see, for example, [Bradburn et al 2007](https://doi.org/10.1002/sim.2528)).  Hence, in this version of `metan`, no continuity correction is applied by default with these models unless in extreme cases. Note however that for display purposes a correction *will* be applied (if necessary) to the individual study estimates and weights.
  * The above can be summarized as: by default, `metan` will analyse the available data in the best way possible.  Any of the behaviour described above may be over-ridden by explicit use of options
* If a random-effects model is specified, overall and subgroup-specific Q statistics are based on the inverse-variance common-effect model.  However, *between*-subgroup heterogeneity is tested by considering the dispersal of subgroup-specific pooled effects from the weighted average of subgroup effects under the specified model, as described for example in chapter 19 of Borenstein et al "Introduction to Meta-analysis" (Wiley, 2009).  Note that under the inverse-variance common-effect model, this approach is equivalent to variance partitioning as used in previous versions of `metan`.
  * Similar behaviour is seen when specifying user-defined weights with `wgt()`, except that overall and subgroup-specific Q statistics measure the dispersal of individual effects from the weighted pooled estimate, using standard inverse-variance weights.  This matches the pre-existing behaviour of metan with the Mantel-Haenszel and Peto methods.  (If a random-effects model is specified with `wgt()`, Q statistics are calculated under the equivalent common-effect model.)
  * For the Mantel-Haenszel and Peto common-effect models, between-subgroup heterogeneity is handled as described above for random-effects models

# Differences from Stata 16's "meta" suite

In June 2019, Stata version 16 introduced a suite of built-in meta-analysis commands, with the prefix `meta`.  Forest plots are generated using a new specific Stata graph type (rather than being generated using a combination of `twoway` commands), and there is better interface with other built-in Stata estimation commands.  However, at the time of writing it has somewhat limited functionality, and it is unclear which directions its future development might take.  Currently, `metan` implements many more random-effects models and additional features, and `forestplot` provides far more flexibility to create non-standard plots.  Plus, of course, `metan` is available to users of earlier versions of Stata.

As of November 2020 (Stata version 16.1), Stata 16's `meta` suite is currently unable, amongst other things, to:
* Display the results of analysis under multiple models in the same output, as can be done using `metan`'s `second()` or `model()` options
* Make use of user-specified weights in analysis or in a forest plot
* Show predictive intervals for study subgroups
* Make use of marker label options to display text at the co-ordinates of study effect estimates

Finally, note that there is an important difference in the way that `metan` and Stata 16's `meta` suite report heterogenity statistics with random-effects models.  `metan` views I-squared (and its transformations H and H-squared) as being descriptive of the observed data, and I-squared is therefore derived from Q regardless of the specified model unless specified otherwise with relevant options (see documentation within Stata).  By constrast, Stata 16's `meta` suite reports I-squared based on Q if a common-effect model is specified, or based on tau-squared if a random-effects model is specified.

# Examples

Basic examples of `metan` syntax and usage may be found in the documentation files within Stata.  Some examples of the scope and flexibility of the `forestplot` routine are given below.  Additional examples may be added here in the future.

* [Example 1](Examples/Example1.md): An "umbrella" meta-analysis, where multiple related pooled results are presented in the same forest plot
* [Example 2](Examples/Example2.md): Fixed- and random-effects results displayed simultaneously, with predictive interval.
* [Example 3](Examples/Example3.md): Two sets of pooled results, colour-coded by subgroup
* [Example 4](Examples/Example4.md): Confidence and predictive intervals represented by overlaid shading
* [Example 5](Examples/Example5.md): "Two-panel" subgroup and interaction plot

# Credits and Acknowledgments

Author and maintainer of current version of `metan`:
* David J Fisher, MRC Clinical Trials Unit at University College London

Authors of previous versions of `metan`:
* Ross J Harris, Department of Social Medicine, University of Bristol
* Mike J Bradburn, Centre for Statistics in Medicine, University of Oxford
* Jon J Deeks, Centre for Statistics in Medicine, University of Oxford
* Roger M Harbord, Department of Social Medicine, University of Bristol
* Doug G Altman, Centre for Statistics in Medicine, University of Oxford
* Thomas J Steichen, Department of Social Medicine, University of Bristol
* Jonathan AC Sterne, Department of Social Medicine, University of Bristol

Additional support and advice:
* Vince Wiggins, StataCorp
* Patrick Royston, MRC Clinical Trials Unit at University College London
* Julian PT Higgins, Centre for Medical Statistics, University of Bristol
* Jonathan AC Sterne, Centre for Medical Statistics, University of Bristol

# Contact

David J Fisher

MRC Clinical Trials Unit at University College London<br>
90 High Holborn<br>
London WC1V 6LJ

d.fisher@ucl.ac.uk

+44 20 7670 4646



# riskscore

<!-- badges: start -->
[<img
src="https://img.shields.io/badge/Slack-RValidationHub-blue?style=flat&logo=slack">](https://RValidationHub.slack.com)
[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
<!-- badges: end -->

<br>

A data package for cataloging
[`riskmetric`](https://github.com/pharmaR/riskmetric) results across public
repositories. `WARNING`: Right now, the `riskscore` is in a PoC stage that is not fully operational. 

### Installation

This package is only available on GitHub. You can install the latest version
using the code below:

``` r
# Install {riskscore} package from GitHub
remotes::install_github("pharmaR/riskscore")

```

### Purpose

There are several different use cases that make the concept of `riskscore` 
valuable, including (but not limited to) the following:

- Guide more effective discussion around how to summarize risk
- Helps communicate changes to `riskmetric`'s summarizing algorithm or interpretations of assessment data
- Aids the `riskmetric` dev team in identifying "edge cases" for analysis and code refinement.
- Provides a channel to distribute handy tools for building `riskmetric` result data (ie, mimicking how our process for external packages could serve as a useful template for when comparing to internal or private repos).
- Allows everyone to report `riskscores` scores in terms of "CRAN percentile" instead of just some arbitrary numeric value.
- Establishes a central repository for package scores, which can be used for many applications, like generating badge scores or trending in a package's score over time to measure performance.


### Usage

Right now, the `riskscore` is in a PoC stage that is not fully operational. 
Soon, we'll begin to introduce more functionality to the package. However, in
the meantime, you can access and play with an initial data.frame containing
`riskmetric` scores for all of CRAN. After you've installed `riskscore` from
Github following the instruction above, you can run the following code to
interact with our initial (placeholder) data.frame:

```r
library(riskscore)
data(cran_scored_20250811)
```

This data.frame contains 19,715 observations (one row per package) and 24 
variables, 18 of which are `riskmetric` assessments. To date, the first 6
columns contain package, version, risk_date, risk_version, pkg_score, and 
rank_score. In the future, these columns may change.

Note: to observe how this data.frame was prepared, you can run the following
code to retrieve an R script used to reproduce the results:

```r
file.edit(system.file("reproduce_scores/reproduce_cran_20250811.R", package = "riskscore"))
```

But, in general, the results will be prepared using a 
`pkg_ref(. source = "pkg_cran_remote", repos = c("https://cran.rstudio.com"))`. 
Any assessment / metric that doesn't return a value for this remote source, will
return an `NA` value, which will not hurt the package's score. More on scoring
is available in the `riskmetric` documentation available [here](https://pharmar.github.io/riskmetric/articles/riskmetric.html#score-our-metrics).


<br>

### {riskmetric}’s Approach to Validation <a href='https://pharmar.github.io/riskmetric/'><img src="man/figures/hex-riskmetric-aspconfig.png" align="right" height="172" style="float:right; height:172px;"/></a>

[`riskmetric`](https://github.com/pharmaR/riskmetric) is a framework to quantify
an R package’s “risk of use” by assessing a number of meaningful metrics
designed to evaluate package development best practices, code documentation,
community engagement, and development sustainability. Together, the
[`riskassessment`](https://github.com/pharmaR/riskassessment) app and the
`riskmetric` package aim to provide some context for validation within regulated
industries.

"Validation" can serve as an umbrella for various terms, and admittedly,
companies will diverge on what may be the “correct approach”. `rismetric`
follows the validation philosophy proposed in [this white
paper](https://www.pharmar.org/white-paper/) published by the R Validation Hub.
Disclaimer: The full robustness and reliability of any software may (and likely
will) require deeper inspection by the reviewing party.

*Special Thanks:* Development of `riskscore`, `riskmetric`, and  `riskassessment`
were made possible thanks to the [R Validation
Hub](https://www.pharmar.org/about/), a collaboration to support the adoption of
R within a biopharmaceutical regulatory setting.

<br>

<br>

<br>

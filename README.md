
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
repositories.

### Installation

This package is only available on GitHub. You can install the latest version
using the code below:

``` r
# Install {riskscore} package from GitHub
remotes::install_github("pharmaR/riskscore")

```

### Purpose



### `{riskmetric}`’s Approach to Validation <a
href='https://pharmar.github.io/riskmetric/'><img
src="man/figures/hex-riskmetric-aspconfig.png" align="right" height="172"
style="float:right; height:172px;"/></a>

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

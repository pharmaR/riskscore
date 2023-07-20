
#' Initial Placeholder Data
#'
#' The initial data.frame is generated as a placeholder until a more streamlined
#' process is implemented. It contains the initial run where all of CRAN was
#' scored using riskmetric v0.2.1 on 2023-06-21.
#'
#' @format A `data.frame` containing 19,715 observations
#'   (one row per package) and 23 variables, 18 of which are riskmetric
#'   assessments.
#'
#' \describe{
#'   \item{package}{The name of a R package hosted on CRAN}
#'   \item{version}{The package's version number}
#'   \item{riskmetric_run_date}{The date riskmetric was run}
#'   \item{riskmetric_version}{The version of riskmetric used to derive scores}
#'   \item{pkg_score}{The package's riskmetric score}
#' }
#' All other variables correspond to the riskmetric assessments that exist at
#'   the time of execution. Please refer to riskmetric's documentation for more
#'   info.
#'
#' @return A data.frame
"v0.2.1_cran_20230621"


#' Most recent riskmetric Scores for CRAN & BioConductor
#'
#' A data.frame containing all of CRAN & BioConductor (v3.22) was scored using
#' riskmetric v0.2.6 on 2026-02-24 & R v4.5.1.
#'
#' @format A `data.frame`
#'
#' \describe{
#'   \item{package}{The name of a R package hosted on CRAN}
#'   \item{version}{The package's version number}
#'   \item{riskmetric_run_date}{The date riskmetric was run}
#'   \item{riskmetric_version}{The version of riskmetric used to derive scores}
#'   \item{pkg_score}{The package's riskmetric score}
#' }
#'   All other variables correspond to the riskmetric scores that exist at the
#'   time of execution. Please refer to riskmetric's documentation for more
#'   info.
#'
#' @return A data.frame
"scored_latest"


#' Most recent riskmetric Assessments for CRAN & BioConductor
#'
#' A data.frame containing all of CRAN & BioConductor (v3.22) was assessed using
#' riskmetric v0.2.6 on 2026-02-24 & R v4.5.1.
#'
#' @format A `data.frame`
#'
#' \describe{
#'   \item{package}{The name of a R package hosted on CRAN}
#'   \item{version}{The package's version number}
#'   \item{riskmetric_run_date}{The date riskmetric was run}
#'   \item{riskmetric_version}{The version of riskmetric used to derive scores}
#' }
#'   All other variables correspond to the riskmetric assessments that exist at
#'   the time of execution. Please refer to riskmetric's documentation for more
#'   info.
#'
#' @return A data.frame
"assessed_latest"

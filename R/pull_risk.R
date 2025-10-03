


#' Pull Risk Data
#'
#' A function to pull risk assessment & / or risk scores for all CRAN packages
#'
#' @param source character
#' @param type character, either 'assessed' or 'scored'
#' @param date character, of format 'YYYY-MM-DD',  'YYYYMMDD', or 'latest'
#'
#' @importFrom stringr str_detect
#'
#' @examples
#' pull_risk(type = "scored")
#'
#' @return A data.frame
#' @keywords internal
#'
pull_risk <- function(
  source = "cran", # anything else is unsupported right now
  type = c("assessed", "scored")[1],
  date = "latest" # can also pull by older date
  ) {
  # keep this here for now so we don't have to install arrow or add it to the description
  # @importFrom arrow read_parquet

  if(!source %in% c("cran")) stop("Only 'cran' source is supported right now")
  if(!date %in% c("latest", "2023-06-21", "2025-08-12", "2025-09-28")) {
    stop("Only 'latest', '2023-06-21', '2025-08-12', or '2025-09-28' dates are supported right now")
  }
  if(stringr::str_detect(date, "-")) {
    date <- gsub("-", "", date)
  }
  if(!type %in% c("assessed", "scored")) stop("Only 'assessed' or 'scored' type is supported right now")

  # read in data
  arrow::read_parquet(
    # system.file(
    file.path(
      "data",
      paste0(source, "_", type, "_", date, ".parquet")
      # , package = "riskscore"
  ))
}

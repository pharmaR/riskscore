


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
  # Dynamically determine available dates from data directory
  available_files <- list.files("data", pattern = paste0("^", source, "_", type, "_[0-9]{8}\\.parquet$"))
  available_dates <- gsub(paste0("^", source, "_", type, "_([0-9]{8})\\.parquet$"), "\\1", available_files)
  available_dates_formatted <- c("latest", gsub("([0-9]{4})([0-9]{2})([0-9]{2})", "\\1-\\2-\\3", available_dates))
  if(!date %in% available_dates_formatted) {
    stop(
      sprintf(
        "Only the following dates are supported right now: %s",
        paste(available_dates_formatted, collapse = ", ")
      )
    )
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

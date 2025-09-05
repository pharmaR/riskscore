#############
## code to prepare `cran_20250812` dataset
#
# This run is going to create a dcf file using riskmetric::pkg_metric_export()
# which means the riskmetric results need to stay in list format
# See https://github.com/pharmaR/riskmetric/pull/386
#
#############

# If needed
# utils::install.packages(c("dplyr", "cranlogs", "labelled")

library(dplyr)
library(cranlogs)
library(labelled)

# utils::install.packages("remotes")
# remotes::install_github("pharmar/riskmetric", force = TRUE, ref = "PACKAGES")
packageVersion("riskmetric") # verify ‘0.2.5’
library(riskmetric)

# utils::install.packages("sessioninfo")
si <- sessioninfo::session_info()
si$packages |> dplyr::filter(package == "riskmetric") # verify e5c6da3 is last commit


data(cran_scored_20250812)
object.size(cran_scored_20250812) / 1000000 # 20 MB

# cran_scored_20250812[1:3,] |>
cran_scored_20250812 |>
  dplyr::select(-c(R_version:riskmetric_version)) |>
  riskmetric::pkg_metric_export(
    file = "inst/extdata/cran_scored_20250812.dcf"
  )

# read dcf file - works!
# dcf_in <- riskmetric::pkg_metric_import("data-raw/cran20250812/cran_scored_20250812.dcf")

# read dcf file using base R
# dcf_in2 <- as.data.frame(read.dcf("data-raw/cran20250812/cran_scored_20250812.dcf"))

# Install {riskscore} package from GitHub
remotes::install_github("pharmaR/riskscore", force = TRUE, ref = "dcf-test")
cran_scored <- riskmetric::pkg_metric_import(
  system.file("extdata/cran_scored_20250812.dcf", package = "riskscore")
)
object.size(cran_scored) / 1000000 # 8 MB







################
#
# Create new DCFs for all CRAN packages
#
################

######
# identify last available day of data
date_avail <- cranlogs::cran_downloads("dplyr", "last-day") |> pull(date) #9/03
# date_avail <- as.Date('2025-09-03')

# Get daily downloads for all pkgs from Rstudio CRAN Mirror for the last year
avail_pkgs <- available.packages("https://cran.rstudio.com/src/contrib")[,1]


# Assess the 'dplyr' pkg to identify which metrics are available for 'pkg_cran_remote'
assessed <- c("dplyr") |>
  riskmetric::pkg_ref(source = "pkg_cran_remote", repos = c("https://cran.rstudio.com")) |>
  dplyr::as_tibble() |> # Okay to use this here, since it just to define the metric weights
  riskmetric::pkg_assess()

initial_scoring <- assessed |> riskmetric::pkg_score()

metric_scores <- initial_scoring |>
  dplyr::select(-c(package, version, pkg_ref)) |>
  t()

# riskmetric doesn't pick up certain metrics for pkg_ref(source = "pkg_cran_remote")
# so we'll set their weights to zero here by defining weights
metric_weights <- ifelse(is.na(metric_scores[,1]), 0, 1)


################
# Assess & Score all of CRAN

#
# ---- Strip function ----
#

# Used to strip out the the .recording / 'with_eval_recording' attribute
# since it made our assessment object blow up in size
strip_recording <- function(assessment) {
  # assessment <- pkg_assessment0 # for debugging
  no_recording <-
    lapply(assessment, \(x) {
      # x <- assessment$covr_coverage # for debugging
      attributes(x)$.recording
      structure(
        x,
        .recording = NULL,
        class = setdiff(class(x),
                        "with_eval_recording")
      )
    })
  class(no_recording) <- class(assessment)
  no_recording
}

#
# ---- Incrementally assess & score cran ----
#
incrmt_cran <- function(pkg_names, label) {
  cat("\n\nKicking off batch", label,"\n")
  # pkg_names <- c("dplyr") # for testing / debugging
  # label <- "'TEST'"
  incrmt_ct <- length(pkg_names)
  cat("\n-->", incrmt_ct, "package(s) to process for batch", label,"\n")
  st <- Sys.time()

  # Start assessment
  assessed_cran0 <-
    pkg_names |>
    riskmetric::pkg_ref(source = "pkg_cran_remote", repos = c("https://cran.rstudio.com")) |>
    # dplyr::as_tibble() |> # No, don't use that! Need to keep in list format
    riskmetric::pkg_assess()

  assessed_cran <- assessed_cran0 |>
    strip_recording() # strip .recording attribute

  object.size(assessed_cran0) # Check size - should be smaller
  object.size(assessed_cran) # Check size - should be smaller

  assessed_cran_export <- assessed_cran |>
    riskmetric::pkg_metric_export()

  cat("\n--> batch", label,"Assessed.\n")
  scored_cran <- assessed_cran |>
    riskmetric::pkg_score(weights = metric_weights)

  scored_cran_export <- scored_cran |>
    riskmetric::pkg_metric_export()

  cat("\n--> batch", label,"scored\n")
  end <- Sys.time()
  # Note: this took a well equipped laptop about 10 hours
  cat("\n-->", capture.output(end - st), ".\n")

  #
  # ---- Prepare the datasets for saving ----
  #
  # Save the assessed and scored datasets
  cran_assessed_20250812 <- assessed_cran |>
    dplyr::mutate(
      R_version = getRversion(),
      riskmetric_run_date = date_avail,
      riskmetric_version = packageVersion("riskmetric")
    ) |>
    dplyr::select(-pkg_ref, package, version, everything())
  saveRDS(cran_assessed_20250812, paste0("data-raw/cran20250812/cran_assessed_20250812_",label,".rds"))

  cran_scored_20250812 <- scored_cran |>
    dplyr::mutate(
      R_version = getRversion(),
      riskmetric_run_date = date_avail,
      riskmetric_version = packageVersion("riskmetric")
    ) |>
    dplyr::arrange(pkg_score) |>
    dplyr::select(-pkg_ref, package, version, pkg_score, everything())
  saveRDS(cran_scored_20250812, paste0("data-raw/cran20250812/cran_scored_20250812_",label,".rds"))
  cat("\n--> batch", label, "saved.\n\n")
}
pkgs_ct <- length(avail_pkgs)
bins <- ceiling(pkgs_ct / 8)
# bins <- 3 # for testing / debugging
incrmt_cran(avail_pkgs[1:bins], "01")
incrmt_cran(avail_pkgs[(1*bins+1):(2*bins)], "02")
incrmt_cran(avail_pkgs[(2*bins+1):(3*bins)], "03")
incrmt_cran(avail_pkgs[(3*bins+1):(4*bins)], "04")
incrmt_cran(avail_pkgs[(4*bins+1):(5*bins)], "05")
incrmt_cran(avail_pkgs[(5*bins+1):(6*bins)], "06")
incrmt_cran(avail_pkgs[(6*bins+1):(7*bins)], "07")
incrmt_cran(avail_pkgs[(7*bins+1):pkgs_ct], "08")



# Later, put components back together & save as .rda file
labs <- paste0("0", 1:8)
# .x <- "01" # rm(.x)
cran_assessed_20250812 <- purrr::map(labs, ~
                                       readRDS(paste0("data-raw/cran20250812/cran_assessed_20250812_",.x,".rds"))
) |>
  purrr::reduce(dplyr::bind_rows)
cran_scored_20250812 <- purrr::map(labs, ~
                                     readRDS(paste0("data-raw/cran20250812/cran_scored_20250812_",.x,".rds"))
) |>
  purrr::reduce(dplyr::bind_rows)

# output as rda - uncomment to run
# usethis::use_data(cran_assessed_latest, overwrite = TRUE)
# usethis::use_data(cran_assessed_20250812, overwrite = TRUE)
usethis::use_data(cran_scored_20250812, overwrite = TRUE)
cran_scored_latest <- cran_scored_20250812
usethis::use_data(cran_scored_latest, overwrite = TRUE)



#
# ---- Quantify Size ----
#
# First, compare size to old run

data("cran_scored_20230621")
object.size(cran_scored_20230621) / 1000000 # 5 MB

data("cran_scored_20250812")
object.size(cran_scored_20250812) / 1000000 # 20 MB

nrow(cran_scored_20250812) - nrow(cran_scored_20230621) # 2,782 more pkgs

# Check size of assessments tibble
data("cran_assessed_20250812")
object.size(cran_assessed_20250812) / 1000000000 # 1.5 GB - TOO BIG!

# If strip_recording wasn't performed above, you can do it after the fact too:




# ---- Clean up ----
#

# Let's strip that junk out .recording & any pkg_errors
assessed_cran <- cran_assessed_20250812

# Oh, there's a pkg_error class'd object too, for 1 pkg: "ape"
# assessed_cran$has_news[589]
# assessed_cran$has_news[590] # error


ass_cran <- assessed_cran |>
  dplyr::select(-c(package, version, pkg_ref,
                   R_version, riskmetric_run_date, riskmetric_version))

#
### Test area ###
# Used to strip out the the .recording / 'with_eval_recording' attribute
# since it made our assessment object blow up in size
# strip_recording <- function(assessment) {
#   # assessment <- ass_cran # for debugging
#   these_cols <- colnames(assessment)
#
#   no_record <- lapply(these_cols, \(col_name) {
#     # col_name <- these_cols[2] # for debugging
#     cat("\n\nCol Name =", col_name, "\n")
#     col_vector <- assessment[[col_name]]
#     col_len <- length(col_vector)
#     lite_col_vector <- lapply(1:col_len, function(i) {
#       # i <- 1 # for debugging
#       val <- col_vector[i]
#       # cat("num =", i, ", val =", val[[1]],"\n")
#       # out <-
#         # list(
#           structure(
#             val[[1]],
#             .recording = NULL,
#             class = setdiff(class(val[[1]]), "with_eval_recording")
#           )
#       # )
#       # attributes(out) <- attributes(val) # need this?
#       # out
#     }) #|> unlist(use.names = FALSE) # need this?
#     object.size(assessment[[col_name]])
#     object.size(lite_col_vector)
#     assessment[[col_name]] <<- lite_col_vector
#   })
#   # assessment[["has_new"]] |> attributes()
#   # object.size(no_record) / 1000000000 # 1.5 GB - TOO BIG!
#   class(no_record) <- class(assessment)
#   no_record
#   # assessment
# }


cran_assessed_lite <- ass_cran |>
  dplyr::mutate(dplyr::across(c(has_news), ~ if("pkg_metric_error" %in% class(.x[[1]])) "pkg_metric_error" else .x[[1]])) |>
  strip_recording() |>
  labelled::set_variable_labels(
    .labels = labelled::get_variable_labels(ass_cran)
  )
# object.size(cran_assessed_lite) / 1000000 # Should be smaller. Likely 1/2 the size


cran_assessed_20250812 <- assessed_cran |>
  dplyr::select(c(package, version, pkg_ref,
                  R_version, riskmetric_run_date, riskmetric_version)) |>
  dplyr::bind_cols(cran_assessed_lite) |>
  dplyr::mutate(
    R_version = getRversion(),
    riskmetric_run_date = as.Date("2025-08-12"),
    riskmetric_version = packageVersion("riskmetric")
  )

object.size(cran_assessed_20250812) / 1000000 # 1.5 GB down to 848 MB

# Now store data

usethis::use_data(cran_assessed_20250812, overwrite = TRUE)
cran_assessed_latest <- cran_assessed_20250812
usethis::use_data(cran_assessed_latest, overwrite = TRUE)

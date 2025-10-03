#############
## code to prepare `cran_20250812` dataset

# If needed
# utils::install.packages(c("riskmetric", "dplyr", "cranlogs", "labelled")

library(dplyr)
# library(cranlogs)
library(riskmetric)
library(arrow)
# library(labelled)
# packageVersion("riskmetric") # ‘0.2.5’

######
# identify last available day of data
# date_avail <- cranlogs::cran_downloads("dplyr", "last-day") |> pull(date) #8/12
date_avail <- as.Date('2025-10-01')

# Get daily downloads for all pkgs from Rstudio CRAN Mirror for the last year
options( repos = c(
  CRAN = paste0("https://packagemanager.posit.co/cran/", date_avail)
  # , CRAN = "https://cran.rstudio.com/src/contrib"
  # , BioC = paste0("https://packagemanager.posit.co/bioconductor/", date_avail)
  # , BioC = "https://bioconductor.org/packages/3.17/bioc"
))
avail_pkgs <- available.packages()[,1]


# Assess the 'dplyr' pkg to identify which metrics are available for 'pkg_cran_remote'
assessed <- c("dplyr") %>%
  riskmetric::pkg_ref(source = "pkg_cran_remote", repos = c("https://cran.rstudio.com")) %>%
  dplyr::as_tibble() %>%
  riskmetric::pkg_assess()

initial_scoring <- assessed %>% riskmetric::pkg_score()

metric_scores <- initial_scoring %>%
  dplyr::select(-c(package, version, pkg_ref)) %>%
  t

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
  these_cols <- colnames(assessment)
  lapply(these_cols, \(col_name) {
    cat("\n\nStripping Col:", col_name, "\n")
    col_vector <- assessment[[col_name]]
    col_len <- length(col_vector)
    lite_col_vector <- lapply(1:col_len, function(i) {
      val <- col_vector[i]
      # cat("num =", i, ", val =", val[[1]],"\n")
      out <-
        list(
          structure(
            val[[1]],
            .recording = NULL,
            class = setdiff(class(val[[1]]), "with_eval_recording")
          )
        )
      attributes(out) <- attributes(val)
      out
    }) #|> unlist(use.names = FALSE) # need this?
    assessment[[col_name]] <<- lite_col_vector
  })
  assessment
}

#
# ---- Incrementally assess & score cran ----
#
incrmt_cran <- function(pkg_names, label) {
  cat("\n\nKicking off batch", label,"\n")
  # pkg_names <- c("dplyr") # for testing / debugging
  # label <- "TEST"
  incrmt_ct <- length(pkg_names)
  cat("\n-->", incrmt_ct, "package(s) to process for batch", label,"\n")
  st <- Sys.time()
  assessed_cran0 <-
    pkg_names |>
    riskmetric::pkg_ref(source = "pkg_cran_remote", repos = c("https://cran.rstudio.com")) |>
    dplyr::as_tibble() |>
    riskmetric::pkg_assess()

  assessed_cran <- assessed_cran0 |>
    # remove any 'pkg_metric_errors'
    dplyr::mutate(dplyr::across(c(has_news), ~ if("pkg_metric_error" %in% class(.x[[1]])) "pkg_metric_error" else .x[[1]])) |>
    strip_recording() # strip .recording attribute
  # object.size(assessed_cran0)
  # object.size(assessed_cran)
  cat("\n--> batch", label,"Assessed.\n")

  scored_cran <- assessed_cran0 %>%
    riskmetric::pkg_score(weights = metric_weights)
  cat("\n--> batch", label,"scored\n")

  end <- Sys.time()
  # Note: this took a well equipped laptop about 10 hours
  cat("\n-->", capture.output(end - st), ".\n")

  #
  # ---- Prepare the datasets for saving ----
  #
  # Save the assessed and scored datasets
  cran_assessed_bundle <- assessed_cran %>%
    dplyr::mutate(
      R_version = getRversion(),
      riskmetric_run_date = date_avail,
      riskmetric_version = packageVersion("riskmetric")
    ) %>%
    dplyr::select( package, version, everything(), -pkg_ref)
  # Doesn't work
  # cran_assessed_bundle |>
  #   arrow::as_arrow_table() |>
  #   arrow::write_parquet(
  #     file.path(folder_path, paste0("cran_assessed_bundle_", label, ".parquet")))
  saveRDS(cran_assessed_bundle,
          file.path(folder_path, paste0("cran_assessed_bundle_",label,".rds")))

  cran_scored_bundle <- scored_cran %>%
    dplyr::mutate(
      R_version = getRversion(),
      riskmetric_run_date = date_avail,
      riskmetric_version = packageVersion("riskmetric")
    ) %>%
    dplyr::arrange(pkg_score) %>%
    dplyr::select(package, version, pkg_score, everything(), -pkg_ref)

  # Doesn't work:
  # arrow::write_parquet(
  #   cran_scored_bundle,
  #   file.path(folder_path, paste0("cran_scored_bundle_", label, ".parquet")))
  saveRDS(cran_scored_bundle, #paste0("data-raw/cran20250812/cran_scored_bundle_",label,".rds"))
          file.path(folder_path, paste0("cran_scored_bundle_",label,".rds")))
  cat("\n--> batch '", label, "' saved.\n\n")
}

# create directory to hold the batch files
date_lab <- gsub("-", "", date_avail)
folder_nm <- paste0("cran", date_lab)
folder_path <- file.path("data-raw", folder_nm)
# if(!dir.exists(folder_path)) dir.create(folder_path)

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



# Comment out everything below here if you just want to run the incremental &
# source as a workbench job

# Later, put components back together & save as .rda file
labs <- paste0("0", 1:8)
# .x <- "01" # rm(.x)
cran_assessed_latest <- purrr::map(labs, ~
    folder_path |>
    file.path(paste0("cran_assessed_bundle_",.x,".rds")) |>  # .parquet
      # arrow::read_parquet()
    readRDS()
  ) |>
  purrr::reduce(dplyr::bind_rows)
# Next, scores
cran_scored_latest <- purrr::map(labs, ~
     folder_path |>
     file.path(paste0("cran_scored_bundle_",.x,".rds")) |>  # .parquet
     # arrow::read_parquet()
     readRDS()
) |>
  purrr::reduce(dplyr::bind_rows)

#
# ---- Quantify Size ----
#

object.size(cran_assessed_date) / 1000000 # 866.1 MB
object.size(cran_scored_date) / 1000000 # 9 MB

#
# ---- Output as .rda or .parquet ----
#

# .rda
# name it as "latest"
usethis::use_data(cran_assessed_latest, overwrite = TRUE)
usethis::use_data(cran_scored_latest, overwrite = TRUE)
# name it after the run date first
cran_assessed_20251001 <- cran_assessed_latest
cran_scored_20251001 <- cran_scored_latest
usethis::use_data(cran_assessed_20251001, overwrite = TRUE)
usethis::use_data(cran_scored_20251001, overwrite = TRUE)

# .parquet - Error: NotImplemented: extension
# name it after the run date first
# arrow::write_parquet( cran_assessed_date,
#   file.path("data", paste0("cran_assessed_", date_lab, ".parquet")))
# arrow::write_parquet( cran_scored_date,
#                       file.path("data", paste0("cran_scored_", date_lab, ".parquet")))
# # name it as "latest"
# arrow::write_parquet( cran_scored_date,
#                       file.path("data", "cran_assessed_latest.parquet"))
# arrow::write_parquet( cran_scored_date,
#                       file.path("data", "cran_scored_latest.parquet"))





#
# ---- Quantify Size ----
#
# First, compare size to old run

# data("cran_scored_20230621")
# object.size(cran_scored_20230621) / 1000000 # 5 MB
#
# data("cran_scored_20250812")
# object.size(cran_scored_20250812) / 1000000 # 20 MB
#
# nrow(cran_scored_20250812) - nrow(cran_scored_20230621) # 2,782 more pkgs
#
# # Check size of assessments tibble
# data("cran_assessed_20250812")
# object.size(cran_assessed_20250812) / 1000000000 # 1.5 GB - TOO BIG!

# If strip_recording wasn't performed above, you can do it after the fact too:




# # ---- Clean up ----
# #
#
# # Let's strip that junk out .recording & any pkg_errors
# assessed_cran <- cran_assessed_20250812
#
# # Oh, there's a pkg_error class'd object too, for 1 pkg: "ape"
# # assessed_cran$has_news[589]
# # assessed_cran$has_news[590] # error
#
#
# ass_cran <- assessed_cran |>
#   dplyr::select(-c(package, version, pkg_ref,
#                    R_version, riskmetric_run_date, riskmetric_version))
#
# #
# ### Test area ###
# # Used to strip out the the .recording / 'with_eval_recording' attribute
# # since it made our assessment object blow up in size
# # strip_recording <- function(assessment) {
# #   # assessment <- ass_cran # for debugging
# #   these_cols <- colnames(assessment)
# #
# #   no_record <- lapply(these_cols, \(col_name) {
# #     # col_name <- these_cols[2] # for debugging
# #     cat("\n\nCol Name =", col_name, "\n")
# #     col_vector <- assessment[[col_name]]
# #     col_len <- length(col_vector)
# #     lite_col_vector <- lapply(1:col_len, function(i) {
# #       # i <- 1 # for debugging
# #       val <- col_vector[i]
# #       # cat("num =", i, ", val =", val[[1]],"\n")
# #       # out <-
# #         # list(
# #           structure(
# #             val[[1]],
# #             .recording = NULL,
# #             class = setdiff(class(val[[1]]), "with_eval_recording")
# #           )
# #       # )
# #       # attributes(out) <- attributes(val) # need this?
# #       # out
# #     }) #|> unlist(use.names = FALSE) # need this?
# #     object.size(assessment[[col_name]])
# #     object.size(lite_col_vector)
# #     assessment[[col_name]] <<- lite_col_vector
# #   })
# #   # assessment[["has_new"]] |> attributes()
# #   # object.size(no_record) / 1000000000 # 1.5 GB - TOO BIG!
# #   class(no_record) <- class(assessment)
# #   no_record
# #   # assessment
# # }
#
#
# cran_assessed_lite <- ass_cran |>
#   dplyr::mutate(dplyr::across(c(has_news), ~ if("pkg_metric_error" %in% class(.x[[1]])) "pkg_metric_error" else .x[[1]])) |>
#   strip_recording() |>
#   labelled::set_variable_labels(
#     .labels = labelled::get_variable_labels(ass_cran)
#   )
# # object.size(cran_assessed_lite) / 1000000 # Should be smaller. Likely 1/2 the size
#
#
# cran_assessed_20250812 <- assessed_cran |>
#   dplyr::select(c(package, version, pkg_ref,
#                   R_version, riskmetric_run_date, riskmetric_version)) |>
#   dplyr::bind_cols(cran_assessed_lite) |>
#   dplyr::mutate(
#     R_version = getRversion(),
#     riskmetric_run_date = as.Date("2025-08-12"),
#     riskmetric_version = packageVersion("riskmetric")
#   )
#
# object.size(cran_assessed_20250812) / 1000000 # 1.5 GB down to 848 MB
#
# # Now store data
#
# usethis::use_data(cran_assessed_20250812, overwrite = TRUE)
# cran_assessed_latest <- cran_assessed_20250812
# usethis::use_data(cran_assessed_latest, overwrite = TRUE)

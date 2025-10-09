#############
## code to prepare `cran_20250812` dataset

# If needed
# utils::install.packages(c("riskmetric", "dplyr", "cranlogs", "labelled")

library(dplyr)
# library(cranlogs)
library(riskmetric)
# library(arrow)
# library(labelled)
# packageVersion("riskmetric") # ‘0.2.5’

######
# identify last available day of data
# date_avail <- cranlogs::cran_downloads("dplyr", "last-day") |> pull(date) #8/12
date_avail <- as.Date('2025-10-01')

# Get daily downloads for all pkgs from Rstudio CRAN Mirror for the last year
options( repos = c(
  CRAN = paste0("https://packagemanager.posit.co/cran/", date_avail)
  # , CRAN = "https://cran.rstudio.com/src/contrib" # old way
  # , BioC = paste0("https://packagemanager.posit.co/bioconductor/", date_avail) # doesn't work
  # , BioC = "https://bioconductor.org/packages/3.17/bioc"
  , BioC = "https://bioconductor.org/packages/3.21/bioc"
))
avail_pkgs <- available.packages() |> as.data.frame()
table(avail_pkgs$Repository)
cran_pkgs <- avail_pkgs[stringr::str_detect(avail_pkgs$Repository, "cran"), ]
bioc_pkgs <- avail_pkgs[stringr::str_detect(avail_pkgs$Repository, "bioc"), ]

cran_pkgs |> nrow()
bioc_pkgs |> nrow()
avail_pkgs |> nrow() # total

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
  cols_ <- colnames(assessment)
  # Uncomment this for next time:
  # these_cols <- cols_[!cols_ %in% c("package", "version", "pkg_ref",
  #       "R_version", "riskmetric_run_date", "riskmetric_version")] # new
  these_cols <- cols_
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

# create directory to hold the batch files
date_lab <- gsub("-", "", date_avail)
folder_nm <- paste0("repos", date_lab)
folder_path <- file.path("data-raw", folder_nm)
# if(!dir.exists(folder_path)) dir.create(folder_path)

incrmt_repo <- function(pkg_names, repo = c('cran', 'bioc')[1], label) {
  # repo = c('cran')
  # pkg_names <- c("dplyr") # for testing / debugging
  # label <- "TEST"

  cat("\n\nKicking off batch", label,"for", repo,"repo.\n")
  incrmt_ct <- length(pkg_names)
  cat("\n-->", incrmt_ct, "package(s) to process for", repo, "batch", label,"\n")
  st <- Sys.time()
  assessed_repo0 <-
    pkg_names |>
    riskmetric::pkg_ref(source = paste("pkg", repo, "remote", sep = "_")) |>
    dplyr::as_tibble() |>
    riskmetric::pkg_assess()

  assessed_repo <- assessed_repo0 |>
    # remove any 'pkg_metric_errors'
    dplyr::mutate(dplyr::across(c(has_news), ~ if("pkg_metric_error" %in% class(.x[[1]])) "pkg_metric_error" else .x[[1]])) |>
    strip_recording() # strip .recording attribute
  # object.size(assessed_repo0)
  # object.size(assessed_repo)
  cat("\n--> batch", label,"Assessed.\n")

  scored_repo <- assessed_repo0 %>%
    riskmetric::pkg_score(weights = metric_weights)
  cat("\n--> batch", label,"scored\n")

  end <- Sys.time()
  # Note: this took a well equipped laptop about 10 hours
  cat("\n-->", capture.output(end - st), ".\n")

  #
  # ---- Prepare the datasets for saving ----
  #
  # Save the assessed and scored datasets
  repo_assessed_bundle <- assessed_repo %>%
    dplyr::mutate(
      R_version = getRversion(),
      riskmetric_run_date = date_avail,
      riskmetric_version = packageVersion("riskmetric")
    ) %>%
    dplyr::select( package, version, everything())#, -pkg_ref) # ran w/o pkg_ref, but should keep it next time
  # Doesn't work
  # repo_assessed_bundle |>
  #   arrow::as_arrow_table() |>
  #   arrow::write_parquet(
  #     file.path(folder_path, paste0(repo, "_assessed_bundle_", label, ".parquet")))
  saveRDS(repo_assessed_bundle,
          file.path(folder_path, paste0(repo, "_assessed_bundle_",label,".rds")))

  repo_scored_bundle <- scored_repo %>%
    dplyr::mutate(
      R_version = getRversion(),
      riskmetric_run_date = date_avail,
      riskmetric_version = packageVersion("riskmetric")
    ) %>%
    dplyr::arrange(pkg_score) %>%
    dplyr::select(package, version, pkg_score, everything())#, -pkg_ref) # ran w/o pkg_ref, but should keep it next time

  # Doesn't work:
  # arrow::write_parquet(
  #   repo_scored_bundle,
  #   file.path(folder_path, paste0(repo, "_scored_bundle_", label, ".parquet")))
  saveRDS(repo_scored_bundle, #paste0("data-raw/cran20250812/cran_scored_bundle_",label,".rds"))
          file.path(folder_path, paste0(repo, "_scored_bundle_",label,".rds")))
  cat("\n-->", repo,"batch '", label, "' saved.\n\n")
}



#
# ---- CRAN Pkgs ----
#

cranny <- cran_pkgs$Package
pkgs_ct <- length(cranny)
bins <- ceiling(pkgs_ct / 8)
# bins <- 3 # for testing / debugging
incrmt_repo(cranny[1:bins], "01")
incrmt_repo(cranny[(1*bins+1):(2*bins)], "02")
incrmt_repo(cranny[(2*bins+1):(3*bins)], "03")
incrmt_repo(cranny[(3*bins+1):(4*bins)], "04")
incrmt_repo(cranny[(4*bins+1):(5*bins)], "05")
incrmt_repo(cranny[(5*bins+1):(6*bins)], "06")
incrmt_repo(cranny[(6*bins+1):(7*bins)], "07")
incrmt_repo(cranny[(7*bins+1):pkgs_ct], "08")





#
# ---- Bioconductor Pkgs ----
#

# Gives vector of pkgs that were in available.packages() but missing assessments
# source("data-raw/cran20251001/missing_output.R")

bio <- bioc_pkgs$Package[bioc_pkgs$Package != "alpine"] # a problem child
pkgs_ct <- length(bio)
bins <- ceiling(pkgs_ct / 8)
# bins <- 54 # for testing / debugging
# bio[54] # was a problem child?
# incrmt_repo(bio[54], "bioc", "01")


# run for real
incrmt_repo(bio[1:bins], "bioc", "01")
incrmt_repo(bio[(1*bins+1):(2*bins)], "bioc", "02")
incrmt_repo(bio[(2*bins+1):(3*bins)], "bioc", "03")
incrmt_repo(bio[(3*bins+1):(4*bins)], "bioc", "04")
incrmt_repo(bio[(4*bins+1):(5*bins)], "bioc", "05")
incrmt_repo(bio[(5*bins+1):(6*bins)], "bioc", "06")
incrmt_repo(bio[(6*bins+1):(7*bins)], "bioc", "07")
incrmt_repo(bio[(7*bins+1):pkgs_ct], "bioc", "08")

# Comment out everything below here if you just want to run the incremental &
# source as a workbench job



# Testing Missing Packages, if any

# source("dev/missing_bioc.R") # get missings
# pkgs_ct <- length(missing_bioc)
# bins <- ceiling(pkgs_ct / 8)
#
# incrmt_repo(pkg_names = runb, repo = "bioc", label = "01_delete")



# Later, put components back together & save as .rda file
# how many files are there that end in "assessed_bundle_XX.rds"?

repo_united <- function(repo){
  file_ct <- list.files(folder_path, pattern = paste0(repo, "_assessed_bundle_")) |> length()
  labs <- paste0("0", 1:file_ct)
  # .x <- "01" # rm(.x)
  repo_assessed_latest <- purrr::map(labs, ~
       folder_path |>
       file.path(paste0(repo, "_assessed_bundle_",.x,".rds")) |>  # .parquet
       # arrow::read_parquet()
       readRDS()
  ) |>
    purrr::reduce(dplyr::bind_rows)|>
    dplyr::mutate(repo_src = repo)
  # Next, scores
  repo_scored_latest <- purrr::map(labs, ~
       folder_path |>
       file.path(paste0(repo, "_scored_bundle_",.x,".rds")) |>  # .parquet
       # arrow::read_parquet()
       readRDS()
  ) |>
    purrr::reduce(dplyr::bind_rows) |>
    dplyr::mutate(repo_src = repo)

  list(assessed = repo_assessed_latest, scored = repo_scored_latest)
}
cran_ <- repo_united("cran")
bioc_ <- repo_united("bioc")
bioc_assessed <- bioc_$assessed |> dplyr::mutate(repo_src_ver = "3.21") # Manual for now
bioc_scored <- bioc_$scored |> dplyr::mutate(repo_src_ver = "3.21") # Manual for now
# runb %in% bioc_assessed$package # check for missing

assessed_latest <- cran_$assessed |>
  dplyr::bind_rows(bioc_assessed)

scored_latest <- cran_$scored |>
  dplyr::bind_rows(bioc_scored)


#
# ---- Quantify Size ----
#

# object.size(assessed_latest) / 1000000 # 956.2 MB
# object.size(scored_latest)   / 1000000   # 10.3 MB

#
# ---- Output as .rda or .parquet ----
#

# .rda
# name it as "latest"
usethis::use_data(scored_latest, overwrite = TRUE)
usethis::use_data(assessed_latest, overwrite = TRUE)

# name it after the run date first
scored_20251001 <- scored_latest
assessed_20251001 <- assessed_latest
usethis::use_data(scored_20251001, overwrite = TRUE)
usethis::use_data(assessed_20251001, overwrite = TRUE)


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

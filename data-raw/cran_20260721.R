#############
## code to prepare `cran_YYYYMMDD` dataset

# If needed
# utils::install.packages(c("riskmetric", "dplyr", "cranlogs", "labelled")
# devtools::install_github("rasmusab/beepr")

library(dplyr)
# library(beeper)
# library(cranlogs)
# utils::install.packages("riskmetric") # v0.2.6
library(riskmetric)
# library(arrow)
# library(labelled)
# packageVersion("riskmetric") # ‘0.2.6’

######
# identify last available day of data
# date_avail <- cranlogs::cran_downloads("dplyr", "last-day") |> pull(date) #2/24
date_avail <- as.Date('2026-07-21')

# Get daily downloads for all pkgs from Rstudio CRAN Mirror for the last year
bioc_ver <- "3.22"
options( repos = c(
  # CRAN = paste0("https://packagemanager.posit.co/cran/", date_avail)
  # , BioCsoft      = paste0("https://bioconductor.org/packages/", bioc_ver, "/bioc")
   BioCann       = paste0("https://bioconductor.org/packages/", bioc_ver, "/data/annotation")
  , BioCexp       = paste0("https://bioconductor.org/packages/", bioc_ver, "/data/experiment")
  , BioCworkflows = paste0("https://bioconductor.org/packages/", bioc_ver, "/workflows")
))
# options('repos')
avail_pkgs <- available.packages() |> as.data.frame()
table(avail_pkgs$Repository)
cran_pkgs <- avail_pkgs[stringr::str_detect(avail_pkgs$Repository, "cran"), ]
# Match all four Bioconductor subrepos (software / annotation / experiment /
# workflows) — their URLs share "bioconductor" but not "bioc".
bioc_pkgs <- avail_pkgs[stringr::str_detect(avail_pkgs$Repository, "bioconductor"), ]

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
    # cat("\n\nStripping Col:", col_name, "\n")
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
  # bin_num <- 66 # for testing / debugging
  # pkg_names <- bio[bin_num] # 'AneuFinder' was a problem child?
  # repo = c('bioc')
  # pkg_names <- c("AneuFinder") # for testing / debugging
  # label <- as.character(bin_num)

  cat("\n\nKicking off batch", label,"for", repo,"repo.\n")
  incrmt_ct <- length(pkg_names)
  cat("\n-->", incrmt_ct, "package(s) to process for", repo, "batch", label,"\n")
  st <- Sys.time()
  ass_repo00 <-
    pkg_names |>
    riskmetric::pkg_ref(source = paste("pkg", repo, "remote", sep = "_"))

  # Drop any refs that resolved to `pkg_missing` (e.g. packages not found in
  # the configured Bioconductor sub-repos). riskmetric's
  # `as_tibble.list_of_pkg_ref` calls
  #   vapply(x, function(xi) as.character(xi$version), character(1L))
  # which errors with "values must be length 1, but FUN(X[[1]]) result is
  # length 0" when a `pkg_missing` ref is present, since missing refs have no
  # resolvable version. Filter them out (with a message) before assessing.
  is_missing <- vapply(ass_repo00, function(xi) inherits(xi, "pkg_missing"),
                       logical(1L))
  if (any(is_missing)) {
    missing_names <- vapply(ass_repo00[is_missing], "[[", character(1L), "name")
    cat("\n--> Dropping", sum(is_missing),
        "package(s) not found in", repo, "repo:",
        paste(missing_names, collapse = ", "), "\n")
    keep_idx <- which(!is_missing)
    ass_repo00 <- vctrs::vec_slice(ass_repo00, keep_idx)
  }
  if (length(ass_repo00) == 0) {
    cat("\n--> No resolvable packages in batch", label, "- skipping.\n")
    return(invisible(NULL))
  }

  assessed_repo0 <-
    ass_repo00 |>
    # as.data.frame()
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

# cranny <- cran_pkgs$Package
# pkgs_ct <- length(cranny)
# bins <- ceiling(pkgs_ct / 8)
# # bins <- 3 # for testing / debugging
# incrmt_repo(cranny[1:bins], "cran", "01")
# incrmt_repo(cranny[(1*bins+1):(2*bins)], "cran", "02")
# incrmt_repo(cranny[(2*bins+1):(3*bins)], "cran", "03")
# incrmt_repo(cranny[(3*bins+1):(4*bins)], "cran", "04")
# incrmt_repo(cranny[(4*bins+1):(5*bins)], "cran", "05")
# incrmt_repo(cranny[(5*bins+1):(6*bins)], "cran", "06")
# incrmt_repo(cranny[(6*bins+1):(7*bins)], "cran", "07")
# incrmt_repo(cranny[(7*bins+1):pkgs_ct], "cran", "08")





#
# ---- Bioconductor Pkgs ----
#

# Gives vector of pkgs that were in available.packages() but missing assessments
# source("data-raw/cran20251001/missing_output.R")

bio <- bioc_pkgs$Package
# Remove problem pkgs:
# bio <- bioc_pkgs$Package[!(bioc_pkgs$Package %in%
#                              c("biodbChebi", "BiRewire", # bundle 1
#                                "consensusDE", "DEP", # bundle 2
#                                "interactiveDisplay", "interactiveDisplayBase", "linkSet", # bundle 4
#                                "MetaNeighbor", "MineICA", "motifbreakR", "netZooR",# bundle 5
#                                "Organism.dplyr", "phenomis", "RcisTarget", # bundle 6
#                                "RgnTX", "RiboProfiling", "rRDP", # bundle 7
#                                "Streamer", "Ularcirc"  # bundle 8
#                                )
#                            )]
# bio <- bioc_pkgs$Package[!(bioc_pkgs$Package %in%
#                              c("adme16cod" #"BiRewire", # bundle 1
                             #   "consensusDE", "DEP", # bundle 2
                             #   "interactiveDisplay", "interactiveDisplayBase", # bundle 3
                             # )
# )]


# ref_2 <- riskmetric::pkg_ref("adme16cod", source = "pkg_bioc_remote")

pkgs_ct <- length(bio)
bins <- ceiling(pkgs_ct / 3)

# Find the bad eggs
# bin <- 1
# bundle <- 1
# bio_run <- bio[((bundle-1)*bins+1):(bundle*bins)]
# bio_run[bin]
# bin <- 220 # for testing / debugging
# bio[bin] # was a problem child
# # incrmt_repo(bio[bin], "bioc", "01")

# run for real
# incrmt_repo(bio[1:bins], "bioc", "01")
# incrmt_repo(bio[(1*bins+1):(2*bins)], "bioc", "02")
# incrmt_repo(bio[(2*bins+1):(3*bins)], "bioc", "03")
# incrmt_repo(bio[(3*bins+1):(4*bins)], "bioc", "04")
# incrmt_repo(bio[(4*bins+1):(5*bins)], "bioc", "05")
# incrmt_repo(bio[(5*bins+1):(6*bins)], "bioc", "06")
# incrmt_repo(bio[(6*bins+1):(7*bins)], "bioc", "07")
# incrmt_repo(bio[(7*bins+1):pkgs_ct], "bioc", "08")


incrmt_repo(bio[1:bins], "bioc", "09") # test

incrmt_repo(bio[1:bins], "bioc", "09")
incrmt_repo(bio[(1*bins+1):(2*bins)], "bioc", "10")
incrmt_repo(bio[(2*bins+1):(3*bins)], "bioc", "11")

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

# Harmonize per-column attributes (e.g. `label`) across a list of bundles so
# that `dplyr::bind_rows()` / vctrs doesn't error out with
# "Some attributes are incompatible" when one bundle happens to be missing
# an attribute (like `label`) that other bundles carry on `pkg_score`
# classed columns.
harmonize_bundle_attrs <- function(bundles) {
  if (length(bundles) <= 1) return(bundles)
  all_cols <- unique(unlist(lapply(bundles, names)))
  # For each column, build the "canonical" attribute set by unioning
  # non-null attributes seen across bundles (first non-null wins per attr name).
  canonical_attrs <- lapply(all_cols, function(col_nm) {
    attr_list <- list()
    for (b in bundles) {
      if (!col_nm %in% names(b)) next
      a <- attributes(b[[col_nm]])
      if (is.null(a)) next
      for (nm in names(a)) {
        if (is.null(attr_list[[nm]])) attr_list[[nm]] <- a[[nm]]
      }
    }
    attr_list
  })
  names(canonical_attrs) <- all_cols
  lapply(bundles, function(b) {
    for (col_nm in names(b)) {
      canon <- canonical_attrs[[col_nm]]
      if (length(canon) == 0) next
      cur <- attributes(b[[col_nm]])
      if (identical(cur, canon)) next
      # Preserve existing attrs, fill in any missing ones from canonical.
      merged <- cur
      for (nm in names(canon)) {
        if (is.null(merged[[nm]])) merged[[nm]] <- canon[[nm]]
      }
      attributes(b[[col_nm]]) <- merged
    }
    b
  })
}

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
    harmonize_bundle_attrs() |>
    # purrr::reduce(
      dplyr::bind_rows()|>
    dplyr::mutate(repo_src = repo)
  # Next, scores
  repo_scored_latest <- purrr::map(labs, ~
      # .x = "03"
       folder_path |>
       file.path(paste0(repo, "_scored_bundle_",.x,".rds")) |>  # .parquet
       # arrow::read_parquet()
       readRDS()
  ) |>
    harmonize_bundle_attrs() |>
    # purrr::reduce(
      dplyr::bind_rows() |>
    dplyr::mutate(repo_src = repo)

  list(assessed = repo_assessed_latest, scored = repo_scored_latest)
}
cran_ <- repo_united(repo = "cran")
bioc_ <- repo_united(repo = "bioc")
bioc_assessed <- bioc_$assessed |> dplyr::mutate(repo_src_ver = "3.22") # Manual for now
bioc_scored <- bioc_$scored |> dplyr::mutate(repo_src_ver = "3.22") # Manual for now
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

# name it after the run date too - nope. No need
# scored_20260224 <- scored_latest
# assessed_20260224 <- assessed_latest
# usethis::use_data(scored_20260224, overwrite = TRUE)
# usethis::use_data(assessed_20260224, overwrite = TRUE)


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








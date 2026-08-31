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
# packageVersion("riskmetric") # ‘0.2.7’

######
# identify last available day of data
# date_avail <- cranlogs::cran_downloads("dplyr", "last-day") |> pull(date) #2/24
date_avail <- as.Date('2026-08-15')

# Get daily downloads for all pkgs from Rstudio CRAN Mirror for the last year
bioc_ver <- "3.22"
options( repos = c(
  CRAN = paste0("https://packagemanager.posit.co/cran/", date_avail),
  BioCsoft      = paste0("https://bioconductor.org/packages/", bioc_ver, "/bioc"),
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
if(!dir.exists(folder_path)) dir.create(folder_path)

incrmt_repo <- function(pkg_names, repo = c('cran', 'bioc')[1], label,
                        keep_missing = TRUE) {
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

  # When `pkg_names` has length 1, riskmetric::pkg_ref() returns a single
  # `pkg_ref` (an environment), not a `list_of_pkg_ref`. Normalize so the
  # rest of the function can uniformly iterate over one-or-more refs.
  if (!inherits(ass_repo00, "list_of_pkg_ref")) {
    ass_repo00 <- vctrs::new_list_of(list(ass_repo00), ptype = list(),
                                     class = "list_of_pkg_ref")
  }

  # Detect refs that will crash `as_tibble.list_of_pkg_ref` — that function
  # runs
  #   vapply(x, function(xi) as.character(xi$version), character(1L))
  # and errors with "values must be length 1, but FUN(X[[1]]) result is
  # length 0" whenever a ref has no resolvable version. Two cases produce
  # this:
  #   1. `pkg_missing` refs (riskmetric couldn't resolve the package name
  #      to any repo — e.g. name not on CRAN and not in the release bioc
  #      software sub-repo).
  #   2. `pkg_bioc_remote` refs for packages that live in a non-software
  #      Bioc sub-repo (annotation / experiment / workflows). These pass
  #      riskmetric's bioc availability check (their Repository URL is a
  #      subpath of a Bioc mirror) but `pkg_bioc()` looks up the version
  #      against the release *software* PACKAGES file only, so
  #      `xi$version` returns `character(0)`.
  # Both cases are equally "missing" for our purposes.
  is_missing <- vapply(ass_repo00, function(xi) {
    if (inherits(xi, "pkg_missing")) return(TRUE)
    v <- tryCatch(xi$version, error = function(e) character(0))
    length(v) == 0L || (length(v) == 1L && is.na(v))
  }, logical(1L))
  missing_names <- character(0)
  if (any(is_missing)) {
    dropped_names <- vapply(ass_repo00[is_missing], "[[",
                            character(1L), "name")
    action <- if (isTRUE(keep_missing)) "Flagging" else "Dropping"
    cat("\n-->", action, sum(is_missing),
        "package(s) with no resolvable version in", repo, "repo:",
        paste(dropped_names, collapse = ", "), "\n")
    keep_idx <- which(!is_missing)
    ass_repo00 <- vctrs::vec_slice(ass_repo00, keep_idx)
    if (isTRUE(keep_missing)) missing_names <- dropped_names
  }

  # If every ref in this batch is pkg_missing, skip the assess/score pipeline
  # entirely (there's nothing riskmetric can process). Missing packages will
  # be re-attached as flagged rows in `repo_united()` if `keep_missing`
  # requested it.
  all_missing <- length(ass_repo00) == 0

  if (!all_missing) {
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
  } else {
    cat("\n--> All packages in batch", label, "were pkg_missing;",
        "skipping assess/score.\n")
    assessed_repo <- NULL
    scored_repo   <- NULL
  }

  end <- Sys.time()
  # Note: this took a well equipped laptop about 10 hours
  cat("\n-->", capture.output(end - st), ".\n")

  #
  # ---- Prepare the datasets for saving ----
  #
  # Bundle files hold ONLY resolvable packages, with the schema riskmetric
  # produces. Missing packages (if any and if keep_missing = TRUE) are saved
  # to a companion `_missing_<label>.rds` file as a plain character vector.
  # `repo_united()` later appends flagged rows for these missing packages to
  # the fully-combined bundle, where all column types (`pkg_score` S3 class,
  # list-columns from strip_recording(), etc.) are already established —
  # sidestepping the ptype-fallback issues that occur when appending inside
  # each batch.
  if (!all_missing) {
    repo_assessed_bundle <- assessed_repo %>%
      dplyr::mutate(
        R_version = getRversion(),
        riskmetric_run_date = date_avail,
        riskmetric_version = packageVersion("riskmetric")
      ) %>%
      dplyr::select( package, version, everything())#, -pkg_ref) # ran w/o pkg_ref, but should keep it next time
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
    saveRDS(repo_scored_bundle,
            file.path(folder_path, paste0(repo, "_scored_bundle_",label,".rds")))
  }

  # Persist the list of missing package names for this batch so repo_united()
  # can pick them up. Only when keep_missing = TRUE.
  if (isTRUE(keep_missing) && length(missing_names) > 0) {
    saveRDS(missing_names,
            file.path(folder_path,
                      paste0(repo, "_missing_", label, ".rds")))
  }

  cat("\n-->", repo,"batch '", label, "' saved.\n\n")
}



#
# ---- CRAN Pkgs ----
#

cranny <- cran_pkgs$Package
pkgs_ct <- length(cranny)
bins <- ceiling(pkgs_ct / 8)
# bins <- 3 # for testing / debugging
incrmt_repo(cranny[1:bins], "cran", "01")
incrmt_repo(cranny[(1*bins+1):(2*bins)], "cran", "02")
incrmt_repo(cranny[(2*bins+1):(3*bins)], "cran", "03")
incrmt_repo(cranny[(3*bins+1):(4*bins)], "cran", "04")
incrmt_repo(cranny[(4*bins+1):(5*bins)], "cran", "05")
incrmt_repo(cranny[(5*bins+1):(6*bins)], "cran", "06")
incrmt_repo(cranny[(6*bins+1):(7*bins)], "cran", "07")
incrmt_repo(cranny[(7*bins+1):pkgs_ct], "cran", "08")





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
bins <- ceiling(pkgs_ct / 8)

# Find the bad eggs
# bin <- 1
# bundle <- 1
# bio_run <- bio[((bundle-1)*bins+1):(bundle*bins)]
# bio_run[bin]
# bin <- 220 # for testing / debugging
# bio[bin] # was a problem child
# # incrmt_repo(bio[bin], "bioc", "01")

# run for real
incrmt_repo(bio[1:bins], "bioc", "01")
incrmt_repo(bio[(1*bins+1):(2*bins)], "bioc", "02")
incrmt_repo(bio[(2*bins+1):(3*bins)], "bioc", "03")
incrmt_repo(bio[(3*bins+1):(4*bins)], "bioc", "04")
incrmt_repo(bio[(4*bins+1):(5*bins)], "bioc", "05")
incrmt_repo(bio[(5*bins+1):(6*bins)], "bioc", "06")
incrmt_repo(bio[(6*bins+1):(7*bins)], "bioc", "07")
incrmt_repo(bio[(7*bins+1):pkgs_ct], "bioc", "08")


# incrmt_repo(bio[1], "bioc", "12") # test
# Note: ran these separately to accomodate these BioC repo URLs after the fact:
#   BioCann       = paste0("https://bioconductor.org/packages/", bioc_ver, "/data/annotation")
# , BioCexp       = paste0("https://bioconductor.org/packages/", bioc_ver, "/data/experiment")
# , BioCworkflows = paste0("https://bioconductor.org/packages/", bioc_ver, "/workflows")
#
# Also, added an argument 'keep_missing' = TRUE by default that tells R
# whether you want to hang onto those missing ref pkgs or to just drop them
# from the RDS file. Note: If dropped, then val.pipeline may say they are missing.
# incrmt_repo(bio[1:bins], "bioc", "09")
# incrmt_repo(bio[(1*bins+1):(2*bins)], "bioc", "10")
# incrmt_repo(bio[(2*bins+1):(3*bins)], "bioc", "11")

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
  # Determine batch labels from the assessed bundle filenames rather than
  # assuming 01..N are all present — a batch where every package was
  # pkg_missing produces no bundle file, so labels can have gaps.
  assessed_files <- list.files(
    folder_path,
    pattern = paste0("^", repo, "_assessed_bundle_.*\\.rds$"),
    full.names = FALSE
  )
  labs <- sub(paste0("^", repo, "_assessed_bundle_(.*)\\.rds$"),
              "\\1", assessed_files)
  labs <- sort(labs)
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

  # Attach pkg_missing = FALSE to every row that came from a real bundle;
  # append flagged rows for any packages saved to `<repo>_missing_*.rds`
  # by incrmt_repo() (only present when keep_missing = TRUE was used).
  # Building the flagged rows here — against the fully-combined bundle —
  # lets us use vctrs::vec_init() with each column's real prototype, so
  # class-decorated columns like `pkg_score` don't get downgraded.
  repo_assessed_latest <- append_missing_bundle_rows(
    repo_assessed_latest, repo, "assessed"
  )
  repo_scored_latest <- append_missing_bundle_rows(
    repo_scored_latest, repo, "scored"
  )

  list(assessed = repo_assessed_latest, scored = repo_scored_latest)
}

# Load `<repo>_missing_<label>.rds` files (character vectors of package
# names) that incrmt_repo() saved for each batch with unresolvable
# packages, and append one flagged row per name to `bundle`. `bundle` is
# assumed to be the fully-combined output of all resolvable batches, so
# every column already has its final S3 class / attributes and vec_init()
# will preserve them on the NA-filled rows.
append_missing_bundle_rows <- function(bundle, repo, kind) {
  # Always emit the pkg_missing flag column; FALSE for existing rows.
  if (!"pkg_missing" %in% names(bundle)) {
    bundle <- dplyr::mutate(bundle, pkg_missing = FALSE)
  }
  miss_files <- list.files(folder_path,
                           pattern = paste0("^", repo, "_missing_.*\\.rds$"),
                           full.names = TRUE)
  if (length(miss_files) == 0L) return(bundle)
  missing_names <- unique(unlist(lapply(miss_files, readRDS),
                                 use.names = FALSE))
  if (length(missing_names) == 0L) return(bundle)

  n_miss <- length(missing_names)
  # Build a row-set with the same schema as `bundle` using each column's
  # prototype so classes/attributes are preserved.
  missing_rows <- lapply(names(bundle), function(nm) {
    vctrs::vec_init(bundle[[nm]], n = n_miss)
  })
  names(missing_rows) <- names(bundle)
  missing_rows <- tibble::as_tibble(missing_rows)

  # Overwrite identifier / metadata columns with real values.
  if (inherits(bundle$package, "list")) {
    missing_rows$package <- lapply(missing_names, identity)
  } else {
    missing_rows$package <- missing_names
  }
  if ("version" %in% names(missing_rows)) {
    if (inherits(bundle$version, "list")) {
      missing_rows$version <- replicate(n_miss, NA_character_,
                                        simplify = FALSE)
    } else {
      missing_rows$version <- NA_character_
    }
  }
  missing_rows$pkg_missing <- TRUE
  if ("R_version" %in% names(missing_rows)) {
    missing_rows$R_version <- getRversion()
  }
  if ("riskmetric_run_date" %in% names(missing_rows)) {
    missing_rows$riskmetric_run_date <- date_avail
  }
  if ("riskmetric_version" %in% names(missing_rows)) {
    missing_rows$riskmetric_version <- packageVersion("riskmetric")
  }
  if ("repo_src" %in% names(missing_rows)) {
    missing_rows$repo_src <- repo
  }
  cat("\n--> Appending", n_miss, "pkg_missing row(s) to", repo,
      kind, "bundle.\n")
  vctrs::vec_rbind(bundle, missing_rows)
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

# object.size(assessed_latest) / 1000000  # 1059.5 MB
# object.size(scored_latest)   / 1000000  # 25.4 MB

#
# ---- Output as .rda or .parquet ----
#

# .rda
# name it as "latest"
usethis::use_data(scored_latest, overwrite = TRUE)
usethis::use_data(assessed_latest, overwrite = TRUE)
usethis:use_version("patch")

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








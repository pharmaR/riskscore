#############
## code to prepare `cran_20250812` dataset

# If needed
# utils::install.packages(c("riskmetric", "dplyr", "cranlogs")

library(dplyr)
library(cranlogs)
library(riskmetric)
# packageVersion("riskmetric") # ‘0.2.5’

######
# identify last available day of data
date_avail <- cranlogs::cran_downloads("dplyr", "last-day") |> pull(date) #6/21

# Get daily downloads for all pkgs from Rstudio CRAN Mirror for the last year
avail_pkgs <- available.packages("https://cran.rstudio.com/src/contrib")[,1]


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

# Need this if assessments converted to a tibble?
# Specifically, the 'with_eval_recording' attribute
# it did make our assessment object blow up in size
strip_recording0 <- function(assessment) {
  lapply(assessment, \(x) {
    out <-
      list(
        structure(
          x[[1]],
          .recording = NULL,
          class = setdiff(class(x[[1]]), "with_eval_recording")
        )
      )
    attributes(out) <- attributes(x)
    out
  })
}


incrmt_cran <- function(pkg_names, label) {
  cat("\n\nKicking off batch", label,"\n")
  # pkg_names <- c("dplyr") # for testing / debugging
  # label <- "'TEST'"
  incrmt_ct <- length(pkg_names)
  cat("\n-->", incrmt_ct, "package(s) to process for batch", label,"\n")
  st <- Sys.time()
  assessed_cran <-
    pkg_names %>%
    riskmetric::pkg_ref(source = "pkg_cran_remote", repos = c("https://cran.rstudio.com")) %>%
    dplyr::as_tibble() %>%
    riskmetric::pkg_assess() %>%
    strip_recording()

  cat("\n--> batch", label,"Assessed.\n")
  scored_cran <- assessed_cran %>%
    riskmetric::pkg_score(weights = metric_weights)

  cat("\n--> batch", label,"scored\n")
  end <- Sys.time()
  # Note: this took a well equipped laptop about 10 hours
  cat("\n-->", capture.output(end - st), ".\n")

  #
  # Prepare the datasets for saving
  #
  # Save the assessed and scored datasets
  cran_assessed_20250812 <- assessed_cran %>%
    dplyr::mutate(
      R_version = getRversion(),
      riskmetric_run_date = date_avail,
      riskmetric_version = packageVersion("riskmetric")
    ) %>%
    dplyr::select(-pkg_ref, package, version, everything())
  saveRDS(cran_assessed_20250812, paste0("data-raw/cran20250812/cran_assessed_20250812_",label,".rds"))

  cran_scored_20250812 <- scored_cran %>%
    dplyr::mutate(
      R_version = getRversion(),
      riskmetric_run_date = date_avail,
      riskmetric_version = packageVersion("riskmetric")
    ) %>%
    dplyr::arrange(pkg_score) %>%
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
cran_assessed_20250812 <- purrr::map(labs, ~
    readRDS(paste0("data-raw/cran20250812/cran_assessed_20250812_",.x,".rds"))
    ) |>
  purrr::reduce(dplyr::bind_rows)
cran_scored_20250812 <- purrr::map(labs, ~
     readRDS(paste0("data-raw/cran20250812/cran_scored_20250812_",.x,".rds"))
  ) |>
  purrr::reduce(dplyr::bind_rows)

# output as rda
usethis::use_data(cran_assessed_20250812, overwrite = TRUE)
usethis::use_data(cran_scored_20250812, overwrite = TRUE)


# test some things
# First, compare size to old run

data("cran_scored_20230621")
object.size(cran_scored_20230621) / 1000000 # 5 MB

data("cran_scored_20250812")
object.size(cran_scored_20250812) / 1000000 # 20 MB

nrow(cran_scored_20250812) - nrow(cran_scored_20230621) # 2,782 more pkgs

# Need this if assessments converted to a tibble?
# Specifically, the 'with_eval_recording' attribute
# it did make our assessment object blow up in size

# attributes(cran_assessed_20250812$remote_checks[1])
# attr(cran_assessed_20250812$remote_checks[1], "with_eval_recording")

data("cran_assessed_20250812")
object.size(cran_assessed_20250812) / 1000000000 # 1.5 GB

# Let's strip that junk out
# strip_recording <- function(x) {
#   out <-
#     list(
#       structure(
#         x[[1]],
#         .recording = NULL,
#         class = setdiff(class(x[[1]]), "with_eval_recording")
#       )
#     )
#   attributes(out) <- attributes(x)
#   out
# }

# metric_value <- case_when(
#   "pkg_metric_error" %in% class(riskmetric_assess[[metric$name]][[1]]) ~ "pkg_metric_error",
#   metric$name == "dependencies" ~ as.character(NROW(riskmetric_assess[[metric$name]][[1]])),
#   metric$name == "reverse_dependencies" ~ as.character(length(as.vector(riskmetric_assess[[metric$name]][[1]]))),
#   metric$is_perc == 1L ~ as.character(round(riskmetric_score[[metric$name]]*100, 2)[[1]]),
#   TRUE ~ as.character(riskmetric_assess[[metric$name]][[1]][1:length(riskmetric_assess[[metric$name]])])
# )

strip_recording <- function(assessment) {
  assessment <- assessed_cran |>
    dplyr::select(-c(package, version, pkg_ref,
           R_version, riskmetric_run_date, riskmetric_version)) |>
    dplyr::mutate(dplyr::across(
      dplyr::everything(), ~
        if("pkg_metric_error" %in% class(.x[[1]])) "pkg_metric_error" else .x[[1]]
        # if(stringr::str_detect(.x[[1]], "pkg_metric_error")) "pkg_metric_error" else .x[[1]]
      )
    )

  these_cols <- colnames(assessment)
  lapply(these_cols, \(col_name) {
    cat("\n\nNew x =", col_name, "\n")
    col_vector <- assessment[[col_name]]
    col_len <- length(col_vector)
    lapply(1:col_len, function(i) {
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
    }) #|> unlist(use.names = FALSE)
  })
}


# Let's strip that junk out
assessed_cran <- cran_assessed_20250812

assessed_cran$has_news[589]
assessed_cran$has_news[590] # error

cran_assessed_20250812 <- assessed_cran %>%
  dplyr::select(-c(package, version, pkg_ref,
                   R_version, riskmetric_run_date, riskmetric_version)) %>%
  dplyr::mutate(dplyr::across(dplyr::everything(), ~
      "pkg_metric_error" %in% class(.x[[1]]) ~ "pkg_metric_error")) %>%
  # purrr::modify(., strip_recording) %>%
  strip_recording()

  # dplyr::as_tibble() %>%
  # dplyr::mutat
  # dplyr::mutate(
  #   R_version = getRversion(),
  #   riskmetric_run_date = date_avail,
  #   riskmetric_version = packageVersion("riskmetric")
  # )

  # apply a function called fun() to every cell in a data.frame mtcars



  # copilot, help me apply strip_recording() across all columns of assessed_cran



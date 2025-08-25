#############
## code to prepare `v0.2.5_cran_20250812` dataset

# If needed
# utils::install.packages(c("riskmetric", "dplyr", "cranlogs")

library(dplyr)
library(cranlogs)
library(riskmetric)
# packageVersion("riskmetric") # ‘0.2.5’

######
# identify last available day of data
date_avail <- cran_downloads("dplyr", "last-day") |> pull(date) #6/21

# Get daily downloads for all pkgs from Rstudio CRAN Mirror for the last year
avail_pkgs <- available.packages("https://cran.rstudio.com/src/contrib")[,1]


# Assess the 'dplyr' pkg to identify which metrics are available for 'pkg_cran_remote'
assessed <- c("dplyr") %>%
  pkg_ref(source = "pkg_cran_remote", repos = c("https://cran.rstudio.com")) %>%
  as_tibble() %>%
  pkg_assess()

initial_scoring <- assessed %>% pkg_score()

metric_scores <- initial_scoring %>%
  select(-c(package, version, pkg_ref)) %>%
  t

# riskmetric doesn't pick up certain metrics for pkg_ref(source = "pkg_cran_remote")
# so we'll set their weights to zero here by defining weights
metric_weights <- ifelse(is.na(metric_scores[,1]), 0, 1)


################
# Score all of CRAN
st <- Sys.time()
assessed_cran0 <- avail_pkgs %>%
  pkg_ref(source = "pkg_cran_remote", repos = c("https://cran.rstudio.com")) %>%
  as_tibble() %>%
  pkg_assess()

scored_cran <- assessed_cran %>%
  pkg_score(weights = metric_weights)
end <- Sys.time()
end - st # Note: this took a well equipped laptop about 10 hours



#
# ---- Scores ----
#
cran_scored_20250812 <- scored_cran %>%
  mutate(
    R_version = getRversion(),
    riskmetric_run_date = date_avail,
    riskmetric_version = packageVersion("riskmetric")
    ) %>%
  arrange(pkg_score) %>%
  select(-pkg_ref, package, version, pkg_score, everything())
saveRDS(cran_scored_20250812, "cran_scored_20250812.rds")


#
# ---- Strip function ----
#
assessed_cran <- assessed_cran0 %>%
  mutate(
    R_version = getRversion(),
    riskmetric_run_date = date_avail,
    riskmetric_version = packageVersion("riskmetric")
  ) %>%
  select(-pkg_ref, package, version, everything())


#
# ---- Strip function ----
#

# Now, strip out the, the .recording / 'with_eval_recording' attribute
# since it made our assessment object blow up in size

strip_recording <- function(assessment) {

  these_cols <- colnames(assessment)
  lapply(these_cols, \(col_name) {
    cat("\n\nCol Name =", col_name, "\n")
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
# ---- Clean up ----
#
cran_assessed_lite <- assessed_cran |>
  dplyr::select(-c(package, version, pkg_ref,
                   R_version, riskmetric_run_date, riskmetric_version)) |>
  dplyr::mutate(dplyr::across(c(has_news), ~ if("pkg_metric_error" %in% class(.x[[1]])) "pkg_metric_error" else .x[[1]])) |>
  strip_recording()

cran_assessed_20250812 <- assessed_cran |>
  dplyr::select(c(package, version, pkg_ref,
                  R_version, riskmetric_run_date, riskmetric_version)) |>
  bind_cols(cran_assessed_lite) |>
  dplyr::mutate(
    R_version = getRversion(),
    riskmetric_run_date = date_avail,
    riskmetric_version = packageVersion("riskmetric")
  )


# Save the dataset
saveRDS(cran_assessed_20250812, "cran_assessed_20250812.rds")

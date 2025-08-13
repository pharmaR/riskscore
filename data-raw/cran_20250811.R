#############
## code to prepare `v0.2.5_cran_20250811` dataset

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
st <- Sys.time()
assessed_cran <-
  # "dplyr" %>%
  avail_pkgs %>%
  riskmetric::pkg_ref(source = "pkg_cran_remote", repos = c("https://cran.rstudio.com")) %>%
  dplyr::as_tibble() %>%
  riskmetric::pkg_assess()

scored_cran <- assessed_cran %>%
  riskmetric::pkg_score(weights = metric_weights)
end <- Sys.time()
end - st # Note: this took a well equipped laptop about 10 hours

# Prepare the datasets for saving, then
# Save the assessed and scored datasets
cran_assessed_20250811 <- assessed_cran %>%
  dplyr::mutate(
    R_version = getRversion(),
    riskmetric_run_date = date_avail,
    riskmetric_version = packageVersion("riskmetric")
  ) %>%
  dplyr::select(-pkg_ref, package, version, everything())
# saveRDS(cran_assessed_20250811, "data/cran_assessed_20250811.rds")

cran_scored_20250811 <- scored_cran %>%
  dplyr::mutate(
    R_version = getRversion(),
    riskmetric_run_date = date_avail,
    riskmetric_version = packageVersion("riskmetric")
  ) %>%
  dplyr::arrange(pkg_score) %>%
  dplyr::select(-pkg_ref, package, version, pkg_score, everything())
# saveRDS(cran_scored_20250811, "data/cran_scored_20250811.rds")


# output as rda
usethis::use_data(cran_assessed_20250811, overwrite = TRUE)
usethis::use_data(cran_scored_20250811, overwrite = TRUE)

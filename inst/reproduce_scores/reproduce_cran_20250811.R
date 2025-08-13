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
assessed_cran <- avail_pkgs %>%
  pkg_ref(source = "pkg_cran_remote", repos = c("https://cran.rstudio.com")) %>%
  as_tibble() %>%
  pkg_assess()

scored_cran <- assessed_cran %>%
  pkg_score(weights = metric_weights)
end <- Sys.time()
end - st # Note: this took a well equipped laptop about 10 hours


v0.2.5_cran_assessed_20250811 <- assessed_cran %>%
  mutate(
    R_version = getRversion(),
    riskmetric_run_date = date_avail,
    riskmetric_version = packageVersion("riskmetric")
  ) %>%
  select(-pkg_ref, package, version, everything())
saveRDS(v0.2.5_cran_assessed_20250811, "v0.2.5_cran_assessed_20250811.rds")

v0.2.5_cran_scored_20250811 <- scored_cran %>%
  mutate(
    R_version = getRversion(),
    riskmetric_run_date = date_avail,
    riskmetric_version = packageVersion("riskmetric")
    ) %>%
  arrange(pkg_score) %>%
  select(-pkg_ref, package, version, pkg_score, everything())
saveRDS(v0.2.5_cran_scored_20250811, "v0.2.5_cran_scored_20250811.rds")



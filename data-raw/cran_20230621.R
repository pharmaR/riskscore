#############
## code to prepare `v0.2.1_cran_20230621` dataset

library(dplyr)
library(cranlogs)

######
# identify last available day of data
date_avail <- cran_downloads("dplyr", "last-day") |> pull(date) #6/21

# Get daily downloads for all pkgs from Rstudio CRAN Mirror for the last year
avail_pkgs <- available.packages("https://cran.rstudio.com/src/contrib")[,1]


# Quick test run on some popular pkgs
library(dplyr)
library(riskmetric)
# packageVersion("riskmetric") # > [1] ‘0.2.1’


assessed <- c("dplyr") %>%
  pkg_ref(source = "pkg_cran_remote", repos = c("https://cran.rstudio.com")) %>%
  as_tibble() %>%
  pkg_assess()

initial_scoring <- assessed %>% pkg_score()

# riskmetric doesn't pick up certain metrics for pkg_ref(source = "pkg_cran_remote")
metric_scores <- initial_scoring %>%
  select(-c(package, version, pkg_ref)) %>%
  t

# so we'll set their weights to zero here by defining weights
metric_weights <- ifelse(is.na(metric_scores[,1]), 0, 1)


################
# Score all of CRAN
st <- Sys.time()
scored_cran <- avail_pkgs %>%
  pkg_ref(source = "pkg_cran_remote", repos = c("https://cran.rstudio.com")) %>%
  as_tibble() %>%
  pkg_assess() %>%
  pkg_score(weights = metric_weights)
end <- Sys.time()
end - st # Note: this took a well equipped laptop about 10 hours

v0.2.1_cran_20230621 <- scored_cran %>%
  mutate(riskmetric_run_date = date_avail,
         riskmetric_version = "0.2.1") %>%
  arrange(package) %>%
  select(-pkg_ref, package, version, riskmetric_run_date, riskmetric_version, pkg_score, everything())

# # A different version of this file was shared for later use, that needs some cleanup
cran_scored_20230621 <- readRDS("data-raw/initial_v0.2.1_cran_20230621.rds") %>%
  select(-c(rank_dwnlds, downloads, rank_score, start_date)) %>%
  mutate(riskmetric_version = "0.2.1") %>%
  select(package, version, riskmetric_run_date = end_date, riskmetric_version, pkg_score, everything())

# output as rda
# cran_scored_20230621 <- v0.2.1_cran_20230621
usethis::use_data(cran_scored_20230621, overwrite = TRUE)

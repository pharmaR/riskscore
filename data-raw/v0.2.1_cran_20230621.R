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


assessed <- c("dplyr")
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
  mutate(date = date_avail) %>%
  arrange(pkg_score) %>%
  mutate(rank_score = row_number())  %>%
  select(-pkg_ref, package, date, version, rank_score, pkg_score, everything())

# # A different version of this file was shared for later use, that needs some cleanup
# v0.2.1_cran_20230621 <- readRDS("data-raw/initial_v0.2.1_cran_20230621.rds") %>%
#   select(-c(rank_dwnlds, downloads, start_date)) %>%
#   select(package, date = end_date, version, rank_score, pkg_score, everything())

usethis::use_data(v0.2.1_cran_20230621, overwrite = TRUE)

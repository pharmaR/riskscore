# riskscore 0.1.1

# riskscore 0.1.0

# riskscore 0.0.3

* Added `scored_20251001` & `assessed_20251001` datasets with `{riskmetric}` output for all CRAN packages as of 2025-10-01.
* Considered saving as `.parquet` instead of `.rda` for more efficient storage and faster loading times. However, decided to stick with `.rda` for now due to its simplicity and wide usage in the R community. However, it's not viable at this time.
- Add BioConductor pkgs to both assessed & scored data.frames exported from pkg

# riskscore 0.0.1

* Revamped process to store assessments (in addition to scores) in a tibble.
* Added `cran_scored_20250812` & `cran_assessed_20250812` datasets with `{riskmetric}` output for all CRAN packages as of 2024-08-12.

# riskscore 0.0.0.9000

* set up pkg skeleton with initial `riskscore` placeholder on 2023-06-21


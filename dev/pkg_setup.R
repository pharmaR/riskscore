
# Workflow for setting up the repository

# create pkg skeleton structure
usethis::create_package(getwd())

# Manually modify the description ----------------------------------------------

# Add a bunch of necessities
usethis::use_lifecycle_badge( "Experimental" ) #Experimental, Maturing, Stable, Superseded, Archived, Dormant, Questioning
usethis::use_news_md( open = TRUE )
usethis::use_mit_license("R Validation Hub")

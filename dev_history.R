usethis::use_build_ignore(files = "dev_history.R")
usethis::use_gpl3_license()

usethis::use_vignette("xtradata")

usethis::use_pipe()

usethis::use_package("httr")
usethis::use_package("assertthat")
usethis::use_package("glue")
usethis::use_package("purrr")
usethis::use_package("jsonlite")
usethis::use_package("curl")



usethis::use_testthat()

usethis::use_test(name = "check_internet")
usethis::use_test(name = "check_API_results")
usethis::use_test(name = "xtradata_requete_features")



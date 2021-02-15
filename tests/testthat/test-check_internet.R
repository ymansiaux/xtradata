test_that("check internet", {
  skip_if_not(curl::has_internet(), "Pas de connexion internet")
  expect_true(check_internet())
})

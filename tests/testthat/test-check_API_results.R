test_that("check GET www.google.com status = 200", {
  skip_if_not(curl::has_internet(), "Pas de connexion internet")
  expect_equal(status_code(httr::GET("http://www.google.com")), 200)
})

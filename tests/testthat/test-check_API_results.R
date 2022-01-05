test_that("check GET www.google.com status = 200", {
  skip_if_not(curl::has_internet(), "Pas de connexion internet")
  expect_true(check_API_results(curl::curl_fetch_memory("www.google.fr")))
})

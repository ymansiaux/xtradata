test_that("check GET www.google.com status = 200", {
  expect_equal(status_code(httr::GET("http://www.google.com")), 200)
})

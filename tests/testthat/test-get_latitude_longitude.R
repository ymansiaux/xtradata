test_that("test fonction get_latitude_longitude", {

  skip_if_not(curl::has_internet(), "Pas de connexion internet")

  MaCle <- Sys.getenv("XTRADATA_KEY")

  req <- xtradata_requete_aggregate(key = MaCle,
                                    typename = "ST_PARK_P",
                                    rangeStart = "2021-06-01",
                                    rangeEnd = "2021-06-02",
                                    rangeStep = "hour")

  res <- get_latitude_longitude(req)

  expect_s3_class(res, "data.frame")
  expect_gt(nrow(res), 0)
  expect_equal(ncol(res),2)
  expect_true(all(sapply(res, is.numeric)))

})



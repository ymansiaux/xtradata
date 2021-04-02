Sys.setlocale('LC_ALL','C')

test_that("test fonction get_latitude_longitude", {

  skip_if_not(curl::has_internet(), "Pas de connexion internet")

  MaCle <- "DATAZBOUBB"

  req <- xtradata_requete_aggregate(key = MaCle,
                                    typename = "PC_CAPTV_P",
                                    rangeStart = "2021-01-01",
                                    rangeEnd = "2021-01-05",
                                    rangeStep = "hour")

  res <- get_latitude_longitude(req)

  expect_s3_class(res, "data.frame")
  expect_gt(nrow(res), 0)
  expect_equal(ncol(res),2)
  expect_true(all(sapply(res, is.numeric)))

})



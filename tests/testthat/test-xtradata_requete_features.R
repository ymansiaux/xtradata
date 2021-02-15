Sys.setlocale('LC_ALL','C')
test_that("recuperation de la couche des parkings hors voirie", {

  skip_if_not(curl::has_internet(), "Pas de connexion internet")

  MaCle <- "DATAZBOUBB"

  req <- xtradata_requete_features(typename  = "ST_PARK_P", key = MaCle)

  expect_is(req, "data.frame")
  expect_equal(nrow(req), 84)

})


test_that("Features : passage de attributes en vecteur R et en array resultats identiques", {

skip_if_not(curl::has_internet(), "Pas de connexion internet")

  MaCle <- "DATAZBOUBB"

  attributes <- list("cdate", "mdate")
  attributesArray <- '["cdate", "mdate"]'

  res1 <- xtradata_requete_features(typename  = "PC_CAPTE_P", key = MaCle,
                                    attributes = attributes)

  res2 <- xtradata_requete_features(typename  = "PC_CAPTE_P", key = MaCle,
                                    attributes = attributesArray)

  expect_gt(nrow(res1), 0)
  expect_gt(nrow(res2), 0)
  expect_equal(dim(res1), dim(res2))
  expect_true(all.equal(res1, res2))

  })



test_that("Features : passage de filter en liste R et en json resultats identiques", {

  skip_if_not(curl::has_internet(), "Pas de connexion internet")

  MaCle <- "DATAZBOUBB"

  filter <- list("type" = "BOUCLE",
                 "mdate" = list(
                   '$gt' = "2020-01-01T08:00:00")
  )


  filterJSON <-'{

   "type": "BOUCLE",
 "mdate": {
     "$gt": "2020-01-01T08:00:00"
    }
  }'

  res1 <- xtradata_requete_features(typename  = "PC_CAPTE_P", key = MaCle,
                                    filter = filter)

  res2 <- xtradata_requete_features(typename  = "PC_CAPTE_P", key = MaCle,
                                    filter = filterJSON)

  expect_gt(nrow(res1), 0)
  expect_gt(nrow(res2), 0)
  expect_equal(dim(res1), dim(res2))
  expect_true(all.equal(res1, res2))

})

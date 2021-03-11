Sys.setlocale('LC_ALL','C')
test_that("Aggregate : passage de filter en liste R et en json resultats identiques", {

  skip_if_not(curl::has_internet(), "Pas de connexion internet")

  MaCle <- "DATAZBOUBB"

  filter <- list("ident" = "CUBPK88",
                 "etat" = "LIBRE",
                 "libres" = list(
                   '$gt' = 100)
  )

  filterJSON <- '{
  "ident": "CUBPK88",
  "etat" : "LIBRE",
  "libres": {
    "$gt": 100
  }
  }'

  res1 <- xtradata_requete_aggregate(typename  = "ST_PARK_P", key = MaCle,
                                     rangeStart = "2020-08-01",
                                     rangeEnd = "2020-08-16",
                                     rangeStep = "hour",
                                     filter = filter)

  res2 <- xtradata_requete_aggregate(typename  = "ST_PARK_P", key = MaCle,
                                     rangeStart = "2020-08-01",
                                     rangeEnd = "2020-08-16",
                                     rangeStep = "hour",
                                     filter = filterJSON)



  expect_gt(nrow(res1), 0)
  expect_gt(nrow(res2), 0)
  expect_equal(dim(res1), dim(res2))
  expect_true(all.equal(res1, res2))

})



test_that("Aggregate : passage de rangeFilter en liste R et en json resultats identiques", {

  skip_if_not(curl::has_internet(), "Pas de connexion internet")

  MaCle <- "DATAZBOUBB"

  filter <- list("ident" = "CUBPK88",
                 "etat" = "LIBRE",
                 "libres" = list(
                   '$gt' = 100)
  )

  rangeFilter <- list("hours" = 5:6,
                       "days" = 1:7,
                       "publicHolidays" = FALSE
                        )
   rangeFilterJSON <- '{
     "hours": [
       5,6
     ],
     "days": [
       1,2,3,4,5,6,7
     ],
     "publicHolidays": false
   }'

   res1 <- xtradata_requete_aggregate(typename  = "ST_PARK_P", key = MaCle,
                                      rangeStart = "2020-08-01",
                                      rangeEnd = "2020-08-16",
                                      rangeStep = "hour",
                                      rangeFilter = rangeFilter,
                                      attributes = list("gid", "libres", "total", "etat", "ident"),
                                      filter = filter)

   res2 <- xtradata_requete_aggregate(typename  = "ST_PARK_P", key = MaCle,
                                      rangeStart = "2020-08-01",
                                      rangeEnd = "2020-08-16",
                                      rangeStep = "hour",
                                      rangeFilter = rangeFilterJSON,
                                      attributes = list("gid", "libres", "total", "etat", "ident"),
                                      filter = filter)



  expect_gt(nrow(res1), 0)
  expect_gt(nrow(res2), 0)
  expect_equal(dim(res1), dim(res2))
  expect_true(all.equal(res1, res2))

})



test_that("Aggregate : passage de attributes en vecteur R et en array resultats identiques", {

  skip_if_not(curl::has_internet(), "Pas de connexion internet")

  MaCle <- "DATAZBOUBB"

  filter <- list("ident" = "CUBPK88",
                 "etat" = "LIBRE",
                 "libres" = list(
                   '$gt' = 100)
  )

  rangeFilter <- list("hours" = 5:6,
                      "days" = 1:7,
                      "publicHolidays" = FALSE
  )


  attributes <- list("gid", "libres")
  attributesArray <- '["gid", "libres"]'

  res1 <- xtradata_requete_aggregate(typename  = "ST_PARK_P", key = MaCle,
                                     rangeStart = "2020-08-01",
                                     rangeEnd = "2020-08-16",
                                     rangeStep = "hour",
                                     rangeFilter = rangeFilter,
                                     attributes = attributes,
                                     filter = filter)

  res2 <- xtradata_requete_aggregate(typename  = "ST_PARK_P", key = MaCle,
                                     rangeStart = "2020-08-01",
                                     rangeEnd = "2020-08-16",
                                     rangeStep = "hour",
                                     rangeFilter = rangeFilter,
                                     attributes = attributesArray,
                                     filter = filter)

  expect_gt(nrow(res1), 0)
  expect_gt(nrow(res2), 0)
  expect_equal(dim(res1), dim(res2))
  expect_true(all.equal(res1, res2))

})


test_that("Aggregate : passage de attributes en clé valeur en liste R et en json resultats identiques", {

  skip_if_not(curl::has_internet(), "Pas de connexion internet")

  MaCle <- "DATAZBOUBB"

  filter <- list("ident" = "CUBPK88",
                 "etat" = "LIBRE",
                 "libres" = list(
                   '$gt' = 100)
  )

  rangeFilter <- list("hours" = 5:6,
                      "days" = 1:7,
                      "publicHolidays" = FALSE
  )


  attributes_key_value_list <- list("gid" = "first", "libres" = "max")
  attributes_key_value_JSON <- '{"gid" : "first", "libres" : "max"}'


  res1 <- xtradata_requete_aggregate(typename  = "ST_PARK_P", key = MaCle,
                                     rangeStart = "2020-08-01",
                                     rangeEnd = "2020-08-16",
                                     rangeStep = "hour",
                                     rangeFilter = rangeFilter,
                                     attributes = attributes_key_value_list,
                                     filter = filter)

  res2 <- xtradata_requete_aggregate(typename  = "ST_PARK_P", key = MaCle,
                                     rangeStart = "2020-08-01",
                                     rangeEnd = "2020-08-16",
                                     rangeStep = "hour",
                                     rangeFilter = rangeFilter,
                                     attributes = attributes_key_value_JSON,
                                     filter = filter)

  expect_gt(nrow(res1), 0)
  expect_gt(nrow(res2), 0)
  expect_equal(dim(res1), dim(res2))
  expect_true(all.equal(res1, res2))

})


test_that("Aggregate : tests filtres combines dans filter", {

  skip_if_not(curl::has_internet(), "Pas de connexion internet")

  MaCle <- "DATAZBOUBB"

  filterJSON_combined <- '{
  "$and": [
    { "total": {"$gte": 500} },
    { "total": {"$lte": 1000} }
  ]
}'


  filter_combined <- list("$and" = list(
    list("total" = list(
      "$gte" = 500
    )),

    list("total" = list(
      "$lte" = 1000
    ))
  ))


  res1 <- xtradata_requete_aggregate(typename  = "ST_PARK_P", key = MaCle,
                                     rangeStart = "2020-08-01",
                                     rangeEnd = "2020-08-16",
                                     filter = filterJSON_combined)

  res2 <- xtradata_requete_aggregate(typename  = "ST_PARK_P", key = MaCle,
                                     rangeStart = "2020-08-01",
                                     rangeEnd = "2020-08-16",
                                     filter = filter_combined)

  expect_gt(nrow(res1), 0)
  expect_gt(nrow(res2), 0)
  expect_equal(dim(res1), dim(res2))
  expect_true(all.equal(res1, res2))

})


date_deb <- "2021-06-01"
date_fin <- "2021-06-05"

library(lubridate)
library(dplyr)

test_that("Aggregate : passage de filter en liste R et en json resultats identiques", {
  skip_if_not(curl::has_internet(), "Pas de connexion internet")

  MaCle <- Sys.getenv("XTRADATA_KEY")

  filter <- list(
    "ident" = "CUBPK88",
    "etat" = "LIBRE",
    "libres" = list(
      "$gt" = 100
    )
  )

  filterJSON <- '{
  "ident": "CUBPK88",
  "etat" : "LIBRE",
  "libres": {
    "$gt": 100
  }
  }'

  res1 <- xtradata_requete_aggregate(
    typename = "ST_PARK_P", key = MaCle,
    rangeStart = date_deb,
    rangeEnd = date_fin,
    rangeStep = "hour",
    filter = filter
  )

  res2 <- xtradata_requete_aggregate(
    typename = "ST_PARK_P", key = MaCle,
    rangeStart = date_deb,
    rangeEnd = date_fin,
    rangeStep = "hour",
    filter = filterJSON
  )



  expect_gt(nrow(res1), 0)
  expect_gt(nrow(res2), 0)
  expect_equal(unique(res1$gid), 321)
  expect_equal(unique(res2$gid), 321)
  expect_gte(min(res1$libres, na.rm = TRUE), 100)
  expect_gte(min(res2$libres, na.rm = TRUE), 100)
  expect_equal(dim(res1), dim(res2))
  expect_true(all.equal(res1, res2))
})


test_that("Aggregate : passage d'un seul paramètre en filter", {
  skip_if_not(curl::has_internet(), "Pas de connexion internet")

  MaCle <- Sys.getenv("XTRADATA_KEY")

  attributes_key_value_JSON <- '{"comptage_5m" : "sum"}'
  filter <- '{"ident": "Z203CT7"}'
  filterJSON <- '{
"ident": "Z203CT7"
}'


  res1 <- xtradata_requete_aggregate(
    typename = "PC_CAPTV_P", key = MaCle,
    rangeStart = date_deb,
    rangeEnd = date_fin,
    rangeStep = "hour",
    filter = filter,
    attributes = attributes_key_value_JSON

  )

  res2 <- xtradata_requete_aggregate(
    typename = "PC_CAPTV_P", key = MaCle,
    rangeStart = date_deb,
    rangeEnd = date_fin,
    rangeStep = "hour",
    filter = filterJSON,
    attributes = attributes_key_value_JSON
  )

  expect_gt(nrow(res1), 0)
  expect_gt(nrow(res2), 0)
  expect_equal(unique(res1$gid), 2451)
  expect_equal(unique(res2$gid), 2451)
  expect_equal(dim(res1), dim(res2))
  expect_true(all.equal(res1, res2))
})

test_that("Aggregate : passage de rangeFilter en liste R et en json resultats identiques", {
  skip_if_not(curl::has_internet(), "Pas de connexion internet")

  MaCle <- Sys.getenv("XTRADATA_KEY")

  filter <- list(
    "ident" = "CUBPK88",
    "etat" = "LIBRE",
    "libres" = list(
      "$gt" = 100
    )
  )

  rangeFilter <- list(
    "hours" = 5:6,
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

  res1 <- xtradata_requete_aggregate(
    typename = "ST_PARK_P", key = MaCle,
    rangeStart = date_deb,
    rangeEnd = date_fin,
    rangeStep = "hour",
    rangeFilter = rangeFilter,
    attributes = list("gid", "libres", "total", "etat", "ident"),
    filter = filter
  )

  res2 <- xtradata_requete_aggregate(
    typename = "ST_PARK_P", key = MaCle,
    rangeStart = date_deb,
    rangeEnd = date_fin,
    rangeStep = "hour",
    rangeFilter = rangeFilterJSON,
    attributes = list("gid", "libres", "total", "etat", "ident"),
    filter = filter
  )

  expect_gt(nrow(res1), 0)
  expect_gt(nrow(res2), 0)
  expect_gte(min(res1$libres, na.rm = TRUE), 100)
  expect_gte(min(res2$libres, na.rm = TRUE), 100)
  expect_equal(dim(res1), dim(res2))
  expect_equal(unique(hour(as_datetime(res1$time, tz = "Europe/Paris"))), c(5,6))
  expect_equal(unique(hour(as_datetime(res2$time, tz = "Europe/Paris"))), c(5,6))
  expect_true(all.equal(res1, res2))
})



test_that("Aggregate : passage de attributes en vecteur R et en array resultats identiques", {
  skip_if_not(curl::has_internet(), "Pas de connexion internet")

  MaCle <- Sys.getenv("XTRADATA_KEY")

  filter <- list(
    "ident" = "CUBPK88",
    "etat" = "LIBRE",
    "libres" = list(
      "$gt" = 100
    )
  )

  rangeFilter <- list(
    "hours" = 5:6,
    "days" = 1:7,
    "publicHolidays" = FALSE
  )


  attributes <- list("gid", "libres")
  attributesArray <- '["gid", "libres"]'

  res1 <- xtradata_requete_aggregate(
    typename = "ST_PARK_P", key = MaCle,
    rangeStart = date_deb,
    rangeEnd = date_fin,
    rangeStep = "hour",
    rangeFilter = rangeFilter,
    attributes = attributes,
    filter = filter
  )

  res2 <- xtradata_requete_aggregate(
    typename = "ST_PARK_P", key = MaCle,
    rangeStart = date_deb,
    rangeEnd = date_fin,
    rangeStep = "hour",
    rangeFilter = rangeFilter,
    attributes = attributesArray,
    filter = filter
  )

  expect_gt(nrow(res1), 0)
  expect_gt(nrow(res2), 0)
  expect_equal(dim(res1), dim(res2))
  expect_true(all.equal(res1, res2))
})


test_that("Aggregate : passage de attributes en clé valeur en liste R et en json resultats identiques", {
  skip_if_not(curl::has_internet(), "Pas de connexion internet")

  MaCle <- Sys.getenv("XTRADATA_KEY")

  filter <- list(
    "ident" = "CUBPK88",
    "etat" = "LIBRE",
    "libres" = list(
      "$gt" = 100
    )
  )

  rangeFilter <- list(
    "hours" = 5:6,
    "days" = 1:7,
    "publicHolidays" = FALSE
  )


  attributes_key_value_list <- list("ident" = "first", "libres" = "max")
  attributes_key_value_JSON <- '{"ident" : "first", "libres" : "max"}'


  res1 <- xtradata_requete_aggregate(
    typename = "ST_PARK_P", key = MaCle,
    rangeStart = date_deb,
    rangeEnd = date_fin,
    rangeStep = "hour",
    rangeFilter = rangeFilter,
    attributes = attributes_key_value_list,
    filter = filter
  )

  res2 <- xtradata_requete_aggregate(
    typename = "ST_PARK_P", key = MaCle,
    rangeStart = date_deb,
    rangeEnd = date_fin,
    rangeStep = "hour",
    rangeFilter = rangeFilter,
    attributes = attributes_key_value_JSON,
    filter = filter
  )

  expect_gt(nrow(res1), 0)
  expect_gt(nrow(res2), 0)
  expect_gte(min(res1$libres, na.rm = TRUE), 100)
  expect_gte(min(res2$libres, na.rm = TRUE), 100)
  expect_equal(dim(res1), dim(res2))
  expect_equal(unique(hour(as_datetime(res1$time, tz = "Europe/Paris"))), c(5,6))
  expect_equal(unique(hour(as_datetime(res2$time, tz = "Europe/Paris"))), c(5,6))
  expect_true(all.equal(res1, res2))
})


test_that("Aggregate : tests filtres combines dans filter", {
  skip_if_not(curl::has_internet(), "Pas de connexion internet")

  MaCle <- Sys.getenv("XTRADATA_KEY")

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


  res1 <- xtradata_requete_aggregate(
    typename = "ST_PARK_P", key = MaCle,
    rangeStart = date_deb,
    rangeEnd = date_fin,
    filter = filterJSON_combined
  )

  res2 <- xtradata_requete_aggregate(
    typename = "ST_PARK_P", key = MaCle,
    rangeStart = date_deb,
    rangeEnd = date_fin,
    filter = filter_combined
  )

  expect_gt(nrow(res1), 0)
  expect_gt(nrow(res2), 0)
  expect_gte(min(res1$total, na.rm = TRUE), 500)
  expect_gte(min(res2$total, na.rm = TRUE), 500)
  expect_lte(min(res1$total, na.rm = TRUE), 1000)
  expect_lte(min(res2$total, na.rm = TRUE), 1000)
  expect_equal(dim(res1), dim(res2))
  expect_true(all.equal(res1, res2))
})


test_that("Aggregate : tests group renvoie la meme chose", {
  skip_if_not(curl::has_internet(), "Pas de connexion internet")

  MaCle <- Sys.getenv("XTRADATA_KEY")

  filter <- list(
    "libres" = list(
      "$gt" = 100
    )
  )

  attributes <- list("libres" = "max")


  res1 <- xtradata_requete_aggregate(
    typename = "ST_PARK_P", key = MaCle,
    rangeStart = date_deb,
    rangeEnd = date_fin,
    rangeStep = "hour",
    filter = filter,
    group= "time+gid",
    attributes = attributes,
    showURL = TRUE
  )

  res1

  res1 <- res1 %>%
    group_by(time) %>%
    summarise(libres = max(libres))

  res2 <- xtradata_requete_aggregate(
    typename = "ST_PARK_P", key = MaCle,
    rangeStart = date_deb,
    rangeEnd = date_fin,
    rangeStep = "hour",
    filter = filter,
    group= "time",
    attributes = attributes
  )

  all.equal(res1$libres, res2$libres)

})

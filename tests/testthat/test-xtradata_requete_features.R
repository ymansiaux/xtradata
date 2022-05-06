library(lubridate)


test_that("recuperation de la couche des parkings hors voirie", {

  skip_if_not(curl::has_internet(), "Pas de connexion internet")

  MaCle <- Sys.getenv("XTRADATA_KEY")

  req <- xtradata_requete_features(typename  = "ST_PARK_P", key = MaCle)

  expect_s3_class(req, c("sf","data.frame"))

})


test_that("Features : passage d'un seul paramètre en filter", {
  skip_if_not(curl::has_internet(), "Pas de connexion internet")

  MaCle <- Sys.getenv("XTRADATA_KEY")

  filter <- '{"ident": "Z203CT7"}'
  filterJSON <- '{
"ident": "Z203CT7\"
}'


  res1 <- xtradata_requete_features(
    typename = "PC_CAPTV_P", key = MaCle,
    filter = filter,
    showURL = TRUE
  )

  res2 <- xtradata_requete_features(
    typename = "PC_CAPTV_P", key = MaCle,
    filter = filterJSON
  )

  expect_gt(nrow(res1), 0)
  expect_gt(nrow(res2), 0)
  expect_equal(unique(res1$gid), 2451)
  expect_equal(unique(res2$gid), 2451)
  expect_equal(dim(res1), dim(res2))
  expect_true(all.equal(res1, res2))
})

test_that("Features : test du fonctionnement d'une requete avec un filter qui contient un $in et derriere une seule valeur", {
  skip_if_not(curl::has_internet(), "Pas de connexion internet")

  MaCle <- Sys.getenv("XTRADATA_KEY")

  filter <- list(
    "ident" = list("$in" = "Z203CT7")
  )

  filter2 <- list(
    "ident" = "Z203CT7"
  )


  res1 <- xtradata_requete_features(
    typename = "PC_CAPTV_P", key = MaCle,
    filter = filter
  )

  res2 <- xtradata_requete_features(
    typename = "PC_CAPTV_P", key = MaCle,
    filter = filterJSON
  )

  expect_equal(nrow(res1),1)
  expect_equal(as.Date(res1$mdate), Sys.Date())
  expect_equal(unique(res1$gid), 2451)
})



test_that("Features : passage de attributes en vecteur R et en array resultats identiques", {

  skip_if_not(curl::has_internet(), "Pas de connexion internet")

  MaCle <- Sys.getenv("XTRADATA_KEY")

  attributes <- list("cdate", "mdate")
  attributesArray <- '["cdate", "mdate"]'

  res1 <- xtradata_requete_features(typename  = "PC_CAPTE_P", key = MaCle,
                                    attributes = attributes)

  res2 <- xtradata_requete_features(typename  = "PC_CAPTE_P", key = MaCle,
                                    attributes = attributesArray)

  expect_gt(nrow(res1), 0)
  expect_gt(nrow(res2), 0)
  expect_equal(colnames(res1), c("type", "cdate", "mdate"))
  expect_equal(colnames(res2), c("type", "cdate", "mdate"))
  expect_equal(dim(res1), dim(res2))
  expect_true(all.equal(res1, res2))

})



test_that("Features : passage de filter en liste R et en json resultats identiques", {

  skip_if_not(curl::has_internet(), "Pas de connexion internet")

  MaCle <- Sys.getenv("XTRADATA_KEY")

  filter <- list("type" = "BOUCLE",
                 "mdate" = list(
                   '$gt' = "2021-09-01T08:00:00")
  )


  filterJSON <-'{

   "type": "BOUCLE",
 "mdate": {
     "$gt": "2021-09-01T08:00:00"
    }
  }'

  res1 <- xtradata_requete_features(typename  = "PC_CAPTE_P", key = MaCle,
                                    filter = filter)

  res2 <- xtradata_requete_features(typename  = "PC_CAPTE_P", key = MaCle,
                                    filter = filterJSON)

  expect_gt(nrow(res1), 0)
  expect_gt(nrow(res2), 0)
  expect_true(all(as_date(res1$mdate) >= as_date("2021-09-01")))
  expect_true(all(as_date(res2$mdate) >= as_date("2021-09-01")))
  expect_equal(dim(res1), dim(res2))
  expect_true(all.equal(res1, res2))

})


test_that("Features : tests filtres combines dans filter", {

  skip_if_not(curl::has_internet(), "Pas de connexion internet")

  MaCle <- Sys.getenv("XTRADATA_KEY")

  filterJSON_combined <- '{
  "$and": [
    { "gid": {"$gte": "1"} },
    { "gid": {"$lte": "5"} }
  ]
}'


  filter_combined <- list("$and" = list(
    list("gid" = list(
      "$gte" = "1"
    )),

    list("gid" = list(
      "$lte" = "5"
    ))
  ))

  res1 <- xtradata_requete_features(typename  = "PC_CAPTE_P", key = MaCle,
                                    filter = filterJSON_combined)

  res2 <- xtradata_requete_features(typename  = "PC_CAPTE_P", key = MaCle,
                                    filter = filter_combined)

  expect_gt(nrow(res1), 0)
  expect_gt(nrow(res2), 0)
  expect_gte(min(res1$gid, na.rm = TRUE), 1)
  expect_gte(min(res2$gid, na.rm = TRUE), 1)
  expect_lte(min(res1$gid, na.rm = TRUE), 5)
  expect_lte(min(res2$gid, na.rm = TRUE), 5)
  expect_equal(dim(res1), dim(res2))
  expect_true(all.equal(res1, res2))

})

test_that("Features : backintime fonctionne", {

  MaCle <- Sys.getenv("XTRADATA_KEY")

  filter <- list(
    "type" = "BOUCLE",
    "mdate" = list(
      "$gt" = "2020-01-01T08:00:00"
    )
  )


  res10 <- xtradata_requete_features(
    typename = "PC_CAPTE_P", key = MaCle,
    filter = filter,
    backintime = "2021-06-01T10:00:00"
  )

  expect_equal(median(as_date(res10$mdate)), as_date("2021-06-01"))


  res11 <- xtradata_requete_features(
    typename = "PC_CAPTE_P", key = MaCle,
    filter = filter,
    backintime = "2021-06-01"
  )

  expect_equal(median(as_date(res11$mdate)), as_date("2021-06-01"))


  res12 <- xtradata_requete_features(
    typename = "PC_CAPTE_P", key = MaCle,
    filter = filter,
    backintime = "-30min"
  )

  expect_equal(median(as_date(res12$mdate)), Sys.Date())


  res13 <- xtradata_requete_features(
    typename = "PC_CAPTE_P", key = MaCle,
    filter = filter,
    backintime = "-5hour"
  )

  expect_equal(median(as_date(res13$mdate)), Sys.Date())

  res14 <- xtradata_requete_features(
    typename = "PC_CAPTE_P", key = MaCle,
    filter = filter,
    backintime = "-7day"
  )

  expect_equal(median(as_date(res14$mdate)), Sys.Date() - days(7))


  res15 <- xtradata_requete_features(
    typename = "PC_CAPTE_P", key = MaCle,
    filter = filter,
    backintime = "-1month"
  )

  expect_equal(median(as_date(res15$mdate)), add_with_rollback(Sys.Date(),-months(1)))

})

test_that("Features : orderby fonctionne", {

  MaCle <- Sys.getenv("XTRADATA_KEY")

  filter <- list(
    "type" = "BOUCLE",
    "mdate" = list(
      "$gt" = "2020-01-01T08:00:00"
    )
  )

  # 2 façons d'utiliser le paramètre orderby
  orderby <- list("mdate", "gid")
  orderbyArray <- '["mdate", "gid"]'

  res16 <- xtradata_requete_features(
    typename = "PC_CAPTE_P", key = MaCle,
    filter = filter,
    orderby = orderby
  )

  res17 <- xtradata_requete_features(
    typename = "PC_CAPTE_P", key = MaCle,
    filter = filter,
    orderby = orderbyArray
  )

  res17

  expect_true(all.equal(res16, res17))

})

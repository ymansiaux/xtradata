#' Recupere les données de latitude et longitude à partir d'une requete issue
#' d'un webservice xtradata
#'
#' @param resultat_requete le data.frame issu d'un appel au WS
#'
#' @return un data frame à 2 colonnes (latitude et longitude)
#' @export
#' @importFrom assertthat assert_that
#'
#' @examples \dontrun{
#' velos <- xtradata_requete_aggregate(key = "Macle",
#                                         typename = "PC_CAPTV_P",
#                                         rangeStart = "2021-01-01",
#                                         rangeEnd = "2021-02-01",
#                                         rangeStep = "hour"
#                                         )
# localisation_velos <- get_latitude_longitude(velos)
get_latitude_longitude <- function(resultat_requete) {

  assert_that(any(class(resultat_requete) %in% c("data.table", "data.frame")))

  assert_that("geometry.coordinates" %in% colnames(resultat_requete))


  coords <- data.frame(do.call(rbind, resultat_requete$geometry.coordinates))
  colnames(coords) <- c("latitude", "longitude")

  return(coords)

}


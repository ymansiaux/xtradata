#' Checks that the result is a 200 status code.
#'
#' @param requete requete sur l'API d'interet
#'
#' @return un booleen (status = 200 : TRUE / FALSE)
#' @importFrom httr status_code
#' @importFrom assertthat assert_that
#'
#' @export
#'
#' @examples
#' requete <- httr::GET("https://api.punkapi.com/v2/beers/3")
#' check_API_results(requete = requete)
check_API_results <- function(requete) {
  assert_that(
    status_code(requete) == 200,
    msg = "The API request returned an error"
  )
}

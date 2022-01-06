#' Checks that the result is a 200 status code.
#'
#' @param requete requete sur l'API d'interet
#'
#' @return un booleen (status = 200 : TRUE / FALSE)
#' @importFrom assertthat assert_that
#'
#' @export
#'
#' @examples
#' requete <- curl::curl_fetch_memory("http://www.google.com")
#' check_API_results(requete = requete)
check_API_results <- function(requete) {
  assert_that(
    requete$status_code == 200,
    msg = paste("The API request returned an error, API response code :", requete$status_code)
  )
}

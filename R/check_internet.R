#' Check if the user has an Internet connexion
#'
#' @return a boolean
#' @export
#' @importFrom httr GET
#' @importFrom assertthat assert_that
#' @examples check_internet()
check_internet <- function() {

  requete <- GET("http://www.google.com")
  assert_that(
    status_code(requete) == 200,
    msg = "No Internet connexion was found"
  )
}

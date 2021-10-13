#' Permet de retrouver le code erreur d'une requete qui n'a pas fonctionne
#'
#' @param request une requete issue d'un appel à un WS xtradata, incorporee
#' dans un try()
#'
#' @return une liste avec le code d'erreur et le message
#' @export
#'
#' @examples
#' get_API_error_code(try(xtradata_requete_features(key = "KEY_INCORRECTE", typename = "ST_PARK_P")))
get_API_error_code <- function(request) {
  assert_that(class(request) == "try-error", msg = "Cette fonction doit recevoir en entree un appel \u00e0 la fonction try, consulter la documentation")

  liste_code_erreurs_API <- list(
    "503" = "Erreur 503 : erreur de r\u00e9ponse du serveur",
    "500" = "Erreur 500 : erreur de r\u00e9ponse du serveur",
    "403" = "Erreur 403 : v\u00e9rifier la cl\u00e9 d\'API",
    "400" = "Erreur 400 : v\u00e9rifier les parametres de la requete"
  )

  code_erreur_API <- attributes(request)$condition
  code_erreur_API <- regmatches(as.character(code_erreur_API), regexpr(pattern = "\\d{3,}", text = as.character(code_erreur_API)))

  return(list("code_erreur" = code_erreur_API, "message_code_erreur" = liste_code_erreurs_API[[code_erreur_API]]))
}

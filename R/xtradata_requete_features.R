#' Renvoie les objets d'une couche (service features Xtradata)
#'
#' @param key Cle de connexion (string)
#'
#' @param typename Le nom de la couche (string)
#'
#' @param crs Système de coordonnées de sortie des géométries renvoyées par le serveur GeoJSON
#' valeurs autorisées : epsg:4326, epsg:3945, epsg:2154, epsg:3857
#'
#' @param filter Filtres à appliquer sur les données. Format liste R ou format JSON (string). Voir exemples
#'
#' @param attributes Liste des noms des attributs de la couche à retourner en résultat.
#'  Afin d'accélérer les traitements, listez uniquement les attributs que vous sont nécessaires.
#'  Si non précisé, tous les attributs seront retournés.
#'  Format vecteur R ou format array (string). Voir exemples
#'
#' @param maxfeatures Nombre maximum d'enregistrement à retourner.
#' Si 0 ou non précisé, tous les enregistrements de la couche seront retournés
#'
#' @return un data frame issu de la requête
#' @export
#'
#' @importFrom assertthat assert_that is.string
#' @importFrom glue glue glue_collapse
#' @importFrom purrr map2 compact vec_depth
#' @importFrom httr GET content
#' @importFrom jsonlite fromJSON toJSON
#' @importFrom utils URLencode
#' @examples \dontrun{
#' # appel sur la couche PC_CAPTE_P
#' filter <-
#'list("type" = "BOUCLE",
#'  "mdate" = list(
#'       '$gt' = "2020-01-01T08:00:00"
       #'  )
       #')

#'attributes <- c("cdate", "mdate")

#'filterJSON <-
#' '{

#' "type": "BOUCLE",
#' "mdate": {
#'   "$gt": "2020-01-01T08:00:00"
#'  }
#'}
#''

#' attributesArray <- '["cdate", "mdate"]'

#'xtradata_requete_features(typename  = "PC_CAPTE_P", key = MaCle,
#'filter = filter)
#'xtradata_requete_features(typename  = "PC_CAPTE_P", key = MaCle,
#'filter = filterJSON)


#'xtradata_requete_features(typename  = "PC_CAPTE_P", key = MaCle,
#'filter = filter, attributes = attributes)
#'xtradata_requete_features(typename  = "PC_CAPTE_P", key = MaCle,
#'filter = filter, attributes = attributesArray)

#'xtradata_requete_features(typename  = "PC_CAPTE_P", key = MaCle,
#'filter = filter, attributes = attributes, maxfeatures = 10)
#'xtradata_requete_features(typename  = "PC_CAPTE_P", key = MaCle,
#'filter = filter, attributes = attributesArray, maxfeatures = 10)

#'
#' }
xtradata_requete_features <- function(key = NULL,
                                      typename  = NULL,
                                      crs = "epsg:4326",
                                      filter = NULL,
                                      attributes = NULL,
                                      maxfeatures = NULL) {
  assert_that(!is.null(typename))
  assert_that(!is.null(key))
  assert_that(crs %in% c("epsg:4326", "epsg:3945", "epsg:2154", "epsg:3857"),
              msg = 'Les valeurs de crs autorisées sont "epsg:4326", "epsg:3945", "epsg:2154", "epsg:3857"')

  check_internet()

  base_url_xtradata_features <-  glue("http://data.bordeaux-metropole.fr/geojson/features/{typename}?")

  if(is.string(filter))   filter <- fromJSON(filter)
  if(is.string(attributes))   attributes <- fromJSON(attributes)


  parametres_requete <- list("key" = key, "crs" = crs,
                             "filter" = filter,  "attributes" = attributes,
                             "maxfeatures" = maxfeatures) %>% compact()

  params_encodes_pour_url <- map2(parametres_requete, names(parametres_requete), function(param, param_name) {

    if(vec_depth(param) == 1 & length(param) == 1) {
      # on doit transformer les listes et les vecteurs, si ce n'est pas le cas pas besoin de passer en JSON
      parametre_encode <- param
    } else {
      parametre_encode <- toJSON(param, auto_unbox = TRUE) %>% URLencode()
    }

    print(glue('&{param_name}={parametre_encode}'))

  })

  params_encodes_pour_url <- glue_collapse(params_encodes_pour_url, sep = "", width = Inf, last = "")

  url <- glue("{base_url_xtradata_features}{params_encodes_pour_url}")

  request <- GET(url)
  check_API_results(request)

  response <- content(request, as = "text", encoding = "UTF-8")

  return(fromJSON(response, flatten = TRUE)$features)

}





#' Renvoie les objets d'une couche (service features Xtradata)
#'
#' @param key Cle de connexion (string)
#'
#' @param typename Le nom de la couche (string)
#'
#' @param crs Système de coordonnees de sortie des geometries renvoyees par le serveur GeoJSON
#' valeurs autorisees : epsg:4326, epsg:3945, epsg:2154, epsg:3857
#'
#' @param filter Filtres à appliquer sur les donnees. Format liste R ou format JSON (string). Voir exemples
#'
#' @param attributes Liste des noms des attributs de la couche à retourner en resultat.
#'  Afin d'accelerer les traitements, listez uniquement les attributs que vous sont necessaires.
#'  Si non precise, tous les attributs seront retournes.
#'  Format vecteur R ou format array (string). Voir exemples
#'
#' @param maxfeatures Nombre maximum d'enregistrement à retourner.
#' Si 0 ou non precise, tous les enregistrements de la couche seront retournes
#'
#' @param backintime Donne accès aux données de la couche sélectionnée à une date donnée.
#'  Ces données étant issues de l'historique stocké sur des bases NoSQL,
#'  certains attributs / valeurs peuvent différer du mode classique sans le paramètre backintime,
#'  issu du SQL. (date ou datetime)
#'
#' @param showURL afficher l'url interrogee (boolean)
#'
#' @return un data frame issu de la requête
#' @export
#'
#' @importFrom assertthat assert_that is.string
#' @importFrom glue glue glue_collapse
#' @importFrom purrr map2 compact vec_depth map_chr
#' @importFrom httr GET content
#' @importFrom jsonlite fromJSON toJSON
#' @importFrom utils URLencode
#'
#' @references  http://data.bordeaux-metropole.fr/geojson/help/
#' @references https://data.bordeaux-metropole.fr/dicopub/#/dico
#'
#' @examples
#' \dontrun{
#' # appel sur la couche PC_CAPTE_P
#' filter <- list(
#'   "type" = "BOUCLE",
#'   "mdate" = list(
#'     "$gt" = "2020-01-01T08:00:00"
#'   )
#' )
#'
#'
#' filterJSON <- '{
#' "type": "BOUCLE",
#' "mdate": {
#'   "$gt": "2020-01-01T08:00:00"
#'  }
#' }'
#'
#' # 2 façons d'utiliser le paramètre filter
#' res1 <- xtradata_requete_features(
#'   typename = "PC_CAPTE_P", key = MaCle,
#'   filter = filter
#' )
#'
#' res2 <- xtradata_requete_features(
#'   typename = "PC_CAPTE_P", key = MaCle,
#'   filter = filterJSON
#' )
#'
#' all.equal(res1, res2)
#'
#' # 2 façons d'utiliser le paramètre attributes
#'
#' attributes <- list("cdate", "mdate")
#' attributesArray <- '["cdate", "mdate"]'
#'
#' res3 <- xtradata_requete_features(
#'   typename = "PC_CAPTE_P", key = MaCle,
#'   filter = filter, attributes = attributes
#' )
#'
#' res4 <- xtradata_requete_features(
#'   typename = "PC_CAPTE_P", key = MaCle,
#'   filter = filter, attributes = attributesArray
#' )
#'
#' all.equal(res3, res4)
#'
#' # limitation de la requete au 10 premiers resultats
#' res5 <- xtradata_requete_features(
#'   typename = "PC_CAPTE_P", key = MaCle,
#'   maxfeatures = 10
#' )
#' nrow(res5)
#'
#'
#' # les filtres sur un meme champ doivent etre combines avec les operateurs
#' # '$and', '$or', '$not'
#'
#' filterJSON_combined <- '{
#' "$and": [
#'  { "gid": {"$gte": "1"} },
#'  { "gid": {"$lte": "5"} }
#' ]
#' }'
#'
#' filter_combined <- list("$and" = list(
#'   list("gid" = list(
#'     "$gte" = "1"
#'   )),
#'
#'   list("gid" = list(
#'     "$lte" = "5"
#'   ))
#' ))
#'
#' res6 <- xtradata_requete_features(
#'   typename = "PC_CAPTE_P", key = MaCle,
#'   filter = filterJSON_combined
#' )
#'
#' res7 <- xtradata_requete_features(
#'   typename = "PC_CAPTE_P", key = MaCle,
#'   filter = filter_combined
#' )
#'
#' all.equal(res6, res7)
#'
#' # possibilite de fournir un tableau de donnees dans l'argument filter
#' filter_and <- list(
#'   "gid" = list("$in" = c(50:55))
#' )
#'
#'
#' filter_and_JSON <- '{
#' "gid": {
#'  "$in": [
#'    50,51,52,53,54,55
#'  ]
#' }}'
#'
#' res8 <- xtradata_requete_features(
#'   typename = "PC_CAPTE_P", key = MaCle,
#'   filter = filter_and
#' )
#'
#' res9 <- xtradata_requete_features(
#'   typename = "PC_CAPTE_P", key = MaCle,
#'   filter = filter_and_JSON
#' )
#'
#' all.equal(res8, res9)
#' }
#'
xtradata_requete_features <- function(key = NULL,
                                      typename = NULL,
                                      crs = "epsg:4326",
                                      filter = NULL,
                                      attributes = NULL,
                                      maxfeatures = NULL,
                                      backintime = NULL,
                                      showURL = FALSE) {
  assert_that(!is.null(typename))
  assert_that(!is.null(key))
  assert_that(crs %in% c("epsg:4326", "epsg:3945", "epsg:2154", "epsg:3857"),
    msg = 'Les valeurs de crs autorisees sont "epsg:4326", "epsg:3945", "epsg:2154", "epsg:3857"'
  )

  check_internet()

  base_url_xtradata_features <- glue("http://data.bordeaux-metropole.fr/geojson/features/{typename}?")

  if (is.string(filter)) filter <- fromJSON(filter)
  if (is.string(attributes)) attributes <- fromJSON(attributes)


  parametres_requete <- list(
    "filter" = filter,
    "key" = key, "crs" = crs,
    "attributes" = attributes,
    "maxfeatures" = maxfeatures, "backintime" = backintime
  ) %>% compact()

  params_encodes_pour_url <- map2(parametres_requete, names(parametres_requete), function(param, param_name) {
    # browser()
    if (vec_depth(param) == 1 & length(param) == 1) {
      # on doit transformer les listes et les vecteurs, si ce n'est pas le cas pas besoin de passer en JSON
      parametre_encode <- param
    } else {
      # cette partie va gérer les tableaux. 1er if : tableau de lg 1, 2eme if : tableau de lg >1
      if (length(unlist(param)) == 1) {
        parametre_encode <- toJSON(param, auto_unbox = FALSE) %>% URLencode()
      } else {
        parametre_encode <- toJSON(param, auto_unbox = TRUE) %>% URLencode()
      }
    }

    glue("&{param_name}={parametre_encode}")
  })

  params_encodes_pour_url <- glue_collapse(params_encodes_pour_url, sep = "", width = Inf, last = "")

  url <- glue("{base_url_xtradata_features}{params_encodes_pour_url}")
  if (showURL) print(url)

  request <- suppressWarnings(GET(url))
  check_API_results(request)

  response <- content(request, as = "text", encoding = "UTF-8")

  df <- fromJSON(response, flatten = TRUE)$features

  if (length(df) > 0) {
    colnames(df) <- map_chr(colnames(df), ~ gsub(x = ., pattern = "properties.", replacement = ""))
  }
  return(as.data.frame(df))
}

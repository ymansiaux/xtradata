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
#' @param orderby Tableau ordonné des attributs de tri.
#'  Par défaut, aucun tri n'est effectué et l'ordre des résultats est imprévisible
#'  Format vecteur R ou format array (string). Voir exemples
#'
#' @param backintime Donne accès aux données de la couche sélectionnée à une date donnée.
#'  Ces données étant issues de l'historique stocké sur des bases NoSQL,
#'  certains attributs / valeurs peuvent différer du mode classique sans le paramètre backintime,
#'  issu du SQL. (date ou datetime)
#'  Le paramètre accepte une date ou une datetime telle que définie dans la RFC 3339,
#'  section 5.6 ou tout format accepté par l'objet JavaScript Date.
#'  une macro relative à la date / heure actuelle, composée d'un nombre et d'une unité parmi min, hour, day ou month. Exemple "-15min" ou "-1 month"
#'
#' @param showURL afficher l'url interrogee (boolean)
#' @param useHTTPS url en HTTPS (boolean)
#'
#' @return un data frame issu de la requête
#' @export
#'
#' @importFrom assertthat assert_that is.string
#' @importFrom glue glue glue_collapse
#' @importFrom purrr map2 compact vec_depth map_chr map
#' @importFrom curl curl_fetch_memory
#' @importFrom jsonlite fromJSON toJSON
#' @importFrom geojsonsf geojson_sf
#' @importFrom utils URLencode
#'
#' @references http://data.bordeaux-metropole.fr/geojson/help/
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
#'
#' # Utilisation de l'argument backintime avec une date ou un datetime
#'
#'filter <- list(
#' "type" = "BOUCLE",
#' "mdate" = list(
#'   "$gt" = "2020-01-01T08:00:00"
#' )
#' )
#'
#' res10 <- xtradata_requete_features(
#'   typename = "PC_CAPTE_P", key = MaCle,
#'   filter = filter,
#'   backintime = "2021-06-01T10:00:00"
#' )
#'
#' tail(res10$mdate)
#'
#' res11 <- xtradata_requete_features(
#'   typename = "PC_CAPTE_P", key = MaCle,
#'   filter = filter,
#'   backintime = "2021-06-01"
#' )
#'
#' tail(res11$mdate)
#'
#' #' # Utilisation de l'argument backintime avec ne macro relative à la date / heure actuelle
#' #' #  composée d'un nombre et d'une unité parmi min, hour, day ou month
#'
#' res12 <- xtradata_requete_features(
#'   typename = "PC_CAPTE_P", key = MaCle,
#'   filter = filter,
#'   backintime = "-30min"
#' )
#'
#' tail(res12$mdate)
#'
#'
#' res13 <- xtradata_requete_features(
#'   typename = "PC_CAPTE_P", key = MaCle,
#'   filter = filter,
#'   backintime = "-5hour"
#' )
#'
#' tail(res13$mdate)
#'
#' res14 <- xtradata_requete_features(
#'   typename = "PC_CAPTE_P", key = MaCle,
#'   filter = filter,
#'   backintime = "-7day"
#' )
#'
#' tail(res14$mdate)
#'
#'
#' res15 <- xtradata_requete_features(
#'   typename = "PC_CAPTE_P", key = MaCle,
#'   filter = filter,
#'   backintime = "-1month"
#' )
#'
#' tail(res15$mdate)
#'
#' # 2 façons d'utiliser le paramètre orderby
#' orderby <- list("mdate", "gid")
#' orderbyArray <- '["mdate", "gid"]'
#'
#' res16 <- xtradata_requete_features(
#'   typename = "PC_CAPTE_P", key = MaCle,
#'   filter = filter,
#'   orderby = orderby,
#'   showURL = TRUE
#' )
#'
#' res17 <- xtradata_requete_features(
#'   typename = "PC_CAPTE_P", key = MaCle,
#'   filter = filter,
#'   orderby = orderbyArray,
#'   showURL = TRUE
#' )
#' res17
#'
#' all.equal(res16, res17)
#'}
#'
xtradata_requete_features <- function(key = NULL,
                                      typename = NULL,
                                      crs = "epsg:4326",
                                      filter = NULL,
                                      attributes = NULL,
                                      maxfeatures = NULL,
                                      orderby = NULL,
                                      backintime = NULL,
                                      showURL = FALSE,
                                      useHTTPS = TRUE) {
  assert_that(!is.null(typename))
  assert_that(!is.null(key))
  assert_that(crs %in% c("epsg:4326", "epsg:3945", "epsg:2154", "epsg:3857"),
              msg = 'Les valeurs de crs autorisees sont "epsg:4326", "epsg:3945", "epsg:2154", "epsg:3857"'
  )

  check_internet()

  base_url_xtradata_features <- ifelse(useHTTPS, glue("https://data.bordeaux-metropole.fr/geojson/features/{typename}?"),
                                       glue("http://data.bordeaux-metropole.fr/geojson/features/{typename}?"))



  if (is.string(filter)) filter <- fromJSON(filter)
  if (is.string(attributes)) attributes <- fromJSON(attributes)
  if (is.string(orderby)) orderby <- fromJSON(orderby)


  parametres_requete <- list(
    "filter" = filter,
    "key" = key, "crs" = crs,
    "attributes" = attributes,
    "maxfeatures" = maxfeatures,
    "backintime" = backintime,
    "orderby" = orderby
  ) %>% compact()

  params_encodes_pour_url <- map2(parametres_requete, names(parametres_requete), function(param, param_name) {

    if (vec_depth(param) == 1 & length(param) == 1) {
      # on gere ici les elements à un niveau clé <-> valeur : ex key = MaCle ou rangeStart = une date quelconque
      parametre_encode <- param
    } else {   # ici element plus complexes, ex les listes avec des sous niveau : les filters ou les rangeStep

      # il y avait un problème avec l'unboxing des éléments contenus dans des $in qui étaient de longueur 1, une retransformation en liste permet de régler le pb
      if(param_name == "filter") {
        param <- map(param, function(.x) {
          if("$in" %in% names(.x)) {
            .x[[1]] <- as.list(.x[[1]])
          }
          return(.x)
        })
      }

      parametre_encode <- toJSON(param, auto_unbox = TRUE) %>% URLencode()
    }

    glue("&{param_name}={parametre_encode}")

  })

  params_encodes_pour_url <- glue_collapse(params_encodes_pour_url, sep = "", width = Inf, last = "")

  url <- glue("{base_url_xtradata_features}{params_encodes_pour_url}")
  if (showURL) print(url)

  request <- curl_fetch_memory(url)

  check_API_results(request)

  response <- rawToChar(request$content)

  df <- try(geojson_sf(response), silent = TRUE)

  if(inherits(df, "try-error")) { # si on a pas de colonne géographique, on ne traite pas les résultats comme du geojson
    df <- fromJSON(response, flatten = TRUE)$features

    if (length(df) > 0) {
      colnames(df) <- map_chr(colnames(df), ~ gsub(x = ., pattern = "properties.", replacement = ""))
    }
  }

  return(df)
}

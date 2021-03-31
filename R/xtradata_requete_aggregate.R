#' Renvoie une aggregation des objets historisés sur une période et un pas de temps donné
#' (service aggregate Xtradata)
#'
#' @param key Cle de connexion (string) (requis)
#'
#' @param typename Le nom de la couche (string) (requis)
#'
#' @param rangeStart Date / heure de début de la fenêtre de temps.
#' La valeur sera arrondie à l'heure / au jour ou au mois entamé (selon la valeur du paramètre rangeStep).
#' Le paramètre accepte une date ou une datetime telle que définie dans la RFC 3339, section 5.6
#' ou tout format accepté par l'objet JavaScript Date. (date ou datetime) (requis)
#'
#' @param rangeEnd Date / heure de fin de la fenêtre de temps.
#' La valeur sera arrondie à l'heure / au jour ou au mois supérieur à celui entamé (selon la valeur du paramètre rangeStep).
#'  Si aucune valeur n'est précisé, la date courante sera utilisée.
#' Le paramètre accepte une date ou une datetime telle que définie dans la RFC 3339, section 5.6
#' ou tout format accepté par l'objet JavaScript Date. (date ou datetime)
#'
#' @param rangeStep Type de pas à utiliser pour découper les données.
#' valeurs autorisées : hour (defaut), day, month (string)
#'
#' @param rangeFilter Filtres temporels à appliquer à l'intérieur de la fenêtre de temps
#' * hours - Tableau des heure(s) de la journée à retourner, de 0 à 23.
#' * days - Tableau des numéros de jours de la semaine à retourner, de 1 à 7, avec 1 pour dimanche et 7 pour samedi.
#' * publicHolidays - true pour renvoyer uniquement les occurences des jours fériés. False désactive le filtre
#' Format liste R ou format JSON (string). Voir exemples
#'
#' @param filter Filtres à appliquer sur les données. Format liste R ou format JSON (string). Voir exemples
#'
#' @param attributes Liste des noms des attributs de la couche à retourner en résultat.
#'  Afin d'accélérer les traitements, listez uniquement les attributs que vous sont nécessaires.
#'  Si non précisé, tous les attributs seront retournés.
#'  Format vecteur R ou format array (string). Voir exemples
#'  Des paramètres additionnels peuvent également être utilisés. Dans ce cas format liste R ou format objet JSON
#'  Voir exemples et réferences
#'
#' @param filter Filtres à appliquer sur les données. Format liste R ou format JSON (string). Voir exemples
#'
#' @param showURL afficher l'url interrogee (boolean)

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
#' # appel sur la couche ST_PARK_P
#'
#' # 2 façons d'utiliser le paramètre filter
#'
#' filter <- list(
#'   "ident" = "CUBPK88",
#'   "etat" = "LIBRE",
#'   "libres" = list(
#'     "$gt" = 100
#'   )
#' )
#'
#' filterJSON <- '{
#' "ident": "CUBPK88",
#' "etat" : "LIBRE",
#' "libres": {
#'  "$gt": 100
#' }
#' }'
#'
#' res1 <- xtradata_requete_aggregate(
#'   typename = "ST_PARK_P", key = MaCle,
#'   rangeStart = "2020-08-01",
#'   rangeEnd = "2020-08-16",
#'   rangeStep = "hour",
#'   filter = filter
#' )
#'
#' res2 <- xtradata_requete_aggregate(
#'   typename = "ST_PARK_P", key = MaCle,
#'   rangeStart = "2020-08-01",
#'   rangeEnd = "2020-08-16",
#'   rangeStep = "hour",
#'   filter = filterJSON
#' )
#'
#'
#' all.equal(res1, res2)
#'
#'
#' # 2 façons d'utiliser le paramètre rangeFilter
#'
#' rangeFilter <- list(
#'   "hours" = 5:6,
#'   "days" = 1:7,
#'   "publicHolidays" = FALSE
#' )
#' rangeFilterJSON <- '{
#'   "hours": [
#'     5,6
#'   ],
#'   "days": [
#'     1,2,3,4,5,6,7
#'   ],
#'   "publicHolidays": false
#' }'
#'
#' res3 <- xtradata_requete_aggregate(
#'   typename = "ST_PARK_P", key = MaCle,
#'   rangeStart = "2020-08-01",
#'   rangeEnd = "2020-08-16",
#'   rangeStep = "hour",
#'   rangeFilter = rangeFilter,
#'   attributes = list("gid", "libres", "total", "etat", "ident"),
#'   filter = filter
#' )
#'
#' res4 <- xtradata_requete_aggregate(
#'   typename = "ST_PARK_P", key = MaCle,
#'   rangeStart = "2020-08-01",
#'   rangeEnd = "2020-08-16",
#'   rangeStep = "hour",
#'   rangeFilter = rangeFilterJSON,
#'   attributes = list("gid", "libres", "total", "etat", "ident"),
#'   filter = filter
#' )
#'
#'
#' all.equal(res3, res4)
#'
#'
#'
#' # 4 facons d'utiliser le parametre attributes
#' attributes <- list("gid", "libres")
#' attributesArray <- '["gid", "libres"]'
#'
#' res5 <- xtradata_requete_aggregate(
#'   typename = "ST_PARK_P", key = MaCle,
#'   rangeStart = "2020-08-01",
#'   rangeEnd = "2020-08-16",
#'   rangeStep = "hour",
#'   rangeFilter = rangeFilterJSON,
#'   attributes = attributes,
#'   filter = filter
#' )
#'
#'
#' res6 <- xtradata_requete_aggregate(
#'   typename = "ST_PARK_P", key = MaCle,
#'   rangeStart = "2020-08-01",
#'   rangeEnd = "2020-08-16",
#'   rangeStep = "hour",
#'   rangeFilter = rangeFilterJSON,
#'   attributes = attributesArray,
#'   filter = filter
#' )
#'
#'
#' res5
#' res6
#'
#' all.equal(res5, res6)
#'
#' attributes_key_value_list <- list("gid" = "first", "libres" = "max")
#' attributes_key_value_JSON <- '{"gid" : "first", "libres" : "max"}'
#'
#' res7 <- xtradata_requete_aggregate(
#'   typename = "ST_PARK_P", key = MaCle,
#'   rangeStart = "2020-08-01",
#'   rangeEnd = "2020-08-16",
#'   rangeStep = "hour",
#'   rangeFilter = rangeFilterJSON,
#'   attributes = attributes_key_value_list,
#'   filter = filter
#' )
#'
#'
#' res8 <- xtradata_requete_aggregate(
#'   typename = "ST_PARK_P", key = MaCle,
#'   rangeStart = "2020-08-01",
#'   rangeEnd = "2020-08-16",
#'   rangeStep = "hour",
#'   rangeFilter = rangeFilterJSON,
#'   attributes = attributes_key_value_JSON,
#'   filter = filter
#' )
#'
#'
#' res7
#' res8
#'
#' all.equal(res7, res8)
#'
#' #' # les filtres sur un meme champ doivent etre combines avec les operateurs
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
#' res9 <- xtradata_requete_features(
#'   typename = "ST_PARK_P", key = MaCle,
#'   filter = filterJSON_combined,
#' )
#'
#' res10 <- xtradata_requete_features(
#'   typename = "ST_PARK_P", key = MaCle,
#'   filter = filter_combined
#' )
#'
#' all.equal(res9, res10)
#'
#'
#' # possibilite de fournir un tableau de donnees dans l'argument filter
#'
#' filter_and <- list(
#'   "gid" = list("$in" = c(247, 593))
#' )
#'
#'
#' filter_and_JSON <- '{
#' "gid": {
#'  "$in": [
#'    247,593
#'  ]
#' }}'
#'
#' res11 <- xtradata_requete_aggregate(
#'   key = MaCle,
#'   typename = "ST_PARK_P",
#'   rangeStart = "2021-02-20", rangeEnd = "2021-02-21",
#'   rangeStep = "hour",
#'   attributes = list("gid", "libres", "total", "etat"),
#'   filter = filter_and
#' )
#'
#' res12 <- xtradata_requete_aggregate(
#'   key = MaCle,
#'   typename = "ST_PARK_P",
#'   rangeStart = "2021-02-20", rangeEnd = "2021-02-21",
#'   rangeStep = "hour",
#'   attributes = list("gid", "libres", "total", "etat"),
#'   filter = filter_and_JSON
#' )
#'
#' all.equal(res11, res12)
#' }
#'
xtradata_requete_aggregate <- function(key = NULL,
                                       typename = NULL,
                                       rangeStart = NULL,
                                       rangeEnd = NULL,
                                       rangeStep = NULL,
                                       rangeFilter = list(
                                         "hours" = 0:23,
                                         "days" = 1:7,
                                         "publicHolidays" = FALSE
                                       ),
                                       attributes = NULL,
                                       filter = NULL,
                                       showURL = FALSE) {
  assert_that(!is.null(typename))
  assert_that(!is.null(key))
  assert_that(!is.null(rangeStart))


  check_internet()

  base_url_xtradata_aggregate <- glue("http://data.bordeaux-metropole.fr/geojson/aggregate/{typename}?")

  if (is.string(filter)) filter <- fromJSON(filter)
  if (is.string(rangeFilter)) rangeFilter <- fromJSON(rangeFilter)
  if (is.string(attributes)) attributes <- fromJSON(attributes)


  parametres_requete <- list(
    "key" = key, "rangeStart" = rangeStart, "rangeEnd" = rangeEnd,
    "rangeStep" = rangeStep, "rangeFilter" = rangeFilter,
    "filter" = filter, "attributes" = attributes
  ) %>% compact()

  params_encodes_pour_url <- map2(parametres_requete, names(parametres_requete), function(param, param_name) {
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

  url <- glue("{base_url_xtradata_aggregate}{params_encodes_pour_url}")
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

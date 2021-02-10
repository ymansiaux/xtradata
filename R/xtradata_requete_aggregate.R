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
#'
#' @param filter Filtres à appliquer sur les données. Format liste R ou format JSON (string). Voir exemples

#' @return un data frame issu de la requête
#' @export
#'
#' @importFrom assertthat assert_that is.string
#' @importFrom glue glue glue_collapse
#' @importFrom purrr map2 compact vec_depth
#' @importFrom httr GET content
#' @importFrom jsonlite fromJSON toJSON
#' @importFrom utils URLencode
#'
#' @references  http://data.bordeaux-metropole.fr/geojson/help/
#' @references https://data.bordeaux-metropole.fr/dicopub/#/dico
#'
#' @examples \dontrun{
#' # appel sur la couche ST_PARK_P
#'
#' filter <- list("ident" = "CUBPK88",
#'    "etat" = "LIBRE",
#'                 "libres" = list(
#'                   '$gt' = 100)
#' )
#' rangeFilter <- list("hours" = 5:6,
#'                     "days" = 1:7,
#'                     "publicHolidays" = FALSE
#'                      )
#' rangeFilterJSON <- '{
#'   "hours": [
#'     5,6
#'   ],
#'   "days": [
#'     1,2,3,4,5,6,7
#'   ],
#'   "publicHolidays": false
# }'
#'
#' # 2 façons d'utiliser le paramètre rangeFilter
#'
#' res1 <- xtradata_requete_aggregate(typename  = "ST_PARK_P", key = MaCle,
#'                                    rangeStart = "2020-08-01",
#'                                    rangeEnd = "2020-08-16",
#'                                    rangeStep = "hour",
#'                                    rangeFilter = rangeFilter,
#'                                    attributes = list("gid", "libres", "total", "etat", "ident"),
#'                                    filter = filter)
#'
#' res2 <- xtradata_requete_aggregate(typename  = "ST_PARK_P", key = MaCle,
#'                                    rangeStart = "2020-08-01",
#'                                    rangeEnd = "2020-08-16",
#'                                    rangeStep = "hour",
#'                                    rangeFilter = rangeFilterJSON,
#'                                    attributes = list("gid", "libres", "total", "etat", "ident"),
#'                                    filter = filter)
#'
#'
#' all.equal(res1, res2)
#'
#'
#'
#'
#'filterJSON <-
#' '{
#'
#' "type": "BOUCLE",
#' "mdate": {
#'   "$gt": "2020-01-01T08:00:00"
#'  }
#'}
#''
#'
#' attributes <- c("cdate", "mdate")
#' attributesArray <- '["cdate", "mdate"]'
#'
#' # 2 façons d'utiliser le paramètre filter
#'xtradata_requete_features(typename  = "PC_CAPTE_P", key = MaCle,
#'filter = filter)
#'xtradata_requete_features(typename  = "PC_CAPTE_P", key = MaCle,
#'filter = filterJSON)
#'
#' # 2 façons d'utiliser le paramètre attributes
#'xtradata_requete_features(typename  = "PC_CAPTE_P", key = MaCle,
#'filter = filter, attributes = attributes)
#'xtradata_requete_features(typename  = "PC_CAPTE_P", key = MaCle,
#'filter = filter, attributes = attributesArray)
#'
#' # limitation de la requete au 10 premiers resultats
#'xtradata_requete_features(typename  = "PC_CAPTE_P", key = MaCle,
#'maxfeatures = 10)
#'
#' }
#'
xtradata_requete_aggregate <- function(key = NULL,
                                       typename  = NULL,
                                       rangeStart = NULL,
                                       rangeEnd = NULL,
                                       rangeStep = NULL,
                                       rangeFilter = list(
                                         "hours" = 0:23,
                                         "days" = 1:7,
                                         "publicHolidays" = FALSE
                                       ),
                                       attributes = NULL,
                                       filter = NULL) {
  assert_that(!is.null(typename))
  assert_that(!is.null(key))
  assert_that(!is.null(rangeStart))

  # browser()

  check_internet()

  base_url_xtradata_aggregate <-  glue("http://data.bordeaux-metropole.fr/geojson/aggregate/{typename}?")

  if(is.string(filter))   filter <- fromJSON(filter)
  if(is.string(rangeFilter))   rangeFilter <- fromJSON(rangeFilter)
  if(is.string(attributes))   attributes <- fromJSON(attributes)


  parametres_requete <- list("key" = key, "rangeStart" = rangeStart, "rangeEnd" = rangeEnd,
                             "rangeStep" = rangeStep, "rangeFilter" = rangeFilter,
                             "filter" = filter,  "attributes" = attributes) %>% compact()

  params_encodes_pour_url <- map2(parametres_requete, names(parametres_requete), function(param, param_name) {

    if(vec_depth(param) == 1 & length(param) == 1) {
      # on doit transformer les listes et les vecteurs, si ce n'est pas le cas pas besoin de passer en JSON
      parametre_encode <- param
    } else {
      parametre_encode <- toJSON(param, auto_unbox = TRUE) %>% URLencode()
    }

    glue('&{param_name}={parametre_encode}')

  })

  params_encodes_pour_url <- glue_collapse(params_encodes_pour_url, sep = "", width = Inf, last = "")

  url <- glue("{base_url_xtradata_aggregate}{params_encodes_pour_url}")

  request <- GET(url)
  check_API_results(request)

  response <- content(request, as = "text", encoding = "UTF-8")

  return(fromJSON(response, flatten = TRUE)$features)

}

 MaCle <- "DATAZBOUBB"

 filter <- list("ident" = "CUBPK88",
    "etat" = "LIBRE",
                 "libres" = list(
                   '$gt' = 100)
 )


 res1 <- xtradata_requete_aggregate(typename  = "ST_PARK_P", key = MaCle,
                                    rangeStart = "2020-08-01",
                                    rangeEnd = "2020-08-16",
                                    rangeStep = "hour",
                                    rangeFilter = list(
                                      "hours" = 5:6,
                                      "days" = 1:7,
                                      "publicHolidays" = FALSE
                                    ),
                                    attributes = c("gid", "libres", "total", "etat", "ident"),
                                    filter = filter)

 res1

 rangeFilterJSON <- '{
   "hours": [
     5,6
   ],
   "days": [
     1,2,3,4,5,6,7
   ],
   "publicHolidays": false
 }'

 res2 <- xtradata_requete_aggregate(typename  = "ST_PARK_P", key = MaCle,
                                    rangeStart = "2020-08-01",
                                    rangeEnd = "2020-08-16",
                                    rangeStep = "hour",
                                    rangeFilter = rangeFilterJSON,
                                    attributes = c("gid", "libres", "total", "etat", "ident"),
                                    filter = filter)




 res1
 res2


 all.equal(res1,res2)


attributes <- list("gid", "libres")
attributesArray <- '["gid", "libres"]'

attributes_key_value <-


res3 <- xtradata_requete_aggregate(typename  = "ST_PARK_P", key = MaCle,
                                   rangeStart = "2020-08-01",
                                   rangeEnd = "2020-08-16",
                                   rangeStep = "hour",
                                   rangeFilter = rangeFilterJSON,
                                   attributes = attributes,
                                   filter = filter)


res4 <- xtradata_requete_aggregate(typename  = "ST_PARK_P", key = MaCle,
                                   rangeStart = "2020-08-01",
                                   rangeEnd = "2020-08-16",
                                   rangeStep = "hour",
                                   rangeFilter = rangeFilterJSON,
                                   attributes = attributesArray,
                                   filter = filter)


res3
res4

all.equal(res3, res4)

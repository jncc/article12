#' Species information
#' 
#' A dataset containing the Article 12 species information, taken from
#' the Birds Directive reporting portal, limited to that required by UK. 
#' Checklist for bird species last  updated 2018-07-05
#' 
#' @format A tibble with 319 rows and 8 variables
#' \describe{
#'   \item{speciescode}{bird species code}
#'   \item{euringcode}{European bird ringing schemes species code}
#'   \item{speciesname}{scientific name}
#'   \item{sub_unit}{bird sub_unit}
#'   \item{common_name}{bird common name}
#'   \item{season}{breeding, winter or passage}
#'   \item{occurrence}{present or colonised}
#'   \item{recommended_unit}{recommended population count unit}
#' }
"information_assessment_look_up"

#' Common name information
#' 
#' A dataset a look up between the common names found in the 
#' Article 12 spreadsheets and Article 12 species codes. 
#' Match is used to indicate common names with a unique species
#' code or where there is more than one species code. For these 
#' common names additional information is provided to allow a 
#' manual selection of the correct species code.
#' 
#' 
#' #' @format A tibble with 334 rows and 6 variables
#' \describe{
#'   \item{speciescode}{bird species code}
#'   \item{common_name}{bird common name}
#'   \item{additional_info}{additional name information}
#'   \item{common_name_match}{unique species code for common name or not}
#'   \item{scientific_name}{article 12 checklist scientific name}
#'   \item{sub_unit}{article 12 checklist sub unit}
#' }
"information_common_name_look_up"
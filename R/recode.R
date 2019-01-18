#' Recode season
#'
#'  This function changes the season text to
#'  the abbreviated text used in the reporting tool
#'
#' @param season character, season text
#'
#' @return character, season abbreviated text
#' @export
#'
#' @examples
#' recode_season("Breeding")
recode_season <- function(season) {
  
  tibble::as.tibble(season) %>% 
    dplyr::mutate(value = dplyr::case_when
                  (
                    stringr::str_detect(stringr::str_to_lower(value), "breeding") ~ "B",
                    stringr::str_detect(stringr::str_to_lower(value), "passage") ~ "P",
                    stringr::str_detect(stringr::str_to_lower(value), "winter") ~ "W",
                    TRUE ~ value
                  )) %>% 
    unlist()
}

#' Recode population units
#' 
#' This function changes the descriptive population units text to
#' the abbreviated text used in the reporting tool 
#'
#' @param population_unit character, population unit text
#'
#' @return character, population unit abbreviated text
#' @export
#'
#' @examples
#' recode_population_units("number of adults")
recode_population_units <- function(population_unit) {
  
  tibble::as.tibble(population_unit) %>% 
    dplyr::mutate(value = dplyr::case_when
                  (
                    stringr::str_to_lower(value) ==  "number of breeding females"  ~  "bfemales",
                    stringr::str_to_lower(value) ==  "number of calling males"  ~  "cmales",
                    stringr::str_to_lower(value) ==  "number of individuals"  ~  "i",
                    stringr::str_to_lower(value) ==  "number of males"  ~  "males",
                    stringr::str_to_lower(value) ==  "number of pairs"  ~  "p",
                    TRUE ~ value
                  )) %>% 
    unlist()
}

#' Recode type of estimate
#' 
#' This function changes the descriptive type of estimate
#' text to the abbreviated text used in the reporting tool 
#'
#' @param estimate character, type of estimate text
#'
#' @return character, type of estimate abbreviated text
#' @export
#'
#' @examples
#' recode_type_of_estimate("Multi-year mean")
recode_type_of_estimate <- function(estimate) {
  
  tibble::as.tibble(estimate) %>% 
    dplyr::mutate(value = dplyr::case_when
                  (
                    stringr::str_detect(stringr::str_to_lower(value), "best estimate") ~ "estimate",
                    stringr::str_detect(stringr::str_to_lower(value), "confidence interval") ~ "interval",
                    stringr::str_detect(stringr::str_to_lower(value), "multi-year mean") ~ "mean",
                    stringr::str_detect(stringr::str_to_lower(value), "minimum") ~ "minimum",
                    TRUE ~ value
                  )) %>% 
    unlist()
}

#' Recode methods used
#'
#' This function changes the descriptive methods used text to
#' the abbreviated text used in the reporting tool
#'
#' @param methods_used character, method used text
#'
#' @return character, methods used abbreviated text
#' @export
#'
#' @examples
#' recode_methods_used("Based mainly on expert opinion with very limited data")
recode_methods_used <- function(methods_used) {
  
  tibble::as.tibble(methods_used) %>% 
    dplyr::mutate(value = dplyr::case_when
                  (
                    stringr::str_detect(stringr::str_to_lower(value), "complete survey") ~ "completeSurvey",
                    stringr::str_detect(stringr::str_to_lower(value), "extrapolation") ~ "estimatePartial",
                    stringr::str_detect(stringr::str_to_lower(value), "^based mainly on expert opinion") ~ "estimateExpert",
                    stringr::str_detect(stringr::str_to_lower(value), "^insufficient or no data") ~ "absentData",
                    TRUE ~ value
                  )) %>% 
    unlist()
}

#' Reverse yes and no
#'
#' This function reverses yes and no as species categories 5.11, 6.16 
#' and 11.7 in data capture spreadsheet ask for change (yes or no) where 
#' as in reporting tool the same categories ask for no change (yes or no)
#'
#' Blanks are converted to yes (no change) in the reporting tool as the
#' reporting tool only stores two states yes or no
#'
#' @param yes_no character, yes, no or NA_character_
#'
#' @return character, yes and no are swopped around, blank converted to yes
#' @export
#'
#' @examples
#' reverse_yes_no(c("Yes", "No", NA))
reverse_yes_no <- function(yes_no_blank) {
  
  tibble::as.tibble(yes_no_blank) %>% 
    dplyr::mutate(value = dplyr::case_when
                  (
                    stringr::str_to_lower(value) == "no" ~ "Yes",
                    is.na(value) ~ "Yes",
                    stringr::str_to_lower(value) == "yes" ~ "No",
                    TRUE ~ value
                  )) %>% 
    unlist()
  
}

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
                    stringr::str_detect(stringr::str_to_lower(value), "calling males")  ~  "cmales",
                    stringr::str_detect(stringr::str_to_lower(value), "individuals")  ~  "i",
                    stringr::str_to_lower(value) ==  "number of males"  ~  "males",
                    stringr::str_detect(stringr::str_to_lower(value), "^males")  ~  "males",
                    stringr::str_detect(stringr::str_to_lower(value), "pairs")  ~  "p",
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
                    stringr::str_detect(stringr::str_to_lower(value), "based mainly on expert opinion") ~ "estimateExpert",
                    stringr::str_detect(stringr::str_to_lower(value), "insufficient or no data") ~ "absentData",
                    stringr::str_detect(stringr::str_to_lower(value), "insufficent or no data") ~ "absentData",
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

#' Recode trends
#' 
#' This function changes the descriptive trends text to
#' the abbreviated text used in the reporting tool 
#'
#' @param trends character, trends text
#'
#' @return character, abbreviated trends text
#' @export
#'
#' @examples
#' recode_trends("Increasing (+)")
recode_trends <- function(trends) {
  
  tibble::as.tibble(trends) %>% 
    dplyr::mutate(value = dplyr::case_when
                  (
                    stringr::str_detect(stringr::str_to_lower(value), "decreasing") ~ "D",
                    stringr::str_to_lower(value) == '-' ~ "D",
                    stringr::str_detect(stringr::str_to_lower(value), "fluctuating") ~ "F",
                    stringr::str_detect(stringr::str_to_lower(value), "increasing") ~ "I",
                    stringr::str_to_lower(value) == '+' ~ "I",
                    stringr::str_detect(stringr::str_to_lower(value), "stable") ~ "S",
                    stringr::str_to_lower(value) == '0' ~ "S",
                    stringr::str_detect(stringr::str_to_lower(value), "^uncertain") ~ "U",
                    stringr::str_detect(stringr::str_to_lower(value), "^unknown") ~ "UNK",
                    stringr::str_to_lower(value) == 'x' ~ "UNK",
                    stringr::str_detect(stringr::str_to_lower(value), "extinct") ~ NA_character_, # David Stroud recommendation
                    TRUE ~ value
                  )) %>% 
    unlist()
}

#' Recode plans
#'
#' This function changes the descriptive plans text to
#' the abbreviated text used in the reporting tool 
#'
#' @param plans character, plans text
#'
#' @return character, abbreviated plans text
#' @export
#'
#' @examples
#' recode_plans("Brief Management Statement")
recode_plans <- function(plans) {
  
  tibble::as.tibble(plans) %>% 
    dplyr::mutate(value = dplyr::case_when
                  (
                    stringr::str_to_lower(value) == "brief management statement" ~ "BMS",
                    stringr::str_to_lower(value) == "management plan" ~ "MP",
                    stringr::str_to_lower(value) == "no plan" ~ "NA",
                    stringr::str_to_lower(value) == "species action plan" ~ "SAP",
                    TRUE ~ value
                  )) %>% 
    unlist()
}

#' Effectiveness of SAPs for globally threatened species
#' 
#' This function changes the effectiveness SAP text to
#' the abbreviated text used in the reporting tool 
#'
#' @param effectiveness character, effectiveness text
#'
#' @return character, abbreviated effectiveness text
#' @export
#'
#' @examples
#' recode_effectiveness_sap("further deteriorating away from the plan’s aim/objective(s)")
recode_effectiveness_sap <- function(effectiveness) {
  
  tibble::as.tibble(effectiveness) %>% 
    dplyr::mutate(value = dplyr::case_when
                  (
                    stringr::str_detect(stringr::str_to_lower(value), "further deteriorating") ~ "deteriorating",
                    stringr::str_detect(stringr::str_to_lower(value), "moving towards") ~ "towards",
                    stringr::str_detect(stringr::str_to_lower(value), "unchanged") ~ "unchanged",
                    TRUE ~ value
                  )) %>% 
    unlist()
}

#' Effectiveness of management plans for huntable species in non-Secure status
#' 
#' This function changes the effectiveness SAP text to
#' the abbreviated text used in the reporting tool 
#'
#' @param effectiveness character, effectiveness text
#'
#' @return character, abbreviated effectiveness text
#' @export
#'
#' @examples
#' recode_effectiveness_mp("further deteriorating") 
recode_effectiveness_mp <- function(effectiveness) {
  
  tibble::as.tibble(effectiveness) %>% 
    dplyr::mutate(value = dplyr::case_when
                  (
                    stringr::str_detect(stringr::str_to_lower(value), "further deteriorating") ~ "deteriorating",
                    stringr::str_detect(stringr::str_to_lower(value), "improving") ~ "improving",
                    stringr::str_detect(stringr::str_to_lower(value), "unchanged") ~ "unchanged",
                    TRUE ~ value
                  )) %>% 
    unlist()
}

#' Recode ranking
#'
#' This function changes the descriptive ranking text
#' to the abbreviated text used in the reporting tool 
#'
#' @param ranking character, ranking text
#'
#' @return character, abbreviated ranking text
#' @export
#'
#' @examples
#' recode_ranking("high importance")
recode_ranking <- function(ranking) {
  
  tibble::as.tibble(ranking) %>% 
    dplyr::mutate(value = dplyr::case_when
                  ( 
                    stringr::str_to_lower(value) == "high importance" ~ "H",
                    stringr::str_to_lower(value) == "medium importance" ~ "M",
                    TRUE ~ value
                  )) %>% 
    unlist()
}

#' Recode pressure and threat locations
#'
#' This function changes the descriptive location text
#' to the abbreviated text used in the reporting tool.
#' Includes both the numeric and text location options
#'
#' @param location character, location text
#'
#' @return character, abbreviated location text
#' @export
#'
#' @examples
#' #' recode_pressure_threat_locations("inside the Member State")
recode_pressure_threat_locations <- function(location) {
  
  tibble::as.tibble(location) %>% 
    dplyr::mutate(value = dplyr::case_when
                  ( 
                    stringr::str_to_lower(value) == "4" ~ "inMS",
                    stringr::str_to_lower(value) == "inside the member state" ~ "inMS",
                    stringr::str_to_lower(value) == "3" ~ "elseEU",
                    stringr::str_to_lower(value) == "elsewhere in the eu" ~ "elseEU",
                    stringr::str_to_lower(value) == "2" ~ "outEU",
                    stringr::str_to_lower(value) == "outside eu" ~ "outEU",
                    stringr::str_to_lower(value) == "1" ~ "inOutEU",
                    stringr::str_to_lower(value) == "both inside and outside eu" ~ "inOutEU",
                    stringr::str_to_lower(value) == "x" ~ "Unk",
                    stringr::str_to_lower(value) == "unknown" ~ "Unk",
                    TRUE ~ value
                  )) %>% 
    unlist()
}

#' Recode measures identified
#' 
#' This function changes the descriptive measures identified text
#' to the abbreviated text used in the reporting tool 
#'
#' @param measure character, measures identified text
#'
#' @return character, measures identified abbreviated text
#' @export
#'
#' @examples
#' recode_measures_identified("Measures needed but cannot be identified")
recode_measures_identified <- function(measure) {
  
  tibble::as.tibble(measure) %>% 
    dplyr::mutate(value = dplyr::case_when
                  (
                    stringr::str_detect(stringr::str_to_lower(value), "none yet taken") ~ "ident",
                    stringr::str_to_lower(value) == "measures_status_ident" ~ "ident",
                    stringr::str_detect(stringr::str_to_lower(value), "cannot be identified") ~ "notident",
                    stringr::str_to_lower(value) == "measures_status_notident" ~ "notident",
                    stringr::str_detect(stringr::str_to_lower(value), "identified and taken") ~ "taken",
                    stringr::str_to_lower(value) == "measures_status_taken" ~ "taken",
                    TRUE ~ value
                  )) %>% 
    unlist()
}

#' Recode species measures purpose
#' 
#' This function changes the descriptive measures purpose text
#' for species to the abbreviated text used in the reporting tool 
#'
#' @param purpose character, measures purpose text 
#'
#' @return character, measures purpose abbreviated text 
#' @export
#'
#' @examples
#' recode_measures_purpose("Maintain the current distribution, population and/or habitat for the species")
recode_measures_purpose <- function(purpose) {
  
  tibble::as.tibble(purpose) %>% 
    dplyr::mutate(value = dplyr::case_when
                  (
                    stringr::str_detect(stringr::str_to_lower(value), "^expand") ~ "expand",
                    stringr::str_to_lower(value) == "measures_purpose_expand" ~ "expand",
                    stringr::str_detect(stringr::str_to_lower(value), "^increase") ~ "increase",
                    stringr::str_to_lower(value) == "measures_purpose_increase" ~ "increase",
                    stringr::str_detect(stringr::str_to_lower(value), "^maintain") ~ "maintain",
                    stringr::str_to_lower(value) == "measures_purpose_maintain" ~ "maintain",
                    stringr::str_detect(stringr::str_to_lower(value), "^restore") ~ "restore",
                    stringr::str_to_lower(value) == "measures_purpose_restore" ~ "restore",
                    TRUE ~ value
                  )) %>% 
    unlist()
}

#' Recode measures location
#' 
#' This function changes the descriptive measures location text
#' to the abbreviated text used in the reporting tool 
#'
#' @param location character, measures location text
#'
#' @return character, measures location abbreviation text
#' @export
#'
#' @examples
#' recode_measures_location("Both inside and outside Natura 2000")
recode_measures_location <- function(location) {
  
  tibble::as.tibble(location) %>% 
    dplyr::mutate(value = dplyr::case_when
                  (
                    stringr::str_detect(stringr::str_to_lower(value), "^only inside") ~ "in",
                    stringr::str_to_lower(value) == "measures_location_in" ~ "in",
                    stringr::str_detect(stringr::str_to_lower(value), "^both") ~ "inOut",
                    stringr::str_to_lower(value) == "measures_location_inout" ~ "inOut",
                    stringr::str_detect(stringr::str_to_lower(value), "^only outside") ~ "out",
                    stringr::str_to_lower(value) == "measures_location_out" ~ "out",
                    TRUE ~ value
                  )) %>% 
    unlist()
}

#' Recode measures response
#' 
#' This function changes the descriptive measures response text
#' to the abbreviated text used in the reporting tool 
#'
#' @param response character, measures response text
#'
#' @return character, measures response abbreviated text
#' @export
#'
#' @examples
#' recode_measures_response("Medium-term results (within the next two reporting periods, 2019-2030)")
recode_measures_response <- function(response) {
  
  tibble::as.tibble(response) %>% 
    dplyr::mutate(value = dplyr::case_when
                  (
                    stringr::str_detect(stringr::str_to_lower(value), "^long-term") ~ "lonTerm",
                    stringr::str_to_lower(value) == "measures_response_long" ~ "lonTerm",
                    stringr::str_detect(stringr::str_to_lower(value), "^medium-term") ~ "medTerm",
                    stringr::str_to_lower(value) == "measures_response_medium" ~ "medTerm",
                    stringr::str_detect(stringr::str_to_lower(value), "^short-term") ~ "srtTerm",
                    stringr::str_to_lower(value) == "measures_response_short" ~ "srtTerm",
                    TRUE ~ value
                  )) %>% 
    unlist()
}

#' Recode reason change
#'
#' This function changes the descriptive reason change
#' text to the abbreviated text used in the reporting tool 
#'
#' @param reason character, reason change text
#'
#' @return character, abbreviated freason change text
#' @export
#'
#' @examples
#' recode_reason_change("Improved knowledge/more accurate data")
recode_reason_change <- function(reason) {
  
  tibble::as.tibble(reason) %>% 
    dplyr::mutate(value = dplyr::case_when
                  (
                    stringr::str_detect(stringr::str_to_lower(value), "genuine") ~ "genuine",
                    stringr::str_detect(stringr::str_to_lower(value), "improved knowledge") ~ "knowledge",
                    stringr::str_detect(stringr::str_to_lower(value), "different method") ~ "method",
                    TRUE ~ value
                  )) %>% 
    unlist()
}





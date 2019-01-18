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

#' Format single year date
#'
#' This function formats dates consisting of a 4 digit year, adding
#' a dash at the end so that the reporting tool can handle these single
#' dates in a year range. It also replaces the unicode dash character 
#'
#' @param date integer or character, 4 digits
#'
#' @return character, 4 digits with end dash
#' @export
#'
#' @examples
#' format_date(2018)
format_date <- function(date) {
  
  tibble::as.tibble(date) %>% 
    dplyr::mutate(value = as.character(value)) %>% 
    dplyr::mutate(value = dplyr::if_else(stringr::str_detect(value, "^[0-9]{4}$"), stringr::str_c(value, "-"), value),
                  value = stringr::str_replace_all(value, "–", "-"),
                  value = stringr::str_remove_all(value, " ")) %>% 
    unlist()
}

#' Format text
#'
#' This function formats free text handling NA characters, end of line
#' characters, double quotes, URL characters and converting unicode
#' characters to the most appropriate ASCII character
#'
#' @param text character, free text
#'
#' @return character, formatted free text
#' @export
#'
#' @examples
#' format_text("▪ #NAUnformatted%20text list starting with NA")
#' format_text("ÀáäãčçéÖöØñřŝüūú")
format_text <- function(text) {
  
  text_formatting <- tibble::as.tibble(text)
  
  # Handle NA as strings
  text_formatting <- text_formatting %>% 
    dplyr::mutate(value = stringr::str_replace_all(value, "#NA#", "#"),
                  value = stringr::str_replace_all(value, "#NA", ""),
                  value = stringr::str_replace_all(value, "^NA#", ""),
                  value = dplyr::if_else(value == "NA", NA_character_, value))
  
  # Handle end of line characters
  text_formatting <- text_formatting %>% 
    dplyr::mutate(value = stringr::str_replace_all(value, "\r\n", "#"),
                  value = stringr::str_replace_all(value, "#{2,}", "#"))
  
  # Handle double quotes
  text_formatting <- text_formatting %>%
    dplyr::mutate(value = stringr::str_replace_all(value, '"', ''))
  
  # Handle URL characters
  text_formatting <- text_formatting %>%
    dplyr::mutate(value = stringr::str_replace_all(value, '%20', ' '))
  
  
  # Handle unicode characters
  # https://stackoverflow.com/questions/9934856/removing-non-ascii-characters-from-data-files
  text_formatting <- text_formatting %>% 
    dplyr::mutate(value = stringi::stri_trans_general(value, "latin-ascii"))
  
  text_formatting <- text_formatting %>% 
    dplyr::mutate(value = stringi::stri_escape_unicode(value),
                  value = stringr::str_replace_all(value, "\\\\u2022", "-"), # bullet point
                  value = stringr::str_replace_all(value, "\\\\u25aa", "-"), # black small square
                  value = stringr::str_replace_all(value, "\\\\u2010", "-"), # hyphen
                  value = stringr::str_replace_all(value, "\\\\u2013", "-"), # en-dash
                  value = stringr::str_replace_all(value, "\\\\u2019", "'"), # opening apostrophe
                  value = stringr::str_replace_all(value, "\\\\u2018", "'"), # closing apostrophe
                  value = stringr::str_replace_all(value, "\\\\u201c", "'"), # left double quotation mark
                  value = stringr::str_replace_all(value, "\\\\u201d", "'"), # right double quotation mark
                  value = stringr::str_replace_all(value, "\\\\u201f", "'"), # double high-reversed-9 quotation mark
                  value = stringr::str_replace_all(value, "\\\\u00b0", " degree "), # degree sign
                  value = stringr::str_replace_all(value, "\\\\u2026", "..."), # horizontal ellipsis
                  value = stringr::str_replace_all(value, "\\\\u2020", "[1]"), # dagger footnote
                  value = stringr::str_replace_all(value, "\\\\u2021", "[2]"), # double dagger footnote
                  value = stringr::str_replace_all(value, "\\\\u2640", " female "), # female sign
                  value = stringr::str_replace_all(value, "\\\\u", ""))
  
  # Handle less than or equal sign
  # 2019-02-06 DO NOT USE commented out as R also converts = to <=
  # text_formatting <- text_formatting %>%
  #  dplyr::mutate(value = stringr::str_replace_all(value, '≤', '<=')) # less than or equal
  
  # Handle miscellaneous characters
  text_formatting <- text_formatting %>%
    dplyr::mutate(value = stringr::str_replace_all(value, "\\\\U00100202", "'"), # apostrophe
                  value = stringr::str_replace_all(value, "’", "'"), # apostrophe
                  value = stringr::str_replace_all(value, "00a0", ""), # No-break space
                  value = stringr::str_replace_all(value, "00b1", "+-"), # plus-minus sign
                  value = stringr::str_replace_all(value, "00a3", "(pounds)"), # pound sign
                  value = stringr::str_replace_all(value, "00b2", "2"), # superscript 2
                  value = stringr::str_replace_all(value, "00b3", "3"), # superscript 3
                  value = stringr::str_replace_all(value, "00ba", " degrees"), # masculine ordinal indicator
                  value = stringr::str_replace_all(value, "00b4", "'"), # acute accent
                  value = stringr::str_replace_all(value, "00b5", "micro"), # micro
                  value = stringr::str_replace_all(value, "00b8", ","), # cedilla
                  value = stringr::str_replace_all(value, "00fa", "u"), # ú
                  #value = stringr::str_replace_all(value, "00f6", "o"), # ö
                  #value = stringr::str_replace_all(value, "00d8", "O"), # Ø
                  #value = stringr::str_replace_all(value, "00f1", "n"), # ñ
                  #value = stringr::str_replace_all(value, "00c0", "A"), # À
                  #value = stringr::str_replace_all(value, "00e9", "e"), # é
                  #value = stringr::str_replace_all(value, "00e1", "a"), # á
                  #value = stringr::str_replace_all(value, "00fc", "u"), # ü
                  #value = stringr::str_replace_all(value, "015d", "s"), # ŝ
                  #value = stringr::str_replace_all(value, "u0161", "us"), # Juŝkaitis
                  #value = stringr::str_replace_all(value, "016b", "u"), # ū
                  #value = stringr::str_replace_all(value, "t0117", "te"), # Kitrytė
                  #value = stringr::str_replace_all(value, "010d", "c"), # č
                  #value = stringr::str_replace_all(value, "u0159", "ur"), # Juřičková
                  #value = stringr::str_replace_all(value, "00e4", "a"), # ä
                  #value = stringr::str_replace_all(value, "00d6", "O"), # Ö
                  #value = stringr::str_replace_all(value, "00e7", "c"), # ç
                  #value = stringr::str_replace_all(value, "00e3", "a"), # ã
                  value = stringr::str_replace_all(value, "\\\\'", "'"))
  
  # Remove trailing white space
  text_formatting <- text_formatting %>%
    dplyr::mutate(value = stringr::str_squish(value))
  
  text_formatting <- unlist(text_formatting)
}
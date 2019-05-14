# information_species_look_up
url <- "http://biodiversity.eionet.europa.eu/activities/Reporting/Article_12/Reports_2019/Files_2019/art12_checklist_v2_update20180616.xls"
dest_file <- "art12_checklist_v2_update20180616.xls"
curl::curl_download(url, dest_file)
information_species_look_up <- readxl::read_excel(dest_file, sheet = "Art12 checklist") %>% 
  janitor::clean_names() %>% 
  dplyr::filter(country == "UK") %>% 
  dplyr::rename(speciescode = species_code,
                speciesname = species_name) %>% 
  dplyr::mutate(season = dplyr::case_when 
                (
                  stringr::str_to_lower(season) == "b" ~ "breeding",
                  stringr::str_to_lower(season) == "w" ~ "winter",
                  stringr::str_to_lower(season) == "p" ~ "passage",
                  TRUE ~ season
                )) %>%
  dplyr::mutate(occurrence = dplyr::case_when 
                (
                  stringr::str_to_lower(occurrence) == "pre" ~ "present",
                  stringr::str_to_lower(occurrence) == "arr" ~ "colonised",
                  stringr::str_to_lower(occurrence) == "exba" ~ "extinct",
                  TRUE ~ occurrence
                )) %>%
  dplyr::mutate(recommended_unit = dplyr::case_when 
                (
                  stringr::str_to_lower(recommended_unit) == "bfemales" ~ "breeding females",
                  stringr::str_to_lower(recommended_unit) == "cmales" ~ "calling males",
                  stringr::str_to_lower(recommended_unit) == "i" ~ "individuals",
                  stringr::str_to_lower(recommended_unit) == "males" ~ "males",  
                  stringr::str_to_lower(recommended_unit) == "p" ~ "pairs",    
                  TRUE ~ recommended_unit
                )) %>% 
  dplyr::select(speciescode,
                euringcode,
                speciesname:season,
                occurrence,
                recommended_unit) %>% 
  dplyr::arrange(speciesname, sub_unit)


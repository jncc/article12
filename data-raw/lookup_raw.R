# information_species_look_up
url <- "http://biodiversity.eionet.europa.eu/activities/Reporting/Article_12/Reports_2019/Files_2019/art12_checklist_v2_update20180616.xls"
dest_file <- "art12_checklist_v2_update20180616.xls"
curl::curl_download(url, dest_file)
information_species_look_up <- readxl::read_excel(dest_file, sheet = "Art12 checklist") %>% 
  janitor::clean_names() %>% 
  dplyr::filter(country == "UK") %>% 
  dplyr::rename(speciescode = species_code,
                speciesname = species_name) %>% 
  dplyr::select(speciescode:recommended_unit) %>% 
  dplyr::arrange(speciesname, sub_unit)




#' @noRd

read_cdm_wiki_table <-
        function(cdmTable) {

                response <- xml2::read_html(paste0("https://github.com/OHDSI/CommonDataModel/wiki/", cdmTable))

                response %>%
                        rvest::html_nodes("table") %>%
                        rvest::html_table() %>%
                        purrr::set_names(c(cdmTable, "CONVENTIONS"))

        }

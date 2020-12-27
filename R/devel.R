#' @noRd

read_cdm_wiki_table <-
        function() {

                response <- xml2::read_html("https://ohdsi.github.io/CommonDataModel/cdm60.html")

                data <-
                response %>%
                        rvest::html_nodes("table") %>%
                        rvest::html_table(fill = TRUE)

                names(data) <-
                response %>%
                        rvest::html_nodes("h3") %>%
                        rvest::html_text()

                data

        }

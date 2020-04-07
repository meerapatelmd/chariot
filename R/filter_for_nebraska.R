#' Returns all possible OMOP domains in Athena
#' @return dataframe of unique `domain_id` variables from the `concept` table
#' @importFrom mySeagull connect_to_local_postgres
#' @import DBI
#' @export

filter_for_nebraska <-
        function(dataframe) {
                dataframe %>%
                        dplyr::filter(vocabulary_id == "Nebraska Lexicon")

        }

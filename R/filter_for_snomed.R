#' Returns all possible OMOP domains in Athena
#' @return dataframe of unique `domain_id` variables from the `concept` table
#' @importFrom dplyr filter
#' @export

filter_for_snomed <-
        function(dataframe) {
                dataframe %>%
                        dplyr::filter(vocabulary_id == "SNOMED")

        }

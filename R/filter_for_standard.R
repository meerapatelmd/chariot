#' Returns all possible OMOP domains in Athena
#' @return dataframe of unique `domain_id` variables from the `concept` table
#' @importFrom dplyr filter
#' @export

filter_for_standard <-
        function(dataframe) {
                dataframe %>%
                        dplyr::filter(standard_concept == "S")

        }

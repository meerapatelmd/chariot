#' Returns all possible OMOP domains in Athena
#' @return dataframe of unique `domain_id` variables from the `concept` table
#' @importFrom dplyr filter
#' @importFrom rubix 
#' @export

filter_for_rxnorm <-
        function(dataframe, rxnorm_extension = TRUE) {
            if (rxnorm_extension == TRUE) {
                dataframe %>%
                    rubix::filter_at_grepl(col = vocabulary_id,
                                           grepl_phrase = "Rx")
            } else {
                dataframe %>%
                    dplyr::filter(vocabulary_id == "RxNorm")
            }
        }

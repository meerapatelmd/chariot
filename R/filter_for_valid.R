#' Returns all possible OMOP domains in Athena
#' @return dataframe of unique `domain_id` variables from the `concept` table. If `rm_columns` = TRUE, removes all the columns that are irrelevant if all valids are filtered for (date, invalid_reason column itself.)
#' @importFrom dplyr filter
#' @export

filter_for_valid <-
        function(dataframe, rm_columns = TRUE) {
            if (rm_columns == TRUE) {
                dataframe %>%
                    dplyr::filter(is.na(invalid_reason)) %>%
                    dplyr::filter(-valid_start_date, -valid_end_date, -invalid_reason)
            } else {
                dataframe %>%
                    dplyr::filter(is.na(invalid_reason)) 
            }

        }

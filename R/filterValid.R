#' Returns all possible OMOP domains in Athena
#' @return .data of unique `domain_id` variables from the `concept` table. If `rm_columns` = TRUE, removes all the columns that are irrelevant if all valids are filtered for (date, invalid_reason column itself.)
#' @importFrom dplyr filter
#' @importFrom dplyr select
#' @export

return_valid <-
        function(.data, rm_columns = TRUE) {
            if (rm_columns == TRUE) {


                .data %>%
                    dplyr::filter(is.na(invalid_reason)) %>%
                    dplyr::select(-valid_start_date, -valid_end_date)


            } else {

                .data %>%
                    dplyr::filter(is.na(invalid_reason))

            }

        }

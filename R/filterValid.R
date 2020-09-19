#' @title Filter for Valid Concepts
#' @description
#' Filter a data frame queried from the Concept Table for only the valid concepts based on the `invalid_reason` column with the option to remove the `valid_start_date` and `valid_end_date` columns.
#'
#' @return
#' A data frame filtered for `invalid_reason == NA` with or without the `valid_start_date` and `valid_end_date` removed.
#'
#' @seealso
#'  \code{\link[dplyr]{filter}},\code{\link[dplyr]{select}}
#'
#' @rdname filterValid
#' @export
#' @importFrom magrittr %>%
#' @importFrom dplyr filter_at select

filterValid <-
        function(.data,
                 has_prefix = NULL,
                 has_suffix = NULL,
                 rm_date_fields = TRUE) {

                        .output <-
                        .filterConcept(.data = .data,
                                       has_prefix = has_prefix,
                                       has_suffix = has_suffix,
                                       concept_col = "invalid_reason",
                                       values = NA_character_)


                        if (rm_date_fields) {

                                columns <- paste0(has_prefix,
                                                  c("valid_start_date",
                                                    "valid_end_date"),
                                                  has_suffix)

                                .output %>%
                                        dplyr::select(-any_of(columns))

                        } else {
                                .output
                        }

        }

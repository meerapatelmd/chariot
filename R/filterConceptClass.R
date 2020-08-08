#' @title Filter Concept Class
#' @param values character vector of length 1 or greater.
#' @seealso
#'  \code{\link[dplyr]{filter_all}}
#' @rdname filterConceptClass
#' @export
#' @importFrom dplyr filter_at

filterConceptClass <-
        function(.data,
                 has_prefix = NULL,
                 has_suffix = NULL,
                 values,
                 invert = FALSE) {


                        columns <- paste0(has_prefix,
                                          "concept_class_id",
                                          has_suffix) %>%
                                                as.list()

                        names(columns) <- "concept_class_id"


                        if (invert) {

                                .data %>%
                                        dplyr::filter_at(vars(all_of(columns$concept_class_id)),
                                                         all_vars(!(. %in% values)))

                        } else {

                                .data %>%
                                        dplyr::filter_at(vars(all_of(columns$concept_class_id)),
                                                         all_vars(. %in% values))
                        }
        }


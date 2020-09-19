#' @title Primitive filtering function
#' @param values character vector of length 1 or greater.
#' @seealso
#'  \code{\link[dplyr]{filter_all}}
#' @rdname .filterConcept
#' @export
#' @importFrom dplyr filter_at

.filterConcept <-
        function(.data,
                 has_prefix = NULL,
                 has_suffix = NULL,
                 concept_col,
                 values,
                 invert = FALSE) {


                        columns <- paste0(has_prefix,
                                          c(
                                          "concept_id",
                                          "concept_name",
                                          "domain_id",
                                          "vocabulary_id",
                                          "concept_class_id",
                                          "standard_concept",
                                          "concept_code",
                                          "valid_start_date",
                                          "valid_end_date",
                                          "invalid_reason"),
                                          has_suffix) %>%
                                                as.list()

                        names(columns) <-  c("concept_id",
                                             "concept_name",
                                             "domain_id",
                                             "vocabulary_id",
                                             "concept_class_id",
                                             "standard_concept",
                                             "concept_code",
                                             "valid_start_date",
                                             "valid_end_date",
                                             "invalid_reason")


                        if (invert) {

                                .data %>%
                                        dplyr::filter_at(vars(all_of(unlist(columns)[concept_col])),
                                                         all_vars(!(. %in% values)))

                        } else {

                                .data %>%
                                        dplyr::filter_at(vars(all_of(unlist(columns)[concept_col])),
                                                         all_vars(. %in% values))
                        }
        }


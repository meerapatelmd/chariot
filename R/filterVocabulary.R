#' Filter Vocabulary
#' @importFrom dplyr filter_at
#' @export

filterVocabulary <-
        function(.data,
                 has_prefix = NULL,
                 has_suffix = NULL,
                 values,
                 invert = FALSE) {


                        .filterConcept(.data = .data,
                                       has_prefix = has_prefix,
                                       has_suffix = has_suffix,
                                       concept_col = "vocabulary_id",
                                       values = values,
                                       invert = invert)



        }

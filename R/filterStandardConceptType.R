#' @title Filter by Standard Concept Type
#' @export

filterStandardConceptType <-
        function(.data,
                 has_prefix = NULL,
                 has_suffix = NULL,
                 values,
                 invert = FALSE) {


                .filterConcept(.data = .data,
                               has_prefix = has_prefix,
                               has_suffix = has_suffix,
                               concept_col = "standard_concept",
                               values = values,
                               invert = invert)


        }

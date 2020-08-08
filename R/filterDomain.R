#' @title Filter Domain
#' @export

filterDomain <-
        function(.data,
                 has_prefix = NULL,
                 has_suffix = NULL,
                 values,
                 invert = FALSE) {


                .filterConcept(.data = .data,
                               has_prefix = has_prefix,
                               has_suffix = has_suffix,
                               concept_col = "domain_id",
                               values = values,
                               invert = invert)


        }

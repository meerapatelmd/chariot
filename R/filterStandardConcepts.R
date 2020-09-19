#' @title Filter Standard Concepts
#' @export

filterStandardConcepts <-
        function(.data,
                 has_prefix = NULL,
                 has_suffix = NULL,
                 invert = FALSE) {

                         filterStandardConceptType(.data = .data,
                                                   has_prefix = has_prefix,
                                                   has_suffix = has_suffix,
                                                   values = "S",
                                                   invert = invert)
                 }

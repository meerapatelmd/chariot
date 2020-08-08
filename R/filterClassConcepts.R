#' @title Filter Class Concepts
#' @export

filterClassConcepts <-
        function(.data,
                 has_prefix = NULL,
                 has_suffix = NULL,
                 invert = FALSE) {

                         filterStandardConceptType(.data = .data,
                                                   has_prefix = has_prefix,
                                                   has_suffix = has_suffix,
                                                   values = "C",
                                                   invert = invert)
                 }

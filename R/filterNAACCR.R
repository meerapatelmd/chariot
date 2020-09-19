#' @title Filter NAACCR
#' @export

filterNAACCR <-
        function(.data,
                 has_prefix = NULL,
                 has_suffix = NULL,
                 invert = FALSE) {

                filterVocabulary(.data = .data,
                                 has_prefix = has_prefix,
                                 has_suffix = has_suffix,
                                 values = "NAACCR",
                                 invert = invert)

        }

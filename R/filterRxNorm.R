#' @title Filter for RxNorm Concepts
#' @export

filterRxNorm <-
        function(.data,
                 has_prefix = NULL,
                 has_suffix = NULL,
                 includeExt = TRUE,
                 invert = FALSE) {


                if (includeExt) {

                        filterVocabulary(.data = .data,
                                         has_prefix = has_prefix,
                                         has_suffix = has_suffix,
                                         values = c("RxNorm", "RxNorm Extension"),
                                         invert = invert)

                }

                        filterVocabulary(.data = .data,
                                         has_prefix = has_prefix,
                                         has_suffix = has_suffix,
                                         values = c("RxNorm"),
                                         invert = invert)

        }

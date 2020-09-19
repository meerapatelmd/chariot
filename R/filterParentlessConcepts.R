#' Get Topmost Ancestors
#' @description
#' This function asks the question: of all the concept ids provided in the input data, which ones do not have a parent?. If the question is: of all the concept ids in the input, what are the topmost ancestor concepts?, try \code{\link{topmostAncestors}}.
#' @param .data data
#' @param ... Arguments passed to the dplyr filter function
#' @import dplyr
#' @export

filterParentlessConcepts <-
        function(.data,
                 column = NULL,
                 ...) {

                .data %>%
                        dplyr::filter(...)

                if (is.null(column)) {
                        column <- colnames(.data)[1]
                }

                ancestors <-
                        leftJoinForAncestors(.data,
                                             descendant_id_column = column)


                ancestors %>%
                        dplyr::group_by_at(vars(!!column)) %>%
                        dplyr::summarize(ALL_ANCESTORS_NA = all(is.na(ancestor_concept_id)), .groups = "drop") %>%
                        dplyr::filter(ALL_ANCESTORS_NA == TRUE) %>%
                        dplyr::select(!!column) %>%
                        dplyr::distinct() %>%
                        leftJoinConcept()

        }


#' @title Create a Search Settings Object
#' @description
#' This function takes the Concept Table fields present in the global environment and aggregates them into a list object that can then be run against a data frame as a whole using the \code{\link{filterSettings}} function.
#'
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @seealso
#'  \code{\link[rubix]{map_names_set}}
#'  \code{\link[purrr]{keep}},\code{\link[purrr]{map}}
#' @rdname set_search_parameters
#' @export
#' @importFrom magrittr %>%
#' @importFrom rubix map_names_set
#' @importFrom purrr keep map


set_search_parameters <-
        function(obj = "SEARCH_SETTINGS",
                 rm_source_obj = TRUE) {

                parameters <-
                        c("concept_code",
                          "vocabulary_id",
                          "domain_id",
                          "concept_class_id",
                          "standard_concept",
                          "invalid_reason")


                settings <-
                        parameters %>%
                        rubix::map_names_set(function(x) if (exists(x, envir = parent.frame())) {
                                get(x, envir = parent.frame())
                        }) %>%
                        purrr::keep(~!is.null(.)) %>%
                        purrr::map(~ifelse(is.na(.), NA_character_, .))



                if (rm_source_obj) {

                        parameters %>%
                        purrr::map(function(x) if (exists(x,envir = parent.frame())) {
                                rm(list = x, envir = parent.frame())
                        })

                }

                assign(obj, value = settings, envir = parent.frame())

        }

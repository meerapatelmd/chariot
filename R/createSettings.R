#' @title Create a .Settings Object
#' @description
#' This function takes the Concept Table fields present in the global environment and aggregates them into a list object `.Settings` that can then be run against a data frame as a whole using the \code{\link{filterSettings}} function.
#'
#' @param concept_code PARAM_DESCRIPTION, Default: NULL
#' @param vocabulary_id PARAM_DESCRIPTION, Default: NULL
#' @param domain_id PARAM_DESCRIPTION, Default: NULL
#' @param concept_class_id PARAM_DESCRIPTION, Default: NULL
#' @param standard_concept PARAM_DESCRIPTION, Default: NULL
#' @param invalid_reason PARAM_DESCRIPTION, Default: NULL
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @seealso
#'  \code{\link[rubix]{map_names_set}}
#'  \code{\link[purrr]{keep}},\code{\link[purrr]{map}}
#' @rdname createSettings
#' @export
#' @importFrom magrittr %>%
#' @importFrom rubix map_names_set
#' @importFrom purrr keep map


createSettings <-
        function() {

                parameters <-
                        c("concept_code",
                          "vocabulary_id",
                          "domain_id",
                          "concept_class_id",
                          "standard_concept",
                          "invalid_reason")


                .Settings <<-
                parameters %>%
                        rubix::map_names_set(function(x) if (exists(x, envir = globalenv())) {
                                                      get(x, envir = globalenv())
                                                }) %>%
                                purrr::keep(~!is.null(.)) %>%
                                purrr::map(~ifelse(is.na(.), NA_character_, .))


                x <-
                parameters %>%
                        purrr::map(function(x) if (exists(x,envir = globalenv())) {
                                                        rm(list = x, envir = globalenv())
                                })

        }



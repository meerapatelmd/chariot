#' Create Settings
#' @import purrr
#' @importFrom magrittr %>%
#' @export


createSettings <-
        function(concept_code = NULL,
                 vocabulary_id = NULL,
                 domain_id = NULL,
                 concept_class_id = NULL,
                 standard_concept = NULL,
                 invalid_reason = NULL) {


                Settings <<-
                list(concept_code = concept_code,
                     vocabulary_id = vocabulary_id,
                     domain_id = domain_id,
                     concept_class_id = concept_class_id,
                     standard_concept = standard_concept,
                     invalid_reason = invalid_reason) %>%
                                purrr::keep(~!is.null(.)) %>%
                                purrr::map(~ifelse(is.na(.), NA_character_, .))

        }



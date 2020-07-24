#' Normalize To HemOnc Components
#' @description This function takes a mixture of HemOnc Regimen and HemOnc Component Concepts and returns all the unique HemOnc Components associated with the input combination.
#' @param hemonc_concept_ids HemOnc Vocabulary Concept Ids of either Regimen or Component concept classes.
#' @import rubix
#' @import dplyr
#' @export



normalizeToHemOncComponents <-
        function(hemonc_concept_ids,
                 schema = NULL) {

                # If any of the concept_ids are regimens, to get their antineoplastic components
                input_concept <- query_concept_id(hemonc_concept_ids)

                qa <- input_concept %>%
                        rubix::filter_for(filter_col = concept_class_id,
                                          inclusion_vector = c("Regimen",
                                                               "Component"),
                                          invert = TRUE)

                if (nrow(qa)) {
                        qaNormalizeToHemOncComponents <<- qa
                        stop('input concept ids are not Regimen or Components. See qaNormalizeToHemOncComponents for more details.')
                }

                input_regimens <- input_concept %>%
                        dplyr::filter(concept_class_id == "Regimen")

                input_components <- input_concept %>%
                        dplyr::filter(concept_class_id == "Component")


                if (nrow(input_regimens)) {


                        component_concept_ids <-
                                c(input_components$concept_id,
                                  queryHemOncRegToAntineo(regimen_concept_ids = input_regimens$concept_id,
                                                          schema = schema) %>%
                                          dplyr::select(has_antineoplastic_concept_id) %>%
                                          unlist() %>%
                                          unname())


                } else {

                        component_concept_ids <- input_components$concept_id

                }

                return(unique(component_concept_ids))
        }

#' Find HemOnc Regimen by Components
#' @description This function takes a set list of HemOnc Components and finds all the HemOnc Regimens that contain these Components in the CONCEPT_RELATIONSHIP table. The resultset is then filtered for the HemOnc Regimens that have the same exact number of Components as the length of the `component_concept_ids` parameter.
#' @param component_concept_ids integer vector of length 1 or greater of all the Component Concept Ids belonging to any regimen to map to a HemOnc Regimen.
#' @import magrittr
#' @import dplyr
#' @export

queryHemOncCompToReg <-
        function(component_concept_ids,
                 schema = NULL) {

                # Get input component count to filter HemOnc Regimens based on their own component_counts
                input_component_count <- length(component_concept_ids)

                # Query Athena DB for all Regimens associated with the Component Concept Ids
                renderHemOncCompToReg(component_concept_ids = component_concept_ids,
                                      schema = schema) %>%
                        query_athena() %>%
                # Getting the number of unique HemOnc Components associated with each of the HemOnc Regimens found and then filtering for the length of the input component_concept_ids vector
                        dplyr::group_by(regimen_concept_id,
                                        regimen_concept_name) %>%
                        dplyr::summarize(component_count = length(unique(component_concept_id))) %>%
                        dplyr::filter(component_count == input_component_count) %>%
                        dplyr::ungroup()
        }

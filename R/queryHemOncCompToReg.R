#' Find HemOnc Regimen by Components
#' @description This function takes a set list of HemOnc Components and finds all the HemOnc Regimens that contain these Components in the CONCEPT_RELATIONSHIP table. The resultset is then filtered for the HemOnc Regimens that have the same exact number of Components as the length of the `component_concept_ids` parameter.
#' @param component_concept_ids integer vector of length 1 or greater of all the Component Concept Ids belonging to any regimen to map to a HemOnc Regimen.
#' @param ... Additional arguments passed to the query_athena function.
#' @import magrittr
#' @import dplyr
#' @export

queryHemOncCompToReg <-
        function(component_concept_ids,
                 schema = NULL,
                 ...) {

                # Get input component count to filter HemOnc Regimens based on their own component_counts
                input_component_count <- length(component_concept_ids)

                # Query Athena DB for all Regimens associated with the inputted Component Concept Ids
                sql_statement <-
                renderHemOncCompToReg(component_concept_ids = component_concept_ids,
                                      schema = schema)

                Regimens <-
                        query_athena(sql_statement = sql_statement,
                                     ...)

                # Query again to get all of the "Has antineoplastic" relationships to HemOnc Components these Regimens have
                HasAntineoplastics <- queryHemOncRegToAntineo(regimen_concept_ids = Regimens$regimen_concept_id,
                                                              schema = schema)



                # Getting the number of unique HemOnc Components associated with each of the HemOnc Regimens found and then filtering for the length of the input component_concept_ids vector
                HasAntineoplastics2 <<-
                HasAntineoplastics %>%
                        dplyr::group_by(regimen_concept_id) %>%
                        dplyr::summarize(has_antineoplastic_count = length(unique(has_antineoplastic_concept_id)), .groups = "drop") %>%
                        dplyr::filter(has_antineoplastic_count == input_component_count) %>%
                        dplyr::ungroup() %>%
                        dplyr::select(regimen_concept_id) %>%
                        left_join_concept() %>%
                        dplyr::select(-any_of("regimen_concept_id")) %>%
                        rubix::rename_all_prefix("regimen_")

                #If only 1 or less rows, the function is complete. Otherwise, the outputs need to be filtered another time since now we have all the Regimens that have the exact component count match as the input and have at least 1 of the input components, but does not necessarily have all the components
#
                if (nrow(HasAntineoplastics2) <= 1) {
                        return(HasAntineoplastics2)
                } else {
                        HasAntineoplastics3 <<-
                                HasAntineoplastics %>%
                                dplyr::select(regimen_concept_id,
                                              has_antineoplastic_concept_id) %>%
                                dplyr::group_by(regimen_concept_id) %>%
                                dplyr::summarise_at(vars(has_antineoplastic_concept_id),
                                                    list(has_all_components = ~all(. %in% component_concept_ids),
                                                         component_count = ~length(unique(.)))) %>%
                                dplyr::filter(has_all_components == TRUE,
                                              component_count == input_component_count) %>%
                                dplyr::select(regimen_concept_id) %>%
                                left_join_concept() %>%
                                dplyr::select(-starts_with("regimen")) %>%
                                rubix::rename_all_prefix("regimen_")

                        return(HasAntineoplastics3)
                }
        }

#' Get a triplet from the concept_relationship table
#' @param names_from predicate of the triplet
#' @param .data data the contains the concept_id (subject)
#' @param .col column in data that contains the concept_id (subject)
#' @export

concept2_triplet <-
        function(.data,
                 .col = NULL,
                 names_from,
                 include_count = TRUE,
                 omop = FALSE,
                 omop_schema = "omop_vocabulary") {



                # Getting pivot
                output_a <-
                pivot_concept2(.data = .data,
                               .col = .col,
                               names_from = names_from,
                               include_count = include_count,
                               omop = omop,
                               omop_schema = omop_schema)


                # Getting concept1 labels
                output_b <-
                    left_join_concept(.data = output_a %>%
                                          dplyr::select(concept_id_1),
                                      omop = omop,
                                      omop_schema = omop_schema,
                                      include_synonyms = FALSE) %>%
                    merge_concepts(into = "Concept1") %>%
                    dplyr::select(-concept_id)


                return(dplyr::left_join(output_a,
                                        output_b,
                                        by = "concept_id_1") %>%
                           rubix::bring_to_front(concept_id_1, Concept1)
                       )



        }

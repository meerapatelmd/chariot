#' Left Join Relationship
#' @export

leftJoinRelationship <-
        function(.data,
                 column = NULL,
                 athena_schema = "public",
                 merge_concept2 = TRUE,
                 override_cache = FALSE) {


                .output1 <-
                        leftJoin(.data = .data,
                         column = column,
                         athena_schema = athena_schema,
                         athena_table = "concept_relationship",
                         athena_column = "concept_id_1",
                         override_cache = override_cache)

                .output2 <-
                leftJoinConcept(.data = .output1 %>%
                                        dplyr::select(concept_id_2),
                                athena_schema = athena_schema,
                                override_cache = override_cache) %>%
                                dplyr::select(-concept_id_2) %>%
                                rubix::rename_all_suffix(suffix = "_2")

                .output <-
                        dplyr::left_join(.output1,
                                         .output2)

                if (merge_concept2) {
                        .output %>%
                                chariot::merge_concepts(into = "Concept2",
                                                        suffix = "_2")
                } else {
                        .output
                }

        }

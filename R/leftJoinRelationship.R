#' Left Join Relationship
#' @export

leftJoinRelationship <-
        function(.data,
                 column = NULL,
                 athena_schema = "public",
                 render_sql = TRUE,
                 conn = NULL) {

                if (is.null(column)) {

                        column <- colnames(.data)[1]

                }



                .output1 <-
                        leftJoin(.data = .data %>%
                                         dplyr::select(all_of(column)),
                                 athena_schema = athena_schema,
                                 athena_table = "concept_relationship",
                                 athena_column = "concept_id_1",
                                 render_sql = render_sql,
                                 conn = conn) %>%
                        dplyr::filter(is.na(invalid_reason)) %>%
                        dplyr::select(-valid_start_date,
                                      -valid_end_date,
                                      -invalid_reason)

                .output1 <-
                        dplyr::left_join(.data,
                                         .output1,
                                         by = column)

                .output2 <-
                        leftJoinConcept(.data = .output1 %>%
                                                dplyr::select(concept_id_2),
                                        athena_schema = athena_schema,
                                        render_sql = render_sql,
                                        conn = conn) %>%
                                        dplyr::select(-concept_id_2) %>%
                                        rubix::rename_all_suffix(suffix = "_2")


                dplyr::left_join(.output1,
                                 .output2,
                                 by = "concept_id_2") %>%
                dplyr::select(!ends_with("_2"),
                              ends_with("_2"),
                              dplyr::everything())


        }

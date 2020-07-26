#' Left Join Relationship
#' @export

leftJoinRelationship <-
        function(.data,
                 column = NULL,
                 athena_schema = "public") {


                .output1 <-
                        leftJoin(.data = .data,
                         column = column,
                         athena_schema = athena_schema,
                         athena_table = "concept_relationship",
                         athena_column = "concept_id_1")

                leftJoinConcept(.data = .output1,
                                athena_schema = athena_schema,
                                column = "concept_id_2")

        }

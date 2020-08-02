#' Query Concept Synonym Table
#' @import pg13
#' @import secretary
#' @import dplyr
#' @import rubix
#' @export


queryPhraseExactSynonym <-
        function(schema,
                 caseInsensitive = TRUE,
                 phrase,
                 limit_n = NULL,
                 ...) {

                sqlStatement <- renderQueryPhraseExactSynonym(schema = schema,
                                                             caseInsensitive = caseInsensitive,
                                                             phrase = phrase,
                                                             limit_n = limit_n)


                .output_a <- query_athena(sql_statement = sqlStatement,
                                        ...)


                .output_b <-
                leftJoinConcept(.output_a %>%
                                        dplyr::select(concept_synonym_id = concept_id),
                                athena_schema = schema) %>%
                                dplyr::select(-concept_synonym_id)


                .output_b %>%
                        dplyr::left_join(.output_a,
                                         by = "concept_id")

                # .output %>%
                #         dplyr::left_join(.output2) %>%
                #         dplyr::filter(concept_name != concept_synonym_name) %>%
                #         dplyr::select(-concept_synonym_id) %>%
                #         rubix::group_by_unique_aggregate(concept_id,
                #                                          concept_name,
                #                                          domain_id,
                #                                          vocabulary_id,
                #                                          concept_class_id,
                #                                          standard_concept,
                #                                          concept_code,
                #                                          valid_start_date,
                #                                          valid_end_date,
                #                                          invalid_reason,
                #                                          agg.col = concept_synonym_name)

        }

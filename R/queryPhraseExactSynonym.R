#' Query Concept Synonym Table for a Phrase Split into Strings
#' @import pg13
#' @import secretary
#' @import dplyr
#' @import rubix
#' @export


queryPhraseExactSynonym <-
        function(schema,
                 caseInsensitive = TRUE,
                 phrase,
                 split,
                 limit_n = NULL,
                 print_sql = TRUE) {

                sql_statement <-
                        pg13::buildQuery(schema = schema,
                                         tableName = "concept_synonym",
                                         whereInField = "concept_synonym_name",
                                         whereInVector = phrase,
                                         caseInsensitive = caseInsensitive)

                if (print_sql) {
                        secretary::typewrite(sql_statement)
                }


                .output <- query_athena(sql_statement = sql_statement) %>%
                                dplyr::select(concept_synonym_id = concept_id,
                                              concept_synonym_name)

                .output2 <-
                leftJoinConcept(.output %>%
                                        dplyr::select(concept_synonym_id))

                .output %>%
                        dplyr::left_join(.output2) %>%
                        dplyr::filter(concept_name != concept_synonym_name) %>%
                        dplyr::select(-concept_synonym_id) %>%
                        rubix::group_by_unique_aggregate(concept_id,
                                                         concept_name,
                                                         domain_id,
                                                         vocabulary_id,
                                                         concept_class_id,
                                                         standard_concept,
                                                         concept_code,
                                                         valid_start_date,
                                                         valid_end_date,
                                                         invalid_reason,
                                                         agg.col = concept_synonym_name)

        }

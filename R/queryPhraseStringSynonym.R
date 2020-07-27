#' Query Concept Synonym Table for a Phrase Split into Strings
#' @import pg13
#' @import secretary
#' @import dplyr
#' @import rubix
#' @export


queryPhraseStringSynonym <-
        function(schema,
                 caseInsensitive = TRUE,
                 phrase,
                 split,
                 limit_n = NULL,
                 print_sql = TRUE) {

                sql_statement <-
                        pg13::buildQueryString(schema = schema,
                                               tableName = "concept_synonym",
                                               whereLikeField = "concept_synonym_name",
                                               string=phrase,
                                               split=split,
                                               limit_n = limit_n,
                                               caseInsensitive = caseInsensitive)

                if (print_sql) {
                        secretary::typewrite(sql_statement)
                }


                .output <- query_athena(sql_statement = sql_statement) %>%
                                dplyr::select(concept_synonym_id = concept_id,
                                              concept_synonym_name)

                leftJoinConcept(.output,
                                column = "concept_synonym_id") %>%
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

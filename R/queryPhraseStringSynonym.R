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
                 render_sql = TRUE,
                 verbose = FALSE,
                 cache_resultset = TRUE,
                 override_cache = FALSE,
                 conn = NULL) {

                sql_statement <-
                        pg13::buildQueryString(schema = schema,
                                               tableName = "concept_synonym",
                                               whereLikeField = "concept_synonym_name",
                                               string=phrase,
                                               split=split,
                                               caseInsensitive = caseInsensitive)

                output1 <-
                        queryAthena(sql_statement = sql_statement,
                                    verbose = verbose,
                                    cache_resultset = cache_resultset,
                                    override_cache = override_cache,
                                    render_sql = render_sql,
                                    conn = conn) %>%
                        dplyr::rename(concept_synonym_id = concept_id)



                leftJoinConcept(output1,
                                column = "concept_synonym_id",
                                athena_schema = schema,
                                conn = conn,
                                render_sql = render_sql) %>%
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

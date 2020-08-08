#' Vocabulary Relationships
#' @description This function retrieves all the relationships of a given vocabulary, identified by the OMOP vocabulary_id.
#' @return dataframe derived from the concept_relationship table where concept_id_1 are all the concepts belonging to the vocabulary_id argument.
#' @import SqlRender
#' @export


queryVocabularyRelationships <-
    function(vocabulary_id,
             verbose = FALSE,
             cache_resultset = TRUE,
             override_cache = FALSE,
             conn = NULL,
             render_sql = FALSE,
             sleepTime = 1,
             ...) {

                        base <- system.file(package = "chariot")
                        path <- paste0(base, "/sql/vocabularyRelationship.sql")

                        sql_statement <-
                                SqlRender::render(SqlRender::readSql(sourceFile = path),
                                                  vocabulary_id = vocabulary_id)

                        queryAthena(sql_statement = sql_statement,
                                    verbose = verbose,
                                    cache_resultset = cache_resultset,
                                    override_cache = override_cache,
                                    conn = conn,
                                    render_sql = render_sql,
                                    sleepTime = sleepTime,
                                    ...)
    }

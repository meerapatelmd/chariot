#' Vocabulary Relationships
#' @description This function retrieves all the relationships of a given vocabulary, identified by the OMOP vocabulary_id.
#' @return dataframe derived from the concept_relationship table where concept_id_1 are all the concepts belonging to the vocabulary_id argument.
#' @import SqlRender
#' @export


queryVocabularyRelationships <-
    function(vocabulary_id) {

                        base <- system.file(package = "chariot")
                        path <- paste0(base, "/sql/vocabularyRelationship.sql")

                        sql_statement <-
                                SqlRender::render(SqlRender::readSql(sourceFile = path),
                                                  vocabulary_id = vocabulary_id)

                        resultset <- query_athena(sql_statement = sql)
                        return(resultset)
    }

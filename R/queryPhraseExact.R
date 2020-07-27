#' Query Exact Phrase
#' @import pg13
#' @export

queryPhraseExact <-
        function(schema,
                 phrase,
                 caseInsensitive) {

                sql_statement <-
                pg13::buildQuery(schema = schema,
                                 tableName = "concept",
                                 whereInField = "concept_name",
                                 whereInVector = phrase,
                                 caseInsensitive = caseInsensitive)


                query_athena(sql_statement = sql_statement)
        }

#' Query Exact Phrase
#' @import pg13
#' @export

queryPhraseExact <-
        function(schema,
                 phrase,
                 caseInsensitive,
                 conn = NULL,
                 render_sql = TRUE,
                 ...) {

                sql_statement <-
                pg13::buildQuery(schema = schema,
                                 tableName = "concept",
                                 whereInField = "concept_name",
                                 whereInVector = phrase,
                                 caseInsensitive = caseInsensitive)


                queryAthena(sql_statement = sql_statement,
                            conn = conn,
                            render_sql = render_sql,
                            ...)
        }

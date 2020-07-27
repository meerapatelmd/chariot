#' Query Like Phrase
#' @import pg13
#' @export

queryPhraseLike <-
        function(schema,
                 phrase,
                 caseInsensitive) {

                sql_statement <-
                        pg13::buildQueryLike(tableName = "concept",
                                             whereLikeField = "concept_name",
                                             whereLikeValue = phrase,
                                             caseInsensitive = caseInsensitive)


                query_athena(sql_statement = sql_statement)
        }

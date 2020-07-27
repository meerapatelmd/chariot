#' Query Concept Table for a Phrase Split into Strings
#' @import pg13
#' @import secretary
#' @export


queryPhraseString <-
        function(schema,
                 caseInsensitive = TRUE,
                 phrase,
                 split,
                 limit_n = NULL,
                 print_sql = TRUE) {

                sql_statement <-
                        pg13::buildQueryString(schema = schema,
                                               tableName = "concept",
                                               whereLikeField = "concept_name",
                                               string=phrase,
                                               split=split,
                                               limit_n = limit_n,
                                               caseInsensitive = caseInsensitive)

                if (print_sql) {
                        secretary::typewrite(sql_statement)
                }


                query_athena(sql_statement = sql_statement)

        }

#' Query Concept Table for a Phrase Split into Strings
#' @import pg13
#' @import secretary
#' @export


queryPhraseString <-
        function(schema,
                 caseInsensitive = TRUE,
                 phrase,
                 split = " ",
                 conn = NULL,
                 render_sql = TRUE,
                 ...) {

                sql_statement <-
                        pg13::buildQueryString(schema = schema,
                                               tableName = "concept",
                                               whereLikeField = "concept_name",
                                               string=phrase,
                                               split=split,
                                               caseInsensitive = caseInsensitive)

                queryAthena(sql_statement = sql_statement,
                            conn = conn,
                            render_sql = render_sql,
                            ...)

        }

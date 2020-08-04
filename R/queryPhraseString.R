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
                 cache_resultset = TRUE,
                 override_cache = FALSE,
                 verbose = FALSE) {

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
                            cache_resultset = cache_resultset,
                            override_cache = override_cache,
                            verbose = verbose)

        }

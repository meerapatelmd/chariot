#' Query Like Phrase
#' @import pg13
#' @export

queryPhraseLike <-
        function(schema,
                 phrase,
                 caseInsensitive,
                 conn = NULL,
                 render_sql = TRUE,
                 cache_resultset = TRUE,
                 override_cache = FALSE,
                 verbose = FALSE) {

                sql_statement <-
                        pg13::buildQueryLike(tableName = "concept",
                                             schema = schema,
                                             whereLikeField = "concept_name",
                                             whereLikeValue = phrase,
                                             caseInsensitive = caseInsensitive)


                queryAthena(sql_statement = sql_statement,
                            conn = conn,
                            render_sql = render_sql,
                            cache_resultset = cache_resultset,
                            override_cache = override_cache,
                            verbose = verbose)
        }

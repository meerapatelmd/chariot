#' Query Concept Synonym Table for Concepts Like Phrase
#' @import pg13
#' @import secretary
#' @import dplyr
#' @import rubix
#' @export


queryPhraseLikeSynonym <-
        function(schema,
                 caseInsensitive = TRUE,
                 phrase,
                 render_sql = TRUE,
                 conn = NULL,
                 verbose = FALSE,
                 cache_resultset = TRUE,
                 override_cache = FALSE) {

                sql_statement <-
                        renderQueryPhraseLikeSynonym(schema = schema,
                                                     phrase = phrase,
                                                     caseInsensitive = caseInsensitive)


                queryAthena(sql_statement = sql_statement,
                            verbose = verbose,
                            render_sql = render_sql,
                            cache_resultset = cache_resultset,
                            override_cache = override_cache,
                            conn = conn)

        }

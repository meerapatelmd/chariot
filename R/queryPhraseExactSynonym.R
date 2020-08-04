#' Query Concept Synonym Table
#' @import pg13
#' @import secretary
#' @import dplyr
#' @import rubix
#' @export


queryPhraseExactSynonym <-
        function(schema,
                 caseInsensitive = TRUE,
                 phrase,
                 render_sql = TRUE,
                 conn = NULL,
                 cache_resultset = TRUE,
                 override_cache = FALSE,
                 verbose = FALSE) {

                sqlStatement <- renderQueryPhraseExactSynonym(schema = schema,
                                                             caseInsensitive = caseInsensitive,
                                                             phrase = phrase)


                queryAthena(sql_statement = sqlStatement,
                            render_sql = render_sql,
                            conn = conn,
                            override_cache = override_cache,
                            cache_resultset = cache_resultset,
                            verbose = verbose)


        }

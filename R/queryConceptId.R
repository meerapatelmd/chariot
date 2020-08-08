#' Lookup a concept id in Athena
#' @import pg13
#' @export

queryConceptId <-
    function(concept_ids,
             schema,
             verbose = FALSE,
             cache_resultset = TRUE,
             override_cache = FALSE,
             conn = NULL,
             render_sql = FALSE,
             sleepTime = 1,
             ...) {


                            sql <-
                            pg13::buildQuery(schema = schema,
                                             tableName = "concept",
                                             whereInField = "concept_id",
                                             whereInVector = concept_ids,
                                             caseInsensitive = FALSE)

                            queryAthena(sql_statement = sql_statement,
                                        verbose = verbose,
                                        cache_resultset = cache_resultset,
                                        override_cache = override_cache,
                                        conn = conn,
                                        render_sql = render_sql,
                                        sleepTime = sleepTime,
                                        ...)

    }

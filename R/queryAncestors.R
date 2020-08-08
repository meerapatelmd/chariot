#' Query ancestors for a given concept_id
#' @export

queryAncestors <-
    function(descendant_concept_ids,
             schema,
             min_levels_of_separation = NULL,
             max_levels_of_separation = NULL,
             verbose = FALSE,
             cache_resultset = TRUE,
             override_cache = FALSE,
             conn = NULL,
             render_sql = FALSE,
             sleepTime = 1,
             ...) {


            sql_statement <- renderQueryAncestors(descendant_concept_ids = descendant_concept_ids,
                                                  schema = schema,
                                                  min_levels_of_separation = min_levels_of_separation,
                                                  max_levels_of_separation = max_levels_of_separation)

            queryAthena(sql_statement = sql_statement,
                        verbose = verbose,
                        cache_resultset = cache_resultset,
                        override_cache = override_cache,
                        conn = conn,
                        render_sql = render_sql,
                        sleepTime = sleepTime,
                         ...)

    }

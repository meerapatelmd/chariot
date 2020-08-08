#' Query a HemOnc Regimen's 'Has antineoplastic' Relationship
#' @export

queryHemOncRegToAntineo <-
        function(regimen_concept_ids,
                 schema = NULL,
                 verbose = FALSE,
                 cache_resultset = TRUE,
                 override_cache = FALSE,
                 conn = NULL,
                 render_sql = FALSE,
                 sleepTime = 1,
                 ...) {

                sql_statement <-
                        renderHemOncRegToAntineoplastics(regimen_concept_ids = regimen_concept_ids,
                                                         schema = schema)

                queryAthena(sql_statement = sql_statement,
                            verbose = verbose,
                            cache_resultset = cache_resultset,
                            override_cache = override_cache,
                            conn = conn,
                            render_sql = render_sql,
                            sleepTime = sleepTime,
                            ...)
        }

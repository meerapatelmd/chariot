#' Generic query function
#' @description This is the low level function that is wrapped for more specific queries. The resultset is cached using the R.cache package
#' @param sql_statement SQL statement
#' @importFrom DBI dbGetQuery
#' @importFrom secretary typewrite_bold
#' @export

queryAthena <-
        function(sql_statement,
                 verbose = FALSE,
                 cache_resultset = TRUE,
                 override_cache = FALSE) {



            if (cache_resultset) {

                    if (override_cache) {

                        conn <- connectAthena()
                        resultset <- pg13::query(conn = conn,
                                                sql_statement = sql_statement)

                        cache_query(resultset, key=sql_statement)
                        dcAthena(conn = conn)

                    } else {

                        resultset <- tryCatch(load_cached_query(key=sql_statement),
                                              error = function(e) NULL)

                        if (is.null(resultset)) {

                                conn <- connectAthena()
                                resultset <- pg13::query(conn = conn,
                                                         sql_statement = sql_statement)
                                cache_query(resultset, key=sql_statement)
                                dcAthena(conn = conn)

                        } else {

                                if (verbose) {

                                        secretary::typewrite_bold("Loading resultset from cache", line_number = 0)

                                }

                        }
                    }

            } else {

                conn <- connectAthena()
                resultset <- pg13::query(conn = conn,
                                         sql_statement = sql_statement)
                dcAthena(conn = conn)

            }

            return(resultset)
        }

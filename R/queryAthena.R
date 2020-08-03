#' Query a Postgres Database
#' @description By default, this function queries a local database named "Athena". If a connection object is passed into the function, the database of the connection object is queried instead.
#' @param sql_statement SQL query
#' @param cache_resultset If TRUE, the resultset from the query will first be loaded from the cache. If there isn't an existing cache entry for this particular query a new one will be made. If FALSE, Athena will be directly queried without an caching operations.
#' @param override_cache If TRUE, the cache will not be loaded and will be overwritten by a new fresh query. For override_cache to take effect, cache_resultset must be set to TRUE.
#' @param conn If provided, serves as the target database from which the query is derived.
#' @return A tibble
#' @importFrom secretary typewrite_bold
#' @importFrom tibble as_tibble
#' @export

queryAthena <-
        function(sql_statement,
                 verbose = FALSE,
                 cache_resultset = TRUE,
                 override_cache = FALSE,
                 conn = NULL,
                 render_sql = FALSE) {

                if (render_sql) {
                        secretary::typewrite_bold("Rendered SQL:")
                        secretary::typewrite(stringr::str_remove_all(sql_statement, "\n"), tabs = 1)
                        cat("\n")
                }



                if (is.null(conn)) {

                        if (cache_resultset) {

                                if (override_cache) {

                                        conn <- connectAthena()
                                        resultset <- pg13::query(conn = conn,
                                                                 sql_statement = sql_statement)


                                        pg13::cacheQuery(resultset,
                                                         sqlQuery = sql_statement,
                                                         db = "athena")

                                        dcAthena(conn = conn)

                                } else {

                                        resultset <- tryCatch(pg13::loadCachedQuery(sqlQuery = sql_statement,
                                                                                    db = "athena"),
                                                              error = function(e) NULL)

                                        if (is.null(resultset)) {

                                                conn <- connectAthena()

                                                resultset <- pg13::query(conn = conn,
                                                                         sql_statement = sql_statement)

                                                pg13::cacheQuery(resultset,
                                                                 sqlQuery = sql_statement,
                                                                 db = "athena")

                                                dcAthena(conn = conn)

                                        } else {

                                                if (verbose) {

                                                        secretary::typewrite_bold("Loaded resultset from cache", line_number = 0)

                                                }

                                        }
                                }

                        } else {

                                conn <- connectAthena()
                                resultset <- pg13::query(conn = conn,
                                                         sql_statement = sql_statement)
                                dcAthena(conn = conn)

                        }
                } else {

                        if (!.hasSlot(conn, name = "jConnection")) {

                                stop('conn object must be a Database Connector JDBC Connection')

                        }


                        db <- conn@jConnection$getCatalog()

                        if (cache_resultset) {

                                if (override_cache) {

                                        resultset <- pg13::query(conn = conn,
                                                                 sql_statement = sql_statement)


                                        pg13::cacheQuery(resultset,
                                                         sqlQuery = sql_statement,
                                                         db = db)


                                } else {


                                        resultset <- tryCatch(pg13::loadCachedQuery(sqlQuery = sql_statement,
                                                                                    db = db),
                                                              error = function(e) NULL)

                                        if (is.null(resultset)) {

                                                resultset <- pg13::query(conn = conn,
                                                                         sql_statement = sql_statement)

                                                pg13::cacheQuery(resultset,
                                                                 sqlQuery = sql_statement,
                                                                 db = db)


                                        } else {

                                                if (verbose) {

                                                        secretary::typewrite_bold("Loaded resultset from cache", line_number = 0)

                                                }

                                        }
                                }

                        } else {
                                if (render_sql) {
                                        secretary::typewrite_bold("Rendered SQL:")
                                        secretary::typewrite(stringr::str_remove_all(sql_statement, "\n"), tabs = 1)
                                        cat("\n")
                                }

                                resultset <- pg13::query(conn = conn,
                                                         sql_statement = sql_statement)

                        }

                }


                return(resultset %>%
                               tibble::as_tibble())

        }


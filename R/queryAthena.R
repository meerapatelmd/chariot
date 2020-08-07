#' @title Query the Athena Postgres Database
#' @description
#' By default, this function queries a local database named "Athena". If a connection object is passed into the function, the database of the connection object is queried instead.
#'
#' @param sql_statement         SQL query
#' @param cache_resultset       If TRUE, the resultset from the query will first be loaded from the cache. The query will be executed if a cached resultset is not retrieved for this particular query, after which the resultset will be cached. If FALSE, Athena or conn will be directly queried without any caching operations.
#' @param override_cache        If TRUE, the cache will not be loaded and will be overwritten by a new query. For override_cache to take effect, cache_resultset must also be set to TRUE.
#' @param conn                  Connection object. If provided, diverts queries to the connection instead of the local Athena instance.
#' @param render_sql            If TRUE, the SQL will be printed back in the console prior to execution. Default: FALSE
#' @param verbose               If TRUE, prints loading and querying operations messages to the console. Default: FALSE
#' @param sleepTime             Argument for `Sys.sleep()` in between queries to allow for halting function execution, especially in cases where other chariot functions are executing multiple queries in succession and require cancellation.
#'
#' @return
#' A tibble
#'
#' @seealso
#'  \code{\link[secretary]{typewrite_bold}},\code{\link[secretary]{typewrite}}
#'  \code{\link[stringr]{str_replace}},\code{\link[stringr]{str_remove}}
#'  \code{\link[pg13]{query}},\code{\link[pg13]{cacheQuery}},\code{\link[pg13]{loadCachedQuery}}
#'  \code{\link[tibble]{as_tibble}}
#' @rdname queryAthena
#' @export
#' @importFrom secretary typewrite_bold typewrite
#' @importFrom stringr str_replace_all str_remove_all
#' @importFrom pg13 query cacheQuery loadCachedQuery
#' @importFrom tibble as_tibble


queryAthena <-
        function(sql_statement,
                 verbose = FALSE,
                 cache_resultset = TRUE,
                 override_cache = FALSE,
                 conn = NULL,
                 render_sql = FALSE,
                 sleepTime = 1) {

                if (render_sql) {

                        cat("\n")
                        secretary::typewrite_bold(paste0("[", as.character(Sys.time()), "]"), "Rendered SQL:")
                        secretary::typewrite(centipede::trimws(stringr::str_replace_all(sql_statement, "\n|\\s{2,}", " ")), tabs = 1)
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


                Sys.sleep(time = sleepTime)
                return(tibble::as_tibble(resultset))

        }


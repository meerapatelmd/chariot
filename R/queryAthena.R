#' @title Query the Athena Postgres Database
#' @description
#' By default, this function queries a local database named "Athena". If a connection object is passed into the function, the database of the connection object is queried instead. The caching feature is only available when using the built-in connection to Athena.
#'
#' @param sql_statement         SQL query
#' @param cache_only            Loads from the cache and does not query the database. A NULL object is returned if a resultset was not cached.
#' @param skip_cache            Skip the caching altogether and directly query the database.
#' @param override_cache        If TRUE, the cache will not be loaded and will be overwritten by a new query. For override_cache to take effect, skip_cache should be FALSE.
#' @param conn                  Connection object. If provided, diverts queries to the connection instead of the local Athena instance without caching features.
#' @param render_sql            If TRUE, the SQL will be printed back in the console prior to execution. Default: FALSE
#' @param verbose               If TRUE, prints loading and querying operations messages to the console. Default: FALSE
#' @param sleepTime             Argument for `Sys.sleep()` in between queries to allow for halting function execution, especially in cases where other chariot functions are executing multiple queries in succession and require cancellation.
#' @param cache_resultset       (deprecated) If TRUE, the resultset from the query will first be loaded from the cache. The query will be executed if a cached resultset is not retrieved for this particular query, after which the resultset will be cached. If FALSE, Athena or conn will be directly queried without any caching operations.
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
                 conn = NULL,
                 cache_only = FALSE,
                 skip_cache = FALSE,
                 override_cache = FALSE,
                 cache_resultset = TRUE,
                 render_sql = FALSE,
                 verbose = FALSE,
                 sleepTime = 1) {

                if (is.null(conn)) {

                        conn_was_missing <- TRUE

                        conn <- connectAthena()


                } else {

                        conn_was_missing <- FALSE

                        if (!.hasSlot(conn, name = "jConnection")) {

                                stop('conn object must be a Database Connector JDBC Connection')

                        }

                }

                if (render_sql) {

                        cat("\n")
                        secretary::typewrite_bold(paste0("[", as.character(Sys.time()), "]"), "Rendered SQL:")
                        secretary::typewrite(centipede::trimws(stringr::str_replace_all(sql_statement, "\n|\\s{2,}", " ")), tabs = 1)
                        cat("\n")

                }

                 if (conn_was_missing) {

                                if (skip_cache) {

                                        if (verbose) {
                                                secretary::typewrite("Skipping cache")
                                        }

                                        resultset <- pg13::query(conn = conn,
                                                                 sql_statement = sql_statement)

                                        # resultset <- tryCatch(pg13::loadCachedQuery(sqlQuery = sql_statement,
                                        #                                             db = "athena"),
                                        #                       error = function(e) NULL)


                                } else {

                                        if (override_cache) {

                                                if (verbose) {
                                                        secretary::typewrite("Overriding cache")
                                                }

                                                resultset <- pg13::query(conn = conn,
                                                                         sql_statement = sql_statement)

                                                pg13::cacheQuery(resultset,
                                                                 sqlQuery = sql_statement,
                                                                 db = "athena")


                                        } else {

                                                if (verbose) {
                                                        secretary::typewrite("Loading Cache")
                                                }


                                                resultset <- tryCatch(pg13::loadCachedQuery(sqlQuery = sql_statement,
                                                                                            db = "athena"),
                                                                      error = function(e) NULL)

                                                if (!cache_only) {

                                                        if (is.null(resultset)) {


                                                                if (verbose) {
                                                                        secretary::typewrite("Cache was NULL, querying Athena")
                                                                }

                                                                resultset <- pg13::query(conn = conn,
                                                                                         sql_statement = sql_statement)

                                                                pg13::cacheQuery(resultset,
                                                                                 sqlQuery = sql_statement,
                                                                                 db = "athena")

                                                        }

                                                } else {

                                                        if (verbose) {

                                                                secretary::typewrite_bold("Loaded resultset from cache", line_number = 0)

                                                        }
                                                }

                                        }
                                }

                 } else {


                         resultset <- pg13::query(conn = conn,
                                                  sql_statement = sql_statement)


                 }

                Sys.sleep(time = sleepTime)


                if (conn_was_missing) {
                        dcAthena(conn = conn)
                }

                return(tibble::as_tibble(resultset))

        }


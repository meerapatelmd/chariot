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
#' @return a [tibble][tibble::tibble-package]
#'
#' @seealso
#'  \code{\link[rlang]{parse_expr}}
#'  \code{\link[pg13]{is_conn_open}},\code{\link[pg13]{query}},\code{\link[pg13]{cacheQuery}},\code{\link[pg13]{loadCachedQuery}}
#'  \code{\link[secretary]{c("typewrite", "typewrite")}},\code{\link[secretary]{character(0)}}
#'  \code{\link[tibble]{as_tibble}}
#' @rdname queryAthena
#' @export
#' @family query functions
#' @importFrom rlang parse_expr
#' @importFrom secretary typewrite magentaTxt
#' @importFrom tibble as_tibble

queryAthena <-
        function(sql_statement,
                 conn,
                 conn_fun = "connectAthena()",
                 cache_only = FALSE,
                 skip_cache = FALSE,
                 override_cache = FALSE,
                 cache_resultset = TRUE,
                 render_sql = TRUE,
                 verbose = TRUE,
                 sleepTime = 1) {


                if (missing(conn)) {

                        conn <- eval(expr = rlang::parse_expr(x = conn_fun))
                        on.exit(expr = dcAthena(conn = conn,
                                                verbose = verbose),
                                add = TRUE,
                                after = TRUE)

                }


                db <- get_conn_db(conn = conn)

                if (skip_cache) {

                        if (verbose) {
                                secretary::typewrite(secretary::magentaTxt("Skipping cache..."))
                        }

                        resultset <- lowLevelQuery(conn = conn,
                                                 sql_statement = sql_statement,
                                                 verbose = verbose,
                                                 render_sql = render_sql)

                } else {

                        if (override_cache) {

                                if (verbose) {
                                        secretary::typewrite(secretary::magentaTxt("Overriding cache... Querying Athena..."))
                                }

                                resultset <- lowLevelQuery(conn = conn,
                                                         sql_statement = sql_statement,
                                                         verbose = verbose,
                                                         render_sql = render_sql)

                                if (verbose) {
                                        secretary::typewrite(secretary::magentaTxt("Caching resultset..."))
                                }

                                lowLevelCache(data = resultset,
                                              query = sql_statement)


                        } else {

                                if (verbose) {
                                        secretary::typewrite(secretary::magentaTxt("Loading Cache..."))
                                        secretary::typewrite(secretary::magentaTxt("Cached SQL:"), sql_statement)
                                }


                                resultset <- lowLevelLoadCache(query = sql_statement)

                                if (!cache_only) {

                                        if (is.null(resultset)) {


                                                if (verbose) {
                                                        secretary::typewrite(secretary::magentaTxt("No cached resultset found... querying Athena..."))
                                                }

                                                Sys.sleep(time = sleepTime)
                                                resultset <- lowLevelQuery(conn = conn,
                                                                         sql_statement = sql_statement,
                                                                         verbose = verbose,
                                                                         render_sql = render_sql)

                                                if (verbose) {
                                                        secretary::typewrite(secretary::magentaTxt("Caching resultset..."))
                                                }

                                                lowLevelCache(data = resultset,
                                                              query = sql_statement)

                                        }


                                } else {

                                        if (verbose) {

                                                secretary::typewrite(secretary::magentaTxt("Cached resultset found..."))

                                        }
                                }

                        }
                }


                tibble::as_tibble(resultset)

        }






#' @title
#' Send a SQL Statement
#' @description
#' Unlike the \code{\link{queryAthena}} function, the render_sql parameter provides a pause after rendering in case the user wants to copy and paste the rendered SQL into a client if the session is interactive. This is particularly useful with large operations that are better executed within a background client.
#' @param conn PARAM_DESCRIPTION, Default: NULL
#' @param sql_statement PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @seealso
#'  \code{\link[pg13]{send}}
#' @rdname sendAthena
#' @export



sendAthena <-
        function(conn,
                 conn_fun = "connectAthena()",
                 sql_statement,
                 verbose = TRUE,
                 render_sql = TRUE) {


                if (missing(conn)) {

                        conn <- eval(expr = rlang::parse_expr(x = conn_fun))
                        on.exit(expr = dcAthena(conn = conn,
                                                verbose = verbose),
                                add = TRUE,
                                after = TRUE)

                }

                check_conn(conn = conn)

                if (render_sql) {

                        typewrite_sql(sql_statement = sql_statement)
                }

                lowLevelSend(conn = conn,
                           sql_statement = sql_statement,
                           verbose = verbose,
                           render_sql = render_sql)

        }










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
#' @importFrom tibble as_tibble


executeAthena <-
        function(sql_statement,
                 conn,
                 conn_fun = "connectAthena()",
                 cache_only = FALSE,
                 skip_cache = FALSE,
                 override_cache = FALSE,
                 cache_resultset = TRUE,
                 render_sql = FALSE,
                 verbose = FALSE,
                 sleepTime = 1) {

                if (missing(conn)) {

                        conn <- eval(expr = rlang::parse_expr(x = conn_fun))
                        on.exit(expr = dcAthena(conn = conn,
                                                verbose = verbose),
                                add = TRUE,
                                after = TRUE)

                }

                check_conn(conn = conn)


                        if (skip_cache) {

                                if (verbose) {
                                        secretary::typewrite("Skipping cache")
                                }

                                resultset <- lowLevelQuery(conn = conn,
                                                         sql_statement = sql_statement,
                                                         verbose = verbose,
                                                         render_sql = render_sql)

                        } else {

                                if (override_cache) {

                                        if (verbose) {
                                                secretary::typewrite("Overriding cache")
                                        }

                                        resultset <- lowLevelQuery(conn = conn,
                                                                 sql_statement = sql_statement,
                                                                 verbose = verbose,
                                                                 render_sql = render_sql)

                                        lowLevelCache(data = resultset,
                                                      query = sql_statement)


                                } else {

                                        if (verbose) {
                                                secretary::typewrite("Loading Cache")
                                        }


                                        resultset <- lowLevelLoadCache(query = sql_statement)

                                        if (!cache_only) {

                                                if (is.null(resultset)) {


                                                        if (verbose) {
                                                                secretary::typewrite("Cache was NULL, querying Athena")
                                                        }

                                                        Sys.sleep(time = sleepTime)
                                                        resultset <- lowLevelQuery(conn = conn,
                                                                                 sql_statement = sql_statement,
                                                                                 verbose = verbose,
                                                                                 render_sql = render_sql)

                                                        lowLevelCache(data = resultset,
                                                                         query = sql_statement)

                                                }

                                        } else {

                                                if (verbose) {

                                                        secretary::typewrite_bold("Loaded resultset from cache", line_number = 0)

                                                }
                                        }

                                }
                        }

                tibble::as_tibble(resultset)

        }






#' @title Query the Athena Postgres Database
#' @description
#' By default, this function queries a local database named "Athena". If a
#' connection object is passed into the function, the database of the connection
#' object is queried instead. A caching feature is included.
#'
#' @param sql_statement         SQL query
#' @param version_key           `VersionKey` class object.
#' @param cache_only            Loads from the cache and does not query the
#' database. A NULL object is returned if a resultset was not cached.
#' @param skip_cache            Skip the caching altogether and directly
#' query the database.
#' @param override_cache        If TRUE, the cache will not be loaded and will
#' be overwritten by a new query. For override_cache to take effect,
#' skip_cache should be FALSE.
#' @param conn                  Connection object. If provided, diverts queries
#' to the connection instead of the local Athena instance without caching
#' features.
#' @param render_sql            If TRUE, the SQL will be printed back in the
#' console prior to execution. Default: FALSE
#' @param verbose               If TRUE, prints loading and querying operations
#' messages to the console. Default: FALSE
#' @param sleepTime             Argument for `Sys.sleep()` in between queries to
#' allow for halting function execution, especially in cases where other chariot
#' functions are executing multiple queries in succession and require
#' cancellation.
#' @param cache_resultset       (deprecated) If TRUE, the resultset from the
#' query will first be loaded from the cache. The query will be executed if a
#' cached resultset is not retrieved for this particular query, after which the
#' resultset will be cached. If FALSE, Athena or conn will be directly queried
#' without any caching operations.
#'
#' @return a [tibble][tibble::tibble-package]
#'
#' @seealso
#'  \code{\link[rlang]{parse_expr}}
#'  \code{\link[pg13]{is_conn_open}},\code{\link[pg13]{query}},
#'  \code{\link[pg13]{cacheQuery}},\code{\link[pg13]{loadCachedQuery}}
#'  \code{\link[secretary]{c("typewrite", "typewrite")}},
#'  \code{\link[tibble]{as_tibble}}
#' @rdname query_athena
#' @export
#' @family query functions
#' @importFrom rlang parse_expr
#' @importFrom secretary typewrite magentaTxt
#' @importFrom tibble as_tibble

query_athena <-
        function(sql_statement,
                 version_key = get_VersionKey(),
                 conn,
                 conn_fun = "pg13::local_connect()",
                 skip_cache = FALSE,
                 override_cache = FALSE,
                 cache_only = FALSE,
                 render_sql = FALSE,
                 render_only = FALSE,
                 verbose = FALSE,
                 sleepTime = 1) {


                new_chariotResultset <-
                        function(x = tibble::tibble(),
                                 fetched_via) {


                                structure(
                                        x,
                                        fetched_via = fetched_via,
                                        class = c("chariotResultset",
                                                  "tbl_df",
                                                  "tbl",
                                                  "data.frame")
                                )



                        }


                if (cache_only) {

                        if (verbose) {
                                secretary::typewrite(secretary::magentaTxt("Loading Cache..."))
                                secretary::typewrite(secretary::magentaTxt("Cached SQL:"), sql_statement)
                        }

                        resultset <-
                                R.cache::loadCache(key = list(sql_statement, version_key),
                                                   dirs = "chariot")


                        new_chariotResultset(x = resultset,
                                             fetched_via = "cache_only")


                }



                if (skip_cache) {
                        if (verbose) {
                                secretary::typewrite(secretary::magentaTxt("Skipping cache..."))
                        }

                        if (missing(conn)) {

                                conn <- eval(rlang::parse_expr(conn_fun))
                                on.exit(expr = dcAthena(conn = conn),
                                        add = TRUE,
                                        after = TRUE)
                        }

                        resultset <- pg13::query(
                                conn = conn,
                                # conn_fun = conn_fun,
                                sql_statement = sql_statement,
                                verbose = verbose,
                                render_sql = render_sql,
                                render_only = render_only)

                        new_chariotResultset(x = resultset,
                                             fetched_via = "skip_cache")



                }

                if (override_cache) {
                        if (verbose) {
                                secretary::typewrite(secretary::magentaTxt("Overriding cache... Querying Athena..."))
                        }

                        if (missing(conn)) {

                                conn <- eval(rlang::parse_expr(conn_fun))
                                on.exit(expr = dcAthena(conn = conn),
                                        add = TRUE,
                                        after = TRUE)
                        }

                        resultset <- pg13::query(
                                conn = conn,
                                # conn_fun = conn_fun,
                                sql_statement = sql_statement,
                                verbose = verbose,
                                render_sql = render_sql,
                                render_only = render_only
                        )


                        if (verbose) {
                                secretary::typewrite(secretary::magentaTxt("Caching resultset..."))
                        }

                        R.cache::saveCache(
                                object = resultset,
                                key = list(sql_statement,
                                           version_key),
                                dirs = "chariot"
                        )

                        new_chariotResultset(x = resultset,
                                             fetched_via = "override_cache")


                }


                if (verbose) {
                        secretary::typewrite(secretary::magentaTxt("Loading Cache..."))
                        secretary::typewrite(secretary::magentaTxt("Cached SQL:"), sql_statement)
                }


                resultset <-
                        R.cache::loadCache(key = list(sql_statement, version_key),
                                           dirs = "chariot")

                if (is.null(resultset)) {
                        if (verbose) {
                                secretary::typewrite(secretary::magentaTxt("No cached resultset found... querying Athena..."))
                        }

                        if (missing(conn)) {

                                conn <- eval(rlang::parse_expr(conn_fun))
                                on.exit(expr = dcAthena(conn = conn),
                                        add = TRUE,
                                        after = TRUE)
                        }

                        Sys.sleep(time = sleepTime)
                        resultset <- pg13::query(
                                conn = conn,
                                # conn_fun = conn_fun,
                                sql_statement = sql_statement,
                                verbose = verbose,
                                render_sql = render_sql,
                                render_only = render_only
                        )


                        if (verbose) {
                                secretary::typewrite(secretary::magentaTxt("Caching resultset..."))
                        }

                        R.cache::saveCache(
                                object = resultset,
                                key = list(sql_statement,
                                           version_key),
                                dirs = "chariot"
                        )


                        new_chariotResultset(x = resultset,
                                             fetched_via = "new_cached_query")

                } else {
                        if (verbose) {
                                secretary::typewrite(secretary::magentaTxt("Cached resultset found..."))
                        }

                        new_chariotResultset(x = resultset,
                                             fetched_via = "cached_query")


                }
        }


#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param chariotResultsetObj PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname fetched_via
#' @export
fetched_via <-
        function(chariotResultsetObj) {

        attributes(chariotResultsetObj)$fetched_via


}

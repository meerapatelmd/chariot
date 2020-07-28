#' Load a cached query
#' @description This function differs from the query_athena function in that it only returns the cached query resultset if it exists and does not query the local database.
#' @param sql_statement SQL statement
#' @importFrom secretary typewrite_bold
#' @export

load_athena_query <-
        function(sql_statement, verbose = FALSE) {
            resultset <- load_cached_query(key=sql_statement)

            if (verbose) {
                    if (is.null(resultset)) {
                        secretary::typewrite_warning("Query is not cached.", line_number = 0)
                    } else {
                        secretary::typewrite_bold("Loading resultset from cache", line_number = 0)
                    }
            }

            return(resultset)
        }

#' Generic query function
#' @param sql_statement SQL statement
#' @importFrom seagull connect_to_local_postgres
#' @importFrom DBI dbGetQuery
#' @export

query_athena <-
        function(sql_statement) {
            resultset <- load_cached_query(key=sql_statement)
            if (is.null(resultset)) {
                conn <- seagull::connect_to_local_postgres(dbname = "athena")
                resultset <- DBI::dbGetQuery(conn, statement = sql_statement)
                cache_query(resultset, key=sql_statement)
                DBI::dbDisconnect(conn)
            }
            return(resultset)
        }

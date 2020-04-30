#' Generic query function
#' @param sql_statement SQL statement
#' @importFrom seagull connect_to_local_postgres
#' @importFrom DBI dbGetQuery
#' @export

query_athena <-
        function(sql_statement) {
                conn <- seagull::connect_to_local_postgres(dbname = "athena")
                resultset <- DBI::dbGetQuery(conn, statement = sql_statement)
                return(resultset)
        }

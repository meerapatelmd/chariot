#' Generic query function
#' @param sql_statement SQL statement
#' @importFrom mySeagull get_query
#' @export



query_athena <-
        function(sql_statement) {
                resultset <- mySeagull::get_athena_query(sql_statement = sql_statement)
                return(resultset)
        }

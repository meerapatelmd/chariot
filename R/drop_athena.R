#' Drop Athena
#' @import seagull
#' @import DatabaseConnector
#' @export

drop_athena <- 
    function() {
        conn <- seagull::connect_to_local_postgres(dbname = "patelm9")
        
        DatabaseConnector::dbSendStatement(conn,
                                           "DROP DATABASE athena;")
        DatabaseConnector::dbDisconnect(conn)
        
    }


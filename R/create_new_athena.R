#' Create new Athena local database
#' @import seagull
#' @import DatabaseConnector
#' @export

create_new_athena <- 
    function() {
        conn <- seagull::connect_to_local_postgres(dbname = "patelm9")
        
        DatabaseConnector::dbSendStatement(conn,
                                           "CREATE DATABASE athena;")
        DatabaseConnector::dbDisconnect(conn)
        
    }

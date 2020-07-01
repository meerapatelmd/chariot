#' Retire Athena version
#' @import seagull
#' @import DatabaseConnector
#' @export

retire_athena_version <- 
    function() {
        
        new_db_name <- paste0("athena", rubix::dated(punct = TRUE))
        
        conn <- seagull::connect_to_local_postgres(dbname = "patelm9")
        
        DatabaseConnector::dbSendStatement(conn,
                                           paste0("ALTER DATABASE athena RENAME TO ", new_db_name, ";"))
        
        DatabaseConnector::dbDisconnect(conn)
    }
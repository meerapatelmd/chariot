#' Drop Athena Tables
#' @description Drops all tables in the format ("V{timestamp}")
#' @import DatabaseConnector
#' @export

drop_left_join_tables <- 
    function() {
        
        conn <- connect_athena()
        
        tables <-
                DatabaseConnector::dbListTables(conn = conn)
        
        left_join_tables <- 
                grep("^V[0-9]{14}$", tables, value = TRUE)
        
        while (length(left_join_tables) > 0) {
                    left_join_table <- left_join_tables[1]
                    
                    DatabaseConnector::dbRemoveTable(conn = conn,
                                                     name = left_join_table)
                    
                    left_join_tables <- left_join_tables[-1]
        }
        
        dc_athena(conn = conn)
    }

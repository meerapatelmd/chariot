#' Execute Athena DDL
#' @import seagull
#' @import DatabaseConnector
#' @export

execute_athena_ddl <- 
    function() {
        
        
        tmp <- tempfile(fileext = ".txt")
        
        download.file(url = "https://raw.githubusercontent.com/OHDSI/CommonDataModel/master/PostgreSQL/OMOP%20CDM%20postgresql%20ddl.txt",
                      destfile = tmp)
        
        conn <- seagull::connect_to_local_postgres(dbname = "athena")
        DatabaseConnector::dbSendStatement(conn = conn,
                                           statement = read_file(tmp))
        
        DatabaseConnector::dbDisconnect(conn = conn)
        
    }


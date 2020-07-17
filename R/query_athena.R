#' Generic query function
#' @description This is the low level function that is wrapped for more specific queries. The resultset is cached using the R.cache package
#' @param sql_statement SQL statement
#' @importFrom DBI dbGetQuery
#' @importFrom secretary typewrite_bold
#' @export

query_athena <-
        function(sql_statement, 
                 verbose = FALSE, 
                 cache_resultset = TRUE, 
                 override_cache = FALSE) {
            
            if (cache_resultset) {
                
                    if (override_cache) {
                        
                        conn <- connect_athena()
                        resultset <- DBI::dbGetQuery(conn, statement = sql_statement)
                        cache_query(resultset, key=sql_statement)
                        dc_athena(conn)
                        
                    } else {
                        
                        resultset <- tryCatch(load_cached_query(key=sql_statement),
                                              error = function(e) NULL)
                        
                        if (is.null(resultset)) {
                            
                                conn <- seagull::connect_to_local_postgres(dbname = "athena")
                                resultset <- DBI::dbGetQuery(conn, statement = sql_statement)
                                cache_query(resultset, key=sql_statement)
                                DBI::dbDisconnect(conn)
                            
                        } else {
                            
                                if (verbose) {
                                    
                                        secretary::typewrite_bold("Loading resultset from cache", line_number = 0)
                                    
                                }
                            
                        }
                    }
                
            } else {
                
                conn <- seagull::connect_to_local_postgres(dbname = "athena")
                resultset <- DBI::dbGetQuery(conn, statement = sql_statement)
                DBI::dbDisconnect(conn)
                
            }
            
            return(resultset)
        }

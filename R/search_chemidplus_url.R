#' Get metadata on a phrase
#' @description Get metadata
#' @import httr
#' @import rvest
#' @import stringr
#' @import police
#' @import centipede
#' @export

search_chemidplus_url <-
    function(phrase,
             type = "contains",
             conn) {
        
            destSchema <- "chemidplus"
            destTable <- "url"

            #Remove all spaces
            phrase <- stringr::str_remove_all(phrase, "\\s")
        
            url0 <- paste0("https://chem.nlm.nih.gov/chemidplus/name/", type, "/",  phrase)
            url <- police::try_catch_error_as_null(url(url0, "rb"))

            if (!is.null(url)) {
                
                            url_status <- "valid"
                    
                            Sys.sleep(5)
    
                            resp <- police::try_catch_error_as_null(xml2::read_html(url))
                            
                            Sys.sleep(5)
                            
                            close(url)
                
                            if (!is.null(resp)) {
                            # If there aren't any #names headers, it is likely that the query resulted in multiple search results and needs to be tied to an RN number
                                
                                        response_type <- "available"
                        
                                
                            # For single page responses
                            } else {
                                
                                    response_type <- "not available"
    
                            }
    
                                    
            } else {
                
                            response_type <- "invalid url"
                    
                            url_status <- "invalid"
                    
            }
            
                
            final <-
                    data.frame(dte = as.character(Sys.time()),
                               phrase = phrase,
                               url0 = url0,
                               url_status = url_status,
                               response_type = response_type,
                               metadata_status = "") %>%
                    rubix::call_mr_clean()
            
            
            closeAllConnections()
            
            sql <- SqlRender::render(SqlRender::readSql("inst/sql/loadChemiDPlusMetadata.sql"), destSchema = destSchema)
            
            DatabaseConnector::executeSql(connection = conn, 
                                          sql, 
                                          progressBar = TRUE, 
                                          reportOverallTime = TRUE)
            
            tablename <- paste0(destSchema, ".", destTable)
            prior_tablename <- paste0(destSchema, ".prior_", destTable)
            
            tablename_exists <- toupper(destTable) %in% toupper((DatabaseConnector::dbListTables(conn = conn, schema = destSchema)))
            
            if (!tablename_exists) {

                        DatabaseConnector::dbWriteTable(conn = conn,
                                                        name = tablename,
                                                        value = final %>%
                                                            as.data.frame())
                
            } else {
                
                        current_metadata <- DatabaseConnector::dbGetQuery(conn = conn,
                                                                          paste0("SELECT * FROM ", tablename))
                
                        final <- 
                                dplyr::bind_rows(current_metadata,
                                                 final)
                        
                       DatabaseConnector::dbSendStatement(conn = conn,
                                                          paste0("DROP TABLE IF EXISTS ", prior_tablename))
                        
                        DatabaseConnector::dbWriteTable(conn = conn,
                                                        name = prior_tablename,
                                                        value = current_metadata %>%
                                                                    as.data.frame())
                        
                        DatabaseConnector::dbRemoveTable(conn = conn,
                                                         name = tablename)
                
                        DatabaseConnector::dbWriteTable(conn = conn,
                                                        name = tablename,
                                                        value = final %>%
                                                            as.data.frame())
                
            }
    }

       
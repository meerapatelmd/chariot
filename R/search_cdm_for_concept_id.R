#' Search CDM instance for a concept_id
#' @importFrom DatabaseConnector dbListTables
#' @importFrom DatabaseConnector dbListFields
#' @importFrom DatabaseConnector dbGetQuery
#' @importFrom tibble tibble
#' @importFrom dplyr bind_rows
#' @importFrom dplyr distinct
#' @export

search_cdm_for_concept_id <- 
    function(conn, concept_id, schema = NULL) {
        
            cdm_tables2 <-
                DatabaseConnector::dbListTables(conn = conn,
                                                schema = schema)
            
            output <- tibble::tibble()
            
            
            while (length(cdm_tables2) > 0) {
                
                    cdm_table <- cdm_tables2[1]
                    
                    cdm_table_fields <-
                            DatabaseConnector::dbListFields(conn = conn,
                                                            name = cdm_table)
                    
                    
                    cdm_table_cid_fields <-
                            grep("concept_id", cdm_table_fields, ignore.case = TRUE,
                                 value = TRUE)
                
                
                    while (length(cdm_table_cid_fields) > 0) {
                        
                        
                            cdm_table_cid_field <- cdm_table_cid_fields[1]
                            
                            
                            sql <- paste0("SELECT * FROM ", cdm_table, " WHERE ", cdm_table_cid_field, " = ", concept_id, " LIMIT 1")
                            

                            resultset <- DatabaseConnector::dbGetQuery(conn = conn, statement = sql)
                    
                    
                            if (nrow(resultset) > 0) {
                                
                                    output <-
                                        dplyr::bind_rows(output,
                                                         tibble::tibble(Table = cdm_table,
                                                                Field = cdm_table_cid_field))
                                
                            }
                            
                    
                            cdm_table_cid_fields <- cdm_table_cid_fields[-1]
                    
                    
                }
                
                
                #secretary::press_enter()
                
                
                
                cdm_tables2 <- cdm_tables2[-1]
                
                
            }
            output %>%
                dplyr::distinct()
        
    }
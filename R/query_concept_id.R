#' Lookup a concept id in Athena
#' @importFrom seagull write_query_where_in
#' @export

query_concept_id <-
    function(concept_ids) {
        
                if (length(concept_ids) == 1) {
                    
                            sql <- paste0("SELECT * FROM concept WHERE concept_id = '", concept_ids, "';")
                            
                } else {
                    
                            sql <-
                            seagull::write_query_where_in(table_name = "concept",
                                                          column_name = "concept_id",
                                                          where_in_vector = concept_ids)
                            
                }
                
                            resultset <- query_athena(sql)
                            
                            return(resultset)
        
    }
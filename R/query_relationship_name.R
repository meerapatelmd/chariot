#' Query relationship_id in relationship table
#' @expot

query_relationship_name <- 
            function(phrase, type = c("like", "exact")) {
                
                    if (type == "exact") {
                        
                            query_athena(paste0("SELECT * FROM relationship WHERE LOWER(relationship_name) = '", tolower(phrase), "'"))
                        
                    } else if (type == "like") {
                        
                            query_athena(paste0("SELECT * FROM relationship WHERE LOWER(relationship_name) LIKE '%", tolower(phrase), "%'"))
                        
                    }
                
            }
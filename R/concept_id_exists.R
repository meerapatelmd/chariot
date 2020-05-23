#' QA to make sure the concept_id exists in OMOP
#' @export


concept_id_exists <-
    function(concept_id) {
        
                    x <- query_concept_id(concept_id)
                    
                    if (!is.null(x)) {
                        
                            return(TRUE)
                        
                    } else {
                        
                            return(FALSE)
                    }
            
    }
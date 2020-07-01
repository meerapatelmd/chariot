#' Query concept children
#' @import dplyr
#' @export


concept_children <- 
    function(parent_id, 
             generations = 1, 
             override_cache = FALSE) {
        
                if (!(exists("concept_parent_table", envir = globalenv()))) {
                    
                            concept_parent_table <<- query_parent_child_table(override_cache = override_cache)
                    
                }

                baseline_child <-
                    concept_parent_table %>%
                    dplyr::filter(parent_concept_id %in% parent_id)
                
                output <- list()
                output[[1]] <- baseline_child
        
                for (i in 1:generations) {
                    
                    if (i != 1) {
                        
                            prior <- output[[(i-1)]]
                            
                            #secretary::press_enter()
                            output[[i]] <- 
                                        prior %>%
                                        dplyr::inner_join(concept_parent_table,
                                                          by = c("child_concept_id" = "parent_concept_id"),
                                                          suffix = c(".prior", ".new")) %>%
                                        dplyr::select(parent_concept_id = child_concept_id, 
                                                      child_concept_id = child_concept_id.new)
                        
                    }
                    
                }
                
                output %>%
                    dplyr::bind_rows()
                
    }



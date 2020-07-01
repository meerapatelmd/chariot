#' Query concept parents
#' @import dplyr
#' @export

concept_parents <- 
    function(child_id, 
             generations = 1, 
             override_cache = FALSE) {
        
                if (!(exists("concept_parent_table", envir = globalenv()))) {
                    
                            concept_parent_table <<- query_parent_child_table(override_cache = override_cache)
                    
                }

                baseline_parent <-
                    concept_parent_table %>%
                    dplyr::filter(child_concept_id %in% child_id)
                
                output <- list()
                output[[1]] <- baseline_parent
        
                for (i in 1:generations) {
                    
                    if (i != 1) {
                        
                            prior <- output[[(i-1)]]
                            
                            #secretary::press_enter()
                            output[[i]] <- 
                                        concept_parent_table %>%
                                        dplyr::inner_join(prior,
                                                          by = c("child_concept_id" = "parent_concept_id"),
                                                          suffix = c(".prior", ".new")) %>%
                                        dplyr::select(parent_concept_id, 
                                                      child_concept_id)
                        
                    }
                    
                }
                
                output %>%
                    dplyr::bind_rows() %>%
                    chariot::left_join_concept_id(dataframe_column = "parent_concept_id",
                                                  include_synonyms = FALSE) %>%
                    dplyr::select(-parent_concept_id) %>%
                    dplyr::rename_at(vars(concept_id:last_col()),
                                     function(x) paste0("parent_", x))
                
    }



#' Query concept parents
#' @import dplyr
#' @export

concept_parents <- 
    function(child_id, 
             generations = 1, 
             override_cache = FALSE) {

                baseline_parent <-
                    query_athena(paste0("SELECT * FROM concept_parent WHERE child_concept_id = '", child_id, "';"))
                
                output <- list()
                output[[1]] <- baseline_parent
        
                for (i in 1:generations) {
                    
                    if (i != 1) {
                        
                            prior <- output[[(i-1)]]
                            
                            #secretary::press_enter()
                            output[[i]] <- 
                                left_join_for_parents(prior %>%
                                                          dplyr::select(prior_parent_concept_id = parent_concept_id)) %>%
                                dplyr::filter(!is.na(prior_parent_concept_id))
                                        # concept_parent_table %>%
                                        # dplyr::inner_join(prior,
                                        #                   by = c("child_concept_id" = "parent_concept_id"),
                                        #                   suffix = c(".prior", ".new")) %>%
                                        # dplyr::select(parent_concept_id, 
                                        #               child_concept_id)
                        
                    }
                    
                }
              
                output2 <-
                     output %>%
                    purrr::keep(~nrow(.)>0)
                
                if (length(output2) != generations) {
                    
                        warning('Maximum possible generations less than "generations" param: ', length(output2))
                }
                
                output2 %>%
                    dplyr::bind_rows() %>%
                    dplyr::mutate(child_concept_id = coalesce(prior_parent_concept_id, child_concept_id)) %>%
                    dplyr::select(parent_concept_id,
                                  child_concept_id)
                
    }



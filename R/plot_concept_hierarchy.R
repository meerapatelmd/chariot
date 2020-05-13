#' Make the family tree dataframe required for the ggenealogy package
#' @param concept_dataframe The concept_dataframe cannot be subsetted since the original concept fields are required to create the labels for the family tree
#' @return dataframe of 1-degree separation labeled as parent and child
#' @importFrom dplyr filter
#' @importFrom dplyr select
#' @importFrom dplyr distinct
#' @importFrom dplyr left_join
#' @import tidyr
#' @importFrom ggenealogy plotAncDes
#' @export

plot_concept_hierarchy <-
    function(concept_id, 
             generations_before = 0, 
             generations_after = 0) {
        
        if (generations_after > 0) {
            output_descendants <- list()
            
            df_a <-
                query_concept_id(concept_id) %>%
                merge_concepts("Parent Concept", shorthand = TRUE) %>%
                dplyr::select(parent_concept_id = concept_id,
                              `Parent Concept`)
            
            df_b <-
                query_descendants(concept_id, max_levels_of_separation = 1) %>%
                filter_for_valid() %>%
                merge_concepts(into = "Child Concept", shorthand = TRUE) %>%
                dplyr::rename(parent_concept_id = ancestor_concept_id,
                              child_concept_id = descendant_concept_id) %>%
                dplyr::select(parent_concept_id, child_concept_id, `Child Concept`)
            
            output_descendants[[1]] <- 
                    dplyr::left_join(df_a,df_b)
            
            #generations_after <- generations_after-1
            
            if (generations_after > 1) {
                for (i in 1:generations_after) {
                    previous_output <- output_descendants[[i]]
                    next_output_index <- i+1
                    
                    df_a <-
                        previous_output %>%
                        dplyr::select(parent_concept_id = child_concept_id,
                                      `Parent Concept` = `Child Concept`)
                    
                    df_b <-
                        query_descendants(ancestor_concept_ids = df_a$parent_concept_id,
                                          max_levels_of_separation = 1) %>%
                        filter_for_valid() %>%
                        merge_concepts(into = "Child Concept", shorthand = TRUE) %>%
                        dplyr::rename(child_concept_id = descendant_concept_id,
                                      parent_concept_id = ancestor_concept_id) %>%
                        dplyr::select(parent_concept_id,
                                      child_concept_id,
                                      `Child Concept`)
                    
                    output_descendants[[next_output_index]] <- dplyr::left_join(df_a,
                                                                                df_b) %>%
                                                                                dplyr::distinct()
                    
                }
                output_descendants <- dplyr::bind_rows(output_descendants)
                
            }
                
        } else {
            output_descendants <- list()
        }
        
        if (generations_before > 0) {
            output_ancestors <- list()
            df_a <-
                query_concept_id(concept_id) %>%
                merge_concepts("Child Concept", shorthand = TRUE) %>%
                dplyr::select(child_concept_id = concept_id,
                              `Child Concept`)
            df_b <-
                query_ancestors(concept_id, max_levels_of_separation = 1) %>%
                filter_for_valid() %>%
                merge_concepts(into = "Parent Concept", shorthand = TRUE) %>%
                dplyr::rename(parent_concept_id = ancestor_concept_id,
                              child_concept_id = descendant_concept_id) %>%
                dplyr::select(parent_concept_id, child_concept_id, `Parent Concept`)
            
            output_ancestors[[1]] <-
                dplyr::left_join(df_a,
                                 df_b)
            
            if (generations_before > 1) {
                for (i in 1:generations_before) {
                    previous_output <- output_ancestors[[i]]
                    next_output_index <- i+1
                    
                    df_a <-
                        previous_output %>%
                        dplyr::select(child_concept_id = parent_concept_id,
                                      `Child Concept` = `Parent Concept`)
                    
                    df_b <-
                        query_ancestors(df_a$child_concept_id,
                                        max_levels_of_separation = 1) %>%
                        filter_for_valid() %>%
                        merge_concepts(into = "Parent Concept", shorthand = TRUE) %>%
                        dplyr::rename(child_concept_id = descendant_concept_id,
                                      parent_concept_id = ancestor_concept_id) %>%
                        dplyr::select(parent_concept_id,
                                      child_concept_id,
                                      `Parent Concept`)
                    
                    output_ancestors[[next_output_index]] <- dplyr::left_join(df_a,
                                                                              df_b) %>%
                        dplyr::distinct()
                    
                }
                output_ancestors <- dplyr::bind_rows(output_ancestors)
                
            }
            
        } else {
            output_ancestors <- list()
        }
        
        
        output <- 
            dplyr::bind_rows(output_ancestors,
                                   output_descendants) %>%
            dplyr::filter_all(all_vars(!is.na(.)))  %>%
            dplyr::rename(child = `Child Concept`,
                          parent = `Parent Concept`)
        
        concept <-
                    output %>%
                    tidyr::pivot_longer(cols = c("parent",
                                                 "child"),
                                        names_to = "concept_level",
                                        values_to = "concept") %>%
                    dplyr::select(concept) %>%
                    dplyr::distinct() %>%
                    rubix::filter_at_grepl(col = concept,
                                           grepl_phrase = paste0(" ", concept_id, " ")) %>%
                    unlist() %>%
                    unname()
        
        ggenealogy::plotAncDes(concept, output, generations_before, generations_after)
        
    }

        
    
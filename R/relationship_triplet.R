#' Relationship triplet
#' @export


relationship_triplet <- 
    function(.data,
             .col = NULL,
             include_count = TRUE,
             omop = FALSE,
             omop_schema = "omop_vocabulary") {
        
        
        
                    output_a <- 
                        pivot_relationship_id(.data = .data,
                                              .col = .col,
                                              include_count = include_count,
                                              omop = omop,
                                              omop_schema = omop_schema)
                    
                    
                    # Getting concept1 labels
                    output_b <- 
                        left_join_concept(.data = output_a %>%
                                              dplyr::select(concept_id_1),
                                          omop = omop,
                                          omop_schema = omop_schema,
                                          include_synonyms = FALSE) %>%
                        merge_concepts(into = "Concept1") %>%
                        dplyr::select(-concept_id)
                    
                    
                    return(dplyr::left_join(output_a, 
                                            output_b,
                                            by = "concept_id_1") %>%
                               rubix::bring_to_front(concept_id_1, Concept1)
                    )
        
    }
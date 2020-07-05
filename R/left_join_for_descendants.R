#' Left Join Ancestors to a dataframe of concept_ids
#' @importFrom dplyr select
#' @importFrom dplyr rename_at
#' @importFrom dplyr distinct
#' @export

left_join_for_descendants <-
    function(.data,
             ancestor_id_column = NULL,
             level = NULL,
             omop = FALSE,
             omop_schema = "omop_vocabulary",
             override_cache = FALSE) {
        
        # if (is.null(ancestor_id_column)) {
        #     ancestor_id_column <- colnames(.data)[1]
        #     
        # }
        
        if (omop) {
        
        if (is.null(level)) {
                        descendants <-
                                left_join_df_omop(.data = .data,
                                             column = ancestor_id_column,
                                             athena_table = "concept_ancestor",
                                             athena_column = "ancestor_concept_id"
                                             )
                        
                        descendants_detail <-
                                left_join_concept(descendants %>%
                                                            dplyr::select(descendant_concept_id),
                                                        include_synonyms = FALSE,
                                                  omop = omop,
                                                  omop_schema = omop_schema) %>%
                                dplyr::select(-!!ancestor_id_column) %>%
                                rubix::rename_all_with_prefix("descendant_")
        
        } else {
            
                    descendants <-
                                left_join_df_omop(.data = .data,
                                             column = ancestor_id_column,
                                             athena_table = "concept_ancestor",
                                             athena_column = "ancestor_concept_id",
                                             where_athena_col = "max_levels_of_separation",
                                             where_athena_col_equals = level)
                    
                    descendants_detail <-
                                left_join_concept(descendants %>%
                                                            dplyr::select(descendant_concept_id),
                                                        include_synonyms = FALSE,
                                                  omop = omop,
                                                  omop_schema = omop_schema) %>%
                                dplyr::select(-!!ancestor_id_column) %>%
                                rubix::rename_all_with_prefix("descendant_")
                    
            
        }
            
        } else {
            
            if (is.null(level)) {
                descendants <-
                    left_join_df(.data = .data,
                                      column = ancestor_id_column,
                                      athena_table = "concept_ancestor",
                                      athena_column = "ancestor_concept_id"
                    )
                
                descendants_detail <-
                    left_join_concept(descendants %>%
                                          dplyr::select(descendant_concept_id),
                                      include_synonyms = FALSE,
                                      omop = omop,
                                      omop_schema = omop_schema) %>%
                    dplyr::select(-!!ancestor_id_column) %>%
                    rubix::rename_all_with_prefix("descendant_")
                
            } else {
                
                descendants <-
                    left_join_df(.data = .data,
                                      column = ancestor_id_column,
                                      athena_table = "concept_ancestor",
                                      athena_column = "ancestor_concept_id",
                                      where_athena_col = "max_levels_of_separation",
                                      where_athena_col_equals = level)
                
                descendants_detail <-
                    left_join_concept(descendants %>%
                                          dplyr::select(descendant_concept_id),
                                      include_synonyms = FALSE,
                                      omop = omop,
                                      omop_schema = omop_schema) %>%
                    dplyr::select(-!!ancestor_id_column) %>%
                    rubix::rename_all_with_prefix("descendant_")
                
                
            } 
            
            
            
            
            
        }
        final_descendants <-
            dplyr::left_join(descendants,
                             descendants_detail,
                             by = "descendant_concept_id") %>%
            dplyr::select(-ancestor_concept_id)
        
    
        return(final_descendants)
        
    }

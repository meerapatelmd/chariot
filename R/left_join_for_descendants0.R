#' Left Join Ancestors to a dataframe of concept_ids
#' @importFrom dplyr select
#' @importFrom dplyr rename_at
#' @importFrom dplyr distinct
#' @export

left_join_for_descendants0 <-
    function(dataframe,
             ancestor_id_column = NULL,
             level = NULL) {
        
        if (!is.null(ancestor_id_column)) {
                        dataframe <-
                                dataframe %>%
                                dplyr::select(all_of(ancestor_id_column))
        }
        
        if (is.null(level)) {
                        descendants <-
                                left_join_df(dataframe = dataframe,
                                                      athena_table = "concept_ancestor",
                                                      athena_column = "ancestor_concept_id")
                        
                        descendants_detail <-
                                left_join_df_to_concept(dataframe = descendants %>%
                                                            dplyr::select(descendant_concept_id),
                                                        include_synonyms = FALSE) %>%
                                dplyr::select(-descendant_concept_id) %>%
                                rubix::rename_all_with_prefix("descendant_")
        
        } else {
            
                    descendants <-
                                left_join_df(dataframe = dataframe,
                                             athena_table = "concept_ancestor",
                                             athena_column = "ancestor_concept_id",
                                             where_athena_col = "max_levels_of_separation",
                                             where_athena_col_equals = level)
                    
                    descendants_detail <-
                                left_join_df_to_concept(dataframe = descendants %>%
                                                            dplyr::select(descendant_concept_id),
                                                        include_synonyms = FALSE) %>%
                                dplyr::select(-descendant_concept_id) %>%
                                rubix::rename_all_with_prefix("descendant_")
                    
            
        }
        final_descendants <-
            dplyr::left_join(descendants,
                             descendants_detail,
                             by = "descendant_concept_id") %>%
            dplyr::select(-ancestor_concept_id)
        
        return(final_descendants)
        
    }

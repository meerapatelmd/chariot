#' Left Join Ancestors to a dataframe of concept_ids
#' @importFrom dplyr select
#' @importFrom dplyr rename_at
#' @importFrom dplyr distinct
#' @export

left_join_for_ancestors <-
    function(.data,
             .descendant_id_column = NULL,
             level = NULL) {

        if (is.null(level)) {
                ancestors <-
                    left_join_df(.data = .data,
                                 .col = .descendant_id_column,
                                 athena_table = "concept_ancestor",
                                 athena_column = "descendant_concept_id")
                
                ancestors_detail <-
                    left_join_concept(ancestors %>%
                                          dplyr::select(ancestor_concept_id),
                                          include_synonyms = FALSE) %>%
                    dplyr::select(-ancestor_concept_id) %>%
                    rubix::rename_all_with_prefix("ancestor_")
        } else {
            
                ancestors <-
                    left_join_df(.data = .data,
                                 .col = .descendant_id_column,
                                 athena_table = "concept_ancestor",
                                 athena_column = "descendant_concept_id",
                                 where_athena_col = "max_levels_of_separation",
                                 where_athena_col_equals = level)
                
                
                ancestors_detail <-
                    left_join_concept(ancestors %>%
                                                dplyr::select(ancestor_concept_id),
                                            include_synonyms = FALSE) %>%
                    dplyr::select(-ancestor_concept_id) %>%
                    rubix::rename_all_with_prefix("ancestor_")
            
            
        }
        
        final_ancestors <-
            dplyr::left_join(ancestors,
                             ancestors_detail,
                             by = "ancestor_concept_id") %>%
            dplyr::select(-descendant_concept_id)
        
        return(final_ancestors)
        
    }

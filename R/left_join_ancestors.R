#' Left Join Ancestors to a dataframe of concept_ids
#' @importFrom dplyr select
#' @importFrom dplyr rename_at
#' @importFrom dplyr distinct
#' @export

left_join_ancestors <-
    function(dataframe,
             dataframe_column = NULL,
             valid_only = TRUE) {
        
        
        ancestors <-
            left_join_df(dataframe = dataframe,
                         dataframe_column = dataframe_column,
                                  athena_table = "concept_ancestor",
                                  athena_column = "descendant_concept_id")
        
        ancestors_detail <-
            left_join_df_to_concept(dataframe = ancestors,
                                    dataframe_column = dataframe_column)
        
        
        if (valid_only) {
            final_ancestors <-
                ancestors_detail %>%
                filter_for_valid() %>%
                dplyr::select(-ancestor_concept_id) %>%
                dplyr::rename_at(vars(!matches(paste0("^", dataframe_column, "$"))), function(x) paste0("ancestor_", x)) %>%
                dplyr::distinct()
        } else {
            final_ancestors <-
                ancestors_detail %>%
                #filter_for_valid() %>%
                dplyr::select(-ancestor_concept_id) %>%
                dplyr::rename_at(vars(!matches(paste0("^", dataframe_column, "$"))), function(x) paste0("ancestor_", x)) %>%
                dplyr::distinct()
        }
        
        return(final_ancestors)
        
    }

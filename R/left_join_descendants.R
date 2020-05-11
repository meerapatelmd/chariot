#' Left Join Ancestors to a dataframe of concept_ids
#' @importFrom dplyr select
#' @importFrom dplyr rename_at
#' @importFrom dplyr distinct
#' @export

left_join_descendants <-
    function(dataframe,
             dataframe_column = NULL,
             valid_only = TRUE) {
        
        
        descendants <-
            left_join_df(dataframe = dataframe,
                         dataframe_column = dataframe_column,
                                  athena_table = "concept_ancestor",
                                  athena_column = "ancestor_concept_id")
        
        descendants_detail <-
            left_join_df_to_concept(dataframe = descendants,
                                    dataframe_column = dataframe_column)
        
        
        if (valid_only) {
            final_descendants <-
                descendants_detail %>%
                filter_for_valid() %>%
                dplyr::select(-descendant_concept_id) %>%
                dplyr::rename_at(vars(!matches(paste0("^", dataframe_column, "$"))), function(x) paste0("descendant_", x)) %>%
                dplyr::distinct()
        } else {
            final_descendants <-
                descendants_detail %>%
                #filter_for_valid() %>%
                dplyr::select(-descendant_id) %>%
                dplyr::rename_at(vars(!matches(paste0("^", dataframe_column, "$"))), function(x) paste0("descendant_", x)) %>%
                dplyr::distinct()
        }
        
        return(final_descendants)
        
    }

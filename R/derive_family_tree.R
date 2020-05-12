#' Make the family tree dataframe required for the ggenealogy package
#' @param concept_dataframe The concept_dataframe cannot be subsetted since the original concept fields are required to create the labels for the family tree
#' @return dataframe of 1-degree separation labeled as parent and child
#' @importFrom dplyr filter
#' @importFrom dplyr select
#' @importFrom dplyr distinct
#' @importFrom dplyr left_join
#' @export


derive_family_tree <-
    function(concept_dataframe,
             concept_dataframe_column = NULL) {
        
        dataframe <- concept_dataframe
        dataframe_column <- concept_dataframe_column
        
        descendants <-
            left_join_df(dataframe,
                                  dataframe_column = dataframe_column,
                                  athena_table = "concept_ancestor",
                                  athena_column = "ancestor_concept_id")
        
        Final_descendants <-
            descendants %>%
            dplyr::filter(max_levels_of_separation == "1") %>%
            dplyr::select(ancestor_concept_id, descendant_concept_id)
        
        if (nrow(Final_descendants) > 0) {
            descendants_detail <-
                left_join_df_to_concept(dataframe = Final_descendants %>%
                                            dplyr::select(descendant_concept_id))
            
            
            Final_descendants_detail <-
                descendants_detail %>%
                #chariot::filter_for_classification() %>%
                #chariot::filter_for_valid() %>%
                merge_concepts(into = "Descendant Concept", shorthand = TRUE) %>%
                dplyr::select(descendant_concept_id, `Descendant Concept`) %>%
                dplyr::distinct()
            
            #print("Line 42 DONE")
            
            Final_Tree <-
                dplyr::left_join(Final_descendants,
                                 Final_descendants_detail) %>%
                dplyr::left_join(dataframe %>%
                                     chariot::merge_concepts(into = "Ancestor Concept", shorthand = TRUE),
                                 by = c("ancestor_concept_id" = dataframe_column)) %>%
                dplyr::select(parent_concept_id = ancestor_concept_id, 
                              parent = "Ancestor Concept", 
                              child_concept_id = descendant_concept_id, 
                              child = "Descendant Concept") %>%
                dplyr::distinct()
            
            return(Final_Tree)
        } else {
            secretary::typewrite_error("No descendants with a max_levels_of_separation == 1 in the input dataframe.")
        }

        
    }

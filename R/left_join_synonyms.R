#' Add synonyms
#' @description Identical to the left_join_concept with param include_synonyms set to TRUE
#' @param include_concept_table Are both the concept table contents and concept synonym table contents expected in the output?
#' @import dplyr
#' @import rubix
#' @export
#' 
left_join_synonyms <- 
    function(dataframe, 
             dataframe_column = NULL,
             include_concept_table = FALSE) {
        
                    # making a dataframe_column object if it was NULL
                    if (is.null(dataframe_column)) {
                        dataframe_column <- colnames(dataframe)
                    }
        
        
                    output_a <- 
                        left_join_df_to_concept(dataframe = dataframe,
                                                dataframe_column = dataframe_column)
        
                    output_b <- 
                        left_join_df(dataframe,
                                     dataframe_column = dataframe_column,
                                     athena_table = "concept_synonym",
                                     athena_column = "concept_id") %>%
                        dplyr::filter(language_concept_id == "4180186")
                
                    
                    # a:b can be a 1:many relationship so deduping
                    output_b2 <-
                        output_b %>%
                        dplyr::select(!!dataframe_column, concept_synonym_name) %>%
                        dplyr::distinct()
                    
                    # Combine concept and concept_synonym resultsets and filter out values where the synonym and concept_name are the same
                    output_b3 <- 
                        dplyr::left_join(output_a,
                                         output_b2,
                                         by = dataframe_column) %>%
                        dplyr::filter(concept_name != concept_synonym_name) %>%
                        rubix::group_by_unique_aggregate(concept_id,
                                                         agg.col = concept_synonym_name)
                
                    #Creating final output
                    if (include_concept_table) {
                        dplyr::left_join(output_a,
                                         output_b3,
                                         by = "concept_id")
                    } else {
                        
                        dplyr::left_join(output_a %>%
                                             dplyr::select(concept_id),
                                         output_b3,
                                         by = "concept_id")
                        
                    }
    }

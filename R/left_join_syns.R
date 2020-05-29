#' Add synonyms
#' @import dplyr
#' @import rubix
#' @export
#' 
left_join_syns <- 
    function(dataframe, 
             dataframe_column = NULL) {
        
        
                output <- 
                    left_join_df(dataframe,
                                 dataframe_column = dataframe_column,
                                 athena_table = "concept_synonym",
                                 athena_column = "concept_id") %>%
                    dplyr::filter(language_concept_id == "4180186")
                
                output %>%
                rubix::group_by_unique_aggregate(concept_id,
                                                 agg.col = concept_synonym_name)
        
    
    }

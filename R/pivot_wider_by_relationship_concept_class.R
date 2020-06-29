#' Get all Loinc System (Specimen) Types for a Lab
#' @description This function takes a dataframe and mutates an additional column providing the specimen type based on the "Has system" relationship id.
#' @param concept_id_col The column in dataframe that points to the concept_id. If NULL, defaults to "concept_id".
#' @param dataframe input data
#' @importFrom dplyr filter
#' @importFrom dplyr select
#' @importFrom dplyr rename
#' @importFrom dplyr left_join
#' @importFrom dplyr distinct
#' @export

pivot_wider_by_relationship_concept_class <-
    function(dataframe,
             concept_id_col = NULL,
             include_count = TRUE) {
        
        
            output <- left_join_relationship(dataframe = dataframe,
                                             dataframe_column = concept_id_col,
                                             merge_concept_2 = FALSE)
            
            output <- 
                    output %>%
                    rubix::rename_all_remove("_2$") %>%
                    merge_concepts(into = "Concept_2",
                                   concept_class_id)
            
            final_output <-
                output %>%
                tidyr::pivot_wider(id_cols = concept_id_1,
                                   names_from = concept_class_id,
                                   values_from = Concept_2,
                                   values_fn = list(Concept_2 = function(x) paste(unique(x)[1:250] %>% 
                                                                                      centipede::no_na(), 
                                                                                  collapse = "\n"))) %>%
                    dplyr::mutate_all(substring, 1, 25000)
            
            
            if (include_count) {
                final_output_count <-
                    output %>%
                    tidyr::pivot_wider(id_cols = concept_id_1,
                                        names_from = concept_class_id,
                                        values_from = Concept_2,
                                        values_fn = list(Concept_2 = function(x) length(unique(x)))) %>%
                    dplyr::rename_at(vars(!concept_id_1), function(x) paste0(x, " Count"))


                
                
                final_output <- 
                    dplyr::left_join(final_output,
                                     final_output_count,
                                     by = "concept_id_1")
                
                return(final_output)
                        
                
            } else {
                
                    return(final_output)
                
            }
        

        
    }

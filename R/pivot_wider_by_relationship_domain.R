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

pivot_wider_by_relationship_domain <-
    function(dataframe,
             concept_id_col = NULL) {
        
        
            output <- left_join_relationship(dataframe = dataframe,
                                             dataframe_column = concept_id_col,
                                             merge_concept_2 = FALSE)
            
            output <- 
                output %>%
                rubix::rename_all_remove("_2$") %>%
                merge_concepts(into = "Concept_2",
                               domain_id)
            
            output %>%
            tidyr::pivot_wider(id_cols = concept_id_1,
                               names_from = domain_id,
                               values_from = Concept_2,
                               values_fn = list(Concept_2 = function(x) paste(unique(x)[1:250], collapse = "\n"))) %>%
                dplyr::mutate_all(substring, 1, 25000)
        

        
    }

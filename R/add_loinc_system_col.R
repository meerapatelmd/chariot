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

add_loinc_system_col <-
    function(dataframe,
             concept_id_col = NULL) {
        
        if (is.null(concept_id_col)) {
            output <-
                left_join_df(dataframe = dataframe %>%
                                          dplyr::select(concept_id),
                                      athena_table = "CONCEPT_RELATIONSHIP",
                                      athena_column = "concept_id_1") %>%
                dplyr::filter(relationship_id == "Has system") %>%
                dplyr::select(concept_id_1,
                              concept_id_2)
            
            output2 <-
                left_join_df_to_concept(dataframe = output %>%
                                                     dplyr::select(concept_id_2),
                                                 concept_column = "concept_id") %>%
                dplyr::select(concept_id, concept_name) %>%
                dplyr::rename(LOINC_System = concept_name,
                              concept_id_2 = concept_id)
            
            final <- dplyr::left_join(dataframe,
                                      output,
                                      by = c("concept_id" = "concept_id_1"))
            
            return(
            dplyr::left_join(final, 
                             output2,
                             by = c("concept_id_2" = "concept_id_2"))) %>%
                dplyr::rename(LOINC_System_concept_id = concept_id_2) %>%
                dplyr::distinct()
        }
        
    }

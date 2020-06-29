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

pivot_wider_by_relative_level <-
    function(dataframe,
             concept_id_col = NULL,
             levels_type = c("min", "max")) {
        
            if (length(levels_type) != 1) {
                    stop("levels_type must be length 1 from c('min', 'max')")
            }
        
            output <- left_join_all_relatives(dataframe = dataframe,
                                              id_column = concept_id_col)
            
            if (is.null(concept_id_col)) {
                
                    concept_id_col <- colnames(dataframe)[1]
                
            }
            
            if (levels_type == "min") {
                
                    output <- 
                        output %>%
                        dplyr::select(-starts_with("max"))
                
            } else {
                
                    output <- 
                        output %>%
                        dplyr::select(-starts_with("min"))
                
            }
            
            # Split A and D
            output <- split(output,
                            output$relative_type)
            
            #Arrange descending from A then ascending from 0 to D
            output$A <- 
                output$A %>%
                dplyr::arrange_at(vars(starts_with(levels_type)),
                                  function(x) desc(as.integer(x))) %>%
                dplyr::mutate_at(vars(starts_with(levels_type)),
                                 function(x) paste0("A_", x))
            
            
            output$D <- 
                output$D %>%
                dplyr::arrange_at(vars(starts_with(levels_type)),
                                  function(x) as.integer(x)) %>%
                dplyr::mutate_at(vars(starts_with(levels_type)),
                                 function(x) paste0("D_", x))
            
            #Binding output back
            output <- 
                output %>%
                dplyr::bind_rows() %>%
                rubix::rename_all_remove("^relative_") %>%
                chariot::merge_concepts(into = "Relative Concept") %>%
                dplyr::select(-concept_id, -type)
            
            #Adding relative type
            output %>%
                tidyr::pivot_wider(id_cols = !!concept_id_col,
                                   names_from = starts_with(levels_type),
                                   values_from = `Relative Concept`,
                                   values_fn = list(`Relative Concept` = function(x) paste(unique(x)[1:250], collapse = "\n"))) %>%
                dplyr::mutate_all(substring, 1, 25000)
    }

#' Pivot all relatives of a set of concepts
#' @description This function takes a dataframe and mutates an additional column providing the specimen type based on the "Has system" relationship id.
#' @param concept_id_col The column in dataframe that points to the concept_id. If NULL, defaults to "concept_id".
#' @param dataframe input data
#' @param names_from concept table column to be pivoted on
#' @examples 
#' Random immunosuppressant concept ids
#' immunosuppressant_concept_ids <- c("35807335","35807331", "21603616", "21600651", "21605199", "21602723") 
#' @importFrom dplyr filter
#' @importFrom dplyr select
#' @importFrom dplyr rename
#' @importFrom dplyr left_join
#' @importFrom dplyr distinct
#' @export

pivot_relative <-
    function(.data,
             id_col = NULL,
             names_from,
             include_count = TRUE,
             omop = FALSE,
             omop_schema = "omop_vocabulary") {
        
        
        if (missing(names_from)) {
            
                stop('argument "names_from" is missing, with no default')
            
        }
        
            output <- left_join_relatives(.data = .data,
                                              .id_column = id_col,
                                          omop = omop,
                                          omop_schema = omop_schema)
            
            if (is.null(id_col)) {
                
                    id_col <- colnames(.data)[1]
                
            }
            
            #Binding output back
            output <- 
                output %>%
                dplyr::bind_rows() %>%
                rubix::rename_all_remove("^relative_") %>%
                chariot::merge_concepts(into = "Relative Concept", !!names_from) %>%
                dplyr::select(-concept_id, -type, -min_levels_of_separation, -max_levels_of_separation)
            
            final_output <-
            output %>%
                tidyr::pivot_wider(id_cols = !!id_col,
                                   names_from = !!names_from,
                                   values_from = `Relative Concept`,
                                   values_fn = list(`Relative Concept` = function(x) paste(unique(x)[1:250] %>%
                                                                                               centipede::no_na(), collapse = "\n"))) %>%
                dplyr::mutate_all(substring, 1, 25000) %>%
                dplyr::mutate_at(vars(!!id_col),
                                 as.integer)
            
            if (include_count) {
                
                final_output_count <-
                    output %>%
                    tidyr::pivot_wider(id_cols = !!id_col,
                                       names_from = !!names_from,
                                       values_from = `Relative Concept`,
                                       values_fn = list(`Relative Concept` = function(x) length(unique(x)))) %>%
                    dplyr::rename_at(vars(!(!!id_col)),
                                     function(x) paste0(x, " Count"))
                
                final_output <- 
                    dplyr::left_join(final_output,
                                     final_output_count,
                                     by = id_col)
                    
                
                return(final_output)
            } else {
                return(final_output)
            }
    }

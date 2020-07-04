#' Join a dataframe object with Athena's concept table
#' @description This function has better performance than the WHERE IN statement for larger searches. A table in the format of "v{unpunctuated timestamp}" is written to the local Athena. A join is performed with a concept table. The table is subsequently dropped. If a dataframe column is not provided as a the column to join the concept table on, the 1st column will be used by default.
#' @param dataframe dataframe to join
#' @param dataframe_column string of the column name to join on. If NULL, the 1st column is used.
#' @param merge_concept_2 return the merged concept strip for concept_id_2, or as a straightforward concept table dataframe?
#' @importFrom seagull create_table_via_temp_file
#' @importFrom seagull drop_table
#' @export

left_join_relationship <-
    function(.data,
             .column = NULL,
             merge_concept2 = TRUE,
             omop = FALSE,
             omop_schema = "omop_vocabulary") {
        
        if (omop) {
            
            
            output_a <-
                left_join_df_omop(.data = .data,
                             .column = .column,
                             athena_table = "concept_relationship",
                             athena_column = "concept_id_1",
                             omop_schema = omop_schema) %>%
                # select for only the concept_relationship table fields
                dplyr::select(concept_id_1:last_col()) %>%
                # remove all invalid relationships and the dates 
                dplyr::filter(is.na(invalid_reason)) %>%
                dplyr::select(-valid_start_date, -valid_end_date, -invalid_reason) %>%  
                dplyr::distinct() %>%
                # Remove all NA rows
                dplyr::filter_all(all_vars(!is.na(.)))
            
            # Getting concept_id_2 information
            output_b <- 
                left_join_concept(output_a %>%
                                      dplyr::select(concept_id_2),
                                  concept_column = "concept_id",
                                  include_synonyms = FALSE,
                                  omop = omop,
                                  omop_schema = omop_schema) %>%
                dplyr::select(concept_id:last_col()) %>%
                rubix::rename_all_suffix("_2")
            
            
            # Merging concept 2
            if (merge_concept2) {
                
                output_b <- 
                    output_b %>%
                    merge_concepts(into = "Concept2", suffix = "_2") 
                
            } 
            
            # Merging final output
            final <-
                output_a %>%
                dplyr::left_join(output_b, by = "concept_id_2") %>%
                dplyr::distinct() %>%
                dplyr::select(!ends_with("2"), ends_with("2"))
            
            
            
            
            
        } else {
                    
                    output_a <-
                    left_join_df(.data = .data,
                                 .column = .column,
                                 athena_table = "concept_relationship",
                                 athena_column = "concept_id_1") %>%
                                # select for only the concept_relationship table fields
                                dplyr::select(concept_id_1:last_col()) %>%
                                # remove all invalid relationships and the dates 
                                dplyr::filter(is.na(invalid_reason)) %>%
                                dplyr::select(-valid_start_date, -valid_end_date, -invalid_reason) %>%  
                                dplyr::distinct() %>%
                                # Remove all NA rows
                                dplyr::filter_all(all_vars(!is.na(.)))
                
                    # Getting concept_id_2 information
                    output_b <- 
                        left_join_concept(output_a %>%
                                                 dplyr::select(concept_id_2),
                                          concept_column = "concept_id",
                                          include_synonyms = FALSE) %>%
                        dplyr::select(concept_id:last_col()) %>%
                        rubix::rename_all_suffix("_2")
                    
                    
                    # Merging concept 2
                    if (merge_concept2) {
                        
                            output_b <- 
                                output_b %>%
                                merge_concepts(into = "Concept2", suffix = "_2") 
    
                    } 
                
                # Merging final output
                final <-
                output_a %>%
                    dplyr::left_join(output_b, by = "concept_id_2") %>%
                    dplyr::distinct() %>%
                    dplyr::select(!ends_with("2"), ends_with("2"))
                
        }
        
        return(final)

    }
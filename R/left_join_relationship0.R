#' Join a dataframe object with Athena's concept table
#' @description This function has better performance than the WHERE IN statement for larger searches. A table in the format of "v{unpunctuated timestamp}" is written to the local Athena. A join is performed with a concept table. The table is subsequently dropped. If a dataframe column is not provided as a the column to join the concept table on, the 1st column will be used by default.
#' @param dataframe dataframe to join
#' @param dataframe_column string of the column name to join on. If NULL, the 1st column is used.
#' @param merge_concept_2 return the merged concept strip for concept_id_2, or as a straightforward concept table dataframe?
#' @importFrom seagull create_table_via_temp_file
#' @importFrom seagull drop_table
#' @export

left_join_relationship0 <-
    function(dataframe,
             dataframe_column = NULL,
             merge_concept_2 = TRUE) {
        
                table_name <- paste0("v", stampede::stamp_this(without_punct = TRUE))
        
                if (is.null(dataframe_column)) {
                    
                            dataframe_column <- colnames(dataframe)[1]
                }
            
                # Loading cache
                output <-
                load_cached_join(function_name = "left_join_relationship",
                                 left_vector = dataframe,
                                 right_table_name = "concept_relationship",
                                 right_column_name = "concept_id_1")
                
                if (is.null(output)) {
                    
                            seagull::create_table_via_temp_file(dataframe = dataframe,
                                                                table_name = table_name,
                                                                dbname = "athena")
                            
                            output <- query_athena(paste0("SELECT * FROM ", table_name, " LEFT JOIN concept_relationship c ON c.concept_id_1 = ", dataframe_column))
                            
                            
                            seagull::drop_table(table_name = table_name,
                                                dbname = "athena")
                            
                            cache_join(function_name = "left_join_relationship",
                                       left_vector = dataframe,
                                       right_table_name = "concept_relationship",
                                       right_column_name = "concept_id_1",
                                       object = output)
                }
                
                # Removing original dataframe_column because it is now concept_id_1
                output <- 
                    output %>%
                    dplyr::select(-(!!dataframe_column))
                
                # Removing all invalid relationships and the dates
                output <- 
                    output %>%
                    dplyr::filter(is.na(invalid_reason)) %>%
                    dplyr::select(-valid_start_date, -valid_end_date, -invalid_reason) %>%
                    dplyr::distinct() %>%
                    # Remove all NA rows
                    dplyr::filter_all(all_vars(!is.na(.)))
                
                # Getting concept_id_2 information
                output_2 <- 
                    left_join_concept_id(output %>%
                                             dplyr::select(concept_id_2),
                                         include_synonyms = FALSE)
                
                if (merge_concept_2) {
                    
                        output_2 <- 
                            output_2 %>%
                            chariot::merge_concepts(into = "Concept_2") %>%
                            dplyr::select(-concept_id)

                } else {
                    
                        output_2 <- 
                            output_2 %>%
                            dplyr::select(-concept_id_2) %>%
                            rubix::rename_all_suffix("_2")
                    
                    
                }
                
                # Merging final output
                output %>%
                    dplyr::left_join(output_2, by = "concept_id_2") %>%
                    dplyr::distinct()

    }
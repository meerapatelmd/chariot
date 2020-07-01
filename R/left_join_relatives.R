#' Get all Ancestors and Descendants of a set of concept ids
#' @importFrom dplyr select
#' @importFrom rubix rename_all_with_replace
#' @importFrom dplyr mutate
#' @importFrom dplyr distinct
#' @importFrom dplyr filter_at
#' @importFrom dplyr bind_rows
#' @export

left_join_relatives <-
    function(dataframe,
             id_column = NULL,
             ancestor_level = NULL,
             descendant_level = NULL) {
        
            
            ##Ancestors 
            anc <- 
                left_join_for_ancestors(dataframe = dataframe,
                                        descendant_id_column = id_column,
                                           level = ancestor_level)
            
            final_anc <-
                    anc %>%
                    rubix::rename_all_with_replace(pattern = "ancestor_",
                                                   replacement = "relative_") %>%
                    dplyr::mutate(relative_type = "A")
            
            ##Descendants
            des <- 
                left_join_for_descendants(dataframe = dataframe,
                                          ancestor_id_column = id_column,
                                             level = descendant_level)
            final_des <-
                    des %>%
                    rubix::rename_all_with_replace(pattern = "descendant_",
                                                   replacement = "relative_") %>%
                    dplyr::mutate(relative_type = "D")
            
            ##Combining
            final <- dplyr::bind_rows(final_anc,
                                      final_des) %>%
                            dplyr::filter_at(vars(relative_concept_id,
                                                  relative_concept_name,
                                                  relative_domain_id,
                                                  relative_vocabulary_id,
                                                  relative_concept_class_id,
                                                  relative_concept_code,
                                                  relative_valid_start_date,
                                                  relative_valid_end_date),
                                             all_vars(!is.na(.))) %>%
                                dplyr::distinct()
            
            return(final)
            
        
    }


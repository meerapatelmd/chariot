#' Return a Ancestor Matrix
#' @description This function returns a matrix of ancestors by descendants to view if a group of concepts have any common ancestors 
#' @importFrom tidyr pivot_wider
#' @export

return_ancestor_matrix <-
                    function(descendant_concept_ids) {
                        
                                ancestry <- query_ancestors(descendant_concept_ids)
                                
                                ancestry %>% 
                                        tidyr::pivot_wider(id_cols = descendant_concept_id,
                                                           names_from = ancestor_concept_id,
                                                           values_from = ancestor_concept_id)
                                        
                        
                    }
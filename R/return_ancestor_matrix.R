#' Return a Ancestor Matrix
#' @description This function returns a matrix of ancestors by descendants to view if a group of concepts have any common ancestors 
#' @examples 
#' # Random immunosuppressant concept ids
#' immunosuppressant_concept_ids <- c("35807335","35807331", "21603616", "21600651", "21605199", "21602723") 
#' return_ancestor_matrix(immunosuppressant_concept_ids)
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



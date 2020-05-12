#' Query ancestors for a given concept_id
#' @inheritParams write_sql_for_ancestors
#' @export

query_ancestors <-
    function(descendant_concept_ids,
             max_levels_of_separation = NULL,
             plot = FALSE) {
        
            sql <- write_sql_for_ancestors(descendant_concept_ids = descendant_concept_ids,
                                             max_levels_of_separation = max_levels_of_separation)
            
            resultset <- query_athena(sql)
            
            if (plot == FALSE) {
                return(resultset)
            } else {
                child <-
                    query_concept_id(descendant_concept_ids) %>%
                    merge_concepts(into = "child",
                                   shorthand = TRUE) %>%
                    dplyr::select(child) %>%
                    dplyr::distinct() %>%
                    unlist()
                
                output <-
                    resultset %>%
                    merge_concepts(into = "parent", shorthand = TRUE) %>%
                    rubix::mutate_new_cols_if_not_exist(column_name = "child",
                                                        value = child) %>%
                    dplyr::select(parent, child) %>%
                    dplyr::distinct() %>%
                    dplyr::filter(parent != child)
                
                
                ggenealogy::plotAncDes(output$child[1], output)
            }
        
    }

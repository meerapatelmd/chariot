#' Query ancestors for a given concept_id
#' @inheritParams write_sql_for_ancestors
#' @export

query_ancestors <-
    function(descendant_concept_ids,
             max_levels_of_separation) {
        
            sql <- write_sql_for_ancestors(descendant_concept_ids = descendant_concept_ids,
                                             max_levels_of_separation = max_levels_of_separation)
            
            resultset <- query_athena(sql)
            return(resultset)
        
    }

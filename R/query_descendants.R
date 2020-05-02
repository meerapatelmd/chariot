#' Query descendants for a given concept_id
#' @inheritParams write_sql_for_descendants
#' @export

query_descendants <-
    function(ancestor_concept_ids,
             max_levels_of_separation = NULL) {
        
            sql <- write_sql_for_descendants(ancestor_concept_ids = ancestor_concept_ids,
                                             max_levels_of_separation = max_levels_of_separation)
            
            resultset <- query_athena(sql)
            return(resultset)
        
    }
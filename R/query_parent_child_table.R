#' Query Parent-Child Relationships from Concept Ancestor Table
#' @description This function takes all the ancestors and descendants with a level == 1 relationships (either max or min) for plotting.
#' @import dplyr
#' @export

query_parent_child_table <- 
    function(override_cache = FALSE) {
        
        query_athena("SELECT * FROM concept_ancestor WHERE min_levels_of_separation = '1' OR max_levels_of_separation = '1'", override_cache = override_cache) %>%
            dplyr::select(parent_concept_id = ancestor_concept_id,
                          child_concept_id = descendant_concept_id) %>%
            dplyr::distinct()
        
        
    }
#' Write SQL to get OMOP Concept Descendants
#' @description This function returns the SQL statement that can be used to query on either a cloud or local instance of the OMOP vocabulary.
#' @param ancestor_concept_id OMOP concept id of the ancestor concept
#' @param max_levels_of_separation Optional. Integer of the maximum levels of separation. 
#' @export

write_sql_for_descendants <-
    function(ancestor_concept_id,
             max_levels_of_separation = NULL) {
        
            if (!is.null(max_levels_of_separation)) {
                sql_statement <-
                    paste0(
                        "SELECT * 
                        FROM concept_ancestor a
                        LEFT JOIN concept
                        ON concept_id = a.descendant_concept_id 
                        WHERE a.ancestor_concept_id = '", ancestor_concept_id, "' 
                        and max_levels_of_separation = '", max_levels_of_separation, "';")
            } else {
                sql_statement <-
                    paste0(
                        "SELECT * 
                        FROM concept_ancestor a
                        LEFT JOIN concept
                        ON concept_id = a.descendant_concept_id 
                        WHERE a.ancestor_concept_id = '", ancestor_concept_id, "';")
                
            }
                
                return(sql_statement)
    }

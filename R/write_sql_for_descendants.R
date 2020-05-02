#' Write SQL to get OMOP Concept Descendants
#' @description This function returns the SQL statement that can be used to query on either a cloud or local instance of the OMOP vocabulary.
#' @param ancestor_concept_ids A vector of 1 or more OMOP concept id of the ancestor concept
#' @param max_levels_of_separation Optional. Integer of the maximum levels of separation. 
#' @importFrom seagull write_where_in_string
#' @export

write_sql_for_descendants <-
    function(ancestor_concept_ids,
             max_levels_of_separation = NULL) {
        
        if (length(ancestor_concept_ids) == 1) {
            if (!is.null(max_levels_of_separation)) {
                sql_statement <-
                    paste0(
                        "SELECT * 
                        FROM concept_ancestor a
                        LEFT JOIN concept
                        ON concept_id = a.descendant_concept_id 
                        WHERE a.ancestor_concept_id = '", ancestor_concept_ids, "' 
                        and max_levels_of_separation = '", max_levels_of_separation, "';")
            } else {
                sql_statement <-
                    paste0(
                        "SELECT * 
                        FROM concept_ancestor a
                        LEFT JOIN concept
                        ON concept_id = a.descendant_concept_id 
                        WHERE a.ancestor_concept_id = '", ancestor_concept_ids, "';")
                
            }
        } else {
            ancestor_concept_ids <- seagull::write_where_in_string(where_in_vector = ancestor_concept_ids)
            if (!is.null(max_levels_of_separation)) {
                sql_statement <-
                    paste0(
                        "SELECT * 
                        FROM concept_ancestor a
                        LEFT JOIN concept
                        ON concept_id = a.descendant_concept_id 
                        WHERE a.ancestor_concept_id IN ", ancestor_concept_ids, " 
                        and max_levels_of_separation = '", max_levels_of_separation, "';")
                
            } else {
                sql_statement <-
                    paste0(
                        "SELECT * 
                        FROM concept_ancestor a
                        LEFT JOIN concept
                        ON concept_id = a.descendant_concept_id 
                        WHERE a.ancestor_concept_id IN ", ancestor_concept_ids, ";")
            }
        }
        
                
                return(sql_statement)
    }

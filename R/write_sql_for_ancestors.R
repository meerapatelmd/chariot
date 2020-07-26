#' Write SQL to get OMOP Concept Ancestors
#' @description This function returns the SQL statement that can be used to query on either a cloud or local instance of the OMOP vocabulary.
#' @param descendant_concept_ids OMOP concept id of the ancestor concept
#' @param max_levels_of_separation Integer of the maximum levels of separation
#' @export

write_sql_for_ancestors <-
    function(descendant_concept_ids,
             max_levels_of_separation = NULL) {

        .Deprecated(new = "renderQueryAncestors")

        if (length(descendant_concept_ids) == 1) {
            if (!is.null(max_levels_of_separation)) {
                sql_statement <-
                    paste0(
                        "SELECT *
                        FROM concept_ancestor a
                        LEFT JOIN concept
                        ON concept_id = a.ancestor_concept_id
                        WHERE a.descendant_concept_id = '", descendant_concept_ids, "'
                        and max_levels_of_separation = '", max_levels_of_separation, "';")
            } else {
                sql_statement <-
                    paste0(
                        "SELECT *
                        FROM concept_ancestor a
                        LEFT JOIN concept
                        ON concept_id = a.ancestor_concept_id
                        WHERE a.descendant_concept_id = '", descendant_concept_ids, "';")

            }
        } else {
            descendant_concept_ids <- seagull::write_where_in_string(where_in_vector = descendant_concept_ids)
            if (!is.null(max_levels_of_separation)) {
                sql_statement <-
                    paste0(
                        "SELECT *
                        FROM concept_ancestor a
                        LEFT JOIN concept
                        ON concept_id = a.ancestor_concept_id
                        WHERE a.descendant_concept_id IN ", descendant_concept_ids, "
                        and max_levels_of_separation = '", max_levels_of_separation, "';")

            } else {
                sql_statement <-
                    paste0(
                        "SELECT *
                        FROM concept_ancestor a
                        LEFT JOIN concept
                        ON concept_id = a.ancestor_concept_id
                        WHERE a.descendant_concept_id IN ", descendant_concept_ids, ";")
            }
        }


        return(sql_statement)
    }


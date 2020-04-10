#' Search the relationships of a concept id
#' @param concept_id omop concept_id
#' @export

write_sql_to_get_id_relationship <-
        function(concept_id) {
                sql_statement <-
                        paste0("SELECT * FROM concept c LEFT JOIN concept_relationship cr ON c.concept_id = cr.concept_id_1 LEFT JOIN concept c2 ON c2.concept_id = cr.concept_id_2 WHERE c.concept_id = '", concept_id, "' OR c2.concept_id = '", concept_id, "';")
                return(sql_statement)
        }

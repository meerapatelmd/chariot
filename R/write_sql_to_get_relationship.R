#' Search the relationships of a concept id
#' @param concept_id omop concept_id
#' @export

write_sql_to_get_relationships <-
        function(concept_id) {
                sql_statement <-
                        paste0("SELECT c.*,
                                       cr.concept_id_2,
                                       cr.relationship_id,
                                       cr.invalid_reason as cr_invalid_reason
                               FROM concept c 
                               LEFT JOIN concept_relationship cr 
                               ON c.concept_id = cr.concept_id_1
                               WHERE c.concept_id = '", concept_id, "';")
                return(sql_statement)
        }

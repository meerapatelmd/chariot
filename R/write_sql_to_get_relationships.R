#' Search the relationships of a concept id
#' @param concept_ids omop concept_id. For this function, it can also be a vector..
#' @export

write_sql_to_get_relationships <-
    function(concept_ids) {
        if (length(concept_ids) == 1) {
            sql_statement <-
                paste0("SELECT c.*,
                                        cr.relationship_id,
                                        cr.invalid_reason as cr_invalid_reason,
                                        c2.concept_id AS concept_id_2,
                                        c2.concept_name AS concept_name_2,
                                        c2.domain_id AS domain_id_2,
                                        c2.vocabulary_id AS vocabulary_id_2,
                                        c2.concept_class_id AS concept_class_id_2,
                                        c2.standard_concept AS standard_concept_2,
                                        c2.concept_code AS concept_code_2,
                                        c2.valid_start_date AS valid_start_date_2,
                                        c2.valid_end_date AS valid_end_date_2,
                                        c2.invalid_reason AS invalid_reason_2
                               FROM concept c 
                               LEFT JOIN concept_relationship cr 
                               ON c.concept_id = cr.concept_id_1 
                               LEFT JOIN concept c2
                               ON c2.concept_id = cr.concept_id_2
                               WHERE c.concept_id = '", concept_ids, "';")
        } else {
            concept_ids <- paste0("'", concept_ids, "'")
            concept_ids <- seagull::prepare_vector(concept_ids)
            concept_ids <- paste0("(", concept_ids, ")")
            sql_statement <-
                paste0("SELECT c.*,
                                        cr.relationship_id,
                                        cr.invalid_reason as cr_invalid_reason,
                                        c2.concept_id AS concept_id_2,
                                        c2.concept_name AS concept_name_2,
                                        c2.domain_id AS domain_id_2,
                                        c2.vocabulary_id AS vocabulary_id_2,
                                        c2.concept_class_id AS concept_class_id_2,
                                        c2.standard_concept AS standard_concept_2,
                                        c2.concept_code AS concept_code_2,
                                        c2.valid_start_date AS valid_start_date_2,
                                        c2.valid_end_date AS valid_end_date_2,
                                        c2.invalid_reason AS invalid_reason_2
                               FROM concept c 
                               LEFT JOIN concept_relationship cr 
                               ON c.concept_id = cr.concept_id_1 
                               LEFT JOIN concept c2
                               ON c2.concept_id = cr.concept_id_2
                               WHERE c.concept_id IN ", concept_ids, ";")
            
        }
                
                
                return(sql_statement)
    }


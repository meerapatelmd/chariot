#' SQL to get all vocabulary relationships
#' @export


write_sql_all_vocab_relationships <- 
    function(vocabulary_id)  {
        sql_statement <- 
            paste0("
            SELECT c1.*,
            cr1.relationship_id,
            cr1.concept_id_2,
            cr1.invalid_reason as cr_invalid_reason
            FROM concept c1
        LEFT JOIN concept_relationship cr1
        ON c1.concept_id = cr1.concept_id_1
        WHERE c1.vocabulary_id = '", vocabulary_id, "';")
        return(sql_statement)
    }


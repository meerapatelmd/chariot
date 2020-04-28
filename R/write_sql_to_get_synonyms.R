#' Get all synonyms of a concept id
#' @param concept_id omop concept_id
#' @export

write_sql_to_get_synonyms <-
        function(concept_id, language_concept_id = c("4180186")) {
            language_concept_id <- paste0("'", language_concept_id, "'")
            
            language_concept_ids <- seagull::prepare_vector(language_concept_id)
            language_concept_ids <- paste0("(", language_concept_ids, ")")
                sql_statement <-
                        paste0("SELECT *
                               FROM concept_synonym c 
                               WHERE c.concept_id = '", concept_id, "' AND c.language_concept_id IN ", language_concept_ids, ";")
                return(sql_statement)
        }

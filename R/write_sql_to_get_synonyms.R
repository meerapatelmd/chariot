#' Get all synonyms of a concept id
#' @param concept_id omop concept_id
#' @export

write_sql_to_get_synonyms <-
        function(concept_id) {
                sql_statement <-
                        paste0("SELECT *
                               FROM concept_synonym c 
                               WHERE c.concept_id = '", concept_id, "';")
                return(sql_statement)
        }

#' Query the synonyms associated with an id
#' @inheritParams write_sql_to_get_synonyms
#' @importFrom dplyr select
#' @export

query_id_synonyms <-
    function(concept_id) {
        sql <- write_sql_to_get_synonyms(concept_id = concept_id)
        resultset <- query_athena(sql)
        
        return(resultset %>%
                   dplyr::select(concept_synonym_name) %>%
                   unlist())
    }
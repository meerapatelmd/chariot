#' Query the synonyms associated with an id
#' @inheritParams write_sql_to_get_synonyms
#' @importFrom dplyr select
#' @export

querySynonyms <-
    function(concept_id,
             schema = NULL,
             language_concept_id = 4180186,
             ...) {

        sql_statement <- renderSynonyms(concept_id = concept_id,
                              schema = schema,
                              language_concept_id = language_concept_id)

        resultset <- query_athena(sql_statement = sql_statement,
                                  ...)

        return(resultset %>%
                   dplyr::select(concept_synonym_name) %>%
                   unlist())
    }

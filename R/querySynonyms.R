#' Query the synonyms associated with an id
#' @inheritParams write_sql_to_get_synonyms
#' @importFrom dplyr select
#' @export

querySynonyms <-
    function(concept_id,
             schema = NULL,
             language_concept_id = 4180186,
             verbose = FALSE,
             cache_resultset = TRUE,
             override_cache = FALSE,
             conn = NULL,
             render_sql = FALSE,
             sleepTime = 1,
             ...) {

                sql_statement <- renderSynonyms(concept_id = concept_id,
                                      schema = schema,
                                      language_concept_id = language_concept_id)

                queryAthena(sql_statement = sql_statement,
                                         verbose = verbose,
                                         cache_resultset = cache_resultset,
                                         override_cache = override_cache,
                                         conn = conn,
                                         render_sql = render_sql,
                                         sleepTime = sleepTime,
                                         ...) %>%
                               dplyr::select(concept_synonym_name) %>%
                               unlist()
    }

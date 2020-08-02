#' Render Query Concept Synonym Table
#' @import pg13
#' @import stringr
#' @export


renderQueryPhraseExactSynonym <-
        function(schema,
                 caseInsensitive = TRUE,
                 phrase,
                 limit_n = NULL) {

                        c(
                        pg13::buildQuery(schema = schema,
                                         fields = c("concept_id", "STRING_AGG(concept_synonym_name, '|') AS concept_synonym_name"),
                                         tableName = "concept_synonym",
                                         whereInField = "concept_synonym_name",
                                         whereInVector = phrase,
                                         caseInsensitive = caseInsensitive,
                                         n = limit_n,
                                         n_type = "limit") %>%
                                stringr::str_remove(pattern = "[;]{1}$"),
                        "GROUP BY concept_id;") %>%
                                paste(collapse = " ")

        }

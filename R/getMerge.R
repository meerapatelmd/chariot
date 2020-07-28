#' Get Merged Concept Id
#' @importFrom dplyr select
#' @export


getMerge <-
    function(concept_id,
             schema = "public") {
            queryConceptId(concept_ids = concept_id,
                           schema = schema) %>%
            merge_concepts(into = "Concept") %>%
            dplyr::select("Concept") %>%
            unlist() %>%
            unname()
    }

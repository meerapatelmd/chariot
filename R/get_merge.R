#' Get Merged Concept Id
#' @importFrom dplyr select
#' @export


get_merge <-
    function(concept_id) {
        query_concept_id(concept_id) %>%
            merge_concepts(into = "Concept") %>%
            dplyr::select("Concept") %>%
            unlist() %>%
            unname()
    }
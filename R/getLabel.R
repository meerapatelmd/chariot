#' Get Merged Concept Id
#' @importFrom dplyr select
#' @export


getLabel <-
    function(concept_id,
             schema = "public") {
            queryConceptId(concept_ids = concept_id,
                           schema = schema) %>%
            makeLabel(into = "Label",
                      remove = TRUE) %>%
            dplyr::select(Label) %>%
            unlist() %>%
            unname()
    }

#' Look up the relationships between concept groups and domains
#' @export


concept_relationships <-
    function(relationship_id) {

            .Deprecated(new = "queryConceptRelationships")


            output_a <-
                query_athena(paste0("SELECT * FROM concept_relationship WHERE relationship_id = '", relationship_id, "';")) %>%
                filter_for_valid() %>%
                dplyr::select(relationship_id, concept_id_1, concept_id_2) %>%
                dplyr::distinct()


            output_b <-
                left_join_concept(output_a,
                                  column = "concept_id_1",
                                  include_synonyms = FALSE) %>%
                dplyr::select(-concept_id_1) %>%
                dplyr::rename_at(vars(concept_id:invalid_reason),
                                 function(x) paste0(x, "_1"))

            output_c <-
                left_join_concept(output_a,
                                  column = "concept_id_2",
                                  include_synonyms = FALSE,
                                  override_cache = TRUE)  %>%
                dplyr::select(-concept_id_2) %>%
                dplyr::rename_at(vars(concept_id:invalid_reason),
                                 function(x) paste0(x, "_2"))


            output2 <-
                dplyr::left_join(output_b,
                                 output_c) %>%
                dplyr::select(contains("vocabulary_id"),
                              contains("domain_id"),
                              contains("concept_class_id"),
                              contains("standard_concept"),
                              relationship_id) %>%
                dplyr::select(ends_with("1"), relationship_id, ends_with("2")) %>%
                dplyr::distinct()

            return(output2)

    }

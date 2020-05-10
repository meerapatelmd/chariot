#' Get RDF-Style Triples from the Athena vocabulary
#' 
#' 





limit = 10000

distinct_classes_c1 <- query_athena("SELECT DISTINCT vocabulary_id,concept_class_id FROM concept;")

output <- data.frame()
while (nrow(distinct_classes_c1) > 0) {
    distinct_class <- distinct_classes_c1 %>%
                            dplyr::filter(row_number() == 1) %>%
                            unlist()
    
    distinct_vocabulary_id <- distinct_class["vocabulary_id"]
    distinct_concept_class_id <- distinct_class["concept_class_id"]
    
    output <- dplyr::bind_rows(output,
                               query_athena(paste0("SELECT * FROM concept WHERE invalid_reason IS NULL AND vocabulary_id = '", distinct_vocabulary_id, "' AND concept_class_id = '", distinct_concept_class_id, "' LIMIT ", limit, ";")))
    
    
    
    distinct_classes_c1 <-
        distinct_classes_c1 %>%
        dplyr::slice(-1)
}
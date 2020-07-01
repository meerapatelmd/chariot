#' Crosswalk concept_id to UMLS based on Code
#' @importFrom metaorite submit_query
#' @importFrom metaorite query_cui
#' @importFrom seagull write_where_in_string
#' @importFrom stingr str_replace_all
#' @import dplyr
#' @export

crosswalk_to_umls_code <- 
    function(concept_id) {
        
            concept_info <- 
                    query_concept_id(concept_ids = concept_id) %>%
                    dplyr::mutate(umls_vocabulary_id = toupper(vocabulary_id)) %>%
                    dplyr::mutate(umls_vocabulary_id = stringr::str_replace_all(umls_vocabulary_id, "^SNOMED$", "SNOMEDCT_US")) %>%
                    dplyr::mutate(umls_vocabulary_id = stringr::str_replace_all(umls_vocabulary_id, "^LOINC$", "LNC"))
            
            code_string <- seagull::write_where_in_string(concept_info$concept_code)
            sab_string <- seagull::write_where_in_string(concept_info$umls_vocabulary_id)
            
            umls_cui <-
                    metaorite::submit_query(paste0("SELECT * FROM MRCONSO WHERE SAB IN ", sab_string, " AND CODE IN ", code_string)) %>%
                        dplyr::select(CUI) %>%
                        dplyr::distinct() %>%
                        unlist() %>%
                        unname()
            
            metaorite::query_cui(umls_cui) %>%
                dplyr::select(SAB, CODE, STR) %>%
                dplyr::distinct()
        
    }
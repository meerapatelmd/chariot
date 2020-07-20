#' Crosswalk concept_id to UMLS based on Code
#' @import metaorite
#' @importFrom seagull write_where_in_string
#' @importFrom stringr str_replace_all
#' @import dplyr
#' @export

crosswalk_from_umls_search <-
    function(phrase) {

            umls <- metaorite::query_exact_phrase_match(phrase = phrase) %>%
                dplyr::mutate(vocabulary_id = stringr::str_replace_all(SAB, "SNOMEDCT_US", "SNOMEDCT")) %>%
                dplyr::mutate(vocabulary_id = stringr::str_replace_all(vocabulary_id, "^LNC$", "LOINC")) %>%
                dplyr::select(umls_vocabulary_id = vocabulary_id,
                              umls_concept_code = CODE)

            code_string <- seagull::write_where_in_string(umls$umls_concept_code)

            output <-
                query_athena(paste0("SELECT * FROM concept WHERE concept_code IN ", code_string)) %>%
                dplyr::filter(vocabulary_id %in% umls$umls_vocabulary_id)

            return(output)


    }

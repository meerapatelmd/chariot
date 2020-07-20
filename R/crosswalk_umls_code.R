#' Crosswalk concept_id to UMLS based on Code
#' @import metaorite
#' @importFrom stringr str_replace_all
#' @import dplyr
#' @export

crosswalk_to_umls_code <-
    function(concept_id) {

            concept_info <-
                    query_concept_id(concept_ids = concept_id) %>%
                    dplyr::mutate(umls_vocabulary_id = toupper(vocabulary_id)) %>%
                    dplyr::mutate(umls_vocabulary_id = stringr::str_replace_all(umls_vocabulary_id, "^SNOMED$", "SNOMEDCT_US")) %>%
                    dplyr::mutate(umls_vocabulary_id = stringr::str_replace_all(umls_vocabulary_id, "^LOINC$", "LNC"))

            base <- system.file(package = "chariot")
            path <- paste0(base, "/sql/metaoriteCode.sql")
            sql_statement <-
                    SqlRender::render(SqlRender::readSql(sourceFile = path),
                                      sab = paste0("'", concept_info$umls_vocabulary_id, "'"),
                                      code = paste0("'", concept_info$concept_code, "'"))

            umls_cui <-
                    metaorite::submitQuery(sql_statement) %>%
                        dplyr::select(CUI) %>%
                        dplyr::distinct() %>%
                        unlist() %>%
                        unname()

            metaorite::queryCUI(umls_cui) %>%
                dplyr::select(SAB, CODE, STR) %>%
                dplyr::distinct()

    }

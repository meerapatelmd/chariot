#' Unmerge OMOP concept element string
#' @description This function unmerges an OMOP concept string created by the merge_omop_concept_elements function
#' @param dataframe dataframe
#' @param omop_string_col column that contains the output from the merge_omop_concept_elements function
#' @importFrom tidyr separate
#' @importFrom tidyr extract
#' @importFrom dplyr enquo
#' @importFrom dplyr rename
#' @importFrom dplyr mutate_at
#' @importFrom dplyr mutate
#' @importFrom dplyr select
#' @importFrom stringr str_remove_all
#' @export

unmerge_omop_concept_elements <-
    function(dataframe, omop_string_col) {
        omop_string_col <- dplyr::enquo(omop_string_col)
        
        dplyr::bind_cols(dataframe,
        dataframe %>%
            tidyr::extract(col = !!omop_string_col,
                           into = c("invalid_reason",
                                    "standard_concept",
                                    "concept_id",
                                    "concept_name",
                                    "vocabulary_id",
                                    "concept_code", 
                                    "domain_id",
                                    "concept_class_id"),
                           regex = "(\\[.{1}\\]) (\\[.{1}\\]) ([^ ]*) (.*?) (\\[.*?) (.*?\\]) (\\[.*?\\]) (\\[.*?\\])" # (\\[[[:alnum:]]) ([[:alnum:]]\\]) (\\[[[:alnum:]]\\]) (\\[[[:alnum:]]\\])"
            ) %>%
            dplyr::mutate_all(stringr::str_remove_all, "^\\[|\\]$") %>%
            dplyr::mutate_at(vars(standard_concept, invalid_reason), stringr::str_replace_all, "^N$|^V$", "") %>%
            dplyr::select(concept_id,
                          concept_name,
                          domain_id,
                          vocabulary_id,
                          concept_class_id,
                          standard_concept,
                          concept_code,
                          invalid_reason,
                          dplyr::everything())
        )
        
        
    }

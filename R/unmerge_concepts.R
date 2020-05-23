#' Unmerge OMOP concept element string
#' @description This function unmerges an OMOP concept string created by the merge_omop_concept_elements function
#' @param dataframe dataframe
#' @param concept_col column that contains the output from the merge_omop_concept_elements function
#' @param remove remove argument passed to the tidyr extract function. If TRUE, removes concept_col in output.
#' @param r_trimws Due to some of the carriage returns in aggregate transformations and other edits in Excel, r_trimws is an argument that if TRUE, trimws the right whitespace of the freshly unmerged columns for any trailing carriage returns.
#' @importFrom tidyr extract
#' @importFrom dplyr enquo
#' @importFrom dplyr mutate_at
#' @importFrom dplyr mutate_all
#' @importFrom dplyr select
#' @importFrom dplyr everything
#' @importFrom stringr str_remove_all
#' @export

unmerge_concepts <-
    function(dataframe, concept_col, remove = TRUE, r_trimws = TRUE) {
                    concept_col <- dplyr::enquo(concept_col)
                    
                    new_cols <- c("invalid_reason",
                                  "standard_concept",
                                  "concept_id",
                                  "concept_name",
                                  "vocabulary_id",
                                  "concept_code", 
                                  "domain_id",
                                  "concept_class_id")
        
                    output <- 
                    dataframe %>%
                        tidyr::extract(col = !!concept_col,
                                       remove = remove,
                                       into = new_cols,
                                       regex = "(\\[.{1}\\]) (\\[.{1}\\]) ([^ ]*) (.*?) (\\[.*?) (.*?\\]) (\\[.*?\\]) (\\[.*?\\])")
                    
                    output <-
                        output %>% 
                                dplyr::mutate_at(vars(all_of(new_cols)), stringr::str_remove_all, "^\\[|\\]$") %>%
                                dplyr::mutate_at(vars(standard_concept, invalid_reason), stringr::str_replace_all, "^N$|^V$", "NA") %>%
                                dplyr::select(concept_id,
                                              concept_name,
                                              domain_id,
                                              vocabulary_id,
                                              concept_class_id,
                                              standard_concept,
                                              concept_code,
                                              invalid_reason,
                                              dplyr::everything()) 

                    
                    if (r_trimws == TRUE) {
                        
                            output <-
                                output %>%
                                dplyr::mutate_at(vars(concept_id,
                                                      concept_name,
                                                      domain_id,
                                                      vocabulary_id,
                                                      concept_class_id,
                                                      standard_concept,
                                                      concept_code,
                                                      invalid_reason),
                                                 trimws,
                                                 which = "right",
                                                 whitespace = "[ \t\r\n]")
                                                 
                    }
                    
                    return(output)
        
    }

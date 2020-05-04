#' Get the concept stem of a LOINC concept name
#' @description This function allows to isolate the "concept" stem in all concept_names formatted in LOINC's system (ie "{Lab Name [units] Etcetera} where Lab Name is the stem".) This allows to search based on a grepl string match for any concepts that have been missed. 
#' @param concept_dataframe a dataframe pulled from the concept table. Requires the concept_name column at minimum.
#' @importFrom rubix filter_at_grepl
#' @importFrom tidyr extract
#' @export


derive_loinc_concept_stem <-
    function(concept_dataframe) {
                        
                    dataframe %>%
                        rubix::filter_at_grepl(col = concept_name,
                                               grepl_phrase = "\\[.*\\]") %>%
                        tidyr::extract(concept_name,
                                       into = c("LOINC_CONCEPT_STEM", "NON_LOINC_CONCEPT_STEM"),
                                       regex = c("(^.*) (\\[.*$)")) %>%
                        dplyr::select(-NON_LOINC_CONCEPT_STEM)
                            
    }

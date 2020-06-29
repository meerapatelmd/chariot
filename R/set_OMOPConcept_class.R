#' Set OMOP Concept S4 Class
#' @export OMOPConcept
#' @exportClass OMOPConcept


OMOPConcept <- setClass("OMOPConcept",
                        representation(concept_id = "character",
                                       concept_name = "character",
                                       concept_name_synonym = "character",
                                       vocabulary_id = "character",
                                       concept_class_id = "character",
                                       domain_id = "character",
                                       invalid_reason = "character",
                                       standard_concept = "character",
                                       concept_code = "character",
                                       umls_concept_name_synonym = "character",
                                       umls_synonym = "tbl_df",
                                       redcap_concept = "tbl_df"),
                        prototype(concept_id = NA_character_,
                                  concept_name = NA_character_,
                                  concept_name_synonym = NA_character_,
                                  vocabulary_id = NA_character_,
                                  concept_class_id = NA_character_,
                                  domain_id = NA_character_,
                                  invalid_reason = NA_character_,
                                  standard_concept = NA_character_,
                                  concept_code = NA_character_))



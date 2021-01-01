#' @title
#' Concept Class (S4)
#'
#' @description
#' This class allows to know the full OMOP concept representation in R Studio.
#'
#' @export concept

concept <- setClass(
  "concept",
  representation(
    concept_id = "numeric",
    concept_name = "character",
    concept_synonym_names = "character",
    domain_id = "character",
    vocabulary_id = "character",
    concept_class_id = "character",
    standard_concept = "character",
    concept_code = "character",
    valid_start_date = "Date",
    valid_end_date = "Date",
    invalid_reason = "character"
  ),
  prototype(
    concept_id = NA_integer_,
    concept_name = NA_character_,
    domain_id = NA_character_,
    vocabulary_id = NA_character_,
    concept_class_id = NA_character_,
    standard_concept = NA_character_,
    concept_code = NA_character_,
    valid_start_date = NA_real_,
    valid_end_date = NA_real_,
    invalid_reason = NA_character_
  )
)

#' @export

is.concept <-
  function(object) {
    class(object) == "concept"
  }

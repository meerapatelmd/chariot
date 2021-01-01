#' @title Filter Class Concepts
#' @export

filterClassConcepts <-
  function(.data,
           has_prefix = NULL,
           has_suffix = NULL,
           invert = FALSE) {
    filterStandardConceptType(
      .data = .data,
      has_prefix = has_prefix,
      has_suffix = has_suffix,
      values = "C",
      invert = invert
    )
  }




#' @title Primitive filtering function
#' @param values character vector of length 1 or greater.
#' @seealso
#'  \code{\link[dplyr]{filter_all}}
#' @rdname filterConcept
#' @export
#' @importFrom dplyr filter_at

filterConcept <-
  function(data,
           has_prefix = NULL,
           has_suffix = NULL,
           concept_col,
           values,
           invert = FALSE) {
    columns <- paste0(
      has_prefix,
      c(
        "concept_id",
        "concept_name",
        "domain_id",
        "vocabulary_id",
        "concept_class_id",
        "standard_concept",
        "concept_code",
        "valid_start_date",
        "valid_end_date",
        "invalid_reason"
      ),
      has_suffix
    ) %>%
      as.list()

    names(columns) <- c(
      "concept_id",
      "concept_name",
      "domain_id",
      "vocabulary_id",
      "concept_class_id",
      "standard_concept",
      "concept_code",
      "valid_start_date",
      "valid_end_date",
      "invalid_reason"
    )


    if (invert) {
      .data %>%
        dplyr::filter_at(
          vars(all_of(unlist(columns)[concept_col])),
          all_vars(!(. %in% values))
        )
    } else {
      .data %>%
        dplyr::filter_at(
          vars(all_of(unlist(columns)[concept_col])),
          all_vars(. %in% values)
        )
    }
  }





#' @title Filter Concept Class
#' @param values character vector of length 1 or greater.
#' @seealso
#'  \code{\link[dplyr]{filter_all}}
#' @rdname filterConceptClass
#' @export
#' @importFrom dplyr filter_at

filterConceptClass <-
  function(.data,
           has_prefix = NULL,
           has_suffix = NULL,
           values,
           invert = FALSE) {
    columns <- paste0(
      has_prefix,
      "concept_class_id",
      has_suffix
    ) %>%
      as.list()

    names(columns) <- "concept_class_id"


    if (invert) {
      .data %>%
        dplyr::filter_at(
          vars(all_of(columns$concept_class_id)),
          all_vars(!(. %in% values))
        )
    } else {
      .data %>%
        dplyr::filter_at(
          vars(all_of(columns$concept_class_id)),
          all_vars(. %in% values)
        )
    }
  }





#' @title Filter Domain
#' @export

filterDomain <-
  function(.data,
           has_prefix = NULL,
           has_suffix = NULL,
           values,
           invert = FALSE) {
    filterConcept(
      .data = .data,
      has_prefix = has_prefix,
      has_suffix = has_suffix,
      concept_col = "domain_id",
      values = values,
      invert = invert
    )
  }




#' @title Filter HemOnc
#' @export

filterHemOnc <-
  function(.data,
           has_prefix = NULL,
           has_suffix = NULL,
           invert = FALSE) {
    filterVocabulary(
      .data = .data,
      has_prefix = has_prefix,
      has_suffix = has_suffix,
      values = "HemOnc",
      invert = invert
    )
  }




#' @title Filter for LOINC
#' @export

filterLOINC <-
  function(.data,
           has_prefix = NULL,
           has_suffix = NULL,
           invert = FALSE) {
    filterVocabulary(
      .data = .data,
      has_prefix = has_prefix,
      has_suffix = has_suffix,
      values = "LOINC",
      invert = invert
    )
  }




#' @title Filter NAACCR
#' @export

filterNAACCR <-
  function(.data,
           has_prefix = NULL,
           has_suffix = NULL,
           invert = FALSE) {
    filterVocabulary(
      .data = .data,
      has_prefix = has_prefix,
      has_suffix = has_suffix,
      values = "NAACCR",
      invert = invert
    )
  }




#' @title Filter Nebraska
#' @export

filterNebraska <-
  function(.data,
           has_prefix = NULL,
           has_suffix = NULL,
           invert = FALSE) {
    filterVocabulary(
      .data = .data,
      has_prefix = has_prefix,
      has_suffix = has_suffix,
      values = "Nebraska Lexicon",
      invert = invert
    )
  }




#' @title Filter for RxNorm Concepts
#' @export

filterRxNorm <-
  function(.data,
           has_prefix = NULL,
           has_suffix = NULL,
           includeExt = TRUE,
           invert = FALSE) {
    if (includeExt) {
      filterVocabulary(
        .data = .data,
        has_prefix = has_prefix,
        has_suffix = has_suffix,
        values = c("RxNorm", "RxNorm Extension"),
        invert = invert
      )
    }

    filterVocabulary(
      .data = .data,
      has_prefix = has_prefix,
      has_suffix = has_suffix,
      values = c("RxNorm"),
      invert = invert
    )
  }




#' @title Filter SNOMED
#' @export

filterSNOMED <-
  function(.data,
           has_prefix = NULL,
           has_suffix = NULL,
           invert = FALSE) {
    filterVocabulary(
      .data = .data,
      has_prefix = has_prefix,
      has_suffix = has_suffix,
      values = "SNOMED",
      invert = invert
    )
  }




#' @title Filter Standard Concepts
#' @export

filterStandardConcepts <-
  function(.data,
           has_prefix = NULL,
           has_suffix = NULL,
           invert = FALSE) {
    filterStandardConceptType(
      .data = .data,
      has_prefix = has_prefix,
      has_suffix = has_suffix,
      values = "S",
      invert = invert
    )
  }




#' @title Filter by Standard Concept Type
#' @export

filterStandardConceptType <-
  function(.data,
           has_prefix = NULL,
           has_suffix = NULL,
           values,
           invert = FALSE) {
    filterConcept(
      .data = .data,
      has_prefix = has_prefix,
      has_suffix = has_suffix,
      concept_col = "standard_concept",
      values = values,
      invert = invert
    )
  }




#' @title Filter for Valid Concepts
#' @description
#' Filter a data frame queried from the Concept Table for only the valid concepts based on the `invalid_reason` column with the option to remove the `valid_start_date` and `valid_end_date` columns.
#'
#' @return
#' A data frame filtered for `invalid_reason == NA` with or without the `valid_start_date` and `valid_end_date` removed.
#'
#' @seealso
#'  \code{\link[dplyr]{filter}},\code{\link[dplyr]{select}}
#'
#' @rdname filterValid
#' @export
#' @importFrom magrittr %>%
#' @importFrom dplyr filter_at select

filterValid <-
  function(.data,
           has_prefix = NULL,
           has_suffix = NULL,
           rm_date_fields = TRUE) {
    .output <-
      filterConcept(
        .data = .data,
        has_prefix = has_prefix,
        has_suffix = has_suffix,
        concept_col = "invalid_reason",
        values = NA_character_
      )


    if (rm_date_fields) {
      columns <- paste0(
        has_prefix,
        c(
          "valid_start_date",
          "valid_end_date"
        ),
        has_suffix
      )

      .output %>%
        dplyr::select(-any_of(columns))
    } else {
      .output
    }
  }




#' Filter Vocabulary
#' @importFrom dplyr filter_at
#' @export

filterVocabulary <-
  function(.data,
           has_prefix = NULL,
           has_suffix = NULL,
           values,
           invert = FALSE) {
    filterConcept(
      .data = .data,
      has_prefix = has_prefix,
      has_suffix = has_suffix,
      concept_col = "vocabulary_id",
      values = values,
      invert = invert
    )
  }

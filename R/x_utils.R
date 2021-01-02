#' Pipe operator
#'
#' See \code{magrittr::\link[magrittr:pipe]{\%>\%}} for details.
#'
#' @name %>%
#' @rdname pipe
#' @keywords internal
#' @export
#' @importFrom magrittr %>%
#' @usage lhs \%>\% rhs
NULL



#' @noRd

normalize_nas <-
  function(data, blanks = TRUE) {
    data[data %in% c("NA", "")] <- NA_character_
    data
  }

#' @noRd

make_temp_table_name <-
  function() {
    paste0("V", stringr::str_remove_all(as.character(Sys.time()), "[[:punct:]]|\\s"))
  }



#' @noRd

generate_concept_filters <-
  function(vocabSchema,
           vocabulary_id,
           domain_id,
           concept_class_id,
           standard_concept,
           invalid_reason) {
    where_clauses <- vector()
    where_clauses_fields <- vector()
    if (!missing(vocabulary_id)) {
      where_clauses_fields <-
        c(
          where_clauses_fields,
          "vocabulary_id"
        )

      vocabulary_id <- paste0("'", vocabulary_id, "'")
      where_clauses <-
        c(
          where_clauses,
          SqlRender::render("@omop_vocabulary_schema.concept.vocabulary_id IN (@vocabulary_id)\n", vocabulary_id = vocabulary_id)
        )
    }

    if (!missing(domain_id)) {
      where_clauses_fields <-
        c(
          where_clauses_fields,
          "domain_id"
        )

      domain_id <- paste0("'", domain_id, "'")
      where_clauses <-
        c(
          where_clauses,
          SqlRender::render("@omop_vocabulary_schema.concept.domain_id IN (@domain_id)\n", domain_id = domain_id)
        )
    }

    if (!missing(concept_class_id)) {
      where_clauses_fields <-
        c(
          where_clauses_fields,
          "concept_class_id"
        )

      concept_class_id <- paste0("'", concept_class_id, "'")
      where_clauses <-
        c(
          where_clauses,
          SqlRender::render("@omop_vocabulary_schema.concept.concept_class_id IN (@concept_class_id)\n", concept_class_id = concept_class_id)
        )
    }

    if (!missing(standard_concept)) {
      where_clauses_fields <-
        c(
          where_clauses_fields,
          "standard_concept"
        )

      if (any("NULL" %in% standard_concept)) {
        part_a <- "@omop_vocabulary_schema.concept.standard_concept IS NULL"
      } else {
        part_a <- vector()
      }

      standard_concept <- standard_concept[!(standard_concept %in% "NULL")]

      if (length(standard_concept)) {
        standard_concept <- paste0("'", standard_concept, "'")

        part_b <- SqlRender::render("@omop_vocabulary_schema.concept.standard_concept IN (@standard_concept)", standard_concept = standard_concept)
      } else {
        part_b <- vector()
      }

      clause_with_null <- paste0("(", c(part_a, part_b) %>% paste(collapse = " OR "), ")")

      where_clauses <-
        c(
          where_clauses,
          clause_with_null
        )
    }


    if (!missing(invalid_reason)) {
      where_clauses_fields <-
        c(
          where_clauses_fields,
          "invalid_reason"
        )

      if (any("NULL" %in% invalid_reason)) {
        part_a <- "@omop_vocabulary_schema.concept.invalid_reason IS NULL"
      } else {
        part_a <- vector()
      }

      invalid_reason <- invalid_reason[!(invalid_reason %in% "NULL")]

      if (length(invalid_reason)) {
        invalid_reason <- paste0("'", invalid_reason, "'")

        part_b <- SqlRender::render("@omop_vocabulary_schema.concept.invalid_reason IN (@invalid_reason)", invalid_reason = invalid_reason)
      } else {
        part_b <- vector()
      }

      clause_with_null <- paste0("(", c(part_a, part_b) %>% paste(collapse = " OR "), ")")


      where_clauses <-
        c(
          where_clauses,
          clause_with_null
        )
    }

    if (length(where_clauses)) {
      where_clauses <- paste(where_clauses, collapse = " AND ")

      SqlRender::render(where_clauses,
        omop_vocabulary_schema = vocabSchema
      )
    }
  }


#' @noRd

n_comma <-
  function(vector) {
    nchar(gsub("[^,]", "", as.character(vector)))
  }

#' @noRd

sQuo <-
  function(vector) {
    sprintf("'%s", vector)
  }

#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param vocabSchema PARAM_DESCRIPTION
#' @param vocabulary_id PARAM_DESCRIPTION
#' @param domain_id PARAM_DESCRIPTION
#' @param concept_class_id PARAM_DESCRIPTION
#' @param standard_concept PARAM_DESCRIPTION
#' @param invalid_reason PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @seealso
#'  \code{\link[SqlRender]{render}}
#' @rdname make_where_clause
#' @export
#' @importFrom SqlRender render


make_where_clause <-
        function(vocabulary_id,
                 domain_id,
                 concept_class_id,
                 standard_concept,
                 invalid_reason) {
                where_clauses <- vector()
                where_clauses_fields <- vector()
                if (!missing(vocabulary_id)) {

                        where_clauses_fields <-
                                c(where_clauses_fields,
                                  "vocabulary_id")

                        vocabulary_id <- paste0("'", vocabulary_id, "'")
                        where_clauses <-
                                c(where_clauses,
                                  SqlRender::render("@vocabSchema.concept.vocabulary_id IN (@vocabulary_id)\n", vocabulary_id = vocabulary_id))
                }

                if (!missing(domain_id)) {

                        where_clauses_fields <-
                                c(where_clauses_fields,
                                  "domain_id")

                        domain_id <- paste0("'", domain_id, "'")
                        where_clauses <-
                                c(where_clauses,
                                  SqlRender::render("@vocabSchema.concept.domain_id IN (@domain_id)\n", domain_id = domain_id))
                }

                if (!missing(concept_class_id)) {

                        where_clauses_fields <-
                                c(where_clauses_fields,
                                  "concept_class_id")

                        concept_class_id <- paste0("'", concept_class_id, "'")
                        where_clauses <-
                                c(where_clauses,
                                  SqlRender::render("@vocabSchema.concept.concept_class_id IN (@concept_class_id)\n", concept_class_id = concept_class_id))

                }

                if (!missing(standard_concept)) {

                        where_clauses_fields <-
                                c(where_clauses_fields,
                                  "standard_concept")

                        if (any("NULL" %in% standard_concept)) {

                                part_a <- "@vocabSchema.concept.standard_concept IS NULL"

                        } else {
                                part_a <- vector()
                        }

                        standard_concept <- standard_concept[!(standard_concept %in% "NULL")]

                        if (length(standard_concept)) {

                                standard_concept <- paste0("'", standard_concept, "'")

                                part_b <- SqlRender::render("@vocabSchema.concept.standard_concept IN (@standard_concept)", standard_concept = standard_concept)

                        } else {

                                part_b <- vector()
                        }

                        clause_with_null <- paste0("(", c(part_a, part_b) %>% paste(collapse = " OR "), ")")

                        where_clauses <-
                                c(where_clauses,
                                  clause_with_null)

                }


                if (!missing(invalid_reason)) {

                        where_clauses_fields <-
                                c(where_clauses_fields,
                                  "invalid_reason")

                        if (any("NULL" %in% invalid_reason)) {

                                part_a <- "@vocabSchema.concept.invalid_reason IS NULL"

                        } else {
                                part_a <- vector()
                        }

                        invalid_reason <- invalid_reason[!(invalid_reason %in% "NULL")]

                        if (length(invalid_reason)) {

                                invalid_reason <- paste0("'", invalid_reason, "'")

                                part_b <- SqlRender::render("@vocabSchema.concept.invalid_reason IN (@invalid_reason)", invalid_reason = invalid_reason)

                        } else {

                                part_b <- vector()
                        }

                        clause_with_null <- paste0("(", c(part_a, part_b) %>% paste(collapse = " OR "), ")")


                        where_clauses <-
                                c(where_clauses,
                                  clause_with_null)

                }

                if (length(where_clauses)) {

                        where_clauses <- paste(where_clauses, collapse = " AND ")

                        where_clauses

                }

        }

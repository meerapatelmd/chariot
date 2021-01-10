#' @title
#' Lookup Relationships of a Set of Concepts
#' @description
#' Returns the forward relationship with concept attributes
#' of both sides of the relationship.
#' @param ... Concept ids.
#' @param check_validity Check to ensure thet the concept ids provided exist
#' @seealso
#'  \code{\link[cli]{cli_rule}}
#'  \code{\link[rlang]{list2}}
#'  \code{\link[SqlRender]{render}}
#' @rdname lookup_relationships
#' @export
#' @importFrom cli cli_rule
#' @importFrom rlang list2
#' @importFrom SqlRender render


lookup_relationships <-
  function(...,
           check_validity = TRUE,
           conn,
           conn_fun = "connectAthena()",
           vocab_schema = "omop_vocabulary",
           cache_only = FALSE,
           skip_cache = FALSE,
           override_cache = FALSE,
           render_sql = FALSE,
           verbose = FALSE,
           sleepTime = 1) {

    if (missing(conn)) {

      conn <- eval(rlang::parse_expr(conn_fun))
      on.exit(expr = dcAthena(conn = conn),
              add = TRUE,
              after = TRUE)
    }


    if (check_validity) {
      if (verbose) {
        cli::cli_rule(left = "Checking Validity")
      }

      concept_ids <- unlist(rlang::list2(...))

      sql_statement <-
        SqlRender::render(
          "
                            SELECT *
                            FROM @vocab_schema.concept c
                            WHERE c.concept_id IN (@concept_ids)
                                    AND c.invalid_reason IS NULL
                            ",
          vocab_schema = vocab_schema,
          concept_ids =  concept_ids
        )

      output <- queryAthena(
        sql_statement = sql_statement,
        conn = conn,
        cache_only = cache_only,
        skip_cache = skip_cache,
        override_cache = override_cache,
        render_sql = render_sql,
        verbose = verbose,
        sleepTime = sleepTime
      )

      if (nrow(output) != length(concept_ids)) {
        invalid_ids <- concept_ids[!(concept_ids %in% output$concept_id)]
        stop("Invalid concept ids: %s", paste(invalid_ids, collapse = ", "))
      }
    }

    sql_statement <-
      SqlRender::render(
        "
                                SELECT
                                        cr.relationship_id,
                                        c1.concept_id AS concept_id_1,
                                        c1.concept_name AS concept_name_1,
                                        c1.domain_id AS domain_id_1,
                                        c1.vocabulary_id AS vocabulary_id_1,
                                        c1.concept_class_id AS concept_class_id_1,
                                        c1.standard_concept AS standard_concept_1,
                                        c1.concept_code AS concept_code_1,
                                        c1.valid_start_date AS valid_start_date_1,
                                        c1.valid_end_date AS valid_end_date_1,
                                        c1.invalid_reason AS invalid_reason_1,
                                        c2.concept_id AS concept_id_2,
                                        c2.concept_name AS concept_name_2,
                                        c2.domain_id AS domain_id_2,
                                        c2.vocabulary_id AS vocabulary_id_2,
                                        c2.concept_class_id AS concept_class_id_2,
                                        c2.standard_concept AS standard_concept_2,
                                        c2.concept_code AS concept_code_2,
                                        c2.valid_start_date AS valid_start_date_2,
                                        c2.valid_end_date AS valid_end_date_2,
                                        c2.invalid_reason AS invalid_reason_2
                                FROM @vocab_schema.concept_relationship cr
                                LEFT JOIN @vocab_schema.concept c1
                                ON cr.concept_id_1 = c1.concept_id
                                LEFT JOIN @vocab_schema.concept c2
                                ON cr.concept_id_2 = c2.concept_id
                                WHERE cr.concept_id_1 IN (@concept_ids)
                                        AND cr.invalid_reason IS NULL
                                ",
        vocab_schema = vocab_schema,
        concept_ids = concept_ids
      )


    queryAthena(
      sql_statement = sql_statement,
      conn = conn,
      cache_only = cache_only,
      skip_cache = skip_cache,
      override_cache = override_cache,
      render_sql = render_sql,
      verbose = verbose,
      sleepTime = sleepTime
    )
  }

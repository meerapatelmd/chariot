#' @title
#' Lookup Vocabulary's Relationships
#'
#' @details
#' Lookup all of a Vocabulary's non-null relationships in the Concept Relationship Table.
#'
#' @inherit vocabulary_level_functions description
#'
#' @inheritParams queryAthena
#' @param vocabulary_id         Vector of 1 or more `vocabulary_id` from the OMOP Vocabulary
#'
#' @seealso
#'  \code{\link[SqlRender]{render}}
#'
#' @rdname lookup_vocab_relationships
#' @export
#' @importFrom SqlRender render
#' @example inst/example/vocabulary_relationships.R

lookup_vocab_relationships <-
  function(vocabulary_id,
           conn,
           conn_fun = "connectAthena()",
           vocab_schema = "omop_vocabulary",
           cache_only = FALSE,
           skip_cache = FALSE,
           override_cache = FALSE,
           render_sql = FALSE,
           render_only = FALSE,
           verbose = FALSE,
           sleepTime = 1) {

    if (missing(conn)) {

      conn <- eval(rlang::parse_expr(conn_fun))
      on.exit(expr = dcAthena(conn = conn),
              add = TRUE,
              after = TRUE)
    }

    queryAthena(
      sql_statement =
        SqlRender::render(
        "
        SELECT DISTINCT
        c.vocabulary_id AS vocabulary_id_1,
        c.concept_class_id AS concept_class_id_1,
        cr.relationship_id,
        c2.vocabulary_id AS vocabulary_id_2,
        c2.concept_class_id AS concept_class_id_2
        FROM @vocab_schema.concept c
        LEFT JOIN @vocab_schema.concept_relationship cr
        ON cr.concept_id_1 = c.concept_id
        LEFT JOIN @vocab_schema.concept c2
        ON c2.concept_id = cr.concept_id_2
        WHERE c.vocabulary_id IN ('@vocabulary_id')
                AND c.invalid_reason IS NULL
                AND cr.invalid_reason IS NULL",
        vocabulary_id = vocabulary_id,
        vocab_schema = vocab_schema
        ),
      conn = conn,
      cache_only = cache_only,
      skip_cache = skip_cache,
      override_cache = override_cache,
      render_sql = render_sql,
      verbose = verbose,
      render_only = render_only,
      sleepTime = sleepTime
    )
  }

#' @title
#' Query a Source Vocabulary's Relationships to Other Vocabularies
#'
#' @details
#' Query for the Source Vocabulary's valid relationships in the Concept Relationship Table and all of the OMOP Source Vocabulary and Concept Classes that the relationships are to. For a resultset for relationships within the vocabulary, see \code{\link{lookup_intravocabulary_relationship}}.
#'
#' @inherit vocabulary_level_functions description
#'
#' @inheritParams lookup_vocabulary_relationships
#'
#' @seealso
#'  \code{\link[SqlRender]{render}}
#'
#' @rdname lookup_relationships_bw_vocab
#'
#' @export
#' @importFrom SqlRender render
#' @example inst/example/vocabulary_relationships.R
lookup_relationships_with_other_vocab <-
  function(vocabulary_id,
           conn,
           conn_fun = "connectAthena()",
           vocab_schema = "omop_vocabulary",
           cache_only = FALSE,
           skip_cache = FALSE,
           override_cache = FALSE,
           cache_resultset = TRUE,
           render_sql = FALSE,
           verbose = FALSE,
           sleepTime = 1) {

    if (missing(conn)) {

      conn <- eval(rlang::parse_expr(conn_fun))
      on.exit(expr = dcAthena(conn = conn),
              add = TRUE,
              after = TRUE)
    }
    queryAthena(
      sql_statement =
        SqlRender::render(
          "
          SELECT DISTINCT
        c.vocabulary_id AS vocabulary_id_1,
        c.concept_class_id AS concept_class_id_1,
        cr.relationship_id,
        c2.vocabulary_id AS vocabulary_id_2,
        c2.concept_class_id AS concept_class_id_2
        FROM @vocab_schema.concept c
        LEFT JOIN @vocab_schema.concept_relationship cr
        ON cr.concept_id_1 = c.concept_id
        LEFT JOIN @vocab_schema.concept c2
        ON c2.concept_id = cr.concept_id_2
        WHERE c.vocabulary_id IN ('@vocabulary_id')
                AND c2.vocabulary_id NOT IN ('@vocabulary_id')
                AND c.invalid_reason IS NULL
                AND c2.invalid_reason IS NULL
                AND cr.invalid_reason IS NULL",
          vocab_schema = vocab_schema,
          vocabulary_id = vocabulary_id
        ),
      conn = conn,
      cache_only = cache_only,
      skip_cache = skip_cache,
      override_cache = override_cache,
      cache_resultset = cache_resultset,
      render_sql = render_sql,
      verbose = verbose,
      sleepTime = sleepTime
    )
  }





#' @title
#' Query a Source Vocabulary's Relationships
#'
#' @details
#' Query for the Source Vocabulary's non-null relationships in the Concept Relationship Table and all of the OMOP Source Vocabulary and Concept Classes that the relationships are to. For a resultset that filters for relationships to other vocabularies, see \code{\link{query_intervocabulary_relationship}}.
#'
#' @inherit vocabulary_level_functions description
#'
#' @inheritParams lookup_vocabulary_relationships
#'
#' @seealso
#'  \code{\link[SqlRender]{render}}
#'
#' @rdname vocab_lookup_intrarelation
#'
#' @export
#' @importFrom SqlRender render
#' @example inst/example/vocabulary_relationships.R
vocab_lookup_intrarelation <-
  function(vocabulary_id,
           conn,
           conn_fun = "connectAthena()",
           vocab_schema = "omop_vocabulary",
           cache_only = FALSE,
           skip_cache = FALSE,
           override_cache = FALSE,
           cache_resultset = TRUE,
           render_sql = FALSE,
           verbose = FALSE,
           sleepTime = 1) {

    if (missing(conn)) {

      conn <- eval(rlang::parse_expr(conn_fun))
      on.exit(expr = dcAthena(conn = conn),
              add = TRUE,
              after = TRUE)
    }
    queryAthena(
      sql_statement =
        SqlRender::render(
          "
          SELECT DISTINCT
        c.vocabulary_id AS vocabulary_id_1,
        c.concept_class_id AS concept_class_id_1,
        cr.relationship_id,
        c2.vocabulary_id AS vocabulary_id_2,
        c2.concept_class_id AS concept_class_id_2
        FROM @vocab_schema.concept c
        LEFT JOIN @vocab_schema.concept_relationship cr
        ON cr.concept_id_1 = c.concept_id
        LEFT JOIN @vocab_schema.concept c2
        ON c2.concept_id = cr.concept_id_2
        WHERE c.vocabulary_id IN ('@vocabulary_id')
                AND c2.vocabulary_id IN ('@vocabulary_id')
                AND c.invalid_reason IS NULL
                AND c2.invalid_reason IS NULL
                AND cr.invalid_reason IS NULL",
          vocab_schema = vocab_schema,
          vocabulary_id = vocabulary_id
        ),
      conn = conn,
      cache_only = cache_only,
      skip_cache = skip_cache,
      override_cache = override_cache,
      cache_resultset = cache_resultset,
      render_sql = render_sql,
      verbose = verbose,
      sleepTime = sleepTime
    )
  }

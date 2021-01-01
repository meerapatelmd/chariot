#' @title
#' Lookup the Ancestors at the Upper Limit of the Max Level of Separation
#' @description
#' Find ancestors of a concept set that are at the maximum of the max_level_of_separation.
#' @param vocabSchema PARAM_DESCRIPTION
#' @param vocabulary_id PARAM_DESCRIPTION
#' @param concept_class_id PARAM_DESCRIPTION
#' @param domain_id PARAM_DESCRIPTION
#' @param conn PARAM_DESCRIPTION, Default: NULL
#' @param skip_cache PARAM_DESCRIPTION, Default: FALSE
#' @param cache_only PARAM_DESCRIPTION, Default: FALSE
#' @param override_cache PARAM_DESCRIPTION, Default: FALSE
#' @param verbose PARAM_DESCRIPTION, Default: FALSE
#' @param render_sql PARAM_DESCRIPTION, Default: FALSE
#' @param sleepTime PARAM_DESCRIPTION, Default: 1
#' @return
#' Concepts from the Concept Table that are upper limit ancestors of the input with an additional field 'upper_limit_max_separation' that provides detail on the degree of separation.
#' @details DETAILS
#' @examples
#' \dontrun{
#' if (interactive()) {
#'   # EXAMPLE1
#' }
#' }
#' @seealso
#'  \code{\link[SqlRender]{render}}
#' @rdname lookup_upper_limit_ancestors
#' @export
#' @importFrom SqlRender render


lookup_upper_limit_ancestors <-
  function(vocabSchema,
           vocabulary_id,
           concept_class_id,
           domain_id,
           conn = NULL,
           skip_cache = FALSE,
           cache_only = FALSE,
           override_cache = FALSE,
           verbose = FALSE,
           render_sql = FALSE,
           sleepTime = 1) {
    concept_filters <-
      generate_concept_filters(
        vocabSchema = vocabSchema,
        vocabulary_id = vocabulary_id,
        concept_class_id = concept_class_id,
        domain_id = domain_id
      )


    if (is.null(concept_filters)) {
      stop("`vocabulary_id`, `concept_class_id`, and/or `domain_id` required")
    } else {
      sql_statement <-
        SqlRender::render(
          "WITH all_ancestors AS (
                                        SELECT DISTINCT ca.ancestor_concept_id AS all_ancestor_concept_id, ca.max_levels_of_separation, ca.min_levels_of_separation
                                        FROM @vocabSchema.concept
                                        INNER JOIN @vocabSchema.concept_ancestor ca
                                        ON ca.descendant_concept_id = @vocabSchema.concept.concept_id
                                        INNER JOIN @vocabSchema.concept c2
                                        ON ca.ancestor_concept_id = c2.concept_id
                                        WHERE @vocabSchema.concept.invalid_reason IS NULL
                                                AND @concept_filters
                                                AND c2.invalid_reason IS NULL
                                        )

                                        SELECT
                                                a.max_levels_of_separation AS upper_limit_max_separation,
                                                c.*
                                        FROM all_ancestors a
                                        LEFT JOIN @vocabSchema.concept c
                                        ON c.concept_id = a.all_ancestor_concept_id
                                        WHERE a.max_levels_of_separation = (
                                                        SELECT max(max_levels_of_separation) AS upper_limit_separation_level
                                                        FROM all_ancestors a2
                                        )
                                                AND c.invalid_reason IS NULL
                                        ",
          vocabSchema = vocabSchema,
          concept_filters = concept_filters
        )
    }


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


#' @title
#' Lookup Orphan Concepts for a Domain, Vocabulary, and/or Concept Class
#' @description
#' Returns input concepts filtered by Domain, Vocabulary, and/or Concept Class that do not have valid ancestors.
#' @param vocabSchema PARAM_DESCRIPTION
#' @param vocabulary_id PARAM_DESCRIPTION
#' @param concept_class_id PARAM_DESCRIPTION
#' @param domain_id PARAM_DESCRIPTION
#' @param conn PARAM_DESCRIPTION, Default: NULL
#' @param skip_cache PARAM_DESCRIPTION, Default: FALSE
#' @param cache_only PARAM_DESCRIPTION, Default: FALSE
#' @param override_cache PARAM_DESCRIPTION, Default: FALSE
#' @param verbose PARAM_DESCRIPTION, Default: FALSE
#' @param render_sql PARAM_DESCRIPTION, Default: FALSE
#' @param sleepTime PARAM_DESCRIPTION, Default: 1
#' @return
#' Concepts from the Concept Table that fit the provided arguments and that do not have an ancestor in the Concept Ancestor Table.
#' @details DETAILS
#' @examples
#' \dontrun{
#' if (interactive()) {
#'   # EXAMPLE1
#' }
#' }
#' @seealso
#'  \code{\link[SqlRender]{render}}
#' @rdname lookup_orphan_concepts
#' @export
#' @importFrom SqlRender render

lookup_orphan_concepts <-
  function(vocabSchema,
           vocabulary_id,
           concept_class_id,
           domain_id,
           conn = NULL,
           skip_cache = FALSE,
           cache_only = FALSE,
           override_cache = FALSE,
           verbose = FALSE,
           render_sql = FALSE,
           sleepTime = 1) {
    concept_filters <-
      generate_concept_filters(
        vocabSchema = vocabSchema,
        vocabulary_id = vocabulary_id,
        concept_class_id = concept_class_id,
        domain_id = domain_id
      )


    if (is.null(concept_filters)) {
      stop("`vocabulary_id`, `concept_class_id`, and/or `domain_id` required")
    } else {
      sql_statement <-
        SqlRender::render(
          "
                                        WITH concept_ancestry AS (
                                        SELECT DISTINCT ca.*
                                        FROM @vocabSchema.concept
                                        INNER JOIN @vocabSchema.concept_ancestor ca
                                        ON ca.descendant_concept_id = @vocabSchema.concept.concept_id
                                        INNER JOIN @vocabSchema.concept c2
                                        ON ca.ancestor_concept_id = c2.concept_id
                                        WHERE @vocabSchema.concept.invalid_reason IS NULL
                                                AND ca.descendant_concept_id <> ca.ancestor_concept_id
                                                AND @concept_filters
                                                AND c2.invalid_reason IS NULL
                                        )

                                        SELECT
                                                @vocabSchema.concept.*
                                        FROM @vocabSchema.concept
                                        LEFT JOIN concept_ancestry ca
                                        ON @vocabSchema.concept.concept_id = ca.descendant_concept_id
                                        WHERE @vocabSchema.concept.invalid_reason IS NULL
                                                AND @concept_filters
                                                AND ca.ancestor_concept_id IS NULL
                                        ",
          vocabSchema = vocabSchema,
          concept_filters = concept_filters
        )
    }


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


#' @title
#' Lookup Topmost Ancestors for a Domain, Vocabulary, and/or Concept Class
#' @description
#' Trace the topmost lineage of a set of concepts  filtered by Domain, Vocabulary, and/or Concept Class by first querying all the ancestors of the concept set and then filtering those ancestors for those that do not ave ancestors themselves.
#' @param vocabSchema PARAM_DESCRIPTION
#' @param vocabulary_id PARAM_DESCRIPTION
#' @param concept_class_id PARAM_DESCRIPTION
#' @param domain_id PARAM_DESCRIPTION
#' @param conn PARAM_DESCRIPTION, Default: NULL
#' @param skip_cache PARAM_DESCRIPTION, Default: FALSE
#' @param cache_only PARAM_DESCRIPTION, Default: FALSE
#' @param override_cache PARAM_DESCRIPTION, Default: FALSE
#' @param verbose PARAM_DESCRIPTION, Default: FALSE
#' @param render_sql PARAM_DESCRIPTION, Default: FALSE
#' @param sleepTime PARAM_DESCRIPTION, Default: 1
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if (interactive()) {
#'   # EXAMPLE1
#' }
#' }
#' @seealso
#'  \code{\link[SqlRender]{render}}
#' @rdname lookup_top_ancestors
#' @export
#' @importFrom SqlRender render

lookup_top_ancestors <-
  function(vocabSchema,
           vocabulary_id,
           concept_class_id,
           domain_id,
           conn = NULL,
           skip_cache = FALSE,
           cache_only = FALSE,
           override_cache = FALSE,
           verbose = FALSE,
           render_sql = FALSE,
           sleepTime = 1) {
    concept_filters <-
      generate_concept_filters(
        vocabSchema = vocabSchema,
        vocabulary_id = vocabulary_id,
        concept_class_id = concept_class_id,
        domain_id = domain_id
      )


    if (is.null(concept_filters)) {
      stop("`vocabulary_id`, `concept_class_id`, and/or `domain_id` required")
    } else {
      sql_statement <-
        SqlRender::render(
          "WITH all_ancestors AS (
                                        SELECT DISTINCT ca.ancestor_concept_id AS all_ancestor_concept_id
                                        FROM @vocabSchema.concept
                                        INNER JOIN @vocabSchema.concept_ancestor ca
                                        ON ca.descendant_concept_id = @vocabSchema.concept.concept_id
                                        INNER JOIN @vocabSchema.concept c2
                                        ON c2.concept_id = ca.ancestor_concept_id
                                        WHERE @vocabSchema.concept.invalid_reason IS NULL
                                                AND @concept_filters
                                                AND c2.invalid_reason IS NULL
                                        ),
                                        ancestors_as_descendants AS (
                                                SELECT ca2.*
                                                FROM all_ancestors a
                                                INNER JOIN @vocabSchema.concept_ancestor ca2
                                                ON ca2.descendant_concept_id = a.all_ancestor_concept_id
                                                WHERE ca2.descendant_concept_id <> ca2.ancestor_concept_id
                                        )

                                        SELECT
                                                b.ancestor_concept_id AS top_ancestor_parent_d,
                                                a.all_ancestor_concept_id AS top_ancestor_concept_id
                                                ,c.concept_name AS top_ancestor_concept_name
                                                ,c.domain_id AS top_ancestor_domain_id
                                                ,c.vocabulary_id AS top_ancestor_vocabulary_id
                                                ,c.concept_class_id AS top_ancestor_concept_class_id
                                                ,c.standard_concept AS top_ancestor_standard_concept
                                                ,c.concept_code AS top_ancestor_concept_code
                                                ,c.valid_start_date AS top_ancestor_valid_start_date
                                                ,c.valid_end_date AS top_ancestor_valid_end_date
                                                ,c.invalid_reason AS top_ancestor_invalid_reason
                                        FROM all_ancestors a
                                        LEFT JOIN @vocabSchema.concept c
                                        ON c.concept_id = a.all_ancestor_concept_id
                                        LEFT JOIN ancestors_as_descendants b
                                        ON b.descendant_concept_id = a.all_ancestor_concept_id
                                        WHERE c.invalid_reason IS NULL
                                                AND b.ancestor_concept_id IS NULL
                                        ",
          vocabSchema = vocabSchema,
          concept_filters = concept_filters
        )
    }


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

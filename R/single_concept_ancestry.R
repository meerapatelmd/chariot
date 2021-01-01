#' @title
#' Lookup the Ancestors of a Concept Id at the Upper Limit of the Max Level of Separation
#' @description
#' Find ancestors of a concept idthat are at the maximum of the max_level_of_separation. To lookup an entire concept set, see \code{\link{lookup_upper_limit_ancestors}}.
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
#' @rdname lookup_cid_upper_limit_ancestors
#' @export
#' @importFrom SqlRender render


lookup_cid_upper_limit_ancestors <-
  function(vocabSchema,
           concept_ids,
           conn = NULL,
           skip_cache = FALSE,
           cache_only = FALSE,
           override_cache = FALSE,
           verbose = FALSE,
           render_sql = FALSE,
           sleepTime = 1) {
    sql_statement <-
      SqlRender::render(
        "WITH all_ancestors AS (
                                        SELECT DISTINCT ca.ancestor_concept_id AS all_ancestor_concept_id, ca.max_levels_of_separation, ca.min_levels_of_separation
                                        FROM @vocabSchema.concept
                                        INNER JOIN @vocabSchema.concept_ancestor ca
                                        ON ca.descendant_concept_id = @vocabSchema.concept.concept_id
                                        WHERE @vocabSchema.concept.concept_id IN (@concept_ids)
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


#' @title
#' Lookup the Parents of a Concept Id
#' @description
#' While \code{\link{lookup_orphan_concepts}} looks up the orphan concepts for an entire concept set, a similar operation can be performed on any given vector of concept_ids. In \code{\link{lookup_orphan_concepts}}, the returned value is a dataframe from the Concept Table of all the concepts that are orphaned in the concept set. Here, the concept_id is looked up regardless with a field in position 1 that is NA if the concept_id is an orphan or a concept_id if the concept_id does have a parent in the Concept Ancestor Table.
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
#' @rdname lookup_cid_parent
#' @export
#' @importFrom SqlRender render

lookup_cid_parent <-
  function(concept_ids,
           vocabSchema,
           conn = NULL,
           skip_cache = FALSE,
           cache_only = FALSE,
           override_cache = FALSE,
           verbose = FALSE,
           render_sql = FALSE,
           sleepTime = 1) {
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
                                        WHERE @vocabSchema.concept.concept_id IN (@concept_ids)
                                                AND ca.descendant_concept_id <> ca.ancestor_concept_id
                                                AND c2.invalid_reason IS NULL
                                        )

                                        SELECT
                                                ca.ancestor_concept_id AS parent_concept_id,
                                                @vocabSchema.concept.*
                                        FROM @vocabSchema.concept
                                        LEFT JOIN concept_ancestry ca
                                        ON @vocabSchema.concept.concept_id = ca.descendant_concept_id
                                        WHERE @vocabSchema.concept.concept_id IN (@concept_ids)
                                        ",
        vocabSchema = vocabSchema,
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


#' @title
#' Lookup Topmost Ancestors of a Concept Id
#' @description
#' Trace the topmost lineage of one or more concept ids. To operate on an entire concept set, see \code{\link{lookup_top_ancestors}}.
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
#' @rdname lookup_cid_top_ancestors
#' @export
#' @importFrom SqlRender render

lookup_cid_top_ancestors <-
  function(vocabSchema,
           concept_ids,
           conn = NULL,
           skip_cache = FALSE,
           cache_only = FALSE,
           override_cache = FALSE,
           verbose = FALSE,
           render_sql = FALSE,
           sleepTime = 1) {
    sql_statement <-
      SqlRender::render(
        "WITH all_ancestors AS (
                                        SELECT DISTINCT ca.ancestor_concept_id AS all_ancestor_concept_id, @vocabSchema.concept.concept_id
                                        FROM @vocabSchema.concept
                                        INNER JOIN @vocabSchema.concept_ancestor ca
                                        ON ca.descendant_concept_id = @vocabSchema.concept.concept_id
                                        WHERE @vocabSchema.concept.concept_id IN (@concept_ids)
                                        ),
                                        ancestors_as_descendants AS (
                                                SELECT DISTINCT ca2.*
                                                FROM all_ancestors a
                                                INNER JOIN @vocabSchema.concept_ancestor ca2
                                                ON ca2.descendant_concept_id = a.all_ancestor_concept_id
                                                WHERE ca2.descendant_concept_id <> ca2.ancestor_concept_id
                                        )

                                        SELECT
                                                a.concept_id,
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

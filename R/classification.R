#' @title
#' Get the Top Classes
#'
#' @description
#' Get the top classes in the Concept Ancestor table by vocabulary with the option of filtering further for domain.
#'
#' @export

lookup_top_classes <-
        function(vocabulary_id,
                 domain_id,
                 vocab_schema = "omop_vocabulary",
                 conn,
                 conn_fun = "connectAthena()",
                 cache_only = FALSE,
                 skip_cache = FALSE,
                 override_cache = FALSE,
                 cache_resultset = TRUE,
                 render_sql = TRUE,
                 verbose = TRUE,
                 sleepTime = 1) {


                if (missing(domain_id)) {

                        queryAthena(sql_statement =
                                            SqlRender::render(
                                                    "
                                WITH ancestry AS (
                                    SELECT DISTINCT ca.ancestor_concept_id, ca.descendant_concept_id
                                    FROM @vocab_schema.concept c
                                    INNER JOIN @vocab_schema.concept_ancestor ca
                                    ON ca.ancestor_concept_id = c.concept_id
                                    INNER JOIN @vocab_schema.concept c2
                                    ON ca.descendant_concept_id = c2.concept_id
                                    WHERE
                                    c.vocabulary_id IN ('@vocabulary_id')
                                    AND c.standard_concept = 'C'
                                    AND c.invalid_reason IS NULL
                                    AND c2.invalid_reason IS NULL
                                    AND c2.standard_concept = 'C'
                                    AND c2.vocabulary_id IN ('@vocabulary_id')
                                    AND ca.ancestor_concept_id <> ca.descendant_concept_id
                                    AND ca.min_levels_of_separation = 1 OR ca.max_levels_of_separation = 1
                                )

                            SELECT DISTINCT c.*
                                FROM ancestry a
                            LEFT JOIN @vocab_schema.concept c
                            ON c.concept_id = a.ancestor_concept_id
                            WHERE a.ancestor_concept_id NOT IN (
                                SELECT a2.descendant_concept_id
                                FROM ancestry a2);",
                                                    vocab_schema = vocab_schema,
                                                    vocabulary_id = vocabulary_id),
                                    conn = conn,
                                    conn_fun = conn_fun,
                                    cache_only = cache_only,
                                    skip_cache = skip_cache,
                                    override_cache = override_cache,
                                    cache_resultset = cache_resultset,
                                    render_sql = render_sql,
                                    verbose = verbose,
                                    sleepTime = sleepTime
                        )



                } else {

                        queryAthena(sql_statement =
                                            SqlRender::render(
                                                    "
                                WITH ancestry AS (
                                    SELECT DISTINCT ca.ancestor_concept_id, ca.descendant_concept_id
                                    FROM @vocab_schema.concept c
                                    INNER JOIN @vocab_schema.concept_ancestor ca
                                    ON ca.ancestor_concept_id = c.concept_id
                                    INNER JOIN @vocab_schema.concept c2
                                    ON ca.descendant_concept_id = c2.concept_id
                                    WHERE
                                    c.vocabulary_id IN ('@vocabulary_id')
                                    AND c.standard_concept = 'C'
                                    AND c.invalid_reason IS NULL
                                    AND c2.invalid_reason IS NULL
                                    AND c2.standard_concept = 'C'
                                    AND c2.vocabulary_id IN ('@vocabulary_id')
                                    AND c.domain_id = '@domain_id'
                                    AND c2.domain_id = '@domain_id'
                                    AND ca.ancestor_concept_id <> ca.descendant_concept_id
                                    AND ca.min_levels_of_separation = 1 AND ca.max_levels_of_separation = 1
                                )

                            SELECT DISTINCT c.*
                                FROM ancestry a
                            LEFT JOIN @vocab_schema.concept c
                            ON c.concept_id = a.ancestor_concept_id
                            WHERE a.ancestor_concept_id NOT IN (
                                SELECT a2.descendant_concept_id
                                FROM ancestry a2);",
                                                    vocab_schema = vocab_schema,
                                                    vocabulary_id = vocabulary_id,
                                                    domain_id = domain_id),
                                    conn = conn,
                                    conn_fun = conn_fun,
                                    cache_only = cache_only,
                                    skip_cache = skip_cache,
                                    override_cache = override_cache,
                                    cache_resultset = cache_resultset,
                                    render_sql = render_sql,
                                    verbose = verbose,
                                    sleepTime = sleepTime
                        )
                }
        }


#' @export


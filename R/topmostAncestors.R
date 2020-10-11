





queryTopmostAncestors <-
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
                generate_concept_filters(vocabSchema = vocabSchema,
                                         vocabulary_id = vocabulary_id,
                                         concept_class_id = concept_class_id,
                                         domain_id = domain_id)


                if (is.null(concept_filters)) {

                        stop('`vocabulary_id`, `concept_class_id`, and/or `domain_id` required')


                } else {


                        sql_statement <-
                                SqlRender::render(
                                        "WITH all_ancestors AS (
                                        SELECT DISTINCT ca.ancestor_concept_id AS all_ancestor_concept_id, ca.max_levels_of_separation, ca.min_levels_of_separation
                                        FROM @vocabSchema.concept
                                        INNER JOIN @vocabSchema.concept_ancestor ca
                                        ON ca.descendant_concept_id = @vocabSchema.concept.concept_id
                                        WHERE @vocabSchema.concept.invalid_reason IS NULL
                                                AND @concept_filters
                                        )

                                        SELECT
                                                a.max_levels_of_separation AS separation_level_upper_limit,
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
                                        concept_filters = concept_filters)


                }


                queryAthena(sql_statement = sql_statement,
                            conn = conn,
                            cache_only = cache_only,
                            skip_cache = skip_cache,
                            override_cache = override_cache,
                            render_sql = render_sql,
                            verbose = verbose,
                            sleepTime = sleepTime)
        }




queryOrphanDescendants <-
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
                        generate_concept_filters(vocabSchema = vocabSchema,
                                                 vocabulary_id = vocabulary_id,
                                                 concept_class_id = concept_class_id,
                                                 domain_id = domain_id)


                if (is.null(concept_filters)) {

                        stop('`vocabulary_id`, `concept_class_id`, and/or `domain_id` required')


                } else {


                        sql_statement <-
                                SqlRender::render(
                                        "WITH all_ancestors AS (
                                        SELECT DISTINCT ca.ancestor_concept_id AS all_ancestor_concept_id
                                        FROM @vocabSchema.concept
                                        INNER JOIN @vocabSchema.concept_ancestor ca
                                        ON ca.descendant_concept_id = @vocabSchema.concept.concept_id
                                        WHERE @vocabSchema.concept.invalid_reason IS NULL
                                                AND @concept_filters
                                        ),
                                        ancestors_as_descendants AS (
                                                SELECT ca2.*
                                                FROM all_ancestors a
                                                INNER JOIN @vocabSchema.concept_ancestor ca2
                                                ON ca2.descendant_concept_id = a.all_ancestor_concept_id
                                                WHERE ca2.descendant_concept_id <> ca2.ancestor_concept_id
                                        )

                                        SELECT
                                                b.ancestor_concept_id,
                                                a.all_ancestor_concept_id AS orphan_concept_id
                                                ,c.concept_name AS orphan_concept_name
                                                ,c.domain_id AS orphan_domain_id
                                                ,c.vocabulary_id AS orphan_vocabulary_id
                                                ,c.concept_class_id AS orphan_concept_class_id
                                                ,c.standard_concept AS orphan_standard_concept
                                                ,c.concept_code AS orphan_concept_code
                                                ,c.valid_start_date AS orphan_valid_start_date
                                                ,c.valid_end_date AS orphan_valid_end_date
                                                ,c.invalid_reason AS orphan_invalid_reason
                                        FROM all_ancestors a
                                        LEFT JOIN @vocabSchema.concept c
                                        ON c.concept_id = a.all_ancestor_concept_id
                                        LEFT JOIN ancestors_as_descendants b
                                        ON b.descendant_concept_id = a.all_ancestor_concept_id
                                        WHERE c.invalid_reason IS NULL
                                                AND b.ancestor_concept_id IS NULL
                                        ",
                                        vocabSchema = vocabSchema,
                                        concept_filters = concept_filters)


                }


                queryAthena(sql_statement = sql_statement,
                            conn = conn,
                            cache_only = cache_only,
                            skip_cache = skip_cache,
                            override_cache = override_cache,
                            render_sql = render_sql,
                            verbose = verbose,
                            sleepTime = sleepTime)
        }







lookup_relationship <-
        function(concept_ids,
                 check_validity = TRUE,
                 conn,
                 vocabSchema,
                 cache_only = FALSE,
                 skip_cache = FALSE,
                 override_cache = FALSE,
                 render_sql = FALSE,
                 verbose = FALSE,
                 sleepTime = 1) {


                if (check_validity) {

                        if (verbose) {

                                cli::cli_rule(left = "Checking Validity")

                        }

                        sql_statement <-
                                SqlRender::render(
                                        "
                            SELECT *
                            FROM @vocabSchema.concept c
                            WHERE c.concept_id IN (@concept_ids)
                                    AND c.invalid_reason IS NULL
                            ",
                                        vocabSchema = vocabSchema,
                                        concept_ids =  concept_ids
                                )

                        output <- queryAthena(sql_statement = sql_statement,
                                              conn = conn,
                                              cache_only = cache_only,
                                              skip_cache = skip_cache,
                                              override_cache = override_cache,
                                              render_sql = render_sql,
                                              verbose = verbose,
                                              sleepTime = sleepTime)

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
                                FROM @vocabSchema.concept_relationship cr
                                LEFT JOIN @vocabSchema.concept c1
                                ON cr.concept_id_1 = c1.concept_id
                                LEFT JOIN @vocabSchema.concept c2
                                ON cr.concept_id_2 = c2.concept_id
                                WHERE cr.concept_id_1 IN (@concept_ids)
                                        AND cr.invalid_reason IS NULL
                                ",
                                vocabSchema = vocabSchema,
                                concept_ids = concept_ids
                        )


                queryAthena(sql_statement = sql_statement,
                            conn = conn,
                            cache_only = cache_only,
                            skip_cache = skip_cache,
                            override_cache = override_cache,
                            render_sql = render_sql,
                            verbose = verbose,
                            sleepTime = sleepTime)


        }




pivot_relationship <-
        function(concept_ids,
                 check_validity = TRUE,
                 conn,
                 vocabSchema,
                 cache_only = FALSE,
                 skip_cache = FALSE,
                 override_cache = FALSE,
                 render_sql = FALSE,
                 verbose = FALSE,
                 sleepTime = 1) {


                output <-
                        lookup_relationship(
                                concept_ids = concept_ids,
                                check_validity = check_validity,
                                conn = conn,
                                vocabSchema = vocabSchema,
                                cache_only = cache_only,
                                skip_cache = skip_cache,
                                override_cache = override_cache,
                                render_sql = render_sql,
                                verbose = verbose,
                                sleepTime = sleepTime)


        }

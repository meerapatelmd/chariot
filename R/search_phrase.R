search_exact_phrase <-
        function(phrase,
                 case_insensitive = TRUE,
                 vocab_schema = "omop_vocabulary",
                 conn,
                 conn_fun = "connectAthena()",
                 cache_only = FALSE,
                 skip_cache = FALSE,
                 override_cache = FALSE,
                 cache_resultset = TRUE,
                 render_sql = TRUE,
                 render_only = FALSE,
                 verbose = TRUE,
                 sleepTime = 1) {


                if (case_insensitive) {
                        sql_statement <-
                                SqlRender::render(
                                        "
                        SELECT c.*, STRING_AGG(cs.concept_synonym_name, '|') AS concept_synonyms
                        FROM @vocab_schema.concept_synonym cs
                        INNER JOIN @vocab_schema.concept c
                        ON c.concept_id = cs.concept_id
                        WHERE LOWER(cs.concept_synonym_name) = LOWER('@phrase')
                        GROUP BY c.concept_id,
                                c.concept_name,
                                c.domain_id,
                                c.vocabulary_id,
                                c.concept_class_id,
                                c.standard_concept,
                                c.concept_code,
                                c.valid_start_date,
                                c.valid_end_date,
                                c.invalid_reason;
                        ",
                                        vocab_schema = vocab_schema,
                                        phrase = phrase
                                )


                } else {
                sql_statement <-
                SqlRender::render(
                        "
                        SELECT c.*, STRING_AGG(cs.concept_synonym_name, '|') AS concept_synonyms
                        FROM @vocab_schema.concept_synonym cs
                        INNER JOIN @vocab_schema.concept c
                        ON c.concept_id = cs.concept_id
                        WHERE cs.concept_synonym_name = '@phrase'
                        GROUP BY c.concept_id,
                                c.concept_name,
                                c.domain_id,
                                c.vocabulary_id,
                                c.concept_class_id,
                                c.standard_concept,
                                c.concept_code,
                                c.valid_start_date,
                                c.valid_end_date,
                                c.invalid_reason;
                        ",
                        vocab_schema = vocab_schema,
                        phrase = phrase
                )
                }


                queryAthena(
                        sql_statement = sql_statement,
                        conn = conn,
                        conn_fun = conn_fun,
                        cache_only = cache_only,
                        skip_cache = skip_cache,
                        override_cache = override_cache,
                        cache_resultset = cache_resultset,
                        render_sql = render_sql,
                        render_only = render_only,
                        verbose = verbose,
                        sleepTime = sleepTime
                )
        }



search_like_phrase <-
        function(phrase,
                 case_insensitive = TRUE,
                 vocab_schema = "omop_vocabulary",
                 conn,
                 conn_fun = "connectAthena()",
                 cache_only = FALSE,
                 skip_cache = FALSE,
                 override_cache = FALSE,
                 cache_resultset = TRUE,
                 render_sql = TRUE,
                 render_only = FALSE,
                 verbose = TRUE,
                 sleepTime = 1) {


                if (case_insensitive) {
                        sql_statement <-
                                SqlRender::render(
                                        "
                        SELECT c.*, STRING_AGG(cs.concept_synonym_name, '|') AS concept_synonyms
                        FROM @vocab_schema.concept_synonym cs
                        INNER JOIN @vocab_schema.concept c
                        ON c.concept_id = cs.concept_id
                        WHERE LOWER(cs.concept_synonym_name) LIKE LOWER('%@phrase%')
                        GROUP BY c.concept_id,
                                c.concept_name,
                                c.domain_id,
                                c.vocabulary_id,
                                c.concept_class_id,
                                c.standard_concept,
                                c.concept_code,
                                c.valid_start_date,
                                c.valid_end_date,
                                c.invalid_reason;
                        ",
                                        vocab_schema = vocab_schema,
                                        phrase = phrase
                                )


                } else {
                        sql_statement <-
                                SqlRender::render(
                                        "
                        SELECT c.*, STRING_AGG(cs.concept_synonym_name, '|') AS concept_synonyms
                        FROM @vocab_schema.concept_synonym cs
                        INNER JOIN @vocab_schema.concept c
                        ON c.concept_id = cs.concept_id
                        WHERE cs.concept_synonym_name LIKE '%@phrase%'
                        GROUP BY c.concept_id,
                                c.concept_name,
                                c.domain_id,
                                c.vocabulary_id,
                                c.concept_class_id,
                                c.standard_concept,
                                c.concept_code,
                                c.valid_start_date,
                                c.valid_end_date,
                                c.invalid_reason;
                        ",
                                        vocab_schema = vocab_schema,
                                        phrase = phrase
                                )
                }


                queryAthena(
                        sql_statement = sql_statement,
                        conn = conn,
                        conn_fun = conn_fun,
                        cache_only = cache_only,
                        skip_cache = skip_cache,
                        override_cache = override_cache,
                        cache_resultset = cache_resultset,
                        render_sql = render_sql,
                        render_only = render_only,
                        verbose = verbose,
                        sleepTime = sleepTime
                )
        }


search_starts_with_phrase <-
        function(phrase,
                 case_insensitive = TRUE,
                 vocab_schema = "omop_vocabulary",
                 conn,
                 conn_fun = "connectAthena()",
                 cache_only = FALSE,
                 skip_cache = FALSE,
                 override_cache = FALSE,
                 cache_resultset = TRUE,
                 render_sql = TRUE,
                 render_only = FALSE,
                 verbose = TRUE,
                 sleepTime = 1) {


                if (case_insensitive) {
                        sql_statement <-
                                SqlRender::render(
                                        "
                        SELECT c.*, STRING_AGG(cs.concept_synonym_name, '|') AS concept_synonyms
                        FROM @vocab_schema.concept_synonym cs
                        INNER JOIN @vocab_schema.concept c
                        ON c.concept_id = cs.concept_id
                        WHERE LOWER(cs.concept_synonym_name) LIKE LOWER('@phrase%')
                        GROUP BY c.concept_id,
                                c.concept_name,
                                c.domain_id,
                                c.vocabulary_id,
                                c.concept_class_id,
                                c.standard_concept,
                                c.concept_code,
                                c.valid_start_date,
                                c.valid_end_date,
                                c.invalid_reason;
                        ",
                                        vocab_schema = vocab_schema,
                                        phrase = phrase
                                )


                } else {
                        sql_statement <-
                                SqlRender::render(
                                        "
                        SELECT c.*, STRING_AGG(cs.concept_synonym_name, '|') AS concept_synonyms
                        FROM @vocab_schema.concept_synonym cs
                        INNER JOIN @vocab_schema.concept c
                        ON c.concept_id = cs.concept_id
                        WHERE cs.concept_synonym_name LIKE '@phrase%'
                        GROUP BY c.concept_id,
                                c.concept_name,
                                c.domain_id,
                                c.vocabulary_id,
                                c.concept_class_id,
                                c.standard_concept,
                                c.concept_code,
                                c.valid_start_date,
                                c.valid_end_date,
                                c.invalid_reason;
                        ",
                                        vocab_schema = vocab_schema,
                                        phrase = phrase
                                )
                }


                queryAthena(
                        sql_statement = sql_statement,
                        conn = conn,
                        conn_fun = conn_fun,
                        cache_only = cache_only,
                        skip_cache = skip_cache,
                        override_cache = override_cache,
                        cache_resultset = cache_resultset,
                        render_sql = render_sql,
                        render_only = render_only,
                        verbose = verbose,
                        sleepTime = sleepTime
                )
        }



search_ends_with_phrase <-
        function(phrase,
                 case_insensitive = TRUE,
                 vocab_schema = "omop_vocabulary",
                 conn,
                 conn_fun = "connectAthena()",
                 cache_only = FALSE,
                 skip_cache = FALSE,
                 override_cache = FALSE,
                 cache_resultset = TRUE,
                 render_sql = TRUE,
                 render_only = FALSE,
                 verbose = TRUE,
                 sleepTime = 1) {


                if (case_insensitive) {
                        sql_statement <-
                                SqlRender::render(
                                        "
                        SELECT c.*, STRING_AGG(cs.concept_synonym_name, '|') AS concept_synonyms
                        FROM @vocab_schema.concept_synonym cs
                        INNER JOIN @vocab_schema.concept c
                        ON c.concept_id = cs.concept_id
                        WHERE LOWER(cs.concept_synonym_name) LIKE LOWER('%@phrase')
                        GROUP BY c.concept_id,
                                c.concept_name,
                                c.domain_id,
                                c.vocabulary_id,
                                c.concept_class_id,
                                c.standard_concept,
                                c.concept_code,
                                c.valid_start_date,
                                c.valid_end_date,
                                c.invalid_reason;
                        ",
                                        vocab_schema = vocab_schema,
                                        phrase = phrase
                                )


                } else {
                        sql_statement <-
                                SqlRender::render(
                                        "
                        SELECT c.*, STRING_AGG(cs.concept_synonym_name, '|') AS concept_synonyms
                        FROM @vocab_schema.concept_synonym cs
                        INNER JOIN @vocab_schema.concept c
                        ON c.concept_id = cs.concept_id
                        WHERE cs.concept_synonym_name LIKE '%@phrase'
                        GROUP BY c.concept_id,
                                c.concept_name,
                                c.domain_id,
                                c.vocabulary_id,
                                c.concept_class_id,
                                c.standard_concept,
                                c.concept_code,
                                c.valid_start_date,
                                c.valid_end_date,
                                c.invalid_reason;
                        ",
                                        vocab_schema = vocab_schema,
                                        phrase = phrase
                                )
                }


                queryAthena(
                        sql_statement = sql_statement,
                        conn = conn,
                        conn_fun = conn_fun,
                        cache_only = cache_only,
                        skip_cache = skip_cache,
                        override_cache = override_cache,
                        cache_resultset = cache_resultset,
                        render_sql = render_sql,
                        render_only = render_only,
                        verbose = verbose,
                        sleepTime = sleepTime
                )
        }



search_split_phrase <-
        function(phrase,
                 split = " |[[:punct:]]",
                 case_insensitive = TRUE,
                 vocab_schema = "omop_vocabulary",
                 conn,
                 conn_fun = "connectAthena()",
                 cache_only = FALSE,
                 skip_cache = FALSE,
                 override_cache = FALSE,
                 cache_resultset = TRUE,
                 render_sql = TRUE,
                 render_only = FALSE,
                 verbose = TRUE,
                 sleepTime = 1) {

                args <- unlist(strsplit(phrase, split = split))
                args <- trimws(args, which = "both")
                args <- args[!(args %in% c(""))]
                args <- paste0("%", args, "%")

                if (case_insensitive) {
                        sql_statement <- SqlRender::render(
                                                        "
                                                        SELECT c.*, STRING_AGG(cs.concept_synonym_name, '|') AS concept_synonyms
                                                        FROM @schema.concept_synonym cs
                                                        INNER JOIN @schema.concept c
                                                        ON c.concept_id  = cs.concept_id
                                                        WHERE @where_clauses
                                                        GROUP BY
                                                                c.concept_id,
                                                                c.concept_name,
                                                                c.domain_id,
                                                                c.vocabulary_id,
                                                                c.concept_class_id,
                                                                c.standard_concept,
                                                                c.concept_code,
                                                                c.valid_start_date,
                                                                c.valid_end_date,
                                                                c.invalid_reason;",
                                                           schema = vocab_schema,
                                                           where_clauses = paste(sprintf("LOWER(cs.concept_synonym_name) LIKE LOWER('%s')", args),
                                                                                 collapse = "\n\t\t\t\t\t\t\t\tAND "))
                }
                else {
                        sql_statement <- SqlRender::render(
                                "
                                                        SELECT c.*, STRING_AGG(cs.concept_synonym_name, '|') AS concept_synonyms
                                                        FROM @schema.concept_synonym cs
                                                        INNER JOIN @schema.concept c
                                                        ON c.concept_id  = cs.concept_id
                                                        WHERE @where_clauses
                                                        GROUP BY
                                                                c.concept_id,
                                                                c.concept_name,
                                                                c.domain_id,
                                                                c.vocabulary_id,
                                                                c.concept_class_id,
                                                                c.standard_concept,
                                                                c.concept_code,
                                                                c.valid_start_date,
                                                                c.valid_end_date,
                                                                c.invalid_reason;",
                                schema = vocab_schema,
                                where_clauses = paste(sprintf("cs.concept_synonym_name LIKE '%s'", args),
                                                      collapse = "\n\t\t\t\t\t\t\t\tAND "))
                }

                queryAthena(
                        sql_statement = sql_statement,
                        conn = conn,
                        conn_fun = conn_fun,
                        cache_only = cache_only,
                        skip_cache = skip_cache,
                        override_cache = override_cache,
                        cache_resultset = cache_resultset,
                        render_sql = render_sql,
                        render_only = render_only,
                        verbose = verbose,
                        sleepTime = sleepTime
                )

        }

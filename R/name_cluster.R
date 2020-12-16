#' @export

name_cluster <-
        function(concept_obj,
                 vocab_schema = "omop_vocabulary",
                 conn,
                 conn_fun = "connectAthena()",
                 cache_only = FALSE,
                 skip_cache = FALSE,
                 override_cache = FALSE,
                 render_sql = TRUE,
                 verbose = TRUE,
                 sleepTime = 1) {

                # concept_id <- 1112807

                if (class(concept_obj) == "concept") {

                        concept_id <- concept@concept_id

                } else {

                        concept_id <- concept_obj
                }


                sql_statement <-
                SqlRender::render(
                        "
                        With synonym AS (
                                SELECT DISTINCT
                                        'concept' AS source_type,
                                        c.concept_id,
                                        cs.concept_synonym_name AS concept_name
                                FROM @vocab_schema.concept c
                                INNER JOIN @vocab_schema.concept_synonym cs
                                ON cs.concept_id = c.concept_id
                                WHERE c.concept_id = @concept_id
                        ),
                        maps_to AS (
                                SELECT DISTINCT
                                        'maps to' AS source_type,
                                        @concept_id AS concept_id,
                                        c2.concept_name
                                FROM @vocab_schema.concept c
                                INNER JOIN @vocab_schema.concept_relationship cr
                                ON cr.concept_id_1 = c.concept_id
                                LEFT JOIN @vocab_schema.concept c2
                                ON c2.concept_id = cr.concept_id_2
                                WHERE c.concept_id = @concept_id
                                        AND cr.invalid_reason IS NULL
                                        AND cr.relationship_id = 'Maps to'
                        )

                        SELECT *
                        FROM synonym
                        UNION
                        SELECT *
                        FROM maps_to
                        ",
                        vocab_schema = vocab_schema,
                        concept_id = concept_id)


                queryAthena(sql_statement = sql_statement,
                            conn = conn,
                            conn_fun = conn_fun,
                            cache_only = cache_only,
                            skip_cache = skip_cache,
                            override_cache = override_cache,
                            render_sql = render_sql,
                            verbose = verbose,
                            sleepTime = sleepTime)
        }

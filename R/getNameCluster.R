




#' @export

getNameCluster <-
        function(concept_id,
                 vocabSchema,
                 conn = NULL,
                 cache_only = FALSE,
                 skip_cache = FALSE,
                 override_cache = FALSE,
                 render_sql = TRUE,
                 verbose = TRUE,
                 sleepTime = 1) {

                # concept_id <- 1112807

                sql_statement <-
                SqlRender::render(
                        "
                        With synonym AS (
                                SELECT c.concept_id, cs.concept_synonym_name AS concept_name
                                FROM @vocabSchema.concept c
                                INNER JOIN @vocabSchema.concept_synonym cs
                                ON cs.concept_id = c.concept_id
                                WHERE c.concept_id = @concept_id
                        ),
                        maps_to AS (
                                SELECT @concept_id AS concept_id,
                                        c2.concept_name
                                FROM @vocabSchema.concept c
                                INNER JOIN @vocabSchema.concept_relationship cr
                                ON cr.concept_id_1 = c.concept_id
                                LEFT JOIN @vocabSchema.concept c2
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
                        vocabSchema = vocabSchema,
                        concept_id = concept_id)


                queryAthena(sql_statement = sql_statement,
                            conn = conn,
                            cache_only = cache_only,
                            skip_cache = skip_cache,
                            override_cache = override_cache,
                            render_sql = render_sql,
                            verbose = verbose,
                            sleepTime = sleepTime)
        }

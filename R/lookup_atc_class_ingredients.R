







lookup_atc_class_ingredients <-
        function(conn,
                 vocab_schema,
                 atc_concept_obj,
                 cache_only = FALSE,
                 skip_cache = FALSE,
                 override_cache = FALSE,
                 render_sql = FALSE,
                 verbose = FALSE,
                 sleepTime = 1) {

                if (!(class(atc_concept_obj) %in% "concept")) {

                        atc_concept_obj<-
                                get_concept(concept_id = atc_concept_obj,
                                            vocab_schema = vocab_schema,
                                            conn = conn,
                                            cache_only = cache_only,
                                            skip_cache = skip_cache,
                                            override_cache = override_cache,
                                            render_sql = render_sql,
                                            verbose = verbose,
                                            sleepTime = sleepTime)
                }


                if (class(atc_concept_obj) %in% "concept") {

                        if (!(atc_concept_obj@vocabulary_id %in% "ATC")) {

                                stop("`atc_concept_obj` is not an ATC concept")
                        }

                        if (!(atc_concept_obj@standard_concept %in% "C")) {

                                stop("`atc_concept_obj` is not a Class concept")
                        }

                        atc_concept_id <- atc_concept_obj@concept_id


                } else {

                        stop("`atc_concept_obj` is not a 'concept' class object")
                }

                sql_statement <-
                SqlRender::render("SELECT c.*
                                FROM @vocab_schema.concept_ancestor ca
                                LEFT JOIN @vocab_schema.concept c
                                ON c.concept_id = ca.descendant_concept_id
                                WHERE ca.ancestor_concept_id = @atc_concept_obj AND
                                        c.vocabulary_id IN ('RxNorm', 'RxNorm Extension') AND
                                        c.concept_class_id IN ('Ingredient') AND
                                        c.invalid_reason IS NULL",
                                  vocab_schema = vocab_schema,
                                  atc_concept_obj = atc_concept_id)


                queryAthena(sql_statement = sql_statement,
                            conn = conn,
                            conn_fun = conn_fun,
                            cache_only = cache_only,
                            skip_cache = skip_cache,
                            override_cache = override_cache,
                            cache_resultset = cache_resultset,
                            render_sql = render_sql,
                            verbose = verbose,
                            sleepTime = sleepTime)

        }

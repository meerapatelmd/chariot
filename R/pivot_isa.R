


pivot_isa <-
        function(schema = "omop_vocabulary",
                 version_key = get_VersionKey(),
                 conn,
                 conn_fun = "pg13::local_connect(verbose = {verbose})",
                 skip_cache = FALSE,
                 override_cache = FALSE,
                 cache_only = FALSE,
                 render_sql = FALSE,
                 render_only = FALSE,
                 verbose = FALSE) {


                if (missing(conn)) {

                        conn <-
                                eval(rlang::parse_expr(glue::glue(conn_fun)))

                        on.exit(pg13::dc(conn = conn,
                                         verbose = verbose),
                                add = TRUE,
                                after = TRUE)


                }

                absolute_max_level <-
                        pg13::query(
                                conn = conn,
                                sql_statement =
                                        glue::glue("SELECT MAX(max_levels_of_separation) FROM {schema}.concept_ancestor;")) %>%
                        unlist() %>%
                        unname()


                sql_statement <-
                        paste(
                glue::glue(
                        "select c1.concept_id, c1.concept_name, c1.vocabulary_id ",
                        "from {schema}.concept c1 ",
                        "inner join {schema}.concept_relationship cr ",
                        "on c1.concept_id = cr.concept_id_1 ",
                        "left join {schema}.concept c2 ",
                        "on c2.concept_id = cr.concept_id_2 ",
                        "where ",
                        "  cr.invalid_reason IS NULL ",
                        "  AND cr.relationship_id = 'Is a'; "),
                collapse = " \n")


                pg13::query(conn = conn,
                            sql_statement = sql_statement)

        }

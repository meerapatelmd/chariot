ds_staged_table_exists <-
        function(conn,
                 conn_fun = "connectAthena()",
                 ds_schema = "patelm9") {

                if (missing(conn)) {

                        conn <- eval(rlang::parse_expr(conn_fun))
                        on.exit(expr = dcAthena(conn = conn),
                                add = TRUE,
                                after = TRUE)
                }

                tables <- pg13::ls_tables(conn = conn,
                                          schema = ds_schema)
                tables <- toupper(tables)

                "DRUG_STRENGTH_STAGED" %in% tables

        }




ds_join_on_drug_concept_id <-
        function(data,
                 drug_concept_id_col = "drug_concept_id",
                 conn,
                 conn_fun = "connectAthena()",
                 ds_schema = "patelm9",
                 verbose = TRUE,
                 render_sql = TRUE,
                 render_only = FALSE) {


                if (!(ds_staged_table_exists(conn = conn,
                                       ds_schema = ds_schema))) {

                        stop(sprintf("'DRUG_STRENGTH_STAGED' table does not
                                     exist in '%s' schema. Run if `ds_process()`
                                     and `ds_stage()` if it hasn't been written.",
                                     ds_schema))
                }


                join(data = data,
                     column = drug_concept_id_col,
                     vocab_table = "drug_strength_staged",
                     vocab_field = "drug_concept_id")


        }




ds_join_on_ingredient_concept_id <-
        function(data,
                 ingredient_concept_id_col = "drug_concept_id",
                 conn,
                 conn_fun = "connectAthena()",
                 ds_schema = "patelm9",
                 verbose = TRUE,
                 render_sql = TRUE,
                 render_only = FALSE) {


                if (!(ds_staged_table_exists(conn = conn,
                                             ds_schema = ds_schema))) {

                        stop(sprintf("'DRUG_STRENGTH_STAGED' table does not
                                     exist in '%s' schema. Run `ds_process()`
                                     and `ds_stage()` if it hasn't been written.",
                                     ds_schema))
                }


                join(data = data,
                     column = ingredient_concept_id_col,
                     vocab_table = "drug_strength_staged",
                     vocab_field = "ingredient_concept_id")


        }

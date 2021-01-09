#' @title
#' Process the Drug Strength Table
#' @description
#' The Drug Strength table is filtered for only valid entries and the 'box_size',
#' 'invalid_reason', 'valid_start_date', and 'valid_end_date' fields are selected
#' against. Concurrently the unit concept ids for the `numerator`, `denominator`, and
#' `amount` in the table are mapped to their names. The resulting Drug Strength
#' Processed table is stored in the given schema.
#'
#' After processing, the values found in the Drug Strength and Drug Strength
#' Processed tables can be staged for calculations. See `\code{\link{ds_stage}}.
#'
#' @return
#' Drug Strength Processed table
#' @seealso
#'  \code{\link[SqlRender]{render}}
#' @rdname ds_process
#' @export
#' @importFrom SqlRender render
#' @family drug strength functions
#' @family drug strength setup functions
ds_process <-
        function(conn,
                 conn_fun = "connectAthena()",
                 vocab_schema = "omop_vocabulary",
                 write_schema = "patelm9",
                 verbose = TRUE,
                 render_sql = TRUE,
                 render_only = FALSE) {

                if (missing(conn)) {

                        conn <- eval(rlang::parse_expr(conn_fun))
                        on.exit(expr = dcAthena(conn = conn),
                                add = TRUE,
                                after = TRUE)
                }

                ds_process_drug_table(conn = conn,
                                      vocab_schema = vocab_schema,
                                      write_schema = write_schema,
                                      verbose = verbose,
                                      render_sql = render_sql,
                                      render_only = render_only)


                ds_process_map_table(conn = conn,
                                     vocab_schema = vocab_schema,
                                     write_schema = write_schema,
                                     verbose = verbose,
                                     render_sql = render_sql,
                                     render_only = render_only)


                sql_statement <-
                        SqlRender::render(
                                "
                DROP TABLE IF EXISTS @write_schema.drug_strength_processed;
                CREATE TABLE @write_schema.drug_strength_processed AS (
                        select
                        	ds.*,
                        	map.amount_unit_concept_name,
                        	map.numerator_unit_concept_name,
                        	map.denominator_unit_concept_name
                        from @write_schema.ds_drug ds
                        left join @write_schema.ds_unit_map map
                        ON map.drug_concept_id = ds.drug_concept_id
                );
                ",
                                write_schema = write_schema)

                sendAthena(conn = conn,
                           sql_statement = sql_statement,
                           verbose = verbose,
                           render_sql = render_sql,
                           render_only = render_only)
        }

#' @title
#' Process the Records in Drug Strength
#' @description
#' The Drug Strength table is filtered for only valid entries and the 'box_size',
#' 'invalid_reason', 'valid_start_date', and 'valid_end_date' fields are selected
#' against.
#'
#' @return
#' A `ds_drug` table in `write_schema`. If it already exists, it will be rewritten.
#' @seealso
#'  \code{\link[SqlRender]{render}}
#' @rdname ds_process_map_table
#' @export
#' @importFrom SqlRender render
#' @family drug strength functions
#' @family drug strength processing functions
ds_process_drug_table <-
        function(conn,
                 conn_fun = "connectAthena()",
                 vocab_schema = "omop_vocabulary",
                 write_schema = "patelm9",
                 verbose = TRUE,
                 render_sql = TRUE,
                 render_only = FALSE) {

                if (missing(conn)) {

                        conn <- eval(rlang::parse_expr(conn_fun))
                        on.exit(expr = dcAthena(conn = conn),
                                add = TRUE,
                                after = TRUE)
                }

                sql_statement <-
                        SqlRender::render(
                                "
                                DROP TABLE IF EXISTS @write_schema.ds_drug;
                                CREATE TABLE @write_schema.ds_drug AS (
                                        SELECT
                                                drug_concept_id,
                                                ingredient_concept_id,
                                                amount_value,
                                                amount_unit_concept_id,
                                                numerator_value,
                                                numerator_unit_concept_id,
                                                denominator_value,
                                                denominator_unit_concept_id
                                        FROM @vocab_schema.drug_strength
                                        WHERE invalid_reason IS NULL
                                )
                                ",
                                vocab_schema = vocab_schema,
                                write_schema = write_schema
                        )

                sendAthena(conn = conn,
                           sql_statement = sql_statement,
                           verbose = verbose,
                           render_sql = render_sql,
                           render_only = render_only)
        }

#' @title
#' Process the Unit Concept Ids in the Drug Strength Table
#' @description
#' Map `amount_unit_concept_id`, `numerator_unit_concept_id`,
#' `denominator_unit_concept_id`` their respective concept names and store in
#' a table.
#' @return
#' A `ds_unit_map` table in `write_schema`. If it already exists, it will be
#' rewritten.
#' @seealso
#'  \code{\link[SqlRender]{render}}
#' @rdname ds_process_map_table
#' @export
#' @importFrom SqlRender render
#' @family drug strength functions
#' @family drug strength processing functions
ds_process_map_table <-
        function(conn,
                 conn_fun = "connectAthena()",
                 vocab_schema = "omop_vocabulary",
                 write_schema = "patelm9",
                 verbose = TRUE,
                 render_sql = TRUE,
                 render_only = FALSE) {

                if (missing(conn)) {

                        conn <- eval(rlang::parse_expr(conn_fun))
                        on.exit(expr = dcAthena(conn = conn),
                                add = TRUE,
                                after = TRUE)
                }

                unit_cols <- c("amount_unit_concept_id",
                               "numerator_unit_concept_id",
                               "denominator_unit_concept_id")

                for (i in seq_along(unit_cols)) {

                        unit_col <- unit_cols[i]
                        unit_type <- stringr::str_remove_all(unit_col,
                                                             pattern = "_concept_id")

                        sql_statement <-
                        SqlRender::render(
                                "
                                DROP TABLE IF EXISTS @write_schema.@unit_col;
                                CREATE TABLE  @write_schema.@unit_col AS (
                                SELECT
                                        ds.drug_concept_id,
                                        ds.ingredient_concept_id,
                                        ds.@unit_col,
                                        c.concept_name AS @unit_type_concept_name
                                FROM @vocab_schema.drug_strength ds
                                LEFT JOIN @vocab_schema.concept c
                                ON ds.@unit_col = c.concept_id
                                );",
                                unit_col = unit_col,
                                unit_type = unit_type,
                                vocab_schema = vocab_schema,
                                write_schema = write_schema
                        )

                        sendAthena(conn = conn,
                                   sql_statement = sql_statement,
                                   verbose = verbose,
                                   render_sql = render_sql,
                                   render_only = render_only)
                }


                sql_statement <-
                                        SqlRender::render(
                                        "
                                        DROP TABLE IF EXISTS @write_schema.ds_unit_map;
                                        CREATE TABLE @write_schema.ds_unit_map AS (
                                        SELECT DISTINCT ds.drug_concept_id, ds.ingredient_concept_id, a.amount_unit_concept_name, n.numerator_unit_concept_name, d.denominator_unit_concept_name
                                        FROM @vocab_schema.drug_strength ds
                                        LEFT JOIN @write_schema.amount_unit_concept_id a
                                        ON a.drug_concept_id = ds.drug_concept_id
                                                AND a.ingredient_concept_id = ds.ingredient_concept_id
                                        LEFT JOIN @write_schema.numerator_unit_concept_id n
                                        ON n.drug_concept_id = ds.drug_concept_id
                                                AND n.ingredient_concept_id = ds.ingredient_concept_id
                                        LEFT JOIN @write_schema.denominator_unit_concept_id d
                                        ON d.drug_concept_id = ds.drug_concept_id
                                                AND d.ingredient_concept_id = ds.ingredient_concept_id
                                        );
                                        DROP TABLE IF EXISTS @write_schema.amount_unit_concept_id;
                                        DROP TABLE IF EXISTS @write_schema.numerator_unit_concept_id;
                                        DROP TABLE IF EXISTS @write_schema.denominator_unit_concept_id;
                                        ",
                                        vocab_schema = vocab_schema,
                                        write_schema = write_schema
                                        )

                sendAthena(conn = conn,
                           sql_statement = sql_statement,
                           verbose = verbose,
                           render_sql = render_sql,
                           render_only = render_only)
        }

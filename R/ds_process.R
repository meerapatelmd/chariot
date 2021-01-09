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

ds_process <-
        function(conn,
                 conn_fun = "connectAthena()",
                 vocab_schema = "omop_vocabulary",
                 write_schema = "patelm9",
                 verbose = TRUE,
                 render_sql = TRUE,
                 render_only = FALSE) {

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
                        ON map.drug_concept_id = ds.drug_concept_id;
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

ds_process_drug_table <-
        function(conn,
                 conn_fun = "connectAthena()",
                 vocab_schema = "omop_vocabulary",
                 write_schema = "patelm9",
                 verbose = TRUE,
                 render_sql = TRUE,
                 render_only = FALSE) {

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

ds_process_map_table <-
        function(conn,
                 conn_fun = "connectAthena()",
                 vocab_schema = "omop_vocabulary",
                 write_schema = "patelm9",
                 verbose = TRUE,
                 render_sql = TRUE,
                 render_only = FALSE) {

                sendAthena(
                        conn = conn,
                        sql_statement =
                                SqlRender::render("
                        DROP TABLE IF EXISTS @write_schema.@units_table;
                        DROP TABLE IF EXISTS @write_schema.@units_table_staged;
                        CREATE TABLE  @write_schema.@units_table_staged (
                                drug_concept_id INTEGER,
                                unit_concept_id_type VARCHAR(255),
                                unit_concept_name VARCHAR(255)
                        );

                        WITH target_concept_units AS (
                                                SELECT
                                                        drug_concept_id,
                                                        unnest(array['amount_unit_concept_id', 'numerator_unit_concept_id', 'denominator_unit_concept_id']) AS unit_concept_id_type,
                                                       unnest(array[amount_unit_concept_id, numerator_unit_concept_id, denominator_unit_concept_id]) AS unit_concept_id
                                                FROM @vocab_schema.drug_strength
                                        ),
                                        target_concept_units2 AS (
                                                SELECT
                                                        u.*,
                                                        c.concept_name AS unit_concept_name
                                                FROM target_concept_units u
                                                LEFT JOIN @vocab_schema.concept c
                                                ON c.concept_id = u.unit_concept_id
                                        )

                                        INSERT INTO @write_schema.@units_table_staged
                                        SELECT drug_concept_id,
                                                unit_concept_id_type,
                                                unit_concept_name
                                        FROM target_concept_units2
                                        WHERE unit_concept_id IS NOT NULL;

                                        CREATE TABLE @write_schema.@units_table AS (
                                        SELECT DISTINCT *
        FROM crosstab('SELECT drug_concept_id, unit_concept_id_type, unit_concept_name FROM @write_schema.@units_table_staged') AS final_result(drug_concept_id INTEGER, amount_unit_concept_name VARCHAR, numerator_unit_concept_name VARCHAR, denominator_unit_concept_name VARCHAR)
                                        );

                                        DROP TABLE @write_schema.@units_table_staged;

                        ",
                                                  write_schema = write_schema,
                                                  units_table = "ds_unit_map",
                                                  vocab_schema = vocab_schema
                                ),
                        verbose = verbose,
                        render_sql = render_sql,
                        render_only = render_only
                )
        }

#' @title
#' Stage Drug Strengths
#'
#' @description
#' Create the Drug Strengths Staged table from the Drug Strengths Processed table.
#' Here, the fractional representations of each the value and the units are
#' created if applicable. The final fields `value` and `unit` represent expressions
#' as varchar that can be evaluated as numeric.
#'
#' With the calculations staged in the Drug Strengths Staged table, it can be
#' joined with the Drug Exposures table to calculate ingredient administrations
#' for research on cumulative doses over time and other research use cases. .
#'
#' @seealso
#'  \code{\link[SqlRender]{render}}
#' @rdname ds_stage
#' @export
#' @importFrom SqlRender render
ds_stage <-
        function(conn,
                 conn_fun = "connectAthena()",
                 write_schema = "patelm9",
                 verbose = TRUE,
                 render_sql = TRUE,
                 render_only = FALSE) {


                ds_stage_unit_fraction(
                        conn = conn,
                        write_schema = write_schema,
                        verbose = verbose,
                        render_sql = render_sql,
                        render_only = render_only
                )

                ds_stage_value_fraction(
                        conn = conn,
                        conn_fun = conn_fun,
                        write_schema = write_schema,
                        verbose = verbose,
                        render_sql = render_sql,
                        render_only = render_only
                )

                sql_statement <-
                        SqlRender::render(
                                "
                DROP TABLE IF EXISTS @write_schema.drug_strength_staged;
                CREATE TABLE @write_schema.drug_strength_staged AS (
                        select
                        	u.drug_concept_id,
                        	u.ingredient_concept_id,
                        	v.value,
                        	u.unit
                        from @write_schema.ds_value_fraction v
                        join @write_schema.ds_unit_fraction u
                        ON u.drug_concept_id = v.drug_concept_id
                                AND u.ingredient_concept_id = v.ingredient_concept_id
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
#' Stage the Fractional Representation of Units
#' @seealso
#'  \code{\link[SqlRender]{render}}
#' @rdname ds_stage_unit_fraction
#' @export
#' @importFrom SqlRender render
ds_stage_unit_fraction <-
        function(conn,
                 conn_fun = "connectAthena()",
                 write_schema = "patelm9",
                 verbose = TRUE,
                 render_sql = TRUE,
                 render_only = FALSE) {

                sql_statement <-
                SqlRender::render(
                "
                DROP TABLE IF EXISTS @write_schema.ds_unit_fraction;
                CREATE TABLE @write_schema.ds_unit_fraction (
                        drug_concept_id INTEGER,
                	ingredient_concept_id INTEGER,
                	amount_value NUMERIC,
                	amount_unit_concept_id INTEGER,
                	amount_unit_concept_name VARCHAR(255),
                	numerator_value NUMERIC,
                	numerator_unit_concept_id INTEGER,
                	numerator_unit_concept_name VARCHAR(255),
                	denominator_value NUMERIC,
                	denominator_unit_concept_id INTEGER,
                	denominator_unit_concept_name VARCHAR(255),
                	unit TEXT
                );

                WITH fraction_unit AS (
                	SELECT
                		dsp.*,
                		CASE
                			WHEN dsp.numerator_unit_concept_name IS NOT NULL AND dsp.denominator_unit_concept_name IS NOT NULL AND dsp.denominator_unit_concept_name <> dsp.numerator_unit_concept_name
                				THEN CONCAT(dsp.numerator_unit_concept_name, '/', dsp.denominator_unit_concept_name)
                			WHEN dsp.numerator_unit_concept_name IS NOT NULL AND dsp.denominator_unit_concept_name IS NULL
                				THEN dsp.numerator_unit_concept_name
                		END fraction_unit_concept_name
                	FROM @write_schema.drug_strength_processed dsp
                )
                INSERT INTO @write_schema.ds_unit_fraction
                SELECT DISTINCT
                	fu.drug_concept_id,
                	fu.ingredient_concept_id,
                	fu.amount_value,
                	fu.amount_unit_concept_id,
                	fu.amount_unit_concept_name,
                	fu.numerator_value,
                	fu.numerator_unit_concept_id,
                	fu.numerator_unit_concept_name,
                	fu.denominator_value,
                	fu.denominator_unit_concept_id,
                	fu.denominator_unit_concept_name,
                	coalesce(fraction_unit_concept_name, amount_unit_concept_name) AS unit
                FROM fraction_unit fu
                ;
                ",
                write_schema = write_schema)

                sendAthena(conn = conn,
                           sql_statement = sql_statement,
                           verbose = verbose,
                           render_sql = render_sql,
                           render_only = render_only)
        }


#' @title
#' Stage the Fractional Representation of Value
#' @seealso
#'  \code{\link[SqlRender]{render}}
#' @rdname ds_stage_value_fraction
#' @export
#' @importFrom SqlRender render
ds_stage_value_fraction <-
        function(conn,
                 conn_fun = "connectAthena()",
                 write_schema = "patelm9",
                 verbose = TRUE,
                 render_sql = TRUE,
                 render_only = FALSE) {

                sql_statement <-
                        SqlRender::render(
                                "
                DROP TABLE IF EXISTS @write_schema.ds_value_fraction;
                CREATE TABLE @write_schema.ds_value_fraction (
                        drug_concept_id INTEGER,
                	ingredient_concept_id INTEGER,
                	amount_value NUMERIC,
                	amount_unit_concept_id INTEGER,
                	amount_unit_concept_name VARCHAR(255),
                	numerator_value NUMERIC,
                	numerator_unit_concept_id INTEGER,
                	numerator_unit_concept_name VARCHAR(255),
                	denominator_value NUMERIC,
                	denominator_unit_concept_id INTEGER,
                	denominator_unit_concept_name VARCHAR(255),
                	value TEXT
                );

                WITH fraction_value AS (
                	SELECT
                		dsp.*,
                		CASE
                			WHEN dsp.numerator_value IS NOT NULL AND dsp.denominator_value IS NOT NULL AND dsp.denominator_value <> dsp.numerator_value
                				THEN CONCAT(dsp.numerator_value, '/', dsp.denominator_value)
                			WHEN dsp.numerator_value IS NOT NULL AND dsp.denominator_value IS NULL
                				THEN dsp.numerator_value::varchar
                		END fraction_value
                	FROM patelm9.drug_strength_processed dsp
                )

                INSERT INTO @write_schema.ds_value_fraction
                SELECT DISTINCT
                	fu.drug_concept_id,
                	fu.ingredient_concept_id,
                	fu.amount_value,
                	fu.amount_unit_concept_id,
                	fu.amount_unit_concept_name,
                	fu.numerator_value,
                	fu.numerator_unit_concept_id,
                	fu.numerator_unit_concept_name,
                	fu.denominator_value,
                	fu.denominator_unit_concept_id,
                	fu.denominator_unit_concept_name,
                	coalesce(fraction_value, amount_value::varchar) AS value
                FROM fraction_value fu
                ;",
                                write_schema = write_schema)

                sendAthena(conn = conn,
                           sql_statement = sql_statement,
                           verbose = verbose,
                           render_sql = render_sql,
                           render_only = render_only)
}
